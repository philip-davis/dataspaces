#include "globalDefines.h"
SUBROUTINE CURVATURE(io, field, K1, K2)
!     ***********************************************************************
!     

!     SUBROUTINE CURVATURE
!     =======================
!     Ray Grout (2008)
!
!     DESCRIPTION
!     ===========
!     POST PROCESSING - GIVEN PV FIELD
!     COMPUTES GRADIENTS OF SURFACE NORMAL VECTOR
!     ROTATES CURVATURE MATRIX TO ALIGN WITH LOCAL NORMAL
!     COMPUTES EIGENVALUES & VECTORS
!     ------------------------------------------------------------------------
  !use topology_m
  use param_m, only : nx, ny, nz, npx, npy,npz, nx_g, ny_g, nz_g, n_spec, iforder
  use reference_m
  use grid_m, only : x, y, z, scale_1x, scale_1y, scale_1z
  !use mixfrac_m
  !use variables_m
  !use s3d_hdf_interface_m
  !use chemkin_m, only : species_name, n_species, reaction_rate
  use filter_m

      IMPLICIT NONE



      ! Data exchange
      real, dimension(nx,ny,nz),intent(in) :: field
      real, dimension(nx,ny,nz),intent(out) :: k1, k2
      real, dimension(nx,ny,nz,3) :: kvec1, kvec2, surfnormal
      integer, intent(in) :: io

      ! Counters
      INTEGER I, J, K, ii, jj

      ! Scratch space / work variables
      REAL SCR(NX, NZ)
      REAL box11, box12, box21, box22
      REAL DELX, grid_x(NX)
      real magnorm(nx,ny,nz)

      REAL MAGGC
      REAL FNORMX(NX,NY,NZ), FNORMY(NX,NY,NZ), FNORMZ(NX,NY,NZ),FNORM(3)

      REAL HIJ(NX, NY, NZ, 3, 3)

!     For rotation
      real angle
      external vdot
      external vmag
      real vdot, vmag
      REAL VROTAX(3), R(3,3), A(3,3)
      real rothij(3,3)
!     For LaPack
      real WR(3), WI(3)
      real VL(3), VN(3), VSCR
      real VR(2,2)
      real WORK(150)
      integer INFO

      REAL RINV(3,3), L1(3,3),L2(3,3), LM1(3,3), LM2(3,3), REFL(3,3), B(2,2)


!     FOR DEGUGGING, PUT SPHERE / TEST FUNCN IN PV FIELD -------------
!      DO K = 1,NZ
!         DO J = 1,NY
!            DO I = 1,NX
!               PV(I,J,K) =  DBLE ( (I - 64)*(I-64) 
!     $              + (J - 64)*(J-64) )!  +
!c     $              2.0d0*(K - 64)*(K-64) )
!
!c               PV(I,J,K) = DBLE(J+K)
!c               PV(I,J,K)=dble(i)
!c     $              + sin(32.0d0*atan(1.0d0)*dble(k)/127.0d0)
!c     $              *cos(32.0d0*atan(1.0d0)*dble(j)/127.0d0)
!            ENDDO
!         ENDDO
!      ENDDO
!C     END FOR DEGUGGING, PUT SPHERE IN PV FIELD ---------



      ! Filter field before normalization
      iforder = 2
      call initialize_filter(io)
      
      do i=1,3
         call filter( surfnormal(1,1,1,i), io )
      enddo
      call allocate_filter_arrays(-1)


     ! Differentiate field to find normal
!      
     call derivative_x(nx,ny,nz,field,surfnormal(:,:,:,1),scale_1x,1)
     call derivative_y(nx,ny,nz,field,surfnormal(:,:,:,2),scale_1y,1)
     call derivative_z(nx,ny,nz,field,surfnormal(:,:,:,3),scale_1z,1)

      WORK(1) = 0.0D0
      work(2) = 0.0d0
      
      magnorm = 0.0d0
      do i=1,3
         magnorm = magnorm + surfnormal(:,:,:,i)**2
      enddo
      where( magnorm .ge.1.0d-10 )
         magnorm = sqrt(magnorm)
 !        surfnormal(:,:,:,1) = surfnormal(:,:,:,1) / magnorm
 !        surfnormal(:,:,:,2) = surfnormal(:,:,:,2) / magnorm
 !         surfnormal(:,:,:,3) = surfnormal(:,:,:,3) / magnorm
      elsewhere
 !        surfnormal(:,:,:,1) = 0.0d0
 !        surfnormal(:,:,:,2) = 0.0d0
 !        surfnormal(:,:,:,3) = 0.0d0
         end where


!     Differentiate flame normal - may need to replace this with low order derivative
      call derivative_x(nx,ny,nz,surfnormal(:,:,:,1),hij(:,:,:,1,1),scale_1x,1)
      call derivative_y(nx,ny,nz,surfnormal(:,:,:,1),hij(:,:,:,1,2),scale_1y,1)
      call derivative_z(nx,ny,nz,surfnormal(:,:,:,1),hij(:,:,:,1,3),scale_1z,1)

      call derivative_x(nx,ny,nz,surfnormal(:,:,:,2),hij(:,:,:,2,1),scale_1x,1)
      call derivative_y(nx,ny,nz,surfnormal(:,:,:,2),hij(:,:,:,2,2),scale_1y,1)
      call derivative_z(nx,ny,nz,surfnormal(:,:,:,2),hij(:,:,:,2,3),scale_1z,1)

      call derivative_x(nx,ny,nz,surfnormal(:,:,:,3),hij(:,:,:,3,1),scale_1x,1)
      call derivative_y(nx,ny,nz,surfnormal(:,:,:,3),hij(:,:,:,3,2),scale_1y,1)
      call derivative_z(nx,ny,nz,surfnormal(:,:,:,3),hij(:,:,:,3,3),scale_1z,1)

      do i=1,3
         do j=1,3
            hij(:,:,:,i,j) = hij(:,:,:,i,j) / magnorm
         enddo
      enddo

!     [[[ ANALYSIS OF CURVATURE
      DO K = 1,NZ
         DO J = 1,NY
            DO I = 1,NX

!     COMPUTE ROTATION TO ALIGN GRID X-DIR WITH FLAME NORMAL

!     FIRST ROTATE INTO X'-Z' PLANE; ROTATE ANGLE ABOUT Z
               FNORM(1) = ABS(surfnormal(i,j,k,1))
               FNORM(2) = ABS(surfnormal(i,j,k,2))
               FNORM(3) = 0.0D0
               WORK(1) = VMAG(FNORM)
               IF (WORK(1).GT.0) THEN
                  FNORM = FNORM / WORK(1)
                  
                  VN(1) = 1.0D0
                  VN(2) = 0.0D0
                  VN(3) = 0.0D0
                  
                  ANGLE = ACOS( VDOT(FNORM,VN))
               ELSE
                  ANGLE = 0.0D0
               ENDIF

!               write(*,*) 'angle1=',angle*45.0d0/atan(1.0d0)

               L1(1,1) = COS( ANGLE )
               L1(2,2) =  COS( ANGLE )
               L1(3,3) = 1.0D0

               L1(1,3) = 0.0D0
               L1(3,1) = 0.0D0
               
               L1(2,3) = 0.0D0
               L1(3,2) = 0.0D0
               
               L1(1,2) =  SIN( ANGLE )
               L1(2,1) = -SIN( ANGLE)

!     NEXT ROTATE ABOUT INTERMEDIATE Y TO ALIGN INTERMEDIATE X WITH X'
               FNORM(1) = ABS(surfnormal(i,j,k,1))
               FNORM(2) = ABS(surfnormal(i,j,k,2))
               FNORM(3) = ABS(surfnormal(i,j,k,3))
               WORK(1) = VMAG(FNORM)
               fnorm = fnorm / work(1)
               
               VN(1) = ABS(surfnormal(i,j,k,1))
               VN(2) = ABS(surfnormal(i,j,k,2))
               VN(3) = 0.0D0
               WORK(1) = VMAG(VN)
               IF(WORK(1).GT.0.0D0) THEN
                  vn = vn / work(1)
                  IF (VDOT(FNORM,VN) .LT. 1.0D0 ) THEN
                     ANGLE = ACOS( min(VDOT(FNORM,VN),1.0d0) )
                  ELSE
                     ANGLE = 0.0D0
                  ENDIF
!                  write(*,*) 'angle2=',angle*45.0d0/atan(1.0d0)
!                  write(*,*) 'normal vector=',surfnormal(i,j,k,1),
!     $                 surfnormal(i,j,k,2),surfnormal(i,j,k,Zidx
               ELSE
                  IF(FNORM(3).EQ.1.0D0) THEN
                     ANGLE = 2.0D0*ATAN(1.0D0)
                  ELSE
                     ANGLE = 0.0D0
                  ENDIF
               ENDIF

               L2(1,1) = COS( ANGLE )
               L2(2,2) = 1.0D0
               L2(3,3) = COS( ANGLE )

               L2(1,3) = SIN( ANGLE )
               L2(3,1) = -SIN( ANGLE)
               
               L2(2,3) = 0.0D0
               L2(3,2) = 0.0D0
               
               L2(1,2) = 0.0D0
               L2(2,1) = 0.0D0
               if(i.eq.58.and.j.eq.30.and.k.eq.64) then
                  write(*,*) ANGLE, FNORM, VDOT(FNORM,VN)
               endif
!     ADD REFLECTION NECESSARY TO GET CORRECT QUADRANT
               DO II=1,3
                  DO JJ=1,3
                     REFL(II,JJ) = 0.0D0
                  ENDDO
               ENDDO
               DO JJ=1,3
                  REFL(JJ,JJ) = 1.0D0
               ENDDO
               IF(ABS(surfnormal(i,j,k,1))*surfnormal(i,j,k,1) .LT. 0.0D0) THEN
                  REFL(1,1) = -1.0D0
               ENDIF

               IF(ABS(surfnormal(i,j,k,2))*surfnormal(i,j,k,2) .LT. 0.0D0) THEN
                  REFL(2,2) = -1.0D0
               ENDIF

               IF(ABS(surfnormal(i,j,k,3))*surfnormal(i,j,k,3) .LT. 0.0D0) THEN
                  REFL(3,3) = -1.0D0
               ENDIF
                  
!     COMBINE ROTATIONS; R = L2 L1 REFL, RINV = REFL L1' L2'

               CALL DGEMM('N','N',3,3,3,1.0d0,L2,3,L1,3,0.0d0,A,3)
               CALL DGEMM('N','N',3,3,3,1.0d0,A,3,REFL,3,0.0d0,R,3)
               
               CALL DGEMM('T','T',3,3,3,1.0d0,L1,3,L2,3,0.0d0,A,3)
               CALL DGEMM('N','N',3,3,3,1.0d0,REFL,3,A,3,0.0d0,RINV,3)

               
!     CHECK ROTATION ON VECTOR
               VN(1) = 1.0D0
               VN(2) = 0.0D0
               VN(3) = 0.0D0

               DO II=1,3
                  VL(II) = 0.0D0
                  DO JJ=1,3
                     VL(II) = VL(II) + RINV(II,JJ)*VN(JJ)
                  ENDDO
               ENDDO
!               write(*,*) 'Rotated VR(:,3):',VL/VMAG(VL)
!               write(*,*) 'Unrotated x-dir:',VL


               vn(1)=fnormx(i,j,k)
               vn(2)=fnormy(i,j,k)
               vn(3)=fnormz(i,j,k)

               do ii=1,3
                  vl(ii) = 0.0d0
                  do jj=1,3
                     vl(ii) = vl(ii) + r(ii,jj)*vn(jj)
                  enddo
               enddo
!               write(*,*) 'Orig. normal vector', VN,'\n'
!               write(*,*) 'Rotated normal vector', VL,'\n'

!C     COMPUTE ROTHIJ
               A = HIJ(I,J,K,:,:)
               CALL DGEMM('N','T',3,3,3,1.0D0,R,3,A,3,0.0D0,ROTHIJ,3)

!               WRITE(*,*) 'STEP ONE - RINV * HIJ'
!               WRITE(*,*) '--------------------------'
!               WRITE(*,'3(D12.4)') ROTHIJ(1,1:3)
!               WRITE(*,'3(D12.4)') ROTHIJ(2,1:3)
!               WRITE(*,'3(D12.4)') ROTHIJ(3,1:3)
!               WRITE(*,*) '--------------------------'
!

               CALL DGEMM('N','N',3,3,3,1.0D0,ROTHIJ,3,RINV,3,0.0D0,A,3)


!$$$               WRITE(*,*) 'A'
!$$$               WRITE(*,*) '--------------------------'
!$$$               WRITE(*,'3(D12.4)') A(1,1:3)
!$$$               WRITE(*,'3(D12.4)') A(2,1:3)
!$$$               WRITE(*,'3(D12.4)') A(3,1:3)
!$$$               WRITE(*,*) '--------------------------'


!               BIGGESTA1 = MAX(BIGGESTA1,ABS(A(1,1)))
!               BIGGESTA1 = MAX(BIGGESTA1,ABS(A(1,2)))
!               BIGGESTA1 = MAX(BIGGESTA1,ABS(A(1,3)))
!               BIGGESTA1 = MAX(BIGGESTA1,ABS(A(3,1)))
!               BIGGESTA1 = MAX(BIGGESTA1,ABS(A(2,1)))
!               
!               if(biggesta1.ge.1.0d0) then
!                  write(0,*)I,J,K
!                  WRITE(0,*) 'A'
!                  WRITE(0,*) '--------------------------'
!                  WRITE(0,'3(D12.4)') A(1,1:3)
!                  WRITE(0,'3(D12.4)') A(2,1:3)
!                  WRITE(0,'3(D12.4)') A(3,1:3)
!                  WRITE(0,*) '--------------------------'
!                  stop
!               endif
!
!     COPY 2,3 ELEMENTS OF A TO 2X2 VECTOR
               B(1,1) = A(2,2)
               B(2,2) = A(3,3)
               B(1,2) = A(2,3)
               B(2,1) = A(3,2)

!     FIND EIGENVECTORS AND EIGENVALUES
               CALL DGEEV( 'N', 'V', 2, B, 2, WR, WI, VSCR, 1, VR,&
                    2, WORK, 150, INFO ) 

               IF(INFO.NE.0) THEN
                  WRITE(0,*) 'INFO =',INFO
                  WRITE(0,*) 'I,J,K=',I,J,K
               ENDIF
!               if(i.eq.80.and.j.eq.64.and.k.eq.64) then
!                  write(*,*) 'B=',B
!                  write(*,*) 'WR=',WR(1),WR(2)
!                  write(*,*) 'VR(:,1)=',VR(:,1)
!                  write(*,*) 'VR(:,2)=',VR(:,2)
!                  write(*,*) 'R=',R
!                  stop
!               endif

               IF( WR(1) .LE. WR(2) ) THEN
                  K1(I,J,K) = WR(1)!, KVEC1, KVEC2, surfnormal)

                  KVEC1(I,J,K,1) = 0.0D0
                  KVEC1(I,J,K,2) = VR(1,1)
                  KVEC1(I,J,K,3) = VR(2,1)

                  K2(I,J,K) = WR(2)
                  KVEC2(I,J,K,1) = 0.0D0
                  KVEC2(I,J,K,2) = VR(1,2)
                  KVEC2(I,J,K,3) = VR(2,2)
!, KVEC1, KVEC2, surfnormal)
               ELSE
                  K2(I,J,K) = WR(1)

                  KVEC2(I,J,K,1) = 0.0D0
                  KVEC2(I,J,K,2) = VR(1,1)
                  KVEC2(I,J,K,3) = VR(2,1)

                  K1(I,J,K) = WR(2)
                  KVEC1(I,J,K,1) = 0.0D0
                  KVEC1(I,J,K,2) = VR(1,2)
                  KVEC1(I,J,K,3) = VR(2,2)
                  
               ENDIF

!     AT THIS POINT KVEC1, KVEC2 ARE IN LOCAL FLAME-NORMAL BASED COORDINATES
!     UNROTATE TO GET BACK TO GRID COORDIANTES


               VN=KVEC1(I,J,K,:)
               DO II=1,3
                  KVEC1(I,J,K,II) = 0.0D0
                  DO JJ=1,3
                     KVEC1(I,J,K,II) = KVEC1(I,J,K,II) + RINV(II,JJ)*VN(JJ)
                  ENDDO
               ENDDO
               
               VN=KVEC2(I,J,K,:)
               DO II=1,3
                  KVEC2(I,J,K,II) = 0.0D0
                  DO JJ=1,3
                     KVEC2(I,J,K,II) = KVEC2(I,J,K,II)+ RINV(II,JJ)*VN(JJ)
                  ENDDO
               ENDDO
               
            ENDDO
         ENDDO
      ENDDO


      END


      FUNCTION VDOT(V1, V2)
      
!     ROUTINE TO COMPUTE DOT PRODUCT IN R3 CARTESIAN SPACE FOR REAL NUMBERS

!     ----------------------------------------------------------------------
!     DATA EXCHANGE
!     -------------
      REAL V1(3), V2(3), VDOT
      INTEGER I
      VDOT = 0.0D0

      DO I=1,3
         VDOT = VDOT + V1(I)*V2(I)
      ENDDO

      RETURN
      END

      FUNCTION VMAG(V1)
      
!     ROUTINE TO COMPUTE MAGNITUDE IN R3 CARTESIAN SPACE FOR REAL NUMBERS

!     ----------------------------------------------------------------------
!     DATA EXCHANGE
!     -------------
      REAL VMAG, V1(3)
      INTEGER I
      VMAG = 0.0D0

      DO I=1,3
         VMAG = VMAG + V1(I)*V1(I)
      ENDDO
      VMAG = SQRT(VMAG)

      RETURN

      END

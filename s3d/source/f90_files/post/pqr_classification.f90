#include "globalDefines.h"
! ==================================================================================
! Routines for classification of turbulent flow topology
! Author - Ray Grout (2009)
!
! Usage:
! - Requires s3d_hdf5_interface to be initialized already and have written basic 
! hdf5 file 
! ==================================================================================

SUBROUTINE pqr_classification(io)
  use variables_m, only : u
  USE hdf5
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g
  use s3d_hdf_interface_m
  use reference_m
  IMPLICIT NONE

  INTEGER io
  
    !----------------------------------------------------------------------
    ! hdf declarations


    CHARACTER(LEN=20) :: groupname  ! Group name
    CHARACTER(LEN=20) :: fieldname  ! Field name
    CHARACTER(LEN=250) :: fielddesc  ! Very long field name
    CHARACTER(LEN=20) :: fieldunits  ! Field units
    CHARACTER(LEN=50) :: filename   ! File name

    LOGICAL :: overwrite
    INTEGER :: error

    ! Storage for classification results
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: classification, P, Q, R
    integer i, j, k

    ALLOCATE(classification(nx,ny,nz))
    ALLOCATE(P(nx,ny,nz))
    ALLOCATE(Q(nx,ny,nz))
    ALLOCATE(R(nx,ny,nz))

    ! CLASSIFY FLOW TOPOLOGY  =======================================================
    CALL CLASSIFY_TOPOLOGY( io, classification, P, Q, R) 


    ! FOR OUTPUT ON SAME GRID AS INPUT ONLY
      filename = write_hdf5_param%filename 
      groupname = "/derived"
      fieldname = "flow_topology"
      fielddesc = " Flow topology classification"
      fieldunits = ""
      overwrite = .false.
      CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, classification, 1.0d0, error,overwrite)

      fieldname = "P"
      fielddesc = " Flow topology classification - P"
      CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, P, 1.0d0, error,overwrite)

      fieldname = "Q"
      fielddesc = " Flow topology classification - Q"
      CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, Q, 1.0d0, error,overwrite)
      
      fieldname = "R"
      fielddesc = " Flow topology classification - R"
      CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, R, 1.0d0, error,overwrite)

      DEALLOCATE(classification)
      DEALLOCATE(P)
      DEALLOCATE(Q)
      DEALLOCATE(R)

  END SUBROUTINE pqr_classification



  SUBROUTINE CLASSIFY_TOPOLOGY( io, classification, P, Q, R) 
  ! ==================================================================================================
  ! ROUTINE TO COMPUTE FLOW TOPOLOGY VIA PQR DECOMPOSITION - RAY GROUT , FEB 2009
  ! Reference: Chong, Perry, and Cantwell,
  !            Phys. Fluids A 2(5), May 1990 pp 765--777
  ! Classification table translated from code provided by J. Chen 
  ! INPUT:
  ! ==================================================================================================
  use param_m, only: nx, ny, nz, nx_g, ny_g, nz_g
  use grid_m
  use topology_m
  use reference_m, only : a_ref
  use variables_m, only :  u
  use mixFrac_m,   only : mixFrac
  IMPLICIT NONE

  integer io
  real, dimension(nx,ny,nz) :: classification


  real, dimension(nx,ny,nz) :: scratch, uvel, vvel, wvel
  real, dimension(nx,ny,nz) :: P, Q, R ! Don't strictly need to keep all of
                                       ! these, but may be convenient to be able to plot them separately

  real, dimension(nx,ny) :: mean_u, mean_v, gmean_u, gmean_v
  real :: numer, denom
  real, dimension(nx,ny,nz,6) :: SIJ, RIJ

  real, dimension(3,3) :: A, B
  real topo_type
  integer i,j,k
  integer mi, mj, mk

  real zero_vel ! Consider that we have a critical point if (UiUi) < zerovel
  zero_vel = 1.0


  ! Differentiate velocity fields to get Sij and Rij
  if( myid == 0 ) then
    write(io,*) 'Beginning evaluation of Sij and Rij'
  endif



  call derivative_x(nx,ny,nz,u(:,:,:,1),scratch,scale_1x,1)
  SIJ(:,:,:,1) = 2.0D0*scratch
  RIJ(:,:,:,1) = 0.0D0


  call derivative_y(nx,ny,nz,u(:,:,:,1),scratch,scale_1y,1)
  SIJ(:,:,:,4) = scratch ! Bug RG 06Oct09 - this line was missing!
  RIJ(:,:,:,4) = scratch

  call derivative_z(nx,ny,nz,u(:,:,:,1),scratch,scale_1z,1)
  SIJ(:,:,:,5) = scratch
  RIJ(:,:,:,5) = scratch

  call derivative_x(nx,ny,nz,u(:,:,:,2),scratch,scale_1x,1)
  SIJ(:,:,:,4) = SIJ(:,:,:,4) + scratch
  RIJ(:,:,:,4) = RIJ(:,:,:,4) - scratch

  call derivative_y(nx,ny,nz,u(:,:,:,2),scratch,scale_1y,1)
  SIJ(:,:,:,2) = 2.0D0*scratch
  RIJ(:,:,:,2) = 0.0D0
 
  call derivative_z(nx,ny,nz,u(:,:,:,2),scratch,scale_1z,1)
  SIJ(:,:,:,6) = scratch
  RIJ(:,:,:,6) = scratch

  call derivative_x(nx,ny,nz,u(:,:,:,3),scratch,scale_1x,1)
  SIJ(:,:,:,5) = SIJ(:,:,:,5) + scratch
  RIJ(:,:,:,5) = RIJ(:,:,:,5) - scratch


  call derivative_y(nx,ny,nz,u(:,:,:,3),scratch,scale_1y,1)
  SIJ(:,:,:,6) =  SIJ(:,:,:,6)  + scratch
  RIJ(:,:,:,6) =  RIJ(:,:,:,6)  - scratch

  call derivative_z(nx,ny,nz,u(:,:,:,3),scratch,scale_1z,1)
  SIJ(:,:,:,3) = 2.0D0*scratch
  RIJ(:,:,:,3) = 0.0D0


  if( myid == 0 ) then
    write(io,*) 'Beginning calculation of PQR '
  endif

  ! Compute P, Q, & R
  do k=1, nz
     do j=1, ny
        do i = 1, nx

          ! Copy Sij & Rij into temporary arrays to make the computation of R clean

           A(1,1) = SIJ(i,j,k,1)
           A(2,2) = SIJ(i,j,k,2)
           A(3,3) = SIJ(i,j,k,3)

           A(2,1) =SIJ(i,j,k,4)
           A(3,1) =SIJ(i,j,k,5)

           A(1,2) = SIJ(i,j,k,4)
           A(3,2) = SIJ(i,j,k,6)

           A(1,3) = SIJ(i,j,k,5)
           A(2,3) = SIJ(i,j,k,6)

           B(1,1) = RIJ(i,j,k,1)
           B(2,2) = RIJ(i,j,k,2)
           B(3,3) = RIJ(i,j,k,3)

           B(2,1) =RIJ(i,j,k,4)
           B(3,1) =RIJ(i,j,k,5)

           B(1,2) = RIJ(i,j,k,4)
           B(3,2) = RIJ(i,j,k,6)

           B(1,3) = RIJ(i,j,k,5)
           B(2,3) = RIJ(i,j,k,6)

          ! P = -tr(SIJ)
          P(i,j,k) = -1.0D0 * sum( SIJ(i,j,k,1:3) ) 

          ! Q = 0.5* (P**2 - (Sij*Sji + Rij*Rji) )
          !   = 0.5* (P**2 - (Sij^2 - Rij^2) ) since Sij is symm & Rij is
          !                                    anti-symm
          Q(i,j,k) = 0.5D0 * ( P(i,j,k)*P(i,j,k) - ( sum( SIJ(i,j,k,:) * SIJ(i,j,k,:) ) &
                                - sum( RIJ(i,j,k,:) * RIJ(i,j,k,:) ) ) )

          ! R = 1/3( -P**3 + 3PQ - Sij*Sjk*Ski - 3RijRjkSki
          R(i,j,k)  = -1.0D0 * P(i,j,k) * P(i,j,k) * P(i,j,k) &
                     + 3.0D0 * P(i,j,k) * Q(i,j,k) 

          do mi=1,3
            do mj=1,3
              R(i,j,k) = R(i,j,k) - A(mi,mj) * sum( A(mj,:) * A(:,mi) ) &
                    - 3.0D0 * B(mi,mj) * sum( B(mj,:) * A(:,mi) )
            enddo
          enddo

          R(i,j,k) = R(i,j,k) / 3.0D0


        enddo
      enddo
    enddo

  if( myid == 0 ) then
    write(io,*) 'Beginning to classify points'
  endif

  ! Compute P, Q, & R
  !scratch = u(:,:,:,1)*u(:,:,:,1) + u(:,:,:,2)*u(:,:,:,2) + u(:,:,:,3)*u(:,:,:,3)
  !zero_vel = (zero_vel / a_ref) **2.0D0
  do k=1, nz
     do j=1, ny
        do i = 1, nx

  !        if( scratch(i,j,k) < zero_vel ) then
  !        ! Classify topology based on PQR
            classification(i,j,k) = topo_type(P(i,j,k), Q(i,j,k), R(i,j,k) )
  !        else
  !          classification(i,j,k) = -2.0
  !        endif

        enddo
      enddo
    enddo

  if( myid == 0 ) then
    write(io,*) 'Finished classifying points'
  endif
  END SUBROUTINE CLASSIFY_TOPOLOGY

  FUNCTION topo_type( P, Q, R )
  ! RG 2009
  ! Adapted from the (c language) routine provided by Jackie

  real :: topo_type
  real, intent(in) :: P, Q, R


  ! Classify based on P,Q,R
  real ::  p2
  real :: ra, rb
  
  p2 = P*P

  topo_type = -1

  if(Q <= p2/3.0) then
    ra = P/3.0D0*(Q - 2.0D0/9.0D0*p2) - 2.0D0/27.0D0 * (-3.0D0*Q + p2)**1.5D0
    rb = P/3.0D0*(Q - 2.0D0/9.0D0*p2) + 2.0D0/27.0D0 * (-3.0D0*Q + p2)**1.5D0
  else 
    ra = 0.0D0
    rb = 0.0D0
  endif

  if(Q > 0.0D0 .and. Q < p2/3.0D0) then

    if(P > 0.0D0) then

      if(R > 0.0 .and. rr < rb) then 
        topo_type = 1 
        return 
      endif ! Type  1a 

      if(R == rb) then 
        topo_type = 3 
        return 
      endif ! Type  2a 

    endif

    if(P < 0.0) then

      if(R > ra .and. rr < 0.0) then 
        topo_type = 2 
        return 
      endif ! Type  1b 

      if(R == rb) then 
        topo_type = 4 
        return 
      endif ! Type  2b 
      
    endif

  endif

  if(Q > 0.0 .and. Q <= p2/3) then

    if(P < 0.0) then

      if(R > P*Q .and. rr < ra) then 
        topo_type = 19 
        return 
      endif ! Type  9b 

    endif

    if(P > 0.0 .and. R > rb .and. rr < P*Q) then 
      topo_type = 20 
      return 
    endif ! Type  10a 

  endif

  if(Q > p2/4.0 .and. Q < p2/3.0) then

    if(P > 0.0) then

      if(R == ra) then 
        topo_type = 3 
        return 
      endif ! Type  2a 

      if(R > 0.0 .and. rr < ra) then 
        topo_type = 20 
        return 
      endif ! Type  10a 
    endif

    if(P < 0.0) then

      if(R == ra) then 
        topo_type = 4 
        return 
      endif ! Type  2b 

      if(R < 0.0 .and. rr > rb) then 
        topo_type = 19 
        return 
      endif ! Type  9b 

    endif

  endif
 
  if(Q == p2/3.0) then
    if(R == ra) then

      if(P > 0.0) then 
        topo_type = 5 
        return 
      endif ! Type  3a 

      if(P < 0.0) then 
        topo_type = 6 
        return 
      endif ! Type  3b 

    endif
  endif

  if(Q > 0.0 .and. Q < p2/4.0) then

    if(R == 0.0) then 

      if(P > 0.0) then 
        topo_type = 7 
        return 
      endif ! Type  4a 

      if(P < 0.0) then 
        topo_type = 8 
        return 
      endif ! Type  4b 

    endif
  endif

  if(Q == p2/4.0) then

    if(R == 0.0) then

      if(P > 0.0) then 
        topo_type = 9 
        return 
      endif ! Type  5a 

      if(P < 0.0) then 
        topo_type = 10 
        return 
      endif ! Type  5b 

    endif
  endif

  if(Q < p2/4.0) then

    if(P >= 0.0) then

      if(R > ra .and. rr < 0.0) then 
        topo_type = 11 
        return 
      endif ! Type  6a 
      
      if(R == ra) then 
        topo_type = 13 
        return 
      endif ! Type  7a 

      if(R < ra) then 
        topo_type = 18 
        return 
      endif ! Type  9a 

    endif

    if(P <= 0.0) then

      if(R > 0.0 .and. rr < rb) then 
        topo_type = 12 
        return 
      endif ! Type  6b 

      if(R == rb) then 
        topo_type = 14 
        return 
      endif ! Type  7b 

      if(R > rb) then 
        topo_type = 21 
        return 
      endif ! Type  10b 

    endif
  endif

  if(Q < 0.0) then

    if(R == 0.0) then 
      topo_type = 17 
      return 
    endif ! Type  8c 

    if(P < 0.0) then

      if(R < 0.0 .and. rr > ra) then 
        topo_type = 11 
        return 
      endif ! Type  6a 

      if(R == ra) then 
        topo_type = 13 
        return 
      endif ! Type  7a 

      if(R < ra) then 
        topo_type = 18 
        return 
      endif ! Type  9a 

    endif

    if(P > 0.0) then

      if(R > 0.0 .and. rr < rb) then 
        topo_type = 12 
        return 
      endif ! Type  6b 

      if(R == rb) then 
        topo_type = 14 
        return 
      endif ! Type  7b 

      if(R > ra) then 
        topo_type = 21 
        return 
      endif ! Type  10b 

    endif
  endif

  if(Q == 0.0 .and. R == 0.0) then

    if(P > 0.0) then 
      topo_type = 15 
      return 
    endif ! Type  8a 

    if(P < 0.0) then 
      topo_type = 16 
      return 
    endif ! Type  8b 

  endif

  if(Q > p2/4.0) then

    if(P >= 0.0 .and. R < 0.0) then 
      topo_type = 18 
      return 
    endif ! Type  9a 

    if(P <= 0.0 .and. R > 0.0) then 
      topo_type = 21 
      return 
    endif ! Type  10b 

    if(R == 0.0) then
      if(P > 0.0) then 
        topo_type = 22 
        return 
      endif ! Type  11a 

      if(P < 0.0) then 
        topo_type = 23 
        return 
      endif ! Type  11b 

    endif
  endif

  if(Q > 0.0) then
    if(P < 0.0) then

      if(R < P*Q) then 
        topo_type = 18 
        return 
      endif ! Type  9a 

      if(R == P*Q) then 
        topo_type = 24 
        return 
      endif ! Type  12a 

    endif

    if(P > 0.0) then

      if(R > P*Q) then 
        topo_type = 21 
        return 
      endif ! Type  10b 

      if(R == P*Q) then 
        topo_type = 25 
        return 
      endif ! Type  12b 
      
    endif

    if(P == 0.0 .and. R == 0.0) then 
      topo_type = 26 
      return 
    endif ! Type  12c 
    
  endif

  if(Q > p2/3.0) then

    if(P < 0.0) then

      if(R < 0.0 .and. rr > P*Q) then 
        topo_type = 19 
        return 
      endif ! Type  9b 

    endif

    if(P > 0.0) then

      if(R > 0.0 .and. rr < P*Q) then 
        topo_type = 20 
        return 
      endif ! Type  10a 

    endif
  endif 

  return
  END FUNCTION

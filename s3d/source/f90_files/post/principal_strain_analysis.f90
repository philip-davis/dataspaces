#include "globalDefines.h"
! ==================================================================================
! Routines for computing turbulent pricipal strains
! Author - Ray Grout (2008)
!
! Usage:
! - Requires s3d_hdf5_interface to be initialized already and have written basic 
! hdf5 file 
! - Needs lapack library linked in
! ==================================================================================
SUBROUTINE strain_scalar_gradient(io)
  use variables_m, only : q, u, temp, pressure, yspecies
  use hdf5
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g
  use s3d_hdf_interface_m
  use reference_m
  use mixfrac_m
  IMPLICIT NONE

  INTEGER io
  
  INTEGER NXF,NYF,NZF, NXF_G, NYF_G, NZF_G

  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: filteredfield

    !----------------------------------------------------------------------
    ! hdf declarations


    CHARACTER(LEN=20) :: groupname  ! Group name
    CHARACTER(LEN=20) :: fieldname  ! Field name
    CHARACTER(LEN=250) :: fielddesc  ! Very long field name
    CHARACTER(LEN=20) :: fieldunits  ! Field units
    CHARACTER(LEN=50) :: filename   ! File name

    LOGICAL :: overwrite
    INTEGER :: error

    ! Storage for principal strains
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: principalstrains_mag
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: principalstrains_dir
    real, dimension(nx,ny,nz,6) :: eij
    integer i, j, k, l
    real, dimension(nx,ny,nz) :: cosgamma, scratch
    real, dimension(nx,ny,nz, 3) :: g ! scalar gradient
    real, dimension(3) :: np

    ! Stuff to compute:
    real, dimension(nx,ny,nz) :: cos1 ! cos angle between scalar gradient and alpha strain

    real, dimension(nx,ny,nz) :: cos2 ! cos angle between projection of scalar
                                      ! gradient into gamma-beta plane and gamma strain

    real, dimension(nx,ny,nz) :: maggeg ! turbulence-scalar interaction term
                                        ! normalized by magnitude of scalar gradient (squared)

    real, dimension(nx,ny,nz) :: magg  ! magnitude of scalar gradient

    ALLOCATE(principalstrains_mag(nx,ny,nz,3))
    ALLOCATE(principalstrains_dir(nx,ny,nz,3,3))

    ! GET PRINCIPAL STRAINS  ===========================================================================
    CALL CALC_PRINCIPAL_STRAINS( io, principalstrains_mag, principalstrains_dir, eij) 
    cosgamma = 0.0

    call specToMixfr(yspecies)

    ! resuse principalstrains_mag, and make sure directions are normalized
    principalstrains_mag = 0.0
    do l=1,3

      principalstrains_mag(:,:,:,l) = principalstrains_dir(:,:,:,l,1)*principalstrains_dir(:,:,:,l,1) &
      + principalstrains_dir(:,:,:,l,2)*principalstrains_dir(:,:,:,l,2) &
      + principalstrains_dir(:,:,:,l,3)*principalstrains_dir(:,:,:,l,3)

      do k=1,nz
        do j=1,ny
          do i=1,nx
            principalstrains_dir(i,j,k,l,:) = principalstrains_dir(i,j,k,l,:) / sqrt(principalstrains_mag(i,j,k,l))
          enddo
        enddo
      enddo

    enddo



    call derivative_x(nx,ny,nz,mixfrac,g(:,:,:,1),scale_1x,1)
    magg =   g(:,:,:,1)*g(:,:,:,1)
    cos1 = principalstrains_dir(:,:,:,1,1)*g(:,:,:,2)

    call derivative_y(nx,ny,nz,mixfrac,g(:,:,:,2),scale_1y,1)
    magg = magg + g(:,:,:,2)*g(:,:,:,2)
    cos1 = cos1 + principalstrains_dir(:,:,:,1,2)*g(:,:,:,2)
                                    
    call derivative_z(nx,ny,nz,mixfrac,scratch,scale_1z,1)
    magg = magg + g(:,:,:,3)*g(:,:,:,3)
    cos1 = cos1 + principalstrains_dir(:,:,:,1,3)*g(:,:,:,3)


    ! at the start of this loop, cos1 contains the projection of g on n_alpha
    do k=1,nz
      do j=1,ny
        do i=1,nx
          magg(i,j,k) = sqrt( magg(i,j,k) )
          ! first np = dot(g, n_alpha) * n_alpha = cos1 * n_alpha
          np = cos1(i,j,k) * principalstrains_dir(i,j,k,1,:)

          ! now subtract np from g
          np = g(i,j,k,:) - np

          ! normalize np
          np = np / sqrt( np(1)*np(1) + np(2)*np(2) + np(3)*np(3) )

          ! finally cos2 = abs( dot(np, n_gamma) )
          cos2(i,j,k) = abs(  np(1) * principalstrains_dir(i,j,k,3,1) &
                            + np(2) * principalstrains_dir(i,j,k,3,2) & 
                            + np(3) * principalstrains_dir(i,j,k,3,3)   )

          ! finish with cos1 by normalizing and taking absolute value, since the
          ! eigenvector could be rotated by pi
          cos1(i,j,k) = abs( cos1(i,j,k) ) / magg(i,j,k)

          ! geg term
          maggeg(i,j,k) = g(i,j,k,1)* ( g(i,j,k,1)*eij(i,j,k,1) + g(i,j,k,2)*eij(i,j,k,4) + g(i,j,k,3)*eij(i,j,k,5) ) &
                        + g(i,j,k,2)* ( g(i,j,k,1)*eij(i,j,k,4) + g(i,j,k,2)*eij(i,j,k,2) + g(i,j,k,3)*eij(i,j,k,6) ) &
                        + g(i,j,k,3)* ( g(i,j,k,1)*eij(i,j,k,5) + g(i,j,k,2)*eij(i,j,k,6) + g(i,j,k,3)*eij(i,j,k,3) ) 
          maggeg(i,j,k) = maggeg(i,j,k) / ( magg(i,j,k) * magg(i,j,k) )


        enddo 
      enddo
    enddo


    ! FOR OUTPUT ON SAME GRID AS INPUT ONLY
      filename = write_hdf5_param%filename 
      groupname = "/derived"
      fieldname = "cos1"
      fielddesc = " dot(g,n_alpha)/magg"
      fieldunits = ""
      overwrite = .false.
      CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, cos1, 1.0d0, error,overwrite)
      
      fieldname = "cos2"
      fielddesc = " dot(np, n_gamma )"
      CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, cos2, 1.0d0, error,overwrite)
    
      fieldname = "maggeg"
      fielddesc = " Magnitude of turbulence-strain term"
      CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, maggeg, 1.0d0, error,overwrite)

      !fieldname = "gamma_strain_z"
      !fielddesc = " Gamma strain direction, z"
      !CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, principalstrains_dir(:,:,:,3,3), 1.0d0, error,overwrite)
    

      DEALLOCATE(principalstrains_mag)
      DEALLOCATE(principalstrains_dir)

    END SUBROUTINE STRAIN_SCALAR_GRADIENT


SUBROUTINE strain_analysis(io)
  use variables_m, only : q, u, temp, pressure, yspecies
  use hdf5
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g
  use s3d_hdf_interface_m
  use reference_m
  use mixfrac_m
  IMPLICIT NONE

  INTEGER io
  
  INTEGER NXF,NYF,NZF, NXF_G, NYF_G, NZF_G

  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: filteredfield

    !----------------------------------------------------------------------
    ! hdf declarations


    CHARACTER(LEN=20) :: groupname  ! Group name
    CHARACTER(LEN=20) :: fieldname  ! Field name
    CHARACTER(LEN=250) :: fielddesc  ! Very long field name
    CHARACTER(LEN=20) :: fieldunits  ! Field units
    CHARACTER(LEN=50) :: filename   ! File name

    LOGICAL :: overwrite
    INTEGER :: error

    ! Storage for principal strains
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: principalstrains_mag
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: principalstrains_dir
    real, dimension(nx,ny,nz,6) :: eij
    integer i, j, k
    real, dimension(nx,ny,nz) :: cosgamma, scratch

    ALLOCATE(principalstrains_mag(nx,ny,nz,3))
    ALLOCATE(principalstrains_dir(nx,ny,nz,3,3))

    ! GET PRINCIPAL STRAINS  ===========================================================================
    CALL CALC_PRINCIPAL_STRAINS( io, principalstrains_mag, principalstrains_dir, eij) 
    cosgamma = 0.0

    call specToMixfr(yspecies)
    !  reuse principalstrans_mag here
    principalstrains_mag = 0.0
    principalstrains_mag(:,:,:,1) = principalstrains_dir(:,:,:,3,1)*principalstrains_dir(:,:,:,3,1) &
                                  + principalstrains_dir(:,:,:,3,2)*principalstrains_dir(:,:,:,3,2) &
                                  + principalstrains_dir(:,:,:,3,3)*principalstrains_dir(:,:,:,3,3)



    call derivative_x(nx,ny,nz,mixfrac,scratch,scale_1x,1)
    principalstrains_mag(:,:,:,2) = principalstrains_mag(:,:,:,2) +  &
                                    scratch*scratch
    cosgamma = principalstrains_dir(:,:,:,3,1)*scratch

    call derivative_y(nx,ny,nz,mixfrac,scratch,scale_1y,1)
    principalstrains_mag(:,:,:,2) = principalstrains_mag(:,:,:,2) +  &
                                    scratch*scratch
    cosgamma = cosgamma + principalstrains_dir(:,:,:,3,2)*scratch
                                    
    call derivative_z(nx,ny,nz,mixfrac,scratch,scale_1z,1)
    principalstrains_mag(:,:,:,2) = principalstrains_mag(:,:,:,2) +  &
                                    scratch*scratch
    cosgamma = cosgamma + principalstrains_dir(:,:,:,3,3)*scratch
    do k=1,nz
      do j=1,ny
        do i=1,nx
          cosgamma(i,j,k) = cosgamma(i,j,k)&
                            / sqrt( principalstrains_mag(i,j,k,1) ) &
                            / sqrt( principalstrains_mag(i,j,k,2) )
        enddo 
      enddo
    enddo


    ! FOR OUTPUT ON SAME GRID AS INPUT ONLY
      filename = write_hdf5_param%filename 
      groupname = "/derived"
      !fieldname = "gamma_strain"
      fieldname = "cosgamma"
      fielddesc = " Gamma strain magnitude"
      fieldunits = ""
      overwrite = .false.
      CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, cosgamma, 1.0d0, error,overwrite)
      
      !fieldname = "gamma_strain_x"
      !fielddesc = " Gamma strain direction, x"
      !CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, principalstrains_dir(:,:,:,3,1), 1.0d0, error,overwrite)
    
      !fieldname = "gamma_strain_y"
      !fielddesc = " Gamma strain direction, y"
      !CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, principalstrains_dir(:,:,:,3,2), 1.0d0, error,overwrite)

      !fieldname = "gamma_strain_z"
      !fielddesc = " Gamma strain direction, z"
      !CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, principalstrains_dir(:,:,:,3,3), 1.0d0, error,overwrite)
    

      DEALLOCATE(principalstrains_mag)
      DEALLOCATE(principalstrains_dir)

  END SUBROUTINE STRAIN_ANALYSIS



  SUBROUTINE CALC_PRINCIPAL_STRAINS( io, principalstrains_mag, principalstrains_dir, eij) 
  ! ==================================================================================================
  ! ROUTINE TO COMPUTE PRINCIPAL STRAINS FROM TURBULENT FIELD - RAY GROUT , APRIL 2008
  ! INPUT:
  ! ==================================================================================================
  use param_m, only: nx, ny, nz
  use grid_m
  use topology_m
  use variables_m, only :  u
  IMPLICIT NONE

  integer io
  real, dimension(nx,ny,nz,3) :: principalstrains_mag
  real, dimension(nx,ny,nz,3,3) :: principalstrains_dir
  real, dimension(nx,ny,nz) :: scratch
  real, dimension(nx,ny,nz,3) :: vort
  real, dimension(nx,ny,nz,6) :: eij
  real, dimension(3,3) :: A
  real, dimension(3) :: EVALS
  integer i,j,k

  integer info, lwork
  parameter(lwork=150)
  real, dimension(lwork) ::  WORK

  !  write(*,*) myid, 'called calc_principal_strains'

  ! Differentiate velocity fields to get Sij

  call derivative_x(nx,ny,nz,u(:,:,:,1),scratch,scale_1x,1)
  EIJ(:,:,:,1) = 2.0D0*scratch

  call derivative_y(nx,ny,nz,u(:,:,:,1),scratch,scale_1y,1)
  EIJ(:,:,:,4) = scratch
  VORT(:,:,:,3) = -scratch

  call derivative_z(nx,ny,nz,u(:,:,:,1),scratch,scale_1z,1)
  EIJ(:,:,:,5) = scratch
  VORT(:,:,:,2) = scratch

  call derivative_x(nx,ny,nz,u(:,:,:,2),scratch,scale_1x,1)
  EIJ(:,:,:,4) = EIJ(:,:,:,4) + scratch
  VORT(:,:,:,3) = VORT(:,:,:,3) + scratch     

  call derivative_y(nx,ny,nz,u(:,:,:,2),scratch,scale_1y,1)
  EIJ(:,:,:,2) = 2.0D0*scratch
 
  call derivative_z(nx,ny,nz,u(:,:,:,2),scratch,scale_1z,1)
  EIJ(:,:,:,6) = scratch
  VORT(:,:,:,1) = -scratch 

  call derivative_x(nx,ny,nz,u(:,:,:,3),scratch,scale_1x,1)
  EIJ(:,:,:,5) = EIJ(:,:,:,5) + scratch
  VORT(:,:,:,2) = VORT(:,:,:,3) - scratch

  call derivative_y(nx,ny,nz,u(:,:,:,3),scratch,scale_1y,1)
  EIJ(:,:,:,6) =  EIJ(:,:,:,6)  + scratch
  VORT(:,:,:,1) = VORT(:,:,:,1) + scratch

  call derivative_z(nx,ny,nz,u(:,:,:,3),scratch,scale_1z,1)
  EIJ(:,:,:,3) = 2.0D0*scratch



  !     Find eigenvalues to get principal strains and eigenvectors
  !     for principal directions
  do k=1, nz
     do j=1, ny
        do i = 1, nx
           A(1,1) = EIJ(i,j,k,1)
           A(2,2) = EIJ(i,j,k,2)
           A(3,3) = EIJ(i,j,k,3)

           A(2,1) = 0.0d0!EIJ(i,j,k,4)
           A(3,1) = 0.0d0!EIJ(i,j,k,5)

           A(1,2) = EIJ(i,j,k,4)
           A(3,2) = 0.0d0!EIJ(i,j,k,6)

           A(1,3) = EIJ(i,j,k,5)
           A(2,3) = EIJ(i,j,k,6)

           INFO = 0

!     SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
           CALL DSYEV( 'V', 'U', 3, A, 3, EVALS, WORK, LWORK, INFO )

      !if(i.eq.64.and.j.eq.32.and.k.eq.16) then
      !   write(*,*) ' Eigenvalues:',EVALS
      !   write(*,*) ' Eigenvector 1:',A(:,1)
      !   write(*,*) ' Eigenvector 2:',A(:,2)
      !   write(*,*) ' Eigenvector 3:',A(:,3)
      !endif

      IF(INFO.NE.0) THEN
         WRITE(0,*) 'INFO =',INFO
         WRITE(0,*) 'I,J,K=',I,J,K
         STOP
      ENDIF

      IF( EVALS(1).LE.EVALS(2) .AND. EVALS(2).LE.EVALS(3) ) THEN
         principalstrains_dir(i,j,k,1,:) = A(:,3) ! alpha strain
         principalstrains_dir(i,j,k,2,:) = A(:,2) ! beta strain
         principalstrains_dir(i,j,k,3,:) = A(:,1) ! gamma strain
         principalstrains_mag(i,j,k,1) = EVALS(3)
         principalstrains_mag(i,j,k,2) = EVALS(2)
         principalstrains_mag(i,j,k,3) = EVALS(1)
      ELSE
         WRITE(0,*) 'EIGENVALUES NOT SORTED CORRECTLY'
         WRITE(0,*) 'INFO=',INFO
         WRITE(0,*) 'EVALS=',EVALS 
         STOP
      ENDIF
      
   ENDDO
ENDDO
ENDDO




  END SUBROUTINE CALC_PRINCIPAL_STRAINS

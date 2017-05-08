#include "globalDefines.h"
!!$        0               ( I_vortex_pair = 1 to initialize vortex pair )
!!$        0.05            ( X_location, at which vortex pair to be introduced )
!!$        0.08            ( Separation distance between centers of vortices )
!!$        0.03            ( Sigma - radius of vortex )
!!$        1.5             ( Radius ratio at which to turn off vortex )
!!$        0.06            ( Peak velocity induced by each vortex in isolation )


subroutine initialize_vortex( q, u, io )
  use param_m
  use topology_m
  use reference_m, only : l_ref, a_ref

  implicit none

  real, intent(inout), dimension(nx,ny,nz,nvar_tot,n_reg) :: q
  real, intent(inout), dimension(nx,ny,nz,3) :: u
  integer, intent(in) :: io

  character*100 :: filename

  real :: x_location    ! position to introduce vortex pair
  real :: separation    ! separation distance between vortex centers
  real :: sigma         ! radius of vortex
  real :: r_ratio       ! radius ratio at which vortices are shut off
  real :: u_max         ! peak velocity induced by each vortex in isolation

  integer :: vortex_type  ! vortex_type selects vortex type. 1=old hat, 2=new hat, 3=Oseen

  integer :: i

! read the vortex.in file and set parameters
  filename = '../input/vortex.in'
  call inquire_about_input_file(filename,io)

  if (myid==0) then
     open( unit=20,file=filename,status='old',form='formatted' )
     read(20,*) vortex_type
     read(20,*) x_location
     read(20,*) separation
     read(20,*) sigma
     read(20,*) r_ratio
     read(20,*) u_max
     close(20)

   ! non-dimensionalize input
	 x_location = x_location/l_ref*0.01
	 separation = separation/l_ref*0.01
	 sigma = sigma/l_ref*0.01
	 u_max = u_max/a_ref*0.01

  endif

  call MPI_Bcast( vortex_type, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast( x_location,  1, MPI_REAL8,   0, gcomm, ierr )
  call MPI_Bcast( separation,  1, MPI_REAL8,   0, gcomm, ierr )
  call MPI_Bcast( sigma,       1, MPI_REAL8,   0, gcomm, ierr )
  call MPI_Bcast( r_ratio,     1, MPI_REAL8,   0, gcomm, ierr )
  call MPI_Bcast( u_max,       1, MPI_REAL8,   0, gcomm, ierr )

! set the velocity field induced by the vortices
  call vtxpar( u, x_location, separation, sigma, r_ratio, u_max, vortex_type, io )

!----------------------------------------------------------
! superimpose this velocity field onto the existing fields
!----------------------------------------------------------

!--- momentum field
  do i=1,3
     q(:,:,:,i,1) = q(:,:,:,i,1) + q(:,:,:,4,1)*u(:,:,:,i)
  enddo

!--- energy field (add in kinetic energy contribution from vortices)
  q(:,:,:,5,1) = q(:,:,:,5,1) + 0.5*q(:,:,:,4,1) * (                  &
       u(:,:,:,1)*u(:,:,:,1) +                              &
       u(:,:,:,2)*u(:,:,:,2) +                              &
       u(:,:,:,3)*u(:,:,:,3) )

  return
end subroutine initialize_vortex


!!$==================================================================================


subroutine vtxpar ( velo, x_location, separation, sigma, r_ratio, u_max, vortex_type, io )
  !---------------------------------------------------------------------------
  !! VTXPAR handles vortex pairs.
  !
  ! Top vortex is counterclockwise, bottom vortex is clockwise for positive
  ! u_max. The velocity induced by the second is simply added to that induced
  ! by the first. The second will induce velocity only in its neighborhood.
  ! The pressure correction is approximate as the first vortex is not
  ! accounted for. Could require cleanup.
  !---------------------------------------------------------------------------
  use param_m, only : nx,ny,nz
  use topology_m
  use grid_m, only : xmin, xmax, ymin, ymax, zmin, zmax, x, y, z

  implicit none

  real, parameter :: pi = 3.14159265358979
  real, parameter :: eps = 1.0e-30

  real, intent(inout) :: velo(nx,ny,nz,3)
  real, intent(in)    :: x_location, separation, sigma, r_ratio, u_max
  integer, intent(in) :: vortex_type, io

! Local variables:

  integer :: i, j, k, ivar
  real :: y_location, z_location, psi, sigma2
  real :: x_loc, y_loc, z_loc, r_loc, a_coefft

! Initialize velocity field

  velo = 0.0

  sigma2 = sigma*sigma

  select case ( vortex_type )

  case(1)
     !=====================
     ! Original vortex hat:
     !=====================

     a_coefft = sigma * u_max/sqrt(2.)/exp(-0.5)
     y_location = ymin + 0.5*abs(ymax-ymin) + separation*0.5
     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              y_loc = y(j) - y_location
              r_loc = sqrt( x_loc*x_loc + y_loc*y_loc )
              if ( r_loc/sigma .lt. r_ratio ) then
                 psi = a_coefft * exp (-r_loc*r_loc/sigma2)
                 velo(i,j,k,1) = - 2. * y_loc * psi/sigma2
                 velo(i,j,k,2) =   2. * x_loc * psi/sigma2
              else
                 velo(i,j,k,1) = 0.
                 velo(i,j,k,2) = 0.
              end if
           end do
        end do
     end do

     a_coefft = - sigma * u_max/sqrt(2.)/exp(-0.5)
     y_location = ymin + 0.5*abs(ymax-ymin) - separation*0.5

     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              y_loc = y(j) - y_location
              r_loc = sqrt( x_loc*x_loc + y_loc*y_loc )
              if ( r_loc/sigma .lt. r_ratio ) then
                 psi = a_coefft * exp (-r_loc*r_loc/sigma2)
                 velo(i,j,k,1) = - 2. * y_loc * psi/sigma2 + velo(i,j,k,1)
                 velo(i,j,k,2) =   2. * x_loc * psi/sigma2 + velo(i,j,k,2)
              end if
           end do
        end do
     end do

  case(2)
     !====================
     ! Recoded vortex hat:
     !====================

     a_coefft = sigma * u_max/exp(-0.5)
     y_location = ymin + 0.5*abs(ymax-ymin) + separation*0.5
     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              y_loc = y(j) - y_location
              r_loc = sqrt( x_loc**2 + y_loc**2 )
              psi = a_coefft * exp (-0.5*r_loc**2/sigma2)
              velo(i,j,k,1) = - y_loc * psi/sigma2
              velo(i,j,k,2) =   x_loc * psi/sigma2
           end do
        end do
     end do
     a_coefft = - sigma * u_max/exp(-0.5)
     y_location = ymin + 0.5*abs(ymax-ymin) - separation*0.5
     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              y_loc = y(j) - y_location
              r_loc = sqrt( x_loc**2 + y_loc**2 )
              psi = a_coefft * exp (-0.5*r_loc**2/sigma2)
              velo(i,j,k,1) = - y_loc * psi/sigma2 + velo(i,j,k,1)
              velo(i,j,k,2) =   x_loc * psi/sigma2 + velo(i,j,k,2)
           end do
        end do
     end do

  case(3)
     !==============
     ! Oseen vortex:
     !==============

     psi =   u_max * sigma / 1.0156833757708d-01
     y_location = ymin + 0.5*abs(ymax-ymin) + separation*0.5
     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              y_loc = y(j) - y_location
              r_loc = sqrt( x_loc**2 + y_loc**2 ) + eps
              a_coefft = 1.-exp(-r_loc**2/sigma2)
              velo(i,j,k,1) = - y_loc/r_loc * psi/(2.*pi*r_loc)*a_coefft
              velo(i,j,k,2) =   x_loc/r_loc * psi/(2.*pi*r_loc)*a_coefft
           end do
        end do
     end do

     psi = - psi
     y_location = ymin + 0.5*abs(ymax-ymin) - separation*0.5

     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              y_loc = y(j) - y_location
              r_loc = sqrt( x_loc**2 + y_loc**2 ) + eps
              a_coefft = 1.-exp(-r_loc**2/sigma2)
              velo(i,j,k,1) = velo(i,j,k,1) - y_loc/r_loc * psi/(2.*pi*r_loc)*a_coefft
              velo(i,j,k,2) = velo(i,j,k,2) + x_loc/r_loc * psi/(2.*pi*r_loc)*a_coefft
           end do
        end do
     end do

  case(9)
     !===================================================
     ! Modified Oseen vortex (Shorter tail) in X-Y plane:
     !     Hong Im, Jan. 97.
     !===================================================

     psi =   u_max * sigma2 / 0.1017
     y_location = ymin + 0.5*abs(ymax-ymin) + separation*0.5
     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              y_loc = y(j) - y_location
              r_loc = sqrt( x_loc**2 + y_loc**2 ) + eps
              a_coefft = 1.-exp(-(r_loc/sigma)**3.)
              velo(i,j,k,1) = -y_loc/r_loc*psi/(2.*pi*r_loc**2)*a_coefft
              velo(i,j,k,2) =  x_loc/r_loc*psi/(2.*pi*r_loc**2)*a_coefft
           end do
        end do
     end do

     psi = - psi
     y_location = ymin + 0.5*abs(ymax-ymin) - separation*0.5
     do k = 1, nz 
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              y_loc = y(j) - y_location
              r_loc = sqrt( x_loc**2 + y_loc**2 ) + eps
              a_coefft = 1.-exp(-(r_loc/sigma)**3.)
              velo(i,j,k,1) = velo(i,j,k,1) - y_loc/r_loc * psi/(2.*pi*r_loc**2)*a_coefft
              velo(i,j,k,2) = velo(i,j,k,2) + x_loc/r_loc * psi/(2.*pi*r_loc**2)*a_coefft
           end do
        end do
     end do


  case(10)
     !==================================================================
     ! Modified Oseen vortex (Shorter tail) in X-Z plane (3D code test):
     !     Hong Im, Aug. 99.
     !==================================================================

     psi =   u_max * sigma2 / 0.1017
     z_location = zmin + 0.5*abs(zmax-zmin) + separation*0.5
     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              z_loc = z(k) - z_location
              r_loc = sqrt( x_loc**2 + z_loc**2 ) + eps
              a_coefft = 1.-exp(-(r_loc/sigma)**3.)
              velo(i,j,k,1) = -z_loc/r_loc*psi/(2.*pi*r_loc**2)*a_coefft
              velo(i,j,k,3) =  x_loc/r_loc*psi/(2.*pi*r_loc**2)*a_coefft
           end do
        end do
     end do

     psi = - psi
     z_location = zmin + 0.5*abs(zmax-zmin) - separation*0.5
     do k = 1, nz
        do j = 1, ny
           do i = 1, nx
              x_loc = x(i) - x_location
              z_loc = z(k) - z_location
              r_loc = sqrt( x_loc**2 + z_loc**2 ) + eps
              a_coefft = 1.-exp(-(r_loc/sigma)**3.)
              velo(i,j,k,1) = velo(i,j,k,1) - z_loc/r_loc * psi/(2.*pi*r_loc**2)*a_coefft
              velo(i,j,k,3) = velo(i,j,k,3) + x_loc/r_loc * psi/(2.*pi*r_loc**2)*a_coefft
           end do
        end do
     end do

  case default

     if (myid.eq.0) then
        write ( io, 9010 ) vortex_type
     end if
     call MPI_Finalize(ierr)
     stop

  end select

9010 format(' error: wrong specification of i_vortex_pair:',i8)

  return
end subroutine vtxpar

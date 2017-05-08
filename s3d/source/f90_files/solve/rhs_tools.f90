#include "globalDefines.h"
! Modified by Ramanan - 04/08/05
! Decouple x, y and z communications. 
subroutine computeScalarGradient( phi, grad_phi )
  !-----------------------------------------------------------------------------
  ! computes the gradient of a scalar quantity phi.
  !-----------------------------------------------------------------------------
  use param_m, only : nx,ny,nz
  use grid_m, only : scale_1x, scale_1y, scale_1z
  implicit none
  real, dimension(nx,ny,nz),   intent(in)  :: phi
  real, dimension(nx,ny,nz,3), intent(out) :: grad_phi

  integer, dimension(4,3) :: req

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_x_comm( nx,ny,nz, phi, req(:,1))
  call derivative_y_comm( nx,ny,nz, phi, req(:,2))
  call derivative_z_comm( nx,ny,nz, phi, req(:,3))

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_x_calc( nx,ny,nz, phi, grad_phi(:,:,:,1), scale_1x, 1, req(:,1))
  call derivative_y_calc( nx,ny,nz, phi, grad_phi(:,:,:,2), scale_1y, 1, req(:,2))
  call derivative_z_calc( nx,ny,nz, phi, grad_phi(:,:,:,3), scale_1z, 1, req(:,3))

  return
end subroutine computeScalarGradient

!!$=============================================================================

!!!subroutine computeScalarGradient5d_boxlib( phi, grad_phi, n4, phimfab ) 
!!!  !-----------------------------------------------------------------------------
!!!  ! computes the gradient of an array of scalar quantities phi. Put the result
!!!  ! into the selected 4D slice of a 5D array. 
!!!  !-----------------------------------------------------------------------------
!!!  use param_m, only : nx,ny,nz, iorder
!!!  use grid_m, only : scale_1x, scale_1y, scale_1z
!!!  implicit none
!!!  integer :: n4, i4     ! bounds and index in 4th dimension (both might be 1)
!!!  real, intent(in) :: phi(1-iorder/2:nx+iorder/2, &
!!!                          1-iorder/2:ny+iorder/2, &
!!!                          1-iorder/2:nz+iorder/2, *) 
!!!
!!!  real, dimension(nx,ny,nz,n4,3), intent(out) :: grad_phi
!!!
!!!  ! Pointer to multifab object - need this to trigger boxlib fill of halo
!!!  integer*8, intent(in) :: phimfab
!!!
!!!  ! Number of components in multifab
!!!  integer :: nphi
!!!  integer :: ip
!!!
!!!
!!!  ! Fill boundaries
!!!  call do_boxlib_fill( phimfab )
!!!
!!!  !call get_mfab_components( phimfab, nphi )
!!!  nphi = n4
!!!
!!!  ! Compute the derivatives
!!!  do ip = 1, nphi
!!!      call derivative_x_calc_boxlib( nx,ny,nz, phi(:,:,:,ip), grad_phi(:,:,:,ip,1) )
!!!      call derivative_y_calc_boxlib( nx,ny,nz, phi(:,:,:,ip), grad_phi(:,:,:,ip,2) )
!!!      call derivative_z_calc_boxlib( nx,ny,nz, phi(:,:,:,ip), grad_phi(:,:,:,ip,3) )
!!!  enddo
!!!
!!!  return
!!!end subroutine computeScalarGradient5d_boxlib

subroutine computeScalarGradient5d( phi, grad_phi, n4, i4) ! johnmc new routine
  !-----------------------------------------------------------------------------
  ! computes the gradient of a scalar quantity phi. put the result
  ! into the selected 4D slice of a 5D array. 
  !-----------------------------------------------------------------------------
  use param_m, only : nx,ny,nz
  use grid_m, only : scale_1x, scale_1y, scale_1z
  implicit none
  integer :: n4, i4     ! bounds and index in 4th dimension (both might be 1)
  real, dimension(nx,ny,nz),   intent(in)  :: phi
  real, dimension(nx,ny,nz,n4,3), intent(out) :: grad_phi

  integer, dimension(4,3) :: req

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_x_comm( nx,ny,nz, phi, req(:,1))
  call derivative_y_comm( nx,ny,nz, phi, req(:,2))
  call derivative_z_comm( nx,ny,nz, phi, req(:,3))

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_x_calc( nx,ny,nz, phi, grad_phi(:,:,:,i4,1), scale_1x, 1, req(:,1))
  call derivative_y_calc( nx,ny,nz, phi, grad_phi(:,:,:,i4,2), scale_1y, 1, req(:,2))
  call derivative_z_calc( nx,ny,nz, phi, grad_phi(:,:,:,i4,3), scale_1z, 1, req(:,3))

  return
end subroutine computeScalarGradient5d



!!$=============================================================================
subroutine computeVectorGradient( u, grad_u )
  !-----------------------------------------------------------------------------
  ! computes the gradient of a vector (which is a tensor)
  !
  !               /                             \
  !               | du1/dx1   du2/dx1   du3/dx1 |
  !               |                             |
  !     grad(u) = | du1/dx2   du2/dx2   du3/dx2 |
  !               |                             |
  !               | du1/dx3   du2/dx3   du3/dx3 |
  !               \                             /
  !-----------------------------------------------------------------------------
  use param_m, only : nx,ny,nz
  use grid_m, only : scale_1x, scale_1y, scale_1z
  implicit none
  real, intent(in), dimension(nx,ny,nz,3) :: u
  real, intent(out), dimension(nx,ny,nz,3,3) :: grad_u
  integer :: i

  do i=1,3
     call computeScalarGradient( u(:,:,:,i), grad_u(:,:,:,:,i) )
  enddo
  
  return
end subroutine computeVectorGradient



!!$=============================================================================



subroutine computeDivergence( f, div_f )
  !-----------------------------------------------------------------------------
  ! computes the divergence of a vector f = (f1, f2, f3) where
  !
  ! div(f) = d(f1)/dx + d(f2)/dy + d(f3)/dz
  !
  ! f      - vector function being differentiatied
  ! div_f  - divergence of vector f
  !-----------------------------------------------------------------------------
  use param_m, only : nx,ny,nz, vary_in_x, vary_in_y, vary_in_z
  use grid_m, only : scale_1x, scale_1y, scale_1z
  implicit none
  real, intent(in),  dimension(nx,ny,nz,3) :: f(nx,ny,nz,3)
  real, intent(out), dimension(nx,ny,nz)   :: div_f

  real, dimension(nx,ny,nz) :: tmp

  if (vary_in_x == 1) then
     call derivative_x(nx, ny, nz, f(:,:,:,1), div_f, scale_1x, 0)
  else
     div_f = 0.0
  endif

  if (vary_in_y == 1) then
     call derivative_y(nx, ny, nz, f(:,:,:,2), tmp, scale_1y, 0)
     div_f = div_f + tmp
  endif

  if (vary_in_z == 1) then
     call derivative_z(nx, ny, nz, f(:,:,:,3), tmp, scale_1z, 0)
     div_f = div_f + tmp
  endif

  return
end subroutine computeDivergence


!!$=============================================================================

#include "globalDefines.h"
! Modified by Ramanan - 04/08/05
! Decouple x, y and z communications. 
subroutine computeScalarGradient_boxlib( phi, grad_phi, phimfab )
  !-----------------------------------------------------------------------------
  ! computes the gradient of a scalar quantity phi.
  !-----------------------------------------------------------------------------
#ifdef F03INTERLANG
  use iso_c_binding
#endif
  use param_m, only : nx,ny,nz, iorder
  use grid_m, only : scale_1x, scale_1y, scale_1z
  implicit none
  real, intent(in) :: phi(1-iorder/2:nx+iorder/2, &
                          1-iorder/2:ny+iorder/2, &
                          1-iorder/2:nz+iorder/2) 

  real, dimension(nx,ny,nz,3), intent(out) :: grad_phi

  ! Pointer to multifab object - need this to trigger boxlib fill of halo
#ifdef F03INTERLANG
  type(c_ptr), intent(in) :: phimfab
#else
  integer*8, intent(in) :: phimfab
#endif


  integer :: ip

  ! Fill boundaries
  call do_boxlib_fill( phimfab )


  ! Compute the derivatives
  call derivative_x_calc_boxlib( nx,ny,nz, phi, grad_phi(:,:,:,1) )
  call derivative_y_calc_boxlib( nx,ny,nz, phi(:,:,:), grad_phi(:,:,:,2) )
  call derivative_z_calc_boxlib( nx,ny,nz, phi(:,:,:), grad_phi(:,:,:,3) )

  return
end subroutine computeScalarGradient_boxlib

!!$=============================================================================

subroutine computeScalarGradient5d_boxlib( phi, grad_phi, n4, phimfab ) 
  !-----------------------------------------------------------------------------
  ! computes the gradient of an array of scalar quantities phi. Put the result
  ! into the selected 4D slice of a 5D array. 
  !-----------------------------------------------------------------------------
#ifdef F03INTERLANG
  use iso_c_binding
#endif
  use param_m, only : nx,ny,nz, iorder
  use grid_m, only : scale_1x, scale_1y, scale_1z
  implicit none
  integer :: n4, i4     ! bounds and index in 4th dimension (both might be 1)
  real, intent(in) :: phi(1-iorder/2:nx+iorder/2, &
                          1-iorder/2:ny+iorder/2, &
                          1-iorder/2:nz+iorder/2, *) 

  real, dimension(nx,ny,nz,n4,3), intent(out) :: grad_phi

  ! Pointer to multifab object - need this to trigger boxlib fill of halo
#ifdef F03INTERLANG
  type(c_ptr), intent(in) :: phimfab
#else
  integer*8, intent(in) :: phimfab
#endif


  ! Number of components in multifab
  integer :: ip

  call do_boxlib_fill( phimfab )

  ! Compute the derivatives
  do ip=1,n4
      call derivative_x_calc_boxlib( nx,ny,nz, phi(:,:,:,ip), grad_phi(:,:,:,ip,1) )
      call derivative_y_calc_boxlib( nx,ny,nz, phi(:,:,:,ip), grad_phi(:,:,:,ip,2) )
      call derivative_z_calc_boxlib( nx,ny,nz, phi(:,:,:,ip), grad_phi(:,:,:,ip,3) )
  enddo

  return
end subroutine computeScalarGradient5d_boxlib


!!$=============================================================================
subroutine computeVectorGradient_boxlib( u, grad_u, umfab )
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
#ifdef F03INTERLANG
  use iso_c_binding
#endif
  use param_m, only : nx,ny,nz, iorder
  use grid_m, only : scale_1x, scale_1y, scale_1z
  implicit none
  real, intent(in) :: u  (1-iorder/2:nx+iorder/2, &
                          1-iorder/2:ny+iorder/2, &
                          1-iorder/2:nz+iorder/2, 3) 
  real, intent(out), dimension(nx,ny,nz,3,3) :: grad_u
  integer :: ip

  ! Pointer to multifab object - need this to trigger boxlib fill of halo
#ifdef F03INTERLANG
  type(c_ptr), intent(in) :: umfab
#else
  integer*8, intent(in) :: umfab
#endif

  ! Fill boundaries
  call do_boxlib_fill( umfab )

  do ip = 1, 3
      call derivative_x_calc_boxlib( nx,ny,nz, u(:,:,:,ip), grad_u(:,:,:,1,ip) )
      call derivative_y_calc_boxlib( nx,ny,nz, u(:,:,:,ip), grad_u(:,:,:,2,ip) )
      call derivative_z_calc_boxlib( nx,ny,nz, u(:,:,:,ip), grad_u(:,:,:,3,ip) )
  enddo
  
  return

end subroutine computeVectorGradient_boxlib




#include "globalDefines.h"

subroutine compute_L_x( u, a, rho, du_dx, dv_dx, dw_dx, dp_dx, dYs_dx, drho_dx, L )
  !-----------------------------------------------------------------------------
  !                         Author: James Sutherland
  !                         Date:   May, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Given the derivatives of primitive variables, this routine computes the L's
  ! in the X-DIRECTION for an entire Y-Z plane
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! L1 = (u-a)/2 * [ dp/dx - rho*a*du/dx ]
  ! L2 = u/a^2 * [ a^2 * drho/dx - dp/dx ]
  ! L3 = u * dv/dx
  ! L4 = u * dw/dx
  ! L5 = (u+a)/2 * [ dp/dx + rho*a*du/dx]
  ! L5+i = u * dYi/dx
  !-----------------------------------------------------------------------------
  use param_m, only : ny,nz, nvar_tot, n_spec
  implicit none
  real, intent(in), dimension(ny,nz) :: u, a, rho, du_dx, dv_dx, dw_dx, dp_dx, drho_dx
  real, intent(in), dimension(ny,nz,n_spec) :: dYs_dx
  real, intent(out), dimension(ny,nz,nvar_tot+1) :: L

  integer :: n

  L(:,:,1) = 0.5*(u-a) * ( dp_dx - rho*a*du_dx )
  L(:,:,2) = u * ( drho_dx - dp_dx/(a*a) )
  L(:,:,3) = u * dv_dx
  L(:,:,4) = u * dw_dx
  L(:,:,5) = 0.5*(u+a) * (dp_dx + rho*a*du_dx)
  do n=1,n_spec
     L(:,:,5+n) = u * dYs_dx(:,:,n)
  enddo
  return
end subroutine compute_L_x


!=========================================================================================


subroutine compute_L_y( v, a, rho, du_dy, dv_dy, dw_dy, dp_dy, dYs_dy, drho_dy, L )
  !-----------------------------------------------------------------------------
  !                         Author: James Sutherland
  !                         Date:   May, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Given the derivatives of primitive variables, this routine computes the L's
  ! in the Y-DIRECTION for an entire X-Z plane
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! L1 = (v-a)/2 * [ dp/dv - rho*a*dv/dy ]
  ! L2 = v/a^2 * [ a^2 * drho/dy - dp/dy ]
  ! L3 = v * du/dy
  ! L4 = v * dw/dy
  ! L5 = (v+a)/2 * [ dp/dy + rho*a*dv/dy]
  ! L5+i = v * dYi/dy
  !-----------------------------------------------------------------------------
  use param_m, only : nx,nz, nvar_tot, n_spec
  implicit none
  real, intent(in), dimension(nx,nz) :: v, a, rho, du_dy, dv_dy, dw_dy, dp_dy, drho_dy
  real, intent(in), dimension(nx,nz,n_spec) :: dYs_dy
  real, intent(out), dimension(nx,nz,nvar_tot+1) :: L

  integer :: n

  L(:,:,1) = 0.5*(v-a) * ( dp_dy - rho*a*dv_dy )
  L(:,:,2) = v * ( drho_dy - dp_dy/(a*a) )
  L(:,:,3) = v * du_dy
  L(:,:,4) = v * dw_dy
  L(:,:,5) = 0.5*(v+a) * (dp_dy + rho*a*dv_dy)
  do n=1,n_spec
     L(:,:,5+n) = v * dYs_dy(:,:,n)
  enddo
  return
end subroutine compute_L_y


!=========================================================================================


subroutine compute_L_z( w, a, rho, du_dz, dv_dz, dw_dz, dp_dz, dYs_dz, drho_dz, L )
  !-----------------------------------------------------------------------------
  !                         Author: James Sutherland
  !                         Date:   May, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Given the derivatives of primitive variables, this routine computes the L's
  ! in the Z-DIRECTION for an entire X-Y plane
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! L1 = (w-a)/2 * [ dp/dz - rho*a*dw/dz ]
  ! L2 = w/a^2 * [ a^2 * drho/dz - dp/dz ]
  ! L3 = w * du/dz
  ! L4 = w * dv/dz
  ! L5 = (w+a)/2 * [ dp/dz + rho*a*dw/dz]
  ! L5+i = w * dYi/dz
  !-----------------------------------------------------------------------------
  use param_m, only : nx,ny, nvar_tot, n_spec
  implicit none
  real, intent(in), dimension(nx,ny) :: w, a, rho, du_dz, dv_dz, dw_dz, dp_dz, drho_dz
  real, intent(in), dimension(nx,ny,n_spec) :: dYs_dz
  real, intent(out), dimension(nx,ny,nvar_tot+1) :: L

  integer :: n

  L(:,:,1) = 0.5*(w-a) * ( dp_dz - rho*a*dw_dz )
  L(:,:,2) = w * ( drho_dz - dp_dz/(a*a) )
  L(:,:,3) = w * du_dz
  L(:,:,4) = w * dv_dz
  L(:,:,5) = 0.5*(w+a) * (dp_dz + rho*a*dw_dz)
  do n=1,n_spec
     L(:,:,5+n) = w * dYs_dz(:,:,n)
  enddo
  return
end subroutine compute_L_z

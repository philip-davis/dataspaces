#include "globalDefines.h"
subroutine compute_d_x( L, a, rho, d )
  !-----------------------------------------------------------------------------
  !                         Author: James Sutherland
  !                         Date:   May, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Given the L's for the X-DIRECTION, this routine computes the d's.
  ! It operates on an entire Y-Z plane.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! d1 = 1/a^2 * [ a^2*L2 + (L5+L1) ]
  ! d2 = L5+L1
  ! d3 = (L5-L1)/(rho*a)
  ! d4 = L3
  ! d5 = L4
  ! d5+i = L5+i
  !-----------------------------------------------------------------------------
  use param_m, only : ny,nz, nvar_tot, n_spec
  implicit none
  real, intent(in),  dimension(ny,nz,nvar_tot+1) :: L
  real, intent(in),  dimension(ny,nz)            :: a, rho
  real, intent(out), dimension(ny,nz,nvar_tot+1) :: d
  integer :: n

  d(:,:,1) = L(:,:,2) + (L(:,:,5) + L(:,:,1))/(a*a)
  d(:,:,2) = L(:,:,5) + L(:,:,1)
  d(:,:,3) = (L(:,:,5) - L(:,:,1)) / (rho*a)
  d(:,:,4) = L(:,:,3)
  d(:,:,5) = L(:,:,4)
  do n=1,n_spec
     d(:,:,5+n) = L(:,:,5+n)
  enddo
  return
end subroutine compute_d_x


!=========================================================================================


subroutine compute_d_y( L, a, rho, d )
  !-----------------------------------------------------------------------------
  !                         Author: James Sutherland
  !                         Date:   May, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Given the L's for the Y-DIRECTION, this routine computes the d's.
  ! It operates on an entire X-Z plane.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! d1 = 1/a^2 * [ a^2*L2 + (L5+L1) ]
  ! d2 = L5+L1
  ! d3 = L3
  ! d4 = (L5-L1)/(rho*a)
  ! d5 = L4
  ! d5+i = L5+i
  !-----------------------------------------------------------------------------
  use param_m, only : nx,nz, nvar_tot, n_spec
  implicit none
  real, intent(in),  dimension(nx,nz,nvar_tot+1) :: L
  real, intent(in),  dimension(nx,nz)            :: a, rho
  real, intent(out), dimension(nx,nz,nvar_tot+1) :: d
  integer :: n

  d(:,:,1) = L(:,:,2) + (L(:,:,5) + L(:,:,1))/(a*a)
  d(:,:,2) = L(:,:,5) + L(:,:,1)
  d(:,:,3) = L(:,:,3)
  d(:,:,4) = (L(:,:,5) - L(:,:,1)) / (rho*a)
  d(:,:,5) = L(:,:,4)
  do n=1,n_spec
     d(:,:,5+n) = L(:,:,5+n)
  enddo
  return
end subroutine compute_d_y


!=========================================================================================


subroutine compute_d_z( L, a, rho, d )
  !-----------------------------------------------------------------------------
  !                         Author: James Sutherland
  !                         Date:   May, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Given the L's for the Z-DIRECTION, this routine computes the d's.
  ! It operates on an entire X-Y plane.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! d1 = 1/a^2 * [ a^2*L2 + 1/2 * (L5+L1) ]
  ! d2 = L5+L1
  ! d3 = L3
  ! d4 = L4
  ! d5 = (L5-L1)/(rho*a)
  ! d5+i = L5+i
  !-----------------------------------------------------------------------------
  use param_m, only : nx,ny, nvar_tot, n_spec
  implicit none
  real, intent(in),  dimension(nx,ny,nvar_tot+1) :: L
  real, intent(in),  dimension(nx,ny)            :: a, rho
  real, intent(out), dimension(nx,ny,nvar_tot+1) :: d
  integer :: n

  d(:,:,1) = L(:,:,2) + (L(:,:,5) + L(:,:,1))/(a*a)
  d(:,:,2) = L(:,:,5) + L(:,:,1)
  d(:,:,3) = L(:,:,3)
  d(:,:,4) = L(:,:,4)
  d(:,:,5) = (L(:,:,5) - L(:,:,1)) / (rho*a)
  do n=1,n_spec
     d(:,:,5+n) = L(:,:,5+n)
  enddo
  return
end subroutine compute_d_z


!=========================================================================================

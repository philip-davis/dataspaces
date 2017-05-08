#include "globalDefines.h"
!=========================================================================================
  subroutine calc_vorticity(vort,vort_mag,u)
!=========================================================================================
! computes the vorticity field associated with the velocity field
!
! vorticity is the curl of the velocity and is usually called omega -
! 
! omega_x = dw/dy - dv/dz,  omega_y = du/dz - dw/dx,  omega_z = dv/dx - du/dy
!
! vort      - vorticity vector
! vort_mag  - magnitude of vortcity vector (work array until then)
! u         - velocity vector (u, v, w)
!----------------------------------------------------------------------------------------
  use param_m, only : nx, ny, nz
  use grid_m, only : scale_1x, scale_1y, scale_1z

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real, intent(inout) :: vort(nx,ny,nz,3)
  real, intent(inout) :: vort_mag(nx,ny,nz)
  real, intent(in) :: u(nx,ny,nz,3)

! declarations local

  integer i, j, k, L
!----------------------------------------------------------------------------------------
! zero vort array

  vort=0.0
  vort_mag=0.0
!----------------------------------------------------------------------------------------
! return if zero or one dimension

  if((nx==1).and.(ny==1).and.(nz==1)) return
  if((nx >1).and.(ny==1).and.(nz==1)) return
  if((nx==1).and.(ny >1).and.(nz==1)) return
  if((nx==1).and.(ny==1).and.(nz >1)) return
!----------------------------------------------------------------------------------------
! compute omega_x = dw/dy - dv/dz

  call derivative_y(nx,ny,nz,u(1,1,1,3),vort_mag,scale_1y,1)
  vort(:,:,:,1) = vort(:,:,:,1) + vort_mag(:,:,:)

  call derivative_z(nx,ny,nz,u(1,1,1,2),vort_mag,scale_1z,1)
  vort(:,:,:,1) = vort(:,:,:,1) - vort_mag(:,:,:)

! compute omega_y = du/dz - dw/dx

  call derivative_z(nx,ny,nz,u(1,1,1,1),vort_mag,scale_1z,1)
  vort(:,:,:,2) = vort(:,:,:,2) + vort_mag(:,:,:)

  call derivative_x(nx,ny,nz,u(1,1,1,3),vort_mag,scale_1x,1)
  vort(:,:,:,2) = vort(:,:,:,2) - vort_mag(:,:,:)

! compute omega_z = dv/dx - du/dy

  call derivative_x(nx,ny,nz,u(1,1,1,2),vort_mag,scale_1x,1)
  vort(:,:,:,3) = vort(:,:,:,3) + vort_mag(:,:,:)

  call derivative_y(nx,ny,nz,u(1,1,1,1),vort_mag,scale_1y,1)
  vort(:,:,:,3) = vort(:,:,:,3) - vort_mag(:,:,:)

! compute vorticity magnitude

  vort_mag(:,:,:) = sqrt(  vort(:,:,:,1)*vort(:,:,:,1)    &
                          +vort(:,:,:,2)*vort(:,:,:,2)    &
                          +vort(:,:,:,3)*vort(:,:,:,3)  )
!----------------------------------------------------------------------------------------
  return
  end

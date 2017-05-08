#include "globalDefines.h"
!=========================================================================================
subroutine point_der1_x(f,df,xloc,scale_1)
!=========================================================================================
!
! BUG FIX 25-NOV-2004 Evatt Hawkes
! Non-uniform grid issue - need to multiply by scale_1(xloc), not scale_1(1)
!
! This routine calculates derivatives in the x-direction at boundaries using 3RD ORDER
! ONE-SIDED derivatives.
!
! It is ONLY intended for use in conjunction with the boundary conditions, where the
! derivative at the boundary is required.
!
! Since this routine only used at boundaries, no communication is required.  This
! is because the derivative stencil must be smaller than the number of points on a given
! processor.
!
! Furthermore, periodicity is not considered here since that is not a real BC.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! evaluates the first derivative in x-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!
!       d()/dx = [ d(eta)/dx ]*[ d()/d(eta) ]
!        [ dx/d(eta) ]^-1*[ d()/d(eta) ]
!
!  scale_1- Proportional to ( dx/d(eta) )^-1 where eta is fictitious
!           uniform grid and x is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along x-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_x
  use param_m, only : vary_in_x, iorder, nx, ny, nz
  use derivative_m, only : ibound

  implicit none
!-----------------------------------------------------------------------------------------
  real, intent(in)  :: f(nx,ny,nz)
  real, intent(out) :: df(ny,nz)

  integer, intent(in) :: xloc   ! specifies x location where derivative is requested.
                                ! Should be equal to "1" or "nx"
  real, intent(in) :: scale_1(nx)

  real ds

!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_x .eq. 0 ) then
     df = 0.0
     return
  endif

  if(xloc /= 1 .and. xloc /= nx) then
     ! ERROR CONDITION
  endif

!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_x.ne.1) then
    ds = real(nx*xpes-1)
  else
    ds = real(nx*xpes-1) * scale_1(1)
  endif
!-----------------------------------------------------------------------------------------
! calculate the derivative at the X-BOUNDARY plane  (yz plane at x=1 or x=nx)

  if (xloc == 1) then

     df(:,:) = ( -11.*f(1,:,:) + 18.*f(2,:,:) - 9.*f(3,:,:) + 2.*f(4,:,:) )/6. * ds

  elseif (xloc == nx) then

     df(:,:) = ( +11.*f(nx,:,:) - 18.*f(nx-1,:,:) + 9.*f(nx-2,:,:) - 2.*f(nx-3,:,:) )/6. *ds

  end if

!-----------------------------------------------------------------------------------------
  if(unif_grid_x.ne.1) then
!    BUG FIX 25 NOV
     df = scale_1(xloc)*df
  endif
!-----------------------------------------------------------------------------------------
  return

end subroutine point_der1_x





!=========================================================================================
subroutine point_der1_y(f,df,yloc,scale_1)
!=========================================================================================
!
! BUG FIX 25-NOV-2004 Evatt Hawkes
! For some reason it was already fixed in this one - wierd.
!
! This routine calculates derivatives in the y-direction at boundaries using 3RD ORDER
! ONE-SIDED derivatives.
!
! It is ONLY intended for use in conjunction with the boundary conditions, where the
! derivative at the boundary is required.
!
! Since this routine only used at boundaries, no communication is required.  This
! is because the derivative stencil must be smaller than the number of points on a given
! processor.
!
! Furthermore, periodicity is not considered here since that is not a real BC.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! evaluates the first derivative in y-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!
!       d()/dy = [ d(eta)/dy ]*[ d()/d(eta) ]
!        [ dy/d(eta) ]^-1*[ d()/d(eta) ]
!
!  scale_1- Proportional to ( dy/d(eta) )^-1 where eta is fictitious
!           uniform grid and y is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along y-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_y
  use param_m, only : vary_in_y, iorder, nx,ny,nz
  use derivative_m, only : ibound

  implicit none
!-----------------------------------------------------------------------------------------
  real, intent(in)  :: f(nx,ny,nz)   ! require 4 points including boundary
  real, intent(out) :: df(nx,nz)

  integer, intent(in) :: yloc   ! specifies y location where derivative is requested.
                                ! Should be 1 or ny
  real, intent(in) :: scale_1(ny)

  real ds

!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_y .eq. 0 ) then
     df = 0.0
     return
  endif

  if(yloc /= 1 .and. yloc /= ny) then
     ! ERROR CONDITION
  endif

!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_y.ne.1) then
    ds = real(ny*ypes-1)
  else
    ds = real(ny*ypes-1) * scale_1(1)
  endif
!-----------------------------------------------------------------------------------------
! calculate the derivative at the Y-BOUNDARY plane  (xz plane at y=1 or y=ny)

  if (yloc == 1) then

     df(:,:) = ( -11.*f(:,1,:) + 18.*f(:,2,:) - 9.*f(:,3,:) + 2.*f(:,4,:) )/6. * ds

  elseif (yloc == ny) then

     df(:,:) = ( +11.*f(:,ny,:) - 18.*f(:,ny-1,:) + 9.*f(:,ny-2,:) - 2.*f(:,ny-3,:) )/6. *ds

  end if

!-----------------------------------------------------------------------------------------
  if(unif_grid_y.ne.1) then
     df = scale_1(yloc)*df     
  endif
!-----------------------------------------------------------------------------------------
  return
end subroutine point_der1_y




!=========================================================================================
subroutine point_der1_z(f,df,zloc,scale_1)
!=========================================================================================
!
! BUG FIX 25-NOV-2004 Evatt Hawkes
! Non-uniform grid issue - need to multiply by scale_1(zloc), not scale_1(1)
!
! This routine calculates derivatives in the z-direction at boundaries using 3RD ORDER
! ONE-SIDED derivatives.
!
! It is ONLY intended for use in conjunction with the boundary conditions, where the
! derivative at the boundary is required.
!
! Since this routine only used at boundaries, no communication is required.  This
! is because the derivative stencil must be smaller than the number of points on a given
! processor.
!
! Furthermore, periodicity is not considered here since that is not a real BC.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! evaluates the first derivative in z-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!
!       d()/dz = [ d(eta)/dz ]*[ d()/d(eta) ]
!        [ dz/d(eta) ]^-1*[ d()/d(eta) ]
!
!  scale_1- Proportional to ( dz/d(eta) )^-1 where eta is fictitious
!           uniform grid and z is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along z-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_z
  use param_m, only : vary_in_z, iorder, nx, ny, nz
  use derivative_m, only : ibound

  implicit none
!-----------------------------------------------------------------------------------------
  real, intent(in)  :: f(nx,ny,nz)   ! require 4 points including boundary
  real, intent(out) :: df(nx,ny)

  integer, intent(in) :: zloc   ! specifies z location where derivative is requested.
                                ! Should be 1 or nz
  real, intent(in) :: scale_1(nz)

  real ds

!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_z .eq. 0 ) then
     df = 0.0
     return
  endif

  if(zloc /= 1 .and. zloc /= nz) then
     ! ERROR CONDITION
  endif

!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_z.ne.1) then
    ds = real(nz*zpes-1)
  else
    ds = real(nz*zpes-1) * scale_1(1)
  endif
!-----------------------------------------------------------------------------------------
! calculate the derivative at the Z-BOUNDARY plane  (xy plane at z=1 or z=nz)

  if (zloc == 1) then

     df(:,:) = ( -11.*f(:,:,1) + 18.*f(:,:,2) - 9.*f(:,:,3) + 2.*f(:,:,4) )/6. * ds

  elseif (zloc == nz) then

     df(:,:) = ( +11.*f(:,:,nz) - 18.*f(:,:,nz-1) + 9.*f(:,:,nz-2) - 2.*f(:,:,nz-3) )/6. *ds

  end if

!-----------------------------------------------------------------------------------------
  if(unif_grid_z.ne.1) then
!   BUG FIX 25-NOV-2004 Evatt Hawkes
!   Non-uniform grid issue - need to multiply by scale_1(zloc), not scale_1(1)
     df = scale_1(zloc)*df
  endif
!-----------------------------------------------------------------------------------------
  return

end subroutine point_der1_z




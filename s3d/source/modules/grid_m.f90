#include "globalDefines.h"
! $Id: grid_m.f90,v 1.1.1.1.2.6 2007/06/04 22:01:08 rsankar Exp $
!----------------------------------------
module grid_m
implicit none

integer unif_grid_x, unif_grid_y, unif_grid_z, unif_grid_all   !uniform grid switch
real min_grid_x, min_grid_y, min_grid_z  !min dimension in stretched grids
real xmin, xmax, ymin, ymax, zmin, zmax
real delx, dely, delz !uniform grid spacing
real, target, allocatable :: x(:), y(:), z(:)   !coordinates
real, target, allocatable :: xg(:), yg(:), zg(:)   !coordinates everywhere
real, target, allocatable :: scale_1x(:), scale_1y(:), scale_1z(:)
real, target, allocatable :: scale_1xg(:), scale_1yg(:), scale_1zg(:)

contains
!----------------------------------------------------------------------
subroutine allocate_grid_arrays(flag)
use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g
implicit none

integer, intent(in) :: flag

if(flag.eq.1) then

  allocate(x(nx));    x=0.0
  allocate(y(ny));    y=0.0
  allocate(z(nz));    z=0.0

  allocate(xg(nx_g));    xg=0.0
  allocate(yg(ny_g));    yg=0.0
  allocate(zg(nz_g));    zg=0.0

  allocate(scale_1x(nx));     scale_1x=0.0
  allocate(scale_1y(ny));     scale_1y=0.0
  allocate(scale_1z(nz));     scale_1z=0.0

  allocate(scale_1xg(nx_g));     scale_1xg=0.0
  allocate(scale_1yg(ny_g));     scale_1yg=0.0
  allocate(scale_1zg(nz_g));     scale_1zg=0.0

elseif(flag.eq.-1) then

  deallocate(x)
  deallocate(y)
  deallocate(z)

  deallocate(xg)
  deallocate(yg)
  deallocate(zg)

  deallocate(scale_1x)
  deallocate(scale_1y)
  deallocate(scale_1z)

  deallocate(scale_1xg)
  deallocate(scale_1yg)
  deallocate(scale_1zg)

endif

return
end subroutine allocate_grid_arrays

!----------------------------------------------------------------------
  subroutine initialize_grid(io)
! generates the grid locations and the grid jacobians for the derivative evaluation
!
! b(x,y,z) - Grid compression parameters in (x,y,z) directions
! fmap     - maps uniform points in s to nonuniform points in
!    physical space
! dfmap    - computes (d(x,y,z)/ds)/x_range as a function of s
! d2fmap   - computes (d^{2}(x,y,z)/ds^2)/x_range as a function of s
! nx,ny,nz - number of grid points in (x,y,z)-direction
! x/y/z_center - -1 ==> compressed points close to left edge
!     0 ==> compressed points close to the middle
!     1 ==> compressed points close to the right edge
!
! note: the programmed transformation is given by
!
! X = (X_{max} - X_{min})*(Sinh(b*s)/Sinh(b)) + X_{min}
!   = X_{range}*(Sinh(b*s)/Sinh(b)) + X_{min}
!
! Sinh(0) = 0, so we can either have maximum grid compression
! on the left or right end of the boundary or in the middle.
! This mapping has DISCONTINUOUS derivatives at the boundaries
! when the grid is compressed.
!
! For x:
!
! ad2fx  - d^2x/ds^2
! adfx   - dx/ds
! x  - Coordinates of possibly nonuniform grid
! xmax   - Largest value of x on grid
! xmin   - Smallest value of x on grid
! unif_grid_=() = 0 - Grid is non-uniform in the ()-direction
!  d()/dx = [ d(s)/dx ]*[ d()/d(s) ]
!   [ dx/d(s) ]^-1*[ d()/d(s) ]
!
!  d^2()/dx^2 = [ dx/d(s) ]^-2*[ d^2()/d(s)^2 ] -
!   [ dx/d(s) ]^-3[ d^2x/d(s)^2 ]*[ d()/d(s) ]
!
! scale_1 - Proportional to ( dx/d(eta) )^-1 where eta is fictitious
!   uniform grid and x is nonuniform grid. When the grid is
!   uniform then scale_1 is constant along x-axis.
! scale_2 - Proportional to ( d^2x/d(eta)^2 )*( dx/d(eta) )^-3.
!    When the grid is uniform then scale_2 is zero.
! scale_3 - Proportional to ( dx/d(eta) )^-2. When the grid is
!   uniform then scale_3 is constant along x-axis.
!
! RG: The code to compute scale_2, scale_3 arrays didn't make it into this 
!     version. It's present in Ramanan's Bunsen code, and I've pasted the
!     code snippet for 1 direction in at the end of this source file for 
!     future reference in case we should want it again
!
! s    - position along fictitious uniform grid -
!    0 <= s <= 1, -1 <= s <=0,  or -1 <= s <= 1
! smin     - either -1 or 0
! smax     - either  0 or 1
!----------------------------------------------------------------------
  use param_m
  use topology_m
  use reference_m

  implicit none

  integer, intent(in) :: io

  integer i
  real dmmy

!----------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing grid module...'
    write(io,*)
  endif

  call allocate_grid_arrays(1)
!----------------------------------------
! non-dimensionalize domain size
! factor of 100.0 converts input in cm to m

  xmin=(xmin/100.0)/l_ref
  xmax=(xmax/100.0)/l_ref

  ymin=(ymin/100.0)/l_ref
  ymax=(ymax/100.0)/l_ref

  zmin=(zmin/100.0)/l_ref
  zmax=(zmax/100.0)/l_ref

! if direction is not active, make min=0 and max=1.0

  if(vary_in_x.ne.1) then 
    xmin=0.0 
    xmax=1.0
  end if
  if(vary_in_y.ne.1) then 
    ymin=0.0 
    ymax=1.0
  end if
  if(vary_in_z.ne.1) then 
    zmin=0.0 
    zmax=1.0
  end if

!----------------------------------------
  delx=xmax-xmin  !default value if x-direction is not active
  dely=ymax-ymin  !default value if y-direction is not active
  delz=zmax-zmin  !default value if z-direction is not active

  unif_grid_all = 1
  if(unif_grid_x.eq.1 .and. vary_in_x.eq.1) then
      delx=delx/real(nx_g-1)
  else
      unif_grid_all = 0
  endif

  if(unif_grid_y.eq.1 .and. vary_in_y.eq.1) then
      dely=dely/real(ny_g-1)
  else
      unif_grid_all = 0
  endif

  if(unif_grid_z.eq.1 .and. vary_in_z.eq.1) then
      delz=delz/real(nz_g-1)
  else
      unif_grid_all = 0
  endif


!----------------------------------------
! X-direction
  if(nx_g .gt. 1 ) then
    do i = 1, nx
      call get_coord('x', xid*nx+i, x(i), scale_1x(i))
    enddo
    do i = 1, nx_g
      call get_coord('x', i, xg(i), scale_1xg(i))
    enddo
  else
    x(1) = xmin
  endif
!----------------------------------------
! Y-direction
  if(ny_g .gt. 1 ) then
    do i = 1, ny
      call get_coord('y', yid*ny+i, y(i), scale_1y(i))
    enddo
    do i = 1, ny_g
      call get_coord('y', i, yg(i), scale_1yg(i))
    enddo
  else
    y(1) = ymin
  endif
!----------------------------------------
! Z-direction
  if(nz_g .gt. 1 ) then
    do i = 1, nz
      call get_coord('z', zid*nz+i, z(i), scale_1z(i))
    enddo
    do i = 1, nz_g
      call get_coord('z', i, zg(i), scale_1zg(i))
    enddo
  else
    z(1) = zmin
  endif

  if(myid.eq.0) call write_header(io,'-')

  return
  end subroutine initialize_grid

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! New routine by Ramanan 
!----------------------------------------------------------------------
subroutine get_coord(dirn, i, coord, scale_1)
use param_m, only: nxm_g, nym_g, nzm_g
use param_m, only: nx_g, ny_g, nz_g
use reference_m, only: l_ref
implicit none
character, intent(in) :: dirn ! x, y or z 
integer, intent(in) :: i ! grid location. range = [1,n[xyz]_g]
real, intent(out) :: coord, scale_1

real s, smin, smax, g_range, s_range, ds, b_stretch, gmin, gmax
integer unif_grid, nm_g

select case(dirn)
 case('x')
  gmax = xmax; gmin = xmin
  unif_grid = unif_grid_x
  nm_g = nxm_g
  b_stretch = 0.0

    if(unif_grid_x.ne.1)then
      b_stretch = min_grid_x/l_ref/(xmax-xmin)*(nx_g-1)
    endif


case('y')
  gmax = ymax; gmin = ymin
  unif_grid = unif_grid_y
  nm_g = nym_g
  b_stretch = 0.0

    if(unif_grid_y.ne.1)then
      b_stretch = min_grid_y/l_ref/(ymax-ymin)*(ny_g-1)
    endif


case('z')
  gmax = zmax; gmin = zmin
  unif_grid = unif_grid_z
  nm_g = nzm_g
  b_stretch = 0.0

    if(unif_grid_z.ne.1)then
      b_stretch = min_grid_z/l_ref/(zmax-zmin)*(nz_g-1)
    endif


end select

if(unif_grid.eq.1) then
  smin = 0; smax = 1
else
  smin = -1; smax =  1
endif

g_range = gmax - gmin
s_range= smax - smin
ds     = (s_range)/(nm_g)

s = smin + ds*real(i-1)
coord =  &
  (g_range/s_range) * ( fmap (1, unif_grid, s, b_stretch ) - smin) +  gmin
scale_1 = (1.0/g_range)/ dfmap (1, unif_grid, s, b_stretch )

return
end subroutine get_coord

!----------------------------------------------------------------------
real function fmap(imap,unif_grid,s,beta)
!=========================================================================================
! defines the function mapping from a uniform grid running from
! (0 to 1), (-1 to 0), or (-1 to 1) to a (non)uniform grid in the
! also in the ideal domain.
!
! This subroutine is intimately related to DEFGRI.f (DEFine GRId).
!
! Important Note:
!
! For this mapping, beta cannot be zero for non-uniform grids; so
! error trapping for (unif_grid.ne.1 .and. Beta .eq. 0.) should
! be done in the calling program to avoid overhead. So the following
! code should appear in the caller:
!
! if (unif_grid_<x/y/z>.ne.1 .and. Beta_<x/y/z> .eq. 0.) then
!   write(6,*) 'Beta=0 is not allowed for non-uniform grids '
!   call terminate_run(6,0)
! endif
!
! imap    - Possible compression schemes
!        (currently unused - only one scheme is implemented)
! unif_grid   - Is the grid uniform (0 = no)
! s       - location on fictitious uniform grid
! beta    - Grid compression parameter
!-----------------------------------------------------------------------------------------
implicit none
!-----------------------------------------------------------------------------------------
integer imap
integer unif_grid
real    beta
real    s

real k, trs
!-----------------------------------------------------------------------------------------
! perform map
!-----------------------------------------------------------------------------------------
if(unif_grid.eq.1) then
  fmap = s
else
!    fmap=sinh(beta*s)/sinh(beta)
! beta is my `a' based on fine grid size. 
  trs = 150.0/200
  k = log(beta*trs)/(trs - 1.0) 
  fmap = beta*s + &
      0.5*(1.0 + tanh(16.0*(s-trs)) ) * ( exp(k*( s-1.0)) - beta*s) + &
      0.5*(1.0 - tanh(16.0*(s+trs)) ) * (-exp(k*(-s-1.0)) - beta*s)
endif
return
end function fmap

!----------------------------------------------------------------------
real function dfmap(imap,unif_grid,s,beta)
!=========================================================================================
! defines d(x)/ds where s is a unifrom grid defined from
! -1 to 1, -1 to 0, or 0 to 1 and x is a possibly nonuniform grid
! defined from xmin to xmax. X is the physical grid.
! This subroutine is intimately related to DEFGRI.f (DEFine GRId).
! notice that this function call may be made in either direction but
! the variable names would have you think that it's only good in the
! x-direction.
!
! please see comments in fmap
!-----------------------------------------------------------------------------------------
implicit none

integer imap
integer unif_grid
real    s
real    beta

real k, trs
!-----------------------------------------------------------------------------------------
! perform map
!-----------------------------------------------------------------------------------------
if(Unif_grid.eq.1) then
  dfmap = 1.0
else
!     dfmap=beta*cosh(beta*s)/sinh(beta)
  trs = 150.0/200
  k = log(beta*trs)/(trs - 1.0) 
  dfmap = beta  + &
    0.5*(1.0 + tanh(16.0*(s-trs)) ) * (k*exp(k*( s-1.0)) - beta  ) + &
    0.5*16.0/cosh(16.0*(s-trs))/cosh(16.0*(s-trs)) * ( exp(k*( s-1.0)) - beta*s) + &
    0.5*(1.0 - tanh(16.0*(s+trs)) ) * (k*exp(k*(-s-1.0)) - beta  ) + &
    0.5*16.0/cosh(16.0*(s+trs))/cosh(16.0*(s+trs)) * ( exp(k*(-s-1.0)) + beta*s)
endif
return
end function dfmap
!=========================================================================================
  real function d2fmap(imap,unif_grid,s,beta)
!=========================================================================================
! defines d^2(x)/ds^2 where s is a unifrom grid defined from
! -1 to 1, -1 to 0, or 0 to 1 and x is a possibly nonuniform grid
! defined from xmin to xmax; x is the physical grid
! this subroutine is intimately related to DEFGRI.f (DEFine GRId).
!
! please see comments in fmap
!-----------------------------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------------------------
! declarations

  integer imap
  integer unif_grid
  real    beta
  real    s
!-----------------------------------------------------------------------------------------
! perform map
!-----------------------------------------------------------------------------------------
  if(unif_grid.eq.1) then
    d2fmap = 0.0
  else
!    d2fmap=beta*beta*sinh(beta*s)/sinh(beta)
    d2fmap = 0.0
  endif
!-----------------------------------------------------------------------------------------
  return
  end function d2fmap
!-----------------------------------------------------------------------------------------

  end module grid_m


!!!! ========================================================================================
!!!! Code snippet for calculation of scale_2 / scale_3 arrays
!!!  select case (z_center)
!!!  case (-1)            ! compress along left edge
!!!     szmin = 0.;    szmax = 1.
!!!  case (0)             ! compress in the middle
!!!     szmin = -1.;    szmax = 1.
!!!  case (1)             ! compress along right edge
!!!     szmin = -1.;    szmax = 0.
!!!  end select
!!!
!!!!----------------------------------------------------------------------------------------
!!!  z_range = ( zmax - zmin )
!!!
!!!  delz=z_range  !default value if z-direction is not active
!!!
!!!  if(unif_grid_z.eq.1) then
!!!     szmin = 0
!!!     szmax = 1
!!!     if(vary_in_z.eq.1) delz=z_range/real(nz_g-1)
!!!  endif
!!!
!!!  sz_range= szmax - szmin;
!!!
!!!  dsz     = (sz_range)/(nzm_g)
!!!
!!!  szmin_l = szmin + zid*dsz*nz
!!!!----------------------------------------------------------------------------------------
!!!! Z-direction
!!!!----------------------------------------------------------------------------------------
!!!  if(nz_g .gt. 1 ) then
!!!    do i = 1, nz
!!!      s = szmin_l + dsz*real(i-1)
!!!      z(i) = (z_range/sz_range) * ( fmap (1, unif_grid_z, s, bz ) - szmin) +  zmin
!!!      adfz(i) = (z_range/sz_range) * dfmap (1, unif_grid_z, s, bz )
!!!      ad2fz(i) = (z_range/sz_range) * d2fmap (1, unif_grid_z, s, bz )
!!!      scale_1z(i) = 1.0 / adfz(i)
!!!      scale_3z(i) = scale_1z(i) * scale_1z(i)
!!!      scale_2z(i) = ad2fz(i) * scale_3z(i) * scale_1z(i)
!!!    enddo
!!!  else
!!!    z(1) = zmin
!!!  endif
!!!! End of code snippet for calculation of scale_2 / scale_3 arrays
!!!! ========================================================================================
  

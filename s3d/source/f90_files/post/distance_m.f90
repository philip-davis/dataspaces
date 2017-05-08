!$Id: distance_m.f90,v 1.1.2.3 2006/08/18 02:43:21 rsankar Exp $
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! To compute the distance function from an iso-surface
! Written for a truly 3D field, not for 2D
!
! *************** WRITTEN BY RAMANAN SANKARAN ***************
! 
! The initial condition is set based on the triangle data
! Here it is necessary that the triangles be computed on the same grid 
! as the distance function. DO NOT skip points during triangulation.
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
module distance_m

implicit none

private
public compute_dist_from_isosurface

real, public, allocatable :: dist(:,:,:)

contains !CONTAINS CONTAINS CONTAINS

!----------------------------------------------------------------------
subroutine compute_dist_from_isosurface(lim)
use isosurf_m, only: surf_field, surf_isolevel
use param_m, only: nx, ny, nz
use reference_m, only: l_ref
use grid_m, only: xmin, xmax, ymin, ymax, zmin, zmax, delx
use premix_drvd_var_m, only: calc_mag_grad
use topology_m, only: myid
implicit none

real, optional, intent(in) :: lim !Distance above lim are not of interest

real, dimension(nx, ny, nz) :: dist_ic, mag_grad
logical, dimension(nx, ny, nz) :: bc, negdist
real :: Lmax

if (.not. present(lim)) then
  Lmax = (xmax-xmin) + (ymax-ymin) + (zmax-zmin)
else
  Lmax = lim
end if

call calc_mag_grad(surf_field, mag_grad)
call set_IC(bc)
dist_ic = Lmax
where(bc) dist_ic = abs( (surf_field-surf_isolevel)/mag_grad)
where(bc .and. dist_ic>delx) dist_ic = delx !Or pick a better upper limit.
negdist = .false. 
where(surf_field < surf_isolevel) negdist = .true. 
call compute_distance_function(dist_ic, bc, Lmax, negdist)


contains !CONTAINS CONTAINS CONTAINS
  !----------------------------------------
  !----------------------------------------
  subroutine set_IC(bc)
  use param_m, only: periodic_x, periodic_y, periodic_z
  use triangulate_m
  use topology_m
  use ghost_nice_m
  implicit none
  logical, intent(out), dimension(nx, ny, nz) :: bc
  
  logical :: b(0:nx+1, 0:ny+1, 0:nz+1)
  integer :: n, i, j, k
  
  b = .false. 
  
  do n = 1, vert_count
    i = vert_loc(1, n)
    j = vert_loc(2, n)
    k = vert_loc(3, n)
    b(i, j, k) = .true.
    select case(vert_dirn(n))
      case(X_EDGE) 
        i = i+1
      case(Y_EDGE) 
        j = j+1
      case(Z_EDGE) 
        k = k+1
    end select
    b(i, j, k) = .true.
  end do
  
  call ghostzone_logical(b, nx+1, ny+1, nz+1, 1, 0, 1, 0, 1, 0)
  
  if(xid>0 .or. periodic_x .eq. 1) & 
      b(1, 1:ny, 1:nz) = b(1, 1:ny, 1:nz) .or. b(0, 1:ny, 1:nz)
  if(yid>0 .or. periodic_y .eq. 1) &
      b(1:nx, 1, 1:nz) = b(1:nx, 1, 1:nz) .or. b(1:nx, 0, 1:nz)
  if(zid>0 .or. periodic_z .eq. 1) &
      b(1:nx, 1:ny, 1) = b(1:nx, 1:ny, 1) .or. b(1:nx, 1:ny, 0)
  
  bc(1:nx, 1:ny, 1:nz) = b(1:nx, 1:ny, 1:nz)
  
  return
  end subroutine set_IC
  !----------------------------------------
end subroutine compute_dist_from_isosurface

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Stores the signed distance function in module variable dist
!  (negated where negdist is present and true)
! Initial condition is given in phi
! Boundary condition is denoted using bc
!----------------------------------------------------------------------
subroutine compute_distance_function(phi, bc, lim, negdist)
use param_m, only: nx, ny, nz
use topology_m
use reference_m, only: l_ref
implicit none
real, intent(in) :: phi(nx, ny, nz)! distance function
logical, intent(in) :: bc(nx, ny, nz) !true value denotes the node is a BC.
real, intent(in) :: lim !Distance above lim are not of interest
logical, optional, intent(in) :: negdist(nx, ny, nz) !true denotes distance is -ve

real :: xg(0:nx+1), yg(0:ny+1), zg(0:nz+1) !Grid with ghost cells
real :: f(0:nx+1,0:ny+1,0:nz+1)
logical :: is_updated(nx, ny, nz)
integer :: iter_count, upd_cnt, upd_cnt_max, upd_cnt_sum
integer i, j, k

call init_coords

f(1:nx, 1:ny, 1:nz) = phi(1:nx, 1:ny, 1:nz)
f = abs(f) !Ensure distance is positive everywhere

iter_count = 0
loop_iter: do 
  iter_count = iter_count + 1
  is_updated = .false. 

  !----------------------------------------
  call ghostzone
  do k =  1, nz,  1; do j =  1, ny,  1; do i =  1, nx,  1
    call update_phi
  end do; end do; end do
  !----------------------------------------
!  call ghostzone
  do k =  1, nz,  1; do j =  1, ny,  1; do i = nx,  1, -1
    call update_phi
  end do; end do; end do
  !----------------------------------------
!  call ghostzone
  do k =  1, nz,  1; do j = ny,  1, -1; do i = nx,  1, -1
    call update_phi
  end do; end do; end do
  !----------------------------------------
!  call ghostzone
  do k =  1, nz,  1; do j = ny,  1, -1; do i =  1, nx,  1
    call update_phi
  end do; end do; end do
  !----------------------------------------
!  call ghostzone
  do k = nz,  1, -1; do j = ny,  1, -1; do i =  1, nx,  1
    call update_phi
  end do; end do; end do
  !----------------------------------------
!  call ghostzone
  do k = nz,  1, -1; do j = ny,  1, -1; do i = nx,  1, -1
    call update_phi
  end do; end do; end do
  !----------------------------------------
!  call ghostzone
  do k = nz,  1, -1; do j =  1, ny,  1; do i = nx,  1, -1
    call update_phi
  end do; end do; end do
  !----------------------------------------
!  call ghostzone
  do k = nz,  1, -1; do j =  1, ny,  1; do i =  1, nx,  1
    call update_phi
  end do; end do; end do
  !----------------------------------------
  upd_cnt = count(is_updated)
  call MPI_Allreduce(upd_cnt, upd_cnt_sum, 1, MPI_INTEGER, MPI_SUM, gcomm, ierr)
  call MPI_Allreduce(upd_cnt, upd_cnt_max, 1, MPI_INTEGER, MPI_MAX, gcomm, ierr)
  if(myid .eq. 0) & 
    print *,'distance function iteration', iter_count, upd_cnt_sum, upd_cnt_max
  if(upd_cnt_max .eq. 0) exit loop_iter
end do loop_iter

if(.not. allocated(dist)) allocate(dist(nx,ny,nz))
dist(1:nx, 1:ny, 1:nz) = f(1:nx, 1:ny, 1:nz)

if(present(negdist)) then
  where(negdist) dist = -dist
end if

return

contains !CONTAINS CONTAINS CONTAINS

!----------------------------------------------------------------------
subroutine update_phi
use param_m, only: periodic_x, periodic_y, periodic_z
use reference_m, only: l_ref
implicit none

real :: fnb(3), del(3), soln
logical  :: det_neg
real :: error_tol

error_tol = 0.1e-6/l_ref ! 0.1 micron tolerance
!----------------------------------------
!Return if this is a boundary condition node
if(bc(i,j,k)) return

!----------------------------------------
!Find the best neighbor in x, y and z directions.
if( xid .eq. 0 .and. i .eq. 1 .and. periodic_x .ne. 1) then
  fnb(1) = f(i+1, j, k)
  del(1) = xg(i+1) - xg(i)
else if( xid .eq. xpes-1 .and. i .eq. nx .and. periodic_x .ne. 1) then
  fnb(1) = f(i-1, j, k)
  del(1) = xg(i) - xg(i-1)
else if ( f(i-1, j, k) .lt. f(i+1, j, k) ) then
  fnb(1) = f(i-1, j, k)
  del(1) = xg(i) - xg(i-1)
else
  fnb(1) = f(i+1, j, k)
  del(1) = xg(i+1) - xg(i)
end if
!----------------------------------------
if( yid .eq. 0 .and. j .eq. 1 .and. periodic_y .ne. 1) then
  fnb(2) = f(i, j+1, k)
  del(2) = yg(j+1) - yg(j)
else if( yid .eq. ypes-1 .and. j .eq. ny .and. periodic_y .ne. 1) then
  fnb(2) = f(i, j-1, k)
  del(2) = yg(j) - yg(j-1)
else if ( f(i, j-1, k) .lt. f(i, j+1, k) ) then
  fnb(2) = f(i, j-1, k)
  del(2) = yg(j) - yg(j-1)
else
  fnb(2) = f(i, j+1, k)
  del(2) = yg(j+1) - yg(j)
end if
!----------------------------------------
if( zid .eq. 0 .and. k .eq. 1 .and. periodic_z .ne. 1) then
  fnb(3) = f(i, j, k+1)
  del(3) = zg(k+1) - zg(k)
else if( zid .eq. zpes-1 .and. k .eq. nz .and. periodic_z .ne. 1) then
  fnb(3) = f(i, j, k-1)
  del(3) = zg(k) - zg(k-1)
else if ( f(i, j, k-1) .lt. f(i, j, k+1) ) then
  fnb(3) = f(i, j, k-1)
  del(3) = zg(k) - zg(k-1)
else
  fnb(3) = f(i, j, k+1)
  del(3) = zg(k+1) - zg(k)
end if
!----------------------------------------------------------------------
! Sort the three neighbors in ascending order
call check_n_swap(fnb(1), fnb(2), del(1), del(2))
call check_n_swap(fnb(1), fnb(3), del(1), del(3))
call check_n_swap(fnb(2), fnb(3), del(2), del(3))

!----------------------------------------------------------------------
! Dont do anything if all neighbors have a greater distance
if(fnb(1) .gt. f(i, j, k)) return

!----------------------------------------------------------------------
! Dont do anything if all neighbors are more than lim distance away
if(fnb(1) .gt. lim) return

!----------------------------------------
! Now solve the quadratic
call solve_quadratic(3, fnb, del, det_neg, soln)
if(det_neg .or. soln .lt. fnb(3)) &
    call solve_quadratic(2, fnb, del, det_neg, soln)
if(det_neg .or. soln .lt. fnb(2)) &
    call solve_quadratic(1, fnb, del, det_neg, soln)
if(soln < f(i, j, k)-error_tol ) then
  f(i, j, k) = soln
  is_updated(i, j, k) = .true. 
end if

return
end subroutine update_phi

!----------------------------------------------------------------------
subroutine check_n_swap(a, b, c, d)
implicit none
real, intent(inout) :: a, b, c, d
real dummy
if(a .le. b) return
dummy = a; a = b; b = dummy
dummy = c; c = d; d = dummy
return
end subroutine check_n_swap

!----------------------------------------------------------------------
subroutine solve_quadratic(n, fnb, del, neg, f)
implicit none
integer, intent(in) :: n
real, intent(in), dimension(n) :: fnb, del
logical, intent(out) :: neg
real, intent(out) :: f

real aa, bb, cc, det

if(n .eq. 1) then 
  neg = .false. 
  f = fnb(1) + del(1)
  return
end if

aa = 1.0/del(1)/del(1) + 1.0/del(2)/del(2)
if( n .eq. 3) aa = aa + 1.0/del(3)/del(3)

bb = -2.0*fnb(1)/del(1)/del(1) - 2.0*fnb(2)/del(2)/del(2)
if( n .eq. 3) bb = bb - 2.0*fnb(3)/del(3)/del(3)

cc = fnb(1)*fnb(1)/del(1)/del(1) + fnb(2)*fnb(2)/del(2)/del(2) - 1.0
if( n .eq. 3) cc = cc + fnb(3)*fnb(3)/del(3)/del(3)

det = bb*bb - 4.0*aa*cc
if(det .lt. 0) then
  neg = .true.
  return
end if

neg = .false.
f = (-bb + sqrt(det))/2.0/aa
return
end subroutine solve_quadratic

!----------------------------------------------------------------------
subroutine init_coords
use param_m, only: periodic_x, periodic_y, periodic_z
use grid_m, only: xall=>xg, yall=>yg, zall=>zg
implicit none

!----------------------------------------
xg(1:nx) = xall(xid*nx+1:(xid+1)*nx)
yg(1:ny) = yall(yid*ny+1:(yid+1)*ny)
zg(1:nz) = zall(zid*nz+1:(zid+1)*nz)
!----------------------------------------
if(xid>0) then
  xg(0) = xall(xid*nx)
else if (periodic_x .eq. 1) then
  xg(0) = 2.0*xg(1) - xg(2)
end if

if(xid<xpes-1) then
  xg(nx+1) = xall((xid+1)*nx+1)
else if (periodic_x .eq. 1) then
  xg(nx+1) = 2.0*xg(nx) - xg(nx-1)
end if
!----------------------------------------
if(yid>0) then
  yg(0) = yall(yid*ny)
else if (periodic_y .eq. 1) then
  yg(0) = 2.0*yg(1) - yg(2)
end if

if(yid<ypes-1) then
  yg(ny+1) = yall((yid+1)*ny+1)
else if (periodic_y .eq. 1) then
  yg(ny+1) = 2.0*yg(ny) - yg(ny-1)
end if
!----------------------------------------
if(zid>0) then
  zg(0) = zall(zid*nz)
else if (periodic_z .eq. 1) then
  zg(0) = 2.0*zg(1) - zg(2)
end if

if(zid<zpes-1) then
  zg(nz+1) = zall((zid+1)*nz+1)
else if (periodic_z .eq. 1) then
  zg(nz+1) = 2.0*zg(nz) - zg(nz-1)
end if
!----------------------------------------
return
end subroutine init_coords

!----------------------------------------------------------------------
subroutine ghostzone
use ghost_nice_m
call ghostzone_real(f, nx, ny, nz, 1, 1, 1, 1, 1, 1)
return
end subroutine ghostzone

!----------------------------------------------------------------------
end subroutine compute_distance_function

!----------------------------------------------------------------------
end module distance_m

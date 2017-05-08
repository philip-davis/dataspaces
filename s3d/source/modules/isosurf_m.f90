#include "globalDefines.h"
!$Id: isosurf_m.f90,v 1.1.2.5 2007/06/04 22:10:20 rsankar Exp $
!----------------------------------------------------------------------
! Module to extract an iso-surface
! 
!  ************ WRITTEN BY RAMANAN SANKARAN ***************    
! 
! Extraction through triangulation is done in another module
! This module is written an as a wrapper for the other
!----------------------------------------------------------------------
module isosurf_m
use triangulate_m
implicit none

!----------------------------------------
! Names from triangulate_m to be made available public
public vert_count
!----------------------------------------

private
public extract_isosurface, write_single_isosurface
public surface_vert_coords, surface_vert_interpolate, surface_vert_normals
public surface_vert_area, grid_vert_area, surface_to_grid

!----------------------------------------
! These are the variables that will define the iso-surface
real, public, allocatable, dimension(:,:,:) :: surf_field
real, parameter, public :: surf_isolevel = 0.65

contains !CONTAINS CONTAINS CONTAINS

!----------------------------------------------------------------------
subroutine extract_isosurface(io, xskipin, yskipin, zskipin)
use param_m, only: nx, ny, nz
use param_m, only: periodic_x, periodic_y, periodic_z
use premix_drvd_var_m, only: calculate_progvar
use topology_m
use ghost_nice_m

implicit none

integer, intent(in) :: io
integer, optional, intent(in) :: xskipin, yskipin, zskipin

real, dimension(nx+1, ny+1, nz+1) :: cg
integer :: ipad=1, jpad=1, kpad=1
integer :: xskip, yskip, zskip

xskip = 1
yskip = 1
zskip = 1
if(present(xskipin)) xskip = xskipin
if(present(yskipin)) yskip = yskipin
if(present(zskipin)) zskip = zskipin

if(myid .eq. 0) write(io,*) 'Starting to extract isosurface'
!----------------------------------------
if(.not.allocated(surf_field)) allocate(surf_field(nx, ny, nz))
call calculate_progvar(surf_field,io)

!surf_isolevel = 0.65
!----------------------------------------
cg(1:nx,1:ny,1:nz) = surf_field
call ghostzone_real(cg, nx, ny, nz, 0, 1, 0, 1, 0, 1)
!----------------------------------------
if(xid .eq. xpes-1 .and. periodic_x .ne. 1) ipad = 0
if(yid .eq. ypes-1 .and. periodic_y .ne. 1) jpad = 0
if(zid .eq. zpes-1 .and. periodic_z .ne. 1) kpad = 0
!----------------------------------------
call triangulate_field_skip (nx+ipad, ny+jpad, nz+kpad, &
      cg(1:nx+ipad, 1:ny+jpad, 1:nz+kpad), surf_isolevel, xskip, yskip, zskip)
!----------------------------------------
if(myid .eq. 0) write(io,*) 'Finished extracting isosurface'
return
end subroutine extract_isosurface

!----------------------------------------------------------------------
subroutine surface_vert_coords(xv, yv, zv)
use param_m
use topology_m
use grid_m
implicit none

integer i
real, dimension(1:vert_count), intent(out) :: xv, yv, zv
real :: xx(nx+1), yy(ny+1), zz(nz+1)

!----------------------------------------
!Compute the vertex coordinates
!Assumes uniform grid in the periodic direction.
do i = 1, nx+1
  if(xid*nx+i <= nx_g) xx(i) = xg(xid*nx+i)
end do
if(xid.eq.xpes-1 .and. periodic_x.eq.1) xx(nx+1) = 2.0*xx(nx)-xx(nx-1)
do i = 1, ny+1
  if(yid*ny+i <= ny_g) yy(i) = yg(yid*ny+i)
end do
if(yid.eq.ypes-1 .and. periodic_y.eq.1) yy(ny+1) = 2.0*yy(ny)-yy(ny-1)
do i = 1, nz+1
  if(zid*nz+i <= nz_g) zz(i) = zg(zid*nz+i)
end do
if(zid.eq.zpes-1 .and. periodic_z.eq.1) zz(nz+1) = 2.0*zz(nz)-zz(nz-1)
call vertex_coords(xx, yy, zz, xv, yv, zv)
return
end subroutine surface_vert_coords

!----------------------------------------------------------------------
subroutine surface_vert_area(vert_area)
use param_m
use topology_m
use grid_m
implicit none

integer i
real, dimension(1:vert_count), intent(out) :: vert_area
real :: xx(nx+1), yy(ny+1), zz(nz+1)

!----------------------------------------
!Compute the vertex coordinates
!Assumes uniform grid in the periodic direction.
do i = 1, nx+1
  if(xid*nx+i <= nx_g) xx(i) = xg(xid*nx+i)
end do
if(xid.eq.xpes-1 .and. periodic_x.eq.1) xx(nx+1) = 2.0*xx(nx)-xx(nx-1)
do i = 1, ny+1
  if(yid*ny+i <= ny_g) yy(i) = yg(yid*ny+i)
end do
if(yid.eq.ypes-1 .and. periodic_y.eq.1) yy(ny+1) = 2.0*yy(ny)-yy(ny-1)
do i = 1, nz+1
  if(zid*nz+i <= nz_g) zz(i) = zg(zid*nz+i)
end do
if(zid.eq.zpes-1 .and. periodic_z.eq.1) zz(nz+1) = 2.0*zz(nz)-zz(nz-1)
call vertex_area_weights(xx, yy, zz, vert_area)
return
end subroutine surface_vert_area

!----------------------------------------------------------------------
subroutine surface_to_grid(surf_var, grid_var, wt)
use param_m, only: nx, ny, nz
use ghost_nice_m
implicit none
real, dimension(1:vert_count), intent(in) :: surf_var
real, dimension(nx, ny, nz), intent(out) :: grid_var
real, dimension(1:vert_count), intent(in), optional :: wt
real, dimension(0:nx+1, 0:ny+1, 0:nz+1) :: work
integer n
integer i, j, k

work = 0.0
do n = 1, vert_count
  i = vert_loc(1, n)
  j = vert_loc(2, n)
  k = vert_loc(3, n)
  if(present(wt)) then
    work(i, j, k) = work(i, j, k) + surf_var(n)*wt(n)
  else
    work(i, j, k) = work(i, j, k) + surf_var(n)
  end if
end do

call ghostzone_real(work, nx+1, ny+1, nz+1, 1, 0, 1, 0, 1, 0, 0.0)
work(1, :, :) = work(1, :, :) + work(0, :, :)
work(:, 1, :) = work(:, 1, :) + work(:, 0, :)
work(:, :, 1) = work(:, :, 1) + work(:, :, 0)

grid_var(1:nx, 1:ny, 1:nz) = work(1:nx, 1:ny, 1:nz)
return
end subroutine surface_to_grid

!----------------------------------------------------------------------
subroutine grid_vert_area(grid_area)
use param_m, only: nx, ny, nz
implicit none
real, dimension(nx, ny, nz), intent(out) :: grid_area

real, dimension(1:vert_count) :: vert_area

call surface_vert_area(vert_area)
call surface_to_grid(vert_area, grid_area)
return
end subroutine grid_vert_area

!----------------------------------------------------------------------
subroutine surface_vert_interpolate(field, field_vert)
use param_m
use topology_m
use ghost_nice_m
implicit none
real, intent(in) :: field(nx, ny, nz)
real, intent(out) :: field_vert(1:vert_count)

real :: fieldg(nx+1, ny+1, nz+1)
integer :: ipad=1, jpad=1, kpad=1

fieldg(1:nx, 1:ny, 1:nz) = field(1:nx, 1:ny, 1:nz)
call ghostzone_real(fieldg, nx, ny, nz, 0, 1, 0, 1, 0, 1)

if(xid .eq. xpes-1 .and. periodic_x .ne. 1) ipad = 0
if(yid .eq. ypes-1 .and. periodic_y .ne. 1) jpad = 0
if(zid .eq. zpes-1 .and. periodic_z .ne. 1) kpad = 0
call vertex_interpolate(fieldg(1:nx+ipad,1:ny+jpad,1:nz+kpad), field_vert)
return
end subroutine surface_vert_interpolate

!----------------------------------------------------------------------
subroutine surface_vert_normals(norm)
use param_m
use topology_m
use grid_m
implicit none

integer i
real, dimension(3, 1:vert_count), intent(out) :: norm

real :: xx(nx+1), yy(ny+1), zz(nz+1)
real, dimension(1:vert_count) :: xv, yv, zv

!----------------------------------------
!Compute the vertex coordinates
!Assumes uniform grid in the periodic direction.
do i = 1, nx+1
  if(xid*nx+i <= nx_g) xx(i) = xg(xid*nx+i)
end do
if(xid.eq.xpes-1 .and. periodic_x.eq.1) xx(nx+1) = 2.0*xx(nx)-xx(nx-1)
do i = 1, ny+1
  if(yid*ny+i <= ny_g) yy(i) = yg(yid*ny+i)
end do
if(yid.eq.ypes-1 .and. periodic_y.eq.1) yy(ny+1) = 2.0*yy(ny)-yy(ny-1)
do i = 1, nz+1
  if(zid*nz+i <= nz_g) zz(i) = zg(zid*nz+i)
end do
if(zid.eq.zpes-1 .and. periodic_z.eq.1) zz(nz+1) = 2.0*zz(nz)-zz(nz-1)
call vertex_coords(xx, yy, zz, xv, yv, zv)

call vertex_normals(xv, yv, zv, .true., norm)

return
end subroutine surface_vert_normals


!----------------------------------------------------------------------
subroutine write_single_isosurface(io)
use param_m, only: nx, ny, nz, nx_g, ny_g, nz_g
use param_m, only: periodic_x, periodic_y, periodic_z
use topology_m
use runtime_m, only: time
use reference_m, only: time_ref, l_ref
implicit none

integer, intent(in) :: io
integer m, n
character time_ext*9
real, dimension(1:vert_count) :: xv, yv, zv
integer vert_max, trng_max, vert_ttl, trng_ttl, vert_cur, trng_cur
integer :: vert_offset(0:npes-1)
real, dimension(:), allocatable :: xv_g, yv_g, zv_g
integer, dimension(:,:), allocatable :: trng_g

if(myid .eq. 0) write(io,*) 'Starting to write single isosurface'

call surface_vert_coords(xv, yv, zv)
!----------------------------------------
call mpi_reduce(vert_count, vert_max, 1, MPI_INTEGER, MPI_MAX, 0, gcomm, ierr)
call mpi_reduce(trng_count, trng_max, 1, MPI_INTEGER, MPI_MAX, 0, gcomm, ierr)
call mpi_reduce(vert_count, vert_ttl, 1, MPI_INTEGER, MPI_SUM, 0, gcomm, ierr)
call mpi_reduce(trng_count, trng_ttl, 1, MPI_INTEGER, MPI_SUM, 0, gcomm, ierr)

if(myid .eq. 0) then
  allocate(xv_g(vert_max))
  allocate(yv_g(vert_max))
  allocate(zv_g(vert_max))
  allocate(trng_g(3, trng_max))
  vert_offset(0) = 0
  write(time_ext,'(1pe9.3)') time*time_ref
  open(unit=10, file = '../post/tecplot/surf-'//time_ext//'.tec')
  write(10, *) 'variables = x y z'
  write(10, *)
  write(10, 612) myid, vert_ttl, trng_ttl
  612 format('zone t="tri',i5.5,'" n=', i9,' e=', i9, &
           ' datapacking=point zonetype=fetriangle')
end if

!----------------------------------------
! Write out the vertex locations
do m = 0, npes-1
  if(myid .ne. 0 .and. myid .ne. m) cycle
  if(myid .ne. 0 .and. myid .eq. m) then
    call mpi_send(vert_count, 1, MPI_INTEGER, 0, 10*m, gcomm, ierr)
    if(vert_count.eq.0) cycle
    call mpi_send(xv, vert_count, MPI_REAL8, 0, 10*m+1, gcomm, ierr)
    call mpi_send(yv, vert_count, MPI_REAL8, 0, 10*m+2, gcomm, ierr)
    call mpi_send(zv, vert_count, MPI_REAL8, 0, 10*m+3, gcomm, ierr)
  end if
  if(myid .eq. 0 .and. myid .ne. m) then
    call mpi_recv(vert_cur, 1, MPI_INTEGER, m, 10*m, gcomm, status, ierr)
    if(m .ne. npes-1) vert_offset(m+1) = vert_offset(m)+vert_cur
    if(vert_cur.eq.0) cycle
    call mpi_recv(xv_g, vert_cur, MPI_REAL8, m, 10*m+1, gcomm, status, ierr)
    call mpi_recv(yv_g, vert_cur, MPI_REAL8, m, 10*m+2, gcomm, status, ierr)
    call mpi_recv(zv_g, vert_cur, MPI_REAL8, m, 10*m+3, gcomm, status, ierr)
  end if
  if(myid .eq. 0 .and. myid .eq. m) then
    vert_cur = vert_count
    vert_offset(m+1) = vert_offset(m)+vert_cur
    if(vert_cur.eq.0) cycle
    xv_g(1:vert_cur) = xv
    yv_g(1:vert_cur) = yv
    zv_g(1:vert_cur) = zv
  end if
  if(myid .eq. 0) then
    do n = 1, vert_cur
      write(10,'(3(1pe12.5,1x))') & 
          xv_g(n)*l_ref*1e3, yv_g(n)*l_ref*1e3, zv_g(n)*l_ref*1e3
    end do
  end if
end do
!----------------------------------------
! Write out the Triangle table
do m = 0, npes -1
  if(myid .ne. 0 .and. myid .ne. m) cycle
  if(myid .ne. 0 .and. myid .eq. m) then
    call mpi_send(trng_count, 1, MPI_INTEGER, 0, 10*m+4, gcomm, ierr)
    if(trng_count .eq. 0) cycle
    call mpi_send(triangle, 3*trng_count, MPI_INTEGER, 0, 10*m+5, gcomm, ierr)
  end if
  if(myid .eq. 0 .and. myid .ne. m) then
    call mpi_recv(trng_cur, 1, MPI_INTEGER, m, 10*m+4, gcomm, status, ierr)
    if(trng_cur .eq. 0) cycle
    call mpi_recv(trng_g,3*trng_cur,MPI_INTEGER,m,10*m+5,gcomm,status,ierr)
  end if
  if(myid .eq. 0 .and. myid .eq. m) then
    trng_cur = trng_count
    if(trng_cur .eq. 0) cycle
    trng_g(1:3,1:trng_cur) = triangle(1:3,1:trng_count)
  end if
  if(myid .eq. 0) then
    do n = 1, trng_cur
      write(10, *) vert_offset(m)+trng_g(1, n), &
                   vert_offset(m)+trng_g(2, n), &
                   vert_offset(m)+trng_g(3, n)
    end do
  end if
end do
!----------------------------------------
close(10)
if(myid .eq. 0) write(io,*) 'Finished writing single isosurface'
return
end subroutine write_single_isosurface

!----------------------------------------------------------------------
end module isosurf_m

#include "globalDefines.h"
! $Id: stat_util_m.f90,v 1.1.2.7 2007/06/05 17:05:45 rsankar Exp $
!----------------------------------------------------------------------
! Collection of stat utils to do averaging in several directions
! written by Ramanan Sankaran. 
! The stats are always an allgather, hence available on all processors. 
!----------------------------------------------------------------------
module stat_util_m
use topology_m
use param_m, only: nx, ny, nz, nx_g, ny_g, nz_g

implicit none

public 
real, parameter :: stat_small = 1.0e-30 ! used to check denominators

contains
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Averaging routines
!----------------------------------------------------------------------
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine calc_field_mean(field, cond, wt, fmean)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, intent(out) :: fmean

real fsum, wsum

call calc_field_mean_numden(field, cond, wt, fsum, wsum)

if(wsum .lt. stat_small) then
  fmean = 0.0
else
  fmean = fsum/wsum
end if

return
end subroutine calc_field_mean

!----------------------------------------------------------------------
subroutine calc_field_mean_numden(field, cond, wt, fsum_g, wsum_g)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, intent(out) :: fsum_g, wsum_g

real :: fsum, wsum
integer i, j, k

fsum = sum(field*wt,mask=cond)
wsum = sum(wt,mask=cond)

call mpi_allreduce(fsum, fsum_g, 1, MPI_REAL8, MPI_SUM, gcomm, ierr)
call mpi_allreduce(wsum, wsum_g, 1, MPI_REAL8, MPI_SUM, gcomm, ierr)

return
end subroutine calc_field_mean_numden

!----------------------------------------------------------------------
subroutine calc_field_accum_numden(field, cond, wt, fsum_io, wsum_io)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, intent(inout) :: fsum_io, wsum_io
real :: fsum_g, wsum_g

call calc_field_mean_numden(field, cond, wt, fsum_g, wsum_g)

fsum_io=fsum_io+fsum_g
wsum_io=wsum_io+wsum_g

return
end subroutine calc_field_accum_numden

!----------------------------------------------------------------------
subroutine calc_yz_mean(field, cond, wt, fmean)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(nx), intent(out) :: fmean

real, dimension(nx):: fsum_yz, wsum_yz

call calc_yz_mean_numden(field, cond, wt, fsum_yz, wsum_yz)

where(wsum_yz .lt. stat_small)
  fmean = 0.0
elsewhere
  fmean = fsum_yz/wsum_yz
end where

return
end subroutine calc_yz_mean

!----------------------------------------------------------------------
subroutine calc_yz_mean_numden(field, cond, wt, fsum_yz, wsum_yz)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(nx), intent(out) :: fsum_yz, wsum_yz

real, dimension(nx) :: fsum, wsum
integer i, j, k

fsum(:) = 0.0
wsum(:) = 0.0

fsum_yz(:) = 0.0
wsum_yz(:) = 0.0

do k = 1, nz
  do j = 1, ny
    do i = 1, nx
      if(cond(i,j,k)) then
        fsum(i) = fsum(i) + field(i,j,k)*wt(i,j,k)
        wsum(i) = wsum(i) + wt(i,j,k)
      end if
    end do
  end do
end do

call mpi_allreduce(fsum, fsum_yz, nx, MPI_REAL8, MPI_SUM, yz_comm, ierr)
call mpi_allreduce(wsum, wsum_yz, nx, MPI_REAL8, MPI_SUM, yz_comm, ierr)

return
end subroutine calc_yz_mean_numden

!----------------------------------------------------------------------
subroutine calc_yz_accum_numden(field, cond, wt, fsum_io, wsum_io)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(nx), intent(inout) :: fsum_io, wsum_io
real, dimension(nx) :: fsum_yz, wsum_yz

call calc_yz_mean_numden(field, cond, wt, fsum_yz, wsum_yz)

fsum_io=fsum_io+fsum_yz
wsum_io=wsum_io+wsum_yz

return
end subroutine calc_yz_accum_numden

!----------------------------------------------------------------------
subroutine calc_xz_mean(field, cond, wt, fmean)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(ny), intent(out) :: fmean

real, dimension(ny) :: fsum_xz, wsum_xz

call calc_xz_mean_numden(field, cond, wt, fsum_xz, wsum_xz)
where(wsum_xz .lt. stat_small)
  fmean = 0.0
elsewhere
  fmean = fsum_xz/wsum_xz
end where

return
end subroutine calc_xz_mean

!----------------------------------------------------------------------
subroutine calc_xz_mean_numden(field, cond, wt, fsum_xz, wsum_xz)
use topology_m, only: xz_comm, MPI_REAL8, MPI_SUM
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(ny), intent(out) :: fsum_xz, wsum_xz

real, dimension(ny) :: fsum, wsum
integer i, j, k

fsum(:) = 0.0
wsum(:) = 0.0

fsum_xz(:) = 0.0
wsum_xz(:) = 0.0

do k = 1, nz
  do j = 1, ny
    do i = 1, nx
      if(cond(i,j,k)) then
        fsum(j) = fsum(j) + field(i,j,k)*wt(i,j,k)
        wsum(j) = wsum(j) + wt(i,j,k)
      end if
    end do
  end do
end do

call mpi_allreduce(fsum, fsum_xz, ny, MPI_REAL8, MPI_SUM, xz_comm, ierr)
call mpi_allreduce(wsum, wsum_xz, ny, MPI_REAL8, MPI_SUM, xz_comm, ierr)

return
end subroutine calc_xz_mean_numden
!----------------------------------------------------------------------
subroutine calc_xz_accum_numden(field, cond, wt, fsum_io, wsum_io)
use topology_m, only: xz_comm, MPI_REAL8, MPI_SUM
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(ny), intent(inout) :: fsum_io, wsum_io
real, dimension(ny) :: fsum_xz, wsum_xz

call calc_xz_mean_numden(field, cond, wt, fsum_xz, wsum_xz)

fsum_io=fsum_io+fsum_xz
wsum_io=wsum_io+wsum_xz

return
end subroutine calc_xz_accum_numden
!----------------------------------------------------------------------
subroutine calc_y_mean_numden(field, cond, wt, fsum_y, wsum_y)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(nx, nz), intent(out) :: fsum_y, wsum_y

real, dimension(nx, nz) :: fsum, wsum
integer i, j, k

fsum(:,:) = 0.0
wsum(:,:) = 0.0

fsum_y(:,:) = 0.0
wsum_y(:,:) = 0.0

do k = 1, nz
  do j = 1, ny
    do i = 1, nx
      if(cond(i,j,k)) then
        fsum(i,k) = fsum(i,k) + field(i,j,k)*wt(i,j,k)
        wsum(i,k) = wsum(i,k) + wt(i,j,k)
      end if
    end do
  end do
end do

call mpi_allreduce(fsum, fsum_y, nx*nz, MPI_REAL8, MPI_SUM, ycomm, ierr)
call mpi_allreduce(wsum, wsum_y, nx*nz, MPI_REAL8, MPI_SUM, ycomm, ierr)

return
end subroutine calc_y_mean_numden

!----------------------------------------------------------------------
subroutine calc_y_accum_numden(field, cond, wt, fsum_io, wsum_io)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(nx, nz), intent(inout) :: fsum_io, wsum_io
real, dimension(nx, nz) :: fsum_y, wsum_y

call calc_y_mean_numden(field, cond, wt, fsum_y, wsum_y)

fsum_io=fsum_io+fsum_y
wsum_io=wsum_io+wsum_y

return
end subroutine calc_y_accum_numden

!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine calc_z_mean(field, cond, wt, fmean)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(nx, ny), intent(out) :: fmean

real, dimension(nx, ny) :: fsum_z, wsum_z

call calc_z_mean_numden(field, cond, wt, fsum_z, wsum_z)

where(wsum_z .lt. stat_small)
  fmean = 0.0
elsewhere
  fmean = fsum_z/wsum_z
end where

return
end subroutine calc_z_mean
!----------------------------------------------------------------------
subroutine calc_z_mean_numden(field, cond, wt, fsum_z, wsum_z)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(nx, ny), intent(out) :: fsum_z, wsum_z

real, dimension(nx, ny) :: fsum, wsum
integer i, j, k

fsum(:,:) = 0.0
wsum(:,:) = 0.0

fsum_z(:,:) = 0.0
wsum_z(:,:) = 0.0

do k = 1, nz
  do j = 1, ny
    do i = 1, nx
      if(cond(i,j,k)) then
        fsum(i,j) = fsum(i,j) + field(i,j,k)*wt(i,j,k)
        wsum(i,j) = wsum(i,j) + wt(i,j,k)
      end if
    end do
  end do
end do

call mpi_allreduce(fsum, fsum_z, nx*ny, MPI_REAL8, MPI_SUM, zcomm, ierr)
call mpi_allreduce(wsum, wsum_z, nx*ny, MPI_REAL8, MPI_SUM, zcomm, ierr)

return
end subroutine calc_z_mean_numden

!----------------------------------------------------------------------
subroutine calc_z_accum_numden(field, cond, wt, fsum_io, wsum_io)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
real, dimension(nx, ny), intent(inout) :: fsum_io, wsum_io
real, dimension(nx, ny) :: fsum_z, wsum_z

call calc_z_mean_numden(field, cond, wt, fsum_z, wsum_z)

fsum_io=fsum_io+fsum_z
wsum_io=wsum_io+wsum_z

return
end subroutine calc_z_accum_numden
!----------------------------------------------------------------------
subroutine calc_z_mean_numden_window(field, cond, wt, window, zcentre, fsum_z, wsum_z)
! routine is basically the same as calc_z_mean_numden but it 
! only averages in z direction over a window centred at zcentre
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
integer, intent(in) :: zcentre, window
real, dimension(nx, ny), intent(out) :: fsum_z, wsum_z

real, dimension(nx, ny) :: fsum, wsum
integer i, j, k

fsum(:,:) = 0.0
wsum(:,:) = 0.0

fsum_z(:,:) = 0.0
wsum_z(:,:) = 0.0

do k = max(1,zcentre-window/2), min(nz,zcentre+window/2)
  do j = 1, ny
    do i = 1, nx
      if(cond(i,j,k)) then
        fsum(i,j) = fsum(i,j) + field(i,j,k)*wt(i,j,k)
        wsum(i,j) = wsum(i,j) + wt(i,j,k)
      end if
    end do
  end do
end do

call mpi_allreduce(fsum, fsum_z, nx*ny, MPI_REAL8, MPI_SUM, zcomm, ierr)
call mpi_allreduce(wsum, wsum_z, nx*ny, MPI_REAL8, MPI_SUM, zcomm, ierr)

return
end subroutine calc_z_mean_numden_window
!----------------------------------------------------------------------
subroutine calc_z_accum_numden_window(field, cond, wt, window, zcentre, fsum_io, wsum_io)
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
integer, intent(in) :: zcentre, window
real, dimension(nx, ny), intent(inout) :: fsum_io, wsum_io
real, dimension(nx, ny) :: fsum_z, wsum_z

call calc_z_mean_numden_window(field, cond, wt, window, zcentre, fsum_z, wsum_z)
fsum_io=fsum_io+fsum_z
wsum_io=wsum_io+wsum_z

return
end subroutine calc_z_accum_numden_window

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Subroutines needed to exploit the symmetry rule when averaging
!----------------------------------------------------------------------
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine calc_Yfld_Ysym(field,symm)
! symm is used to denote whether the field is 
! symmetric (.true.) or anti-symmetric (.false.)
implicit none
real, dimension(ny), intent(inout) :: field
logical, intent(in) :: symm

real, dimension(ny_g) :: field_g
integer j

call mpi_gather(field, ny, MPI_REAL8, field_g, ny, MPI_REAL8, 0, ycomm, ierr)

loop_j: do j = 1, ny_g
  ! Only yid = 0 executes this loop.
  if (yid .ne. 0) exit loop_j
  !do this loop for half the domain only.
  if( j .gt. ny_g+1-j) exit loop_j

  if (symm) then
    field_g(j) = 0.5*(field_g(j)+field_g(ny_g+1-j))
    field_g(ny_g+1-j) = field_g(j)
  else
    field_g(j) = 0.5*(field_g(j)-field_g(ny_g+1-j))
    field_g(ny_g+1-j) = -field_g(j)
  end if
end do loop_j

call mpi_scatter(field_g, ny, MPI_REAL8, field, ny, MPI_REAL8, 0, ycomm, ierr)

return
end subroutine calc_Yfld_Ysym

!----------------------------------------------------------------------
subroutine calc_xz_sym_mean(field, cond, wt, symm, fmean)
! Symm is true for symmetry and false for anti-symmetry.
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
logical, intent(in) :: symm
real, dimension(ny), intent(out) :: fmean


call calc_xz_mean(field, cond, wt, fmean)
call calc_Yfld_Ysym(fmean,symm)

return
end subroutine calc_xz_sym_mean

!----------------------------------------------------------------------
subroutine calc_XYfld_Ysym(field,symm)
! symm is used to denote whether the field is 
! symmetric (.true.) or anti-symmetric (.false.)
implicit none
real, dimension(nx, ny), intent(inout) :: field
logical, intent(in) :: symm

real, dimension(nx, ny_g) :: field_g
integer j

! Lucky here. ny_g is the last dimension. No strided data type is needed.
call mpi_gather &
  (field, nx*ny, MPI_REAL8, field_g, nx*ny, MPI_REAL8, 0, ycomm, ierr)

loop_j: do j = 1, ny_g
  ! Only yid = 0 executes this loop.
  if (yid .ne. 0) exit loop_j
  !do this loop for half the domain only.
  if( j .gt. ny_g+1-j) exit loop_j

  if (symm) then
    field_g(:,j) = 0.5*(field_g(:,j)+field_g(:,ny_g+1-j))
    field_g(:,ny_g+1-j) = field_g(:,j)
  else
    field_g(:,j) = 0.5*(field_g(:,j)-field_g(:,ny_g+1-j))
    field_g(:,ny_g+1-j) = -field_g(:,j)
  end if
end do loop_j

call mpi_scatter &
  (field_g, nx*ny, MPI_REAL8, field, nx*ny, MPI_REAL8, 0, ycomm, ierr)

return
end subroutine calc_XYfld_Ysym

!----------------------------------------------------------------------
subroutine calc_z_sym_mean(field, cond, wt, symm, fmean)
! Symm is true for symmetry and false for anti-symmetry.
implicit none

real, dimension(nx, ny, nz), intent(in) :: field
logical, dimension(nx, ny, nz), intent(in) :: cond
real, dimension(nx, ny, nz), intent(in) :: wt
logical, intent(in) :: symm
real, dimension(nx, ny), intent(out) :: fmean


call calc_z_mean(field, cond, wt, fmean)
call calc_XYfld_Ysym(fmean,symm)

return
end subroutine calc_z_sym_mean

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Subroutines needed to compute a moving average
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_Xfld_Xmovsum(field, n)
!a local sum of field values from i-n through i+n 
!is stored at each i location
implicit none
real, dimension(nx), intent(inout) :: field
integer, intent(in) :: n

real, dimension(nx_g) :: field_g, field_s
integer i, m

call mpi_gather(field, nx, MPI_REAL8, field_g, nx, MPI_REAL8, 0, xcomm, ierr)

field_s = 0.0
loop_i: do i = 1, nx_g
  ! Only xid = 0 executes this loop.
  if (xid .ne. 0) exit loop_i
  loop_m: do m = i-n, i+n
    if(m .lt. 1 .or. m .gt. nx_g) cycle loop_m
    field_s(i) = field_s(i) + field_g(m)
  end do loop_m
end do loop_i

call mpi_scatter(field_s, nx, MPI_REAL8, field, nx, MPI_REAL8, 0, xcomm, ierr)

return
end subroutine calc_Xfld_Xmovsum

!----------------------------------------------------------------------
subroutine calc_sum_yrange(var,sumy,iy1,iy2)
implicit none
real, dimension(ny), intent(in) :: var
real, intent(out) :: sumy
integer, intent(in) :: iy1,iy2

real, dimension(ny, ypes) :: var_tmp
real, dimension(ny_g):: var_g

integer i, m

!call mpi_gather & 
!  (field, nx*ny, MPI_REAL8, field_tmp, nx*ny, MPI_REAL8, 0, xcomm, ierr)
call mpi_gather & 
  (var, ny, MPI_REAL8, var_tmp, ny, MPI_REAL8, 0, ycomm, ierr)

loop_i1: do i = 1, ypes
  ! Only zid = 0 executes this loop.
  if (yid .ne. 0) exit loop_i1
  var_g( (i-1)*ny+1:i*ny ) = var_tmp(:, i)
end do loop_i1

! var_g extends all along the z-direction so it is
! simple to do the sum over the required range of iz_g

sumy=0.0
do i=iy1,iy2
  sumy=sumy+var_g(i)
enddo

! put this value on every process
call MPI_Bcast(sumy, 1, MPI_REAL8, 0, ycomm, ierr)

return
!end subroutine calc_XYfld_Xmovsum
end subroutine calc_sum_yrange
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_sum_zrange(var,sumz,iz1,iz2)
implicit none
real, dimension(nz), intent(in) :: var
real, intent(out) :: sumz
integer, intent(in) :: iz1,iz2

real, dimension(nz, zpes) :: var_tmp
real, dimension(nz_g):: var_g

real, dimension(nx, ny, xpes) :: field_tmp
real, dimension(nx_g, ny) :: field_g, field_s
integer i, m

!call mpi_gather & 
!  (field, nx*ny, MPI_REAL8, field_tmp, nx*ny, MPI_REAL8, 0, xcomm, ierr)
call mpi_gather & 
  (var, nz, MPI_REAL8, var_tmp, nz, MPI_REAL8, 0, zcomm, ierr)

loop_i1: do i = 1, zpes
  ! Only zid = 0 executes this loop.
  if (zid .ne. 0) exit loop_i1
  var_g( (i-1)*nz+1:i*nz ) = var_tmp(:, i)
end do loop_i1

! var_g extends all along the z-direction so it is
! simple to do the sum over the required range of iz_g

sumz=0.0
do i=iz1,iz2
  sumz=sumz+var_g(i)
enddo

! put this value on every process
call MPI_Bcast(sumz, 1, MPI_REAL8, 0, zcomm, ierr)

return
!end subroutine calc_XYfld_Xmovsum
end subroutine calc_sum_zrange
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_XYfld_Xmovsum(field, n)
implicit none
real, dimension(nx, ny), intent(inout) :: field
integer, intent(in) :: n

real, dimension(nx, ny, xpes) :: field_tmp
real, dimension(nx_g, ny) :: field_g, field_s
integer i, m

call mpi_gather & 
  (field, nx*ny, MPI_REAL8, field_tmp, nx*ny, MPI_REAL8, 0, xcomm, ierr)

loop_i1: do i = 1, xpes
  ! Only xid = 0 executes this loop.
  if (xid .ne. 0) exit loop_i1
  field_g( (i-1)*nx+1:i*nx, : ) = field_tmp(:, :, i)
end do loop_i1

field_s = 0.0
loop_i2: do i = 1, nx_g
  ! Only xid = 0 executes this loop.
  if (xid .ne. 0) exit loop_i2
  loop_m: do m = i-n, i+n
    if(m .lt. 1 .or. m .gt. nx_g) cycle loop_m
    field_s(i,:) = field_s(i,:) + field_g(m,:)
  end do loop_m
end do loop_i2

loop_i3: do i = 1, xpes
  ! Only xid = 0 executes this loop.
  if (xid .ne. 0) exit loop_i3
  field_tmp(:, :, i) = field_s( (i-1)*nx+1:i*nx, : )
end do loop_i3

call mpi_scatter &
  (field_tmp, nx*ny, MPI_REAL8, field, nx*ny, MPI_REAL8, 0, xcomm, ierr)

return
end subroutine calc_XYfld_Xmovsum
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_XZfld_Xmovsum(field, n)
implicit none
real, dimension(nx, nz), intent(inout) :: field
integer, intent(in) :: n

real, dimension(nx, nz, xpes) :: field_tmp
real, dimension(nx_g, nz) :: field_g, field_s
integer i, m

call mpi_gather & 
  (field, nx*nz, MPI_REAL8, field_tmp, nx*nz, MPI_REAL8, 0, xcomm, ierr)

loop_i1: do i = 1, xpes
  ! Only xid = 0 executes this loop.
  if (xid .ne. 0) exit loop_i1
  field_g( (i-1)*nx+1:i*nx, : ) = field_tmp(:, :, i)
end do loop_i1

field_s = 0.0
loop_i2: do i = 1, nx_g
  ! Only xid = 0 executes this loop.
  if (xid .ne. 0) exit loop_i2
  loop_m: do m = i-n, i+n
    if(m .lt. 1 .or. m .gt. nx_g) cycle loop_m
    field_s(i,:) = field_s(i,:) + field_g(m,:)
  end do loop_m
end do loop_i2

loop_i3: do i = 1, xpes
  ! Only xid = 0 executes this loop.
  if (xid .ne. 0) exit loop_i3
  field_tmp(:, :, i) = field_s( (i-1)*nx+1:i*nx, : )
end do loop_i3

call mpi_scatter &
  (field_tmp, nx*nz, MPI_REAL8, field, nx*nz, MPI_REAL8, 0, xcomm, ierr)

return
end subroutine calc_XZfld_Xmovsum
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_XYfld_Ymovsum(field, n)
implicit none
real, dimension(nx, ny), intent(inout) :: field
integer, intent(in) :: n

real, dimension(nx, ny, ypes) :: field_tmp
real, dimension(nx, ny_g) :: field_g, field_s
integer i, m

call mpi_gather & 
  (field, nx*ny, MPI_REAL8, field_tmp, nx*ny, MPI_REAL8, 0, ycomm, ierr)

loop_i1: do i = 1, ypes
  ! Only yid = 0 executes this loop.
  if (yid .ne. 0) exit loop_i1
   field_g(:,(i-1)*ny+1:i*ny ) = field_tmp(:, :, i)
end do loop_i1

field_s = 0.0
loop_i2: do i = 1, ny_g
  ! Only yid = 0 executes this loop.
  if (yid .ne. 0) exit loop_i2
  loop_m: do m = i-n, i+n
    if(m .lt. 1 .or. m .gt. ny_g) cycle loop_m
    field_s(:,i) = field_s(:,i) + field_g(:,m)
  end do loop_m
end do loop_i2

loop_i3: do i = 1, ypes
  ! Only yid = 0 executes this loop.
  if (yid .ne. 0) exit loop_i3
  field_tmp(:, :, i) = field_s( :, (i-1)*ny+1:i*ny )
end do loop_i3

call mpi_scatter &
  (field_tmp, nx*ny, MPI_REAL8, field, nx*ny, MPI_REAL8, 0, ycomm, ierr)

return
end subroutine calc_XYfld_Ymovsum
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! I/O Routines
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Ascii text output for tecplot
!----------------------------------------------------------------------
subroutine write_Xfld(io,field,ref)
! Used in conjunction with YZ-averaging
implicit none

integer, intent(in) :: io
real, dimension(nx), intent(in) :: field
real, intent(in) :: ref

real, dimension(nx_g) :: field_g
integer i

if(yz_id .ne. 0) return

call mpi_gather(field, nx, MPI_REAL8, field_g, nx, MPI_REAL8, 0, xcomm, ierr)

if(myid .eq. 0) write(io, '(10(1pe12.5,1x))') (field_g(i)*ref, i = 1, nx_g)

return
end subroutine write_Xfld

!----------------------------------------------------------------------
subroutine write_Yfld(io,field,ref)
! Used in conjunction with XZ-averaging
implicit none

integer, intent(in) :: io
real, dimension(ny), intent(in) :: field
real, intent(in) :: ref

real, dimension(ny_g) :: field_g
integer i

if(xz_id .ne. 0) return
call mpi_gather(field, ny, MPI_REAL8, field_g, ny, MPI_REAL8, 0, ycomm, ierr)

if(myid .eq. 0) write(io, '(10(1pe12.5,1x))') (field_g(i)*ref, i = 1, ny_g)

return
end subroutine write_Yfld

!----------------------------------------------------------------------
subroutine write_XYfld(io,field,ref)
! Used in conjunction with Z-averaging
implicit none

integer, intent(in) :: io
real, dimension(nx, ny), intent(in) :: field
real, intent(in) :: ref

real, dimension(nx, ny, 0:xpes-1) :: field_g
integer i, j, l, m

if(zid .ne. 0) return

!First step, collect everything in the x-direction.
!All nx_g points should now be present in the xid==0 processors. 
if(xid == 0) field_g(:,:,0) = field(:,:)
LPI: do i = 1, xpes-1
  if(xid==i) then
    call MPI_Send(field,          nx*ny, MPI_REAL8, 0, i, xcomm, ierr)
  elseif (xid==0) then
    call MPI_Recv(field_g(1,1,i), nx*ny, MPI_REAL8, i, i, xcomm, status, ierr)
  end if
end do LPI

if(xid .ne. 0) return

!Now start writing them in sequence. 
LPJ2: do j = 0, ypes-1
  if( j .ne. 0) then
    if( yid==j) then
      call MPI_Send(field_g, nx_g*ny, MPI_REAL8, 0, j, ycomm, ierr)
    elseif (yid == 0) then
      call MPI_Recv(field_g, nx_g*ny, MPI_REAL8, j, j, ycomm, status, ierr)
    end if
  end if
  if(yid .eq. 0) write(io, '(10(1pe12.5,1x))')  &
              (((field_g(l,m,i)*ref, l=1,nx), i=0,xpes-1), m = 1, ny)
end do LPJ2

return
end subroutine write_XYfld

!----------------------------------------------------------------------
! Binary dump of averaged fields
!----------------------------------------------------------------------
subroutine dump_XYfld(string, field)
implicit none
character(*) string
real, intent(in) :: field(nx, ny)
character filename*200, myid_ext*5

if (zid .ne. 0) return

! Append my xy-id to the filename string
write(myid_ext, '(I5.5)') xy_id
filename = "../post/"//trim(string)//"."//trim(myid_ext)
open(unit=329, file=trim(filename), status='unknown', form='unformatted')
write(329) field
close(329)

return
end subroutine dump_XYfld

!----------------------------------------------------------------------
subroutine load_XYfld(string, field)
implicit none
character(*) string
real, intent(out) :: field(nx, ny)
character filename*200, myid_ext*5

if (zid .eq. 0) then
  ! Append my xy-id to the filename string
  write(myid_ext, '(I5.5)') xy_id
  filename = "../post/"//trim(string)//"."//trim(myid_ext)
  open(unit=329, file=trim(filename), status='old', form='unformatted')
  read(329) field
  close(329)
end if
call MPI_Bcast(field, nx*ny, MPI_REAL8, 0, zcomm, ierr)

return
end subroutine load_XYfld

!----------------------------------------------------------------------
!----------------------------------------------------------------------
! PDF and binning utilities
!----------------------------------------------------------------------
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine make_bins(nval, field, cond, comm, nbin, bin, tail)
implicit none
integer, intent(in) :: nval
real, intent(in) :: field(nval)
logical, intent(in) :: cond(nval)
integer, intent(in) :: comm 
integer, intent(in) :: nbin
real, intent(out) :: bin(0:nbin)
real, optional, intent(in) :: tail

real :: mnm, mxm, mnm_g, mxm_g
integer, parameter :: nbin_tmp=1000
real :: bin_tmp(0:nbin_tmp), cum(0:nbin_tmp+1)
real :: wt(nval)
real :: cum_min, cum_max
integer cnt, cnt_g
integer n

if(present(tail)) then
  cum_min = tail
  cum_max = 1.0 - tail
else
  cum_min = 0.01
  cum_max = 0.99
end if

cnt = count(cond)
call MPI_Allreduce(cnt, cnt_g, 1, MPI_INTEGER, MPI_SUM, comm, ierr)

if(cnt_g.eq.0) then
  mnm_g = 0.0
  mxm_g = 0.0
  call make_bins_lmts(mnm_g, mxm_g, nbin, bin)
  return
end if

mnm = minval(field,mask=cond)
mxm = maxval(field,mask=cond)
call MPI_Allreduce(mnm, mnm_g, 1, MPI_REAL8, MPI_MIN, comm, ierr)
call MPI_Allreduce(mxm, mxm_g, 1, MPI_REAL8, MPI_MAX, comm, ierr)

call make_bins_lmts(mnm_g, mxm_g, nbin_tmp, bin_tmp)

wt = 1.0
call calc_cumnormhist(nval, field, cond, wt, nbin_tmp, bin_tmp, comm, cum)

do n = 0, nbin_tmp
  if(cum(n).gt.cum_min) exit
end do
mnm_g = bin_tmp(max(0,n-1))

do n = nbin_tmp, 0, -1
  if(cum(n).lt.cum_max) exit
end do
mxm_g = bin_tmp(min(nbin_tmp,n+1))

call make_bins_lmts(mnm_g, mxm_g, nbin, bin)

return
end subroutine make_bins

!----------------------------------------------------------------------
subroutine make_bins_lmts(mnm, mxm, nbin, bin)
implicit none
real, intent(in) :: mnm, mxm
integer, intent(in) :: nbin
real, intent(out) :: bin(0:nbin)

integer i
real delta

delta = (mxm-mnm)/real(nbin)
do i = 0, nbin
  bin(i) = mnm+real(i)*delta
end do

return
end subroutine make_bins_lmts

!----------------------------------------------------------------------
subroutine calc_hist(nval, field, cond, wt, nbin, bin, comm, hist)
! Computes a histogram - a weighted conditional histogram
! Needs to be normalized to turn hist into a pdf
implicit none
integer, intent(in) :: nval
real, intent(in) :: field(nval)
logical, intent(in) :: cond(nval)
real, intent(in) :: wt(nval)
integer, intent(in) :: nbin
real, intent(in) :: bin(0:nbin)
integer, intent(in) :: comm
real, intent(out) :: hist(0:nbin+1)

integer iloc
real mxm, mnm, inv_delta
real this_hist(0:nbin+1)
integer i

mnm = bin(0)
mxm = bin(nbin)
if(mnm.ge.mxm) return
inv_delta = real(nbin)/(bin(nbin) - bin(0)) !Assume equi-spaced bins

this_hist(:) = 0.0
hist(:) = 0.0
do i = 1, nval
    if(.not. cond(i) ) cycle
    if( field(i) .lt. mnm) then
      this_hist(0) = this_hist(0) + wt(i) !head
    elseif (field(i) .gt. mxm) then
      this_hist(nbin+1) = this_hist(nbin+1) + wt(i) !tail
    else
      iloc = int( inv_delta*(field(i)-bin(0))) + 1
      this_hist(iloc) = this_hist(iloc) + wt(i)
    end if
end do

call mpi_allreduce &
        (this_hist, hist, nbin+2, MPI_REAL8, MPI_SUM, comm, ierr)

return
end subroutine calc_hist

!----------------------------------------------------------------------
subroutine calc_cumnormhist(nval, field, cond, wt, nbin, bin, comm, cum)
implicit none
integer, intent(in) :: nval
real, intent(in) :: field(nval)
logical, intent(in) :: cond(nval)
real, intent(in) :: wt(nval)
integer, intent(in) :: nbin
real, intent(in) :: bin(0:nbin)
integer, intent(in) :: comm
real, intent(out) :: cum(0:nbin+1)

real hist(0:nbin+1)

call calc_hist(nval, field, cond, wt, nbin, bin, comm, hist)
call calc_cumnorm_from_hist(nbin, hist, cum)

return
end subroutine calc_cumnormhist

!----------------------------------------------------------------------
subroutine calc_norm_from_hist(nbin, bin, hist, pdf)
! Calculates the normalized histogram
implicit none
integer, intent(in) :: nbin
real, intent(in) :: bin(0:nbin)
real, intent(in) :: hist(0:nbin+1)
real, intent(out) :: pdf(1:nbin)

integer i
real cumsum, inv_delta

cumsum = sum(hist)
pdf(1:nbin) = hist(1:nbin)
!normalize
if(cumsum .ne. 0) pdf(:) = pdf(:)/cumsum
!turn to pdf 
inv_delta = real(nbin)/(bin(nbin) - bin(0)) !Assume equi-spaced bins
pdf(:) = pdf(:)*inv_delta

return
end subroutine calc_norm_from_hist

!----------------------------------------------------------------------
subroutine calc_cumnorm_from_hist(nbin, hist, cum)
! Calculates the cumulated normalized histogram
implicit none
integer, intent(in) :: nbin
real, intent(in) :: hist(0:nbin+1)
real, intent(out) :: cum(0:nbin+1)

integer i
real cumsum

cumsum = sum(hist)
cum(0) = hist(0)
do i = 1, nbin+1
  cum(i) = cum(i-1)+hist(i)
end do
if(cumsum .ne. 0) cum(:) = cum(:)/cumsum

return
end subroutine calc_cumnorm_from_hist

!----------------------------------------------------------------------
subroutine calc_field_hist(field, cond, wt, nbin, bin, hist)
implicit none
real, intent(in) :: field(nx, ny, nz)
logical, intent(in) :: cond(nx, ny, nz)
real, intent(in) :: wt(nx, ny, nz)
integer, intent(in) :: nbin
real, intent(in) :: bin(0:nbin)
real, intent(out) :: hist(0:nbin+1)

call calc_hist(nx*ny*nz, &
              reshape(field,(/nx*ny*nz/) ), &
              reshape(cond,(/nx*ny*nz/) ), &
              reshape(wt,(/nx*ny*nz/) ), nbin, bin, gcomm, hist)
return
end subroutine calc_field_hist

!----------------------------------------------------------------------
subroutine calc_field_cumnormhist(field, cond, wt, nbin, bin, hist)
implicit none
real, intent(in) :: field(nx, ny, nz)
logical, intent(in) :: cond(nx, ny, nz)
real, intent(in) :: wt(nx, ny, nz)
integer, intent(in) :: nbin
real, intent(in) :: bin(0:nbin)
real, intent(out) :: hist(0:nbin+1)

call calc_cumnormhist(nx*ny*nz, &
              reshape(field,(/nx*ny*nz/) ), &
              reshape(cond,(/nx*ny*nz/) ), &
              reshape(wt,(/nx*ny*nz/) ), nbin, bin, gcomm, hist)
return
end subroutine calc_field_cumnormhist

!----------------------------------------------------------------------
subroutine calc_yz_hist(field, cond, wt, nbin, bin, hist)
implicit none
real, intent(in) :: field(ny, nz)
logical, intent(in) :: cond(ny, nz)
real, intent(in) :: wt(ny, nz)
integer, intent(in) :: nbin
real, intent(in) :: bin(0:nbin)
real, intent(out) :: hist(0:nbin+1)

call calc_hist(ny*nz, &
              reshape(field,(/ny*nz/) ), &
              reshape(cond,(/ny*nz/) ), &
              reshape(wt,(/ny*nz/) ), nbin, bin, yz_comm, hist)
return
end subroutine calc_yz_hist
!----------------------------------------------------------------------
subroutine calc_yz_hist_addtl(field, cond, wt, nbin, bin, hist)
implicit none
real, intent(in) :: field(ny, nz)
logical, intent(in) :: cond(ny, nz)
real, intent(in) :: wt(ny, nz)
integer, intent(in) :: nbin
real, intent(in) :: bin(0:nbin)
real, intent(inout) :: hist(0:nbin+1)
real :: hist_increment(0:nbin+1)

call calc_hist(ny*nz, &
              reshape(field,(/ny*nz/) ), &
              reshape(cond,(/ny*nz/) ), &
              reshape(wt,(/ny*nz/) ), nbin, bin, yz_comm, hist_increment)

hist=hist+hist_increment
return
end subroutine calc_yz_hist_addtl
!----------------------------------------------------------------------
subroutine normalize_pdf(nbin, pdf)
implicit none
integer, intent(in) :: nbin
real, intent(inout) :: pdf(0:nbin+1)
real :: pdfsum

pdfsum=sum(pdf)
pdf=pdf/pdfsum

return
end subroutine normalize_pdf


end module stat_util_m

#include "globalDefines.h"
! $Id: frozenfeed_m.f90,v 1.1.2.1.2.1 2006/08/22 20:59:18 rsankar Exp $
!----------------------------------------------------------------------
! Written by Ramanan Sankaran
! Module to feed frozen turbulence through the inflow plane
!----------------------------------------------------------------------
module frozenfeed_m
implicit none

private

public :: write_frozenfeed_data, frozenfeed_u, frozenfeed_uprime, &
          get_turbulence_from_feeddata, init_frozenfeed_read

real, public, dimension(:,:), allocatable :: vel_bar
real, public :: feedVel !Velocity at which to feed the frozen field

integer, parameter :: ifeedfile=3184
integer :: feed_max
integer :: feed_current, slice_current
real :: feed_spacing

real :: tl=0.0, tm=0.0, tr=0.0
real, dimension(:,:,:), pointer :: ul, um, ur, dummy

contains
!----------------------------------------------------------------------
subroutine write_frozenfeed_data(io, uprime)
use param_m, only: nx_g, nx, ny, nz
use grid_m, only: delx
use topology_m
implicit none
integer, intent(in):: io
real, intent(in) :: uprime(nx, ny, nz, 3)

character :: yzid_ext*5, filename*100
integer :: i, px

real(kind=4) :: up4(nx, ny, nz, 3)

if(myid .eq. 0) write(io, *) 'Writing Frozen Feed Data'
!----------------------------------------
if(myid .eq. 0) then
#ifdef SYSTEMCALLWONTWORK
  call makedirectory(trim('../data/frozenfeed/')//char(0))
#else
  call execute_command(trim('mkdir ../data/frozenfeed/'))
#endif
end if

!----------------------------------------
call MPI_Barrier(gcomm, ierr)
if(xid .eq. 0) then
  write(yzid_ext, '(I5.5)') yz_id
  filename = '../data/frozenfeed/field.'//yzid_ext
  open(unit=ifeedfile, file=trim(filename), &
       status='replace', form='unformatted')
  write(ifeedfile) ny, nz
  write(ifeedfile) nx_g
  write(ifeedfile) delx
end if
!----------------------------------------
loopxid: do px = xpes-1, 0, -1
  !Barrier is not necessary, but to prevent request flooding.
  call MPI_Barrier(xcomm, ierr)
  if(xid .ne. px .and. xid .ne. 0) cycle loopxid
  if(xid .eq. px) up4 = uprime
  if(xid .eq. px .and. px .ne. 0) &
    call MPI_Send(up4, nx*ny*nz*3, MPI_REAL4,  0, px, xcomm, ierr)
  if(xid .eq. 0 .and. px .ne. 0) &
    call MPI_Recv(up4, nx*ny*nz*3, MPI_REAL4, px, px, xcomm, status, ierr)
  if(xid .eq. 0) then
    loopi: do i = nx, 1, -1
      !Only u' is written to the file. Not U_bar+u'
      write(ifeedfile) up4(i, 1:ny, 1:nz, 1:3)
    end do loopi
  end if
end do loopxid
!----------------------------------------
if(xid .eq. 0) close(ifeedfile)
if(myid .eq. 0) write(io, *) 'Finished Writing Feed Data'

return
end subroutine write_frozenfeed_data

!----------------------------------------------------------------------
subroutine read_feed_data(time, u)
use topology_m, only: myid
use param_m, only: ny, nz
implicit none
real, intent(out) :: time, u(ny, nz, 3)
real(kind=4) :: u4(ny, nz, 3)
integer :: d1, d2, d3
real :: d4

feed_current = feed_current+1
slice_current = slice_current+1
time = real(feed_current)*feed_spacing/feedVel
if(slice_current>feed_max) then
  if(myid .eq. 0) print *, 'rewinding and refeeding turbulence'
  rewind(ifeedfile)
  read(ifeedfile) d1, d2
  read(ifeedfile) d3
  read(ifeedfile) d4
  slice_current = slice_current-feed_max
end if
read(ifeedfile) u4
u = u4
!Add u_bar to u'
u(:,:,1) = vel_bar + u(:,:,1)

return
end subroutine read_feed_data

!----------------------------------------------------------------------
subroutine read_next_data
implicit none
tl = tm
tm = tr

dummy => ul
ul => um
um => ur
ur => dummy
nullify(dummy)
call read_feed_data(tr, ur)
return
end subroutine read_next_data

!----------------------------------------------------------------------
subroutine init_frozenfeed_read
use topology_m, only: mypid=>yz_id, pid=>xid
use param_m, only: ny, nz, nx_g
implicit none
character :: mypid_ext*5, filename*100
integer :: d1, d2

if(pid .ne. 0) return

write(mypid_ext,'(I5.5)') mypid
filename = '../data/frozenfeed/field.'//mypid_ext
open(unit=ifeedfile, file=trim(filename), status='old', form='unformatted')
read(ifeedfile) d1, d2
read(ifeedfile) feed_max
read(ifeedfile) feed_spacing

allocate(ul(ny, nz, 3))
allocate(um(ny, nz, 3))
allocate(ur(ny, nz, 3))
nullify(dummy)
feed_current = -1
slice_current = 0

call read_feed_data(tl, ul)
call read_feed_data(tm, um)
call read_feed_data(tr, ur)

return
end subroutine init_frozenfeed_read

!----------------------------------------------------------------------
subroutine frozenfeed_u(time, u)
use topology_m, only: mypid=>yz_id, pid=>xid
use param_m, only: ny, nz
use reference_m, only: time_ref, a_ref
use grid_m, only : y, ymin, ymax
implicit none

real, intent(in) :: time
real, intent(out) :: u(ny, nz, 3)
real facl, facm, facr
integer j

if(pid .ne. 0) return

if(time < tl) then
  print *, 'time < tl in frozenfeed_u', mypid, time*time_ref, tl*time_ref
  stop
end if

do while(time > tr)
  call read_next_data
end do

if(time < tm) then
  facl = (tm - time)/(tm - tl)
  facm = (time - tl)/(tm - tl)
  u = facl*ul + facm*um
else
  facm = (tr - time)/(tr - tm)
  facr = (time - tm)/(tr - tm)
  u = facm*um + facr*ur
end if

!fix boundary velocity ! Chun Sang Yoo, assure v-velocity at y boundaries > 0 : Jun/08/06
!fix boundary velocity do j = 1,ny
!fix boundary velocity   if (y(j) < ymin+(ymax-ymin)/4.) then
!fix boundary velocity     u(j,:,2) = u(j,:,2) - 1.0/a_ref*(y(j)+(ymax-ymin)/4.0)/(3.*ymin+ymax)*4.
!fix boundary velocity   elseif (y(j) > (ymax-ymin)/4.) then
!fix boundary velocity     u(j,:,2) = u(j,:,2) + 1.0/a_ref*(y(j)-(ymax-ymin)/4.0)/(3.*ymax+ymin)*4.
!fix boundary velocity   endif
!fix boundary velocity enddo


return
end subroutine frozenfeed_u

!----------------------------------------------------------------------
subroutine frozenfeed_uprime(time, up)
use topology_m, only: mypid=>yz_id, pid=>xid
use param_m, only: ny, nz
use reference_m, only: time_ref
implicit none
real, intent(in) :: time
real, dimension(ny, nz), intent(out) :: up

if(pid .ne. 0) return

if(time < tl) then
  print *, 'time < tl in frozenfeed_uprime', mypid, time*time_ref, tl*time_ref
  stop
end if

do while( time > tr) 
  call read_next_data
end do

if(time < tm) then
  up(:,:) = (um(:,:,1) - ul(:,:,1))/(tm-tl)
else
  up(:,:) = (ur(:,:,1) - um(:,:,1))/(tr-tm)
end if

return
end subroutine frozenfeed_uprime

!----------------------------------------------------------------------
! This subroutine assumes the feeddata was written with the same delx
! Therefore, does not attempt any interpolation
subroutine get_turbulence_from_feeddata(io, uprime)
use param_m, only: nx_g, nx, ny, nz
use topology_m
implicit none
integer, intent(in):: io
real, intent(out) :: uprime(nx, ny, nz, 3)

character :: yzid_ext*5, filename*100
integer :: i, px
integer :: d1, d2
real :: d3
integer :: nx_feed, nx_skip

real(kind=4) :: up4(nx, ny, nz, 3), up4_plane(ny, nz, 3)

if(myid .eq. 0) write(io, *) 'Reading turbulence from frozen feed data'
!----------------------------------------
if(xid .eq. 0) then
  write(yzid_ext, '(I5.5)') yz_id
  filename = '../data/frozenfeed/field.'//yzid_ext
  open(unit=ifeedfile, file=trim(filename), &
       status='old', form='unformatted')
  read(ifeedfile) d1, d2
  read(ifeedfile) nx_feed
  read(ifeedfile) d3

  if(nx_feed<nx_g) then
    write(io, *) 'nx_feed < nx_g. Not enough data to fill the domain'
    stop
  end if
  nx_skip = nx_feed - nx_g
  !Skip the extra length
  do i = 1, nx_skip
    read(ifeedfile) up4_plane(1:ny, 1:nz, 1:3)
  end do
end if

!----------------------------------------
loopxid: do px = xpes-1, 0, -1
  !Barrier is not necessary, but to prevent request flooding.
  call MPI_Barrier(xcomm, ierr)
  if(xid .ne. px .and. xid .ne. 0) cycle loopxid
  if(xid .eq. 0) then
    loopi: do i = nx, 1, -1
      read(ifeedfile) up4(i, 1:ny, 1:nz, 1:3)
    end do loopi
  end if
  if(xid .eq. 0 .and. px .ne. 0) &
    call MPI_Send(up4, nx*ny*nz*3, MPI_REAL4, px, px, xcomm, ierr)
  if(xid .eq. px .and. px .ne. 0) &
    call MPI_Recv(up4, nx*ny*nz*3, MPI_REAL4, 0, px, xcomm, status, ierr)
  if(xid .eq. px) uprime = up4
end do loopxid
!----------------------------------------
if(xid .eq. 0) close(ifeedfile)
if(myid .eq. 0) write(io, *) 'Finished reading turbulence from feed data'

return
end subroutine get_turbulence_from_feeddata

end module frozenfeed_m


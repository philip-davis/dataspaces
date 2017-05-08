#include "globalDefines.h"
!----------------------------------------------------------------------
! Written by Ramanan Sankaran
! Module to feed "temporally evolving" turbulence. 
! An alternative to the "spatial" frozen feedturb
!----------------------------------------------------------------------
module temporalfeed_m

implicit none

private

public init_temporalfeed, end_temporalfeed
public write_temporalfeed_data
public temporalfeed_u, temporalfeed_uprime
public temporalfeed_u_all

!real, public :: feed_save_inc = 1.0, feed_save=1.0
real, public :: meanVel

real(kind=4) :: time_offset

integer,parameter :: ifeedfile=3184
integer,public :: i_time_write

integer, public :: i_temporalfeed = 0
integer, public :: i_frozenfeed = 0
integer, public :: i_temporalspecies = 0

character*1 my_feed_rw

real :: plane_fr = 1.0
integer plane_nx, plane_xid

integer io_temporal

real :: tl=0.0, tr=0.0
real, dimension(:,:,:), pointer :: ul, ur, dummy1
real, dimension(:,:,:), pointer :: yl, yr, dummy2
real, dimension(:,:), pointer :: thl, thr, dummy3

contains


!----------------------------------------------------------------------
subroutine init_temporalfeed(io)

!The processor id used will change based on feed direction.
use topology_m, only: mypid2=>yz_id, pid1=>xid, myid, gcomm, ierr
use runtime_m, only: i_restart
use param_m, only: run_mode, nx, ny, nz, nx_g, nsc
use reference_m, only: time_ref

implicit none
  include 'mpif.h'

! declarations passed in
 character*1 :: rw

! local declarations
character*5 mypid_ext
character*20 file_status
integer n1, n2, n3
integer plane_nxg
integer io

character*100 :: filename
logical :: exist

io_temporal=io

! instead of using compiler directives to determine whether we read or write
! read the temporal feed input file and set i_temporalfeed accordingly (0=neither,
! 1=write, 2=read).

! set filename in inquire

  filename = '../input/temporalfeed.in'
  call inquire_about_input_file(filename,io)

! read and broadcast the turbulence parameters from the turbulence file

  if(myid == 0) then
    open( unit=20, file=filename, status='old', form='formatted' )
    read(20,*) i_temporalfeed
    read(20,*) i_frozenfeed
    read(20,*) i_temporalspecies
    read(20,*) i_time_write
    read(20,*) plane_fr
    read(20,*) time_offset
    close(20)
  endif

  call MPI_Bcast( i_temporalfeed,    1, MPI_INTEGER, 0, gcomm, ierr )  
  call MPI_Bcast( i_frozenfeed,      1, MPI_INTEGER, 0, gcomm, ierr )  
  call MPI_Bcast( i_temporalspecies, 1, MPI_INTEGER, 0, gcomm, ierr )  
  call MPI_Bcast( i_time_write,      1, MPI_INTEGER, 0, gcomm, ierr )  
  call MPI_Bcast( plane_fr,         1, MPI_REAL8, 0, gcomm, ierr )  
  call MPI_Bcast( time_offset,      1, MPI_REAL4, 0, gcomm, ierr )  

!-----------------------------------------------------------------------------------------
! non-dimensionalize any dimensional variables read from file.

!-----------------------------------------------------------------------------------------

if(i_temporalfeed.eq.1)then
    rw='w'
elseif(i_temporalfeed.eq.2)then
    rw='r'
else
    return
endif

if(rw .eq. 'w') then
    
  plane_nxg = int(plane_fr*nx_g)
  plane_xid = int((plane_nxg-1)/nx)-1
  plane_nx  = mod(plane_nxg,nx)

  if(plane_nx .eq. 0) plane_nx = nx

  if(pid1 .ne. plane_xid) return
  
  if(i_restart .eq. 0 .and. run_mode .eq. 'solve') then
    file_status = 'replace'
  else 
    file_status = 'unknown'
  endif

  write(mypid_ext, '(I5.5)') mypid2
  open(unit=ifeedfile, file=trim("../input/feed_tevolturb/tevol_turbdata."//mypid_ext), &
       status=trim(file_status), form='unformatted')

!  write(ifeedfile) ny, nz

else

  !Feed is from the left YZ plane
  if(pid1 .ne. 0) return
  write(mypid_ext, '(I5.5)') mypid2
  open(unit=ifeedfile, file=trim("../input/feed_tevolturb/tevol_turbdata."//mypid_ext), &
       status='old', form='unformatted')

  allocate(ul(ny, nz, 3))
  allocate(ur(ny, nz, 3))
  nullify(dummy1)
  
if(i_temporalspecies.eq.1)then
  allocate(yl(ny, nz, nsc))
  allocate(yr(ny, nz, nsc))
  allocate(thl(ny, nz))
  allocate(thr(ny, nz))
  nullify(dummy2)
  nullify(dummy3)
  call read_feed_data_all(tl, ul, yl, thl)
  call read_feed_data_all(tr, ur, yr, thr)

else

  call read_feed_data(tl, ul)
  call read_feed_data(tr, ur)
  
endif    

  if(mypid2 .eq. 0) print *, 'initial read', mypid2, pid1, tl*time_ref, tr*time_ref
  if(mypid2 .eq. 0) print *

  if(tr < tl) then 
    print *, 'Error in initial feed read. tr<tl', mypid2, tl, tr
    stop
  end if

end if

my_feed_rw = rw

write(io,*)'finised init_temporal'

return
end subroutine init_temporalfeed


!----------------------------------------------------------------------
subroutine end_temporalfeed

use topology_m, only: pid1=>xid

implicit none

if(my_feed_rw .eq. 'r') then
  deallocate(ul)
  deallocate(ur)
  if(i_temporalspecies.eq.1)then
  deallocate(yl)
  deallocate(yr)
  deallocate(thl)
  deallocate(thr)
  endif
  close(ifeedfile)
else
  if(pid1 .ne. plane_xid) return
  call flush(ifeedfile)
  close(ifeedfile)
end if

return
end subroutine end_temporalfeed


!----------------------------------------------------------------------

subroutine write_temporalfeed_data(io)

use runtime_m, only: i_time, time
! This will change based on feed direction
use topology_m, only: myid, pid1=>xid
use param_m, only: nx, ny, nz, nsc
use reference_m, only:time_ref, a_ref, t_ref
use variables_m, only: u, yspecies, temp

implicit none

! declarations passed in
integer, intent(in) :: io

! local declarations
real(kind=4) :: my_time
real(kind=8) :: my_u(ny,nz,3)
real(kind=8) :: my_y(ny,nz,nsc)
real(kind=8) :: my_t(ny,nz)
integer isc

if(pid1 .ne. plane_xid) return

my_time     = real(time             ,kind=4)
my_u(:,:,:) = real(u(plane_nx,:,:,:),kind=8)

if(my_feed_rw .eq. 'r') return

! store in dimensional units for better portability between different runs
! with different reference states
my_time = my_time * time_ref
my_u = my_u * a_ref

write(ifeedfile) my_time
write(ifeedfile) my_u

if(i_temporalspecies.eq.1)then
do isc=1,nsc    
my_y(:,:,isc) = real(yspecies(plane_nx,:,:,isc),kind=8)
enddo
my_t(:,:)   = real(temp(plane_nx,:,:),kind=8)

my_t = my_t * t_ref
    
write(ifeedfile) my_y
write(ifeedfile) my_t
endif

!call flush(ifeedfile)

return
end subroutine write_temporalfeed_data


!----------------------------------------------------------------------
subroutine read_feed_data(time, u)

use param_m, only: nx, ny, nz, nsc
use reference_m, only: time_ref, a_ref, t_ref

implicit none

! declarations passed in
real, intent(out) :: time
real, dimension(ny, nz, 3), intent(out) :: u

! local declarations
real(kind=4) :: my_time
real(kind=4) :: my_u(ny,nz, 3)

if(my_feed_rw .eq. 'w') stop 'Bad attempt to read from feedfile'

read(ifeedfile) my_time
read(ifeedfile) my_u

! from dimensional to code units
!!$my_time = my_time / time_ref
!my_time = (my_time - 3.0001e-3)/ time_ref
my_time = (my_time - time_offset)/ time_ref
my_u = my_u / a_ref

time = real(my_time)
u    = real(my_u)

return
end subroutine read_feed_data

!----------------------------------------------------------------------
subroutine read_feed_data_all(time, u, yspc, tmpr)

use param_m, only: nx, ny, nz, nsc
use reference_m, only: time_ref, a_ref, t_ref

implicit none

! declarations passed in
real, intent(out) :: time
real, dimension(ny, nz, 3), intent(out) :: u
real, dimension(ny, nz, nsc), intent(out) :: yspc
real, dimension(ny, nz), intent(out) :: tmpr 

! local declarations
real(kind=4) :: my_time
real(kind=8) :: my_u(ny,nz, 3)
real(kind=8) :: my_y(ny,nz, nsc)
real(kind=8) :: my_t(ny,nz)

!esr integer isc, k,j

if(my_feed_rw .eq. 'w') stop 'Bad attempt to read from feedfile'
read(ifeedfile) my_time
read(ifeedfile) my_u
read(ifeedfile) my_y
read(ifeedfile) my_t

! from dimensional to code units
!!$my_time = my_time / time_ref
!my_time = (my_time - 3.0001e-3)/ time_ref
my_time = (my_time - time_offset)/ time_ref
my_u = my_u / a_ref
my_t = my_t / t_ref

time = real(my_time)
u    = real(my_u)   
yspc = real(my_y)
tmpr = real(my_t)

return
end subroutine read_feed_data_all

!----------------------------------------------------------------------
subroutine temporalfeed_u(time, u)

use topology_m, only: mypid=>yz_id, pid=>xid
use param_m, only: nx, ny, nz, nsc
use reference_m, only: time_ref

implicit none

! declarations passed in
real, intent(in) :: time
real, dimension(ny, nz, 3), intent(out) :: u

! local declarations
real facl, facr

if(pid .ne. 0) return
!if(mypid .eq. 0) print *, 'current time', time

if(time < tl)  then
  print *, 'time < tl in temporalfeed_u', mypid, time, tl
  stop
end if

do while( time > tr) 
  call read_next_data
  if(mypid .eq. 0) then
     print *, '--------------------------------------------------'
     print *, 'time advanced in turbulent feed data'
     print *, 'simulation time:', time*time_ref
     print *, 'time of left sample:', tl*time_ref
     print *, 'time of right sample:', tr*time_ref
     print *, '--------------------------------------------------'
  endif
end do

facl = (tr-time)/(tr-tl)
facr = (time-tl)/(tr-tl)

u = facl*ul + facr*ur

return
end subroutine temporalfeed_u
!----------------------------------------------------------------------
subroutine temporalfeed_u_all(time, u, yspec, temp)

use topology_m, only: mypid=>yz_id, pid=>xid, myid
use param_m, only: nx, ny, nz, nsc
use reference_m, only: time_ref

implicit none

! declarations passed in
real, intent(in) :: time
real, dimension(ny, nz, 3), intent(out) :: u
real, dimension(ny, nz, nsc), intent(out) :: yspec
real, dimension(ny, nz), intent(out) :: temp

! local declarations
real facl, facr

if(pid .ne. 0) return
!if(mypid .eq. 0) print *, 'current time', time

if(time < tl)  then
!  print *, 'time < tl in temporalfeed_u_all', mypid, time, tl
  write(io_temporal,*) 'time < tl in temporalfeed_u_all', mypid, time, tl
  stop
end if

do while( time > tr) 
  call read_next_data_all
  if(mypid .eq. 0) then
     print *, '--------------------------------------------------'
     print *, 'time advanced in turbulent feed data'
     print *, 'simulation time:', time*time_ref
     print *, 'time of left sample:', tl*time_ref
     print *, 'time of right sample:', tr*time_ref
     print *, '--------------------------------------------------'
  endif
end do

facl = (tr-time)/(tr-tl)
facr = (time-tl)/(tr-tl)

u = facl*ul + facr*ur
yspec = facl*yl + facr*yr
temp = facl*thl + facr*thr

return
end subroutine temporalfeed_u_all


!----------------------------------------------------------------------
subroutine temporalfeed_uprime(time, up)
! ask ChunSang if it is a good idea to add dYidx and dTdx here.

use topology_m, only: mypid=>yz_id, pid=>xid
use param_m, only: nx, ny, nz
use reference_m, only: time_ref

implicit none

! declarations passed in
real, intent(in) :: time
real, dimension(ny, nz), intent(out) :: up

! local declarations
real facl, facr

if(pid .ne. 0) return

if(time < tl) then
  print *, 'time < tl in temporalfeed_uprime', mypid, time, tl
  stop
end if


do while( time > tr) 
  call read_next_data
!  if(mypid .eq. 0) print *, 'time advanced in feed', time*time_ref, tl*time_ref, tr*time_ref
end do

up(:,:) = (ur(:,:,1) - ul(:,:,1))/(tr-tl)

return
end subroutine temporalfeed_uprime


!----------------------------------------------------------------------
subroutine read_next_data

use topology_m, only: mypid=>yz_id

implicit none

tl = tr

dummy1 => ul
ul => ur
ur => dummy1
call read_feed_data(tr, ur)

if(tr < tl) then 
  print *, 'Error in feed read. tr<tl', mypid, tl, tr
  stop
end if

return
end subroutine read_next_data
!----------------------------------------------------------------------
subroutine read_next_data_all

use topology_m, only: mypid=>yz_id, myid

implicit none

tl = tr

dummy1 => ul
ul => ur
ur => dummy1
dummy2 => yl
yl => yr
yr => dummy2
dummy3 => thl
thl=> thr
thr=> dummy3
call read_feed_data_all(tr, ur, yr, thr)

if(tr < tl) then 
  print *, 'Error in feed read. tr<tl', mypid, tl, tr
  stop
end if

return
end subroutine read_next_data_all

end module temporalfeed_m

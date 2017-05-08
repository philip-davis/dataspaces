#include "globalDefines.h"
!$Id: ghost_nice_typefree.f90,v 1.1.2.2 2006/07/14 01:36:29 rsankar Exp $
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! *******  Written by Ramanan Sankaran  ********
! To fill ghostzones on the sides of a 3D arrays.
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Pad the field on the boundaries using ghost zones
! "bdryfl" is an optional argument. 
! At the boundaries, pad with "bdryfl" when available. 
! Otherwise leave those locations untouched.
!----------------------------------------------------------------------
subroutine GHOSTSUBNAME &
    (field, nx, ny, nz, pad_xl, pad_xr, pad_yl, pad_yr, pad_zl, pad_zr, bdryfl)
use topology_m
implicit none

integer, intent(in)::nx, ny, nz, pad_xl, pad_xr, pad_yl, pad_yr, pad_zl, pad_zr
GHOSTFORTRANTYPE, intent(inout) :: & 
  field(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, 1-pad_zl:nz+pad_zr)
GHOSTFORTRANTYPE, optional, intent(in) :: bdryfl

!----------------------------------------
call fill_xdirn
call fill_ydirn
call fill_zdirn

return

CONTAINS !CONTAINS CONTAINS CONTAINS
!----------------------------------------------------------------------
subroutine fill_xdirn
use param_m, only: periodic_x
implicit none

GHOSTFORTRANTYPE, dimension(pad_xl, ny, nz) :: snd_rt, rcv_lt
GHOSTFORTRANTYPE, dimension(pad_xr, ny, nz) :: snd_lt, rcv_rt
integer :: nbr_lt, nbr_rt
integer :: req(4), stat(MPI_Status_size)
integer m

nbr_lt = xid-1
nbr_rt = xid+1

if(periodic_x .eq. 1) then
  if(xid .eq. 0) nbr_lt = xpes-1
  if(xid .eq. xpes-1) nbr_rt = 0
end if

req(:) = MPI_REQUEST_NULL

!----------------------------------------
if(nbr_lt>=0) then 
  if(pad_xr>0) then
    snd_lt = field(1:pad_xr, 1:ny, 1:nz)
    call MPI_ISend(snd_lt, pad_xr*ny*nz, &
      GHOSTMPITYPE,nbr_lt,1,xcomm,req(1),ierr)
  end if
  if(pad_xl>0) then
    call MPI_IRecv(rcv_lt, pad_xl*ny*nz, &
      GHOSTMPITYPE,nbr_lt,2,xcomm,req(2),ierr)
  end if
end if
!----------------------------------------
if(nbr_rt<xpes) then
  if(pad_xl>0) then
    snd_rt = field(nx+1-pad_xl:nx, 1:ny, 1:nz)
    call MPI_ISend(snd_rt, pad_xl*ny*nz, &
      GHOSTMPITYPE,nbr_rt,2,xcomm,req(3),ierr)
  end if
  if(pad_xr>0) then
    call MPI_IRecv(rcv_rt, pad_xr*ny*nz, &
      GHOSTMPITYPE,nbr_rt,1,xcomm,req(4),ierr)
  end if
end if
!----------------------------------------
if(nbr_lt>=0) then
  if(pad_xr>0) call mpi_wait(req(1), stat, ierr)
  if(pad_xl>0) call mpi_wait(req(2), stat, ierr)
  if(pad_xl>0) field(1-pad_xl:0, 1:ny, 1:nz) = rcv_lt
else if (present(bdryfl)) then
  field(1-pad_xl:0, 1:ny, 1:nz) = bdryfl
else
  do m = 1-pad_xl,0
    field(m,:,:) = field(1,:,:)
  end do
end if
!----------------------------------------
if(nbr_rt<xpes) then
  if(pad_xl>0) call mpi_wait(req(3), stat, ierr)
  if(pad_xr>0) call mpi_wait(req(4), stat, ierr)
  if(pad_xr>0) field(nx+1:nx+pad_xr, 1:ny, 1:nz) = rcv_rt
else if (present(bdryfl)) then
  field(nx+1:nx+pad_xr, 1:ny, 1:nz) = bdryfl
else 
  do m = nx+1,nx+pad_xr
    field(m,:,:) = field(nx,:,:)
  end do
end if
!----------------------------------------
return
end subroutine fill_xdirn

!----------------------------------------------------------------------
subroutine fill_ydirn
use param_m, only: periodic_y
implicit none

GHOSTFORTRANTYPE, dimension(1-pad_xl:nx+pad_xr, pad_yl, nz) :: snd_rt, rcv_lt
GHOSTFORTRANTYPE, dimension(1-pad_xl:nx+pad_xr, pad_yr, nz) :: snd_lt, rcv_rt
integer :: nbr_lt, nbr_rt
integer :: req(4), stat(MPI_Status_size)
integer m

nbr_lt = yid-1
nbr_rt = yid+1

if(periodic_y .eq. 1) then
  if(yid .eq. 0) nbr_lt = ypes-1
  if(yid .eq. ypes-1) nbr_rt = 0
end if

req(:) = MPI_REQUEST_NULL

!----------------------------------------
if(nbr_lt>=0) then 
  if(pad_xr>0) then
    snd_lt = field(1-pad_xl:nx+pad_xr, 1:pad_yr, 1:nz)
    call MPI_ISend(snd_lt, (nx+pad_xl+pad_xr)*pad_yr*nz, &
      GHOSTMPITYPE,nbr_lt,1,ycomm,req(1),ierr)
  end if
  if(pad_xl>0) then
    call MPI_IRecv(rcv_lt, (nx+pad_xl+pad_xr)*pad_yl*nz, &
      GHOSTMPITYPE,nbr_lt,2,ycomm,req(2),ierr)
  end if
end if
!----------------------------------------
if(nbr_rt<ypes) then
  if(pad_xl>0) then
    snd_rt = field(1-pad_xl:nx+pad_xr, ny+1-pad_yl:ny, 1:nz)
    call MPI_ISend(snd_rt, (nx+pad_xl+pad_xr)*pad_yl*nz, &
      GHOSTMPITYPE,nbr_rt,2,ycomm,req(3),ierr)
  end if
  if(pad_xr>0) then
    call MPI_IRecv(rcv_rt, (nx+pad_xl+pad_xr)*pad_yr*nz, &
      GHOSTMPITYPE,nbr_rt,1,ycomm,req(4),ierr)
  end if
end if
!----------------------------------------
if(nbr_lt>=0) then
  if(pad_xr>0) call mpi_wait(req(1), stat, ierr)
  if(pad_xl>0) call mpi_wait(req(2), stat, ierr)
  if(pad_xl>0) field(1-pad_xl:nx+pad_xr, 1-pad_yl:0, 1:nz) = rcv_lt
else if (present(bdryfl)) then
  field(1-pad_xl:nx+pad_xr, 1-pad_yl:0, 1:nz) = bdryfl
else
  do m = 1-pad_yl,0
    field(:,m,:) = field(:,1,:)
  end do
end if
!----------------------------------------
if(nbr_rt<ypes) then
  if(pad_xl>0) call mpi_wait(req(3), stat, ierr)
  if(pad_xr>0) call mpi_wait(req(4), stat, ierr)
  if(pad_xr>0) field(1-pad_xl:nx+pad_xr, ny+1:ny+pad_yr, 1:nz) = rcv_rt
else if (present(bdryfl)) then
  field(1-pad_xl:nx+pad_xr, ny+1:ny+pad_yr, 1:nz) = bdryfl
else
  do m = ny+1,ny+pad_yr
    field(:,m,:) = field(:,ny,:)
  end do
end if
!----------------------------------------
return
end subroutine fill_ydirn
!----------------------------------------------------------------------
subroutine fill_zdirn
use param_m, only: periodic_z
implicit none

GHOSTFORTRANTYPE, dimension(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, pad_zl) :: snd_rt, rcv_lt
GHOSTFORTRANTYPE, dimension(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, pad_zr) :: snd_lt, rcv_rt
integer :: nbr_lt, nbr_rt
integer :: req(4), stat(MPI_Status_size)
integer m

nbr_lt = zid-1
nbr_rt = zid+1

if(periodic_z .eq. 1) then
  if(zid .eq. 0) nbr_lt = zpes-1
  if(zid .eq. zpes-1) nbr_rt = 0
end if

req(:) = MPI_REQUEST_NULL
!----------------------------------------
if(nbr_lt>=0) then 
  if(pad_xr>0) then
    snd_lt = field(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, 1:pad_zr)
    call MPI_ISend(snd_lt, (nx+pad_xl+pad_xr)*(ny+pad_yl+pad_yr)*pad_zr, &
      GHOSTMPITYPE,nbr_lt,1,zcomm,req(1),ierr)
  end if
  if(pad_xl>0) then
    call MPI_IRecv(rcv_lt, (nx+pad_xl+pad_xr)*(ny+pad_yl+pad_yr)*pad_zl, &
      GHOSTMPITYPE,nbr_lt,2,zcomm,req(2),ierr)
  end if
end if
!----------------------------------------
if(nbr_rt<zpes) then
  if(pad_xl>0) then
    snd_rt = field(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, nz+1-pad_zl:nz)
    call MPI_ISend(snd_rt, (nx+pad_xl+pad_xr)*(ny+pad_yl+pad_yr)*pad_zl, & 
      GHOSTMPITYPE,nbr_rt,2,zcomm,req(3),ierr)
  end if
  if(pad_xr>0) then
    call MPI_IRecv(rcv_rt, (nx+pad_xl+pad_xr)*(ny+pad_yl+pad_yr)*pad_zr, &
      GHOSTMPITYPE,nbr_rt,1,zcomm,req(4),ierr)
  end if
end if
!----------------------------------------
if(nbr_lt>=0) then
  if(pad_xr>0) call mpi_wait(req(1), stat, ierr)
  if(pad_xl>0) call mpi_wait(req(2), stat, ierr)
  if(pad_xl>0) field(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, 1-pad_zl:0) = rcv_lt
else if (present(bdryfl)) then
  field(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, 1-pad_zl:0) = bdryfl
else
  do m = 1-pad_zl,0
    field(:,:,m) = field(:,:,1)
  end do
end if
!----------------------------------------
if(nbr_rt<zpes) then
  if(pad_xl>0) call mpi_wait(req(3), stat, ierr)
  if(pad_xr>0) call mpi_wait(req(4), stat, ierr)
  if(pad_xr>0) field(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, nz+1:nz+pad_zr) = rcv_rt
else if (present(bdryfl)) then
  field(1-pad_xl:nx+pad_xr, 1-pad_yl:ny+pad_yr, nz+1:nz+pad_zr) = bdryfl
else
  do m = nz+1,nz+pad_zr
    field(:,:,m) = field(:,:,nz)
  end do
end if
!----------------------------------------
return
end subroutine fill_zdirn

!----------------------------------------------------------------------
end subroutine GHOSTSUBNAME



#include "globalDefines.h"
!$Id: derivative_m.f90,v 1.2.12.1 2006/08/22 20:47:14 rsankar Exp $
!=========================================================================================
  module derivative_m
!=========================================================================================
! module for derivative variables
!-----------------------------------------------------------------------------------------
! Change Record
! 18-May-2005 Evatt Hawkes modified the derivative routines to communicate through a 
!             contiguous array, instead of strided MPI_datatypes. This was done to fix
!             an issue with the infiniband switch, 
!             and should be helpful on other platforms too.
! 24-NOV-2004 Evatt Hawkes - adding neg_f_x etc for single plane in-plane derivative ops
! 
!-----------------------------------------------------------------------------------------

  implicit none
!-----------------------------------------------------------------------------------------
! integers

  integer :: ibound=3
  integer :: i_symmetry=0 ! set as non-symmetric by default
                          ! must modify all calls to derivatives to make symmetry work
                          ! this is a big job and anyone who does it should speak with SDM

  integer :: isym_x=0     ! set as non-symmetric by default
  integer :: isym_y=0     ! set as non-symmetric by default
  integer :: isym_z=0     ! set as non-symmetric by default

! ghost cell arrays

#ifdef COARRAYCOMMUNICATION
  real, allocatable :: neg_f_x(:,:,:)[:], pos_f_x(:,:,:)[:]
  real, allocatable :: neg_f_y(:,:,:)[:], pos_f_y(:,:,:)[:]
  real, allocatable :: neg_f_z(:,:,:)[:], pos_f_z(:,:,:)[:]
#else
  real, allocatable :: neg_f_x(:,:,:), pos_f_x(:,:,:)
  real, allocatable :: neg_f_y(:,:,:), pos_f_y(:,:,:)
  real, allocatable :: neg_f_z(:,:,:), pos_f_z(:,:,:)
! 18-MAY-2005 Evatt Hawkes
! infiniband workarounds
  real, allocatable :: neg_fs_x(:,:,:), pos_fs_x(:,:,:)
  real, allocatable :: neg_fs_y(:,:,:), pos_fs_y(:,:,:)
#endif

! Data structures for starting multiple exchanges before derivative calculation
! RG 21APR08

  integer, parameter :: deriv_list_size = 10
  TYPE :: deriv_comm_data
      logical :: inuse = .false.
      logical :: posted = .false.
      real :: checksum = 0.0d0
      character(len=10) :: fldname = ''
      integer :: neg_nbr, pos_nbr
      integer, dimension(4) :: req
  END TYPE deriv_comm_data

  TYPE(deriv_comm_data), ALLOCATABLE, DIMENSION(:) :: deriv_x_list
  TYPE(deriv_comm_data), ALLOCATABLE, DIMENSION(:) :: deriv_y_list
  TYPE(deriv_comm_data), ALLOCATABLE, DIMENSION(:) :: deriv_z_list

  ! Recv buffers
  real, allocatable :: neg_f_x_buf(:,:,:,:), pos_f_x_buf(:,:,:,:)
  real, allocatable :: neg_f_y_buf(:,:,:,:), pos_f_y_buf(:,:,:,:)
  real, allocatable :: neg_f_z_buf(:,:,:,:), pos_f_z_buf(:,:,:,:)

  ! Send buffers
  real, allocatable :: neg_fs_x_buf(:,:,:,:), pos_fs_x_buf(:,:,:,:)
  real, allocatable :: neg_fs_y_buf(:,:,:,:), pos_fs_y_buf(:,:,:,:)
  real, allocatable :: neg_fs_z_buf(:,:,:,:), pos_fs_z_buf(:,:,:,:)
! ============================================================================= 

! 24-NOV-2004 Evatt Hawkes
! ghost cell arrays for in-plane derivative operations

  real, target, allocatable :: neg_f_x_xy(:,:), pos_f_x_xy(:,:)
  real, target, allocatable :: neg_f_y_xy(:,:), pos_f_y_xy(:,:)

  real, target, allocatable :: neg_f_x_xz(:,:), pos_f_x_xz(:,:)
  real, target, allocatable :: neg_f_z_xz(:,:), pos_f_z_xz(:,:)

  real, target, allocatable :: neg_f_y_yz(:,:), pos_f_y_yz(:,:)
  real, target, allocatable :: neg_f_z_yz(:,:), pos_f_z_yz(:,:)

! 07-Jun-2006 Chun Sang Yoo

  real, target, allocatable :: neg_fs_x_xy(:,:), pos_fs_x_xy(:,:)
  real, target, allocatable :: neg_fs_y_xy(:,:), pos_fs_y_xy(:,:)

  real, target, allocatable :: neg_fs_x_xz(:,:), pos_fs_x_xz(:,:)
  real, target, allocatable :: neg_fs_z_xz(:,:), pos_fs_z_xz(:,:)

  real, target, allocatable :: neg_fs_y_yz(:,:), pos_fs_y_yz(:,:)
  real, target, allocatable :: neg_fs_z_yz(:,:), pos_fs_z_yz(:,:)
!-----------------------------------------------------------------------------------------
  contains
!=========================================================================================
  subroutine allocate_derivative_arrays(flag)
!=========================================================================================
! allocate derivative arrays
!-----------------------------------------------------------------------------------------
  use param_m, only : nx, ny, nz, iorder

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed

  integer flag
!-----------------------------------------------------------------------------------------
! derivative arrays

  if(flag.eq.1) then

#ifdef COARRAYCOMMUNICATION
      allocate(neg_f_x(iorder/2,ny,nz)[0:*])
      allocate(pos_f_x(iorder/2,ny,nz)[0:*])

      allocate(neg_f_y(nx,iorder/2,nz)[0:*])
      allocate(pos_f_y(nx,iorder/2,nz)[0:*])

      allocate(neg_f_z(nx,ny,iorder/2)[0:*])
      allocate(pos_f_z(nx,ny,iorder/2)[0:*])
#else
      allocate(neg_f_x(iorder/2,ny,nz))
      allocate(pos_f_x(iorder/2,ny,nz))

      allocate(neg_f_y(nx,iorder/2,nz))
      allocate(pos_f_y(nx,iorder/2,nz))

      allocate(neg_f_z(nx,ny,iorder/2))
      allocate(pos_f_z(nx,ny,iorder/2))

!     18-MAY-2005 Evatt Hawkes
!     infiniband work-arounds

      allocate(neg_fs_x(iorder/2,ny,nz))
      allocate(pos_fs_x(iorder/2,ny,nz))

      allocate(neg_fs_y(nx,iorder/2,nz))
      allocate(pos_fs_y(nx,iorder/2,nz))

#endif

!     24-NOV-2004 Evatt Hawkes
!     ghost cell arrays for in-plane derivative operations

      allocate(neg_f_x_xy(iorder/2,ny))
      allocate(pos_f_x_xy(iorder/2,ny))

      allocate(neg_f_y_xy(nx,iorder/2))
      allocate(pos_f_y_xy(nx,iorder/2))

      allocate(neg_f_x_xz(iorder/2,nz))
      allocate(pos_f_x_xz(iorder/2,nz))

      allocate(neg_f_z_xz(nx,iorder/2))
      allocate(pos_f_z_xz(nx,iorder/2))

      allocate(neg_f_y_yz(iorder/2,nz))
      allocate(pos_f_y_yz(iorder/2,nz))

      allocate(neg_f_z_yz(ny,iorder/2))
      allocate(pos_f_z_yz(ny,iorder/2))

!     07-Jun-2006 Chun Sang Yoo
      allocate(neg_fs_x_xy(iorder/2,ny))
      allocate(pos_fs_x_xy(iorder/2,ny))

      allocate(neg_fs_y_xy(nx,iorder/2))
      allocate(pos_fs_y_xy(nx,iorder/2))

      allocate(neg_fs_x_xz(iorder/2,nz))
      allocate(pos_fs_x_xz(iorder/2,nz))

      allocate(neg_fs_z_xz(nx,iorder/2))
      allocate(pos_fs_z_xz(nx,iorder/2))

      allocate(neg_fs_y_yz(iorder/2,nz))
      allocate(pos_fs_y_yz(iorder/2,nz))

      allocate(neg_fs_z_yz(ny,iorder/2))
      allocate(pos_fs_z_yz(ny,iorder/2))

      allocate(deriv_x_list(deriv_list_size))
      allocate(deriv_y_list(deriv_list_size))
      allocate(deriv_z_list(deriv_list_size))

      allocate(neg_f_x_buf(iorder/2,ny,nz,deriv_list_size))
      allocate(pos_f_x_buf(iorder/2,ny,nz,deriv_list_size))

      allocate(neg_f_y_buf(nx,iorder/2,nz,deriv_list_size))
      allocate(pos_f_y_buf(nx,iorder/2,nz,deriv_list_size))

      allocate(neg_f_z_buf(nx,ny,iorder/2,deriv_list_size))
      allocate(pos_f_z_buf(nx,ny,iorder/2,deriv_list_size))

      allocate(neg_fs_x_buf(iorder/2,ny,nz,deriv_list_size))
      allocate(pos_fs_x_buf(iorder/2,ny,nz,deriv_list_size))

      allocate(neg_fs_y_buf(nx,iorder/2,nz,deriv_list_size))
      allocate(pos_fs_y_buf(nx,iorder/2,nz,deriv_list_size))

      allocate(neg_fs_z_buf(nx,ny,iorder/2,deriv_list_size))
      allocate(pos_fs_z_buf(nx,ny,iorder/2,deriv_list_size))

  elseif(flag.eq.-1) then

      deallocate(neg_f_x)
      deallocate(pos_f_x)

      deallocate(neg_f_y)
      deallocate(pos_f_y)

      deallocate(neg_f_z)
      deallocate(pos_f_z)

#ifndef COARRAYCOMMUNICATION
!     18-MAY-2005 Evatt Hawkes
!     infiniband work-arounds

      deallocate(neg_fs_x)
      deallocate(pos_fs_x)

      deallocate(neg_fs_y)
      deallocate(pos_fs_y)
#endif

!     24-NOV-2004 Evatt Hawkes
!     ghost cell arrays for in-plane derivative operations

      deallocate(neg_f_x_xy)
      deallocate(pos_f_x_xy)

      deallocate(neg_f_y_xy)
      deallocate(pos_f_y_xy)

      deallocate(neg_f_x_xz)
      deallocate(pos_f_x_xz)

      deallocate(neg_f_z_xz)
      deallocate(pos_f_z_xz)

      deallocate(neg_f_y_yz)
      deallocate(pos_f_y_yz)

      deallocate(neg_f_z_yz)
      deallocate(pos_f_z_yz)

!     07-Jun-2006 Chun Sang Yoo

      deallocate(neg_fs_x_xy)
      deallocate(pos_fs_x_xy)

      deallocate(neg_fs_y_xy)
      deallocate(pos_fs_y_xy)

      deallocate(neg_fs_x_xz)
      deallocate(pos_fs_x_xz)

      deallocate(neg_fs_z_xz)
      deallocate(pos_fs_z_xz)

      deallocate(neg_fs_y_yz)
      deallocate(pos_fs_y_yz)

      deallocate(neg_fs_z_yz)
      deallocate(pos_fs_z_yz)

      deallocate(deriv_x_list)
      deallocate(deriv_y_list)
      deallocate(deriv_z_list)

      deallocate(neg_f_x_buf)
      deallocate(pos_f_x_buf)

      deallocate(neg_f_y_buf)
      deallocate(pos_f_y_buf)

      deallocate(neg_f_z_buf)
      deallocate(pos_f_z_buf)

      deallocate(neg_fs_x_buf)
      deallocate(pos_fs_x_buf)

      deallocate(neg_fs_y_buf)
      deallocate(pos_fs_y_buf)

      deallocate(neg_fs_z_buf)
      deallocate(pos_fs_z_buf)

  endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine allocate_derivative_arrays
!=========================================================================================
  subroutine initialize_derivative(io)
!=========================================================================================
! initializes derivative variables
!-----------------------------------------------------------------------------------------
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io
!-----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing derivative module...'
    write(io,*)
  endif
!-----------------------------------------------------------------------------------------
! allocate arrays

  call allocate_derivative_arrays(1)
!-----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!-----------------------------------------------------------------------------------------
  end subroutine initialize_derivative
!-----------------------------------------------------------------------------------------
!=========================================================================================
  subroutine print_deriv_list(io)
!=========================================================================================
! Print out table of pending derivative operations
!-----------------------------------------------------------------------------------------
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io
!-----------------------------------------------------------------------------------------
! locals
  integer ilist

!-----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'=')
    call write_header(io,'-')
    write(io,'(1a,i5,1a)') 'x-direction table for id ',myid, ':'
    write(io,800) 'Number', 'In use', 'Rcv. Posted', 'Checksum', 'Name', 'neg_nbr', 'pos_nbr'
    do ilist = 1, deriv_list_size
        write(io,810) ilist, deriv_x_list(ilist)%inuse, &
                      deriv_x_list(ilist)%posted, &
                      deriv_x_list(ilist)%checksum,&
                      deriv_x_list(ilist)%fldname, &
                      deriv_x_list(ilist)%neg_nbr, &
                      deriv_x_list(ilist)%pos_nbr
    enddo
    call write_header(io,'-')
    write(io,'(1a,i5,1a)') 'y-direction table for id ',myid, ':'
    write(io,800) 'Number', 'In use', 'Rcv. Posted', 'Checksum', 'Name', 'neg_nbr', 'pos_nbr'
    do ilist = 1, deriv_list_size
        write(io,810) ilist, deriv_y_list(ilist)%inuse, &
                      deriv_y_list(ilist)%posted, &
                      deriv_y_list(ilist)%checksum,&
                      deriv_y_list(ilist)%fldname, &
                      deriv_y_list(ilist)%neg_nbr, &
                      deriv_y_list(ilist)%pos_nbr
    enddo
    call write_header(io,'-')
    write(io,'(1a,i5,1a)') 'z-direction table for id ',myid, ':'
    write(io,800) 'Number', 'In use', 'Rcv. Posted', 'Checksum', 'Name', 'neg_nbr', 'pos_nbr'
    do ilist = 1, deriv_list_size
        write(io,810) ilist, deriv_z_list(ilist)%inuse, &
                      deriv_z_list(ilist)%posted, &
                      deriv_z_list(ilist)%checksum,&
                      deriv_z_list(ilist)%fldname, &
                      deriv_z_list(ilist)%neg_nbr, &
                      deriv_z_list(ilist)%pos_nbr
    enddo
    call write_header(io,'-')
    call write_header(io,'=')
  endif

  800 format(a10, a10, a15, a15, a10, a20, a10, a10)
  810 format(i10, l10, l15, f15.5, a20, i10, i10)
!-----------------------------------------------------------------------------------------
  end subroutine print_deriv_list
!-----------------------------------------------------------------------------------------
  end module derivative_m


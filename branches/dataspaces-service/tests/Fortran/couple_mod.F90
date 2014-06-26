!!
! Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are permitted provided
! that the following conditions are met:
!
! - Redistributions of source code must retain the above copyright notice, this list of conditions and
! the following disclaimer.
! - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
! the following disclaimer in the documentation and/or other materials provided with the distribution.
! - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers University, nor the names of its
! contributors may be used to endorse or promote products derived from this software without specific prior
! written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
! WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
! PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
! DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
! HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
! NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
!
!!

!!
!  Ciprian Docan (2009)  TASSL Rutgers University
!  docan@cac.rutgers.edu
!  Tong Jin (2011) TASSL Rutgers University
!  tjin@cac.rutgers.edu
!!
module couple_comm

  integer :: npx, npy, npz  ! # of processors in x-y-z direction
  integer :: spx, spy, spz  ! block size per processor per direction
  integer :: timesteps      ! # of coupling scenarios (iterations)
  integer :: npapp          ! # of processors in the application
  integer :: ndim           ! # number of dimension
  integer :: appid          ! appplication id number

  integer :: rank, nproc

  real (kind=8), dimension(:,:), allocatable :: m2d
  real (kind=8), dimension(:,:,:), allocatable :: m3d

  integer :: off_x, off_y, off_z

  integer :: ts

contains

subroutine parse_args()
!   implicit none

! #ifndef __GFORTRAN__
! #ifndef __GNUC__
!     interface
!          integer function iargc()
!          end function iargc
!     end interface
! #endif
! #endif
  character (len=256) :: buf
  integer :: argc

  argc = iargc()
  if (argc < 9) then
     print *, "Wrong number of arguments !"
     call exit(1)
  endif

  call getarg(1,buf)
  read (buf, '(i5)') npapp

  call getarg(2,buf)
  read (buf, '(i5)') ndim

  call getarg(3, buf)
  read (buf, '(i5)') npx

  call getarg(4, buf)
  read (buf, '(i5)') npy

  call getarg(5, buf)
  read (buf, '(i5)') npz

  call getarg(6, buf)
  read (buf, '(i6)') spx

  call getarg(7, buf)
  read (buf, '(i6)') spy

  call getarg(8, buf)
  read (buf, '(i6)') spz

  call getarg(9, buf)
  read (buf, '(i9)') timesteps

  call getarg(10, buf)
  read (buf, '(i6)') appid

!  timesteps=1
!  if (argc > 7) then
!     call getarg(8, buf)
!     read (buf, '(i9)') timesteps
!  endif
end subroutine ! parse_args


subroutine allocate_2d()
  implicit none

! Matrix representation
! +-----> (x)
! |
! |
! v (y)
!

!   if (nproc > npy) then
!      off_x = mod(rank, npx) * spx
!   else if (nproc == npy) then
!      off_x = 0
!   else
!      off_x = rank*spx
!   endif

!   if (nproc > npx) then
!      off_y = rank/npy * spy
!   else 
!      off_y = 0
!   endif
  off_x = mod(rank, npx) * spx
  off_y = rank/npx * spy

!! Note the arguments order
  allocate(m2d(spy,spx))
end subroutine ! allocate_2d

subroutine release_2d()
  deallocate(m2d)
end subroutine

subroutine generate_2d()
  implicit none

  m2d = 1.0 * (rank+1) + 0.01 * ts
end subroutine ! generate_2d


subroutine allocate_3d()
  implicit none

! Matrix representation
!    ^ (z)
!   /
!  /
! +-----> (x)
! |
! |
! v (y)
!
  off_x = mod(rank, npx) * spx
  off_y = mod(rank/npx, npy) * spy
  off_z = (rank / npy / npx) * spz

  allocate(m3d(spy,spx,spz))
end subroutine ! allocate_3d


subroutine generate_3d()
  implicit none

  m3d = 1.0 * (rank+1) + 0.01 * ts
end subroutine ! generate_3d

end module couple_comm

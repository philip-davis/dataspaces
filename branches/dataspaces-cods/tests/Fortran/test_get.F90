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

!!
!! Test program for the geometry shared spaces.
!!

program test_gss
  use couple_comm
  implicit none
  include 'mpif.h'
  integer comm, mpi_rank
  integer color, key

  integer :: err

!!  real (kind=8), dimension(:,:,:), allocatable :: m3d_target
!!  real (kind=8), dimension(:,:,:), allocatable :: m3d_target

  call parse_args()

  call MPI_INIT(err)
  call MPI_BARRIER(MPI_COMM_WORLD, err)
  call MPI_COMM_RANK(MPI_COMM_WORLD, key, err)
  
  color = 2 
  call MPI_COMM_SPLIT(MPI_COMM_WORLD, color, key, comm, err)
  call MPI_COMM_RANK(comm, mpi_rank, err)

  call dspaces_init(npapp, appid, comm, err)
  call dspaces_rank(rank)
  call dspaces_peers(nproc)

  call ftimer_init
  call ftimer_start

!! call test_2d(n, loop)
!! call test_3d(n, loop)
!! call test_2d_block_block(n, loop)

  call allocate_3d();

!!  allocate(m3d_target(spy,spx,spz))

!! row_major = 0, column_major = 1
!!  call dspaces_set_storage_type(0)

!!  call dspaces_set_storage_type(0)

  do ts=1,timesteps
     if (rank == 1) then
        print '("At timestep ",i0)', ts
     endif
     call couple_read_3d(comm)
  enddo

  call dspaces_barrier()
  call dspaces_finalize()

  call MPI_BARRIER(comm, err)
  call MPI_FINALIZE(err)

end program


subroutine test_2d(n, loop)

  real (kind=8) :: a(n,n), ref_mat(n, n)
  integer :: i, j, k
  real (kind=8) :: f = 1.0
  integer :: xl, yl, xu, yu
  real (kind=8) :: min

  real (kind=8) :: tm_get, tm_start, tm_end


!!
!! Construct the reference matrix to check the results against.
!!
  do i=1,n
     do j=1,n
        ref_mat(i,j) = f
        f = f + 1.0
     enddo
  enddo

!! Test GET
  do i=1,loop
     print *, 'At i = ', i
     call dspaces_lock_on_read

     call ftimer_read(tm_start)

!     call get_from_coordinates(a, i-1, 1, 4, 10, 11, ref_mat, n)
!     call get_from_coordinates(a, i-1, 15, 20, 10, 11,ref_mat, n)
!     call get_from_coordinates(a, i-1, 10, 11, 1, 10,ref_mat, n)
!     call get_from_coordinates(a, i-1, 10, 11, 11, 20,ref_mat, n)

     !! Get the first column
!     call get_from_coordinates(a, i-1, 1, n, 1, 1,ref_mat, n)
     !! Get the last column
!     call get_from_coordinates(a, i-1, 1, n, n, n,ref_mat, n)
     !! Get the first row 
!     call get_from_coordinates(a, i-1, 1, 1, 1, n, ref_mat, n)
     !! Get the last raw
!     call get_from_coordinates(a, i-1, n, n, 1, n, ref_mat, n)

     call ftimer_read(tm_end)

     call dspaces_unlock_on_read

     tm_get = tm_end - tm_start
     !! call ftimer_log(tm_get, 1)

     !! call matrix_inc(ref_mat, n)
  enddo

  print *, "Oh god i = ", i

end subroutine


!!
!! Routine to test the exchange of 3D data objects.
!!
subroutine test_3d(n, loop)
  real (kind=8), dimension(:,:,:), allocatable :: m3d
  integer :: i, j, k
  real (kind=8) :: f = 1.0
  real (kind=8) :: tm_get, tm_start, tm_end
  integer :: rank, peers

  call dspaces_rank(rank)
  call dspaces_peers(peers)

!! Data decomposition (*, block, *)
  allocate(m3d(n, n/peers, n))
  j=(rank * n/peers)+1
  k=((rank+1) * n/peers)

!! Test GET
  do i=1,loop
     print *, 'At i = ', i
     call dspaces_lock_on_read

     call ftimer_read(tm_start)

     call dspaces_get("m3d", i-1, 8, j-1, 0, 0, k-1, n-1, n-1, m3d)

     call ftimer_read(tm_end)

     call dspaces_unlock_on_read

     tm_get = tm_end - tm_start
     !! call ftimer_log(tm_get, 1)
  enddo

  deallocate(m3d)
  print *, "Oh god i = ", i
end subroutine


subroutine couple_read_2d()
  use couple_comm
  implicit none

  call dspaces_lock_on_read()

  call dspaces_get("m2d", ts, 8, &
       off_x, off_y, 0, &
       off_x + spx-1, off_y + spy-1, 0, &
       m2d)

  call dspaces_unlock_on_read()
end subroutine ! couple_read_2d


subroutine compute_min()
  use couple_comm
  implicit none

  integer :: i, j, k
  real(kind=8) :: min

  min = m3d(1,1,1)
  do i=1,spy
     do j=1,spx
        do k=1,spz
           if (min > m3d(i,j,k)) then
              min = m3d(i,j,k)
           endif
        enddo
     enddo
  enddo


  print *, 'min value retrieved from the space is m3d(1,1,1): ', min, m3d(1,1,1)

end subroutine

subroutine couple_read_3d(comm)
  use couple_comm
  implicit none

  real (kind=8) :: tm_get, tm_start, tm_end

  integer :: M, N
  integer :: err
  integer :: comm
  integer (kind=8) lbb(3)
  integer (kind=8) ubb(3)


  call dspaces_lock_on_read("common_lock", comm)
!   call dspaces_get("M", 0, 4, &
!        0, 0, 0, &
!        0, 0, 0, &
!        M)

!   call dspaces_get("N", 0, 4, &
!        0, 0, 0, &
!        0, 0, 0, &
!        N)

  call ftimer_read(tm_start)

!!  call dspaces_get("/m3d_with_path_and_some_more_chars/value", ts-ts, 8, &
!!       off_x, off_y, off_z, &
!!       off_x+spx-1, off_y+spy-1, off_z+spz-1, &
!!       m3d, err)

  lbb(1) = off_x
  lbb(2) = off_y
  lbb(3) = off_z

  ubb(1) = off_x+spx-1
  ubb(2) = off_y+spy-1
  ubb(3) = off_z+spz-1

  call dspaces_get("/m3d_with_path_and_some_more_chars/value", ts-ts, 8, &
       ndim, lbb, ubb, &
       m3d, err)


  call compute_min()

  call ftimer_read(tm_end)

  call dspaces_unlock_on_read("common_lock", comm)

  tm_get = tm_end - tm_start
  !! call ftimer_log(tm_get, 1)
end subroutine ! couple_read_3d

subroutine test_equal_3d(a, b, n, m, p)
  implicit none
  integer :: n, m, p
  real (kind=8) :: a(n,m,p), b(n, m, p)
  integer :: i, j, k
  integer :: feq

  feq = 1

  do i=1,n
     do j=1,m
        do k=1,p
           if (a(i,j,k) /= b(i,j,k)) feq = 0
        enddo
     enddo
  enddo

  if (feq == 1) then 
     print *, "Equal matrices"
  else 
     print *, "Non equal matrices"
  endif

end subroutine

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
!! To run the program do:
!! ./app npapp npx npy npz spx spy spz timestems
!! The arguments mean:
!!     npapp - number of processors in the application
!!     np{x,y,z} - number of processors in the * direction (data decomposition)
!!     sp{x,y,z} - size of the data block in the * direction
!!     timestep  - number of iterations

program test_put
  use couple_comm
  implicit none
  include 'mpif.h'
  integer comm, mpi_rank
  integer color, key 

  integer :: err

!! This is an example on how to parse command line arguments in Fortran
!   if (iargc() == 0) then
!      print *, "Please provide command line arguments."
!      stop
!   endif
!   call getarg(1, arg)

  call parse_args()

  call MPI_INIT(err)
  call MPI_BARRIER(MPI_COMM_WORLD, err)
  call MPI_COMM_RANK(MPI_COMM_WORLD, key, err)
  
  color = 1
  call MPI_COMM_SPLIT(MPI_COMM_WORLD, color, key, comm, err)
  call MPI_COMM_RANK(comm, mpi_rank, err)

  call dspaces_init(npapp, 1,comm, err)
  call dspaces_rank(rank)
  call dspaces_peers(nproc)
  call ftimer_init()
  call ftimer_start()
  call allocate_3d()
  do ts=1,timesteps
     call generate_3d()
     call couple_write_3d(comm)
  enddo

!! call test_3d(n, loop)
!! call test_2d(128, 1001)
!! call test_2d_block_block ! (n, loop)

  call dspaces_barrier
  call dspaces_finalize

  call MPI_BARRIER(comm, err)
  call MPI_FINALIZE(err)

end program

subroutine matrix_inc(a, n)
  real (kind=8) :: a(n,n)

  do i=1,n
     do j = 1,n
        a(i,j) = a(i,j) + 10.0
     enddo
  enddo

end subroutine


subroutine test_3d(n, loop)
  real (kind=8), dimension(:,:,:), allocatable :: m3d
  integer :: i, j, k
  real (kind=8) :: f
  real (kind = 8) :: tm_tab(10), tm_start, tm_end
  integer :: rank, peers

  call dspaces_rank(rank)
  call dspaces_peers(peers)

  !! Data decomposition: (block, *, *)
  allocate(m3d(n/peers,n,n))

  f = 1.0 + rank * n * n/peers * n
  do i=1,n/peers
     do j=1,n
        do k=1,n
           m3d(i,j,k) = f 
           f = f + 1.0
        enddo
     enddo
  enddo

  j=(rank * n/peers)+1
  k=((rank+1) * n/peers)

  do i=1,loop
     call dspaces_lock_on_write

     call ftimer_read(tm_start)

     !! Syntax is: var_name, version, size_elem, {bounding box}, data values.
     !! call dspaces_put("m3d", i-1, 8, 0, j-1, 0, n-1, k-1, n-1, m3d(j:k,1:n,1:n))
     call dspaces_put("m3d", i-1, 8, 0, j-1, 0, n-1, k-1, n-1, m3d)

     call ftimer_read(tm_end)
     tm_tab(1) = tm_end - tm_start

     call dspaces_put_sync
     call ftimer_read(tm_end)
     tm_tab(2) = tm_end - tm_start

     print *, "Ok, I put ", rank+1, ", part of the array."
     call dspaces_unlock_on_write

     !! I have 2 timers to report
     !! call ftimer_log(tm_tab, 2)
  enddo

  deallocate(m3d)
end subroutine


subroutine test_2d(n, loop)
  integer :: dens(n)
  real (kind=8) :: a(n,n)
  integer :: i, j, k
  real (kind=8) :: f = 1.0
  real (kind = 8) :: tm_tab(10), tm_start, tm_end
  integer :: rank, peers

  call dspaces_rank(rank)
  call dspaces_peers(peer)

  do i=1,n
     do j=1,n
        a(i,j) = f
        f = f + 1.0
     enddo
     dens(i)=i+10
  enddo

!! Compute the distribution based on processor rank. Divide the matrix
!! based on rows: rank0 : [1 ..n/2), rank1: [n/2, n]
!! Distribution: (*, block)

 j=(rank * n/2)+1
 k=((rank+1) * n/2)
 do i=1,1001
    call dspaces_lock_on_write

    call dspaces_put("psi", rank, 4, rank, rank, rank, rank, rank, rank, i)
    call dspaces_put("dens", i-1, 4, j-1, 0, 0, k-1, 0, 0, dens(j:k))

    if (rank == 0) then
       call dspaces_put("m3d", i-1, 4, 0, 0, 0, 7, 7, 7, m3d)
    endif

    call ftimer_read(tm_start)
    !! Syntax is: var_name, version, size_elem, {bounding box}, data values.
    call dspaces_put("flux_curent", i-1, 8, 0, j-1, 0, n-1, k-1, 0, a(j:k, 1:n))
    call ftimer_read(tm_end)
    tm_tab(1) = tm_end - tm_start

    call dspaces_put_sync
    call ftimer_read(tm_end)
    tm_tab(2) = tm_end - tm_start

    print *, "Ok, I put ", rank+1, ", part of the array."
    call dspaces_unlock_on_write

    !! I have 2 timers to report
    !! call ftimer_log(tm_tab, 2)

    !! call matrix_inc(a, n)
 enddo
end subroutine


subroutine couple_write_2d()
  use couple_comm
  implicit none

  call dspaces_lock_on_write

  call dspaces_put("m2d", ts, 8, &
       off_x, off_y, 0, &
       off_x+spx-1, off_y+spy-1, 0, &
       m2d)
!  call dspaces_put_sync

  call dspaces_unlock_on_write
end subroutine ! couple_write_2d


subroutine couple_write_3d(comm)
  use couple_comm
  implicit none

  real (kind=8) :: tm_tab(2), tm_start, tm_end
  integer :: M = -3, N = 5
  integer :: err
  integer :: comm
  integer (KIND=8) lbb(3)
  integer (KIND=8) ubb(3)


  m3d(spy,spx,spz) = -m3d(spy,spx,spz)

  call dspaces_lock_on_write("common_lock", comm)
!   call dspaces_put("M", 0, 4, &
!        0, 0, 0, &
!        0, 0, 0, &
!        M)

!   call dspaces_put("N", 0, 4, &
!        0, 0, 0, &
!        0, 0, 0, &
!        N)

  !! Version number is forced to 0 (saves some memory space).
  call ftimer_read(tm_start)

!!  call dspaces_put("/m3d_with_path_and_some_more_chars/value", ts-ts, 8, &
!!       off_x, off_y, off_z, &
!!       off_x+spx-1, off_y+spy-1, off_z+spz-1, &
!!       m3d, err)

  
  lbb(1) = off_x
  lbb(2) = off_y
  lbb(3) = off_z

  ubb(1) = off_x+spx-1
  ubb(2) = off_y+spy-1
  ubb(3) = off_z+spz-1

  call dspaces_put("/m3d_with_path_and_some_more_chars/value", ts-ts, 8, &
       ndim, lbb, ubb, &
       m3d, err)



  call ftimer_read(tm_end)
  tm_tab(1) = tm_end - tm_start

  call dspaces_put_sync(err)!problem

  call ftimer_read(tm_end)
  tm_tab(2) = tm_end - tm_start

  call dspaces_unlock_on_write("common_lock", comm)
  !! call ftimer_log(tm_tab, 2)

  print *, 'min value written to the space is m3d(1,1,1): ', m3d(1,1,1)

end subroutine !couple_write_3d

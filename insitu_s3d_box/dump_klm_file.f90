#include "globalDefines.h"
!=========================================================================================
  subroutine dump_klm_files_mfab(work_ng, ncomp_ng, work_g, ncomp_g, num_ghost)
!=========================================================================================
! writes a file data
!-----------------------------------------------------------------------------------------
#ifdef F03INTERLANG
  use iso_c_binding
#endif
  use topology_m, only : myid, gcomm, ierr
  use param_m, only : nx,ny,nz, iorder
  use runtime_m, only : run_title, time, i_time, tstep
#ifdef MIXAVG
  use transport_m, only : computeCoefficients,getDiffusionCoeff   !MixAvg
#endif
#ifdef LEWIS
  use transport_m, only : computeCoefficients                     !Lewis
#endif
!  use premix_drvd_var_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in
  integer :: io = 6
  integer, intent(in) :: ncomp_ng, ncomp_g, num_ghost
  real, intent(in) :: work_ng(1:nx, 1:ny, 1:nz, ncomp_ng), work_g(1-num_ghost:nx+num_ghost, 1-num_ghost:ny+num_ghost, 1-num_ghost:nz+num_ghost, ncomp_g)
  
! local declarations
!-----------------------------------------------------------------------------------------
  integer :: icomp
  real, dimension(nx,ny,nz) :: field
  integer, save :: counter = 0
  character*4 count_ext
  character*4 seq
  character*9 tail
  character*100 head
  character*100 dirname
  character*100 tarcmd, tartgt, tarsrc

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

! message
  if(myid==0) write(io,*) 'Writing viz field files for Kwan-Liu Ma'

! increment counter
  counter = counter + 1


! set counter stamp for file
!  write(count_ext, '(I4.4)') i_time
  if(counter.le.9)then
    write(count_ext,'(A3,I1)') '000',counter 
  elseif((counter.gt.9).and.(counter.le.99))then
    write(count_ext,'(A2,I2)') '00',counter 
  elseif((counter.gt.99).and.(counter.le.999))then
    write(count_ext,'(A1,I3)') '0',counter 
  elseif((counter.gt.999).and.(counter.le.9999))then
    write(count_ext,'(I4)') counter 
  endif

  tail = '_'//trim(count_ext)//'.raw'

    dirname = '../post/klm/'//trim(run_title)//'_'//trim(count_ext)
    head = trim(dirname)//'/'//trim(run_title)//'_'
    if(myid == 0)then
      tarcmd = 'mkdir '//trim(dirname)
    endif
     call makedirectory(trim(dirname)//char(0))

!-----------------------------------------------------------------------------------------

  do icomp = 1, ncomp_ng
      write(seq,'(I4.4)') icomp 
      if(myid.eq.0)write(io,*) 'calling write files on nonghost data'
      call write_klm_field(work_ng(1:nx, 1:ny, 1:nz, icomp), &
                           trim(head)//'ng_FIELD_'//trim(seq)//trim(tail), &
                           1.0)
  enddo
  do icomp = 1, ncomp_g
      write(seq,'(I4.4)') icomp 
      if(myid.eq.0)write(io,*) 'calling write files on ghost data'
      call write_klm_field(work_g(1:nx, 1:ny, 1:nz, icomp), &
                           trim(head)//'g_FIELD_'//trim(seq)//trim(tail), &
                           1.0)
  enddo

!-----------------------------------------------------------------------------------------
  return
  end subroutine dump_klm_files_mfab


!=========================================================================================
  subroutine write_klm_field(field,filename,scaling)
!=========================================================================================
! 
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx,ny,nz,nx_g,ny_g,nz_g

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real, dimension(nx,ny,nz) :: field
  character(LEN=*) :: filename
  real :: scaling
!----------------------------------------------------------------------------------------
! local declarations

  integer i,j,k,L
!  real, allocatable, dimension(:,:,:) :: field_g
  real*4, allocatable, dimension(:,:,:) :: field_g
  integer field_recv_type ! a local variable defining the type for receiving each field
  integer xy_plane_type ! a local variable defining the type for the x-y planes 
                        ! corresponding to constant k
  integer sizeof        ! the size in bytes of the MPI input data-type 
  integer procid,req
  integer :: reqvec(2), statvec(MPI_STATUS_SIZE,2)

! local 4-byte real copies
  real*4 :: field_l(nx,ny,nz)
!  real :: field_l(nx,ny,nz)

  real*4 :: field_r(nx,ny,1)
!  real :: field_r(nx,ny,1)

  integer, parameter :: rsize = 4
!  integer, parameter :: rsize = 8

  integer mpi_real_type

  integer :: kc, jc

  character*2 ztag

  logical :: append_files = .true.

!----------------------------------------------------------------------------------------

  !call MPI_Barrier(gcomm,ierr)
  !if(myid==0)then
  !  write(7,*) 'local copy'
  !  call flush(7)
  !endif


! make a local copy of the field and scale
  field_l = real(field*scaling,kind(field_l))

! allocate space only only xy_id==0 to save memory
  if(xy_id==0)then
    allocate(field_g(nx_g,ny_g,1)); field_g=0.0
  endif 
!----------------------------------------------------------------------------------------
! define types:

  if(rsize==8)then
    mpi_real_type = MPI_REAL8
  else
    mpi_real_type = MPI_REAL4
  endif


! loop over z processors writing
  do k=0,zpes-1

   
! whether we do all this depends on whether we are splitting or concatenating in z.
  if(((append_files).and.zid==k).or.((.not.append_files).and.k==0))then

! loop over z planes
  do kc = 1, nz

! loop over x y processors
  do j=0,ypes-1
  do i=0,xpes-1

    procid=i+xpes*j

!   do the sends and receives
    if(xy_id == procid) call MPI_Isend(field_l(1,1,kc),nx*ny*1,mpi_real_type,&
                                      0,xy_id,xy_comm,reqvec(1),ierr)

    if(xy_id == 0) call MPI_IRecv(field_r(1,1,1),nx*ny*1,mpi_real_type, &
                                 procid,procid,xy_comm,reqvec(2),ierr)

!   waits
    if(xy_id==procid) call MPI_Wait(reqvec(1),statvec(:,1),ierr)
    if(xy_id==0) call MPI_Wait(reqvec(2),statvec(:,2),ierr)

!   local copy
    if(xy_id==0) field_g(i*nx+1:i*nx+nx,j*ny+1:j*ny+ny,1) = field_r(:,:,1)
   
    call MPI_Barrier(xy_comm,ierr)
  enddo
  enddo

!  call MPI_Barrier(gcomm,ierr)  
!  if(xy_id==0)then  
!    write(7,*) 'done MPI'  
!    call flush(7)  
!  endif  

!----------------------------------------------------------------------------------------
  if(xy_id==0)then !only the edge processes write

    if(append_files)then  
!     each zid writes at a time, appending.
!       write variable data
        call writebin_append(field_g(:,:,:),nx_g*ny_g*1,trim(filename)//char(0))
    else 
!     each z plane writes a separate file
      write(ztag,'(I2.2)') zid
      call writebin(field_g(:,:,:),nx_g*ny_g*nz,trim(filename)//'_'//trim(ztag)//char(0))
!      call writebin_append(field_g(:,:,:),nx_g*ny_g*1,trim(filename)//'_'//trim(ztag)//char(0))
    endif

  endif !xyid==0 writing
!----------------------------------------------------------------------------------------

  enddo !kc grid loop

  endif !((append_files).and.zid==k).or.((.not.append_files).and.k==0)

  if(append_files) call MPI_Barrier(zcomm,ierr)

  enddo !k processor loop

!-----------------------------------------------------------------------------------------
! deallocate
  if(xy_id==0) deallocate(field_g)
!-----------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------
  end subroutine write_klm_field

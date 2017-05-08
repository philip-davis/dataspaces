#include "globalDefines.h"
!========================================================================================
  program s3d
!========================================================================================
! Direct Numerical Simulation of the 3-D compressible Navier-Stokes
! equations with detailed chemical reactions
!----------------------------------------------------------------------------------------
  use param_m
  use topology_m
  use chemkin_m
  use runtime_m

  implicit none
!----------------------------------------------------------------------------------------
! integer variable declarations

  integer io          !output io unit
  character nodeid_str*18, rank_ext*5
  integer ierr_adios, L
!----------------------------------------------------------------------------------------
! start initialization of MPI parameters

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myid, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_dup (MPI_COMM_WORLD, gcomm,ierr)

  write(rank_ext,'(i5.5)') myid
  nodeid_str = 'Rank '//trim(rank_ext)//' is node'
  !call print_xtnodeid(trim(nodeid_str)//char(0))
!----------------------------------------------------------------------------------------
! read data from s3d.in

  call read_input(6)
!----------------------------------------------------------------------------------------
! set output to screen or file

  if(i_write.eq.0) then
    io=6
  else
    io=7
    if(myid.eq.0) then
      open(unit=io,file='../data/'//trim(run_title)//'.out', status='REPLACE')
    endif
  endif
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'=')
    write(io,*) 'Hello, and welcome to S3D!'
    call write_header(io,'=')
    call writeopts()
  endif
!----------------------------------------------------------------------------------------
! get start date and time and write to file

  if(myid.eq.0) then
    call write_header(io,'-')
    call date_and_time(dat_1,tim_1)
    write(io,*) 'this run started at:'
    write(io,*)
    call write_date_and_time(dat_1,tim_1,io)
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------------------------
! initialize primary modules
!----------------------------------------------------------------------------------------
! initialize chemkin module

  call initialize_chemkin(io,myid,ierr,gcomm)


! initialize adios
#ifdef ADIOS

if( myid == 0 ) then
    DO L=1,n_species
        call add_species_to_xml( L, trim(species_name(L))//char(0) );
    enddo
endif

! wait for root to finish updating xml file
call MPI_Barrier(gcomm, ierr )
!call terminate_run(io,0)
  call adios_init( "s3d.xml"//CHAR(0), ierr_adios )
  if( myid == 0 ) then
      if (ierr_adios< 0 ) then
          write (*,*) "adios broken"
      else
          write(*,*) "Initialized ADIOS; ", ierr_adios
      endif
  endif
#endif

! initialize param module (must be done after initializing chemkin module)

  call initialize_param(io,myid,ierr,gcomm)

! intialize topology module (must be done after initializing param module)

  call initialize_topology(io,nx,ny,nz,npx,npy,npz,iorder,iforder)

! sync processors

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! start tasks here
!----------------------------------------------------------------------------------------
! mode of operation

  if(run_mode == 'solve') then          !solve governing equations
    call solve_driver(io)
  else
    call post_driver(io)      !post process results
  endif
!----------------------------------------------------------------------------------------
! get final date and time and write to file

  if(myid.eq.0) then
    call write_header(io,'-')
    write(io,*) 'S3D started at:'
    write(io,*)
    call write_date_and_time(dat_1,tim_1,io)
    write(io,*)
    write(io,*) 'S3D ended at:'
    write(io,*)
    call date_and_time(dat_2,tim_2)
    call write_date_and_time(dat_2,tim_2,io)
  endif
!----------------------------------------------------------------------------------------
! write closing statement

  if(myid.eq.0) then
    call write_header(io,'=')
    write(io,*) 'Program finished normally, goodbye!'
    call write_header(io,'=')
  endif

! close output file if it exists

  if(i_write.ne.0) then
    if(myid.eq.0) close(io)
  endif

! clean up io
#ifdef ADIOS
if( myid == 0 ) write(io,*) 'finalizing adios'
  call adios_finalize( myid, ierr_adios )
#endif
!----------------------------------------------------------------------------------------
! deallocate arrays

  call allocate_chemkin_arrays(-1)
!----------------------------------------------------------------------------------------
! cleanup MPI processes

  call MPI_Finalize(ierr)
!----------------------------------------------------------------------------------------
  end program s3d

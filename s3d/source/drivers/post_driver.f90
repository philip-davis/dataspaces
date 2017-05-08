#include "globalDefines.h"
!----------------------------------------------------------------------
subroutine post_driver(io)
  ! -----------------------------------------------------------------------------
  !
  ! Routine for post-processing DNS data
  !
  ! This routine can have extremely variable behaviour; to control what
  ! it does two mechanisms are used: compiler directives, and the flags
  ! set from the input file post.in. The compiler directives determine if
  ! support for a particular analysis group is built, and the flag set by
  ! post.in determines if the analysis will be run. The current groups
  ! are:
  !
  ! POST_TAU
  ! POST_RANS_BUNSEN
  ! POST_LES_FILTER
  ! POST_KLM_OUTPUT
  ! POST_SURFACE
  ! POST_HDF5
  !
  ! -----------------------------------------------------------------------------


  ! =============================================================================
  ! -----------------------------------------------------------------------------
  ! Core S3D modules 
  ! -----------------------------------------------------------------------------
  
  use param_m                   ! initialized in main.f90
  use topology_m                ! initialized in main.f90
  use chemkin_m                 ! initialized in main.f90

  use runtime_m, only : i_time, time
  use variables_m
  use work_m
  use grid_m
  use reference_m, only: initialize_reference
  use transport_m
  use derivative_m
  use thermchem_m
  use runtime_m, only : run_title
  use mixfrac_m
  !use tracer_m

#ifdef MPIIO
  use mpi_io_m
#endif

  ! -----------------------------------------------------------------------------
  ! Analysis modules - only include if necessary source has been built and linked
  ! -----------------------------------------------------------------------------
#ifdef POST_HDF5
  use s3d_hdf_interface_m
#endif  

#ifdef POST_RANS_BUNSEN
  use bunsen_post_m
  use zclookup_m, only : SpecToProg
#endif
  
#ifdef POST_LES_FILTER
  use par_filter_m
#endif

#ifdef VECTORVERSION
  use gibbs_rxn_table_m, only: init_gibbstable
#endif
  
#ifdef BUILD_TAU_POST_SOURCES
#define POST_TAU
#endif

#ifdef POST_TAU
  use timescales_m, only : filter_field_2pss
#endif

! Remove this for now - needs updating for q not in variables module
  !use isosurf_m


  ! -----------------------------------------------------------------------------
  ! Variable declarations
  ! -----------------------------------------------------------------------------

  implicit none

  ! Flags for analysis groups
  integer tau_post
  logical rans_bunsen
  logical dump_klm
  logical dump_hdf5
  logical les_filter
  logical curvature
  integer tecplot_skip
  logical post_pqr
  logical pstrain_analysis
  integer io_morph
  integer isosurf
  integer io_method_input

  ! I/O bits
  integer, intent(in) :: io         ! output io unit
  integer :: io_logfile = 15
  character*100 :: filename
  logical :: exists, file_end=.false.

  character precsn, dirtime*10
  integer idummy

  ! -----------------------------------------------------------------------------
  ! Interfaces
  ! -----------------------------------------------------------------------------
  
  interface
     subroutine read_savefile(io, precsn, dirtime)
       integer, intent(in) :: io
       character, intent(in), optional :: precsn
       character(*), intent(in), optional :: dirtime
     end subroutine read_savefile
  end interface

  ! =============================================================================
  ! -----------------------------------------------------------------------------
  ! S3D Initialization
  ! -----------------------------------------------------------------------------

  call initialize_derivative(io)                ! initialize derivative module
  ! Ramanan - 07/16/06 Reference must be initialized before transport
  call initialize_reference(io,pr)          ! must be done after transport
  call initialize_transport(io)             ! initialize transport module
  call initialize_grid(io)                  ! must be done after reference
  call initialize_thermchem(io)             ! must be done after reference

  n_reg = 1

  call allocate_variables_arrays( 1 )       ! allocate memory for variables
  call allocate_work_arrays( 1 )            ! allocate memory for work arrays

#ifdef VECTORVERSION
  call init_gibbstable
#endif

#ifdef MPIIO
  if (io_method .EQ. 1) then
      call mpi_io_set_filetype(0)
  endif
#endif

  !if(tracer_ctrl.eq.1)  call initialize_tracer(io)


  ! -----------------------------------------------------------------------------
  ! Read post.in input file
  ! -----------------------------------------------------------------------------

  filename='../input/post.in'
  call inquire_about_input_file(filename,io)

  if(myid.eq.0) then
     open(unit=1,file=trim(filename),status='old')

     read(1,*)
     read(1,*)
     read(1,*)

     read(1,*) dump_klm
     read(1,*) curvature
     read(1,*) rans_bunsen
     read(1,*) les_filter
     read(1,*) tau_post
     read(1,*) dump_hdf5
     read(1,*) tecplot_skip
     read(1,*) post_pqr
     read(1,*) pstrain_analysis
     read(1,*) io_morph
     read(1,*) isosurf

     close(1)

     ! Check validity of options list
#ifndef POST_HDF5
     if(dump_hdf5) then
        write(io,*)
        write(io,*)'Warning: dump_hdf5 is true, but POST_HDF5 is not defined'
        term_status=1
     endif
#endif

#ifndef POST_RANS_BUNSEN
     if(rans_bunsen) then
        write(io,*)
        write(io,*)'Warning: rans_bunsen_strat is true, but POST_RANS_BUNSEN_STRAT is not defined'
        term_status=1
     endif
#endif

#ifndef POST_LES_FILTER
     if(les_filter) then
        write(io,*)
        write(io,*)'Warning: les_filter is true, but POST_LES_FILTER is not defined'
        term_status=1
     endif
#endif

#ifndef POST_TAU
     if(TAU_POST.gt.0) then
        write(io,*)
        write(io,*)'Warning: tau_post is true, but POST_TAU is not defined'
        term_status=1
     endif
#endif

#ifndef PQR_CLASSIFICATION
     if(post_pqr) then
        write(io,*)
        write(io,*)'Warning: pqr_classification is true, but PQR_CLASSIFICATION is not defined'
        term_status=1
     endif
#else
    if(post_pqr.and. (.not. dump_hdf5 ) ) then
        write(io,*)
        write(io,*)'Warning: pqr_classification is true, but dump_hdf5 is false'
        term_status=1
    endif

#endif


  endif
  ! call check_term_status(io,0) ! This is a bit harsh...

  call MPI_Bcast(dump_klm            , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(curvature           , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(rans_bunsen         , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(les_filter          , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(tau_post            , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(dump_hdf5           , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(tecplot_skip        , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(post_pqr            , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(pstrain_analysis    , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(io_morph            , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(isosurf             , 1, MPI_INTEGER, 0, gcomm, ierr)

  ! -----------------------------------------------------------------------------
  ! Initialize user defined analysis routines
  ! -----------------------------------------------------------------------------

#ifdef POST_LES_FILTER
  if( les_filter ) then
     call init_par_filter(io)
  endif
#endif

#ifdef POST_HDF5
   call initialize_hdf5_interface(io)
#endif

#if (defined  POST_RANS_BUNSEN)
     if( rans_bunsen ) then
        !~~~ analysis from the rans_bunsen.f90 module ~~~
   call initialize_rans_analysis(io)
     endif
#endif
  ! -----------------------------------------------------------------------------
  ! Read the file containing the list of files to be post-processed
  ! -----------------------------------------------------------------------------

  if(myid==0) then
     filename = '../input/'//trim(run_title)//'.savefile.log'
     inquire(file=filename, exist=exists)
     if(.not. exists) then
        write(io,*)
        write(io,*)'ERROR: file does not exist:',trim(filename)
        write(io,*)'ABORTING!!!'
        term_status=1
     endif
  endif
  call check_term_status(io,0)

  if(myid==0) then
     open(unit=io_logfile, file=filename, status='old', form='formatted')
     ! chew up the header lines of this file
     read(io_logfile,*) 
     read(io_logfile,*) 
  endif


  ! =============================================================================
  ! -----------------------------------------------------------------------------
  ! START loop over all time steps
  ! -----------------------------------------------------------------------------
  LOGENTRYLOOP: do   ! keep doing this until the log file runs out of entries...
  
     ! --------------------------------------------------------------------------
     ! Load the data for this timestep and setup S3D arrays
     ! --------------------------------------------------------------------------

     if(myid==0) then
        file_end = .true.
        read(io_logfile,50,end=60,err=60) dirtime, idummy, precsn
#ifdef BUNSENPOST
50      format(a9, 6x, i7, 6x, a1)
#else
50      format(a10, 6x, i7, 6x, a1)
#endif
        ! if we got here, we successfully read a line from the log file
        ! so set the "file_end" flag to false
        file_end = .false.
        !Ramanan - 07/20/05..  and check for illegal values...
        if( i_time<0 .or. time<0 .or. trim(dirtime)=='') file_end = .true. 
60      continue
     endif

     !    broadcast the file status - if the end of the file was reached, 
     !    we will gracefully exit the code.
     call MPI_Bcast(file_end,1,MPI_LOGICAL,0,gcomm,ierr)
     if(file_end) then
        ! Changed by Ramanan Sankaran
        ! Dont quit like that... do the finishing steps. 
        exit LOGENTRYLOOP
        !call terminate_run(io,0)
     endif

          
     ! broadcast the information on the file to be post-processed 
     ! nondimensionalize the time
     if(myid == 0) write(io,*) 'Processing  ', dirtime
     call MPI_Bcast(precsn , 1, MPI_CHARACTER, 0, gcomm, ierr )
     call MPI_Bcast(dirtime, 10, MPI_CHARACTER, 0, gcomm, ierr )

     call read_savefile( io, precsn, dirtime )  
     call set_variables( io ) 

     !if( tracer_ctrl.eq.1 ) then
     !   call read_tracer_savefile(io, '../data/tracer-'//trim(dirtime))
     !end if

     ! --------------------------------------------------------------------------
     ! Initialize derived quantities
     ! --------------------------------------------------------------------------


     ! calculate mixture fraction
     ! RG: This causes problems for cases without streams.in setup 
      call specToMixfr(yspecies)

     ! calculate transport coefficients
#ifdef MIXAVG
     call computeCoefficients(pressure,temp,yspecies,1.0/volum)
#endif

#ifdef LEWIS
     ! RG: I'm not sure that cpmix and temp are set here; if not, need to call
     !     calc_temp from thermochem_m
     call computeCoefficients(cpmix, temp)
#endif

     ! --------------------------------------------------------------------------
     ! START user post processing routines
     ! --------------------------------------------------------------------------
     

#ifdef POST_HDF5
     if( dump_hdf5 ) then
         call write_hdf5(io, ierr)
         !if( pstrain_analysis ) then
         !    call strain_scalar_gradient(io)
         !endif
     endif
#endif

#ifdef POST_LES_FILTER
     if( les_filter ) then
        call filter_field(io)
     endif
#endif

#ifdef PQR_CLASSIFICATION
     if( post_pqr ) then
       if( myid == 0 ) then
         write(io,*) 'Performing PQR-based classification'
       endif
        call pqr_classification(io)
     endif
#endif

     if( tecplot_skip > 0 ) then
! Remove this for now - needs updating for q not in variables module
        !call write_tecplot_skip(io, tecplot_skip)
     endif

#if (defined  POST_RANS_BUNSEN) 
     if( rans_bunsen ) then
        !~~~ analysis from the rans_bunsen.f90 module ~~~
        call rans_analysis_driver(io,.false.)
     endif
#endif

#ifdef POST_TAU
        !~~~ analysis for time scales ~~~
     if( tau_post.eq.1 ) then
        call filter_field_1pss(io, .false. )
     elseif( tau_post.eq.2 ) then
        call filter_field_2pss(io, .false. )
!       call filter_field_2pss_TT(io,.false. )
     else
        if(myid.eq.0)write(io,*)'no tau_post options selected'
     endif
#endif

!#ifdef POST_KLM_OUTPUT
#ifdef POST_KLM
     if( dump_klm ) then
        !~~~ write data fields to be visualised by Hong Feng ~~~
        call dump_klm_files(io)
     endif
#endif
  
     if(io_morph.ne.0)then

#ifdef MPIIO
       io_method_input = io_method
! I/O method: 0:Fortran I/O, 1:MPI-IO, 2:PnetCDF, 3:HDF5
       io_method = io_morph - 1   
  if (io_method .EQ. 1) then
      call mpi_io_set_filetype(0)
  endif
#endif

       call write_savefile(io, 's')

#ifdef MPIIO
  if (io_method .EQ. 1) call mpi_io_set_filetype(-1)  
     io_method=io_method_input
#endif
     endif  !io_morph

     if(isosurf .eq. 1 ) then
! Remove this for now - needs updating for q not in variables module
       !call  extract_isosurface(io)
       !call write_single_isosurface(io)
     endif

     ! --------------------------------------------------------------------------
     ! END user post processing routines
     ! --------------------------------------------------------------------------




  enddo LOGENTRYLOOP
  if(myid==0) close( io_logfile )
  ! End of loop over all time steps 
  ! =============================================================================
  ! -----------------------------------------------------------------------------
  ! Do the final analysis steps
  ! -----------------------------------------------------------------------------

#if (defined  POST_RANS_BUNSEN)
  if( rans_bunsen ) then
     !~~~~ analysis from the rans_bunsen.f90 module~~~~
     call rans_analysis_driver(io,.true.)
  endif
#endif

#ifdef POST_TAU
  !~~~ analysis for time scales~~~~~~~~~~~~
   if( tau_post.eq.1 ) then
      call filter_field_1pss(io, .true. )
   elseif( tau_post.eq.2 ) then
      call filter_field_2pss(io, .true. )
!     call filter_field_2pss_TT(io,.true. )
   else
      if(myid.eq.0)write(io,*)'no tau_post options selected'
   endif
#endif

  ! =============================================================================
  ! -----------------------------------------------------------------------------
  ! Deallocate arrays
  ! -----------------------------------------------------------------------------
  call allocate_variables_arrays(-1)
  call allocate_work_arrays(-1)
  call allocate_grid_arrays(-1)
  
  return
end subroutine post_driver
! =============================================================================
! =============================================================================

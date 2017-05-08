include "globalDefines.h"
! $Id: solve_driver.f90,v 1.4.2.4.2.1 2006/05/16 22:51:00 rsankar Exp $
!----------------------------------------------------------------------
  subroutine solve_driver(io)
!----------------------------------------------------------------------
  !use topology_m, only : gcomm, ierr, myid,  npes
  use topology_m

  use param_m, only : dat_1,nx_g, ny_g, nz_g, io_method
  use param_m, only : nx, ny, nz, nsc, n_reg, nvar_tot

  use reference_m, only : time_ref
  use reference_m, only : initialize_reference  !routine reference

  use runtime_m, only : i_restart, i_time_res, i_time, i_time_end
  use runtime_m, only : i_time_mon, i_time_tec, i_time_save
  use runtime_m, only : time, tstep, time_save, time_save_inc

  use filter_m, only : initialize_filter, allocate_filter_arrays  !routine references

  use rk_m, only : q_err_max
  use rk_m, only : i_time_cont
  use rk_m, only : initialize_rk, allocate_rk_arrays  !routine references


  use thermchem_m, only : initialize_thermchem        !routine reference
  use thermchem_m, only : allocate_thermchem_arrays   !routine reference

  use variables_m, only : temp, pressure
  use variables_m, only : allocate_variables_arrays  !routine reference

  use work_m, only : allocate_work_arrays  !routine reference

  use grid_m, only : initialize_grid  !routine reference
  use grid_m, only : allocate_grid_arrays  !routine reference
  use grid_m, only : unif_grid_all  !routine reference

  use bc_m, only : initialize_bc  !routine reference
  use bc_m, only : allocate_bc_arrays  !routine reference

  use transport_m, only : pr

  use transport_m, only : initialize_transport  !routine reference
  use transport_m, only : allocate_transport_arrays  !routine reference

  use derivative_m, only : initialize_derivative  !routine reference
  use derivative_m, only : allocate_derivative_arrays  !routine reference

#ifdef VECTORVERSION
  use gibbs_rxn_table_m, only: init_gibbstable
#endif

  !use tracer_m
  use mixfrac_m ! dol

#ifdef MPIIO
  use mpi_io_m
#endif

  use temporalfeed_m, only: end_temporalfeed, write_temporalfeed_data, & 
                        i_time_write, i_temporalfeed
!                        feed_save, feed_save_inc, i_temporalfeed
  implicit none
!----------------------------------------------------------------------
! declarations of variables passed in

  integer, intent(in) :: io

! declarations of local variables

  integer :: L, i, j
  real :: temp_max, pres_max  !maximum temperature and pressure in domain
  character*10 :: tim_start, tim_stop     !for start and end time (wall clock)
  real :: time_taken
  double precision cost_time(2)
  integer :: time_start(8), time_stop(8)
  integer :: tec_del
  integer :: tim_watch(8)

  ! Moving this here as a prelude to allocating through boxlib
  real, allocatable :: q(:,:,:,:,:) !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)


  ! Allocate rk registers (incl. solution vector)
!----------------------------------------------------------------------
! set variable tec_del (0=save all tecplot files, 1=delete previous file upon new write)

  tec_del=1
!----------------------------------------------------------------------
  call allocate_work_arrays(1)
  call initialize_reference(io,pr)
  call initialize_rk(io)
  call initialize_transport(io)
  call initialize_filter(io)
  call initialize_derivative(io)

!-- initialize thermo and chemistry module (must be done after reference module)
  call initialize_thermchem(io)

!-- initialize grid module (must be done after reference module)
  call initialize_grid(io)

!-- initialize boundary conditions module
  call initialize_bc(io)

#ifdef MPIIO
  ! wkliao: define MPI I/O file type to be used to define file view
  if (io_method .EQ. 1) then
      call mpi_io_set_filetype(0)
  endif
#endif

!-- initialize tracer module if used
  !if(tracer_ctrl.eq.1)  call initialize_tracer(io)

!-- Allocate workspace and initialize field
!    Need rk module initialied to set n_reg
  allocate(q(nx,ny,nz,nvar_tot,n_reg));   q=0.0
  call initialize_field(io,q,nx,ny,nz,nvar_tot,n_reg)
  if(myid.eq.1)write(io,*)'field initialized with i_temporalfeed=',i_temporalfeed

#ifdef VECTORVERSION
  call init_gibbstable
#endif

  call allocate_mixFrac_arrays( 1 )       ! dol

!-- generate active file (must be done last)
  call generate_active_file(io)

#ifndef SYSTEMCALLWONTWORK
  call tar_input_files(io)
#endif


!----------------------------------------------------------------------
! write files for the initial condition
  if(i_restart.eq.0) then
    call write_savefile(io, 'd')
!    call write_tecplot_skip(io,2)
!    call write_tecplot_yzplane(io, 0.5)
! always write inlet turbulence data for first timestep
     if(i_temporalfeed.eq.1) then
         call write_temporalfeed_data(io)
         if(myid.eq.0)write(io,*)'first temporal feed slice written'
     endif  
!if(tracer_ctrl.eq.1)    call write_tracer_tecplot(io)

  else
!    call write_tecplot_skip(io,2)
!    call write_tecplot_yzplane(io, 0.5) 
  endif
!---------------------------------------------------------------------- !---------------------------------------------------------------------- ! check spatial resolution requirement of initial field


  if(i_time_res.ge.0) then
      if( unif_grid_all .eq. 1 ) & 
     call check_resolution(io, q, nx, ny, nz, nvar_tot, n_reg)
  endif
!----------------------------------------------------------------------
! start loop over all time steps
!----------------------------------------------------------------------
  i_time=0

  call MPI_Barrier(gcomm, ierr)
  call date_and_time(dat_1,tim_start)
  call date_and_time(values=time_start(:))
  cost_time(2) = MPI_Wtime()

  do while (i_time < i_time_end)
!----------------------------------------------------------------------

    i_time = i_time + 1

#ifndef ARK
call set_timestep(io, q, nx, ny, nz, nvar_tot, n_reg)
#endif

!if(tracer_ctrl.eq.1)then
!    if (i_time-1 .eq. i_tracer_time) then
!      call advance_tracer(1)
!    end if
!endif

    call integrate(io, q, nx, ny, nz, nvar_tot, n_reg)
    time=time+tstep

!if(tracer_ctrl.eq.1)then
!    if (i_time .eq. i_tracer_time + num_tracer_half_step) then
!      call advance_tracer(2)
!      call advance_tracer(3)
!    else if (i_time .eq. i_tracer_time + 2*num_tracer_half_step) then
!      call advance_tracer(4)
!      i_tracer_time = i_time
!      call insert_tracers(io, 400, .true.)
!    end if
!endif


!   sync processors
!   Commented out by Ramanan - 04/18/05
!    call MPI_Barrier(gcomm,ierr)

!   monitor min/max values and look at active file for updates
    if(i_time_mon.gt.0) then
      if((i_time.le.10).or.(mod(i_time,i_time_mon).eq.0)) then
        call monitor(io,temp_max,pres_max, q, nx, ny, nz, nvar_tot, n_reg )
        if(myid.eq.0)  then
          call date_and_time(values=tim_watch(:))
          write(io,100) tim_watch(5), tim_watch(6), tim_watch(7), i_time, time *(time_ref), tstep*(time_ref), &
                          q_err_max, temp_max, pres_max 
        end if
      endif
      if(mod(i_time,i_time_mon).eq.0) call read_active_file(io)
    endif

!   FIELD DATA
!   write save file
    if(  (i_time >= i_time_end)  .or. &  !Last time-step
         ( (i_time_save > 0) .and.  (mod(i_time,i_time_save) == 0) ) ) then
        call write_savefile(io, 'd')
    endif
    if(  (i_time_save > 0) .and. (time*time_ref > time_save) ) then
        time_save=time_save+time_save_inc
        call write_savefile(io, 's')
    endif

!   write tecplot file
    if(  (i_time_tec > 0 .and. mod(i_time,i_time_tec) == 0) ) then
!    if(  i_time >= i_time_end .or.  &
!        (i_time_tec > 0 .and. mod(i_time,i_time_tec) == 0) ) then
!      call write_tecplot_skip(io,2)
!    call write_tecplot_yzplane(io, 0.5)


#ifdef NETCDF
!     call write_netcdf_xyplane(io, 0.5)
#endif

    endif
    
!   write time evolving inlet turbulence data
    if(i_temporalfeed.eq.1)then
!esr    if(time .gt. feed_save .or. i_time .ge. i_time_end) then
    if(mod(i_time,i_time_write).eq.0 .or. i_time .ge. i_time_end) then
      call write_temporalfeed_data(io)
!esr      feed_save=feed_save+feed_save_inc
      if(myid.eq.0)write(io,*)'temporal feed slice written'
    end if
    endif

!if(tracer_ctrl.eq.1)then
!!   TRACER DATA
!!   write save files
!    if(  (i_time >= i_time_end)  .or. &  !Last time-step
!         ( (i_time_save > 0) .and.  &
!         (mod(i_time,int(trace_save_fctr*i_time_save)) == 0) ) ) then
!        call write_tracer_savefile(io)
!
!    endif
!
!!   tecplot
!!    if(mod(i_time, itracertec).eq.0) call write_tracer_tecplot(io)
!
!endif ! tracer_ctrl

!   check spatial resolution by analyzing the energy spectrum
    if( i_time_res.gt.0  ) then
       if ( mod(i_time,i_time_res).eq.0 ) then
         if( unif_grid_all.eq.1 )  then
            call check_resolution(io)
          endif
        endif
    endif

!   flush io
#if defined(PC) || defined(SGI)
#else
      if(myid.eq.0) call flush(io)
#endif

!----------------------------------------------------------------------
! end of loop over all time steps

  enddo
  call MPI_Barrier(gcomm, ierr)

!filter feed data     if(myid==0)write(io,*)'Filtering and writing the velocity field'
!filter feed data     call filter_feeddata(io)
!filter feed data     if(myid==0)write(io,*)'Filtering of feeddata is done'

  cost_time(1) = MPI_Wtime()

  call date_and_time(dat_1,tim_stop)
  if(myid==0) then
     call write_header(io,'=')
     write(io,*)'Timestepping began at: '
     call write_date_and_time(dat_1,tim_start,io)
     write(io,*)'Timestepping ended at: '
     call write_date_and_time(dat_1,tim_stop,io)
     call date_and_time(values=time_stop(:))
     time_taken = &
     60.0*real(time_stop(6)-time_start(6))&
     +  real(time_stop(7)-time_start(7))&
     +0.001*real(time_stop(8)-time_start(8))
     write(io,'(A, F8.1, A)') 'S3D Time taken: ', (cost_time(1)-cost_time(2)), ' s'
     time_taken = time_taken / i_time * npes / (nx_g*ny_g*nz_g) * 1.0e6
!     write(io,110) 'S3D Cost: ', time_taken, ' microseconds per gridpoint per tiemstep'
     write(io,110) 'S3D Cost : ', &
                   ( cost_time(1) - cost_time(2) ) / i_time / (nx_g*ny_g*nz_g) * 1.0e6 * npes, &
                   ' microseconds per gridpoint per tiemstep'

     call write_header(io,'=')
     
  endif

!----------------------------------------------------------------------
! deallocate arrays

  deallocate(q)
  call allocate_mixFrac_arrays( -1 )       ! dol
  call allocate_derivative_arrays(-1)
  call allocate_filter_arrays(-1)
  call allocate_thermchem_arrays(-1)
  call allocate_transport_arrays(-1)
  call allocate_rk_arrays(-1)
  call allocate_variables_arrays(-1)
  call allocate_work_arrays(-1)
  call allocate_grid_arrays(-1)
  call allocate_bc_arrays(-1)
#ifdef MPIIO
  if (io_method .EQ. 1) call mpi_io_set_filetype(-1)  
#endif
  !if(tracer_ctrl.eq.1)  call deallocate_tracer
  if(i_temporalfeed.ne.0) call end_temporalfeed
!----------------------------------------------------------------------
! format statements

  100 format(i2.2,':',i2.2,':',i2.2,1x,i8,2x,1pe10.4,2x,1pe9.3,2x,1pe9.3,2x,0pf7.2,2x,0pf6.2)
  110 format(1a, 1f8.1,1a )
!----------------------------------------------------------------------
  return
  end subroutine solve_driver

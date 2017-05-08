#include "globalDefines.h"
!$Id: init_field.f90,v 1.2.2.6.2.1 2006/06/08 18:37:52 rsankar Exp $
!========================================================================================
subroutine initialize_field(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
!========================================================================================
! initializes primitive and solution variables amoung other things
!----------------------------------------------------------------------------------------
  use topology_m

  use runtime_m, only : run_title, time_save, time_save_inc, i_restart, time, i_time
  use runtime_m, only : tstep, time_restart

#ifndef BUILD_LIBS3D
  use rk_m, only : tstep_init, cont_switch, tstep_min
#endif

  use param_m, only : nsc, n_elem, n_spec, run_mode, nx, ny, nz, nx_g, ny_g, nz_g
  use param_m, only : vary_in_x, vary_in_y, vary_in_z

  use reference_m

  use turbulence_m

  use thermchem_m

  use variables_m

  use bc_m, only : store_hard_bc, nrf_x0, nrf_xl, nrf_y0, nrf_yl, nrf_z0, nrf_zl, pout

  use grid_m, only : xmin, xmax, ymin, ymax, zmin, zmax, x, y

  use frozenfeed_m, only : write_frozenfeed_data, get_turbulence_from_feeddata, &
                           init_frozenfeed_read

  use temporalfeed_m, only: init_temporalfeed, i_temporalfeed, i_frozenfeed

!  use bunsen_post_m, only: print_xzavgs

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer, intent(in) :: io
  integer, intent(in) :: qnx, qny, qnz, qnvar_tot, qn_reg
  real, dimension(qnx,qny,qnz,qnvar_tot,qn_reg) :: q !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)

! local declarations

  real :: a_l             !local speed of sound
  real :: a_max           !maximum global speed of sound
  real :: a_min           !minimum global speed of sound
  real :: Lxyz(3)         !domain lengths in each direction
  real :: L_max           !longest distance in domain
  real :: a_time_max      !time for max wave to travel the longest distance (sec)
  real :: a_time_min      !time for min wave to travel the longest distance (sec)
  integer :: dir, i1,i2,i3
  real :: L1,L2,L3

  real :: half_width, decay_factor, s, sigma
  real :: fracYr, ycent1, ycent2, u_filter  !variables taken from dol's tj code.
  real :: uprime(nx, ny, nz, 3)
  real :: uprofile(ny)
  integer j

  real :: pout_inputfile

  interface
  subroutine read_savefile(io, precsn, dirtime)
    integer, intent(in) :: io
    character, intent(in), optional :: precsn
    character(*), intent(in), optional :: dirtime
  end subroutine read_savefile
  end interface

!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing variables...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! allocate arrays

  call allocate_variables_arrays(1)
!----------------------------------------------------------------------------------------
! initialize primative variables (yspecies, temp, pressure, and u only!)
! with user-specified routines.
!
! Note: Upon restart (i_restart=1), user-specified initialization routines should only 
!       initialize any necessary modules and not the primative variables. Any primative
!       variables initialized with i_restart=1 will be overwritten when the restart
!       files are read.
!----------------------------------------------------------------------------------------
  call MPI_Barrier(gcomm, ierr)

  i_time=0
  time=0.0
  time_save=time_save_inc
  time_restart=0.0

  select case (trim(run_title))

! add initialization cases here
! add initialization cases here
  case ('hcci')
     call initialize_hcci(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('ignition_test')
     call initialize_ignition_test(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('ignition_H2_test')
     call initialize_ignition_H2_test(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('bomb')
     call initialize_bomb(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('1d_flame_test')
     call initialize_1d_flame_test(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('pressure_wave_test')
     call initialize_pressure_wave_test(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('turbulent_air_test')
     call initialize_turbulent_air_test(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('opthinrad_test')
     call initialize_opthinrad_test(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('compheat_test')
     call initialize_compheat_test(io,yspecies,temp,pressure,u)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)

  case ('lifted')
     call initialize_lifted &
       (io,yspecies,temp,pressure,u, uprofile)
!     call initialize_lifted &
!       (io,yspecies,temp,pressure,u, half_width, decay_factor)
     call set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart) 
  case ('lifted_feed')
     call initialize_lifted_feed &
       (io,yspecies,temp,pressure,u, uprofile)
     call set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)  

  case ('tj')
     call initialize_tj(io, yspecies, temp, pressure, u)   !add the variables you want to collect from the input file.
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)

  case ('bunsen')
     call initialize_bunsen &
       (io,yspecies,temp,pressure,u, half_width, decay_factor)
     call set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)  
  case ('bunsen_feed')
     call initialize_bunsen_feed &
       (io,yspecies,temp,pressure,u, half_width, decay_factor)
     call set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)  
  case ('strat')
     call initialize_strat &
       (io,yspecies,temp,pressure,u, half_width, decay_factor)
     call set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)  
  case ('strat_feed')
     call initialize_strat_feed &
       (io,yspecies,temp,pressure,u, half_width, decay_factor)
     call set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)  
  case ('tracer_test')
     !call initialize_tracer_test &
     !  (io,yspecies,temp,pressure,u, half_width, decay_factor)
     !call set_variables(io)
     !call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  case ('burgers')
     call initialize_burgers(io,yspecies,temp,pressure,u)
     call set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
     call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
  

  case default
     if(myid==0) then
        write(io,*) 'improper initialization of primative variables'
        write(io,*) 'a initialization routine does not exist for run_title: ',  &
             trim(run_title)
     endif
     call terminate_run(io,0)  !must be called by all processors
  end select
  
  pout_inputfile = pout
!----------------------------------------------------------------------------------------
! Restart code from prevous data files.
! Restarting will overwrite any initialization done in the initialization routines above!
!----------------------------------------------------------------------------------------

  if( (i_restart==1) .and. (run_mode=='solve') )  then

    if(myid == 0) write(io,*) 'restarting from saved files...'

!   read restart files
    call read_savefile(io)

    if(pout.ne.pout_inputfile) then
      write(io,*)'WARNING: pout in the restart file does not match pout in the initialization'
      pout=pout_inputfile
    endif

!   set initial timestep for controller to a conservative fraction of
!   previous timestep
#ifndef BUILD_LIBS3D
    if(cont_switch==1) tstep_init=(tstep*time_ref)/3.0  !factor of 3.0 is a conservative measure
#endif
!   set time_restart (seconds)

    time_restart=time*time_ref
    if(myid == 0) write(io,'(a16,1pe9.3,a6)') ' restart time = ',time_restart,' (sec)'

#ifndef BUILD_LIBS3D
!   set save time (bug fix on restarts by evatt)
    if(time==0.0) then 
      time_save=time_save_inc
      tstep_init=max(tstep_min,tstep_init)
      tstep=max(tstep_min,tstep_init)
    end if
#else
    tstep=1.0D-9/time_ref !this should give a safe value for post processing, but take care. 
#endif

#ifndef BUILD_LIBS3D
    if(cont_switch==0)then
     if(myid.eq.0)write(io,*)'WARNING from init_field: We are setting tstep equal to tstep_min' 
     if(myid.eq.0)write(io,*)'for the restart, the time step will not change during the run.'
     tstep=tstep_min/time_ref
    endif
#endif


  endif
!----------------------------------------------------------------------------------------
! write header

  call MPI_Barrier(gcomm, ierr)

  if(myid.eq.0) then
    write(io,*) 'continuing with initialization...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! set derived variables from initialized primative variables

  call set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
!----------------------------------------------------------------------------------------
! store hard boundary conditions (volum must be set)

!  call store_hard_bc(io,q(1,1,1,1,1),temp,pressure,volum,i_restart)
!----------------------------------------------------------------------------------------
! initialize the temporal feed, note this also sets i_frozenfeed
  if(  (trim(run_title) .eq. 'lifted'.and.i_turbulence.eq.1)  &
   .or.(trim(run_title) .eq. 'bunsen'.and.i_turbulence.eq.1)  &
   .or.(trim(run_title) .eq. 'strat' .and.i_turbulence.eq.1)) then
      call init_temporalfeed(io)
  if(myid.eq.0)write(io,*)'after temporal init i_frozenfeed=',i_frozenfeed,'i_temporalfeed=',i_temporalfeed
  endif
!----------------------------------------------------------------------------------------
! sync processors

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! initialize turbulence and/or vortex pairs
  if(i_restart==0 .and. i_turbulence .eq. 1) then
     select case (trim(run_title))

     case ('hcci')
     call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
     case ('bomb')
     call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
     case ('ignition_test')
     call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
     case ('1d_flame_test')
     call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
     case ('pressure_wave_test')
     call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
     case ('turbulent_air_test')
     call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
     case ('opthinrad_test')
     call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
     case ('compheat_test')
     call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)


     case ('lifted_feed')
       call setup_turbulence(io,uprime,xmin,xmax, &
        -0.5*ny_g*(xmax-xmin)/real(nx_g),0.5*ny_g*(xmax-xmin)/real(nx_g),zmin,zmax,x,re) ! this assumes grid is uniform in x direction
!            -ny_g*15e-6/2./l_ref,ny_g*15e-6/2/l_ref,zmin,zmax,x,re)
       !    Ramanan's filter for u-prime, lifted flame case
       do j = 1, ny
         uprime(:,j,:,:) = uprime(:,j,:,:) * uprofile(j)
       
!         s = decay_factor*half_width
!         sigma = 2.5*s
!         uprime(:,j,:,:) = uprime(:,j,:,:)* 0.5 *&
!           (       tanh((y(j)-0.5*(ymax+ymin) + half_width )/sigma)   &
!                 - tanh((y(j)-0.5*(ymax+ymin) - half_width )/sigma) )
       end do
       call init2_turb(io, q, uprime)
       call write_frozenfeed_data(io, uprime)
!       call print_xzavgs(io)
     case ('lifted')
       if(i_frozenfeed.eq.2)then
       call get_turbulence_from_feeddata(io, uprime)
       call init2_turb(io, q, uprime)
       endif

     case ('bunsen_feed')
       !CAREFUL HERE.....
       call setup_turbulence(io,uprime,xmin,xmax,-0.5*ny_g*(xmax-xmin)/real(nx_g),0.5*ny_g*(xmax-xmin)/real(nx_g),zmin,zmax,x,re) ! this assumes grid is uniform in x direction
!       call setup_turbulence(io,uprime,xmin,xmax,-4.0e-3/l_ref,4.0e-3/l_ref,zmin,zmax,x,re) ! case A
!       call setup_turbulence(io,uprime,xmin,xmax,-6.0e-3/l_ref,6.0e-3/l_ref,zmin,zmax,x,re) ! case C
       !    Ramanan's filter for u-prime, bunsen flame case
       s = decay_factor*half_width
       do j = 1, ny
         uprime(:,j,:,:) = uprime(:,j,:,:)* 0.5 *&
            (       tanh((y(j)-0.5*(ymax+ymin) + half_width - 3.0*s)/s)   &
                  - tanh((y(j)-0.5*(ymax+ymin) - half_width + 3.0*s)/s) )
       end do
       call init2_turb(io, q, uprime)
       call write_frozenfeed_data(io, uprime)
!       call print_xzavgs(io)
     case ('bunsen')
       if(i_frozenfeed.eq.2)then
       call get_turbulence_from_feeddata(io, uprime)
       call init2_turb(io, q, uprime)
       endif
       
     case ('strat_feed')
       !CAREFUL HERE.....
       call setup_turbulence(io,uprime,xmin,xmax,-4.0e-3/l_ref,4.0e-3/l_ref,zmin,zmax,x,re)
       !call setup_turbulence(io,uprime,xmin,xmax,-6.0e-3/l_ref,6.0e-3/l_ref,zmin,zmax,x,re)
       !    Ramanan's filter for u-prime, bunsen flame case
       s = decay_factor*half_width
       do j = 1, ny
         uprime(:,j,:,:) = uprime(:,j,:,:)* 0.5 *&
            (       tanh((y(j)-0.5*(ymax+ymin) + half_width - 3.0*s)/s)   &
                  - tanh((y(j)-0.5*(ymax+ymin) - half_width + 3.0*s)/s) )
       end do
       call init2_turb(io, q, uprime)
       call write_frozenfeed_data(io, uprime)
!       call print_xzavgs(io)
     case ('strat')
       if(i_frozenfeed.eq.2)then
       call get_turbulence_from_feeddata(io, uprime)
       call init2_turb(io, q, uprime)
       endif
       

     case ('tj')   
! the tj case previously had the turublence damping hard coded 
! in turublence_m, moved here by esr 17sept08.
!esr       call initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)

       call setup_turbulence(io,uprime,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
      fracYr  = 0.096/1.7408     
      ycent1 = (ymax+ymin)/2.0 - fracYr * (ymax-ymin)/2.0   
      ycent2 = (ymax+ymin)/2.0 + fracYr * (ymax-ymin)/2.0  

       sigma  = 0.032*0.01/l_ref                          ! actual width of transitions 
       do j=1, ny
         u_filter = 0.5*(1.0+tanh((y(j)-ycent1)*2.0/sigma)) * &
                    0.5*(1.0+tanh((ycent2-y(j))*2.0/sigma))
         uprime(:,j,:,:) = uprime(:,j,:,:) * u_filter
       enddo

       call init2_turb(io, q, uprime)

     end select
  elseif(i_restart==0 .and. i_turbulence .eq. 2) then  ! initialize vortex pairs
        call initialize_vortex( q, u, io )
    
  elseif(i_turbulence==1)then   
    call get_turbulence_params(re,io,1)
  endif

  if(i_frozenfeed.ne.0) then
  if(  (trim(run_title) .eq. 'lifted'.and.i_turbulence.eq.1)  &
   .or.(trim(run_title) .eq. 'bunsen'.and.i_turbulence.eq.1)  &
   .or.(trim(run_title) .eq. 'strat' .and.i_turbulence.eq.1)) then
     call init_frozenfeed_read 
  endif
  endif


  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! update primative variables to be absolutely consistent with derived variables

  call get_mass_frac(q(1,1,1,1,1),volum,yspecies)
  call get_velocity_vec(u,q(1,1,1,1,1),volum)
  call calc_temp(temp,q(:,:,:,5,1)*volum, u, yspecies )   !set T, Cp_mix
  call calc_gamma( gamma, cpmix, mixMW )                  !set gamma
  call calc_press( pressure, q(:,:,:,4,1), temp, mixMW )  !set pressure
!------------------------------------------------------------------------------
! calculate maximum speed of sound for given initial conditions

  a_l=maxval(sqrt(gamma*pressure*volum))*a_ref
  call MPI_Allreduce(a_l,a_max,1,MPI_REAL8,MPI_MAX,gcomm,ierr)

  a_l=minval(sqrt(gamma*pressure*volum))*a_ref
  call MPI_Allreduce(a_l,a_min,1,MPI_REAL8,MPI_MIN,gcomm,ierr)

  if(myid.eq.0) then
    write(io,100) 'maximum speed of sound = ',a_max, ' m/s'
    write(io,100) 'mimimum speed of sound = ',a_min, ' m/s'
    write(io,*)
  endif

! set longest domain distance

  Lxyz(:)=0.0
  if(vary_in_x==1) Lxyz(1)=(xmax-xmin)*l_ref
  if(vary_in_y==1) Lxyz(2)=(ymax-ymin)*l_ref
  if(vary_in_z==1) Lxyz(3)=(zmax-zmin)*l_ref

  L_max=sqrt(Lxyz(1)**2 + Lxyz(2)**2 + Lxyz(3)**2)

! calculate time for min and max acoustic waves to traverse domain

  a_time_max=L_max/a_max
  a_time_min=L_max/a_min

  if((nx/=1).and.(ny/=1).and.(nz/=1)) then
    if(myid.eq.0) then
      write(io,101) 'maximum acoustic time =',a_time_max, ' sec'
      write(io,101) 'minimum acoustic time =',a_time_min, ' sec'
      write(io,*)
    endif
  endif
!----------------------------------------------------------------------------------------
! write closing header

  if(myid.eq.0) then
    write(io,102) 'initialization completed for time = ', time*time_ref
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------------------------
! format statements

  100 format(1x,a,f7.2,a)
  101 format(1x,a,1pe9.2,a)
  102 format(1x,a,1pe9.2)
!----------------------------------------------------------------------------------------
  return
end subroutine initialize_field

#include "globalDefines.h"
!========================================================================================
subroutine integrate(io, q, qnx, qny, qnz, qnvar_tot, qn_reg, &
                     yspc, yspcmfab, &
                     temp, tempmfab, &
                     uvel, uvelmfab, &
                     op_work, op_workmfab, &
                     insitu_vars_ghost, insumfab_ghost,  &
                     insitu_vars_noghost, do_insitu, insitu_ghost_size ) !HK JB
!========================================================================================
! integrates solution vector one time step using ERK
!
! register notes:
! q(:,:,:,:,1) - contains the current solution vector (always)
! q(:,:,:,:,2) - contains the current rhs evaluation
! q(:,:,:,:,3) - contains the current carry-over vector
!
! BUG FIX 16-AUG-04 Evatt Hawkes
! - avmolwt was not updated after the sub-stage, leading to first order errors in 
!   the step T and pressure
!----------------------------------------------------------------------------------------
  use rk_m, only : cfl_no
  use rk_m, only : rk_p, nstage, rk_time, rk_err, rk_alpha, rk_beta
  use rk_m, only : q_err
  use rk_m, only : controller  !routine reference

  use param_m, only : nvar_tot, iforder, nx, ny, nz, n_reg, iorder, n_spec
  
  use runtime_m, only : i_time, i_restart
  use runtime_m, only : tstep, time, time_restart, time_accum
  use runtime_m, only : run_title

  use variables_m, only : volum, pressure
  use variables_m, only : get_mass_frac  !routine reference

  use thermchem_m, only : gamma, avmolwt, cpmix, mixMW
  use thermchem_m, only : calc_temp, calc_gamma, calc_press  !routine references

  use filter_m, only : i_time_fil
  use filter_m, only : filter  !routine reference

  use grid_m, only : delx, dely, delz

  use thermchem_m, only : calc_inv_avg_mol_wt


  use topology_m, only : myid    !remove this after debugging


! only have this in here for debugging pressure slice extraction
  use topology_m   
  use param_m, only : nx_g
  use reference_m, only : p_ref, pres_atm, t_ref !HK
#ifdef F03INTERLANG
  use iso_c_binding
#endif



  implicit none
!----------------------------------------------------------------------------------------
! declarations passed

  integer io

  integer, intent(in) :: qnx, qny, qnz, qnvar_tot, qn_reg, do_insitu, insitu_ghost_size !HK
  real, dimension(qnx,qny,qnz,qnvar_tot,qn_reg) :: q !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)
  ! Pointer to multifab object - need this to trigger boxlib fill of halo
#ifdef F03INTERLANG
  type(c_ptr), intent(in) :: yspcmfab, tempmfab, uvelmfab, op_workmfab, insumfab_ghost
#else
  integer*8, intent(in) :: yspcmfab, tempmfab, uvelmfab, op_workmfab, insumfab_ghost
#endif

  real, intent(inout) :: yspc(1-iorder/2:qnx+iorder/2, &
                              1-iorder/2:qny+iorder/2, &
                              1-iorder/2:qnz+iorder/2, &
                              1:n_spec)
  real, intent(inout) :: temp(1-iorder/2:nx+iorder/2, &
                                 1-iorder/2:ny+iorder/2, &
                                 1-iorder/2:nz+iorder/2)
  real, intent(inout) :: uvel(1-iorder/2:nx+iorder/2, &
                                 1-iorder/2:ny+iorder/2, &
                                 1-iorder/2:nz+iorder/2, &
                                 3)
  real, intent(inout) :: op_work(1-iorder/2:nx+iorder/2, &
                                 1-iorder/2:ny+iorder/2, &
                                 1-iorder/2:nz+iorder/2, &
                                 *)
!HK
  real, intent(inout) :: insitu_vars_ghost(1-insitu_ghost_size:nx+insitu_ghost_size, &
                                     1-insitu_ghost_size:ny+insitu_ghost_size, &
                                     1-insitu_ghost_size:nz+insitu_ghost_size, &
                                     2)
  real, intent(inout) :: insitu_vars_noghost(1:nx, &
                                     1:ny, &
                                     1:nz, &
                                     9)

! local declarations

  integer jstage
  integer i, j, k, L
  real small
  logical update_insitu !HK

! only have this in here for debugging pressure slice extraction
  real :: p_line(nx_g), p_min_l, p_max_l, p_min,p_max
  character :: itime_char*5, fname*100
  real :: p_conv_atm

  p_conv_atm = p_ref/pres_atm
!----------------------------------------------------------------------------------------
! set small number

  small=-10.0**(-precision(small))
!----------------------------------------------------------------------------------------

#if USE_ERK_CONTROLLER
! set timestep
  call set_timestep(io, q, uvel, qnx, qny, qnz, qnvar_tot, qn_reg)

#endif
!----------------------------------------------------------------------------------------
! zero some arrays

  q(:,:,:,:,2)=0.0              !zero rhs register
  q(:,:,:,:,3)=q(:,:,:,:,1)     !set initial carry-over vector equal to solution vector
  q_err(:,:,:,:,1)=0.0          !zero first register of q_err vector

!----------------------------------------------------------------------------------------
! set initial time_accum

  time_accum=time
!----------------------------------------------------------------------------------------
! HK: 03 March 2012
! We want copies of some primitive and derived vars for in_situ analyses
! without additional pain of extra computation or communication.
! We pass the insitu_vars arrays and insumfab ptr to rhsf only during the  
! first RK substage during which we make the necessary copies. This is done coz
! The primitive vars get updated in subsequent rk substeps because q(:,:,:,:,1) gets updated. 
!
! start jstage loop
!----------------------------------------------------------------------------------------
  update_insitu = .false.

  STAGE: do jstage = 1, nstage, 1

   !-- calculate rhs for all equations

!HK     call rhsf( q(:,:,:,:,1),q(:,:,:,:,2), yspc, yspcmfab,  &
!HK                temp, tempmfab, uvel, uvelmfab, op_work, op_workmfab)
     if( (jstage.eq.1) .and. (do_insitu.eq.1) ) then 
      update_insitu = .true.
     else
      update_insitu = .false.
     endif

      call rhsf( q(:,:,:,:,1),q(:,:,:,:,2), yspc, yspcmfab,  &
                 temp, tempmfab, uvel, uvelmfab, op_work, op_workmfab, &
                 insitu_vars_ghost, insumfab_ghost, insitu_vars_noghost, update_insitu, insitu_ghost_size)

   !-- update time_accum
     time_accum=time+rk_time(jstage)*tstep

   !-- update error
     q_err(:,:,:,:,1) = q_err(:,:,:,:,1) + rk_err(jstage) * tstep * q(:,:,:,:,2)

   !-- update solution vector
     q(:,:,:,:,1) = q(:,:,:,:,3) + rk_alpha(jstage) * tstep * q(:,:,:,:,2)

   !-- update carry-over vector
     q(:,:,:,:,3) = q(:,:,:,:,1) + rk_beta(jstage) * tstep * q(:,:,:,:,2)

   !-- impose hard boundary conditions
     call impose_hard_bc(q,yspc(1:nx,1:ny,1:nz,:),avmolwt)

  enddo STAGE


!----------------------------------------------------------------------------------------
! filter out high-wavenumber waves

  if((iforder.gt.1).and.(i_time_fil.gt.0)) then
     if(mod(i_time,i_time_fil).eq.0) then
        do L = 1, nvar_tot
           call filter(q(1,1,1,L,1),io)
        enddo
     endif
  endif
!----------------------------------------------------------------------------------------
! ensure that the species mass fractions stay positive

  do L = 6, nvar_tot, 1
   q(:,:,:,L,1)=max(q(:,:,:,L,1),small)
  enddo
!----------------------------------------------------------------------------------------
! extract primative variables and enforce hard boundary conditions
! for last stage (after filter and species trim)

! impose hard boundary conditions
  call impose_hard_bc(q,yspc(1:nx,1:ny,1:nz,:),avmolwt)

  call get_mass_frac(q(1,1,1,1,1),volum,yspc(1:nx,1:ny,1:nz,:))
  call get_velocity_vec(uvel(1:nx,1:ny,1:nz,:),q(1,1,1,1,1),volum)

! BUG FIX 16-AUG-04 - avmolwt ws not updated after the sub-stage,
! leading to first order errors in the step T and pressure
  call calc_inv_avg_mol_wt( yspc(1:nx,1:ny,1:nz,:), avmolwt )         ! set inverse of mixture MW

  call calc_temp(temp(1:nx,1:ny,1:nz),q(:,:,:,5,1)*volum, uvel(1:nx,1:ny,1:nz,:), yspc(1:nx,1:ny,1:nz,:) ) ! set T, Cp_mix
  call calc_gamma( gamma, cpmix, mixMW )                ! set gamma
  call calc_press( pressure, q(:,:,:,4,1), temp(1:nx,1:ny,1:nz), mixMW )  ! set pressure

  ! Diagnostics - pull out pressure along a slice in the x-direction

  p_min_l=minval(pressure(:,:,:))
  p_max_l=maxval(pressure(:,:,:))

  call MPI_Allreduce(p_min_l,p_min,1,MPI_REAL8,MPI_MIN,gcomm,ierr)
  call MPI_Allreduce(p_max_l,p_max,1,MPI_REAL8,MPI_MAX,gcomm,ierr)

  p_min=p_min*p_conv_atm   !atm
  p_max=p_max*p_conv_atm   !atm
! !HK  if(yid==0 .and. zid==0 ) then
! !HK      call MPI_Gather( pressure(:,ny/2,nz/2), nx, MPI_REAL8, p_line, nx, MPI_REAL8, 0, xcomm, ierr)
!   if(yid==ypes/2 .and. zid==zpes/2 ) then
!       call MPI_Gather( temp(1:nx,ny/2,nz/2), nx, MPI_REAL8, p_line, nx, MPI_REAL8, 0, xcomm, ierr)

!       if( xid==0 ) then
!           write(itime_char, '(I5.5)') i_time
! !HK          fname = 'pressure.'//trim(itime_char)
!           fname = 'temperature.'//trim(itime_char)
!           open(unit=10, file=trim(fname), status='unknown', form='formatted')
!           do i=1,nx_g
!               !write(10, '(3D25.10)') p_min, p_max, p_line(i)*p_conv_atm
!               write(10, '(1PE10.4)') p_line(i)*t_ref !HK p_conv_atm
!           enddo
!           close(10)
!       endif
!   endif
!----------------------------------------------------------------------------------------
  return
end subroutine integrate

!----------------------------------------------------------------------
! Routine by Ramanan
! A wrapper to the 'controller'
subroutine set_timestep(io, q, uvel, qnx, qny, qnz, qnvar_tot, qn_reg)

use rk_m, only : cfl_no, rk_p, controller
use runtime_m, only : i_time, i_restart, tstep, time, time_restart, run_title
use variables_m, only : volum, pressure
use variables_m, only : get_mass_frac  !routine reference
use thermchem_m, only : gamma
use grid_m, only : delx, dely, delz
use param_m, only : nx,ny,nz,iorder

implicit none
integer, intent(in) :: io

integer, intent(in) :: qnx, qny, qnz, qnvar_tot, qn_reg
  real, intent(inout) :: uvel(1-iorder/2:nx+iorder/2, &
                                 1-iorder/2:ny+iorder/2, &
                                 1-iorder/2:nz+iorder/2, &
                                 3)
real, dimension(qnx,qny,qnz,qnvar_tot,qn_reg) :: q !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)

call controller(tstep,time,i_time,i_restart,time_restart, &
              run_title,rk_p,q,uvel(1:nx,1:ny,1:nz,:),volum,pressure,gamma,delx,dely,delz,&
              cfl_no,io)

return
end subroutine set_timestep

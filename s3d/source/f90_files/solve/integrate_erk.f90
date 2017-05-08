#include "globalDefines.h"
!========================================================================================
subroutine integrate(io, q, qnx, qny, qnz, qnvar_tot, qn_reg, &
                     yspc, yspcmfab, op_work, op_workmfab)
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
  use rk_m, only : integrate_erk_johnmc  ! flag for johmc routine

  use param_m, only : nvar_tot, iforder, nx, ny, nz, n_reg, iorder, n_spec
  
  use runtime_m, only : i_time, i_restart
  use runtime_m, only : tstep, time, time_restart, time_accum
  use runtime_m, only : run_title

  use variables_m, only : u, volum, temp, pressure, yspecies
  use variables_m, only : get_mass_frac  !routine reference

  use thermchem_m, only : gamma, avmolwt, cpmix, mixMW
  use thermchem_m, only : calc_temp, calc_gamma, calc_press  !routine references

  use filter_m, only : i_time_fil
  use filter_m, only : filter  !routine reference

  use grid_m, only : delx, dely, delz

  use thermchem_m, only : calc_inv_avg_mol_wt


  use topology_m, only : myid    !remove this after debugging



  implicit none
!----------------------------------------------------------------------------------------
! declarations passed

  integer io

  integer, intent(in) :: qnx, qny, qnz, qnvar_tot, qn_reg
  real, dimension(qnx,qny,qnz,qnvar_tot,qn_reg) :: q !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)
  ! Pointer to multifab object - need this to trigger boxlib fill of halo
  integer*8, intent(in) :: yspcmfab, op_workmfab

  real, intent(inout) :: yspc(1-iorder/2:qnx+iorder/2, &
                              1-iorder/2:qny+iorder/2, &
                              1-iorder/2:qnz+iorder/2, &
                              1:n_spec)
  real, intent(inout) :: op_work(1-iorder/2:nx+iorder/2, &
                                 1-iorder/2:ny+iorder/2, &
                                 1-iorder/2:nz+iorder/2, &
                                 *)

! local declarations

  integer jstage
  integer i, j, k, L
  real small
!----------------------------------------------------------------------------------------
! set small number

  small=-10.0**(-precision(small))
!----------------------------------------------------------------------------------------
#if USE_ERK_CONTROLLER
! set timestep
  call set_timestep(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)

#endif
!----------------------------------------------------------------------------------------
! zero some arrays

  q(:,:,:,:,2)=0.0              !zero rhs register
  q(:,:,:,:,3)=q(:,:,:,:,1)     !set initial carry-over vector equal to solution vector
  q_err(:,:,:,:,1)=0.0          !zero first register of q_err vector

  if (integrate_erk_johnmc == 0) then
!----------------------------------------------------------------------------------------
! set initial time_accum

  time_accum=time
!----------------------------------------------------------------------------------------
! start jstage loop
!----------------------------------------------------------------------------------------
  STAGE: do jstage = 1, nstage, 1

   !-- calculate rhs for all equations

     call rhsf(q(:,:,:,:,1),q(:,:,:,:,2), yspc, yspcmfab, op_work, op_workmfab)

   !-- update time_accum
     time_accum=time+rk_time(jstage)*tstep

   !-- update error
     q_err(:,:,:,:,1) = q_err(:,:,:,:,1) + rk_err(jstage) * tstep * q(:,:,:,:,2)

   !-- update solution vector
     q(:,:,:,:,1) = q(:,:,:,:,3) + rk_alpha(jstage) * tstep * q(:,:,:,:,2)

   !-- update carry-over vector
     q(:,:,:,:,3) = q(:,:,:,:,1) + rk_beta(jstage) * tstep * q(:,:,:,:,2)

   !-- impose hard boundary conditions
     call impose_hard_bc(q,yspecies,avmolwt)

  enddo STAGE
  else
      call integrate_erk_jstage_lt(q, q_err, rk_err, rk_alpha, &
      rk_beta, rk_time, yspecies, avmolwt, nx, ny, nz, nvar_tot, &
      n_reg, nstage, size(yspecies,4), tstep, time, time_accum)
  endif

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
  call impose_hard_bc(q,yspecies,avmolwt)

  call get_mass_frac(q(1,1,1,1,1),volum,yspecies)
  call get_velocity_vec(u,q(1,1,1,1,1),volum)

! BUG FIX 16-AUG-04 - avmolwt ws not updated after the sub-stage,
! leading to first order errors in the step T and pressure
  call calc_inv_avg_mol_wt( yspecies, avmolwt )         ! set inverse of mixture MW

  call calc_temp(temp,q(:,:,:,5,1)*volum, u, yspecies ) ! set T, Cp_mix
  call calc_gamma( gamma, cpmix, mixMW )                ! set gamma
  call calc_press( pressure, q(:,:,:,4,1), temp, mixMW )  ! set pressure
!----------------------------------------------------------------------------------------
  return
end subroutine integrate

!----------------------------------------------------------------------
! Routine by Ramanan
! A wrapper to the 'controller'
subroutine set_timestep(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)

use rk_m, only : cfl_no, rk_p, controller
use runtime_m, only : i_time, i_restart, tstep, time, time_restart, run_title
use variables_m, only : u, volum, pressure
use variables_m, only : get_mass_frac  !routine reference
use thermchem_m, only : gamma
use grid_m, only : delx, dely, delz

implicit none
integer, intent(in) :: io

integer, intent(in) :: qnx, qny, qnz, qnvar_tot, qn_reg
real, dimension(qnx,qny,qnz,qnvar_tot,qn_reg) :: q !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)

call controller(tstep,time,i_time,i_restart,time_restart, &
              run_title,rk_p,q,u,volum,pressure,gamma,delx,dely,delz,&
              cfl_no,io)

return
end subroutine set_timestep

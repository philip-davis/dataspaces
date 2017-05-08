#include "globalDefines.h"
subroutine rhsf( q, rhs, yspc, yspcmfab, op_work, op_workmfab )
      ! Pointer to multifab object - need this to trigger boxlib fill of halo
!----------------------------------------------------------------------
! Changes 
! Ramanan Sankaran - 01/04/05
! 1. Diffusive fluxes are computed without having to convert units.
! Ignore older comments about conversion to CGS units.
! This saves a lot of flops.
! 2. Mixavg and Lewis transport modules have been made interchangeable
! by adding dummy arguments in both.
  !-----------------------------------------------------------------------------
  !                     Author:  James Sutherland
  !                     Date:    April, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! This routine calculates the time rate of change for the
  ! momentum, continuity, energy, and species equations.
  !
  ! See each section of the code for more detail on how each equation is computed.
  !
  ! In general, this approach tries to minimize the amount of derivatives taken,
  ! since these are very expensive.  Thus, the convective and diffusive terms are
  ! lumped into a single derivative.  This **could** lead to inaccuracies in an 
  ! equation if one term is small compared to another, yet its gradient is large.
  ! For example, if we consider d(A)/dx + d(B)/dx, we may write this as d(A+B)/dx
  ! which is exact analytically.  However, if A is of order 10^5 and B is of
  ! order 10^-5, then A+B is dominated by A, and the derivative operator may be
  ! insensitive to fluctuations in B.
  !
  ! Having said this, I haven't seen much difference in answers between this
  ! formulation and the "old" formulation, where this approach was not taken.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! recall the ordering of the solution vector:
  !
  !   q(:,:,:,1)  ->  x momentum  (rho*u)
  !   q(:,:,:,2)  ->  y momentum  (rho*v)
  !   q(:,:,:,3)  ->  z momentum  (rho*w)
  !   q(:,:,:,4)  ->  density
  !   q(:,:,:,5)  ->  energy
  !   q(:,:,:,6)  ->  scalar
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! NOTE: It is imperitive that "q" and "rhs" are stored in SEPARATE memory
  !       locations.  Otherwise, overwrites will occur and that is very bad!
  !-----------------------------------------------------------------------------

  use param_m, only : nx, ny, nz, n_spec, nvar_tot, run_mode
  use param_m, only : vary_in_x, vary_in_y, vary_in_z, periodic_x, periodic_y, periodic_z, iorder, nsc
  use grid_m, only : scale_1x, scale_1y, scale_1z
  use topology_m
  use bc_m, only : nrf_x0, nrf_xl
  use runtime_m, only : i_time, time_accum, tstep

  use reference_m
  use transport_m
  use thermchem_m
  use variables_m, only : u, pressure, yspecies, temp, volum, get_mass_frac
  use chemkin_m, only : reaction_rate, reaction_rate_bounds

  use compheat_m        ! compression heating tools
  use opthinrad_m       ! optical thin radiation tools

  use work_m, only : tmp  => work1_1    ! used to pass arguments to derivative routine
  use work_m, only : tmp2 => work1_2    ! used as d(tmp)/dx

  use rk_m, only : rk_type  !trigger to add reaction rate to rhs for erk

  use derivative_m, only : print_deriv_list

  implicit none

!-------------------------- Passed Variables -------------------------

  real, dimension(nx,ny,nz,nvar_tot), intent(in)  :: q
  real, target, dimension(nx,ny,nz,nvar_tot), intent(out) :: rhs

  ! mfab pointers
  integer*8, intent(in) :: yspcmfab, op_workmfab
  real, intent(inout) :: yspc(1-iorder/2:nx+iorder/2, &
                              1-iorder/2:ny+iorder/2, &
                              1-iorder/2:nz+iorder/2, &
                              1:n_spec)
  real, intent(inout) :: op_work(1-iorder/2:nx+iorder/2, &
                                 1-iorder/2:ny+iorder/2, &
                                 1-iorder/2:nz+iorder/2, &
                                 *)

!-------------------------- Local Variables --------------------------

  !notes by ramanan - 01/05/05
  !The array dimensioning can be misleading
  !For grad_u, 4th dimension is the direction and 5th dimension is the velocity component
  !For grad_Ys, 4th dimension is the species and 5th dimension is the direction

  real, target, dimension(nx,ny,nz,3,3)      :: mom_store
  ! Added by Ramanan - tau does not need a separate array
  ! It can be stored in the same place as grad_u
  real, pointer, dimension(:,:,:,:,:) :: tau, grad_u
  !Added by Ramanan - to remember grad's for the boundary condition
  real, dimension(ny, nz, 3) :: grad_u_x0, grad_u_xL
  real, dimension(nx, nz, 3) :: grad_u_y0, grad_u_yL
  real, dimension(nx, ny, 3) :: grad_u_z0, grad_u_zL

  real, target, dimension(nx,ny,nz,3)        :: enrg_store
  ! Added by Ramanan - heatFlux does not need a separate array
  ! It can be stored in the same place as grad_T
  real, pointer, dimension(:,:,:,:)        :: heatFlux, grad_T

  real, target, dimension(nx,ny,nz,n_spec,3) :: spcs_store
  ! Added by Ramanan - diffFlux does not need a separate array
  ! It can be stored in the same place as grad_Ys
  real, pointer, dimension(:,:,:,:,:) :: diffFlux, grad_Ys
  !Added by Ramanan - to remember grad's for the boundary condition
  real, dimension(ny, nz, n_spec) :: grad_Ys_x0, grad_Ys_xL
  real, dimension(nx, nz, n_spec) :: grad_Ys_y0, grad_Ys_yL
  real, dimension(nx, ny, n_spec) :: grad_Ys_z0, grad_Ys_zL
  !Added by Ramanan - h_spec doesnt need its own space. 
  !I chose to store it in the space of rhs
  real, pointer, dimension(:,:,:,:) :: h_spec
  !Extra storage is needed to remember the h_spec at the boundary planes
  real, dimension(ny, nz, n_spec) :: h_spec_x0, h_spec_xL 
  real, dimension(nx, nz, n_spec) :: h_spec_y0, h_spec_yL 
  real, dimension(nx, ny, n_spec) :: h_spec_z0, h_spec_zL 
  !Added by Ramanan - rr_r doesnt need its own space. 
  !It can be stored in the diffFlux vector.
  real, pointer, dimension(:,:,:,:) :: rr_r
  !Extra storage is needed to remember the rr_r at the boundary planes
  real, dimension(ny, nz, n_spec) :: rr_r_x0, rr_r_xL
  real, dimension(nx, nz, n_spec) :: rr_r_y0, rr_r_yL
  real, dimension(nx, ny, n_spec) :: rr_r_z0, rr_r_zL

  !Storage for open boundary conditions
  real, dimension(ny,nz,3,3) :: grad_u_x0_all, grad_u_xL_all
  real, dimension(ny,nz,3,3) :: tau_x0, tau_xL

  real, dimension(nx,nz,3,3) :: grad_u_y0_all, grad_u_yL_all
  real, dimension(nx,nz,3,3) :: tau_y0, tau_yL

  real, dimension(nx,ny,3,3) :: grad_u_z0_all, grad_u_zL_all
  real, dimension(nx,ny,3,3) :: tau_z0, tau_zL

  real, dimension(ny,nz) :: div_heatFlux_x0, div_tau_x_x0, div_tau_y_x0, div_tau_z_x0
  real, dimension(ny,nz) :: div_heatFlux_xL, div_tau_x_xL, div_tau_y_xL, div_tau_z_xL

  real, dimension(nx,nz) :: div_heatFlux_y0, div_tau_x_y0, div_tau_y_y0, div_tau_z_y0
  real, dimension(nx,nz) :: div_heatFlux_yL, div_tau_x_yL, div_tau_y_yL, div_tau_z_yL

  real, dimension(nx,ny) :: div_heatFlux_z0, div_tau_x_z0, div_tau_y_z0, div_tau_z_z0
  real, dimension(nx,ny) :: div_heatFlux_zL, div_tau_x_zL, div_tau_y_zL, div_tau_z_zL

  real, dimension(ny,nz,n_spec) :: div_diffFlux_x0, div_diffFlux_xL
  real, dimension(nx,nz,n_spec) :: div_diffFlux_y0, div_diffFlux_yL
  real, dimension(nx,ny,n_spec) :: div_diffFlux_z0, div_diffFlux_zL

#ifdef GETRATES_NEEDS_DIFFUSION
  real, dimension(nx,ny,nz,n_spec)    :: diffusion
#endif

  integer :: i,j,k,n,m

  integer :: idxA, idxB, idxC, idxD, idxE
!*******************
!variables to be moved into new reference module...?
! comment by Ramanan Sankaran - 01/03/05
!  - variables unnecessary - fluxes dont need unit conversion anymore
!  real :: tau_ref, diffFlux_ref, heatFlux_ref, grad_u_ref, temp_ref, grad_T_ref, h_ref, grad_Ys_ref
!*******************

!----------------------- Executable Statements -----------------------

! set reference stuff.
! factors of 10^n are to convert from SI to CGS

! tau_ref      = rho_ref * a_ref*a_ref * 10.    ! [g/(cm*s^2)]
! heatFlux_ref = rho_ref * a_ref**3 * 1.0e3     ! [erg/(cm*s^2)] = [g/s^3]
! diffFlux_ref = rho_ref * a_ref * 0.1          ! [g/(cm*s^2)]
! grad_T_ref   = t_ref/l_ref * 0.01             ! [K/cm]
! temp_ref     = t_ref                          ! [K]
! grad_u_ref   = a_ref/l_ref                    ! [1/s]
! h_ref        = cp_ref*t_ref * 1.0e4           ! [erg/g] = [cm^2/s^2]   = a_ref^2 * 1e4
! grad_Ys_ref  = 0.01 / l_ref                   ! [1/cm]


!*******************************************************************************

!Added by Ramanan - 01/05/05
!Fool proofing the sharing of memory space. This way we can prevent mis-over-writes.
!Any untimely calls to these arrays will crash the code.

nullify(tau)
grad_u => mom_store

nullify(diffFlux)
grad_Ys => spcs_store

nullify(heatFlux)
grad_T => enrg_store

nullify (rr_r)

!===============================================================================
! STEP 1:  Impose boundary conditions on primitive & conserved variables as needed
! THIS HAS BEEN MOVED TO TSTEP...
!
!===============================================================================
!!$! Do this for "hard inflow", wall BC's, etc.
!!$
!!$  call impose_hard_bc(q,yspecies,avmolwt)


!===============================================================================
! STEP 2:  Compute quantities that will be needed in several different equations
!===============================================================================
! Compute the following quantities:
!       yspecies - species mass fractions
!       temp     - temperature
!       avmolwt  - inverse of mixture molecular weight  1/MW_mix
!       cp_mix   - mixture heat capacity at constant pressure
!       gamma    -  gamma=Cp/Cv  (ratio of heat capacities)
!       pressure - thermodynamic pressure, calculated from Ideal Gas Law
!       u        - velocity vector
!       h_spec   - species enthalpies
!
  call get_mass_frac( q, volum, yspecies )              ! get Ys from rho*Ys, volum from rho
  yspc(1:nx,1:ny,1:nz,1:n_spec) = yspecies(1:nx, 1:ny, 1:nz, 1:n_spec)

  call get_velocity_vec( u, q, volum )                  ! fill the velocity vector
  call calc_inv_avg_mol_wt( yspecies, avmolwt )         ! set inverse of mixture MW
  call calc_temp(temp, q(:,:,:,5)*volum, u, yspecies )  ! set T, Cp_mix
  call calc_gamma( gamma, cpmix, mixMW )                ! set gamma
  call calc_press( pressure, q(:,:,:,4), temp, mixMW )  ! set pressure

! calculate species enthalpies

  !added by ramanan - 01/06/05
  h_spec => rhs(:,:,:,1:n_spec)

  call calc_specEnth_allpts(temp, h_spec)

! grad_u - Velocity gradient tensor.  This is used to construct the
!          stress tensor.  It is also needed for the boundary conditions.
! grad_T - Temperature gradient required in the evaluation of
!          several transport terms
! grad_Y - Species mass fraction gradients may be required in transport
!          evaluation as well as for boundary conditions.
!
  !notes by ramanan - 01/05/05
  !The array dimensioning can be misleading
  !For grad_u, 4th dimension is the direction and 5th dimension is the velocity component
  !For grad_Ys, 4th dimension is the species and 5th dimension is the direction

  call computeVectorGradient( u, grad_u )
  call computeScalarGradient( temp, grad_T )
  do n=1,n_spec
      !call computeScalarGradient( yspecies(:,:,:,n), grad_Ys(:,:,:,n,:) )
      call computeScalarGradient5d_boxlib( yspc, grad_Ys(:,:,:,:,:), n_spec, yspcmfab )  
  enddo

!Added by Ramanan - 01/05/05
!Store the boundary grad values
  if(vary_in_x==1)then
    if (xid==0) then
      grad_u_x0 = grad_u(1,:,:,1,:)
      grad_u_x0_all = grad_u(1,:,:,:,:)
      grad_Ys_x0 = grad_Ys(1,:,:,:,1)
      h_spec_x0 = h_spec(1,:,:,:)
    end if
  
    if (xid==xpes-1) then
      grad_u_xL = grad_u(nx,:,:,1,:)
      grad_u_xL_all = grad_u(nx,:,:,:,:)
      grad_Ys_xL = grad_Ys(nx,:,:,:,1)
      h_spec_xL = h_spec(nx,:,:,:)
    end if
  endif

  if(vary_in_y==1)then
    if (yid==0) then
      grad_u_y0 = grad_u(:,1,:,2,:)
      grad_u_y0_all = grad_u(:,1,:,:,:)
      grad_Ys_y0 = grad_Ys(:,1,:,:,2)
      h_spec_y0 = h_spec(:,1,:,:)
    end if
  
    if (yid==ypes-1) then
      grad_u_yL = grad_u(:,ny,:,2,:)
      grad_u_yL_all = grad_u(:,ny,:,:,:)
      grad_Ys_yL = grad_Ys(:,ny,:,:,2)
      h_spec_yL = h_spec(:,ny,:,:)
    end if
  endif

  if(vary_in_z==1)then
    if (zid==0) then
      grad_u_z0 = grad_u(:,:,1,3,:)
      grad_u_z0_all = grad_u(:,:,1,:,:)
      grad_Ys_z0 = grad_Ys(:,:,1,:,3)
      h_spec_z0 = h_spec(:,:,1,:)
    end if
  
    if (zid==zpes-1) then
      grad_u_zL = grad_u(:,:,nz,3,:)
      grad_u_zL_all = grad_u(:,:,nz,:,:)
      grad_Ys_zL = grad_Ys(:,:,nz,:,3)
      h_spec_zL = h_spec(:,:,nz,:)
    end if
  endif


!*******************************************************************************



!===============================================================================
! STEP 3: Compute flux terms for equations as required.  Then impose boundary 
!         conditions on the flux terms.
!===============================================================================
! Calculation of:
!       diffusion velocities (species equation, energy equation)
!       heat flux vector (energy equation)
!       stress tensor (momentum and energy equations)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NOTE:
!   By convention, transport packages will be done in dimensional form.  Thus,
!   we must dimensionalize the input and then non-dimensionalize the output.
!   All units are expected in CGS form...

! factors of 10^n are for units conversion (SI->CGS)
!!$  ! For constant Lewis number transport:
!----------------------------------------------------------------------
! commented out by Ramanan Sankaran - 01/03/05
! do not need to switch between units any more
! saves cpu time
!----------------------------------------------------------------------
!  call getDiffusiveFluxTerms( grad_u*grad_u_ref,           temp*temp_ref,       &
!                              grad_T*grad_T_ref,           grad_Ys*grad_Ys_ref, &
!                              q(:,:,:,4)*rho_ref*1.0e-3,   cpmix*cp_ref*1.0e4,  &
!                              h_spec*h_ref,                tau,                 &
!                              diffFlux,                    heatFlux              )
!
!----------------------------------------------------------------------

  call getDiffusiveFluxTerms &
              ( grad_u ,temp, grad_T ,yspecies, grad_Ys, pressure , &
                q(:,:,:,4), cpmix, h_spec)

!Added by Ramanan - 01/05/05
!tau etc. used to be a separate array on their own
!Now they are being stored in same place as the grad's
!For consistency with old code, let the variable tau point to the right location
!Fool proofing the sharing of memory space. This way we can prevent mis-over-writes.
!Anymore calls to grad_u are wrong and will crash the code. 

nullify(grad_u)
tau => mom_store

nullify(grad_Ys)
diffFlux => spcs_store

nullify(grad_T)
heatFlux => enrg_store

nullify(h_spec)

!----------------------------------------------------------------------
! Commented out by Ramanan Sankaran - 01/03/05
! No need to switch between units
!----------------------------------------------------------------------
!  tau      = tau      / tau_ref         ! stress tensor
!  diffFlux = diffFlux / diffFlux_ref    ! species diffusive flux vector
!  heatFlux = heatFlux / heatFlux_ref    ! heat flux vector

! modify fluxes at boundaries as required.  This should only be done at OPEN boundaries.

  call bc_flux( u, tau, heatFlux, diffFlux )
  call wallBCFlux( heatFlux, diffFlux, tau, pressure, temp, q(:,:,:,4), mixMW )

!*******************************************************************************
! Change by Chun Sang Yoo (03/15/06)
! Calculate boundary values


!  call computeDivergence(heatFlux(:,:,:,:), tmp2)
 
  if(vary_in_x==1)then
    if (xid==0) then
      call point_der1_x(heatFlux(:,:,:,1),tmp(1,:,:),1,scale_1x)
      call deriv_inplane_1(heatFlux(1,:,:,2),tmp2(1,:,:),1,ny,nz,1)
      tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
      call deriv_inplane_2(heatFlux(1,:,:,3),tmp2(1,:,:),1,ny,nz,1)
      tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
      tau_x0(:,:,:,:) = tau(1,:,:,:,:)
      div_heatFlux_x0(:,:) = tmp(1,:,:)
    endif

    if (xid==xpes-1) then
      call point_der1_x(heatFlux(:,:,:,1),tmp(nx,:,:),nx,scale_1x)
      call deriv_inplane_1(heatFlux(nx,:,:,2),tmp2(nx,:,:),1,ny,nz,1)
      tmp(nx,:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
      call deriv_inplane_2(heatFlux(nx,:,:,3),tmp2(nx,:,:),1,ny,nz,1)
      tmp(nx,:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
      tau_xL(:,:,:,:) = tau(nx,:,:,:,:)
      div_heatFlux_xL(:,:) = tmp(nx,:,:)
    endif
  endif
  
#ifndef USE_HISTORIC_BC
  if(vary_in_y==1)then
    if (yid==0) then
      call point_der1_y(heatFlux(:,:,:,2),tmp(:,1,:),1,scale_1y)
      call deriv_inplane_1(heatFlux(:,1,:,1),tmp2(:,1,:),2,nx,nz,1)
      tmp(:,1,:) = tmp(:,1,:) + tmp2(:,1,:)
      call deriv_inplane_2(heatFlux(:,1,:,3),tmp2(:,1,:),2,nx,nz,1)
      tau_y0(:,:,:,:) = tau(:,1,:,:,:)
      div_heatFlux_y0(:,:) = tmp(:,1,:) + tmp2(:,1,:)
    endif

    if (yid==ypes-1) then
      call point_der1_y(heatFlux(:,:,:,2),tmp(:,ny,:),ny,scale_1y)
      call deriv_inplane_1(heatFlux(:,ny,:,1),tmp2(:,ny,:),2,nx,nz,1)
      tmp(:,ny,:) = tmp(:,ny,:) + tmp2(:,ny,:)
      call deriv_inplane_2(heatFlux(:,ny,:,3),tmp2(:,ny,:),2,nx,nz,1)
      tau_yL(:,:,:,:) = tau(:,ny,:,:,:)
      div_heatFlux_yL(:,:) = tmp(:,ny,:) + tmp2(:,ny,:)
    endif
  endif 

  if(vary_in_z==1)then
    if (zid==0) then
      call point_der1_z(heatFlux(:,:,:,3),tmp(:,:,1),1,scale_1z)
      call deriv_inplane_1(heatFlux(:,:,1,1),tmp2(:,:,1),3,nx,ny,1)
      tmp(:,:,1) = tmp(:,:,1) + tmp2(:,:,1)
      call deriv_inplane_2(heatFlux(:,:,1,2),tmp2(:,:,1),3,nx,ny,1)
      tau_z0(:,:,:,:) = tau(:,:,1,:,:)
      div_heatFlux_z0(:,:) = tmp(:,:,1) + tmp2(:,:,1)
    endif

    if (zid==zpes-1) then
      call point_der1_z(heatFlux(:,:,:,3),tmp(:,:,nz),nz,scale_1z)
      call deriv_inplane_1(heatFlux(:,:,nz,1),tmp2(:,:,nz),3,nx,ny,1)
      tmp(:,:,nz) = tmp(:,:,nz) + tmp2(:,:,nz)
      call deriv_inplane_2(heatFlux(:,:,nz,2),tmp2(:,:,nz),3,nx,ny,1)
      tau_zL(:,:,:,:) = tau(:,:,nz,:,:)
      div_heatFlux_zL(:,:) = tmp(:,:,nz) + tmp2(:,:,nz)
    endif
  endif
  
#endif
!  call computeDivergence(tau(:,:,:,1,:), tmp2)
  
  if(vary_in_x==1)then
    if (xid==0) then
      call point_der1_x(tau(:,:,:,1,1),tmp(1,:,:),1,scale_1x)
      call deriv_inplane_1(tau(1,:,:,1,2),tmp2(1,:,:),1,ny,nz,1)
      tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
      call deriv_inplane_2(tau(1,:,:,1,3),tmp2(1,:,:),1,ny,nz,1)
      tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
      div_tau_x_x0(:,:) = tmp(1,:,:)
    endif

    if (xid==xpes-1) then
      call point_der1_x(tau(:,:,:,1,1),tmp(nx,:,:),nx,scale_1x)
      call deriv_inplane_1(tau(nx,:,:,1,2),tmp2(nx,:,:),1,ny,nz,1)
      tmp(nx,:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
      call deriv_inplane_2(tau(nx,:,:,1,3),tmp2(nx,:,:),1,ny,nz,1)
      div_tau_x_xL(:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
    endif
  endif
#ifndef USE_HISTORIC_BC
  if(vary_in_y==1)then
    if (yid==0) then
      call point_der1_y(tau(:,:,:,1,2),tmp(:,1,:),1,scale_1y)
      call deriv_inplane_1(tau(:,1,:,1,1),tmp2(:,1,:),2,nx,nz,1)
      tmp(:,1,:) = tmp(:,1,:) + tmp2(:,1,:)
      call deriv_inplane_2(tau(:,1,:,1,3),tmp2(:,1,:),2,nx,nz,1)
      div_tau_x_y0(:,:) = tmp(:,1,:) + tmp2(:,1,:)
    endif

    if (yid==ypes-1) then
      call point_der1_y(tau(:,:,:,1,2),tmp(:,ny,:),ny,scale_1y)
      call deriv_inplane_1(tau(:,ny,:,1,1),tmp2(:,ny,:),2,nx,nz,1)
      tmp(:,ny,:) = tmp(:,ny,:) + tmp2(:,ny,:)
      call deriv_inplane_2(tau(:,ny,:,1,3),tmp2(:,ny,:),2,nx,nz,1)
      div_tau_x_yL(:,:) = tmp(:,ny,:) + tmp2(:,ny,:)
    endif
  endif

  if(vary_in_z==1)then
    if (zid==0) then
      call point_der1_z(tau(:,:,:,1,3),tmp(:,:,1),1,scale_1z)
      call deriv_inplane_1(tau(:,:,1,1,1),tmp2(:,:,1),3,nx,ny,1)
      tmp(:,:,1) = tmp(:,:,1) + tmp2(:,:,1)
      call deriv_inplane_2(tau(:,:,1,1,2),tmp2(:,:,1),3,nx,ny,1)
      div_tau_x_z0(:,:) = tmp(:,:,1) + tmp2(:,:,1)
    endif

    if (zid==zpes-1) then
      call point_der1_z(tau(:,:,:,1,3),tmp(:,:,nz),nz,scale_1z)
      call deriv_inplane_1(tau(:,:,nz,1,1),tmp2(:,:,nz),3,nx,ny,1)
      tmp(:,:,nz) = tmp(:,:,nz) + tmp2(:,:,nz)
      call deriv_inplane_2(tau(:,:,nz,1,2),tmp2(:,:,nz),3,nx,ny,1)
      div_tau_x_zL(:,:) = tmp(:,:,nz) + tmp2(:,:,nz)
    endif
  endif

#endif
  
!  call computeDivergence(tau(:,:,:,2,:), tmp2)
  
  if(vary_in_x==1)then
    if (xid==0) then
      call point_der1_x(tau(:,:,:,2,1),tmp(1,:,:),1,scale_1x)
      call deriv_inplane_1(tau(1,:,:,2,2),tmp2(1,:,:),1,ny,nz,1)
      tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
      call deriv_inplane_2(tau(1,:,:,2,3),tmp2(1,:,:),1,ny,nz,1)
      div_tau_y_x0(:,:) = tmp(1,:,:) + tmp2(1,:,:)
    endif

    if (xid==xpes-1) then
      call point_der1_x(tau(:,:,:,2,1),tmp(nx,:,:),nx,scale_1x)
      call deriv_inplane_1(tau(nx,:,:,2,2),tmp2(nx,:,:),1,ny,nz,1)
      tmp(nx,:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
      call deriv_inplane_2(tau(nx,:,:,2,3),tmp2(nx,:,:),1,ny,nz,1)
      div_tau_y_xL(:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
    endif
  endif
  
#ifndef USE_HISTORIC_BC
  if(vary_in_y==1)then
    if (yid==0) then
      call point_der1_y(tau(:,:,:,2,2),tmp(:,1,:),1,scale_1y)
      call deriv_inplane_1(tau(:,1,:,2,1),tmp2(:,1,:),2,nx,nz,1)
      tmp(:,1,:) = tmp(:,1,:) + tmp2(:,1,:)
      call deriv_inplane_2(tau(:,1,:,2,3),tmp2(:,1,:),2,nx,nz,1)
      div_tau_y_y0(:,:) = tmp(:,1,:) + tmp2(:,1,:)
    endif

    if (yid==ypes-1) then
      call point_der1_y(tau(:,:,:,2,2),tmp(:,ny,:),ny,scale_1y)
      call deriv_inplane_1(tau(:,ny,:,2,1),tmp2(:,ny,:),2,nx,nz,1)
      tmp(:,ny,:) = tmp(:,ny,:) + tmp2(:,ny,:)
      call deriv_inplane_2(tau(:,ny,:,2,3),tmp2(:,ny,:),2,nx,nz,1)
      div_tau_y_yL(:,:) = tmp(:,ny,:) + tmp2(:,ny,:)
    endif
  endif
  
  if(vary_in_z==1)then
    if (zid==0) then
      call point_der1_z(tau(:,:,:,2,3),tmp(:,:,1),1,scale_1z)
      call deriv_inplane_1(tau(:,:,1,2,1),tmp2(:,:,1),3,nx,ny,1)
      tmp(:,:,1) = tmp(:,:,1) + tmp2(:,:,1)
      call deriv_inplane_2(tau(:,:,1,2,2),tmp2(:,:,1),3,nx,ny,1)
      div_tau_y_z0(:,:) = tmp(:,:,1) + tmp2(:,:,1)
    endif

    if (zid==zpes-1) then
      call point_der1_z(tau(:,:,:,2,3),tmp(:,:,nz),nz,scale_1z)
      call deriv_inplane_1(tau(:,:,nz,2,1),tmp2(:,:,nz),3,nx,ny,1)
      tmp(:,:,nz) = tmp(:,:,nz) + tmp2(:,:,nz)
      call deriv_inplane_2(tau(:,:,nz,2,2),tmp2(:,:,nz),3,nx,ny,1)
      div_tau_y_zL(:,:) = tmp(:,:,nz) + tmp2(:,:,nz)
    endif
  endif
#endif
!  call computeDivergence(tau(:,:,:,3,:), tmp2)

  if(vary_in_x==1)then
    if (xid==0) then
      call point_der1_x(tau(:,:,:,3,1),tmp(1,:,:),1,scale_1x)
      call deriv_inplane_1(tau(1,:,:,3,2),tmp2(1,:,:),1,ny,nz,1)
      tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
      call deriv_inplane_2(tau(1,:,:,3,3),tmp2(1,:,:),1,ny,nz,1)
      tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
      div_tau_z_x0(:,:) = tmp(1,:,:)
    endif

    if (xid==xpes-1) then
      call point_der1_x(tau(:,:,:,3,1),tmp(nx,:,:),nx,scale_1x)
      call deriv_inplane_1(tau(nx,:,:,3,2),tmp2(nx,:,:),1,ny,nz,1)
      tmp(nx,:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
      call deriv_inplane_2(tau(nx,:,:,3,3),tmp2(nx,:,:),1,ny,nz,1)
      div_tau_z_xL(:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
    endif
  endif

#ifndef USE_HISTORIC_BC
  if(vary_in_y==1)then
    if (yid==0) then
      call point_der1_y(tau(:,:,:,3,2),tmp(:,1,:),1,scale_1y)
      call deriv_inplane_1(tau(:,1,:,3,1),tmp2(:,1,:),2,nx,nz,1)
      tmp(:,1,:) = tmp(:,1,:) + tmp2(:,1,:)
      call deriv_inplane_2(tau(:,1,:,3,3),tmp2(:,1,:),2,nx,nz,1)
      div_tau_z_y0(:,:) = tmp(:,1,:) + tmp2(:,1,:)
    endif

    if (yid==ypes-1) then
      call point_der1_y(tau(:,:,:,3,2),tmp(:,ny,:),ny,scale_1y)
      call deriv_inplane_1(tau(:,ny,:,3,1),tmp2(:,ny,:),2,nx,nz,1)
      tmp(:,ny,:) = tmp(:,ny,:) + tmp2(:,ny,:)
      call deriv_inplane_2(tau(:,ny,:,3,3),tmp2(:,ny,:),2,nx,nz,1)
      div_tau_z_yL(:,:) = tmp(:,ny,:) + tmp2(:,ny,:)
    endif
  endif
 
  if(vary_in_z==1)then
    if (zid==0) then
      call point_der1_z(tau(:,:,:,3,3),tmp(:,:,1),1,scale_1z)
      call deriv_inplane_1(tau(:,:,1,3,1),tmp2(:,:,1),3,nx,ny,1)
      tmp(:,:,1) = tmp(:,:,1) + tmp2(:,:,1)
      call deriv_inplane_2(tau(:,:,1,3,2),tmp2(:,:,1),3,nx,ny,1)
      div_tau_z_z0(:,:) = tmp(:,:,1) + tmp2(:,:,1)
    endif

    if (zid==zpes-1) then
      call point_der1_z(tau(:,:,:,3,3),tmp(:,:,nz),nz,scale_1z)
      call deriv_inplane_1(tau(:,:,nz,3,1),tmp2(:,:,nz),3,nx,ny,1)
      tmp(:,:,nz) = tmp(:,:,nz) + tmp2(:,:,nz)
      call deriv_inplane_2(tau(:,:,nz,3,2),tmp2(:,:,nz),3,nx,ny,1)
      div_tau_z_zL(:,:) = tmp(:,:,nz) + tmp2(:,:,nz)
    endif
  endif
#endif
  do n=1,n_spec

!    call computeDivergence(diffFlux(:,:,:,n,:), tmp2)

    if(vary_in_x==1)then
      if (xid==0) then
        call point_der1_x(diffFlux(:,:,:,n,1),tmp(1,:,:),1,scale_1x)
        call deriv_inplane_1(diffFlux(1,:,:,n,2),tmp2(1,:,:),1,ny,nz,1)
        tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
        call deriv_inplane_2(diffFlux(1,:,:,n,3),tmp2(1,:,:),1,ny,nz,1)
        tmp(1,:,:) = tmp(1,:,:) + tmp2(1,:,:)
        div_diffFlux_x0(:,:,n) = tmp(1,:,:)
      endif

      if (xid==xpes-1) then
        call point_der1_x(diffFlux(:,:,:,n,1),tmp(nx,:,:),nx,scale_1x)
        call deriv_inplane_1(diffFlux(nx,:,:,n,2),tmp2(nx,:,:),1,ny,nz,1)
        tmp(nx,:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
        call deriv_inplane_2(diffFlux(nx,:,:,n,3),tmp2(nx,:,:),1,ny,nz,1)
        tmp(nx,:,:) = tmp(nx,:,:) + tmp2(nx,:,:)
        div_diffFlux_xL(:,:,n) = tmp(nx,:,:)
      endif
    endif
#ifndef USE_HISTORIC_BC
    if(vary_in_y==1)then
      if (yid==0) then
        call point_der1_y(diffFlux(:,:,:,n,2),tmp(:,1,:),1,scale_1y)
        call deriv_inplane_1(diffFlux(:,1,:,n,2),tmp2(:,1,:),2,nx,nz,1)
        tmp(:,1,:) = tmp(:,1,:) + tmp2(:,1,:)
        call deriv_inplane_2(diffFlux(:,1,:,n,3),tmp2(:,1,:),2,nx,nz,1)
        div_diffFlux_y0(:,:,n) = tmp(:,1,:) + tmp2(:,1,:)
      endif

      if (yid==ypes-1) then
        call point_der1_y(diffFlux(:,:,:,n,2),tmp(:,ny,:),ny,scale_1y)
        call deriv_inplane_1(diffFlux(:,ny,:,n,2),tmp2(:,ny,:),2,nx,nz,1)
        tmp(:,ny,:) = tmp(:,ny,:) + tmp2(:,ny,:)
        call deriv_inplane_2(diffFlux(:,ny,:,n,3),tmp2(:,ny,:),2,nx,nz,1)
        div_diffFlux_yL(:,:,n) = tmp(:,ny,:) + tmp2(:,ny,:)
      endif
    endif

    if(vary_in_z==1)then
      if (zid==0) then
        call point_der1_z(diffFlux(:,:,:,n,3),tmp(:,:,1),1,scale_1z)
        call deriv_inplane_1(diffFlux(:,:,1,n,1),tmp2(:,:,1),3,nx,ny,1)
        tmp(:,:,1) = tmp(:,:,1) + tmp2(:,:,1)
        call deriv_inplane_2(diffFlux(:,:,1,n,2),tmp2(:,:,1),3,nx,ny,1)
        div_diffFlux_z0(:,:,n) = tmp(:,:,1) + tmp2(:,:,1)
      endif

      if (zid==zpes-1) then
        call point_der1_z(diffFlux(:,:,:,n,3),tmp(:,:,nz),nz,scale_1z)
        call deriv_inplane_1(diffFlux(:,:,nz,n,1),tmp2(:,:,nz),3,nx,ny,1)
        tmp(:,:,nz) = tmp(:,:,nz) + tmp2(:,:,nz)
        call deriv_inplane_2(diffFlux(:,:,nz,n,2),tmp2(:,:,nz),3,nx,ny,1)
        div_diffFlux_zL(:,:,n) = tmp(:,:,nz) + tmp2(:,:,nz)
      endif
    endif
#endif

  enddo

!===============================================================================
! STEP 4:  Assemble the RHS for each governing equation
!===============================================================================
#define MOMENX 1 
#define MOMENY 2 
#define MOMENZ 3 
#define CONTIN 4 
#define ENERGY 5 


! Pre-post recvs for momentum, continuity, energy
  if (vary_in_x==1) then
      !call derivative_x_post( nx,ny,nz, tmp, MOMENX, 'momx-dx' )
      if (vary_in_y==1) then
          !call derivative_y_post( nx,ny,nz, tmp, MOMENX, 'momx-dy' )
      endif
      if( vary_in_z==1) then
          !call derivative_z_post( nx,ny,nz, tmp, MOMENX, 'momx-dz' )
      endif
  endif

  if (vary_in_y==1) then
      if( vary_in_x==1) then
          call derivative_x_post( nx,ny,nz, tmp, MOMENY, 'momy-dx' )
      endif
      call derivative_y_post( nx,ny,nz, tmp, MOMENY, 'momy-dy' )
      if (vary_in_z==1) then
          call derivative_z_post( nx,ny,nz, tmp, MOMENY, 'momy-dz' )
      endif
  endif

  if (vary_in_z==1) then
      if( vary_in_x==1) then
          call derivative_x_post( nx,ny,nz, tmp, MOMENZ, 'momz-dx' )
      endif
      if (vary_in_y==1) then
          call derivative_y_post( nx,ny,nz, tmp, MOMENZ, 'momz-dy' )
      endif
      call derivative_z_post( nx,ny,nz, tmp, MOMENZ, 'momz-dz' )
  endif

  if (vary_in_x==1) then
      call derivative_x_post( nx,ny,nz, tmp, CONTIN, 'cont-dx' )
      call derivative_x_post( nx,ny,nz, tmp, ENERGY, 'energy-dx' )
  endif
  if (vary_in_y==1) then
      call derivative_y_post( nx,ny,nz, tmp, CONTIN, 'cont-dy' )
      call derivative_y_post( nx,ny,nz, tmp, ENERGY, 'energy-dy' )
  endif
  if( vary_in_z==1) then
      call derivative_z_post( nx,ny,nz, tmp, CONTIN, 'cont-dz' )
      call derivative_z_post( nx,ny,nz, tmp, ENERGY, 'energy-dz' )
  endif




  !call print_deriv_list(6)
!-------------------- Momentum Equations -------------------
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! X Direction
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! rhs = -d( rho*u*u + p - tau_xx )/dx 
  !       -d( rho*u*v - tau_xy )/dy
  !       -d( rho*u*w - tau_xz )/dz
  !
  ! the "if" statements below are just to avoid
  ! empty calculations if a direction is not active
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vary_in_x==1) then

     op_work(1:nx,1:ny,1:nz,1) = -q(:,:,:,1)*u(:,:,:,1) - pressure + tau(:,:,:,1,1)
     !tmp = -q(:,:,:,1)*u(:,:,:,1) - pressure + tau(:,:,:,1,1)
     !call derivative_x_send( nx,ny,nz, tmp, MOMENX, 'momx-dx' )
     !call derivative_x_calc_buff( nx,ny,nz, tmp, rhs(:,:,:,1), &
     !                           scale_1x, 1, MOMENX )

     call do_boxlib_fill( op_workmfab )
     call derivative_x_calc_boxlib( nx,ny,nz,op_work(:,:,:,1),rhs(:,:,:,1) )


     if (vary_in_y==1) then
     !   tmp = q(:,:,:,1)*u(:,:,:,2) - tau(:,:,:,1,2)
        op_work(1:nx,1:ny,1:nz,1) = q(:,:,:,1)*u(:,:,:,2) - tau(:,:,:,1,2)
        !call derivative_y_send( nx,ny,nz, tmp, MOMENX, 'momx-dy' )
        !call derivative_y_calc_buff( nx,ny,nz, tmp, tmp2, &
        !                           scale_1y, 1, MOMENX)
        call do_boxlib_fill( op_workmfab )
        call derivative_y_calc_boxlib( nx,ny,nz,op_work(:,:,:,1),tmp2 )
        rhs(:,:,:,1) = rhs(:,:,:,1) - tmp2
     endif

     if (vary_in_z==1) then
        !tmp = q(:,:,:,1)*u(:,:,:,3) - tau(:,:,:,1,3)
        op_work(1:nx,1:ny,1:nz,1) = q(:,:,:,1)*u(:,:,:,3) - tau(:,:,:,1,3)
        call do_boxlib_fill( op_workmfab )
        !call derivative_z_send( nx,ny,nz, tmp, MOMENX, 'momx-dz' )
        !call derivative_z_calc_buff( nx,ny,nz, tmp, tmp2, & 
        !                           scale_1z, 1, MOMENX )

        call derivative_z_calc_boxlib( nx,ny,nz,op_work(:,:,:,1),tmp2 )
        rhs(:,:,:,1) = rhs(:,:,:,1) - tmp2
     endif

  else
     rhs(:,:,:,1) = 0.0
  endif

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Y Direction
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! rhs = -d( rho*v*u - tau_xy )/dx 
  !       -d( rho*v*v + p - tau_yy )/dy
  !       -d( rho*v*w - tau_yz )/dz
  !
  ! the "if" statements below are to avoid empty
  ! calculations if a direction is not active
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vary_in_y==1) then

     if (vary_in_x==1) then
        tmp = -q(:,:,:,2) * u(:,:,:,1) + tau(:,:,:,2,1)
        call derivative_x_send( nx,ny,nz, tmp, MOMENY, 'momy-dx')
        call derivative_x_calc_buff( nx,ny,nz, tmp, rhs(:,:,:,2), &
                                     scale_1x, 1, MOMENY)
        !call derivative_x( nx,ny,nz, tmp, rhs(:,:,:,2), &
        !                           scale_1x, 1) 
     else
        rhs(:,:,:,2) = 0.0
     endif

     tmp = q(:,:,:,2) * u(:,:,:,2) + pressure - tau(:,:,:,2,2)
     call derivative_y_send( nx,ny,nz, tmp, MOMENY, 'momy-dy')
     call derivative_y_calc_buff( nx,ny,nz, tmp, tmp2, scale_1y, 1, &
                                MOMENY)
     !call derivative_y( nx,ny,nz, tmp, tmp2, scale_1y, 1)
     rhs(:,:,:,2) = rhs(:,:,:,2) - tmp2

     if (vary_in_z==1) then
        tmp = q(:,:,:,2) * u(:,:,:,3) - tau(:,:,:,2,3)
        call derivative_z_send( nx,ny,nz, tmp, MOMENY, 'momy-dz' )
        call derivative_z_calc_buff( nx,ny,nz, tmp, tmp2, scale_1z, &
                                   1, MOMENY)
        !call derivative_z( nx,ny,nz, tmp, tmp2, scale_1z)
        rhs(:,:,:,2) = rhs(:,:,:,2) - tmp2
     endif

  else
     rhs(:,:,:,2) = 0.0
  endif

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Z Direction
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! rhs = -d( rho*w*u - tau_xz )/dx 
  !       -d( rho*w*v - tau_yz )/dy
  !       -d( rho*w*w + p - tau_zz )/dz
  !
  ! the "if" statements below are to avoid empty
  ! calculations if a direction is not active
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vary_in_z==1) then

     if (vary_in_x==1) then
        tmp = -q(:,:,:,3) * u(:,:,:,1) + tau(:,:,:,1,3)
        call derivative_x_send( nx,ny,nz, tmp, MOMENZ, 'momz-dx' )
        call derivative_x_calc_buff( nx,ny,nz, tmp, rhs(:,:,:,3), scale_1x, 1, &
                                   MOMENZ )
        !call derivative_x( nx,ny,nz, tmp, rhs(:,:,:,3), scale_1x, 1) 
     else
        rhs(:,:,:,3) = 0.0
     endif

     if (vary_in_y==1) then
        tmp = q(:,:,:,3) * u(:,:,:,2) - tau(:,:,:,2,3)
        call derivative_y_send( nx,ny,nz, tmp, MOMENZ, 'momz-dy' )
        call derivative_y_calc_buff( nx,ny,nz, tmp, tmp2, scale_1y, 1, &
                                   MOMENZ)
        !call derivative_y( nx,ny,nz, tmp, tmp2, scale_1y, 1)
        rhs(:,:,:,3) = rhs(:,:,:,3) - tmp2
     endif

     tmp = q(:,:,:,3) * u(:,:,:,3) + pressure - tau(:,:,:,3,3)
     call derivative_z_send( nx,ny,nz, tmp, MOMENZ, 'momz-dz' )
     call derivative_z_calc_buff( nx,ny,nz, tmp, tmp2, scale_1z, 1, &
                                MOMENZ)
     !call derivative_z( nx,ny,nz, tmp, tmp2, scale_1z, 1)
     rhs(:,:,:,3) = rhs(:,:,:,3) - tmp2

  else
     rhs(:,:,:,3) = 0.0
  endif


!------------------- Continuity Equation -------------------
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! rhs = - d( rho*u )/dx
  !       - d( rho*v )/dy
  !       - d( rho*w )/dz
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vary_in_x==1) then
     !call derivative_x( nx,ny,nz, -q(:,:,:,1), rhs(:,:,:,4), scale_1x, 1) 
     call derivative_x_send( nx,ny,nz, -q(:,:,:,1), CONTIN, 'cont-dx' )
     call derivative_x_calc_buff( nx,ny,nz, -q(:,:,:,1), rhs(:,:,:,4), scale_1x, 1, &
                                CONTIN)
  else
     rhs(:,:,:,4) = 0.0
  endif

  if (vary_in_y==1) then
     call derivative_y_send( nx,ny,nz, q(:,:,:,2), CONTIN, 'cont-dy' )
     call derivative_y_calc_buff( nx,ny,nz, q(:,:,:,2), tmp, scale_1y, 1, &
                                CONTIN)
     !call derivative_y( nx,ny,nz, q(:,:,:,2), tmp, scale_1y, 1)
     rhs(:,:,:,4) = rhs(:,:,:,4) - tmp
  endif

  if (vary_in_z==1) then
     call derivative_z_send( nx,ny,nz, q(:,:,:,3), CONTIN, 'cont-dz')
     call derivative_z_calc_buff( nx,ny,nz, q(:,:,:,3), tmp, scale_1z, 1, &
                                CONTIN)
     !call derivative_z( nx,ny,nz, q(:,:,:,3), tmp, scale_1z, 1)
     rhs(:,:,:,4) = rhs(:,:,:,4) - tmp
  endif


!------------------- Energy Equation ------------------

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! rhs = d( -u*(rho*Et+p) + u*tau_xx + v*tau_xy + w*tau_xz - qx )/dx
  !     + d( -v*(rho*Et+p) + u*tau_xy + v*tau_yy + w*tau_yz - qy )/dy
  !     + d( -w*(rho*Et+p) + u*tau_xz + v*tau_yz + w*tau_zz - qz )/dz
  !     + source terms
  ! source terms appearing in the enerqy equation are due to radiation.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (vary_in_x==1) then
     tmp = - u(:,:,:,1)*(q(:,:,:,5) + pressure - tau(:,:,:,1,1))        &
           + u(:,:,:,2)*tau(:,:,:,1,2)                                  &
           + u(:,:,:,3)*tau(:,:,:,1,3)                                  &
           - heatFlux(:,:,:,1)
     call derivative_x_send( nx,ny,nz, tmp, ENERGY, 'energy-dx')
     call derivative_x_calc_buff( nx,ny,nz, tmp, rhs(:,:,:,5), scale_1x, 1 , &
                                ENERGY)
     !call derivative_x( nx,ny,nz, tmp, rhs(:,:,:,5), scale_1x, 1 )
  else
     rhs(:,:,:,5) = 0.0
  endif

  if (vary_in_y==1) then
     tmp = u(:,:,:,1)*tau(:,:,:,2,1)                            &
         - u(:,:,:,2)*(q(:,:,:,5) + pressure - tau(:,:,:,2,2))  &
         + u(:,:,:,3)*tau(:,:,:,2,3)                            &
         - heatFlux(:,:,:,2)
     call derivative_y_send( nx,ny,nz,tmp, ENERGY, 'energy-dy' )
     call derivative_y_calc_buff( nx,ny,nz,tmp,tmp2,scale_1y,1,&
                                ENERGY)
     !call derivative_y( nx,ny,nz,tmp,tmp2,scale_1y,1)
     rhs(:,:,:,5) = rhs(:,:,:,5) + tmp2
  endif

  if (vary_in_z==1) then
     tmp = u(:,:,:,1)*tau(:,:,:,1,3)                            &
         + u(:,:,:,2)*tau(:,:,:,2,3)                            &
         - u(:,:,:,3)*(q(:,:,:,5) + pressure - tau(:,:,:,3,3))  &
         - heatFlux(:,:,:,3)
     call derivative_z_send( nx,ny,nz,tmp, ENERGY, 'energy-dz' )
     call derivative_z_calc_buff( nx,ny,nz,tmp,tmp2,scale_1z,1, &
                                ENERGY)
     !call derivative_z( nx,ny,nz,tmp,tmp2,scale_1z,1)
     rhs(:,:,:,5) = rhs(:,:,:,5) + tmp2
  endif

  if (i_opthinrad == 1) then
     call opthinrad( tmp, yspecies, pressure, temp )
     rhs(:,:,:,5) = rhs(:,:,:,5) + tmp
  endif

!------------------ Species Equations ------------------
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! rhs = d( -rho*u*Y - Jx )/dx
  !       d( -rho*v*Y - Jy )/dy
  !       d( -rho*w*Y - Jz )/dz
  !       + rr
  ! where:
  !       Jx -> diffusive flux in X direction (non-dimensional)
  !       Jy -> diffusive flux in Y direction (non-dimensional)
  !       Jz -> diffusive flux in Z direction (non-dimensional)
  !       rr -> Species MASS production rate  (non-dimensional)
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! Modified by Ramanan. 04/08/05
! Do the loop separately for the three directions. 
! Uncoupling the three directions might improve communication. 
#define SPECX 1
#define SPECY 2
#define SPECZ 3
#define SPCDIFX 4
#define SPCDIFY 5
#define SPCDIFZ 6
  SPECIESX: do n=1,n_spec-1

     !Commented out by Ramanan - 01/06/05
     ! rr_r is added later. See code below this do loop
     !if(rk_type=='erk') then
     !  rhs(:,:,:,5+n) = rr_r(:,:,:,n)
     !else
     !  rhs(:,:,:,5+n) = 0.0
     !endif


     ! add convective and diffusive terms...

     if (vary_in_x==1) then
#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_x_post( nx,ny,nz, tmp, SPCDIFX, 'diffFlux-x')
#endif
        call derivative_x_post( nx,ny,nz, tmp, SPECX, 'spec-x')

#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_x_send( nx,ny,nz, -diffFlux(:,:,:,n,1), SPCDIFX, 'diffFlux-x' )
#endif
        tmp(:,:,:) = -q(:,:,:,5+n)*u(:,:,:,1) - diffFlux(:,:,:,n,1)

        call derivative_x_send( nx,ny,nz, tmp, SPECX, 'spec-x' )
#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_x_calc_buff( nx,ny,nz,-diffFlux(:,:,:,n,1),tmp2,scale_1x, 1, SPCDIFX )
        diffusion(:,:,:,n) = tmp2
#endif        
        call derivative_x_calc_buff( nx,ny,nz, tmp, tmp2, scale_1x, 1, SPECX )
        rhs(:,:,:,5+n) = tmp2
     else
        rhs(:,:,:,5+n) = 0.0
#ifdef GETRATES_NEEDS_DIFFUSION
        diffusion(:,:,:,n) = 0.0
#endif
     endif
  enddo SPECIESX

  SPECIESY: do n=1,n_spec-1
     if (vary_in_y==1) then

#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_y_post( nx,ny,nz,-diffFlux(:,:,:,n,2),SPCDIFY,'diffFlux-y' )
#endif
        call derivative_y_post( nx,ny,nz, tmp, SPECY, 'spec-y' )

#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_y_send( nx,ny,nz,-diffFlux(:,:,:,n,2),SPCDIFY, 'diffFlux-y' )
#endif
        tmp(:,:,:) = -q(:,:,:,5+n)*u(:,:,:,2) - diffFlux(:,:,:,n,2)

        call derivative_y_send( nx,ny,nz, tmp, SPECY, 'spec-y' )
#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_y_calc_buff( nx,ny,nz,-diffFlux(:,:,:,n,2),tmp2,scale_1y, 1, SPCDIFY )
        diffusion(:,:,:,n) = diffusion(:,:,:,n) + tmp2
#endif
        call derivative_y_calc_buff( nx,ny,nz,tmp, tmp2, scale_1y, 1, SPECY )
        rhs(:,:,:,5+n) = rhs(:,:,:,5+n)+tmp2
     endif
  enddo SPECIESY

  SPECIESZ: do n=1,n_spec-1
     if (vary_in_z==1) then

#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_z_post( nx,ny,nz, -diffFlux(:,:,:,n,3), SPCDIFZ, 'diffFlux-z' )
#endif
        call derivative_z_post( nx,ny,nz, tmp, SPECZ, 'spec-z' )

#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_z_send( nx,ny,nz,-diffFlux(:,:,:,n,3), SPCDIFZ, 'diffFlux-z' )
#endif

        tmp(:,:,:) = -q(:,:,:,5+n)*u(:,:,:,3) - diffFlux(:,:,:,n,3)

        call derivative_z_send( nx,ny,nz, tmp,  SPECZ, 'spec-z' )

#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_z_calc_buff( nx,ny,nz,-diffFlux(:,:,:,n,3),tmp2,scale_1z, 1, SPCDIFZ )
        diffusion(:,:,:,n) = diffusion(:,:,:,n) + tmp2
#endif
        call derivative_z_calc_buff( nx,ny,nz, tmp, tmp2, scale_1z, 1, SPECZ )
        rhs(:,:,:,5+n) = rhs(:,:,:,5+n)+tmp2
     endif
  enddo SPECIESZ

#ifdef GETRATES_NEEDS_DIFFUSION
!Calculate diffusion term of the last species for n-heptane mechanism

  if (vary_in_x==1) then
     call derivative_x_post( nx,ny,nz,-diffFlux(:,:,:,n_spec,1),SPCDIFX, 'diffFlux-x') 
     call derivative_x_send( nx,ny,nz,-diffFlux(:,:,:,n_spec,1),SPCDIFX, 'diffFlux-x') 
  endif

  if (vary_in_y==1) then
     call derivative_y_post( nx,ny,nz,-diffFlux(:,:,:,n_spec,2),SPCDIFY, 'diffFlux-y' )
     call derivative_y_send( nx,ny,nz,-diffFlux(:,:,:,n_spec,2),SPCDIFY, 'diffFlux-y' )
  endif

  if (vary_in_z==1) then
     call derivative_z_post( nx,ny,nz,-diffFlux(:,:,:,n_spec,3), SPCDIFZ, 'diffFlux-z' )
     call derivative_z_send( nx,ny,nz,-diffFlux(:,:,:,n_spec,3), SPCDIFZ, 'diffFlux-z' )
  endif

  if (vary_in_x==1) then
     call derivative_x_calc_buff( nx,ny,nz,-diffFlux(:,:,:,n_spec,1),tmp2,scale_1x, 1, SPCDIFX ) 
     diffusion(:,:,:,n_spec) = tmp2
  else
     diffusion(:,:,:,n_spec) = 0.0
  endif

  if (vary_in_y==1) then
     call derivative_y_calc_buff( nx,ny,nz,-diffFlux(:,:,:,n_spec,2),tmp2,scale_1y, 1, SPCDIFY )
     diffusion(:,:,:,n_spec) = diffusion(:,:,:,n_spec) + tmp2
  endif

  if (vary_in_z==1) then
     call derivative_z_calc_buff( nx,ny,nz,-diffFlux(:,:,:,n_spec,3),tmp2,scale_1z, 1, SPCDIFZ )
     diffusion(:,:,:,n_spec) = diffusion(:,:,:,n_spec) + tmp2
  endif

!  diffusion(:,:,:,n_spec) = 0.0
#endif

!Changes by Ramanan Sankaran - 01/06/05
! Reaction rate used to be calculated for both ERK and ARK
! For ARK the source term is added later in integration
! For ARK, at this point, we need the rr_r only at the boundary points
! That is what the rewritten code does.
!
! Furthermore rr_r does not need its own array
! It can be computed directly in the diffFlux vector, 
! which is not needed anymore

  rr_r => diffFlux(:,:,:,:,1)

  if (i_react==1) then
     if(rk_type=='erk') then
#ifdef GETRATES_NEEDS_DIFFUSION
  call reaction_rate( rr_r, temp, pressure, yspecies,  &
       diffusion, tstep, g_ref, rho_ref, a_ref, l_ref, t_o )
#else
  call reaction_rate( rr_r, temp, pressure, yspecies,  &
                         g_ref, rho_ref, a_ref, l_ref, t_o )
#endif
  
#ifdef X1
! Added by Mark Fahey, ORNL. 
! Helps on Cray
!DIR$ CONCURRENT
#endif
       rhs(:,:,:,6:5+n_spec-1) = rhs(:,:,:,6:5+n_spec-1)+rr_r(:,:,:,1:n_spec-1)
     else
       rr_r = 0.0
     endif
  else
    rr_r = 0.0
     rr_r_x0 = 0.0
     rr_r_xL = 0.0
     rr_r_y0 = 0.0
     rr_r_yL = 0.0
     rr_r_z0 = 0.0
     rr_r_zL = 0.0
  endif

  !Added by Ramanan - 01/06/05
  !store the boundary rr_r values.
  !If ERK it was computed above so use those values
  !If ARK compute only for the necessary sections.
  if (i_react==1) then
  if(vary_in_x==1)then
    if (xid==0) then
      if(rk_type == 'erk') then
        rr_r_x0 = rr_r(1,:,:,:)
!      else 
!#ifdef GETRATES_NEEDS_DIFFUSION
!       call reaction_rate_bounds(rr_r_x0, temp(1,:,:), & 
!             pressure(1,:,:), yspecies(1,:,:,:), diffusion(1,:,:,:), tstep, &
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, 1, 1, ny, 1, nz)
!#else
!       call reaction_rate_bounds(rr_r_x0, temp(1,:,:), & 
!             pressure(1,:,:), yspecies(1,:,:,:),&
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, 1, 1, ny, 1, nz)
!#endif
      end if
    end if
  
    if (xid==xpes-1) then
      if(rk_type == 'erk') then
        rr_r_xL = rr_r(nx,:,:,:)
!      else 
!#ifdef GETRATES_NEEDS_DIFFUSION
!       call reaction_rate_bounds(rr_r_xL, temp(nx,:,:), &
!             pressure(nx,:,:), yspecies(nx,:,:,:), diffusion(nx,:,:,:), tstep, &
!             g_ref, rho_ref, a_ref, l_ref, t_o, nx, nx, 1, ny, 1, nz)
!#else
!       call reaction_rate_bounds(rr_r_xL, temp(nx,:,:), &
!             pressure(nx,:,:), yspecies(nx,:,:,:),&
!             g_ref, rho_ref, a_ref, l_ref, t_o, nx, nx, 1, ny, 1, nz)
!#endif
      end if
    end if
  endif

  if(vary_in_y==1)then
    if (yid==0) then
      if(rk_type == 'erk') then
        rr_r_y0 = rr_r(:,1,:,:)
!      else 
!#ifdef GETRATES_NEEDS_DIFFUSION
!       call reaction_rate_bounds(rr_r_y0, temp(:,1,:), &
!             pressure(:,1,:), yspecies(:,1,:,:), diffusion(:,1,:,:), tstep, &
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, 1, 1, nz)
!#else
!        call reaction_rate_bounds(rr_r_y0, temp(:,1,:), &
!             pressure(:,1,:), yspecies(:,1,:,:),&
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, 1, 1, nz)
!#endif
      end if
    end if
  
    if (yid==ypes-1) then
      if(rk_type == 'erk') then
        rr_r_yL = rr_r(:,ny,:,:)
!      else 
!#ifdef GETRATES_NEEDS_DIFFUSION
!       call reaction_rate_bounds(rr_r_yL, temp(:,ny,:), &
!             pressure(:,ny,:), yspecies(:,ny,:,:), diffusion(:,ny,:,:), tstep, &
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, ny, ny, 1, nz)
!#else
!       call reaction_rate_bounds(rr_r_yL, temp(:,ny,:), &
!             pressure(:,ny,:), yspecies(:,ny,:,:),&
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, ny, ny, 1, nz)
!#endif
      end if
    end if
  endif

  if(vary_in_z==1)then
    if (zid==0) then
      if(rk_type == 'erk') then
        rr_r_z0 = rr_r(:,:,1,:)
!      else 
!#ifdef GETRATES_NEEDS_DIFFUSION
!       call reaction_rate_bounds(rr_r_z0, temp(:,:,1), &
!             pressure(:,:,1), yspecies(:,:,1,:), diffusion(:,:,1,:), tstep, &
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, ny, 1, 1)
!#else
!       call reaction_rate_bounds(rr_r_z0, temp(:,:,1), &
!             pressure(:,:,1), yspecies(:,:,1,:),&
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, ny, 1, 1)
!#endif
      end if
    end if
  
    if (zid==zpes-1) then
      if(rk_type == 'erk') then
        rr_r_zL = rr_r(:,:,nz,:)
!      else 
!#ifdef GETRATES_NEEDS_DIFFUSION
!       call reaction_rate_bounds(rr_r_zL, temp(:,:,nz), &
!             pressure(:,:,nz), yspecies(:,:,nz,:), diffusion(:,:,nz,:), tstep,&
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, ny, nz, nz)
!#else
!       call reaction_rate_bounds(rr_r_zL, temp(:,:,nz), &
!             pressure(:,:,nz), yspecies(:,:,nz,:),&
!             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, ny, nz, nz)
!#endif
      end if
    end if
  endif
  endif
  nullify (rr_r)



!*******************************************************************************


  if (i_compheat == 1) then
     !-----------------------------------------------------------------------
     ! calculate compression heating source terms and add to all equations.
     !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     ! note: this model depends on the primative variables, not the values of
     !       the q-vector.
     ! note: the sources are accumulated to EACH rhs inside the routine
     !-----------------------------------------------------------------------
     call compheat(rhs,q,u,yspecies,temp,pressure,volum,gamma,avmolwt,time_accum,i_time)
  endif

!===============================================================================
! STEP 5:  Impose Navier-Stokes Characteristic Boundary Conditions
!===============================================================================

! impose NSCBC's using Characteristic Wave analysis
! notes by Ramanan - nscbc subroutines have an if condition on xid, yid etc.
! Even though all pid's call the subroutine, others dont have anything execute.
!  if(vary_in_x==1 ) & 
!       call nscbc_x( q, h_spec_x0, h_spec_xL, rr_r_x0, rr_r_xL, &
!                     grad_Ys_x0, grad_Ys_xL, grad_u_x0, grad_u_xL, rhs )
!  if(vary_in_y==1 ) & 
!       call nscbc_y( q, h_spec_y0, h_spec_yL, rr_r_y0, rr_r_yL, &
!                     grad_Ys_y0, grad_Ys_yL, grad_u_y0, grad_u_yL, rhs )
!  if(vary_in_z==1 ) &
!       call nscbc_z( q, h_spec_z0, h_spec_zL, rr_r_z0, rr_r_zL, &
!                     grad_Ys_z0, grad_Ys_zL, grad_u_z0, grad_u_zL, rhs )

! Change by Chun Sang Yoo (03/15/06)

  if(vary_in_x==1 ) & 
       call nscbc_x( q, h_spec_x0, h_spec_xL, rr_r_x0, rr_r_xL, &
                     grad_Ys_x0, grad_Ys_xL, grad_u_x0, grad_u_xL, rhs, &
                     grad_u_x0_all, grad_u_xL_all, tau_x0, tau_xL, &
                     div_heatFlux_x0, div_heatFlux_xL, div_tau_x_x0, div_tau_x_xL, &
                     div_tau_y_x0, div_tau_y_xL, div_tau_z_x0, div_tau_z_xL, &
                     div_diffFlux_x0, div_diffFlux_xL )
  if(vary_in_y==1 ) & 
#ifndef USE_HISTORIC_BC
       call nscbc_y( q, h_spec_y0, h_spec_yL, rr_r_y0, rr_r_yL, &
                     grad_Ys_y0, grad_Ys_yL, grad_u_y0, grad_u_yL, rhs, &
                     grad_u_y0_all, grad_u_yL_all, tau_y0, tau_yL, &
                     div_heatFlux_y0, div_heatFlux_yL, div_tau_x_y0, div_tau_x_yL, &
                     div_tau_y_y0, div_tau_y_yL, div_tau_z_y0, div_tau_z_yL, &
                     div_diffFlux_y0, div_diffFlux_yL )
#else
       call nscbc_y( q, h_spec_y0, h_spec_yL, rr_r_y0, rr_r_yL, &
                     grad_Ys_y0, grad_Ys_yL, grad_u_y0, grad_u_yL, rhs )
#endif
  if(vary_in_z==1 ) &
#ifndef USE_HISTORIC_BC
       call nscbc_z( q, h_spec_z0, h_spec_zL, rr_r_z0, rr_r_zL, &
                     grad_Ys_z0, grad_Ys_zL, grad_u_z0, grad_u_zL, rhs, &
                     grad_u_z0_all, grad_u_zL_all, tau_z0, tau_zL, &
                     div_heatFlux_z0, div_heatFlux_zL, div_tau_x_z0, div_tau_x_zL, &
                     div_tau_y_z0, div_tau_y_zL, div_tau_z_z0, div_tau_z_zL, &
                     div_diffFlux_z0, div_diffFlux_zL )
#else
       call nscbc_z( q, h_spec_z0, h_spec_zL, rr_r_z0, rr_r_zL, &
                     grad_Ys_z0, grad_Ys_zL, grad_u_z0, grad_u_zL, rhs )
#endif
!jcs 6-9-03
  if( vary_in_x==1 .and. periodic_x==0 .and. run_mode .eq. 'solve') then
     if( nrf_x0==-2 ) then
        call inflowA_BCx(q(:,:,:,4), rhs(:,:,:,4), h_spec_x0, rr_r_x0, &
                         grad_u_x0)
     endif
     if( nrf_xl==-2 ) then
        call inflowA_BCx(q(:,:,:,4), rhs(:,:,:,4), h_spec_xL, rr_r_xL, &
                         grad_u_xL)
     endif
  endif


!---------------------------- FINISHED ----------------------------

  return
end subroutine rhsf

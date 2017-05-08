#include "globalDefines.h"
subroutine rhsf( q, rhs )
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
  use param_m, only : vary_in_x, vary_in_y, vary_in_z, periodic_x
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

  implicit none

!-------------------------- Passed Variables -------------------------

  real, dimension(nx,ny,nz,nvar_tot), intent(in)  :: q
  real, target, dimension(nx,ny,nz,nvar_tot), intent(out) :: rhs

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

  real, target, dimension(nx,ny,nz,3,n_spec) :: spcs_store
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

#ifdef GETRATES_NEEDS_DIFFUSION
  real, dimension(nx, ny, nz, n_spec) :: diffusion
#endif

  integer :: i,j,k,n,m,ierrr

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
     call computeScalarGradient( yspecies(:,:,:,n), grad_Ys(:,:,:,:,n) )
  enddo

!Added by Ramanan - 01/05/05
!Store the boundary grad values
  if(vary_in_x==1)then
    if (xid==0) then
      grad_u_x0 = grad_u(1,:,:,1,:)
      grad_Ys_x0 = grad_Ys(1,:,:,1,:)
      h_spec_x0 = h_spec(1,:,:,:)
    end if
  
    if (xid==xpes-1) then
      grad_u_xL = grad_u(nx,:,:,1,:)
      grad_Ys_xL = grad_Ys(nx,:,:,1,:)
      h_spec_xL = h_spec(nx,:,:,:)
    end if
  endif

  if(vary_in_y==1)then
    if (yid==0) then
      grad_u_y0 = grad_u(:,1,:,2,:)
      grad_Ys_y0 = grad_Ys(:,1,:,2,:)
      h_spec_y0 = h_spec(:,1,:,:)
    end if
  
    if (yid==ypes-1) then
      grad_u_yL = grad_u(:,ny,:,2,:)
      grad_Ys_yL = grad_Ys(:,ny,:,2,:)
      h_spec_yL = h_spec(:,ny,:,:)
    end if
  endif

  if(vary_in_z==1)then
    if (zid==0) then
      grad_u_z0 = grad_u(:,:,1,3,:)
      grad_Ys_z0 = grad_Ys(:,:,1,3,:)
      h_spec_z0 = h_spec(:,:,1,:)
    end if
  
    if (zid==zpes-1) then
      grad_u_zL = grad_u(:,:,nz,3,:)
      grad_Ys_zL = grad_Ys(:,:,nz,3,:)
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



!===============================================================================
! STEP 4:  Assemble the RHS for each governing equation
!===============================================================================


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

     tmp = -q(:,:,:,1)*u(:,:,:,1) - pressure + tau(:,:,:,1,1)
     call derivative_x( nx,ny,nz, tmp, rhs(:,:,:,1), scale_1x, 1 )

     if (vary_in_y==1) then
        tmp = q(:,:,:,1)*u(:,:,:,2) - tau(:,:,:,1,2)
        call derivative_y( nx,ny,nz, tmp, tmp2, scale_1y, 1 )
        rhs(:,:,:,1) = rhs(:,:,:,1) - tmp2
     endif

     if (vary_in_z==1) then
        tmp = q(:,:,:,1)*u(:,:,:,3) - tau(:,:,:,1,3)
        call derivative_z( nx,ny,nz, tmp, tmp2, scale_1z, 1 )
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
        call derivative_x( nx,ny,nz, tmp, rhs(:,:,:,2), scale_1x, 1 )
     else
        rhs(:,:,:,2) = 0.0
     endif

     tmp = q(:,:,:,2) * u(:,:,:,2) + pressure - tau(:,:,:,2,2)
     call derivative_y( nx,ny,nz, tmp, tmp2, scale_1y, 1 )
     rhs(:,:,:,2) = rhs(:,:,:,2) - tmp2

     if (vary_in_z==1) then
        tmp = q(:,:,:,2) * u(:,:,:,3) - tau(:,:,:,2,3)
        call derivative_z( nx,ny,nz, tmp, tmp2, scale_1z, 1 )
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
        call derivative_x( nx,ny,nz, tmp, rhs(:,:,:,3), scale_1x, 1 )
     else
        rhs(:,:,:,3) = 0.0
     endif

     if (vary_in_y==1) then
        tmp = q(:,:,:,3) * u(:,:,:,2) - tau(:,:,:,2,3)
        call derivative_y( nx,ny,nz, tmp, tmp2, scale_1y, 1 )
        rhs(:,:,:,3) = rhs(:,:,:,3) - tmp2
     endif

     tmp = q(:,:,:,3) * u(:,:,:,3) + pressure - tau(:,:,:,3,3)
     call derivative_z( nx,ny,nz, tmp, tmp2, scale_1z, 1 )
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
     call derivative_x( nx,ny,nz, -q(:,:,:,1), rhs(:,:,:,4), scale_1x, 1 )
  else
     rhs(:,:,:,4) = 0.0
  endif

  if (vary_in_y==1) then
     call derivative_y( nx,ny,nz, q(:,:,:,2), tmp, scale_1y, 1 )
     rhs(:,:,:,4) = rhs(:,:,:,4) - tmp
  endif

  if (vary_in_z==1) then
     call derivative_z( nx,ny,nz, q(:,:,:,3), tmp, scale_1z, 1 )
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
     call derivative_x( nx,ny,nz, tmp, rhs(:,:,:,5), scale_1x, 1 )
  else
     rhs(:,:,:,5) = 0.0
  endif

  if (vary_in_y==1) then
     tmp = u(:,:,:,1)*tau(:,:,:,2,1)                            &
         - u(:,:,:,2)*(q(:,:,:,5) + pressure - tau(:,:,:,2,2))  &
         + u(:,:,:,3)*tau(:,:,:,2,3)                            &
         - heatFlux(:,:,:,2)
     call derivative_y( nx,ny,nz,tmp,tmp2,scale_1y,1 )
     rhs(:,:,:,5) = rhs(:,:,:,5) + tmp2
  endif

  if (vary_in_z==1) then
     tmp = u(:,:,:,1)*tau(:,:,:,1,3)                            &
         + u(:,:,:,2)*tau(:,:,:,2,3)                            &
         - u(:,:,:,3)*(q(:,:,:,5) + pressure - tau(:,:,:,3,3))  &
         - heatFlux(:,:,:,3)
     call derivative_z( nx,ny,nz,tmp,tmp2,scale_1z,1 )
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
        !call pat_region_begin(1,'loop prior to call to der_x',ierrr)
        do k=1,nz
        do j=1,ny
        do i=1,nx
        tmp(i,j,k) = -q(i,j,k,5+n)*u(i,j,k,1) - diffFlux(i,j,k,n,1)
        enddo
        enddo
        enddo
        !call pat_region_end(1,ierrr)
        call derivative_x( nx,ny,nz, tmp, tmp2, scale_1x, 1 )
        !call pat_region_begin(2,'loop after to call to der_x',ierrr)
        do k=1,nz
        do j=1,ny
        do i=1,nx
        rhs(i,j,k,5+n) = tmp2(i,j,k)
        enddo
        enddo
        enddo
        !call pat_region_end(2,ierrr)
#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_x( nx,ny,nz, -diffFlux(i,j,k,n,1), tmp2, scale_1x, 1 )
        do k=1,nz
        do j=1,ny
        do i=1,nx
        diffusion(i,j,k,n) = tmp2(i,j,k)
        enddo
        enddo
        enddo
#endif
     else
#ifdef GETRATES_NEEDS_DIFFUSION
        do k=1,nz
        do j=1,ny
        do i=1,nx
        rhs(i,j,k,5+n) = 0.0
        diffusion(i,j,k,n)=0.0
        enddo
        enddo
        enddo
#else
        do k=1,nz
        do j=1,ny
        do i=1,nx
        rhs(i,j,k,5+n) = 0.0
        enddo
        enddo
        enddo
#endif
     endif
  enddo SPECIESX

  SPECIESY: do n=1,n_spec-1
     if (vary_in_y==1) then
        !call pat_region_begin(3,'loop prior to call to der_y',ierrr)
        do k=1,nz
        do j=1,ny
        do i=1,nx
        tmp(i,j,k) = -q(i,j,k,5+n)*u(i,j,k,2) - diffFlux(i,j,k,n,2)
        enddo
        enddo
        enddo
        !call pat_region_end(3,ierrr)
        call derivative_y( nx,ny,nz, tmp, tmp2, scale_1y, 1 )
        !call pat_region_begin(4,'loop after to call to der_y',ierrr)
        do k=1,nz
        do j=1,ny
        do i=1,nx
        rhs(i,j,k,5+n) = rhs(i,j,k,5+n)+tmp2(i,j,k)
        enddo
        enddo
        enddo
        !call pat_region_end(4,ierrr)
#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_y( nx,ny,nz, -diffFlux(i,j,k,n,2), tmp2, scale_1y, 1 )
        do k=1,nz
        do j=1,ny
        do i=1,nx
        diffusion(i,j,k,n) = diffusion(i,j,k,n) + tmp2(i,j,k)
        enddo
        enddo
        enddo
#endif
     endif
  enddo SPECIESY

  SPECIESZ: do n=1,n_spec-1
     if (vary_in_z==1) then
        !call pat_region_begin(5,'loop prior to call to der_z',ierrr)
        do k=1,nz
        do j=1,ny
        do i=1,nx
        tmp(i,j,k) = -q(i,j,k,5+n)*u(i,j,k,3) - diffFlux(i,j,k,n,3)
        enddo
        enddo
        enddo
        !call pat_region_end(5,ierrr)
        call derivative_z( nx,ny,nz, tmp, tmp2, scale_1z, 1 )
        !call pat_region_begin(6,'loop after to call to der_z',ierrr)
        do k=1,nz
        do j=1,ny
        do i=1,nx
        rhs(i,j,k,5+n) = rhs(i,j,k,5+n)+tmp2(i,j,k)
        enddo
        enddo
        enddo
        !call pat_region_end(6,ierrr)
#ifdef GETRATES_NEEDS_DIFFUSION
        call derivative_z( nx,ny,nz, -diffFlux(i,j,k,n,3), tmp2, scale_1z, 1 )
        do k=1,nz
        do j=1,ny
        do i=1,nx
        diffusion(i,j,k,n) = diffusion(i,j,k,n) + tmp2(i,j,k)
        enddo
        enddo
        enddo
#endif
     endif
  enddo SPECIESZ

#ifdef GETRATES_NEEDS_DIFFUSION
!Calculate diffusion term of the last species for n-heptane mechanism

  if (vary_in_x==1) then
     call derivative_x( nx,ny,nz,-diffFlux(:,:,:,n_spec,1),tmp2,scale_1x, 1 ) 
     do k=1,nz
     do j=1,ny
     do i=1,nz
     diffusion(i,j,k,n_spec) = tmp2(i,j,k)
     enddo
     enddo
     enddo
  else
     do k=1,nz
     do j=1,ny
     do i=1,nz
     diffusion(i,j,k,n_spec) = 0.0
     enddo
     enddo
     enddo
  endif

  if (vary_in_y==1) then
     call derivative_y( nx,ny,nz,-diffFlux(:,:,:,n_spec,2),tmp2,scale_1y, 1 )
     do k=1,nz
     do j=1,ny
     do i=1,nz
     diffusion(i,j,k,n_spec) = diffusion(i,j,k,n_spec) + tmp2(i,j,k)
     enddo
     enddo
     enddo
  endif

  if (vary_in_z==1) then
     call derivative_z( nx,ny,nz,-diffFlux(:,:,:,n_spec,3),tmp2,scale_1z, 1 )
     do k=1,nz
     do j=1,ny
     do i=1,nz
     diffusion(i,j,k,n_spec) = diffusion(i,j,k,n_spec) + tmp2(i,j,k)
     enddo
     enddo
     enddo
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
       call reaction_rate( rr_r, temp, pressure, yspecies, diffusion, tstep,  &
                           g_ref, rho_ref, a_ref, l_ref, t_o )
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
  endif

  !Added by Ramanan - 01/06/05
  !store the boundary rr_r values.
  !If ERK it was computed above so use those values
  !If ARK compute only for the necessary sections.
  if(vary_in_x==1)then
    if (xid==0) then
      if(rk_type == 'erk') then
        rr_r_x0 = rr_r(1,:,:,:)
      else 
#ifdef GETRATES_NEEDS_DIFFUSION
       call reaction_rate_bounds(rr_r_x0, temp(1,:,:), & 
             pressure(1,:,:), yspecies(1,:,:,:), diffusion(1,:,:,:), tstep, &
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, 1, 1, ny, 1, nz)
#else
       call reaction_rate_bounds(rr_r_x0, temp(1,:,:), & 
             pressure(1,:,:), yspecies(1,:,:,:),&
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, 1, 1, ny, 1, nz)
#endif
      end if
    end if
  
    if (xid==xpes-1) then
      if(rk_type == 'erk') then
        rr_r_xL = rr_r(nx,:,:,:)
      else 
#ifdef GETRATES_NEEDS_DIFFUSION
       call reaction_rate_bounds(rr_r_xL, temp(nx,:,:), &
             pressure(nx,:,:), yspecies(nx,:,:,:), diffusion(nx,:,:,:), tstep, &
             g_ref, rho_ref, a_ref, l_ref, t_o, nx, nx, 1, ny, 1, nz)
#else       
       call reaction_rate_bounds(rr_r_xL, temp(nx,:,:), &
             pressure(nx,:,:), yspecies(nx,:,:,:),&
             g_ref, rho_ref, a_ref, l_ref, t_o, nx, nx, 1, ny, 1, nz)
#endif
      end if
    end if
  endif

  if(vary_in_y==1)then
    if (yid==0) then
      if(rk_type == 'erk') then
        rr_r_y0 = rr_r(:,1,:,:)
      else 
#ifdef GETRATES_NEEDS_DIFFUSION
          call reaction_rate_bounds(rr_r_y0, temp(:,1,:), &
             pressure(:,1,:), yspecies(:,1,:,:), diffusion(:,1,:,:), tstep, &
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, 1, 1, nz)
#else          
          call reaction_rate_bounds(rr_r_y0, temp(:,1,:), &
             pressure(:,1,:), yspecies(:,1,:,:),&
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, 1, 1, nz)
#endif         
      end if
    end if
  
    if (yid==ypes-1) then
      if(rk_type == 'erk') then
        rr_r_yL = rr_r(:,ny,:,:)
      else 
#ifdef GETRATES_NEEDS_DIFFUSION
       call reaction_rate_bounds(rr_r_yL, temp(:,ny,:), &
             pressure(:,ny,:), yspecies(:,ny,:,:), diffusion(:,ny,:,:), tstep, &
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, ny, ny, 1, nz)
#else
       call reaction_rate_bounds(rr_r_yL, temp(:,ny,:), &
             pressure(:,ny,:), yspecies(:,ny,:,:),&
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, ny, ny, 1, nz)
#endif       
      end if
    end if
  endif

  if(vary_in_z==1)then
    if (zid==0) then
      if(rk_type == 'erk') then
        rr_r_z0 = rr_r(:,:,1,:)
      else 
#ifdef GETRATES_NEEDS_DIFFUSION          
       call reaction_rate_bounds(rr_r_z0, temp(:,:,1), &
             pressure(:,:,1), yspecies(:,:,1,:), diffusion(:,:,1,:), tstep, &
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, ny, 1, 1)
#else
       call reaction_rate_bounds(rr_r_z0, temp(:,:,1), &
             pressure(:,:,1), yspecies(:,:,1,:),&
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, ny, 1, 1)
#endif
      end if
    end if
  
    if (zid==zpes-1) then
      if(rk_type == 'erk') then
        rr_r_zL = rr_r(:,:,nz,:)
      else 
#ifdef GETRATES_NEEDS_DIFFUSION          
       call reaction_rate_bounds(rr_r_zL, temp(:,:,nz), &
             pressure(:,:,nz), yspecies(:,:,nz,:), diffusion(:,:,nz,:), tstep, &
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, ny, nz, nz)
#else
       call reaction_rate_bounds(rr_r_zL, temp(:,:,nz), &
             pressure(:,:,nz), yspecies(:,:,nz,:),&
             g_ref, rho_ref, a_ref, l_ref, t_o, 1, nx, 1, ny, nz, nz)
#endif
      end if
    end if
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
  if(vary_in_x==1 ) & 
       call nscbc_x( q, h_spec_x0, h_spec_xL, rr_r_x0, rr_r_xL, &
                     grad_Ys_x0, grad_Ys_xL, grad_u_x0, grad_u_xL, rhs )
  if(vary_in_y==1 ) & 
       call nscbc_y( q, h_spec_y0, h_spec_yL, rr_r_y0, rr_r_yL, &
                     grad_Ys_y0, grad_Ys_yL, grad_u_y0, grad_u_yL, rhs )
  if(vary_in_z==1 ) &
       call nscbc_z( q, h_spec_z0, h_spec_zL, rr_r_z0, rr_r_zL, &
                     grad_Ys_z0, grad_Ys_zL, grad_u_z0, grad_u_zL, rhs )

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

!  wall bc according to james - not implemented
!  Evatt Hawkes DEC 2004
!  call wallBCCleanup( rhs, pressure, tau )

!---------------------------- FINISHED ----------------------------

!  call MPI_Barrier( gcomm, ierr )

  return
end subroutine rhsf


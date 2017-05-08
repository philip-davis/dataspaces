#include "globalDefines.h"
! Changes
! Chunsang Yoo (02/10/06)
!
! Nonreflecting inflow boundary conditions: Specifying Li values by the sum of 
! solution variable damping terms like pressure daminping term in nonreflecting 
! outflow boundary conditions and transverse derivative terms 
!
! Add v,w in subroutine update_L_x and transverse terms to calculate L_i values
! for nonreflecting inflow boundary conditions.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-------------------------------------------------------------------------------
!                     Author:  James Sutherland
!                     Date:    May, 2002
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! The routines in this file impose boundary conditions for OPEN BOUNDARIES
! by setting characteristic wave amplitudes.  If you would like to add different
! open BC's, this is the place to treat the wave amplitudes.  These different
! BC's should trigger off of the "nrf" flag from the main input file "s3d.in"
!
! For example, this routine currently treats the NON-REFLECTING BC's by
! eliminating all incoming waves and modifying some outgoing waves.
!
! If you wanted to implement another open BC (eg "soft inflow"), do it here.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Any open boundaries that are added to the code should be added here as
! appropriate.  Be sure that you do not overspecify the boundary conditions.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!                        | Type of boundary condition|
!   |--------------------|---------------------------|
!   | Flow Situation     |   Euler   | Navier-Stokes |
!   |====================|===========|===============|
!   | Supersonic Inflow  |  5+(N-1)  |   5+(N-1)     |  N is the number
!   | Supersonic Outflow |    0      |   4+(N-1)     |  of species.
!   |                    |           |               |
!   | Subsonic Inflow    |  4+(N-1)  |   5+(N-1)     |
!   | Subsonic Outflow   |    1      |   4+(N-1)     |
!   --------------------------------------------------
!__________________________________________________________________________
! SUBSONIC INFLOW:
!   We must specify 1+(N-1) purely Navier-Stokes boundary conditions.  This
!   is done on the flux terms in "bc_flux".  The single remaining Euler
!   boundary condition is specified below.
!__________________________________________________________________________
! SUBSONIC OUTFLOW:
!   We must specify 3+(N-1) Navier-Stokes boundary conditions.  This is done
!   on the flux terms (Tau, Heat Flux, Diffusion Flux) in "bc_flux".  The
!   remaining 3+(N-1) Euler boundary conditions are specified here.
!__________________________________________________________________________
! SUPERSONIC INFLOW:  (not currently implemented)
!   We must specify 5+(N-1) Euler BC's and NO Navier-Stokes BC's.  Thus,
!   if/when this condition is implemented, ALL 5+(N-1) BC's should be
!   imposed here!
!__________________________________________________________________________
! SUPERSONIC OUTFLOW: (not currently implemented)
!   We must specify 4+(N-1) purely Navier-Stokes BC's.  Thus, for supersonic
!   outflow, nothing should be changed in this routine.
!-------------------------------------------------------------------------------
!
!
! NON-REFLECTING BOUNDARY CONDITIONS:
!       To impose non-reflecting bc's, we must kill some wave amplitudes.
!       This is done by setting L's to specific values.  See Chris' document
!       for more details!    
!-------------------------------------------------------------------------------



subroutine update_L_x( u, v, w, temp, a, gamma, p, rho, Yi, mixMW, specMW, h_spec,  &
                       rxn_rate, xpos, L )
  !-----------------------------------------------------------------------------
  ! Modifiy L's (wave amplitudes) for various boundary conditions.
  !
  ! INPUT:
  !     u        (ny,nz) X-velocity
  !     v        (ny,nz) Y-velocity
  !     w        (ny,nz) Z-velocity
  !     temp     (ny,nz) temperature
  !     a        (ny,nz) speed of sound
  !     gamma    (ny,nz) ratio of heat capacities
  !     p        (ny,nz) pressure
  !     rho      (ny,nz) density
  !     Yi       (ny,nz,n_spec) species mass fraction
  !     mixMW    (ny,nz) mixture molecular weight
  !     specMW   (n_spec) species molecular weights
  !     rxn_rate (ny,nz,n_spec) mass production rate of each species
  !     xpos     integer specifying which X boundary we are at
  !-----------------------------------------------------------------------------
  use param_m, only : nx,ny,nz,n_spec,nvar_tot
  use topology_m
  use bc_m, only : relax_ct, pout, nrf_x0, nrf_xl, qx_bc
  use grid_m, only : xmin, xmax
  use reference_m, only : t_ref, cp_ref, univ_gascon, a_ref
#ifdef FEEDTURB
  use feedturb_m, only : vel_bar
#endif

  implicit none


  real, intent(in), dimension(ny,nz) :: u, v, w, temp, a, gamma, p
  real, intent(in), dimension(ny,nz) :: rho, mixMW
  real, intent(in), dimension(n_spec) :: specMW
  real, intent(in), dimension(ny,nz,n_spec) :: h_spec, rxn_rate

  integer, intent(in) :: xpos
  real, intent(inout), dimension(ny,nz,nvar_tot+1) :: L
  real, dimension(ny,nz,nvar_tot+1) :: L_trans

  real, dimension(ny,nz) :: S_p
  real, dimension(ny,nz,n_spec) :: S_Yi, Yi
  real, dimension(ny,nz) :: dp_dy, dp_dz, dv_dy, dv_dz, &
                            du_dy, du_dz, dw_dy, dw_dz, drho_dy, drho_dz, &
                            dYi_dy, dYi_dz
  real :: sum_SYi

  real, dimension(ny,nz) :: ubnd

  real :: my_Ma_max, Ma_max, damp
  integer :: j,k,n

  ! set source term for the pressure equation.  This term contributes to L1, L2, L5
  S_p = 0.0
  do n=1,n_spec
     S_p = S_p + rxn_rate(:,:,n) * (h_spec(:,:,n)*(1.0-gamma) + gamma*p*mixMW/(rho*specMW(n)))
     S_Yi(:,:,n) = rxn_rate(:,:,n) / rho
  enddo

  ! S_Yi = 0.0    ! set this to zero for now.  This is only used on a non-reflecting inflow, and
                  ! it seems to cause all sorts of stability problems.

  if (xpos == 1) then
   !===============================
   ! X=1 Boundary (Left Boundary)
   !===============================

     BC_0: select case( nrf_x0 )

        ! add other open BC's here...

     case(-1)
        !---------------------------------------------------
        ! nonreflecting inflow conditions (by Chunsang)
        !---------------------------------------------------

      ! calculate the Maximum Mach number on this face.

        my_Ma_max = maxval(abs(u/a))
        call MPI_Allreduce(my_Ma_max, Ma_max, 1, MPI_REAL8, MPI_MAX, yz_comm, ierr)

      ! Calculate spatial derivatives of solution variables for transverse terms
        call deriv_inplane_1(u(:,:),du_dy(:,:),1,ny,nz,1)
        call deriv_inplane_1(v(:,:),dv_dy(:,:),1,ny,nz,1)
        call deriv_inplane_1(w(:,:),dw_dy(:,:),1,ny,nz,1)
        call deriv_inplane_1(p(:,:),dp_dy(:,:),1,ny,nz,1)
        call deriv_inplane_1(rho(:,:),drho_dy(:,:),1,ny,nz,1)

        call deriv_inplane_2(u(:,:),du_dz(:,:),1,ny,nz,1)
        call deriv_inplane_2(v(:,:),dv_dz(:,:),1,ny,nz,1)
        call deriv_inplane_2(w(:,:),dw_dz(:,:),1,ny,nz,1)
        call deriv_inplane_2(p(:,:),dp_dz(:,:),1,ny,nz,1)
        call deriv_inplane_2(rho(:,:),drho_dz(:,:),1,ny,nz,1)

      ! Calculate transverse terms for Li values
        L_trans(:,:,2)=-v(:,:)*(drho_dy(:,:) - dp_dy(:,:)/a(:,:)**2) &
                       -w(:,:)*(drho_dz(:,:) - dp_dz(:,:)/a(:,:)**2)
        L_trans(:,:,3)=-(v(:,:)*dv_dy(:,:) + w(:,:)*dv_dz(:,:)    &
                                             + dp_dy(:,:)/rho(:,:))
        L_trans(:,:,4)=-(v(:,:)*dw_dy(:,:) + w(:,:)*dw_dz(:,:)    &
                                             + dp_dz(:,:)/rho(:,:))
        L_trans(:,:,5) = -0.5*((v*dp_dy(:,:)+w*dp_dz(:,:))     &
                         + rho*a*a*(dv_dy(:,:)+dw_dz(:,:))     &
                         + rho*a*(v*du_dy(:,:)+w*du_dz(:,:)))
  
        do n=1,n_spec
          call deriv_inplane_1(Yi(:,:,n),dYi_dy(:,:),1,ny,nz,1)
          call deriv_inplane_2(Yi(:,:,n),dYi_dz(:,:),1,ny,nz,1)
          L_trans(:,:,5+n)= -(v*dYi_dy(:,:)+w*dYi_dz(:,:))
        enddo

      ! Specifying Li values as damping terms no matter what value u velocity has 
      ! L1 value doesn't change

        damp = 30.0
#ifndef USE_HISTORIC_BC
        L(:,:,2) = -damp*(univ_gascon/cp_ref/mixMW) &
                   /abs(xmax-xmin)*rho/a*(temp-qx_bc(1,:,:,5))
        L(:,:,3) = damp*a/abs(xmax-xmin)*(v-qx_bc(1,:,:,2))
        L(:,:,4) = damp*a/abs(xmax-xmin)*(w-qx_bc(1,:,:,3))
        L(:,:,5) = damp*a*rho*a*(1.0-Ma_max**2.0)/abs(xmax-xmin)/2.0 &
                 * (u-qx_bc(1,:,:,1))

        do n=1,n_spec
           L(:,:,5+n)= damp*a/abs(xmax-xmin)*(Yi(:,:,n)-qx_bc(1,:,:,5+n))
        enddo
#else

        L(:,:,2) = -50.0*(univ_gascon/cp_ref/mixMW) &
                   /abs(xmax-xmin)*rho/a*(temp-qx_bc(1,:,:,5))
        L(:,:,3) = 50.0*a/abs(xmax-xmin)*(v-qx_bc(1,:,:,2))
        L(:,:,4) = 50.0*a/abs(xmax-xmin)*(w-qx_bc(1,:,:,3))
        L(:,:,5) = 50.0*a*rho*a*(1.0-Ma_max**2.0)/abs(xmax-xmin)/2.0 &
                 * (u-qx_bc(1,:,:,1))

        do n=1,n_spec
           L(:,:,5+n)= 50.0*a/abs(xmax-xmin)*(Yi(:,:,n)-qx_bc(1,:,:,5+n))
        enddo
#endif

      ! Add transverse effect to Li values
        L(:,:,2)=L(:,:,2)+L_trans(:,:,2)-S_p/a**2
        L(:,:,3)=L(:,:,3)+L_trans(:,:,3)
        L(:,:,4)=L(:,:,4)+L_trans(:,:,4)
        L(:,:,5)=L(:,:,5)+L_trans(:,:,5)+0.5*S_p
        do n=1,n_spec
           L(:,:,5+n)=L(:,:,5+n)+L_trans(:,:,5+n)+S_Yi(:,:,n)
        enddo
                 
     case(1)
        !------------------------------
        !     non-reflecting BC's
        !------------------------------

      ! calculate the Maximum Mach number on this face.

        my_Ma_max = maxval(abs(u/a))
        call MPI_Allreduce(my_Ma_max, Ma_max, 1, MPI_REAL8, MPI_MAX, yz_comm, ierr)

      ! modify L's as required

        damp = 0.5*relax_ct * (1.0-Ma_max*Ma_max) / abs(xmax-xmin)

      ! L5 is modified whether flow is going in or out...
        L(:,:,5) = damp*a*(p-pout) + 0.5*S_p

      ! modify L's if flow is coming in. Otherwise, L's are unchanged (except L5)
        do k=1,nz
           do j=1,ny
              if (u(j,k) > 0.0) then
                 ! L1 is unchanged
                 L(j,k,2) = -S_p(j,k) / (a(j,k)*a(j,k))
                 L(j,k,3) = 0.0
                 L(j,k,4) = 0.0
                 L(j,k,5) = 0.5*S_p(j,k)
                 do n=1,n_spec
                    L(j,k,5+n) = S_Yi(j,k,n)    ! rxn_rate(j,k,n)/rho(j,k)
                 enddo
              endif
           enddo
        enddo

     end select BC_0

  elseif( xpos == nx ) then

   !===============================
   ! X=Lx Boundary (Right Boundary)
   !===============================

     BC_L: select case( nrf_xl )

     ! add other open BC's here...

     case(1)
        !------------------------------
        !     non-reflecting BC's
        !------------------------------

      ! calculate the Maximum Mach number on this face.

        my_Ma_max = maxval(abs(u/a))
        call MPI_Allreduce(my_Ma_max, Ma_max, 1, MPI_REAL8, MPI_MAX, yz_comm, ierr)

      ! modify L's as required

        damp = 0.5*relax_ct * (1.0-Ma_max*Ma_max) / abs(xmax-xmin)

      ! L1 is modified whether flow is going in or out...
        L(:,:,1) = damp*a*(p-pout) + 0.5*S_p

      ! Calculate spatial derivatives of solution variables for transverse terms
        call deriv_inplane_1(u(:,:),du_dy(:,:),1,ny,nz,1)
        call deriv_inplane_1(v(:,:),dv_dy(:,:),1,ny,nz,1)
        call deriv_inplane_1(p(:,:),dp_dy(:,:),1,ny,nz,1)

        call deriv_inplane_2(u(:,:),du_dz(:,:),1,ny,nz,1)
        call deriv_inplane_2(w(:,:),dw_dz(:,:),1,ny,nz,1)
        call deriv_inplane_2(p(:,:),dp_dz(:,:),1,ny,nz,1)

      ! Calculate transverse terms for Li values
        L_trans(:,:,1) = - 0.5*((v*dp_dy(:,:)+w*dp_dz(:,:))    &
                         + rho*a*a*(dv_dy(:,:)+dw_dz(:,:))     &
                         - rho*a*(v*du_dy(:,:)+w*du_dz(:,:)))
  
      ! If abs(u)/a equals zero the transverse correction becomes unstable,
      ! the limit of 0.99 is introduced to prevent this, 13 April 2009.
        L(:,:,1)=L(:,:,1)+min(0.99,(1.0-abs(u)/a))*L_trans(:,:,1)
 
      ! modify L's if flow is coming in. Otherwise, L's are unchanged (except L1)
        do k=1,nz
           do j=1,ny
              if (u(j,k) < 0.0) then
                 L(j,k,1) = 0.5*S_p(j,k)
                 L(j,k,2) = -S_p(j,k)/(a(j,k)*a(j,k))
                 L(j,k,3) = 0.0
                 L(j,k,4) = 0.0
                 ! L5 is unchanged
                 do n=1,n_spec
                    L(j,k,5+n) = S_Yi(j,k,n)    ! rxn_rate(j,k,n) / rho(j,k)
                 enddo
              endif
           enddo
        enddo


      ! Fix for outflow boundary (avoids inflow from outlet plane)
      ! this is triggered when the x-velocity becomes less than ubnd. The velocity
      ! target is then set to the inlet plane velocity.
! gbansal - vv BUG FIX on suggestion of Evatt Hawkes 02/17/2011
       damp = 50.0
       if (nrf_x0 .le. 0) then
        ubnd = 0.1*qx_bc(2,:,:,1)
       else
        ubnd = 0.0
       endif


        do k=1,nz
           do j=1,ny
             if (u(j,k) < ubnd(j,k)) then
                 L(j,k,1) = 0.5*S_p(j,k) + L_trans(j,k,1)  &
                     - damp*a(j,k)*rho(j,k)*a(j,k)*(1.0-Ma_max**2.0)/abs(xmax-xmin)/2.0 &
                     * (u(j,k)-ubnd(j,k))
             endif
           enddo
         enddo

     end select BC_L

  endif

end subroutine update_L_x


!===============================================================================
#ifndef USE_HISTORIC_BC
subroutine update_L_y( u, v, w, temp, a, gamma, p, rho, Yi, mixMW, specMW, h_spec, &
                       rxn_rate, ypos, L )
#else
subroutine update_L_y( u, v, w, temp, a, gamma, p, rho, mixMW, specMW, h_spec, &
                       rxn_rate, ypos, L )
#endif
!
! Modifiy L's (wave amplitudes) for various boundary conditions.
!
  use param_m, only : nx,ny,nz,n_spec,nvar_tot
  use topology_m
  use bc_m, only : relax_ct, pout, nrf_y0, nrf_yl, qy_bc
  use grid_m, only : ymin, ymax
  use reference_m, only : cp_ref, univ_gascon, a_ref

  implicit none

  real, intent(in), dimension(nx,nz) :: u, v, w, temp, a, gamma, p
  real, intent(in), dimension(nx,nz) :: rho, mixMW
  real, intent(in), dimension(n_spec) :: specMW
  real, intent(in), dimension(nx,nz,n_spec) :: h_spec, rxn_rate
  integer, intent(in) :: ypos
  real, intent(inout), dimension(nx,nz,nvar_tot+1) :: L

  real, dimension(nx,nz) :: S_p
  real, dimension(nx,nz,n_spec) :: S_Yi, Yi
  real, dimension(nx,nz) :: dp_dx, dp_dz, du_dx, du_dz, dv_dx, dv_dz, dw_dx, dw_dz, &
                            drho_dx, drho_dz, dyi_dx, dyi_dz

  real, dimension(nx,nz) :: vbnd
  real, dimension(nx,nz,nvar_tot+1) :: L_trans
  real :: sum_SYi
  real :: my_Ma_max, Ma_max, damp
  integer :: i,k,n

  ! set source term for the pressure equation.  This term contributes to L1, L2, L5
  S_p = 0.0
  do n=1,n_spec
     S_p = S_p + rxn_rate(:,:,n) * (h_spec(:,:,n)*(1.0-gamma) + gamma*p*mixMW/(rho*specMW(n)))
     !S_Yi(:,:,n) = rxn_rate(:,:,n) / rho
  enddo
  S_Yi = 0.0    ! set this to zero for now.  This is only used on a non-reflecting inflow, and
                ! it seems to cause all sorts of stability problems.

  if (ypos == 1) then
   !===============================
   ! Y=1 Boundary (Left Boundary)
   !===============================
     BC_0: select case( nrf_y0 )

        ! add other open BC's here...

     case(-1)
        !---------------------------------------------------
        ! nonreflecting inflow conditions (by Chunsang Yoo)
        !---------------------------------------------------

      ! calculate the Maximum Mach number on this face.

        my_Ma_max = maxval(abs(v/a))
        call MPI_Allreduce(my_Ma_max, Ma_max, 1, MPI_REAL8, MPI_MAX, xz_comm, ierr)

      ! Calculate spatial derivatives of solution variables for transverse terms
        call deriv_inplane_1(u(:,:),    du_dx(:,:),2,nx,nz,1)
        call deriv_inplane_1(v(:,:),    dv_dx(:,:),2,nx,nz,1)
        call deriv_inplane_1(w(:,:),    dw_dx(:,:),2,nx,nz,1)
        call deriv_inplane_1(p(:,:),    dp_dx(:,:),2,nx,nz,1)
        call deriv_inplane_1(rho(:,:),drho_dx(:,:),2,nx,nz,1)

        call deriv_inplane_2(u(:,:),    du_dz(:,:),2,nx,nz,1)
        call deriv_inplane_2(v(:,:),    dv_dz(:,:),2,nx,nz,1)
        call deriv_inplane_2(w(:,:),    dw_dz(:,:),2,nx,nz,1)
        call deriv_inplane_2(p(:,:),    dp_dz(:,:),2,nx,nz,1)
        call deriv_inplane_2(rho(:,:),drho_dz(:,:),2,nx,nz,1)

      ! Calculate transverse terms for Li values
        L_trans(:,:,2)=-u(:,:)*(drho_dx(:,:) - dp_dx(:,:)/a(:,:)**2) &
                       -w(:,:)*(drho_dz(:,:) - dp_dz(:,:)/a(:,:)**2)
        L_trans(:,:,3)=-(u(:,:)*du_dx(:,:) + w(:,:)*du_dz(:,:)    &
                                             + dp_dx(:,:)/rho(:,:))
        L_trans(:,:,4)=-(u(:,:)*dw_dx(:,:) + w(:,:)*dw_dz(:,:)    &
                                             + dp_dz(:,:)/rho(:,:))
        L_trans(:,:,5) = -0.5*((u*dp_dx(:,:)+w*dp_dz(:,:))     &
                         + rho*a*a*(du_dx(:,:)+dw_dz(:,:))     &
                         + rho*a*(u*dv_dx(:,:)+w*dv_dz(:,:)))
  
        do n=1,n_spec
          call deriv_inplane_1(Yi(:,:,n),dYi_dx(:,:),2,nx,nz,1)
          call deriv_inplane_2(Yi(:,:,n),dYi_dz(:,:),2,nx,nz,1)
          L_trans(:,:,5+n)= -(u*dYi_dx(:,:)+w*dYi_dz(:,:))
        enddo

      ! Specifying Li values as damping terms no matter what value u velocity has 
      ! L1 value doesn't change
        
        damp = 10.0

        L(:,:,2) = -damp*(univ_gascon/cp_ref/mixMW) &
                   /abs(ymax-ymin)*rho/a*(temp-qy_bc(:,1,:,5))
        L(:,:,3) = damp*a/abs(ymax-ymin)*(u-qy_bc(:,1,:,1))
        L(:,:,4) = damp*a/abs(ymax-ymin)*(w-qy_bc(:,1,:,3))
        L(:,:,5) = damp*a*rho*a*(1.0-Ma_max**2.0)/abs(ymax-ymin)/2.0 &
                 * (v-qy_bc(:,1,:,2))

        do n=1,n_spec
           L(:,:,5+n)= damp*a/abs(ymax-ymin)*(Yi(:,:,n)-qy_bc(:,1,:,5+n))
        enddo

      ! Add transverse effect to Li values
        L(:,:,2)=L(:,:,2)+L_trans(:,:,2)-S_p/a**2
        L(:,:,3)=L(:,:,3)+L_trans(:,:,3)
        L(:,:,4)=L(:,:,4)+L_trans(:,:,4)
        L(:,:,5)=L(:,:,5)+L_trans(:,:,5)+0.5*S_p
        do n=1,n_spec
           L(:,:,5+n)=L(:,:,5+n)+L_trans(:,:,5+n)+S_Yi(:,:,n)
        enddo
 
     case(1)
        !------------------------------
        !     non-reflecting BC's
        !------------------------------

      ! calculate the Maximum Mach number on this face.

        my_Ma_max = maxval(abs(v/a))
        call MPI_Allreduce(my_Ma_max, Ma_max, 1, MPI_REAL8, MPI_MAX, xz_comm, ierr)

      ! modify L's as required

        damp = 0.5*relax_ct * (1.0-Ma_max*Ma_max) / abs(ymax-ymin)

      ! L5 is modified whether flow is going in or out...
        L(:,:,5) = damp*a*(p-pout) + 0.5*S_p

      ! Calculate spatial derivatives of solution variables for transverse terms
        call deriv_inplane_1(u(:,:),du_dx(:,:),2,nx,nz,1)
        call deriv_inplane_1(v(:,:),dv_dx(:,:),2,nx,nz,1)
        call deriv_inplane_1(p(:,:),dp_dx(:,:),2,nx,nz,1)

        call deriv_inplane_2(v(:,:),dv_dz(:,:),2,nx,nz,1)
        call deriv_inplane_2(w(:,:),dw_dz(:,:),2,nx,nz,1)
        call deriv_inplane_2(p(:,:),dp_dz(:,:),2,nx,nz,1)

      ! Calculate transverse terms for Li values
        L_trans(:,:,5) = - 0.5*((u*dp_dx(:,:)+w*dp_dz(:,:))    &
                         + rho*a*a*(du_dx(:,:)+dw_dz(:,:))     &
                         + rho*a*(u*dv_dx(:,:)+w*dv_dz(:,:)))
       
      ! If abs(v)/a equals zero the transverse correction becomes unstable,
      ! the limit of 0.99 is introduced to prevent this, 13 April 2009.
        L(:,:,5)=L(:,:,5)+min(0.99,(1.0-abs(v)/a))*L_trans(:,:,5)

      ! modify L's if flow is coming in. Otherwise, L's are unchanged (except L5)
        do k=1,nz
           do i=1,nx
              if (v(i,k) > 0.0) then
                 ! L1 is unchanged
                 L(i,k,2) = -S_p(i,k) / (a(i,k)*a(i,k))
                 L(i,k,3) = 0.0
                 L(i,k,4) = 0.0
                 L(i,k,5) = 0.5*S_p(i,k)
                 do n=1,n_spec
                    L(i,k,5+n) = 0.0
                 enddo
              endif
           enddo
        enddo

     end select BC_0

  elseif( ypos == ny ) then

   !===============================
   ! Y=Ly Boundary (Right Boundary)
   !===============================

     BC_L: select case( nrf_yl )

     case(1)
        !------------------------------
        !     non-reflecting BC's
        !------------------------------

      ! calculate the Maximum Mach number on this face.

        my_Ma_max = maxval(abs(v/a))
        call MPI_Allreduce(my_Ma_max, Ma_max, 1, MPI_REAL8, MPI_MAX, xz_comm, ierr)

      ! modify L's as required

        damp = 0.5*relax_ct * (1.0-Ma_max*Ma_max) / abs(ymax-ymin)

      ! L1 is modified whether flow is going in or out...
        L(:,:,1) = damp*a*(p-pout) + 0.5*S_p

      ! Calculate spatial derivatives of solution variables for transverse terms
        call deriv_inplane_1(u(:,:),du_dx(:,:),2,nx,nz,1)
        call deriv_inplane_1(v(:,:),dv_dx(:,:),2,nx,nz,1)
        call deriv_inplane_1(p(:,:),dp_dx(:,:),2,nx,nz,1)

        call deriv_inplane_2(v(:,:),dv_dz(:,:),2,nx,nz,1)
        call deriv_inplane_2(w(:,:),dw_dz(:,:),2,nx,nz,1)
        call deriv_inplane_2(p(:,:),dp_dz(:,:),2,nx,nz,1)

      ! Calculate transverse terms for Li values
        L_trans(:,:,1) = - 0.5*((u*dp_dx(:,:)+w*dp_dz(:,:))    &
                         + rho*a*a*(du_dx(:,:)+dw_dz(:,:))     &
                         - rho*a*(u*dv_dx(:,:)+w*dv_dz(:,:)))
 
      ! If abs(v)/a equals zero the transverse correction becomes unstable,
      ! the limit of 0.99 is introduced to prevent this, 13 April 2009.
        L(:,:,1)=L(:,:,1)+min(0.99,(1.0-abs(v)/a))*L_trans(:,:,1)

      ! modify L's if flow is coming in. Otherwise, L's are unchanged (except L1)
        do k=1,nz
           do i=1,nx
              if (v(i,k) < 0.0) then
                 L(i,k,1) = 0.5*S_p(i,k)
                 L(i,k,2) = -S_p(i,k)/(a(i,k)*a(i,k))   !BUG!!!  This was incorrect before 2-18-03
!!!the bug:                 L(i,k,2) = 0.0
                 L(i,k,3) = 0.0
                 L(i,k,4) = 0.0
               ! L5 is unchanged
                 do n=1,n_spec
                    L(i,k,5+n) = 0.0
                 enddo
              endif
           enddo
        enddo

      ! Fix for outflow boundary (avoids inflow from outlet plane)
#ifdef FIXYOUTFLOW
        damp = 50.0
        vbnd = 5.0/a_ref

        do k=1,nz
           do i=1,nx
             if (v(i,k) < vbnd(i,k)) then
                L(i,k,1) = 0.5*S_p(i,k) + L_trans(i,k,1)  &
                     - damp*a(i,k)*rho(i,k)*a(i,k)*(1.0-Ma_max**2.0)/abs(ymax-ymin)/2.0 &
                     * (v(i,k)-vbnd(i,k))
             endif
          enddo
       enddo
#endif
     end select BC_L

  endif

end subroutine update_L_y


!===============================================================================

#ifndef USE_HISTORIC_BC
subroutine update_L_z( u, v, w, temp, a, gamma, p, rho, Yi, mixMW, specMW, h_spec, &
                       rxn_rate, zpos, L )
#else
subroutine update_L_z( w, a, gamma, p, rho, mixMW, specMW, h_spec, rxn_rate, zpos, L )
#endif
!
! Modifiy L's (wave amplitudes) for various boundary conditions.
!
  use param_m, only : nx,ny,nz,n_spec,nvar_tot
  use topology_m
  use bc_m, only : relax_ct, pout, nrf_z0, nrf_zl
  use grid_m, only : zmin, zmax
  use reference_m

  implicit none
#ifndef USE_HISTORIC_BC
  real, intent(in), dimension(nx,ny) :: u, v, temp
#else
  real, dimension(nx,ny) :: u, v, temp
#endif
  real, intent(in), dimension(nx,ny) :: w, a, gamma, p
  real, intent(in), dimension(nx,ny) :: rho, mixMW
  real, intent(in), dimension(n_spec) :: specMW
  real, intent(in), dimension(nx,ny,n_spec) :: h_spec, rxn_rate
  integer, intent(in) :: zpos
  real, intent(inout), dimension(nx,ny,nvar_tot+1) :: L

  real, dimension(nx,ny) :: S_p
  real, dimension(nx,ny,n_spec) :: S_Yi, Yi
  real, dimension(nx,ny,nvar_tot+1) :: L_trans
  real, dimension(nx,ny) :: dp_dx, dp_dy, du_dx, du_dy, dv_dx, dv_dy, dw_dx, dw_dy, &
                            drho_dx, drho_dy, dyi_dx, dyi_dy
  real :: sum_SYi
  real :: my_Ma_max, Ma_max, damp
  integer :: i,j,n

  ! set source term for the pressure equation.  This term contributes to L1, L2, L5
  S_p = 0.0
  do n=1,n_spec
     S_p = S_p + rxn_rate(:,:,n) * (h_spec(:,:,n)*(1.0-gamma) + gamma*p*mixMW/(rho*specMW(n)))
     !S_Yi(:,:,n) = rxn_rate(:,:,n) / rho
  enddo
  S_Yi = 0.0    ! set this to zero for now.  This is only used on a non-reflecting inflow, and
                ! it seems to cause all sorts of stability problems.

  if (zpos == 1) then
   !===============================
   ! Z=1 Boundary (Left Boundary)
   !===============================
     BC_0: select case( nrf_z0 )

     case(1)
        !------------------------------
        !     non-reflecting BC's
        !------------------------------

      ! calculate the Maximum Mach number on this face.

        my_Ma_max = maxval(abs(w/a))
        call MPI_Allreduce(my_Ma_max, Ma_max, 1, MPI_REAL8, MPI_MAX, xy_comm, ierr)

      ! modify L's as required

        damp = 0.5*relax_ct * (1.0-Ma_max*Ma_max) / abs(zmax-zmin)

      ! L5 is modified whether flow is going in or out...
        L(:,:,5) = damp*a*(p-pout) + 0.5*S_p
#ifndef USE_HISTORIC_BC

      ! Calculate spatial derivatives of solution variables for transverse terms
        call deriv_inplane_1(u(:,:),du_dx(:,:),3,nx,ny,1)
        call deriv_inplane_1(w(:,:),dw_dx(:,:),3,nx,ny,1)
        call deriv_inplane_1(p(:,:),dp_dx(:,:),3,nx,ny,1)

        call deriv_inplane_2(v(:,:),dv_dy(:,:),3,nx,ny,1)
        call deriv_inplane_2(w(:,:),dw_dy(:,:),3,nx,ny,1)
        call deriv_inplane_2(p(:,:),dp_dy(:,:),3,nx,ny,1)

      ! Calculate transverse terms for Li values
        L_trans(:,:,5) = - 0.5*((u*dp_dx(:,:)+v*dp_dy(:,:))    &
                         + rho*a*a*(du_dx(:,:)+dv_dy(:,:))     &
                         + rho*a*(u*dw_dx(:,:)+v*dw_dy(:,:)))
 
      ! If abs(w)/a equals zero the transverse correction becomes unstable,
      ! the limit of 0.99 is introduced to prevent this, 13 April 2009.
        L(:,:,5)=L(:,:,5)+min(0.99,(1.0-abs(w)/a))*L_trans(:,:,5)

#endif

      ! modify L's if flow is coming in. Otherwise, L's are unchanged (except L5)
        do j=1,ny
           do i=1,nx
              if (w(i,j) > 0.0) then
                 ! L1 is unchanged
                 L(i,j,2) = -S_p(i,j) / (a(i,j)*a(i,j))
                 L(i,j,3) = 0.0
                 L(i,j,4) = 0.0
#ifndef USE_HISTORIC_BC
                 L(i,j,5) = 50.0*a(i,j)*rho(i,j)*a(i,j)*(1.0-Ma_max**2.0)/abs(zmax-zmin)/2.0 &
                          * (w(i,j)+50.0/a_ref) + 0.5*S_p(i,j) + L_trans(i,j,5)
#else
                 L(i,j,5) = 0.5*S_p(i,j)
#endif
                 do n=1,n_spec
                    L(i,j,5+n) = 0.0
                 enddo
              endif
           enddo
        enddo

     end select BC_0

  elseif( zpos == nz ) then

   !===============================
   ! Z=Lz Boundary (Right Boundary)
   !===============================

     BC_L: select case( nrf_zl )

     case(1)
        !------------------------------
        !     non-reflecting BC's
        !------------------------------

      ! calculate the Maximum Mach number on this face.

        my_Ma_max = maxval(abs(w/a))
        call MPI_Allreduce(my_Ma_max, Ma_max, 1, MPI_REAL8, MPI_MAX, xy_comm, ierr)

      ! modify L's as required

        damp = 0.5*relax_ct * (1.0-Ma_max*Ma_max) / abs(zmax-zmin)

      ! L1 is modified whether flow is going in or out...
        L(:,:,1) = damp*a*(p-pout) + 0.5*S_p
#ifndef USE_HISTORIC_BC
      ! Calculate spatial derivatives of solution variables for transverse terms
        call deriv_inplane_1(u(:,:),du_dx(:,:),3,nx,ny,1)
        call deriv_inplane_1(w(:,:),dw_dx(:,:),3,nx,ny,1)
        call deriv_inplane_1(p(:,:),dp_dx(:,:),3,nx,ny,1)

        call deriv_inplane_2(v(:,:),dv_dy(:,:),3,nx,ny,1)
        call deriv_inplane_2(w(:,:),dw_dy(:,:),3,nx,ny,1)
        call deriv_inplane_2(p(:,:),dp_dy(:,:),3,nx,ny,1)

      ! Calculate transverse terms for Li values
        L_trans(:,:,1) = - 0.5*((u*dp_dx(:,:)+v*dp_dy(:,:))    &
                         + rho*a*a*(du_dx(:,:)+dv_dy(:,:))     &
                         - rho*a*(u*dw_dx(:,:)+v*dw_dy(:,:)))
 
      ! If abs(w)/a equals zero the transverse correction becomes unstable,
      ! the limit of 0.99 is introduced to prevent this, 13 April 2009.
        L(:,:,1)=L(:,:,1)+min(0.99,(1.0-abs(w)/a))*L_trans(:,:,1)

#endif
      ! modify L's if flow is coming in. Otherwise, L's are unchanged (except L1)
        do j=1,ny
           do i=1,nx
              if (w(i,j) < 0.0) then
               ! L5 is unchanged
#ifndef USE_HISTORIC_BC
                 L(i,j,1) = -50.0*a(i,j)*rho(i,j)*a(i,j)*(1.0-Ma_max**2.0)/abs(zmax-zmin)/2.0 &
                          * (w(i,j)-50.0/a_ref) + 0.5*S_p(i,j) + L_trans(i,j,1)
#else
                 L(i,j,1) = 0.5*S_p(i,j)
#endif
                 L(i,j,2) = -S_p(i,j)/(a(i,j)*a(i,j))
                 L(i,j,3) = 0.0
                 L(i,j,4) = 0.0
                 do n=1,n_spec
                    L(i,j,5+n) = 0.0
                 enddo
              endif
           enddo
        enddo

     end select BC_L

  endif

end subroutine update_L_z

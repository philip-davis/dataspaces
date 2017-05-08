#include "globalDefines.h"
! Changes - 
! Ramanan Sankaran 01/05/05
! The whole grad_u and grad_Ys matrix used to be passed in
! I eliminated the separate storage for tau and diffFlux
! and had them stored in the place of the grad's. 
! The boundary gradients alone are remembered in a smaller array.
! So the arguments of the routines in this file were changed
! to receive the smaller size arrays. The new arrays being passed in 
! have only the required data - nothing more.
!*****************************************************************
!
! 6-24-03  James changed
!    if (xid==0 .and. periodic_x==0 .and. nrf_x0/=0) then
! to
!    if (xid==0 .and. periodic_x==0 .and. abs(nrf_x0)==1) then
! for x, y, and z directions...
!
! also implemented "soft inflow" bcs in x-direction
!
! BUG FIX 14-JAN-2005 Evatt Hawkes
! previous formulation fix not correctly implemented
! bug found by Ramanan and Andrea     
!*****************************************************************


!-----------------------------------------------------------------------------
!                         Author: James Sutherland
!                         Date:   May, 2002
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! This routine imposes Navier Stokes Characteristic Boundary conditions at all
! open boundaries (except hard inflow) in the boundary-normal direction.  It
! uses the method of characteristics to eliminate waves in each primitive
! variable as they propagate through a boundary.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! INPUT:
!     q       - conserved variables        (nx,ny,nz,nvar_tot)
!     grad_Ys - Mass Frac Gradient         (nx,ny,nz,n_spec,3)
!     grad_u  - velocity gradient tensor   (nx,ny,nz,3,3)
!     rhs     - Right Hand Side of eqns.   (nx,ny,nz,nvar_tot)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! OUTPUT:
!     rhs - Modified Right Hand Side of eqns.   (nx,ny,nz,nvar_tot)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! The basic approach to implement the NSCBC's is the following:
!
!  1. Remove some stuff from the RHS of the equations at the boundaries.
!
!  2. Compute correction terms for each equation at the boundary.
!
!  3. Update the RHS for each equation on the boundary using the correction
!     terms calculated in (2).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Equation ordering:   |  Conserved Variables:  |  Primitive Variables:  |
!     (1) x-momentum   |      rho*u             |     u                  |
!     (2) y-momentum   |      rho*v             |     v                  |
!     (3) z-momentum   |      rho*w             |     w                  |
!     (4) continuity   |      rho               |     rho                |
!     (5) energy       |      rho*Et            |     Et                 |
!  (6...) Species      |      rho*Yi            |     Yi                 |
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! At each boundary, we will require the derivative of each primitive variable
! in the boundary-normal direction:
!
!  du/dn   - x-velocity gradient normal to boundary               [passed]
!  dv/dn   - y-velocity gradient normal to boundary               [passed]
!  dw/dn   - z-velocity gradient normal to boundary               [passed]
!  drho/dn - density gradient normal to boundary                  [computed]
!  dp/dn   - pressure gradient normal to the boundary             [computed]
!  dYi/dn  - species mass fraction gradient normal to boundary    [passed]
!
! We assume that the velocity gradient tensor and the species mass fraction
! gradients are available.  Then we must only compute pressure gradients and
! density gradients normal to each boundary.  This will be done using
! point-wise derivative operators at the boundaries.  Thus, all of this is
! local computation and should require no processor communication.
!-----------------------------------------------------------------------------


!!$==================================================================================
!!$==================================================================================
!!$==================================================================================


! Add more terms for diffusion terms. Chun Sang Yoo 05/06
! Ramanan Sankaran - 01/05/05 - Replaced old grad's with the new arguments
!subroutine nscbc_x( q, h_spec_x0, h_spec_xL, rr_r_x0, rr_r_xL,   & 
!         grad_Ys_x0, grad_Ys_xL, grad_u_x0, grad_u_xL, rhs )
subroutine nscbc_x( q, h_spec_x0, h_spec_xL, rr_r_x0, rr_r_xL,   &
                    grad_Ys_x0, grad_Ys_xL, grad_u_x0, grad_u_xL, rhs, &
                    grad_u_x0_all, grad_u_xL_all, tau_x0, tau_xL, &
                    div_heatFlux_x0, div_heatFlux_xL, div_tau_x_x0, div_tau_x_xL, &
                    div_tau_y_x0, div_tau_y_xL, div_tau_z_x0, div_tau_z_xL, &
                    div_diffFlux_x0, div_diffFlux_xL )
  !-----------------------------------------------------
  ! X-DIRECTION IMPLEMENTATION OF BOUNDARY CONDITIONS
  !-----------------------------------------------------
  use param_m, only : nx,ny,nz, nvar_tot, n_spec, periodic_x, vary_in_y, vary_in_z, nsc
  use grid_m, only : scale_1x
  use topology_m

  use bc_m

  use thermchem_m, only : gamma, cpmix, avmolwt, mixMW
  use reference_m, only : univ_gascon, a_ref, t_ref
  use chemkin_m, only : molwt, molwt_c
  use runtime_m, only : time_accum
!#ifdef FROZENFEED  !I don't think FROZENFEED is defined anywhere
  use frozenfeed_m, only : frozenfeed_u
!#endif
  use temporalfeed_m, only: temporalfeed_u, temporalfeed_u_all, i_temporalfeed, i_frozenfeed, i_temporalspecies
  use turbulence_m, only : i_turbulence

  use variables_m, only : yspecies, u, volum, pressure, temp

  implicit none

  real, intent(in),    dimension(nx,ny,nz,nvar_tot) :: q
  real, intent(in),    dimension(ny,nz,n_spec)   :: h_spec_x0, h_spec_xL
  real, intent(in),    dimension(ny,nz,n_spec) :: rr_r_x0, rr_r_xL
  real, intent(in),    dimension(ny,nz,n_spec) :: grad_Ys_x0, grad_Ys_xL
  real, intent(in),    dimension(ny,nz,3) :: grad_u_x0, grad_u_xL
  real, intent(inout), dimension(nx,ny,nz,nvar_tot) :: rhs

  real, intent(in),    dimension(ny,nz,3,3) :: grad_u_x0_all, grad_u_xL_all
  real, intent(in),    dimension(ny,nz,3,3) :: tau_x0, tau_xL
  real, intent(in),    dimension(ny,nz) :: div_heatFlux_x0, div_tau_x_x0, div_tau_y_x0, div_tau_z_x0
  real, intent(in),    dimension(ny,nz) :: div_heatFlux_xL, div_tau_x_xL, div_tau_y_xL, div_tau_z_xL
  real, intent(in),    dimension(ny,nz,n_spec) :: div_diffFlux_x0, div_diffFlux_xL

! these quantities will only be calculated on the X-Boundary faces (YZ planes)
  real, dimension(ny,nz) :: dp_dx, drho_dx, a, sumterm, R, tmp
  real, dimension(ny,nz,nvar_tot+1) :: Lx, d_x, V_x

  integer :: n


!******************************************************************************
!-------------------------  Left BOUNDARY TREATMENT ---------------------------
!******************************************************************************

!#ifdef FROZENFEED  ! I don't think FROZENFEED is defined anywhere. 
  if (nrf_x0==-1 .and. i_turbulence==1) then
      if(i_temporalfeed.eq.2)then  
        if(i_temporalspecies.eq.1)then
          call temporalfeed_u_all(time_accum, qx_bc(1,:,:,1:3), qx_bc(1,:,:,6:5+nsc), qx_bc(1,:,:,5) ) !use variable 5 for temperature
        else    
          call temporalfeed_u(time_accum, qx_bc(1,:,:,1:3))
        endif  
      elseif(i_frozenfeed.eq.2)then
        call frozenfeed_u(time_accum, qx_bc(1,:,:,1:3))
      endif
  endif
!#endif

! only processors on left boundary do this.
  if (xid==0 .and. periodic_x==0 .and. abs(nrf_x0)==1) then

   !===========================================================================
   ! STEP 1:  Subtract boundary information from current RHS for each equation
   !          and discard it.  It will later be replaced by the approporiate
   !          information for the NSCBCS.
   !===========================================================================

   ! compute derivatives that we still need (not passed in).
     call point_der1_x( pressure,   dp_dx,   1, scale_1x ) ! compute dp/dx   at left X-Boundary
     call point_der1_x( q(:,:,:,4), drho_dx, 1, scale_1x ) ! compute drho/dx at left X-Boundary
     
   ! set the sound speed
     R = univ_gascon * (1.0/a_ref**2)*t_ref*avmolwt(1,:,:) ! non-dimensional gas constant
     a = sqrt(gamma(1,:,:)*R*temp(1,:,:))

   !----------------------------------------------------------------------------
   ! Now construct the Lx's and d's

     call compute_L_x( u(1,:,:,1), a, q(1,:,:,4), grad_u_x0(:,:,1),    &
                       grad_u_x0(:,:,2), grad_u_x0(:,:,3), dp_dx,     &
                       grad_Ys_x0, drho_dx, Lx )

     call compute_d_x( Lx, a, q(1,:,:,4), d_x )

   !----------------------------------------------------------------------------
   ! now that we have the d's, remove the appropriate terms from the RHS for each eqn

     !---- momentum ----
     rhs(1,:,:,1) = rhs(1,:,:,1) + (u(1,:,:,1)*d_x(:,:,1) + q(1,:,:,4)*d_x(:,:,3))
     if(vary_in_y==1) rhs(1,:,:,2) = rhs(1,:,:,2) + (u(1,:,:,2)*d_x(:,:,1) + q(1,:,:,4)*d_x(:,:,4))
     if(vary_in_z==1) rhs(1,:,:,3) = rhs(1,:,:,3) + (u(1,:,:,3)*d_x(:,:,1) + q(1,:,:,4)*d_x(:,:,5))

     !---- continuity ----
     rhs(1,:,:,4) = rhs(1,:,:,4) + d_x(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_x(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(1,:,:)*temp(1,:,:)*mixMW(1,:,:)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_x0(:,:,n) - h_spec_x0(:,:,n_spec))*d_x(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(1,:,:,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(1,:,:,5) = rhs(1,:,:,5) +                                                      &
          ( (q(1,:,:,5)*volum(1,:,:) + (R - cpmix(1,:,:))*temp(1,:,:))*d_x(:,:,1)       &
          + d_x(:,:,2)/(gamma(1,:,:)-1.0) + sumterm )
     rhs(1,:,:,5) = rhs(1,:,:,5) + q(1,:,:,1)*d_x(:,:,3)
     if(vary_in_y==1) rhs(1,:,:,5) = rhs(1,:,:,5) + q(1,:,:,2)*d_x(:,:,4)
     if(vary_in_z==1) rhs(1,:,:,5) = rhs(1,:,:,5) + q(1,:,:,3)*d_x(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(1,:,:,5+n) = rhs(1,:,:,5+n) + (yspecies(1,:,:,n)*d_x(:,:,1)           &
                                           + q(1,:,:,4)*d_x(:,:,5+n))
     enddo

   !===============================================================================
   ! STEP 2:  Modify the Lx's as needed to impose Nonreflecting BC's.
   !          Then compute the new d's from the modified Lx's.
   !===============================================================================

     call update_L_x(u(1,:,:,1), u(1,:,:,2), u(1,:,:,3), temp(1,:,:), a,  &
            gamma(1,:,:), pressure(1,:,:), q(1,:,:,4), yspecies(1,:,:,:), &
            mixMW(1,:,:),  molwt, h_spec_x0(:,:,:), rr_r_x0, 1, Lx)

   ! Change by Chun Sang Yoo (See Yoo et al., CTM, vol 9, 2005)
   ! Add V_x or diffusive terms to Lx (S_x or source terms are added in update_L_x)

     if (nrf_x0 == -1) then     ! nonreflecting inflow conditions

       V_x(:,:,5) = (gamma(1,:,:)-1.0) *                         &
                     tau_x0(:,:,1,1)*grad_u_x0_all(:,:,1,1)

       if (vary_in_y == 1) V_x(:,:,5) = V_x(:,:,5) + (gamma(1,:,:)-1.0) *       &
                                      ( tau_x0(:,:,2,1)*grad_u_x0_all(:,:,2,1)  &
                                      + tau_x0(:,:,1,2)*grad_u_x0_all(:,:,1,2)  &
                                      + tau_x0(:,:,2,2)*grad_u_x0_all(:,:,2,2))


       if (vary_in_z == 1) V_x(:,:,5) = V_x(:,:,5) + (gamma(1,:,:)-1.0) *       &
                                      ( tau_x0(:,:,3,1)*grad_u_x0_all(:,:,3,1)  &
                                      + tau_x0(:,:,1,3)*grad_u_x0_all(:,:,1,3)  &
                                      + tau_x0(:,:,3,2)*grad_u_x0_all(:,:,3,2)  &
                                      + tau_x0(:,:,2,3)*grad_u_x0_all(:,:,2,3)  &
                                      + tau_x0(:,:,3,3)*grad_u_x0_all(:,:,3,3))

       V_x(:,:,5) = V_x(:,:,5) - (gamma(1,:,:)-1.0)*div_heatFlux_x0(:,:)

       do n=1,n_spec
         V_x(:,:,5) = V_x(:,:,5) + (gamma(1,:,:)-1.0)*(h_spec_x0(:,:,n)        &
                    - cpmix(1,:,:)*temp(1,:,:)*mixMW(1,:,:)/molwt(n))*div_diffFlux_x0(:,:,n)
         V_x(:,:,5+n) = div_diffFlux_x0(:,:,n)/q(1,:,:,4)
       enddo

       V_x(:,:,2) = -V_x(:,:,5)/a(:,:)**2

       V_x(:,:,5) = V_x(:,:,5) + a(:,:)*div_tau_x_x0(:,:)
       V_x(:,:,3) = div_tau_y_x0(:,:)/q(1,:,:,4)
       V_x(:,:,4) = div_tau_z_x0(:,:)/q(1,:,:,4)

       Lx(:,:,2)=Lx(:,:,2) + V_x(:,:,2)
       Lx(:,:,3)=Lx(:,:,3) + V_x(:,:,3)
       Lx(:,:,4)=Lx(:,:,4) + V_x(:,:,4)
       Lx(:,:,5)=Lx(:,:,5) + 0.5*V_x(:,:,5)
       do n=1,n_spec
         Lx(:,:,5+n) = Lx(:,:,5+n) + V_x(:,:,5+n)
       enddo

     elseif (nrf_x0 == 1) then     ! nonreflecting outflow boundary conditions

       V_x(:,:,5) = (gamma(1,:,:)-1.0) *                         &
                     tau_x0(:,:,1,1)*grad_u_x0_all(:,:,1,1)

       if (vary_in_y == 1) V_x(:,:,5) = V_x(:,:,5) + (gamma(1,:,:)-1.0) *       &
                                      ( tau_x0(:,:,2,1)*grad_u_x0_all(:,:,2,1)  &
                                      + tau_x0(:,:,1,2)*grad_u_x0_all(:,:,1,2)  &
                                      + tau_x0(:,:,2,2)*grad_u_x0_all(:,:,2,2))


       if (vary_in_z == 1) V_x(:,:,5) = V_x(:,:,5) + (gamma(1,:,:)-1.0) *       &
                                      ( tau_x0(:,:,3,1)*grad_u_x0_all(:,:,3,1)  &
                                      + tau_x0(:,:,1,3)*grad_u_x0_all(:,:,1,3)  &
                                      + tau_x0(:,:,3,2)*grad_u_x0_all(:,:,3,2)  &
                                      + tau_x0(:,:,2,3)*grad_u_x0_all(:,:,2,3)  &
                                      + tau_x0(:,:,3,3)*grad_u_x0_all(:,:,3,3))

       V_x(:,:,5) = V_x(:,:,5) - (gamma(1,:,:)-1.0)*div_heatFlux_x0(:,:)

       do n=1,n_spec
         V_x(:,:,5) = V_x(:,:,5) + (gamma(1,:,:)-1.0)*(h_spec_x0(:,:,n)        &
                    - cpmix(1,:,:)*temp(1,:,:)*mixMW(1,:,:)/molwt(n))*div_diffFlux_x0(:,:,n)
       enddo

       V_x(:,:,5) = V_x(:,:,5) + a(:,:)*div_tau_x_x0(:,:)
       Lx(:,:,5)=Lx(:,:,5) + 0.5*V_x(:,:,5)

     endif

     call compute_d_x( Lx, a, q(1,:,:,4), d_x )

   !===============================================================================
   ! STEP 3:  Put the corrected term back into each RHS.
   !===============================================================================

     !---- momentum ----
     rhs(1,:,:,1) = rhs(1,:,:,1) - (u(1,:,:,1)*d_x(:,:,1) + q(1,:,:,4)*d_x(:,:,3))
     if(vary_in_y==1) rhs(1,:,:,2) = rhs(1,:,:,2) - (u(1,:,:,2)*d_x(:,:,1) + q(1,:,:,4)*d_x(:,:,4))
     if(vary_in_z==1) rhs(1,:,:,3) = rhs(1,:,:,3) - (u(1,:,:,3)*d_x(:,:,1) + q(1,:,:,4)*d_x(:,:,5))

     !---- continuity ----
     rhs(1,:,:,4) = rhs(1,:,:,4) - d_x(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_x(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(1,:,:)*temp(1,:,:)*mixMW(1,:,:)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_x0(:,:,n) - h_spec_x0(:,:,n_spec))*d_x(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(1,:,:,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(1,:,:,5) = rhs(1,:,:,5) -                                                      &
          ( (q(1,:,:,5)*volum(1,:,:) + (R - cpmix(1,:,:))*temp(1,:,:))*d_x(:,:,1)       &
          + d_x(:,:,2)/(gamma(1,:,:)-1.0) + sumterm )
   ! kinetic energy
     rhs(1,:,:,5) = rhs(1,:,:,5) - q(1,:,:,1)*d_x(:,:,3)
     if(vary_in_y==1) rhs(1,:,:,5) = rhs(1,:,:,5) - q(1,:,:,2)*d_x(:,:,4)
     if(vary_in_z==1) rhs(1,:,:,5) = rhs(1,:,:,5) - q(1,:,:,3)*d_x(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(1,:,:,5+n) = rhs(1,:,:,5+n) - ( yspecies(1,:,:,n)*d_x(:,:,1)          &
                                            + q(1,:,:,4)*d_x(:,:,5+n) )
     enddo
  endif    ! only processors on left boundary do this.


!******************************************************************************
!------------------------- RIGHT BOUNDARY TREATMENT ---------------------------
!******************************************************************************

! only processors on the right boundary do this
  if(xid==xpes-1 .and. periodic_x==0 .and. abs(nrf_xl)==1) then

   !===========================================================================
   ! STEP 1:  Subtract boundary information from current RHS for each equation
   !          and discard it.  It will later be replaced by the approporiate
   !          information for the NSCBCS.
   !===========================================================================

   ! compute derivatives that we still need (not passed in).
     call point_der1_x( pressure, dp_dx, nx, scale_1x )
     call point_der1_x( q(:,:,:,4), drho_dx, nx, scale_1x )

   ! set the sound speed
     R = univ_gascon * (1.0/a_ref**2)*t_ref*avmolwt(nx,:,:)  ! non-dimensional gas constant
     a = sqrt(gamma(nx,:,:)*R*temp(nx,:,:))

   !----------------------------------------------------------------------------
   ! Now construct the Lx's and d's

     call compute_L_x( u(nx,:,:,1), a, q(nx,:,:,4), grad_u_xL(:,:,1),         &
                       grad_u_xL(:,:,2), grad_u_xL(:,:,3), dp_dx,           &
                       grad_Ys_xL, drho_dx, Lx )

     call compute_d_x( Lx, a, q(nx,:,:,4), d_x )

   !----------------------------------------------------------------------------
   ! now that we have the d's, remove the appropriate terms from the RHS for each eqn

     !---- momentum ----
     rhs(nx,:,:,1) = rhs(nx,:,:,1) + (u(nx,:,:,1)*d_x(:,:,1) + q(nx,:,:,4)*d_x(:,:,3))
     if(vary_in_y==1) rhs(nx,:,:,2) = rhs(nx,:,:,2) + (u(nx,:,:,2)*d_x(:,:,1) + q(nx,:,:,4)*d_x(:,:,4))
     if(vary_in_z==1) rhs(nx,:,:,3) = rhs(nx,:,:,3) + (u(nx,:,:,3)*d_x(:,:,1) + q(nx,:,:,4)*d_x(:,:,5))

     !---- continuity ----
     rhs(nx,:,:,4) = rhs(nx,:,:,4) + d_x(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_x(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(nx,:,:)*temp(nx,:,:)*mixMW(nx,:,:)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_xL(:,:,n) - h_spec_xL(:,:,n_spec))*d_x(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(nx,:,:,4)
     ! END BUG FIX 14-JAN-2005     

     rhs(nx,:,:,5) = rhs(nx,:,:,5) +                                                    &
          ( (q(nx,:,:,5)*volum(nx,:,:) + (R - cpmix(nx,:,:))*temp(nx,:,:))*d_x(:,:,1)   &
          + d_x(:,:,2)/(gamma(nx,:,:)-1.0) + sumterm )
   ! kinetic energy
     rhs(nx,:,:,5) = rhs(nx,:,:,5) + q(nx,:,:,1)*d_x(:,:,3)
     if(vary_in_y==1) rhs(nx,:,:,5) = rhs(nx,:,:,5) + q(nx,:,:,2)*d_x(:,:,4)
     if(vary_in_y==1) rhs(nx,:,:,5) = rhs(nx,:,:,5) + q(nx,:,:,3)*d_x(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(nx,:,:,5+n) = rhs(nx,:,:,5+n) + ( yspecies(nx,:,:,n)*d_x(:,:,1)       &
                                          + q(nx,:,:,4)*d_x(:,:,5+n) )
     enddo

   !============================================================================
   ! STEP 2:  Modify the Lx's as needed to impose Nonreflecting BC's.
   !          Then compute the new d's from the modified Lx's.
   !============================================================================

!     call update_L_x( u(nx,:,:,1), a, gamma(nx,:,:), pressure(nx,:,:),          &
!                      q(nx,:,:,4), mixMW(nx,:,:), molwt, h_spec_xL(:,:,:),      &
!                      rr_r_xL, nx, Lx )
     call update_L_x(u(nx,:,:,1), u(nx,:,:,2), u(nx,:,:,3), temp(nx,:,:), a,  &
            gamma(nx,:,:), pressure(nx,:,:), q(nx,:,:,4), yspecies(nx,:,:,:), &
            mixMW(nx,:,:),  molwt, h_spec_xL(:,:,:), rr_r_xL, nx, Lx)

   ! Change by Chun Sang Yoo (See Yoo et al., CTM, vol 9, 2005)
   ! Add V_x or diffusive terms to Lx (S_x or source terms are added in update_L_x)

     if (nrf_xl == -1) then   ! nonreflecting inflow conditions

       V_x(:,:,1) = (gamma(nx,:,:)-1.0) *                        &
                     tau_xL(:,:,1,1)*grad_u_xL_all(:,:,1,1)

       if (vary_in_y == 1) V_x(:,:,1) = V_x(:,:,1) + (gamma(nx,:,:)-1.0) *      &
                                      ( tau_xL(:,:,2,1)*grad_u_xL_all(:,:,2,1)  &
                                      + tau_xL(:,:,1,2)*grad_u_xL_all(:,:,1,2)  &
                                      + tau_xL(:,:,2,2)*grad_u_xL_all(:,:,2,2))

       if (vary_in_z == 1) V_x(:,:,1) = V_x(:,:,1) + (gamma(nx,:,:)-1.0) *      &
                                      ( tau_xL(:,:,3,1)*grad_u_xL_all(:,:,3,1)  &
                                      + tau_xL(:,:,1,3)*grad_u_xL_all(:,:,1,3)  &
                                      + tau_xL(:,:,3,2)*grad_u_xL_all(:,:,3,2)  &
                                      + tau_xL(:,:,2,3)*grad_u_xL_all(:,:,2,3)  &
                                      + tau_xL(:,:,3,3)*grad_u_xL_all(:,:,3,3))

       V_x(:,:,1) = V_x(:,:,1) - (gamma(nx,:,:)-1.0)*div_heatFlux_xL(:,:)

       do n=1,n_spec
         V_x(:,:,1) = V_x(:,:,1) + (gamma(nx,:,:)-1.0)*(h_spec_xL(:,:,n)        &
                    - cpmix(nx,:,:)*temp(nx,:,:)*mixMW(nx,:,:)/molwt(n))*div_diffFlux_xL(:,:,n)
         V_x(:,:,5+n) = div_diffFlux_xL(:,:,n)/q(nx,:,:,4)
       enddo

       V_x(:,:,2) = -V_x(:,:,1)/a(:,:)**2

       V_x(:,:,1) = V_x(:,:,1) - a(:,:)*div_tau_x_xL(:,:)
       V_x(:,:,3) = div_tau_y_xL(:,:)/q(nx,:,:,4)
       V_x(:,:,4) = div_tau_z_xL(:,:)/q(nx,:,:,4)

       Lx(:,:,1)=Lx(:,:,1) + 0.5*V_x(:,:,1)
       Lx(:,:,2)=Lx(:,:,2) + V_x(:,:,2)
       Lx(:,:,3)=Lx(:,:,3) + V_x(:,:,3)
       Lx(:,:,4)=Lx(:,:,4) + V_x(:,:,4)
       do n=1,n_spec
         Lx(:,:,5+n) = Lx(:,:,5+n) + V_x(:,:,5+n)
       enddo

     elseif (nrf_xl == 1) then     ! nonreflecting outflow boundary conditions

       V_x(:,:,1) = (gamma(nx,:,:)-1.0) *                        &
                     tau_xL(:,:,1,1)*grad_u_xL_all(:,:,1,1)

       if (vary_in_y == 1) V_x(:,:,1) = V_x(:,:,1) + (gamma(nx,:,:)-1.0) *      &
                                      ( tau_xL(:,:,2,1)*grad_u_xL_all(:,:,2,1)  &
                                      + tau_xL(:,:,1,2)*grad_u_xL_all(:,:,1,2)  &
                                      + tau_xL(:,:,2,2)*grad_u_xL_all(:,:,2,2))

       if (vary_in_z == 1) V_x(:,:,1) = V_x(:,:,1) + (gamma(nx,:,:)-1.0) *      &
                                      ( tau_xL(:,:,3,1)*grad_u_xL_all(:,:,3,1)  &
                                      + tau_xL(:,:,1,3)*grad_u_xL_all(:,:,1,3)  &
                                      + tau_xL(:,:,3,2)*grad_u_xL_all(:,:,3,2)  &
                                      + tau_xL(:,:,2,3)*grad_u_xL_all(:,:,2,3)  &
                                      + tau_xL(:,:,3,3)*grad_u_xL_all(:,:,3,3))

       V_x(:,:,1) = V_x(:,:,1) - (gamma(nx,:,:)-1.0)*div_heatFlux_xL(:,:)

       do n=1,n_spec
         V_x(:,:,1) = V_x(:,:,1) + (gamma(nx,:,:)-1.0)*(h_spec_xL(:,:,n)        &
                    - cpmix(nx,:,:)*temp(nx,:,:)*mixMW(nx,:,:)/molwt(n))*div_diffFlux_xL(:,:,n)
       enddo
   
       V_x(:,:,1) = V_x(:,:,1) - a(:,:)*div_tau_x_xL(:,:)
       Lx(:,:,1)=Lx(:,:,1) + 0.5*V_x(:,:,1)
     
     endif

     call compute_d_x( Lx, a, q(nx,:,:,4), d_x )

   !============================================================================
   ! STEP 3:  Put the correction term back into each RHS.
   !============================================================================

     !---- momentum ----
     rhs(nx,:,:,1) = rhs(nx,:,:,1) - (u(nx,:,:,1)*d_x(:,:,1) + q(nx,:,:,4)*d_x(:,:,3))
     if(vary_in_y==1) rhs(nx,:,:,2) = rhs(nx,:,:,2) - (u(nx,:,:,2)*d_x(:,:,1) + q(nx,:,:,4)*d_x(:,:,4))
     if(vary_in_z==1) rhs(nx,:,:,3) = rhs(nx,:,:,3) - (u(nx,:,:,3)*d_x(:,:,1) + q(nx,:,:,4)*d_x(:,:,5))

     !---- continuity ----
     rhs(nx,:,:,4) = rhs(nx,:,:,4) - d_x(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_x(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(nx,:,:)*temp(nx,:,:)*mixMW(nx,:,:)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_xL(:,:,n) - h_spec_xL(:,:,n_spec))*d_x(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(nx,:,:,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(nx,:,:,5) = rhs(nx,:,:,5) -                                                    &
          ( (q(nx,:,:,5)*volum(nx,:,:) + (R - cpmix(nx,:,:))*temp(nx,:,:))*d_x(:,:,1)   &
          + d_x(:,:,2)/(gamma(nx,:,:)-1.0) + sumterm )
   ! kinetic energy
     rhs(nx,:,:,5) = rhs(nx,:,:,5) - q(nx,:,:,1)*d_x(:,:,3)
     if(vary_in_y==1) rhs(nx,:,:,5) = rhs(nx,:,:,5) - q(nx,:,:,2)*d_x(:,:,4)
     if(vary_in_z==1) rhs(nx,:,:,5) = rhs(nx,:,:,5) - q(nx,:,:,3)*d_x(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(nx,:,:,5+n) = rhs(nx,:,:,5+n) - ( yspecies(nx,:,:,n)*d_x(:,:,1)       &
                                          + q(nx,:,:,4)*d_x(:,:,5+n) )
     enddo
  endif          ! only processors on the right boundary do this

!------------------------------- DONE -------------------------------

  return
end subroutine nscbc_x




!!$==================================================================================
!!$==================================================================================
!!$==================================================================================



! Add more terms for diffusion terms. Chun Sang Yoo 05/06
! Ramanan Sankaran - 01/05/05 - Replaced old grad's with the new arguments
#ifndef USE_HISTORIC_BC
subroutine nscbc_y( q, h_spec_y0, h_spec_yL, rr_r_y0, rr_r_yL,&
                    grad_Ys_y0, grad_Ys_yL, grad_u_y0, grad_u_yL, rhs, &
                    grad_u_y0_all, grad_u_yL_all, tau_y0, tau_yL, &
                    div_heatFlux_y0, div_heatFlux_yL, div_tau_x_y0, div_tau_x_yL, &
                    div_tau_y_y0, div_tau_y_yL, div_tau_z_y0, div_tau_z_yL, &
                    div_diffFlux_y0, div_diffFlux_yL )
#else
subroutine nscbc_y( q, h_spec_y0, h_spec_yL, rr_r_y0, rr_r_yL,&
             grad_Ys_y0, grad_Ys_yL, grad_u_y0, grad_u_yL, rhs )
#endif
  !-----------------------------------------------------
  ! Y-DIRECTION IMPLEMENTATION OF BOUNDARY CONDITIONS
  !-----------------------------------------------------
  use param_m, only : nx,ny,nz, nvar_tot, n_spec, periodic_y, vary_in_x, vary_in_z
  use grid_m, only : scale_1y
  use topology_m

  use bc_m

  use thermchem_m, only : gamma, cpmix, avmolwt, mixMW
  use reference_m, only : univ_gascon, a_ref, t_ref     ! used to compute sound speed
  use chemkin_m, only : molwt, molwt_c

  use variables_m, only : yspecies, u, volum, pressure, temp

  implicit none

  real, intent(in),    dimension(nx,ny,nz,nvar_tot) :: q
  real, intent(in),    dimension(nx,nz,n_spec)   :: h_spec_y0, h_spec_yL
  real, intent(in),    dimension(nx,nz,n_spec) :: rr_r_y0, rr_r_yL
  real, intent(in),    dimension(nx,nz,n_spec) :: grad_Ys_y0, grad_Ys_yL
  real, intent(in),    dimension(nx,nz,3)      :: grad_u_y0, grad_u_yL
  real, intent(inout), dimension(nx,ny,nz,nvar_tot) :: rhs

#ifndef USE_HISTORIC_BC
  real, intent(in),    dimension(nx,nz,3,3) :: grad_u_y0_all, grad_u_yL_all
  real, intent(in),    dimension(nx,nz,3,3) :: tau_y0, tau_yL
  real, intent(in),    dimension(nx,nz) :: div_heatFlux_y0, div_tau_x_y0, div_tau_y_y0, div_tau_z_y0
  real, intent(in),    dimension(nx,nz) :: div_heatFlux_yL, div_tau_x_yL, div_tau_y_yL, div_tau_z_yL
  real, intent(in),    dimension(nx,nz,n_spec) :: div_diffFlux_y0, div_diffFlux_yL
#endif

! these quantities will only be calculated on the Y-Boundary faces (XZ planes)
  real, dimension(nx,nz) :: dp_dy, drho_dy, a, sumterm, R, tmp
  real, dimension(nx,nz,nvar_tot+1) :: Ly, d_y, V_y

  integer :: n


!******************************************************************************
!-------------------------  Left BOUNDARY TREATMENT ---------------------------
!******************************************************************************

! only processors on left boundary do this.
  if (yid==0 .and. periodic_y==0 .and. abs(nrf_y0)==1) then

   !===========================================================================
   ! STEP 1:  Subtract boundary information from current RHS for each equation
   !          and discard it.  It will later be replaced by the approporiate
   !          information for the NSCBCS.
   !===========================================================================

   ! compute derivatives that we still need (not passed in).
     call point_der1_y( pressure,   dp_dy,   1, scale_1y ) ! compute dp/dy   at left Y-Boundary
     call point_der1_y( q(:,:,:,4), drho_dy, 1, scale_1y ) ! compute drho/dy at left Y-Boundary

   ! set the sound speed
     R = univ_gascon * (1.0/a_ref**2)*t_ref*avmolwt(:,1,:) ! non-dimensional gas constant
     a = sqrt(gamma(:,1,:)*R*temp(:,1,:))

   !----------------------------------------------------------------------------
   ! Now construct the Ly's and d's

     call compute_L_y( u(:,1,:,2), a, q(:,1,:,4), grad_u_y0(:,:,1),    &
                       grad_u_y0(:,:,2), grad_u_y0(:,:,3), dp_dy,     &
                       grad_Ys_y0, drho_dy, Ly )

     call compute_d_y( Ly, a, q(:,1,:,4), d_y )

   !----------------------------------------------------------------------------
   ! now that we have the d's, remove the appropriate terms from the RHS for each eqn

     !---- momentum ----
     if(vary_in_x==1) rhs(:,1,:,1) = rhs(:,1,:,1) + (u(:,1,:,1)*d_y(:,:,1) + q(:,1,:,4)*d_y(:,:,3))
     rhs(:,1,:,2) = rhs(:,1,:,2) + (u(:,1,:,2)*d_y(:,:,1) + q(:,1,:,4)*d_y(:,:,4))
     if(vary_in_z==1) rhs(:,1,:,3) = rhs(:,1,:,3) + (u(:,1,:,3)*d_y(:,:,1) + q(:,1,:,4)*d_y(:,:,5))

     !---- continuity ----
     rhs(:,1,:,4) = rhs(:,1,:,4) + d_y(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_y(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(:,1,:)*temp(:,1,:)*mixMW(:,1,:)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_y0(:,:,n) - h_spec_y0(:,:,n_spec))*d_y(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(:,1,:,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(:,1,:,5) = rhs(:,1,:,5) +                                                      &
          ( (q(:,1,:,5)*volum(:,1,:) + (R - cpmix(:,1,:))*temp(:,1,:))*d_y(:,:,1)       &
          + d_y(:,:,2)/(gamma(:,1,:)-1.0) + sumterm )
     ! kinetic energy
     if(vary_in_x==1) rhs(:,1,:,5) = rhs(:,1,:,5) + q(:,1,:,1)*d_y(:,:,3)
     rhs(:,1,:,5) = rhs(:,1,:,5) + q(:,1,:,2)*d_y(:,:,4)
     if(vary_in_z==1) rhs(:,1,:,5) = rhs(:,1,:,5) + q(:,1,:,3)*d_y(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(:,1,:,5+n) = rhs(:,1,:,5+n) + (yspecies(:,1,:,n)*d_y(:,:,1)           &
                                           + q(:,1,:,4)*d_y(:,:,5+n))
     enddo

   !===============================================================================
   ! STEP 2:  Modify the Ly's as needed to impose Nonreflecting BC's.
   !          Then compute the new d's from the modified Ly's.
   !===============================================================================

#ifndef USE_HISTORIC_BC
     call update_L_y( u(:,1,:,1), u(:,1,:,2), u(:,1,:,3), temp(:,1,:),  &
                      a, gamma(:,1,:), pressure(:,1,:),q(:,1,:,4), yspecies(:,1,:,:), &
                      mixMW(:,1,:), molwt, h_spec_y0(:,:,:),rr_r_y0, 1, Ly )
#else
     call update_L_y( u(:,1,:,1), u(:,1,:,2), u(:,1,:,3), temp(:,1,:),  &
                      a, gamma(:,1,:), pressure(:,1,:),q(:,1,:,4), mixMW(:,1,:), &
                      molwt, h_spec_y0(:,:,:),rr_r_y0, 1, Ly )
#endif
   ! Change by Chun Sang Yoo (See Yoo et al., CTM, vol 9, 2005)
   ! Add V_y or diffusive terms to Ly (S_y or source terms are added in update_L_y)

#ifndef USE_HISTORIC_BC

     if (nrf_y0 == -1) then     ! nonreflecting inflow conditions

       V_y(:,:,5) = (gamma(:,1,:)-1.0) *                     &
                     tau_y0(:,:,2,2)*grad_u_y0_all(:,:,2,2)

       if (vary_in_x == 1) V_y(:,:,5) = V_y(:,:,5) + (gamma(:,1,:)-1.0) *       &
                                      ( tau_y0(:,:,1,1)*grad_u_y0_all(:,:,1,1)  &
                                      + tau_y0(:,:,1,2)*grad_u_y0_all(:,:,1,2)  &
                                      + tau_y0(:,:,2,1)*grad_u_y0_all(:,:,2,1))

       if (vary_in_z == 1) V_y(:,:,5) = V_y(:,:,5) + (gamma(:,1,:)-1.0) *       &
                                      ( tau_y0(:,:,3,1)*grad_u_y0_all(:,:,3,1)  &
                                      + tau_y0(:,:,1,3)*grad_u_y0_all(:,:,1,3)  &
                                      + tau_y0(:,:,3,2)*grad_u_y0_all(:,:,3,2)  &
                                      + tau_y0(:,:,2,3)*grad_u_y0_all(:,:,2,3)  &
                                      + tau_y0(:,:,3,3)*grad_u_y0_all(:,:,3,3))

       V_y(:,:,5) = V_y(:,:,5) - (gamma(:,1,:)-1.0)*div_heatFlux_y0(:,:)

       do n=1,n_spec
         V_y(:,:,5) = V_y(:,:,5) + (gamma(:,1,:)-1.0)*(h_spec_y0(:,:,n)        &
                    - cpmix(:,1,:)*temp(:,1,:)*mixMW(:,1,:)/molwt(n))*div_diffFlux_y0(:,:,n)
         V_y(:,:,5+n) = div_diffFlux_y0(:,:,n)/q(:,1,:,4)
       enddo

       V_y(:,:,2) = -V_y(:,:,5)/a(:,:)**2

       V_y(:,:,5) = V_y(:,:,5) + a(:,:)*div_tau_y_y0(:,:)
       V_y(:,:,3) = div_tau_x_y0(:,:)/q(:,1,:,4)
       V_y(:,:,4) = div_tau_z_y0(:,:)/q(:,1,:,4)

       Ly(:,:,2)=Ly(:,:,2) + V_y(:,:,2)
       Ly(:,:,3)=Ly(:,:,3) + V_y(:,:,3)
       Ly(:,:,4)=Ly(:,:,4) + V_y(:,:,4)
       Ly(:,:,5)=Ly(:,:,5) + 0.5*V_y(:,:,5)
       do n=1,n_spec
         Ly(:,:,5+n) = Ly(:,:,5+n) + V_y(:,:,5+n)
       enddo

     elseif (nrf_y0 == 1) then     ! nonreflecting outflow boundary conditions

       V_y(:,:,5) = (gamma(:,1,:)-1.0) *                     &
                    tau_y0(:,:,2,2)*grad_u_y0_all(:,:,2,2)

       if (vary_in_x == 1) V_y(:,:,5) = V_y(:,:,5) + (gamma(:,1,:)-1.0) *       &
                                      ( tau_y0(:,:,1,1)*grad_u_y0_all(:,:,1,1)  &
                                      + tau_y0(:,:,1,2)*grad_u_y0_all(:,:,1,2)  &
                                      + tau_y0(:,:,2,1)*grad_u_y0_all(:,:,2,1))

       if (vary_in_z == 1) V_y(:,:,5) = V_y(:,:,5) + (gamma(:,1,:)-1.0) *       &
                                      ( tau_y0(:,:,3,1)*grad_u_y0_all(:,:,3,1)  &
                                      + tau_y0(:,:,1,3)*grad_u_y0_all(:,:,1,3)  &
                                      + tau_y0(:,:,3,2)*grad_u_y0_all(:,:,3,2)  &
                                      + tau_y0(:,:,2,3)*grad_u_y0_all(:,:,2,3)  &
                                      + tau_y0(:,:,3,3)*grad_u_y0_all(:,:,3,3))

       V_y(:,:,5) = V_y(:,:,5) - (gamma(:,1,:)-1.0)*div_heatFlux_y0(:,:)

       do n=1,n_spec
         V_y(:,:,5) = V_y(:,:,5) + (gamma(:,1,:)-1.0)*(h_spec_y0(:,:,n)        &
                    - cpmix(:,1,:)*temp(:,1,:)*mixMW(:,1,:)/molwt(n))*div_diffFlux_y0(:,:,n)
       enddo


       V_y(:,:,5) = V_y(:,:,5) + a(:,:)*div_tau_y_y0(:,:)
       Ly(:,:,5)=Ly(:,:,5) + 0.5*V_y(:,:,5)
     endif

#endif

     call compute_d_y( Ly, a, q(:,1,:,4), d_y )

   !===============================================================================
   ! STEP 3:  Put the corrected term back into each RHS.
   !===============================================================================

     !---- momentum ----
     if(vary_in_x==1) rhs(:,1,:,1) = rhs(:,1,:,1) - (u(:,1,:,1)*d_y(:,:,1) + q(:,1,:,4)*d_y(:,:,3))
     rhs(:,1,:,2) = rhs(:,1,:,2) - (u(:,1,:,2)*d_y(:,:,1) + q(:,1,:,4)*d_y(:,:,4))
     if(vary_in_z==1) rhs(:,1,:,3) = rhs(:,1,:,3) - (u(:,1,:,3)*d_y(:,:,1) + q(:,1,:,4)*d_y(:,:,5))

     !---- continuity ----
     rhs(:,1,:,4) = rhs(:,1,:,4) - d_y(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_y(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(:,1,:)*temp(:,1,:)*mixMW(:,1,:)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_y0(:,:,n) - h_spec_y0(:,:,n_spec))*d_y(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(:,1,:,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(:,1,:,5) = rhs(:,1,:,5) -                                                      &
          ( (q(:,1,:,5)*volum(:,1,:) + (R - cpmix(:,1,:))*temp(:,1,:))*d_y(:,:,1)       &
          + d_y(:,:,2)/(gamma(:,1,:)-1.0) + sumterm )
   ! kinetic energy
     if(vary_in_x==1) rhs(:,1,:,5) = rhs(:,1,:,5) - q(:,1,:,1)*d_y(:,:,3)
     rhs(:,1,:,5) = rhs(:,1,:,5) - q(:,1,:,2)*d_y(:,:,4)
     if(vary_in_z==1) rhs(:,1,:,5) = rhs(:,1,:,5) - q(:,1,:,3)*d_y(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(:,1,:,5+n) = rhs(:,1,:,5+n) - ( yspecies(:,1,:,n)*d_y(:,:,1)          &
                                        + q(:,1,:,4)*d_y(:,:,5+n) )
     enddo
  endif    ! only processors on left boundary do this.


!******************************************************************************
!------------------------- RIGHT BOUNDARY TREATMENT ---------------------------
!******************************************************************************

! only processors on the right boundary do this
  if(yid==ypes-1 .and. periodic_y==0 .and. abs(nrf_yl)==1) then
   !===========================================================================
   ! STEP 1:  Subtract boundary information from current RHS for each equation
   !          and discard it.  It will later be replaced by the approporiate
   !          information for the NSCBCS.
   !===========================================================================

   ! compute derivatives that we still need (not passed in).
     call point_der1_y( pressure, dp_dy, ny, scale_1y )
     call point_der1_y( q(:,:,:,4), drho_dy, ny, scale_1y )

   ! set the sound speed
     R = univ_gascon * (1.0/a_ref**2)*t_ref*avmolwt(:,ny,:)  ! non-dimensional gas constant
     a = sqrt(gamma(:,ny,:)*R*temp(:,ny,:))

   !----------------------------------------------------------------------------
   ! Now construct the Ly's and d's

     call compute_L_y( u(:,ny,:,2), a, q(:,ny,:,4), grad_u_yL(:,:,1),         &
                       grad_u_yL(:,:,2), grad_u_yL(:,:,3), dp_dy,           &
                       grad_Ys_yL, drho_dy, Ly )

     call compute_d_y( Ly, a, q(:,ny,:,4), d_y )

   !----------------------------------------------------------------------------
   ! now that we have the d's, remove the appropriate terms from the RHS for each eqn

     !---- momentum ----
     if(vary_in_x==1) rhs(:,ny,:,1) = rhs(:,ny,:,1) + (u(:,ny,:,1)*d_y(:,:,1) + q(:,ny,:,4)*d_y(:,:,3))
     rhs(:,ny,:,2) = rhs(:,ny,:,2) + (u(:,ny,:,2)*d_y(:,:,1) + q(:,ny,:,4)*d_y(:,:,4))
     if(vary_in_z==1) rhs(:,ny,:,3) = rhs(:,ny,:,3) + (u(:,ny,:,3)*d_y(:,:,1) + q(:,ny,:,4)*d_y(:,:,5))

     !---- continuity ----
     rhs(:,ny,:,4) = rhs(:,ny,:,4) + d_y(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_y(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(:,ny,:)*temp(:,ny,:)*mixMW(:,ny,:)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_yL(:,:,n) - h_spec_yL(:,:,n_spec))*d_y(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(:,ny,:,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(:,ny,:,5) = rhs(:,ny,:,5) +                                                    &
          ( (q(:,ny,:,5)*volum(:,ny,:) + (R - cpmix(:,ny,:))*temp(:,ny,:))*d_y(:,:,1)   &
          + d_y(:,:,2)/(gamma(:,ny,:)-1.0) + sumterm )

     if(vary_in_x==1) rhs(:,ny,:,5) = rhs(:,ny,:,5) + q(:,ny,:,1)*d_y(:,:,3)
     rhs(:,ny,:,5) = rhs(:,ny,:,5) + q(:,ny,:,2)*d_y(:,:,4)
     if(vary_in_z==1) rhs(:,ny,:,5) = rhs(:,ny,:,5) + q(:,ny,:,3)*d_y(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(:,ny,:,5+n) = rhs(:,ny,:,5+n) + ( yspecies(:,ny,:,n)*d_y(:,:,1)       &
                                              + q(:,ny,:,4)*d_y(:,:,5+n) )
     enddo

   !============================================================================
   ! STEP 2:  Modify the Ly's as needed to impose Nonreflecting BC's.
   !          Then compute the new d's from the modified Ly's.
   !============================================================================

#ifndef USE_HISTORIC_BC
     call update_L_y( u(:,ny,:,1), u(:,ny,:,2), u(:,ny,:,3), temp(:,ny,:),         &
                      a, gamma(:,ny,:), pressure(:,ny,:), q(:,ny,:,4),             & 
                      yspecies(:,ny,:,:), mixMW(:,ny,:),                           &
                      molwt,h_spec_yL(:,:,:), rr_r_yL, ny, Ly )
#else

     call update_L_y( u(:,ny,:,1), u(:,ny,:,2), u(:,ny,:,3), temp(:,ny,:),         &
                      a, gamma(:,ny,:), pressure(:,ny,:),                          &
                      q(:,ny,:,4), mixMW(:,ny,:), molwt, h_spec_yL(:,:,:),         &
                      rr_r_yL, ny, Ly )
#endif

   ! Change by Chun Sang Yoo (See Yoo et al., CTM, vol 9, 2005)
   ! Add V_y or diffusive terms to Ly (S_y or source terms are added in update_L_y)
#ifndef USE_HISTORIC_BC

     if (nrf_yl == -1) then     ! nonreflecting inflow conditions

       V_y(:,:,1) = V_y(:,:,1) + (gamma(:,ny,:)-1.0) *                    &
                                 tau_yL(:,:,2,2)*grad_u_yL_all(:,:,2,2)

       if (vary_in_x == 1) V_y(:,:,1) = V_y(:,:,1) + (gamma(:,ny,:)-1.0) *      &
                                      ( tau_yL(:,:,1,1)*grad_u_yL_all(:,:,1,1)  &
                                      + tau_yL(:,:,1,2)*grad_u_yL_all(:,:,1,2)  &
                                      + tau_yL(:,:,2,1)*grad_u_yL_all(:,:,2,1))

       if (vary_in_z == 1) V_y(:,:,1) = V_y(:,:,1) + (gamma(:,ny,:)-1.0) *      &
                                      ( tau_yL(:,:,3,1)*grad_u_yL_all(:,:,3,1)  &
                                      + tau_yL(:,:,1,3)*grad_u_yL_all(:,:,1,3)  &
                                      + tau_yL(:,:,2,3)*grad_u_yL_all(:,:,2,3)  &
                                      + tau_yL(:,:,3,2)*grad_u_yL_all(:,:,3,2)  &
                                      + tau_yL(:,:,3,3)*grad_u_yL_all(:,:,3,3))

       V_y(:,:,1) = V_y(:,:,1) - (gamma(:,ny,:)-1.0)*div_heatFlux_yL(:,:)

       do n=1,n_spec
         V_y(:,:,1) = V_y(:,:,1) + (gamma(:,ny,:)-1.0)*(h_spec_yL(:,:,n)        &
                    - cpmix(:,ny,:)*temp(:,ny,:)*mixMW(:,ny,:)/molwt(n))*div_diffFlux_yL(:,:,n)
         V_y(:,:,5+n) = div_diffFlux_yL(:,:,n)/q(:,ny,:,4)
       enddo

       V_y(:,:,2) = -V_y(:,:,1)/a(:,:)**2

       V_y(:,:,1) = V_y(:,:,1) - a(:,:)*div_tau_y_yL(:,:)
       V_y(:,:,3) = div_tau_x_yL(:,:)/q(:,ny,:,4)
       V_y(:,:,4) = div_tau_z_yL(:,:)/q(:,ny,:,4)

       Ly(:,:,1)=Ly(:,:,1) + 0.5*V_y(:,:,1)
       Ly(:,:,2)=Ly(:,:,2) + V_y(:,:,2)
       Ly(:,:,3)=Ly(:,:,3) + V_y(:,:,3)
       Ly(:,:,4)=Ly(:,:,4) + V_y(:,:,4)
       do n=1,n_spec
         Ly(:,:,5+n) = Ly(:,:,5+n) + V_y(:,:,5+n)
       enddo

     elseif (nrf_yl == 1) then     ! nonreflecting outflow boundary conditions

       V_y(:,:,1) = (gamma(:,ny,:)-1.0) *                    &
                    tau_yL(:,:,2,2)*grad_u_yL_all(:,:,2,2)

       if (vary_in_x == 1) V_y(:,:,1) = V_y(:,:,1) + (gamma(:,ny,:)-1.0) *      &
                                      ( tau_yL(:,:,1,1)*grad_u_yL_all(:,:,1,1)  &
                                      + tau_yL(:,:,1,2)*grad_u_yL_all(:,:,1,2)  &
                                      + tau_yL(:,:,2,1)*grad_u_yL_all(:,:,2,1))

       if (vary_in_z == 1) V_y(:,:,1) = V_y(:,:,1) + (gamma(:,ny,:)-1.0) *      &
                                      ( tau_yL(:,:,3,1)*grad_u_yL_all(:,:,3,1)  &
                                      + tau_yL(:,:,1,3)*grad_u_yL_all(:,:,1,3)  &
                                      + tau_yL(:,:,2,3)*grad_u_yL_all(:,:,2,3)  &
                                      + tau_yL(:,:,3,2)*grad_u_yL_all(:,:,3,2)  &
                                      + tau_yL(:,:,3,3)*grad_u_yL_all(:,:,3,3))

       V_y(:,:,1) = V_y(:,:,1) - (gamma(:,ny,:)-1.0)*div_heatFlux_yL(:,:)

       do n=1,n_spec
         V_y(:,:,1) = V_y(:,:,1) + (gamma(:,ny,:)-1.0)*(h_spec_yL(:,:,n)        &
                    - cpmix(:,ny,:)*temp(:,ny,:)*mixMW(:,ny,:)/molwt(n))*div_diffFlux_yL(:,:,n)
       enddo
  
       V_y(:,:,1) = V_y(:,:,1) - a(:,:)*div_tau_y_yL(:,:) 
       Ly(:,:,1)=Ly(:,:,1) + 0.5*V_y(:,:,1)
     
     endif

#endif
     call compute_d_y( Ly, a, q(:,ny,:,4), d_y )

   !============================================================================
   ! STEP 3:  Put the correction term back into each RHS.
   !============================================================================

     !---- momentum ----
     if(vary_in_x==1) rhs(:,ny,:,1) = rhs(:,ny,:,1) - (u(:,ny,:,1)*d_y(:,:,1) + q(:,ny,:,4)*d_y(:,:,3))
     rhs(:,ny,:,2) = rhs(:,ny,:,2) - (u(:,ny,:,2)*d_y(:,:,1) + q(:,ny,:,4)*d_y(:,:,4))
     if(vary_in_z==1) rhs(:,ny,:,3) = rhs(:,ny,:,3) - (u(:,ny,:,3)*d_y(:,:,1) + q(:,ny,:,4)*d_y(:,:,5))

     !---- continuity ----
     rhs(:,ny,:,4) = rhs(:,ny,:,4) - d_y(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_y(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(:,ny,:)*temp(:,ny,:)*mixMW(:,ny,:)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_yL(:,:,n) - h_spec_yL(:,:,n_spec))*d_y(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(:,ny,:,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(:,ny,:,5) = rhs(:,ny,:,5) -                                                    &
          ( (q(:,ny,:,5)*volum(:,ny,:) + (R - cpmix(:,ny,:))*temp(:,ny,:))*d_y(:,:,1)   &
          + d_y(:,:,2)/(gamma(:,ny,:)-1.0) + sumterm )
     if(vary_in_x==1) rhs(:,ny,:,5) = rhs(:,ny,:,5) - q(:,ny,:,1)*d_y(:,:,3)
     rhs(:,ny,:,5) = rhs(:,ny,:,5) - q(:,ny,:,2)*d_y(:,:,4)
     if(vary_in_z==1) rhs(:,ny,:,5) = rhs(:,ny,:,5) - q(:,ny,:,3)*d_y(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(:,ny,:,5+n) = rhs(:,ny,:,5+n) - ( yspecies(:,ny,:,n)*d_y(:,:,1)       &
                                          + q(:,ny,:,4)*d_y(:,:,5+n) )
     enddo
  endif          ! only processors on the right boundary do this

!------------------------------- DONE -------------------------------

  return
end subroutine nscbc_y



!!$==================================================================================
!!$==================================================================================
!!$==================================================================================



! Add more terms for diffusion terms. Chun Sang Yoo 05/06
! Ramanan Sankaran - 01/05/05 - Replaced old grad's with the new arguments
#ifndef USE_HISTORIC_BC
subroutine nscbc_z( q, h_spec_z0, h_spec_zL, rr_r_z0, rr_r_zL, &
                    grad_Ys_z0, grad_Ys_zL, grad_u_z0, grad_u_zL, rhs, &
                    grad_u_z0_all, grad_u_zL_all, tau_z0, tau_zL, &
                    div_heatFlux_z0, div_heatFlux_zL, div_tau_x_z0, div_tau_x_zL, &
                    div_tau_y_z0, div_tau_y_zL, div_tau_z_z0, div_tau_z_zL, &
                    div_diffFlux_z0, div_diffFlux_zL )
#else
subroutine nscbc_z( q, h_spec_z0, h_spec_zL, rr_r_z0, rr_r_zL, &
               grad_Ys_z0, grad_Ys_zL, grad_u_z0, grad_u_zL, rhs )
#endif
  !-----------------------------------------------------
  ! Z-DIRECTION IMPLEMENTATION OF BOUNDARY CONDITIONS
  !-----------------------------------------------------
  use param_m, only : nx,ny,nz, nvar_tot, n_spec, periodic_z, vary_in_x, vary_in_y
  use grid_m, only : scale_1z
  use topology_m

  use bc_m

  use thermchem_m, only : gamma, cpmix, avmolwt, mixMW
  use reference_m, only : univ_gascon, a_ref, t_ref     ! used to compute sound speed
  use chemkin_m, only : molwt, molwt_c

  use variables_m, only : yspecies, u, volum, pressure, temp

  implicit none

  real, intent(in),    dimension(nx,ny,nz,nvar_tot) :: q
  real, intent(in),    dimension(nx,ny,n_spec)   :: h_spec_z0, h_spec_zL
  real, intent(in),    dimension(nx,ny,n_spec) :: rr_r_z0, rr_r_zL
  real, intent(in),    dimension(nx,ny,n_spec) :: grad_Ys_z0, grad_Ys_zL
  real, intent(in),    dimension(nx,ny,3)      :: grad_u_z0, grad_u_zL
  real, intent(inout), dimension(nx,ny,nz,nvar_tot) :: rhs
#ifndef USE_HISTORIC_BC

  real, intent(in),    dimension(nx,ny,3,3) :: grad_u_z0_all, grad_u_zL_all
  real, intent(in),    dimension(nx,ny,3,3) :: tau_z0, tau_zL
  real, intent(in),    dimension(nx,ny) :: div_heatFlux_z0, div_tau_x_z0, div_tau_y_z0, div_tau_z_z0
  real, intent(in),    dimension(nx,ny) :: div_heatFlux_zL, div_tau_x_zL, div_tau_y_zL, div_tau_z_zL
  real, intent(in),    dimension(nx,ny,n_spec) :: div_diffFlux_z0, div_diffFlux_zL

#endif

! these quantities will only be calculated on the Z-Boundary faces (XY planes)
  real, dimension(nx,ny) :: dp_dz, drho_dz, a, sumterm, R, tmp
  real, dimension(nx,ny,nvar_tot+1) :: Lz, d_z, V_z

  integer :: n


!******************************************************************************
!-------------------------  Left BOUNDARY TREATMENT ---------------------------
!******************************************************************************

! only processors on left boundary do this.
  if (zid==0 .and. periodic_z==0 .and. abs(nrf_z0)==1) then

   !===========================================================================
   ! STEP 1:  Subtract boundary information from current RHS for each equation
   !          and discard it.  It will later be replaced by the approporiate
   !          information for the NSCBCS.
   !===========================================================================

   ! compute derivatives that we still need (not passed in).
     call point_der1_z( pressure, dp_dz, 1, scale_1z )     ! compute dp/dz   at left Z-Boundary
     call point_der1_z( q(:,:,:,4), drho_dz, 1, scale_1z ) ! compute drho/dz at left Z-Boundary

   ! set the sound speed
     R = univ_gascon * (1.0/a_ref**2)*t_ref*avmolwt(:,:,1)   ! non-dimensional gas constant
     a = sqrt(gamma(:,:,1)*R*temp(:,:,1))

   !----------------------------------------------------------------------------
   ! Now construct the Lz's and d's

     call compute_L_z( u(:,:,1,3), a, q(:,:,1,4), grad_u_z0(:,:,1),    &
                       grad_u_z0(:,:,2), grad_u_z0(:,:,3), dp_dz,     &
                       grad_Ys_z0, drho_dz, Lz )

     call compute_d_z( Lz, a, q(:,:,1,4), d_z )

   !----------------------------------------------------------------------------
   ! now that we have the d's, remove the appropriate terms from the RHS for each eqn

     !---- momentum ----
     if(vary_in_x==1) rhs(:,:,1,1) = rhs(:,:,1,1) + (u(:,:,1,1)*d_z(:,:,1) + q(:,:,1,4)*d_z(:,:,3))
     if(vary_in_y==1) rhs(:,:,1,2) = rhs(:,:,1,2) + (u(:,:,1,2)*d_z(:,:,1) + q(:,:,1,4)*d_z(:,:,4))
     rhs(:,:,1,3) = rhs(:,:,1,3) + (u(:,:,1,3)*d_z(:,:,1) + q(:,:,1,4)*d_z(:,:,5))

     !---- continuity ----
     rhs(:,:,1,4) = rhs(:,:,1,4) + d_z(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_z(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(:,:,1)*temp(:,:,1)*mixMW(:,:,1)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_z0(:,:,n) - h_spec_z0(:,:,n_spec))*d_z(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(:,:,1,4)
     ! END BUG FIX 14-JAN-2005

     rhs(:,:,1,5) = rhs(:,:,1,5) +                                              &
          ( (q(:,:,1,5)*volum(:,:,1) + (R - cpmix(:,:,1))*temp(:,:,1))*d_z(:,:,1) &
          + d_z(:,:,2)/(gamma(:,:,1)-1.0) + sumterm )
     ! kinetic energy
     if(vary_in_x==1) rhs(:,:,1,5) = rhs(:,:,1,5) + q(:,:,1,1)*d_z(:,:,3)
     if(vary_in_y==1) rhs(:,:,1,5) = rhs(:,:,1,5) + q(:,:,1,2)*d_z(:,:,4)
     rhs(:,:,1,5) = rhs(:,:,1,5) + q(:,:,1,3)*d_z(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(:,:,1,5+n) = rhs(:,:,1,5+n) + (yspecies(:,:,1,n)*d_z(:,:,1)           &
                                        + q(:,:,1,4)*d_z(:,:,5+n))
     enddo

   !===============================================================================
   ! STEP 2:  Modify the Lz's as needed to impose Nonreflecting BC's.
   !          Then compute the new d's from the modified Lz's.
   !===============================================================================

#ifndef USE_HISTORIC_BC
     call update_L_z( u(:,:,1,1), u(:,:,1,2), u(:,:,1,3), temp(:,:,1), a,           &
                      gamma(:,:,1), pressure(:,:,1), q(:,:,1,4), yspecies(:,:,1,:), &
                      mixMW(:,:,1), molwt, h_spec_z0(:,:,:), rr_r_z0, 1, Lz) 
#else
     call update_L_z( u(:,:,1,3), a, gamma(:,:,1), pressure(:,:,1),             &
                      q(:,:,1,4), mixMW(:,:,1), molwt, h_spec_z0(:,:,:),         &
                      rr_r_z0, 1, Lz )
#endif

   ! Change by Chun Sang Yoo (See Yoo et al., CTM, vol 9, 2005)
   ! Add V_z or diffusive terms to Lz (S_z or source terms are added in update_L_z)
#ifndef USE_HISTORIC_BC

     if (nrf_z0 == -1) then     ! nonreflecting inflow conditions
       
       V_z(:,:,5) = (gamma(:,:,1)-1.0) *                     &
                     tau_z0(:,:,3,3)*grad_u_z0_all(:,:,3,3)
       
       if (vary_in_x == 1) V_z(:,:,5) = V_z(:,:,5) + (gamma(:,:,1)-1.0) *       &
                                      ( tau_z0(:,:,1,1)*grad_u_z0_all(:,:,1,1)  &
                                      + tau_z0(:,:,1,3)*grad_u_z0_all(:,:,1,3)  &
                                      + tau_z0(:,:,3,1)*grad_u_z0_all(:,:,3,1))
       
       if (vary_in_y == 1) V_z(:,:,5) = V_z(:,:,5) + (gamma(:,:,1)-1.0) *       &
                                      ( tau_z0(:,:,1,2)*grad_u_z0_all(:,:,1,2)  &
                                      + tau_z0(:,:,2,1)*grad_u_z0_all(:,:,2,1)  &
                                      + tau_z0(:,:,2,2)*grad_u_z0_all(:,:,2,2)  &
                                      + tau_z0(:,:,2,3)*grad_u_z0_all(:,:,2,3)  &
                                      + tau_z0(:,:,3,2)*grad_u_z0_all(:,:,3,2))
       
       V_z(:,:,5) = V_z(:,:,5) - (gamma(:,:,1)-1.0)*div_heatFlux_z0(:,:)
       
       do n=1,n_spec
         V_z(:,:,5) = V_z(:,:,5) + (gamma(:,:,1)-1.0)*(h_spec_z0(:,:,n)        &
                    - cpmix(:,:,1)*temp(:,:,1)*mixMW(:,:,1)/molwt(n))*div_diffFlux_z0(:,:,n)
         V_z(:,:,5+n) = div_diffFlux_z0(:,:,n)/q(:,:,1,4)
       enddo
       
       V_z(:,:,2) = -V_z(:,:,5)/a(:,:)**2
       
       V_z(:,:,5) = V_z(:,:,5) + a(:,:)*div_tau_z_z0(:,:)
       V_z(:,:,3) = div_tau_x_z0(:,:)/q(:,:,1,4)
       V_z(:,:,4) = div_tau_y_z0(:,:)/q(:,:,1,4)
       
       Lz(:,:,2)=Lz(:,:,2) + V_z(:,:,2)
       Lz(:,:,3)=Lz(:,:,3) + V_z(:,:,3)
       Lz(:,:,4)=Lz(:,:,4) + V_z(:,:,4)
       Lz(:,:,5)=Lz(:,:,5) + 0.5*V_z(:,:,5)
       do n=1,n_spec 
         Lz(:,:,5+n) = Lz(:,:,5+n) + V_z(:,:,5+n)
       enddo
     
     elseif (nrf_z0 == 1) then     ! nonreflecting outflow boundary conditions
       
       V_z(:,:,5) = (gamma(:,:,1)-1.0) *                     &
                     tau_z0(:,:,3,3)*grad_u_z0_all(:,:,3,3)
       
       if (vary_in_x == 1) V_z(:,:,5) = V_z(:,:,5) + (gamma(:,:,1)-1.0) *       &
                                      ( tau_z0(:,:,1,1)*grad_u_z0_all(:,:,1,1)  &
                                      + tau_z0(:,:,1,3)*grad_u_z0_all(:,:,1,3)  &
                                      + tau_z0(:,:,3,1)*grad_u_z0_all(:,:,3,1))
       
       if (vary_in_y == 1) V_z(:,:,5) = V_z(:,:,5) + (gamma(:,:,1)-1.0) *       &
                                      ( tau_z0(:,:,1,2)*grad_u_z0_all(:,:,1,2)  &
                                      + tau_z0(:,:,2,1)*grad_u_z0_all(:,:,2,1)  &
                                      + tau_z0(:,:,2,2)*grad_u_z0_all(:,:,2,2)  &
                                      + tau_z0(:,:,2,3)*grad_u_z0_all(:,:,2,3)  &
                                      + tau_z0(:,:,3,2)*grad_u_z0_all(:,:,3,2))
       
       V_z(:,:,5) = V_z(:,:,5) - (gamma(:,:,1)-1.0)*div_heatFlux_z0(:,:)
       
       do n=1,n_spec
         V_z(:,:,5) = V_z(:,:,5) + (gamma(:,:,1)-1.0)*(h_spec_z0(:,:,n)        &
                    - cpmix(:,:,1)*temp(:,:,1)*mixMW(:,:,1)/molwt(n))*div_diffFlux_z0(:,:,n)
       enddo

       V_z(:,:,5) = V_z(:,:,5) + a(:,:)*div_tau_z_z0(:,:)
       Lz(:,:,5)=Lz(:,:,5) + 0.5*V_z(:,:,5)

     endif

#endif
     call compute_d_z( Lz, a, q(:,:,1,4), d_z )

   !===============================================================================
   ! STEP 3:  Put the corrected term back into each RHS.
   !===============================================================================

     !---- momentum ----
     if(vary_in_x==1) rhs(:,:,1,1) = rhs(:,:,1,1) - (u(:,:,1,1)*d_z(:,:,1) + q(:,:,1,4)*d_z(:,:,3))
     if(vary_in_y==1) rhs(:,:,1,2) = rhs(:,:,1,2) - (u(:,:,1,2)*d_z(:,:,1) + q(:,:,1,4)*d_z(:,:,4))
     rhs(:,:,1,3) = rhs(:,:,1,3) - (u(:,:,1,3)*d_z(:,:,1) + q(:,:,1,4)*d_z(:,:,5))

     !---- continuity ----
     rhs(:,:,1,4) = rhs(:,:,1,4) - d_z(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_z(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(:,:,1)*temp(:,:,1)*mixMW(:,:,1)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_z0(:,:,n) - h_spec_z0(:,:,n_spec))*d_z(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(:,:,1,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(:,:,1,5) = rhs(:,:,1,5) -                                              &
          ( (q(:,:,1,5)*volum(:,:,1) + (R - cpmix(:,:,1))*temp(:,:,1))*d_z(:,:,1) &
          + d_z(:,:,2)/(gamma(:,:,1)-1.0) + sumterm )
   ! kinetic energy
     if(vary_in_x==1) rhs(:,:,1,5) = rhs(:,:,1,5) - q(:,:,1,1)*d_z(:,:,3) 
     if(vary_in_y==1) rhs(:,:,1,5) = rhs(:,:,1,5) - q(:,:,1,2)*d_z(:,:,4)
     rhs(:,:,1,5) = rhs(:,:,1,5) - q(:,:,1,3)*d_z(:,:,5)
     
     !---- species ----
     do n=1,n_spec-1
        rhs(:,:,1,5+n) = rhs(:,:,1,5+n) - ( yspecies(:,:,1,n)*d_z(:,:,1)          &
                                        + q(:,:,1,4)*d_z(:,:,5+n) )
     enddo
  endif    ! only processors on left boundary do this.


!******************************************************************************
!------------------------- RIGHT BOUNDARY TREATMENT ---------------------------
!******************************************************************************

! only processors on the right boundary do this
  if(zid==zpes-1 .and. periodic_z==0 .and. abs(nrf_zl)==1) then
   !===========================================================================
   ! STEP 1:  Subtract boundary information from current RHS for each equation
   !          and discard it.  It will later be replaced by the approporiate
   !          information for the NSCBCS.
   !===========================================================================

   ! compute derivatives that we still need (not passed in).
     call point_der1_z( pressure, dp_dz, nz, scale_1z )
     call point_der1_z( q(:,:,:,4), drho_dz, nz, scale_1z )

   ! set the sound speed
     R = univ_gascon * (1.0/a_ref**2)*t_ref*avmolwt(:,:,nz)  ! nondimensional gas constant
     a = sqrt(gamma(:,:,nz)*R*temp(:,:,nz))

   !----------------------------------------------------------------------------
   ! Now construct the Lz's and d's

     call compute_L_z( u(:,:,nz,3), a, q(:,:,nz,4), grad_u_zL(:,:,1),         &
                       grad_u_zL(:,:,2), grad_u_zL(:,:,3), dp_dz,           &
                       grad_Ys_zL, drho_dz, Lz )

     call compute_d_z( Lz, a, q(:,:,nz,4), d_z )

   !----------------------------------------------------------------------------
   ! now that we have the d's, remove the appropriate terms from the RHS for each eqn

     !---- momentum ----
     if(vary_in_x==1) rhs(:,:,nz,1) = rhs(:,:,nz,1) + (u(:,:,nz,1)*d_z(:,:,1) + q(:,:,nz,4)*d_z(:,:,3))
     if(vary_in_y==1) rhs(:,:,nz,2) = rhs(:,:,nz,2) + (u(:,:,nz,2)*d_z(:,:,1) + q(:,:,nz,4)*d_z(:,:,4))
     rhs(:,:,nz,3) = rhs(:,:,nz,3) + (u(:,:,nz,3)*d_z(:,:,1) + q(:,:,nz,4)*d_z(:,:,5))

     !---- continuity ----
     rhs(:,:,nz,4) = rhs(:,:,nz,4) + d_z(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_z(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(:,:,nz)*temp(:,:,nz)*mixMW(:,:,nz)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_zL(:,:,n) - h_spec_zL(:,:,n_spec))*d_z(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(:,:,nz,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(:,:,nz,5) = rhs(:,:,nz,5) +                                                &
          ( (q(:,:,nz,5)*volum(:,:,nz) + (R - cpmix(:,:,nz))*temp(:,:,nz))*d_z(:,:,1) &
          + d_z(:,:,2)/(gamma(:,:,nz)-1.0) + sumterm )

   ! kinetic energy
     if(vary_in_x==1) rhs(:,:,nz,5) = rhs(:,:,nz,5) + q(:,:,nz,1)*d_z(:,:,3)
     if(vary_in_y==1) rhs(:,:,nz,5) = rhs(:,:,nz,5) + q(:,:,nz,2)*d_z(:,:,4)
     rhs(:,:,nz,5) = rhs(:,:,nz,5) + q(:,:,nz,3)*d_z(:,:,5)

     !---- species ----
     do n=1,n_spec-1
        rhs(:,:,nz,5+n) = rhs(:,:,nz,5+n) + ( yspecies(:,:,nz,n)*d_z(:,:,1)       &
                                          + q(:,:,nz,4)*d_z(:,:,5+n) )
     enddo

   !============================================================================
   ! STEP 2:  Modify the Lz's as needed to impose Nonreflecting BC's.
   !          Then compute the new d's from the modified Lz's.
   !============================================================================
#ifndef USE_HISTORIC_BC
     call update_L_z( u(:,:,nz,1), u(:,:,nz,2), u(:,:,nz,3), temp(:,:,nz), a,           &
                      gamma(:,:,nz), pressure(:,:,nz), q(:,:,nz,4), yspecies(:,:,nz,:), &
                      mixMW(:,:,nz), molwt, h_spec_zL(:,:,:), rr_r_zL, nz, Lz) 
#else
     call update_L_z( u(:,:,nz,3), a, gamma(:,:,nz), pressure(:,:,nz),             &
                      q(:,:,nz,4), mixMW(:,:,nz), molwt, h_spec_zL(:,:,:),         &
                      rr_r_zL, nz, Lz )
#endif

   ! Change by Chun Sang Yoo (See Yoo et al., CTM, vol 9, 2005)
   ! Add V_z or diffusive terms to Lz (S_z or source terms are added in update_L_z)
#ifndef USE_HISTORIC_BC

     if (nrf_zl == -1) then     ! nonreflecting inflow conditions

       V_z(:,:,1) = (gamma(:,:,nz)-1.0) *                    &
                     tau_zL(:,:,3,3)*grad_u_zL_all(:,:,3,3)

       if (vary_in_x == 1) V_z(:,:,1) = V_z(:,:,1) + (gamma(:,:,nz)-1.0) *      &
                                      ( tau_zL(:,:,1,1)*grad_u_zL_all(:,:,1,1)  &
                                      + tau_zL(:,:,1,3)*grad_u_zL_all(:,:,1,3)  &
                                      + tau_zL(:,:,3,1)*grad_u_zL_all(:,:,3,1))

       if (vary_in_y == 1) V_z(:,:,1) = V_z(:,:,1) + (gamma(:,:,nz)-1.0) *      &
                                      ( tau_zL(:,:,1,2)*grad_u_zL_all(:,:,1,2)  &
                                      + tau_zL(:,:,2,1)*grad_u_zL_all(:,:,2,1)  &
                                      + tau_zL(:,:,2,2)*grad_u_zL_all(:,:,2,2)  &
                                      + tau_zL(:,:,2,3)*grad_u_zL_all(:,:,2,3)  &
                                      + tau_zL(:,:,3,2)*grad_u_zL_all(:,:,3,2))

       V_z(:,:,1) = V_z(:,:,1) - (gamma(:,:,nz)-1.0)*div_heatFlux_zL(:,:)

       do n=1,n_spec
         V_z(:,:,1) = V_z(:,:,1) + (gamma(:,:,nz)-1.0)*(h_spec_zL(:,:,n)        &
                    - cpmix(:,:,nz)*temp(:,:,nz)*mixMW(:,:,nz)/molwt(n))*div_diffFlux_zL(:,:,n)
         V_z(:,:,5+n) = div_diffFlux_zL(:,:,n)/q(:,:,nz,4)
       enddo

       V_z(:,:,2) = -V_z(:,:,1)/a(:,:)**2

       V_z(:,:,1) = V_z(:,:,1) - a(:,:)*div_tau_z_zL(:,:)
       V_z(:,:,3) = div_tau_x_zL(:,:)/q(:,:,nz,4)
       V_z(:,:,4) = div_tau_y_zL(:,:)/q(:,:,nz,4)

       Lz(:,:,1)=Lz(:,:,1) + 0.5*V_z(:,:,1)
       Lz(:,:,2)=Lz(:,:,2) + V_z(:,:,2)
       Lz(:,:,3)=Lz(:,:,3) + V_z(:,:,3)
       Lz(:,:,4)=Lz(:,:,4) + V_z(:,:,4)
       do n=1,n_spec
         Lz(:,:,5+n) = Lz(:,:,5+n) + V_z(:,:,5+n)
       enddo

     elseif (nrf_zl == 1) then     ! nonreflecting outflow boundary conditions

       V_z(:,:,1) = (gamma(:,:,nz)-1.0) *                    &
                     tau_zL(:,:,3,3)*grad_u_zL_all(:,:,3,3)

       if (vary_in_x == 1) V_z(:,:,1) = V_z(:,:,1) + (gamma(:,:,nz)-1.0) *      &
                                      ( tau_zL(:,:,1,1)*grad_u_zL_all(:,:,1,1)  &
                                      + tau_zL(:,:,1,3)*grad_u_zL_all(:,:,1,3)  &
                                      + tau_zL(:,:,3,1)*grad_u_zL_all(:,:,3,1))

       if (vary_in_y == 1) V_z(:,:,1) = V_z(:,:,1) + (gamma(:,:,nz)-1.0) *      &
                                      ( tau_zL(:,:,1,2)*grad_u_zL_all(:,:,1,2)  &
                                      + tau_zL(:,:,2,1)*grad_u_zL_all(:,:,2,1)  &
                                      + tau_zL(:,:,2,2)*grad_u_zL_all(:,:,2,2)  &
                                      + tau_zL(:,:,2,3)*grad_u_zL_all(:,:,2,3)  &
                                      + tau_zL(:,:,3,2)*grad_u_zL_all(:,:,3,2))

       V_z(:,:,1) = V_z(:,:,1) - (gamma(:,:,nz)-1.0)*div_heatFlux_zL(:,:)

       do n=1,n_spec
         V_z(:,:,1) = V_z(:,:,1) + (gamma(:,:,nz)-1.0)*(h_spec_zL(:,:,n)        &
                    - cpmix(:,:,nz)*temp(:,:,nz)*mixMW(:,:,nz)/molwt(n))*div_diffFlux_zL(:,:,n)
       enddo
  
       V_z(:,:,1) = V_z(:,:,1) - a(:,:)*div_tau_z_zL(:,:) 
       Lz(:,:,1)=Lz(:,:,1) + 0.5*V_z(:,:,1)
     
     endif

#endif
     call compute_d_z( Lz, a, q(:,:,nz,4), d_z )

   !============================================================================
   ! STEP 3:  Put the correction term back into each RHS.
   !============================================================================

     !---- momentum ----
     if(vary_in_x==1) rhs(:,:,nz,1) = rhs(:,:,nz,1) - (u(:,:,nz,1)*d_z(:,:,1) + q(:,:,nz,4)*d_z(:,:,3))
     if(vary_in_y==1) rhs(:,:,nz,2) = rhs(:,:,nz,2) - (u(:,:,nz,2)*d_z(:,:,1) + q(:,:,nz,4)*d_z(:,:,4))
     rhs(:,:,nz,3) = rhs(:,:,nz,3) - (u(:,:,nz,3)*d_z(:,:,1) + q(:,:,nz,4)*d_z(:,:,5))

     !---- continuity ----
     rhs(:,:,nz,4) = rhs(:,:,nz,4) - d_z(:,:,1)

     !---- energy ----
     
     ! BUG FIX 14-JAN-2005 Evatt Hawkes
     ! previous formulation fix not correctly implemented
     sumterm(:,:) = 0.0
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (molwt_c(n) - molwt_c(n_spec))*d_z(:,:,5+n)      
     enddo
     sumterm(:,:) = -cpmix(:,:,nz)*temp(:,:,nz)*mixMW(:,:,nz)*sumterm(:,:)
     do n=1,n_spec-1
       sumterm(:,:) = sumterm(:,:) + (h_spec_zL(:,:,n) - h_spec_zL(:,:,n_spec))*d_z(:,:,5+n)
     enddo
     sumterm(:,:) = sumterm(:,:)*q(:,:,nz,4)
     ! END BUG FIX 14-JAN-2005
     
     rhs(:,:,nz,5) = rhs(:,:,nz,5) -                                                &
          ( (q(:,:,nz,5)*volum(:,:,nz) + (R - cpmix(:,:,nz))*temp(:,:,nz))*d_z(:,:,1) &
          + d_z(:,:,2)/(gamma(:,:,nz)-1.0) + sumterm )
   ! kinetic energy
     if(vary_in_x==1) rhs(:,:,nz,5) = rhs(:,:,nz,5) - q(:,:,nz,1)*d_z(:,:,3)
     if(vary_in_y==1) rhs(:,:,nz,5) = rhs(:,:,nz,5) - q(:,:,nz,2)*d_z(:,:,4)
     rhs(:,:,nz,5) = rhs(:,:,nz,5) - q(:,:,nz,3)*d_z(:,:,5)
 
     !---- species ----
     do n=1,n_spec-1
        rhs(:,:,nz,5+n) = rhs(:,:,nz,5+n) - ( yspecies(:,:,nz,n)*d_z(:,:,1)       &
                                          + q(:,:,nz,4)*d_z(:,:,5+n) )
     enddo
  endif          ! only processors on the right boundary do this

!------------------------------- DONE -------------------------------

  return
end subroutine nscbc_z

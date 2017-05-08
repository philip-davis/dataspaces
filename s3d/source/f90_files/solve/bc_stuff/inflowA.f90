#include "globalDefines.h"
subroutine inflowA_BCx( rho, drho_dt, h_spec, rr_r, du_dx )
  use param_m, only : nx,ny,nz, nvar_tot, n_spec, periodic_x
  use param_m, only : vary_in_y, vary_in_z
  use grid_m, only : scale_1x
  use topology_m

  use bc_m
  use runtime_m, only : time

  use thermchem_m, only : gamma, cpmix, avmolwt, mixMW
  use reference_m, only : univ_gascon, a_ref, t_ref
  use chemkin_m, only : molwt, molwt_c

  use variables_m, only : yspecies, u, volum, pressure, temp

  implicit none

  real, intent(in), dimension(nx,ny,nz) :: rho
  real, intent(inout), dimension(nx,ny,nz) :: drho_dt
  real, intent(in), dimension(ny,nz,n_spec) :: h_spec, rr_r
  real, intent(in), dimension(ny,nz) :: du_dx
  real, dimension(ny,nz,n_spec) :: dY_dt
  real, dimension(ny,nz) :: R, a, Sp, d1, dp_dx, drho_dx, L2, L5, L1, du_dt, dT_dt, tmp
  real :: h_n, w_n
  integer :: n

! get time derivatives  - must be called by all processes
  if(periodic_x==0 .and. nrf_x0==-2) call time_der( time, dT_dt, du_dt, dY_dt )

  if(xid==0 .and. periodic_x==0 .and. nrf_x0==-2) then

   ! set the sound speed
     R = univ_gascon * (1.0/a_ref**2.0)*t_ref*avmolwt(1,:,:) ! non-dimensional gas constant
     a = sqrt(gamma(1,:,:)*R*temp(1,:,:))                  ! speed of sound.

   ! compute d1 and remove it from the continuity eqn.
   ! d1 = 1/a^2 * [a^2*L2 + L5+L1]
     call point_der1_x( pressure, dp_dx, 1, scale_1x ) ! compute dp/dx   at left X-Boundary
     call point_der1_x( rho, drho_dx, 1, scale_1x )    ! compute drho/dx at left X-Boundary

     L2 = u(1,:,:,1)*( drho_dx - dp_dx/(a*a) )
     L5 = 0.5*(u(1,:,:,1)+a)*(dp_dx + rho(1,:,:)*a*du_dx)
     L1 = 0.5*(u(1,:,:,1)-a)*(dp_dx - rho(1,:,:)*a*du_dx)
     d1 = L2 + (L5+L1)/(a*a)
     drho_dt(1,:,:) = drho_dt(1,:,:) + d1

   ! compute the pressure source term
     Sp = 0.0
     do n=1,n_spec-1
        Sp = Sp + rr_r(:,:,n)*( (1-gamma(1,:,:))*(h_spec(:,:,n)-h_spec(:,:,n_spec))          &
                + gamma(1,:,:)*pressure(1,:,:)*mixMW(1,:,:)*volum(1,:,:)*(molwt_c(n)-molwt_c(n_spec)) )
     enddo

   ! update d1 and add it back to the continuity eqn.
!     call time_der( time, dT_dt, du_dt, dY_dt )
     L5 = L1 - rho(1,:,:)*a*(du_dt)
     tmp = 0.0
     do n=1,n_spec-1
        tmp = tmp + dY_dt(:,:,n)*(molwt_c(n)-molwt_c(n_spec))
     enddo
     L2 = -Sp/(R*temp(1,:,:)) + (gamma(1,:,:)-1.0)/a**2*(L1+L5)             &
          + rho(1,:,:)/temp(1,:,:)*dT_dt + rho(1,:,:)*mixMW(1,:,:)*tmp
     d1 = L2 + (L5+L1)/a**2.0
     drho_dt(1,:,:) = drho_dt(1,:,:) - d1
  endif



! get time derivatives
  if(periodic_x==0 .and. nrf_xl==-2) call time_der( time, dT_dt, du_dt, dY_dt )

  if(xid==(xpes-1) .and. periodic_x==0 .and. nrf_xl==-2) then
   ! set the sound speed
     R = univ_gascon * (1.0/a_ref**2.0)*t_ref*avmolwt(nx,:,:) ! non-dimensional gas constant
     a = sqrt(gamma(nx,:,:)*R*temp(nx,:,:))                 ! speed of sound.

   ! compute d1 and remove it from the continuity eqn.
   ! d1 = 1/a^2 * [a^2*L2 + L5+L1]
     call point_der1_x( pressure, dp_dx, nx, scale_1x ) ! compute dp/dx   at left X-Boundary
     call point_der1_x( rho, drho_dx, nx, scale_1x )    ! compute drho/dx at left X-Boundary

     L2 = u(nx,:,:,1)*( drho_dx - dp_dx/(a*a) )
     L5 = 0.5*(u(nx,:,:,1)+a)*(dp_dx + rho(nx,:,:)*a*du_dx)
     L1 = 0.5*(u(nx,:,:,1)-a)*(dp_dx - rho(nx,:,:)*a*du_dx)
     d1 = L2 + (L5+L1)/(a*a)
     drho_dt(nx,:,:) = drho_dt(nx,:,:) + d1

   ! compute the pressure source term
     Sp = 0.0
     do n=1,n_spec-1
        Sp = Sp + rr_r(:,:,n)*((1-gamma(nx,:,:))*(h_spec(:,:,n)-h_spec(:,:,n_spec))          &
                + gamma(nx,:,:)*pressure(nx,:,:)*mixMW(nx,:,:)*volum(nx,:,:)    &
                *(molwt_c(n)-molwt_c(n_spec)) )
     enddo

   ! update d1 and add it back to the continuity eqn.
!     now needs to be called by all procs since communication is done
!     call time_der( time, dT_dt, du_dt, dY_dt )
     L1 = L5 + rho(nx,:,:)*a*(du_dt)
     tmp = 0.0
     do n=1,n_spec-1
        tmp = tmp + dY_dt(:,:,n)*(molwt_c(n)-molwt_c(n_spec))
     enddo
     L2 = -Sp/(R*temp(nx,:,:)) + (gamma(nx,:,:)-1.0)/a**2*(L1+L5)             &
          + rho(nx,:,:)/temp(nx,:,:)*dT_dt + rho(nx,:,:)*mixMW(nx,:,:)*tmp
     d1 = L2 + (L5+L1)/a**2.0
     drho_dt(nx,:,:) = drho_dt(nx,:,:) - d1
  endif

  return

contains

  subroutine time_der( t, dT_dt, du_dt, dY_dt )
    use param_m, only : ny,nz
    use runtime_m, only : tstep
    use variables_m, only : u
    use turbulence_m, only : i_turbulence
    use frozenfeed_m, only: frozenfeed_uprime
    use temporalfeed_m, only: temporalfeed_uprime, i_temporalfeed, i_frozenfeed

    implicit none
    real, intent(in) :: t
    real, dimension(ny,nz), intent(out) :: dT_dt, du_dt
    real, dimension(ny,nz,n_spec), intent(out) :: dY_dt
    real, dimension(ny,nz,3) :: tmp
    real :: dt
    real :: x

    dT_dt = 0.0
    dY_dt = 0.0
    du_dt = 0.0
    if(i_turbulence == 1) then
      ! get time derivative of inlet velocity field.
      if(i_temporalfeed.eq.2)then
        call temporalfeed_uprime(t, du_dt)
      elseif(i_frozenfeed.eq.2)then
        call frozenfeed_uprime(t, du_dt)
      endif
    endif
    return
  end subroutine time_der

end subroutine inflowA_BCx

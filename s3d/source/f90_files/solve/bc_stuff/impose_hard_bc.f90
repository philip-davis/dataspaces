#include "globalDefines.h"
!=========================================================================================
  subroutine impose_hard_bc(q,yspecies,avmolwt)
!=========================================================================================
! specifies hard inlet conditions at left or right x-boundary for
! fluid velocity, temperature, and species
! note: density is NOT specified
!
! note: this is for subsonic only
!
! note: this routine acts on the q-vector only
!
! note: aliased work arrays are used locally only
!-----------------------------------------------------------------------------------------
  use param_m
  use topology_m
  use chemkin_m, only : molwt_c
  use bc_m, only : nrf_x0, nrf_xl, nrf_y0, nrf_yl, nrf_z0, nrf_zl
  use bc_m, only : qx_bc, qy_bc, qz_bc
  use thermchem_m, only : Ru, calc_inv_avg_mol_wt, mixEnth

  use turbulence_m, only : i_turbulence
  use runtime_m, only : time, time_accum
  use variables_m, only : u

  use reference_m, only : l_ref

  use frozenfeed_m, only: frozenfeed_u
  use temporalfeed_m, only: temporalfeed_u, temporalfeed_u_all, i_temporalfeed, i_frozenfeed, i_temporalspecies

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real q(nx,ny,nz,nvar_tot)
  real yspecies(nx,ny,nz,nsc+1)
  real avmolwt(nx,ny,nz)

! local declarations

  integer :: i, j, k, isc, n

  real :: enthmix, r_gas
  real kinetic_Ex(ny,nz), kinetic_Ey(nx,nz), kinetic_Ez(nx,ny)

! set fluctuating quantities as necessary
  if(i_turbulence==1) then
     if(nrf_x0 <= 0 ) then
!    James version
!        qx_bc(1,:,:,1:3) = getInletVelocity(time,1)
!    new James version
        if(i_temporalfeed.eq.2)then
          if(i_temporalspecies.eq.1)then  
            call temporalfeed_u_all(time_accum, qx_bc(1,:,:,1:3), qx_bc(1,:,:,6:5+nsc), qx_bc(1,:,:,5) ) !use variable 5 for temperature initially
          else    
            call temporalfeed_u(time_accum, qx_bc(1,:,:,1:3))
          endif
        elseif(i_frozenfeed.eq.2)then
          call frozenfeed_u(time_accum, qx_bc(1,:,:,1:3))
        endif
!%!    Evatt version
!%        call update_x_scan
!%        qx_bc(1,:,:,1:3) = getInletVelocity(x_scan,1)
     endif
  endif

!-----------------------------------------------------------------------------------------
! set species mass fractions at boundaries from stored values
!-----------------------------------------------------------------------------------------
! x-direction

  if( vary_in_x == 1 .and. periodic_x == 0 ) then
     if( xid==0 ) then
        if( nrf_x0 == 0 .or. nrf_x0 == -2 .or. nrf_x0 == -1 ) then
           do isc = 1, nsc
              yspecies(1,:,:,isc) = qx_bc(1,:,:,isc+5)
           enddo
        endif
     endif
     if( xid==xpes-1 ) then
        if( nrf_xl == 0 .or. nrf_xl == -2 .or. nrf_xl == -1 ) then
           do isc = 1, nsc
              yspecies(nx,:,:,isc) = qx_bc(2,:,:,isc+5)
           enddo
        endif
     endif
  endif

! y-direction

  if( vary_in_y == 1 .and. periodic_y == 0 ) then
     if( yid==0 ) then
        if( nrf_y0 == 0 .or. nrf_y0 == -2 ) then
           do isc = 1, nsc
              yspecies(:,1,:,isc) = qy_bc(:,1,:,isc+5)
           enddo
        endif
     endif
     if( yid==ypes-1 ) then
        if( nrf_yl == 0 .or. nrf_yl == -2 ) then
           do isc = 1, nsc
              yspecies(:,ny,:,isc) = qy_bc(:,2,:,isc+5)
           enddo
        endif
     endif
  endif

! z-direction

  if( vary_in_z == 1 .and. periodic_z == 0 ) then
     if( zid==0 ) then
        if( nrf_z0 == 0 .or. nrf_z0 == -2 ) then
           do isc = 1, nsc
              yspecies(:,:,1,isc) = qz_bc(:,:,1,isc+5)
           enddo
        endif
     endif
     if( zid==zpes-1 ) then
        if( nrf_zl == 0 .or. nrf_zl == -2 ) then
           do isc = 1, nsc
              yspecies(:,:,nz,isc) = qz_bc(:,:,2,isc+5)
           enddo
        endif
     endif
  endif
!-----------------------------------------------------------------------------------------
! calculate mixture (average) molecular weight

  call calc_inv_avg_mol_wt(yspecies,avmolwt)
!-----------------------------------------------------------------------------------------
! set boundaries in q-vector for x-direction (x=0)
!-----------------------------------------------------------------------------------------
  if( vary_in_x == 1 .and. periodic_x == 0 ) then

     if( xid==0 ) then
        select case( nrf_x0)
        case( 0, -2 )
           !-- velocity equations
           do isc = 1, 3
              q(1,:,:,isc) = q(1,:,:,4)*qx_bc(1,:,:,isc)
           enddo

           !-- species equations
           do isc = 1, nsc
              q(1,:,:,5+isc) = q(1,:,:,4)*qx_bc(1,:,:,5+isc)
           enddo
        case( -1 )
           !-- y and z velocity equations
           do isc = 2, 3
              q(1,:,:,isc) = q(1,:,:,4)*qx_bc(1,:,:,isc)
           enddo

           !-- species equations
           do isc = 1, nsc
              q(1,:,:,5+isc) = q(1,:,:,4)*qx_bc(1,:,:,5+isc)
           enddo

        case( -5, -6 )
           !-- velocity equations
           q(1,:,:,1:3) = 0.0

        end select

        select case( nrf_x0 )
        case( 0, -1, -2, -6 )
           !-- energy equation (based on stored temperature)
           do k = 1, nz
              do j = 1, ny
                 r_gas= avmolwt(1,j,k) * Ru
                 enthmix = mixEnth( yspecies(1,j,k,:), qx_bc(1,j,k,5) )
                 q(1,j,k,5)=q(1,j,k,4) * ( enthmix - r_gas*qx_bc(1,j,k,5) )  &
                      +0.5*q(1,j,k,4)*(qx_bc(1,j,k,1)**2+qx_bc(1,j,k,2)**2+qx_bc(1,j,k,3)**2)
              enddo
           enddo
        end select
     endif
!-----------------------------------------------------------------------------------------
! set boundaries in q-vector for x-direction (x=Lx)
!-----------------------------------------------------------------------------------------
     if( xid==xpes-1 ) then

        select case(nrf_xl)
        case( 0, -2 )
           !-- velocity equations
           do isc = 1, 3
              q(nx,:,:,isc) = q(nx,:,:,4)*qx_bc(2,:,:,isc)
           enddo

           !-- species equations
           do isc = 1, nsc
              q(nx,:,:,5+isc) = q(nx,:,:,4)*qx_bc(2,:,:,5+isc)
           enddo
        case( -1 )
           !-- y and z velocity equations
           do isc = 2, 3
              q(nx,:,:,isc) = q(nx,:,:,4)*qx_bc(2,:,:,isc)
           enddo

           !-- species equations
           do isc = 1, nsc
              q(nx,:,:,5+isc) = q(nx,:,:,4)*qx_bc(2,:,:,5+isc)
           enddo
        case( -5, -6 )
           !-- velocity equations
           q(nx,:,:,1:3) = 0.0

        end select

        select case( nrf_xl )
        case( 0, -1, -2, -6 )
           !-- energy equation (based on stored temperature)
           do k = 1, nz
              do j = 1, ny
                 r_gas= avmolwt(nx,j,k) * Ru
                 enthmix = mixEnth( yspecies(nx,j,k,:), qx_bc(2,j,k,5) )
                 q(nx,j,k,5)=q(nx,j,k,4) * ( enthmix - r_gas*qx_bc(2,j,k,5) )  &
                      +0.5*q(nx,j,k,4)*(qx_bc(2,j,k,1)**2+qx_bc(2,j,k,2)**2+qx_bc(2,j,k,3)**2)
              enddo
           enddo
        end select
     endif
  endif
!-----------------------------------------------------------------------------------------
! set boundaries in q-vector for y-direction (y=0)
!-----------------------------------------------------------------------------------------
  if( vary_in_y == 1 .and. periodic_y == 0 ) then

     if( yid==0 ) then
        select case( nrf_y0 )
        case( 0, -2 )
           !-- velocity equations
           do isc = 1, 3
              q(:,1,:,isc) = q(:,1,:,4)*qy_bc(:,1,:,isc)
           enddo

           !-- species equations
           do isc = 1, nsc
              q(:,1,:,5+isc) = q(:,1,:,4)*qy_bc(:,1,:,5+isc)
           enddo
        case( -5, -6 )
           !-- velocity equations
           q(:,1,:,1:3) = 0.0
        end select

        select case( nrf_y0 )
        case( 0, -2, -6 )
           !-- energy equation (based on stored temperature)
           do k = 1, nz
              do i = 1, nx
                 r_gas = avmolwt(i,1,k) * Ru
                 enthmix = mixEnth( yspecies(i,1,k,:), qy_bc(i,1,k,5) )
                 q(i,1,k,5)=q(i,1,k,4) * ( enthmix - r_gas*qy_bc(i,1,k,5) )  &
                      +0.5*q(i,1,k,4)*(qy_bc(i,1,k,1)**2+qy_bc(i,1,k,2)**2+qy_bc(i,1,k,3)**2)
              enddo
           enddo
        end select
     endif
!-----------------------------------------------------------------------------------------
! set boundaries in q-vector for y-direction (y=Ly)
!-----------------------------------------------------------------------------------------
     if( yid==ypes-1 ) then
        select case( nrf_yl )
        case( 0, -2 )
           !-- velocity equations
           do isc = 1, 3
              q(:,ny,:,isc) = q(:,ny,:,4)*qy_bc(:,2,:,isc)
           enddo

           !-- species equations
           do isc = 1, nsc
              q(:,ny,:,5+isc) = q(:,ny,:,4)*qy_bc(:,2,:,5+isc)
           enddo
        case( -5, -6 )
           !-- velocity equations
           q(:,ny,:,1:3) = 0.0
        end select

        select case( nrf_yl )
        case( 0, -2, -6 )
           !-- energy equation (based on stored temperature)
           do k = 1, nz
              do i = 1, nx
                 r_gas= avmolwt(i,ny,k) * Ru
                 enthmix = mixEnth( yspecies(i,ny,k,:), qy_bc(i,2,k,5) )
                 q(i,ny,k,5)=q(i,ny,k,4) * ( enthmix - r_gas*qy_bc(i,2,k,5) )  &
                      +0.5*q(i,ny,k,4)*(qy_bc(i,2,k,1)**2+qy_bc(i,2,k,2)**2+qy_bc(i,2,k,3)**2)
              enddo
           enddo
        end select
     endif
  endif
!-----------------------------------------------------------------------------------------
! set boundaries in q-vector for z-direction (z=0)
!-----------------------------------------------------------------------------------------
  if( vary_in_z == 1 .and. periodic_z == 0 ) then

     if (zid==0) then
        select case( nrf_z0 )
        case( 0, -2 )
           !-- velocity equations
           do isc = 1, 3
              q(:,:,1,isc) = q(:,:,1,4)*qz_bc(:,:,1,isc)
           enddo

           !-- species equations
           do isc = 1, nsc
              q(:,:,1,5+isc) = q(:,:,1,4)*qz_bc(:,:,1,5+isc)
           enddo
        case( -5, -6 )
           !-- velocity equations
           q(:,:,1,1:3) = 0.0
        end select

        select case( nrf_z0 )
        case( 0, -2, -6 )
           !-- energy equation (based on stored temperature)
           do j = 1, ny
              do i = 1, nx
                 r_gas= avmolwt(i,j,1) * Ru
                 enthmix = mixEnth( yspecies(i,j,1,:), qz_bc(i,j,1,5) )
                 q(i,j,1,5)=q(i,j,1,4) * ( enthmix - r_gas*qz_bc(i,j,1,5) )  &
                      +0.5*q(i,j,1,4)*(qz_bc(i,j,1,1)**2+qz_bc(i,j,1,2)**2+qz_bc(i,j,1,3)**2)
              enddo
           enddo
        end select
     endif
!-----------------------------------------------------------------------------------------
! set boundaries in q-vector for z-direction (z=Lz)
!-----------------------------------------------------------------------------------------
     if( zid==zpes-1 ) then
        select case( nrf_zl )
        case( 0, -2 )
           !-- velocity equations
           do isc = 1, 3
              q(:,:,nz,isc) = q(:,:,nz,4)*qz_bc(:,:,2,isc)
           enddo

           !-- species equations
           do isc = 1, nsc
              q(:,:,nz,5+isc) = q(:,:,nz,4)*qz_bc(:,:,2,5+isc)
           enddo
        case( -5, -6 )
           !-- velocity equations
           q(:,:,nz,1:3) = 0.0
        end select

        select case( nrf_zl )
        case( 0, -2, -6 )
           !-- energy equation (based on stored temperature)
           do j = 1, ny
              do i = 1, nx
                 r_gas= avmolwt(i,j,nz) * Ru
                 enthmix = mixEnth( yspecies(i,j,nz,:), qz_bc(i,j,2,5) )
                 q(i,j,nz,5)=q(i,j,nz,4) * ( enthmix - r_gas*qz_bc(i,j,2,5) )  &
                      +0.5*q(i,j,nz,4)*(qz_bc(i,j,2,1)**2+qz_bc(i,j,2,2)**2+qz_bc(i,j,2,3)**2)
              enddo
           enddo
        end select
     endif
  endif

  return
end subroutine impose_hard_bc


!=========================================================================================
!
! BUG FIX 25-NOV-2004 Evatt Hawkes
!
!     I rewrote this routine based on James's suggestion below.  During this process
!     I realised that this whoile routine was pointless as implemented.  I am leaving it
!     in here because parts will be needed  
!     
!
! BUG FIX 7-29-03  (James)  Chris Kennedy found this one.  In the wall bc's, we set
!         dp_dx = dtauxx_dx + dtauyx_dy + dtauzx_dz  by modifying the RHS.  This is
!         done for each direction.  When I first coded this, I forgot the tangential
!         stress terms, i.e. dtauyx_dy and dtau_zx_dz
!
!    I HAVE NOT FIXED THIS BUG.  The reasons are twofold:
!      1.  It is a pain, because I don't have routines written to compute
!          derivatives parallel to a boundary plane (only perpendicular)
!      2.  I think that this BC should actually modify the continuity equation.
!          That means casting it in terms of density rather than pressure.
!          The problem with specifying pressure (or its gradient) is that this
!          only directly affects the momentum equations, which are not really
!          evolved at a wall (momentum is zero).
!
subroutine wallBCCleanup( rhs, pressure, tau , u)
  use param_m
  use grid_m
  use topology_m, only : xid, yid, zid, xpes, ypes, zpes
  use bc_m, only : nrf_x0, nrf_xl, nrf_y0, nrf_yl, nrf_z0, nrf_zl
  implicit none

  real, intent(inout), dimension(nx,ny,nz,nvar_tot) :: rhs
  real, intent(in), dimension(nx,ny,nz) :: pressure
  real, intent(in), dimension(nx,ny,nz,3,3) :: tau
  real, intent(in), dimension(nx,ny,nz,3) :: u

  real, dimension(ny,nz) :: dtauxx_dx, dp_dx, dtauyx_dy, dtauzx_dz
  real, dimension(nx,nz) :: dtauyy_dy, dp_dy, dtauxy_dx, dtauzy_dz
  real, dimension(nx,ny) :: dtauzz_dz, dp_dz, dtauxz_dx, dtauyz_dy

  real, dimension(ny,nz) :: tmp_yz
  real, dimension(nx,nz) :: tmp_xz
  real, dimension(nx,ny) :: tmp_xy


  if( vary_in_x == 1 .and. periodic_x == 0 ) then
     if(xid==0) then
        select case( nrf_x0 )
        case( -5, -6 )

!          calculate needed derivatives and correction term

           call point_der1_x( tau(:,:,:,1,1), dtauxx_dx, 1, scale_1x )
           call point_der1_x( pressure, dp_dx, 1, scale_1x )
           tmp_yz(:,:) = dp_dx  - dtauxx_dx

           ! calc and remove d(tau_yx)/dy if necessary
           if(vary_in_y==1) then
             call deriv_inplane_1(tau(1,:,:,1,2),dtauyx_dy,1,ny,nz,1)
             tmp_yz(:,:) = tmp_yz(:,:) - dtauyx_dy
           endif

           ! calc and remove d(tau_zx)/dz if necessary
           if(vary_in_z==1) then
             call deriv_inplane_2(tau(1,:,:,1,3),dtauzx_dz,1,ny,nz,1)
             tmp_yz(:,:) = tmp_yz(:,:) - dtauzx_dz
           endif

!          adjust momentum equations
!          no slip wall condition
           rhs(1,:,:,1:3) = 0.0

!!          slip wall condition?
!           (rhs(1,:,:,2:3) unchanged?)
!           rhs(1,:,:,1) = rhs(1,:,:,1) + tmp_yz(:,:)

!          adjust energy equation           
           rhs(1,:,:,5) = rhs(1,:,:,5) + u(1,:,:,1)*tmp_yz(:,:)

        end select
     endif
     if( xid==xpes-1 ) then
        select case( nrf_xl )
        case( -5, -6 )
           call point_der1_x( tau(:,:,:,1,1), dtauxx_dx, nx, scale_1x )
           call point_der1_x( pressure, dp_dx, nx, scale_1x )
           tmp_yz(:,:) = dp_dx - dtauxx_dx

           ! calc and remove d(tau_yx)/dy if necessary
           if(vary_in_y==1) then
             call deriv_inplane_1(tau(nx,:,:,1,2),dtauyx_dy,1,ny,nz,1)
             tmp_yz(:,:) = tmp_yz(:,:) - dtauyx_dy
           endif

           ! calc and remove d(tau_zx)/dz if necessary
           if(vary_in_z==1) then
             call deriv_inplane_2(tau(nx,:,:,1,3),dtauzx_dz,1,ny,nz,1)
             tmp_yz(:,:) = tmp_yz(:,:) - dtauzx_dz
           endif

!          adjust momentum equations
!          no slip wall condition
           rhs(nx,:,:,1:3) = 0.0

!!          slip wall condition?
!           (rhs(nx,:,:,2:3) unchanged?)
!           rhs(nx,:,:,1) = rhs(nx,:,:,1) + tmp_yz(:,:)

!          adjust energy equation           
           rhs(nx,:,:,5) = rhs(nx,:,:,5) + u(nx,:,:,1)*tmp_yz(:,:)

        end select
     endif
  end if

  if( vary_in_y == 1 .and. periodic_y == 0 ) then
     if(yid==0) then
        select case( nrf_y0 )
        case( -5, -6 )
           call point_der1_y( tau(:,:,:,2,2), dtauyy_dy, 1, scale_1y )
           call point_der1_y( pressure, dp_dy, 1, scale_1y )
           tmp_xz(:,:) =  dp_dy - dtauyy_dy

           ! calc and remove d(tau_xy)/dx if necessary
           if(vary_in_x==1) then
             call deriv_inplane_1(tau(:,1,:,2,1),dtauxy_dx,2,nx,nz,1)
             tmp_xz(:,:) = tmp_xz(:,:) - dtauxy_dx
           endif

           ! calc and remove d(tau_zy)/dz if necessary
           if(vary_in_z==1) then
             call deriv_inplane_2(tau(:,1,:,2,3),dtauzy_dz,2,nx,nz,1)
             tmp_xz(:,:) = tmp_xz(:,:) - dtauzy_dz
           endif

!          adjust momentum equations
!          no slip wall condition
           rhs(:,1,:,1:3) = 0.0

!!          slip wall condition?
!           (rhs(:,1,:,1,3) unchanged?)
!           rhs(:,1,:,2) = rhs(:,1,:,2) + tmp_xz(:,:)

!          adjust energy equation           
           rhs(:,1,:,5) = rhs(:,1,:,5) + u(:,1,:,2)*tmp_xz(:,:)

        end select
     endif
     if(yid==ypes-1) then
        select case( nrf_yl )
        case( -5, -6 )

           call point_der1_y( tau(:,:,:,2,2), dtauyy_dy, ny, scale_1y )
           call point_der1_y( pressure, dp_dy, ny, scale_1y )
           tmp_xz(:,:) =  dp_dy - dtauyy_dy

           ! calc and remove d(tau_xy)/dx if necessary
           if(vary_in_x==1) then
             call deriv_inplane_1(tau(:,ny,:,2,1),dtauxy_dx,2,nx,nz,1)
             tmp_xz(:,:) = tmp_xz(:,:) - dtauxy_dx
           endif

           ! calc and remove d(tau_zy)/dz if necessary
           if(vary_in_z==1) then
             call deriv_inplane_2(tau(:,ny,:,2,3),dtauzy_dz,2,nx,nz,1)
             tmp_xz(:,:) = tmp_xz(:,:) - dtauzy_dz
           endif

!          adjust momentum equations
!          no slip wall condition
           rhs(:,ny,:,1:3) = 0.0

!!          slip wall condition?
!           (rhs(:,ny,:,1,3) unchanged?)
!           rhs(:,ny,:,2) = rhs(:,ny,:,2) + tmp_xz(:,:)

!          adjust energy equation           
           rhs(:,ny,:,5) = rhs(:,ny,:,5) + u(:,ny,:,2)*tmp_xz(:,:)

        end select
     endif
  end if

  if( vary_in_z == 1 .and. periodic_z == 0 ) then
     if(zid==0) then
        select case( nrf_z0 )
        case( -5, -6 )
           call point_der1_z( tau(:,:,:,3,3), dtauzz_dz, 1, scale_1z )
           call point_der1_z( pressure, dp_dz, 1, scale_1z )
           tmp_xy(:,:) =  dp_dy - dtauzz_dz

           ! calc and remove d(tau_xy)/dx if necessary
           if(vary_in_x==1) then
             call deriv_inplane_1(tau(:,:,1,3,1),dtauxz_dx,3,nx,ny,1)
             tmp_xy(:,:) = tmp_xy(:,:) - dtauxz_dx
           endif

           ! calc and remove d(tau_zy)/dz if necessary
           if(vary_in_z==1) then
             call deriv_inplane_2(tau(:,:,1,3,2),dtauyz_dy,3,nx,ny,1)
             tmp_xy(:,:) = tmp_xy(:,:) - dtauyz_dy
           endif

!          adjust momentum equations
!          no slip wall condition
           rhs(:,:,1,1:3) = 0.0

!!          slip wall condition?
!           (rhs(:,:,1,1:2) unchanged?)
!           rhs(:,:,1,3) = rhs(:,:,1,3) + tmp_xy(:,:)

!          adjust energy equation           
           rhs(:,:,1,5) = rhs(:,:,1,5) + u(:,:,1,3)*tmp_xy(:,:)

        end select
     endif
     if(zid==zpes-1) then
        select case( nrf_zl )
        case( -5, -6 )
           call point_der1_z( tau(:,:,:,3,3), dtauzz_dz, nz, scale_1z )
           call point_der1_z( pressure, dp_dz, nz, scale_1z )
           tmp_xy(:,:) =  dp_dy - dtauzz_dz

           ! calc and remove d(tau_xy)/dx if necessary
           if(vary_in_x==1) then
             call deriv_inplane_1(tau(:,:,nz,3,1),dtauxz_dx,3,nx,ny,1)
             tmp_xy(:,:) = tmp_xy(:,:) - dtauxz_dx
           endif

           ! calc and remove d(tau_zy)/dz if necessary
           if(vary_in_z==1) then
             call deriv_inplane_2(tau(:,:,nz,3,2),dtauyz_dy,3,nx,ny,1)
             tmp_xy(:,:) = tmp_xy(:,:) - dtauyz_dy
           endif

!          adjust momentum equations
!          no slip wall condition
           rhs(:,:,nz,1:3) = 0.0

!!          slip wall condition?
!           (rhs(:,:,nz,1:2) unchanged?)
!           rhs(:,:,nz,3) = rhs(:,:,nz,3) + tmp_xy(:,:)

!          adjust energy equation           
           rhs(:,:,nz,5) = rhs(:,:,nz,5) + u(:,:,nz,3)*tmp_xy(:,:)

        end select
     endif
  end if

end subroutine wallBCCleanup

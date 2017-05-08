#include "globalDefines.h"
!========================================================================================
! BUG FIX 6-26-03 bug fixed in bc_flux.  When the wall bcs were added, this condition
!         was not excluded from bc_flux, which deals with open bc's.  Thus, the boundary
!         was being overspecified (once in bc_flux, again in wallBcFlux).
!
! BUG FIX 6-20-03 in wall bc's.  At the x=Lx, y=Ly, z=Lz boundaries, there was a test
!         on nrf_x0, nrf_y0, nrf_z0 that should have been on nrf_xl, nrf_yl, nrf_zl
!
! VERSION 1.01
!
!    Revision info: (7-25-02)  Made by James Sutherland (sutherland@crsim.utah.edu)
!       Modified treatment of fluxes to impose conditions on derivatives of fluxes
!       rather than on the fluxes themselves.  Also fixed some ommissions on
!       stress tensor stuff...
!
! VERSION 1.00
!========================================================================================

subroutine bc_flux( u, tau, heatFlux, diffFlux )
  !-----------------------------------------------------------------------------
  !                     Author:  James Sutherland
  !                     Date:    May, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Impose boundary conditions on flux terms at ALL OPEN BOUNDARIES.
  !
  ! If you consider CLOSED BOUNDARIES, this routine should NOT be used!
  !______________________________________________________________________
  ! SUBSONIC INFLOW:
  !   X-Direction:        |   Y-Direction:        |   Z-Direction:
  !       tau_xx          |       tau_yy          |       tau_zz
  !_______________________|_______________________|______________________
  ! SUBSONIC OUTFLOW:
  !   X-Direction:        |   Y-Direction:        |  Z-Direction:
  !       tau_xy (tau_yx) |       tau_yx (tau_xy) |       tau_zx (tau_xz)
  !       tau_xz (tau_zx) |       tau_yz (tau_zy) |       tau_zy (tau_yz)
  !       heatFlux_x      |       heatFlux_y      |       heatFlux_z
  !       diffFlux_x      |       diffFlux_y      |       diffFlux_z
  !_______________________|_______________________|______________________
  ! SUPERSONIC INFLOW: do not specify any flux terms
  !______________________________________________________________________
  ! SUPERSONIC OUTFLOW:
  !   X-Direction:        |   Y-Direction:        |  Z-Direction:
  !       tau_xx          |       tau_yx (tau_xy) |       tau_zx (tau_xz)
  !       tau_xy (tau_yx) |       tay_yy          |       tau_zy (tau_yz)
  !       tau_xz (tau_zx) |       tau_yz (tau_zy) |       tau_zz
  !       heatFlux_x      |       heatFlux_y      |       heatFlux_z
  !       diffFlux_x      |       diffFlux_y      |       diffFlux_z
  !----------------------------------------------------------------------
  ! NOTE: Currently only the SUBSONIC conditions are implemented.
  !       If you want supersonic conditions, then you will need
  !       to enforce the conditions described above, and additionally
  !       modify the Nonreflecting boundary condition implementation.
  !-----------------------------------------------------------------------------
  use param_m, only : nx, ny, nz, n_spec,                       &
                      vary_in_x, vary_in_y, vary_in_z,          &
                      periodic_x, periodic_y, periodic_z
  use topology_m, only : xid, yid, zid, xpes, ypes, zpes
  use bc_m
  implicit none

  real, intent(in),    dimension(nx,ny,nz,3)        :: u
  real, intent(inout), dimension(nx,ny,nz,3,3)      :: tau
  real, intent(inout), dimension(nx,ny,nz,3)        :: heatFlux
  real, intent(inout), dimension(nx,ny,nz,n_spec,3) :: diffFlux
  integer :: i,j,k

! impose BC's in each direction in turn

!----------------------------------------------
! X DIRECTION BOUNDARY TREATMENT FOR FLUX TERMS
!----------------------------------------------
  if( vary_in_x==1 .and. periodic_x==0 ) then

     if( xid==0 .and. nrf_x0 >= -1 ) then

     ! LEFT X BOUNDARY (YZ PLANE AT X=0)

        do k=1,nz
           do j=1,ny
!!$              if( u(1,j,k,1) > 0.0 ) then       ! inflow condition
!!$                 tau(1,j,k,1,1) = 0.0           !   tau_xx
!!$              else                              ! outflow condition
!!$                 tau(1,j,k,1,2) = 0.0           !   tau_xy
!!$                 tau(1,j,k,2,1) = 0.0           !   tau_yx
!!$                 tau(1,j,k,1,3) = 0.0           !   tau_xz
!!$                 tau(1,j,k,3,1) = 0.0           !   tau_zx
!!$                 heatFlux(1,j,k,1) = 0.0        !   heatFlux_x
!!$                 diffFlux(1,j,k,:,1) = 0.0      !   diffFlux_x  for all species
!!$              endif
!!$
              if( u(1,j,k,1) > 0.0 ) then       
               !-- inflow condition
               !     d(tau_xx)/dx = 0.0
                 tau(1,j,k,1,1) = ( 18.*tau(2,j,k,1,1) - 9.*tau(3,j,k,1,1)    &
                      + 2.*tau(4,j,k,1,1) )/( 11.)
              else
               !-- outflow condition
               !     d(tau_xy)/dx = 0.0
               !     d(tau_yx)/dx = 0.0
               !     d(tau_xz)/dx = 0.0
               !     d(tau_zx)/dx = 0.0
               !     d(heatFlux_x)/dx = 0.0
               !     d(diffFlux_x)/dx = 0.0
                 tau(1,j,k,1,2) = ( 18.*tau(2,j,k,1,2) - 9.*tau(3,j,k,1,2)    &
                      + 2.*tau(4,j,k,1,2) )/( 11.)
                 tau(1,j,k,2,1) = ( 18.*tau(2,j,k,2,1) - 9.*tau(3,j,k,2,1)    &
                      + 2.*tau(4,j,k,2,1) )/( 11.)

                 tau(1,j,k,1,3) = ( 18.*tau(2,j,k,1,3) - 9.*tau(3,j,k,1,3)    &
                      + 2.*tau(4,j,k,1,3) )/( 11.)
                 tau(1,j,k,3,1) = ( 18.*tau(2,j,k,3,1) - 9.*tau(3,j,k,3,1)    &
                      + 2.*tau(4,j,k,3,1) )/( 11.)

                 heatFlux(1,j,k,1) = ( 18.*heatFlux(2,j,k,1) - 9.*heatFlux(3,j,k,1)    &
                      + 2.*heatFlux(4,j,k,1) )/( 11.)

                 diffFlux(1,j,k,:,1) = ( 18.*diffFlux(2,j,k,:,1) - 9.*diffFlux(3,j,k,:,1)    &
                      + 2.*diffFlux(4,j,k,:,1) )/( 11.)
              endif
           enddo
        enddo
     endif

     if( xid==(xpes-1) .and. nrf_xl >= -1 ) then

      ! RIGHT X BOUNDARY (YZ PLANE AT X=Lx)

        do k=1,nz
           do j=1,ny
!!$              if( u(nx,j,k,1) < 0.0 ) then      ! inflow condition
!!$                 tau(nx,j,k,1,1) = 0.0          !   tau_xx
!!$              else                              ! outflow condition
!!$                 tau(nx,j,k,1,2) = 0.0          !   tau_xy
!!$                 tau(nx,j,k,2,1) = 0.0          !   tau_yx
!!$                 tau(nx,j,k,1,3) = 0.0          !   tau_xz
!!$                 tau(nx,j,k,3,1) = 0.0          !   tau_zx
!!$                 heatFlux(nx,j,k,1) = 0.0       !   heatFlux_x
!!$                 diffFlux(nx,j,k,:,1) = 0.0     !   diffFlux_x  for all species
!!$              endif
!!$
              if( u(nx,j,k,1) < 0.0 ) then
               !-- inflow condition
               !     d(tau_xx)/dx = 0.0
                 tau(nx,j,k,1,1) = ( -18.*tau(nx-1,j,k,1,1) + 9.*tau(nx-2,j,k,1,1)      &
                      - 2.*tau(nx-3,j,k,1,1) )/(-11.)
              else
               !-- outflow condition
               !     d(tau_xy)/dx = 0.0
               !     d(tau_yx)/dx = 0.0
               !     d(tau_xz)/dx = 0.0
               !     d(tau_zx)/dx = 0.0
               !     d(heatFlux_x)/dx = 0.0
               !     d(diffFlux_x)/dx = 0.0
                 tau(nx,j,k,1,2) = ( -18.*tau(nx-1,j,k,1,2) + 9.*tau(nx-2,j,k,1,2)      &
                      - 2.*tau(nx-3,j,k,1,2) )/(-11.)
                 tau(nx,j,k,2,1) = ( -18.*tau(nx-1,j,k,2,1) + 9.*tau(nx-2,j,k,2,1)      &
                      - 2.*tau(nx-3,j,k,2,1) )/(-11.)

                 tau(nx,j,k,1,3) = ( -18.*tau(nx-1,j,k,1,3) + 9.*tau(nx-2,j,k,1,3)      &
                      - 2.*tau(nx-3,j,k,1,3) )/(-11.)
                 tau(nx,j,k,3,1) = ( -18.*tau(nx-1,j,k,3,1) + 9.*tau(nx-2,j,k,3,1)      &
                      - 2.*tau(nx-3,j,k,3,1) )/(-11.)

                 heatFlux(nx,j,k,1) = ( -18.*heatFlux(nx-1,j,k,1) + 9.*heatFlux(nx-2,j,k,1)      &
                      - 2.*heatFlux(nx-3,j,k,1) )/(-11.)

                 diffFlux(nx,j,k,:,1) = ( -18.*diffFlux(nx-1,j,k,:,1) + 9.*diffFlux(nx-2,j,k,:,1)      &
                      - 2.*diffFlux(nx-3,j,k,:,1) )/(-11.)
              endif
           enddo
        enddo
     endif

  endif

!----------------------------------------------
! Y DIRECTION BOUNDARY TREATMENT FOR FLUX TERMS
!----------------------------------------------
  if( vary_in_y==1 .and. periodic_y==0 ) then

     if( yid==0 .and. nrf_y0>=0 ) then

      ! LEFT Y BOUNDARY (XZ PLANE AT Y=0)

        do k=1,nz
           do i=1,nx
!!$              if( u(i,1,k,2) > 0.0 ) then       ! inflow condition
!!$                 tau(i,1,k,2,2) = 0.0           !    tau_yy
!!$              else                              ! outflow condition
!!$                 tau(i,1,k,1,2) = 0.0           !    tau_xy
!!$                 tau(i,1,k,2,1) = 0.0           !    tau_yx
!!$                 tau(i,1,k,2,3) = 0.0           !    tau_yz
!!$                 tau(i,1,k,3,2) = 0.0           !    tau_zy
!!$                 heatFlux(i,1,k,2) = 0.0        !    heatFlux_y
!!$                 diffFlux(i,1,k,:,2) = 0.0      !    diffFlux_y  for all species
!!$              endif

              if( u(i,1,k,2) > 0.0 ) then       
               !-- inflow condition
               !     d(tau_yy)/dy = 0.0
                 tau(i,1,k,2,2) = ( 18.*tau(i,2,k,2,2) - 9.*tau(i,3,k,2,2)    &
                      + 2.*tau(i,4,k,2,2) )/( 11.)
              else
               !-- outflow condition
               !     d(tau_xy)/dy = 0.0
               !     d(tau_yx)/dy = 0.0
               !     d(tau_zy)/dy = 0.0
               !     d(tau_yz)/dy = 0.0
               !     d(heatFlux_y)/dy = 0.0
               !     d(diffFlux_y)/dy = 0.0
                 tau(i,1,k,1,2) = ( 18.*tau(i,2,k,1,2) - 9.*tau(i,3,k,1,2)    &
                      + 2.*tau(i,4,k,1,2) )/( 11.)
                 tau(i,1,k,2,1) = ( 18.*tau(i,2,k,2,1) - 9.*tau(i,3,k,2,1)    &
                      + 2.*tau(i,4,k,2,1) )/( 11.)

                 tau(i,1,k,2,3) = ( 18.*tau(i,2,k,2,3) - 9.*tau(i,3,k,2,3)    &
                      + 2.*tau(i,4,k,2,3) )/( 11.)
                 tau(i,1,k,3,2) = ( 18.*tau(i,2,k,3,2) - 9.*tau(i,3,k,3,2)    &
                      + 2.*tau(i,4,k,3,2) )/( 11.)

                 heatFlux(i,1,k,2) = ( 18.*heatFlux(i,2,k,2) - 9.*heatFlux(i,3,k,2)    &
                      + 2.*heatFlux(i,4,k,2) )/( 11.)

                 diffFlux(i,1,k,:,2) = ( 18.*diffFlux(i,2,k,:,2) - 9.*diffFlux(i,3,k,:,2)    &
                      + 2.*diffFlux(i,4,k,:,2) )/( 11.)
              endif
           enddo
        enddo
     endif

     if( yid==ypes-1 .and. nrf_yl>=0 ) then

      ! RIGHT Y BOUNDARY (XZ PLANE AT Y=Ly)

        do k=1,nz
           do i=1,nx
!!$              if( u(i,ny,k,2) < 0.0 ) then      ! inflow condition
!!$                 tau(i,ny,k,2,2) = 0.0          !    tau_yy
!!$              else                              ! outflow condition
!!$                 tau(i,ny,k,1,2) = 0.0          !    tau_xy
!!$                 tau(i,ny,k,2,1) = 0.0          !    tau_yx
!!$                 tau(i,ny,k,2,3) = 0.0          !    tau_yz
!!$                 tau(i,ny,k,3,2) = 0.0          !    tau_zy
!!$                 heatFlux(i,ny,k,2) = 0.0       !    heatFlux_y
!!$                 diffFlux(i,ny,k,:,2) = 0.0     !    diffFlux_y  for all species
!!$              endif

              if( u(i,ny,k,2) < 0.0 ) then       
               !-- inflow condition
               !     d(tau_xx)/dx = 0.0
                 tau(i,ny,k,2,2) = ( 18.*tau(i,ny-1,k,2,2) - 9.*tau(i,ny-2,k,2,2)    &
                      + 2.*tau(i,ny-3,k,2,2) )/( 11.)
              else
               !-- outflow condition
               !     d(tau_xy)/dx = 0.0
               !     d(tau_yx)/dx = 0.0
               !     d(tau_yz)/dx = 0.0
               !     d(tau_zy)/dx = 0.0
               !     d(heatFlux_x)/dx = 0.0
               !     d(diffFlux_x)/dx = 0.0
                 tau(i,ny,k,1,2) = ( 18.*tau(i,ny-1,k,1,2) - 9.*tau(i,ny-2,k,1,2)    &
                      + 2.*tau(i,ny-3,k,1,2) )/( 11.)
                 tau(i,ny,k,2,1) = ( 18.*tau(i,ny-1,k,2,1) - 9.*tau(i,ny-2,k,2,1)    &
                      + 2.*tau(i,ny-3,k,2,1) )/( 11.)

                 tau(i,ny,k,2,3) = ( 18.*tau(i,ny-1,k,2,3) - 9.*tau(i,ny-2,k,2,3)    &
                      + 2.*tau(i,ny-3,k,2,3) )/( 11.)
                 tau(i,ny,k,3,2) = ( 18.*tau(i,ny-1,k,3,2) - 9.*tau(i,ny-2,k,3,2)    &
                      + 2.*tau(i,ny-3,k,3,2) )/( 11.)

                 heatFlux(i,ny,k,2) = ( 18.*heatFlux(i,ny-1,k,2) - 9.*heatFlux(i,ny-2,k,2)    &
                      + 2.*heatFlux(i,ny-3,k,2) )/( 11.)

                 diffFlux(i,ny,k,:,2) = ( 18.*diffFlux(i,ny-1,k,:,2) - 9.*diffFlux(i,ny-2,k,:,2)    &
                      + 2.*diffFlux(i,ny-3,k,:,2) )/( 11.)
              endif

           enddo
        enddo
     endif

  endif

!----------------------------------------------
! Z DIRECTION BOUNDARY TREATMENT FOR FLUX TERMS
!----------------------------------------------
  if( vary_in_z==1 .and. periodic_z==0 ) then

     if( zid==0 .and. nrf_z0>=0 ) then

      ! LEFT Z BOUNDARY (XY PLANE AT Z=0)

        do j=1,ny
           do i=1,nx
!!$              if( u(i,j,1,3) > 0.0 ) then       ! inflow condition
!!$                 tau(i,j,1,3,3) = 0.0           !    tau_zz
!!$              else                              ! outflow condition
!!$                 tau(i,j,1,1,3) = 0.0           !    tau_xz
!!$                 tau(i,j,1,3,1) = 0.0           !    tau_zx
!!$                 tau(i,j,1,2,3) = 0.0           !    tau_yz
!!$                 tau(i,j,1,3,2) = 0.0           !    tau_zy
!!$                 heatFlux(i,j,1,3) = 0.0        !    heatFlux_z
!!$                 diffFlux(i,j,1,:,3) = 0.0      !    diffFlux_z  for all species
!!$              endif

              if( u(i,j,1,3) > 0.0 ) then       
               !-- inflow condition
               !     d(tau_zz)/dz = 0.0
                 tau(i,j,1,3,3) = ( 18.*tau(i,j,2,3,3) - 9.*tau(i,j,3,3,3)    &
                      + 2.*tau(i,j,4,3,3) )/( 11.)
              else
               !-- outflow condition
               !     d(tau_xy)/dz = 0.0
               !     d(tau_yx)/dz = 0.0
               !     d(heatFlux_z)/dz = 0.0
               !     d(diffFlux_z)/dz = 0.0
                 tau(i,j,1,1,3) = ( 18.*tau(i,j,2,1,3) - 9.*tau(i,j,3,1,3)    &
                      + 2.*tau(i,j,4,1,3) )/( 11.)
                 tau(i,j,1,3,1) = ( 18.*tau(i,j,2,3,1) - 9.*tau(i,j,3,3,1)    &
                      + 2.*tau(i,j,4,3,1) )/( 11.)

                 tau(i,j,1,2,3) = ( 18.*tau(i,j,2,2,3) - 9.*tau(i,j,3,2,3)    &
                      + 2.*tau(i,j,4,2,3) )/( 11.)
                 tau(i,j,1,3,2) = ( 18.*tau(i,j,2,3,2) - 9.*tau(i,j,3,3,2)    &
                      + 2.*tau(i,j,4,3,2) )/( 11.)

                 heatFlux(i,j,1,3) = ( 18.*heatFlux(i,j,2,3) - 9.*heatFlux(i,j,3,3)    &
                      + 2.*heatFlux(i,j,4,3) )/( 11.)

                 diffFlux(i,j,1,:,3) = ( 18.*diffFlux(i,j,2,:,3) - 9.*diffFlux(i,j,3,:,3)    &
                      + 2.*diffFlux(i,j,4,:,3) )/( 11.)
              endif
           enddo
        enddo
     endif

     if( zid==zpes-1 .and. nrf_zl>=0 ) then

      ! RIGHT Z BOUNDARY (XY PLANE AT Z=Lz)

        do j=1,ny
           do i=1,nx
!!$              if( u(i,j,nz,3) < 0.0 ) then      ! inflow condition
!!$                 tau(i,j,nz,3,3) = 0.0          !    tau_zz
!!$              else                              ! outflow condition
!!$                 tau(i,j,nz,1,3) = 0.0          !    tau_xz
!!$                 tau(i,j,nz,3,1) = 0.0          !    tau_zx
!!$                 tau(i,j,nz,2,3) = 0.0          !    tau_yz
!!$                 tau(i,j,nz,3,2) = 0.0          !    tau_zy
!!$                 heatFlux(i,j,nz,3) = 0.0       !    heatFlux_z
!!$                 diffFlux(i,j,nz,:,3) = 0.0     !    diffFlux_z  for all species
!!$              endif

              if( u(i,j,nz,3) < 0.0 ) then       
               !-- inflow condition
               !     d(tau_zz)/dz = 0.0
                 tau(i,j,nz,3,3) = ( 18.*tau(i,j,nz-1,3,3) - 9.*tau(i,j,nz-2,3,3)    &
                      + 2.*tau(i,j,nz-3,3,3) )/( 11.)
              else
               !-- outflow condition
               !     d(tau_xy)/dz = 0.0
               !     d(tau_yx)/dz = 0.0
               !     d(heatFlux_z)/dz = 0.0
               !     d(diffFlux_z)/dz = 0.0
                 tau(i,j,nz,1,3) = ( 18.*tau(i,j,nz-1,1,3) - 9.*tau(i,j,nz-2,1,3)    &
                      + 2.*tau(i,j,nz-3,1,3) )/( 11.)
                 tau(i,j,nz,3,1) = ( 18.*tau(i,j,nz-1,3,1) - 9.*tau(i,j,nz-2,3,1)    &
                      + 2.*tau(i,j,nz-3,3,1) )/( 11.)

                 tau(i,j,nz,2,3) = ( 18.*tau(i,j,nz-1,2,3) - 9.*tau(i,j,nz-2,2,3)    &
                      + 2.*tau(i,j,nz-3,2,3) )/( 11.)
                 tau(i,j,nz,3,2) = ( 18.*tau(i,j,nz-1,3,2) - 9.*tau(i,j,nz-2,3,2)    &
                      + 2.*tau(i,j,nz-3,3,2) )/( 11.)

                 heatFlux(i,j,nz,3) = ( 18.*heatFlux(i,j,nz-1,3) - 9.*heatFlux(i,j,nz-2,3)    &
                      + 2.*heatFlux(i,j,nz-3,3) )/( 11.)

                 diffFlux(i,j,nz,:,3) = ( 18.*diffFlux(i,j,nz-1,:,3) - 9.*diffFlux(i,j,nz-2,:,3)    &
                      + 2.*diffFlux(i,j,nz-3,:,3) )/( 11.)
              endif
           enddo
        enddo
     endif

  endif

  return
end subroutine bc_flux

!=========================================================================================

subroutine wallBCFlux( heatFlux, specFlux, tau, pressure, temp, rho, mw )

  use param_m !, only : nx,ny,nz,n_spec, vary_in_x, vary_in_y, vary_in_z, periodic_x, periodic_y, periodic_z
  use bc_m, only : nrf_x0, nrf_xl, nrf_y0, nrf_yl, nrf_z0, nrf_zl
  use topology_m, only: xid, yid, zid, xpes, ypes, zpes

  implicit none

  real, intent(inout), dimension(nx,ny,nz,3)        :: heatFlux
  real, intent(inout), dimension(nx,ny,nz,n_spec,3) :: specFlux
  real, intent(in), dimension(nx,ny,nz,3,3)         :: tau
  real, intent(inout), dimension(nx,ny,nz)          :: pressure, rho
  real, intent(inout), dimension(nx,ny,nz)          :: temp, mw

  real, dimension(ny,nz) :: dtau_dx
  real, dimension(nx,nz) :: dtau_dy
  real, dimension(nx,ny) :: dtau_dz

  integer :: i,j,k

!-- X Direction

  if( vary_in_x == 1 .and. periodic_x == 0 ) then
    if (xid .eq. 0) then
     select case( nrf_x0 )
     case( -5, -6 )

      !-- zero the species diffusive flux normal to the boundary.
        specFlux(1,:,:,:,1) = 0.0

      !-- if we have an adiabatic wall, zero the normal heat flux.
        if(nrf_x0 == -5) heatFlux(1,:,:,1) = 0.0

     end select
    end if

    if (xid .eq. xpes-1) then
     select case( nrf_xl )
     case( -5, -6 )

      !-- zero the species diffusive flux normal to the boundary.
        specFlux(nx,:,:,:,1) = 0.0

      !-- if we have an adiabatic wall, zero the normal heat flux.
        if(nrf_xl == -5) heatFlux(nx,:,:,1) = 0.0

     end select
    endif
  endif

!-- Y Direction

  if( vary_in_y == 1 .and. periodic_y == 0 ) then
    if(yid .eq. 0) then
     select case( nrf_y0 )
     case( -5, -6 )

      !-- zero the species diffusive flux normal to the boundary.
        specFlux(:,1,:,:,2) = 0.0

      !-- if we have an adiabatic wall, zero the normal heat flux.
        if(nrf_y0 == -5) heatFlux(:,1,:,2) = 0.0

     end select
    end if

    if(yid .eq. ypes-1) then
     select case( nrf_yl )
     case( -5, -6 )

      !-- zero the species diffusive flux normal to the boundary.
        specFlux(:,ny,:,:,2) = 0.0

      !-- if we have an adiabatic wall, zero the normal heat flux.
        if(nrf_yl == -5) heatFlux(:,ny,:,2) = 0.0

     end select
    end if
  endif

!-- Z Direction

  if( vary_in_z == 1 .and. periodic_z == 0 ) then
    if(zid .eq. 0) then
     select case( nrf_z0 )
     case( -5, -6 )

      !-- zero the species diffusive flux normal to the boundary.
        specFlux(:,:,1,:,3) = 0.0

      !-- if we have an adiabatic wall, zero the normal heat flux.
        if(nrf_z0 == -5) heatFlux(:,:,1,3) = 0.0

     end select
    end if

    if(zid .eq. zpes-1) then
     select case( nrf_zl )
     case( -5, -6 )

      !-- zero the species diffusive flux normal to the boundary.
        specFlux(:,:,nz,:,3) = 0.0

      !-- if we have an adiabatic wall, zero the normal heat flux.
        if(nrf_zl == -5) heatFlux(:,:,nz,3) = 0.0

     end select
    end if
  endif

  return
end subroutine wallBCFlux

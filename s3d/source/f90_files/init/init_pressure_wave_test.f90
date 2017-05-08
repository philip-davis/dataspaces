#include "globalDefines.h"
!$Id: init_pressure_wave_test.f90,v 1.1.1.1.10.1 2006/04/04 01:37:24 rsankar Exp $
!========================================================================================
  subroutine initialize_pressure_wave_test( io, yspecies, temp, pressure, u )
!========================================================================================
! This subroutine initializes the following quantities:                        !
!                                                                              !
!   yspecies - Species mass fractions                                          !
!   temp     - Temperature                                                     !
!   pressure - pressure field                                                  !
!   u        - Mean velocity field                                             !
!                                                                              !
! It is currently set up to do a gaussian temperature spike in an otherwise    ! 
! quiescent field to initialize a pressure wave.                               !
!----------------------------------------------------------------------------------------
  use param_m
  use topology_m
  use grid_m, only : x, y, z, xmin, xmax, ymin, ymax, zmin, zmax
  use reference_m
  use chemkin_m, only : species_name, molwt_c

  use frozenfeed_m, only : vel_bar, feedVel
  use work_m, only : gauss => work1_1

  use thermchem_m, only : p0, rho0, Ru
  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real, intent(inout), dimension(nx,ny,nz) :: temp, pressure
  real, intent(inout), dimension(nx,ny,nz,n_spec) :: yspecies
  real, intent(inout), dimension(nx,ny,nz,3) :: u

  integer, intent(in) :: io

! local declarations

  integer :: i,j,k,L

  real :: maxTemp = 900,       ambientTemp = 900
  real :: ambient_avmolwt
  real :: variance
  real :: maxP = 1.5,          ambientP = 1.0  ! in atm

  real :: maxPt(3)
  real :: junk, facx, facy, facz, max, max_g

  real, parameter :: pi = 3.14159265358979
  character*2 ext

!----------------------------------------------------------------------------------------
! write header

  if(myid == 0) then
    write(io,*) 'initializing pressure wave test...'
    write(io,*)
  endif
!-------------------------------------------------------------------------------------
! set species for air

  yspecies = 0.0
  do L=1,n_spec,1
    if(trim(species_name(L)).eq.'o2'.or. trim(species_name(L)).eq.'O2') then
      yspecies(:,:,:,L)=0.233
      if(myid == 0) then
        write(io,*) 'initializing species', L, 'as 0.233'
        write(io,*)
      endif
    endif
    if(trim(species_name(L)).eq.'n2' .or. trim(species_name(L)).eq.'N2') then
      yspecies(:,:,:,L)=0.767
      if(myid == 0) then
        write(io,*) 'initializing species', L, 'as 0.767'
        write(io,*)
      endif
    endif
  enddo
!-------------------------------------------------------------------------------------
! set mean velocities

  u(:,:,:,1) = 0.0                ! mean u-velocity set to zero
  u(:,:,:,2) = 0.0                ! mean v-velocity set to zero
  u(:,:,:,3) = 0.0                ! mean w-velocity set to zero

!-------------------------------------------------------------------------------------
! set mean convective velocity for turbulence feeding. See "feedTurb_m.f90"
! these variables MUST be set at RESTARTS

  feedVel = maxval(u(:,:,:,1))          ! scanning velocity
  allocate(vel_bar(ny,nz))
  vel_bar = u(1,:,:,1)                  ! mean velocity at inlet plane
  ! ideally, vel_bar is uniform and feedVel==vel_bar

!-------------------------------------------------------------------------------------
! set temperature

  ambientTemp = ambientTemp / t_ref
  maxTemp = maxTemp / t_ref

! set center of spike to center of domain
! BUG FIX:ENH 11-OCT-2010. 
  maxPt = 0.0
  if (vary_in_x == 1)   maxPt(1) = (xmax+xmin) / 2. !(xmax-xmin) / 2.
  if (vary_in_y == 1)   maxPt(2) = (ymax+ymin) / 2. !(ymax-ymin) / 2.
  if (vary_in_z == 1)   maxPt(3) = (zmax+zmin) / 2. !(zmax-zmin) / 2.

! set the variance

  variance = 0.0
  if (vary_in_x == 1)   variance = variance + (xmax-maxPt(1))**2
  if (vary_in_y == 1)   variance = variance + (ymax-maxPt(2))**2
  if (vary_in_z == 1)   variance = variance + (zmax-maxPt(3))**2
  variance = 0.01*variance

! compute temperature using a gaussian distribution centered at the middle of the domain

  facx = 0.0
  facy = 0.0
  facz = 0.0

  do k=1,nz
     do j=1,ny
        do i=1,nx

           if (vary_in_x == 1)   facx = x(i)-maxPt(1)
           if (vary_in_y == 1)   facy = y(j)-maxPt(2)
           if (vary_in_z == 1)   facz = z(k)-maxPt(3)

           facy = 0.0d0; facz=0.0d0;

           junk = sqrt( facx**2 + facy**2 + facz**2 )

           gauss(i,j,k) = 1/sqrt(2.0*pi*variance) * exp(-junk**2 / (2.0*variance))

        enddo
     enddo
  enddo

  max = maxval(gauss)

  call MPI_Allreduce(max,max_g,1,MPI_REAL8,MPI_MAX,gcomm,ierr)

  gauss = gauss / max_g

  temp = ambientTemp + (maxTemp-ambientTemp) * gauss
!-------------------------------------------------------------------------------------
! set pressure


  pressure = (ambientP + (maxP-ambientP) * gauss) * pres_atm / p_ref
  p0 = ambientP * pres_atm / p_ref

  ! Ambient density
  !-- compute inverse of mixture molecular weight
    L = n_spec
    ambient_avmolwt = yspecies(1,1,1,L) * molwt_c(L)
    do L = 1, n_spec-1
       ambient_avmolwt = ambient_avmolwt + yspecies(1,1,1,L) * molwt_c(L)
    enddo
  rho0=p0/(Ru*ambientTemp*ambient_avmolwt)
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
      write(io,*) 'Pressure extents for root:', minval(pressure), maxval(pressure)
    call write_header(io,'-')
    call flush(io)
  endif

!-------------------------------------------------------------------------------------
  end subroutine initialize_pressure_wave_test

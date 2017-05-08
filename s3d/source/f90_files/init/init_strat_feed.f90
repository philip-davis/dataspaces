#include "globalDefines.h"
!----------------------------------------------------------------------
  subroutine initialize_strat_feed &
    ( io, yspecies, temp, pressure, u, half_width, decay_factor )
!----------------------------------------------------------------------
! Written by Ramanan Sankaran
! Modified by Ed Richardson for stratified cases.
! To initialize the bunsen feed box. 
! WARNING, THIS METHOD DOES NOT GUARANTEE THAT ENTHALPY IS A STRAIGHT LINE.
! TO DO THAT YOU NEED TO FIND ENTHALPY OF THE TWO UNREACTED MIXTURES
! AND USE MIXTURE FRACTION AND A STRAIGHT LINE TO FIND THE ENTHALPY
! AT EVERY POINT (NEGLECTING K.E. IS PROBABLY OK) THEN FIND TEMPERATURE
! FROM THAT. BE CAREFUL IF YOU DO SINCE ZETA IS NOT THE SAME AS
! MIXTURE FRACTION
!
! NB: ZETA=(rho*xi-rho1*xi1)/(rho2*xi2-rho1*xi1)
! this is necessary to ensure that the overall equivalence ratio is 
! as desired.
!
!----------------------------------------------------------------------
  use param_m
  use topology_m
  use grid_m, only : x, y, z, xmin, xmax, ymin, ymax, zmin, zmax
  use zclookup_m,  only : zclookup,zclookup_1val,calc_zrho0,initialize_zclookup
  use reference_m
  use chemkin_m, only : species_name
  use thermchem_m, only : avmolwt, calc_inv_avg_mol_wt

  use frozenfeed_m, only : vel_bar, feedVel

  implicit none
!----------------------------------------------------------------------
  real, intent(inout), dimension(nx,ny,nz) :: temp, pressure
  real, intent(inout), dimension(nx,ny,nz,n_spec) :: yspecies
  real, intent(inout), dimension(nx,ny,nz,3) :: u
  real, intent(out) :: half_width, decay_factor

  integer, intent(in) :: io

  character*100 :: filename
  integer, parameter :: io_bunsen=204

  real u_inlet, Sl_guess, thickness, press_in, co_flow
  real s, vel, dens
  real rhou_inlet(ny), rho_inlet(ny)
  real rhou_inlet_yz(ny,nz),rho_inlet_yz(ny,nz)
  real, dimension(nx,ny,nz) :: c,zeta
  real mix_loc_up,mix_loc_down,zthick

!  real sin_alpha, tan_alpha
  real sigma, fact1
  real flame_locy, flame_locy_up, flame_locy_down
  real fade_loc_up,fade_loc_down,fadethick
  real slot_locy_up, slot_locy_down
  integer i, j, k
  real :: vel_bar_xinlet(ny, nz)

  real :: rhofac(nz)
  real :: rho0_mid,rhoL_mid,rho0_k,rhoL_k
 
  real :: ceta(ny),rho0(ny),rhoM(ny),rho1(ny),uvel0(ny),uvel1(ny)
  real :: ysp(n_spec)
  real :: t

!----------------------------------------------------------------------
! write header

  if(myid == 0) then
    call write_header(io,'-')
    write(io,*) 'initializing bunsen flame ....'
    write(io,*)
  endif
!----------------------------------------------------------------------
! Read the input file
  filename = '../input/strat.in'
  call inquire_about_input_file(filename, io)

  if(myid .eq. 0) then
    open(unit=io_bunsen, file = trim(filename), status = 'old')

    !header
    read(io_bunsen, *)

    !slot half width (input in mm)
    read(io_bunsen, *) half_width
      !convert to SI
      half_width = half_width*1e-3

    !inflow velocity in m/s
    read(io_bunsen, *) u_inlet

    !coflow velocity in m/s
    read(io_bunsen, *) co_flow

    !Flame speed guess in m/s
    read(io_bunsen, *) sl_guess

    !Flame thickness in mm
    read(io_bunsen, *) thickness
      !convert to SI
      thickness = thickness*1e-3

    !Co-flow profile decays to zero using this factor
    !Fraction of h, half-width. Non-dimensionalization is not needed.
    read(io_bunsen, *) decay_factor

    !Pressure at inlet 
    read(io_bunsen, *) press_in

    close(io_bunsen)

    !nondimensionalize
    half_width = half_width/l_ref
    u_inlet = u_inlet/a_ref
    co_flow = co_flow/a_ref
    sl_guess = sl_guess/a_ref
    thickness = thickness/l_ref
    press_in  = press_in*pres_atm/p_ref
  end if

! broadcast input data
  call MPI_Bcast(half_width,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(u_inlet,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(co_flow,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(sl_guess,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(thickness,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(decay_factor,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(press_in,1,MPI_REAL8,0,gcomm,ierr)

! thickness equals 2sigma for tanhprof

  sigma = thickness/2.0

!set angles
!  sin_alpha=sl_guess/u_inlet
!  tan_alpha=sl_guess/sqrt(u_inlet**2-sl_guess**2)

! set the c profile

  do i = 1, nx
!    flame_locy = half_width - x(i)*tan_alpha
    flame_locy = half_width
    !Push flame_locy outside so that the flame begins outside h.
    flame_locy = flame_locy+3.0*sigma
    flame_locy_up = 0.5*(ymax+ymin) + flame_locy
    flame_locy_down = 0.5*(ymax+ymin) - flame_locy

!    flame_locy_up=0.5*(ymax+ymin) + half_width
!    flame_locy_down=0.5*(ymax+ymin) - half_width

    do j = 1, ny
      c(i,j,1) = 1.0 -  &
                 0.25* (1.0 - tanh( (y(j)-flame_locy_up)/sigma))* &
                       (1.0 + tanh( (y(j)-flame_locy_down)/sigma))
    end do
  end do
  ceta(:) = c(1,:,1)

  if( nz .gt. 1)  then
    do k = 2, nz
      c(:,:,k) = c(:,:,1)
    end do
  end if

! set the zeta profile
! this is two tanh steps at z=1/4 and z=3/4, so that 
! zeta =1 at z=0 and zeta=1 at z=0.5

  if(myid.eq.0)write(*,*)'NZ = ',NZ


  zeta = 1.0
!  zthick = 0.05*(zmax-zmin)
  zthick = half_width/2.0
  mix_loc_up = zmin+0.75*(zmax-zmin)
  mix_loc_down = zmin+0.25*(zmax-zmin)
  fade_loc_up = ymin+0.9*(ymax-ymin)
  fade_loc_down = ymin+0.1*(ymax-ymin)
  fadethick = 0.02*(ymax-ymin)
  if( nz .gt. 1) then
    do  k = 1, nz
      do j = 1, ny
        zeta(1,j,k) = 1.0 -  &
                    0.25* (1.0 - tanh( (z(k)-mix_loc_up)/zthick))*  &
                          (1.0 + tanh( (z(k)-mix_loc_down)/zthick))

! an additional fix that brings z to a constant value (=0.5) along the iy=1,iy=ny_g bounds
                                                                                                                                                
        zeta(1,j,k) =   zeta(1,j,k) * &
                        0.25* (1.0 - tanh( (y(j)-fade_loc_up)/fadethick))* &
                              (1.0 + tanh( (y(j)-fade_loc_down)/fadethick)) &
                      + 0.5 * (1.0 -  &
                        0.25* (1.0 - tanh( (y(j)-fade_loc_up)/fadethick))* &
                              (1.0 + tanh( (y(j)-fade_loc_down)/fadethick)))
                                                                                                                                                
      enddo
    enddo
  endif

  if( nx .gt. 1) then
    do i=2,nx
      zeta(i,:,:)=zeta(1,:,:)
    enddo
  endif

!declare zeta,mix_loc_up,mix_loc_down,zthick

  call zclookup(c,zeta, yspecies, temp, io)


!  if(xid.eq.0)then
!    do i=1,ny
!    write(*,*)'y',myid,i,yspecies(1,i,1,n_spec),yspecies(1,i,nz,n_spec)
!    enddo
!  endif

  !deallocate c-table
  call initialize_zclookup(io,-1)

  s = decay_factor * half_width
  slot_locy_up   = 0.5*(ymax+ymin)+half_width-3.0*s
  slot_locy_down = 0.5*(ymax+ymin)-half_width+3.0*s

! set mass flux based on inlet speed and coflow speed
  do j = 1, ny
!  just u here. rho is multiplied later.
    rhou_inlet(j) = & 
        co_flow * & 
          ( 1.0 - 0.5*(  &
               tanh((y(j)-slot_locy_down)/s)  &
             - tanh((y(j)-slot_locy_up  )/s)  ) ) &
      + u_inlet * & 
          0.5*(  &
               tanh((y(j)-slot_locy_down)/s)  &
             - tanh((y(j)-slot_locy_up  )/s)  ) 
  end do

! calculate inverse of average molecular weight
  call calc_inv_avg_mol_wt(yspecies,avmolwt)

! set pressure
  pressure(:,:,:)=press_in

! set density from equation of state - use c as temporary
  fact1=univ_gascon*(1.0/a_ref**2)*t_ref
  c(:,:,:)=pressure(:,:,:)/(fact1*temp(:,:,:)*avmolwt(:,:,:))

! set local x-velocity based on conservation of momentum in x-direction
  rho_inlet_yz(:,:) = c(1,:,:)
  call mpi_bcast(rho_inlet_yz, ny*nz, MPI_REAL, 0, xcomm, ierr)
                                                                                                                                  
  do j = 1, ny
    call calc_zrho0(rho0(j),ceta(j),0.0,press_in,io)
    call calc_zrho0(rhoM(j),ceta(j),0.5,press_in,io)
    call calc_zrho0(rho1(j),ceta(j),1.0,press_in,io)
                                                                                                                                  

    do k = 1, nz
      rhou_inlet_yz(j,k) = (1.0-zeta(1,j,k))*Uvel0(j) +  &
                                 zeta(1,j,k)*Uvel1(j)
      rhou_inlet_yz(j,k) = rhou_inlet_yz(j,k)*c(1,j,k)
    enddo
                                                                                                                                  
  enddo
                                                                                                                                  
  do k = 1, nz
    do j = 1, ny
      u(:,j,k,1)=rhou_inlet_yz(j,k)/c(:,j,k)
    enddo
  end do

  u(:,:,:,2:3) = 0.0

  if(xid==0) then
     vel_bar_xinlet = u(1,:,:,1)
  endif

  call MPI_Bcast(vel_bar_xinlet,ny*nz,MPI_REAL8,0,xcomm,ierr)

  ! set mean velocity for feeding turbulence
  feedVel = u_inlet
  allocate(vel_bar(ny,nz))
  vel_bar = vel_bar_xinlet
!----------------------------------------------------------------------
  if(myid.eq.0) then
    call write_header(io,'-')
  endif

!----------------------------------------------------------------------
  return
  end subroutine initialize_strat_feed


#include "globalDefines.h"
! $Id: init_bunsen.f90,v 1.1.2.4 2006/04/25 23:31:49 rsankar Exp $
!----------------------------------------------------------------------
  subroutine initialize_strat &
    ( io, yspecies, temp, pressure, u, half_width, decay_factor )
!----------------------------------------------------------------------
! Written by Ramanan Sankaran
! To initialize a Bunsen - slot burner - flame.
! Based on Evatt Hawkes' v-flame code
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

  integer, intent(in) :: io
  real, intent(out) :: half_width, decay_factor

  character*100 :: filename
  integer, parameter :: io_bunsen=204

  real u_inlet, Sl_guess, thickness, press_in, co_flow
  real s, vel, dens
  real rhou_inlet(ny), rho_inlet(ny)
  real rhou_inlet_yz(ny,nz), rho_inlet_yz(ny,nz)
  real, dimension(nx,ny,nz) :: c,zeta
  real mix_loc_up,mix_loc_down,zthick,zoffset
  integer istrat

  real sin_alpha, tan_alpha, sigma, fact1
  real flame_locy, flame_locy_up, flame_locy_down
  real fade_loc_up,fade_loc_down, fadethick
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

    !Stratification control: If positive or zero it is number of stratification cycles along z direction
    !                        If negative 1 it is back supported
    !                        If negative 2 it is front supported    
    read(io_bunsen, *) istrat

    read(io_bunsen, *) zoffset

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
  call MPI_Bcast(istrat,1,MPI_INTEGER,0,gcomm,ierr) 
  call MPI_Bcast(zoffset,1,MPI_REAL8,0,gcomm,ierr)

! thickness equals 2sigma for tanhprof

  sigma = thickness/2.0

!set angles
  sin_alpha=sl_guess/u_inlet
  tan_alpha=sl_guess/sqrt(u_inlet**2-sl_guess**2)

! set the c profile

  do i = 1, nx
    flame_locy = half_width - x(i)*tan_alpha
    !Push flame_locy outside so that the flame begins outside h.
    flame_locy = flame_locy+3.0*sigma
    flame_locy_up = 0.5*(ymax+ymin) + flame_locy
    flame_locy_down = 0.5*(ymax+ymin) - flame_locy
    do j = 1, ny
      c(i,j,1) = 1.0 -  &
                 0.25* (1.0 - tanh( (y(j)-flame_locy_up)/sigma))* &
                       (1.0 + tanh( (y(j)-flame_locy_down)/sigma))
    end do
  end do
  ceta(:)=c(1,:,1)

  if( nz .gt. 1)  then
    do k = 2, nz
      c(:,:,k) = c(:,:,1)
    end do
  end if

  if(istrat.gt.0)then  ! stratification normal to flame, istrat is the number of cycles in the z-direction
                       ! so far the only option is 1 cycle. esr 13March09 

! set the zeta profile
! this is two tanh steps at z=1/4 and z=3/4, so that
! zeta =1 at z=0 and zeta=1 at z=0.5
                                                                                                                    
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

  elseif(istrat.eq.-1)then !back supported flame: products richer than reactants, stratification parallel to flame. 


  zeta = 1.0

  do i = 1, nx
    flame_locy = half_width 
    flame_locy = flame_locy-zoffset*sigma
    flame_locy_up = 0.5*(ymax+ymin) + flame_locy
    flame_locy_down = 0.5*(ymax+ymin) - flame_locy
    do j = 1, ny
      zeta(i,j,1) = 1.0 -  &
                 0.25* (1.0 - tanh( (y(j)-flame_locy_up)/sigma))* &
                       (1.0 + tanh( (y(j)-flame_locy_down)/sigma))
    end do
  end do

  if( nz .gt. 1) then
    do i=2,nz
      zeta(:,:,i)=zeta(:,:,1)
    enddo
  endif


  elseif(istrat.eq.-2)then  !back supported flame: products richer than reactants, stratification parallel to flame.  

  zeta = 1.0

  do i = 1, nx
    flame_locy = half_width 
    flame_locy = flame_locy-zoffset*sigma
    flame_locy_up = 0.5*(ymax+ymin) + flame_locy
    flame_locy_down = 0.5*(ymax+ymin) - flame_locy
    do j = 1, ny
      zeta(i,j,1) = 0.25* (1.0 - tanh( (y(j)-flame_locy_up)/sigma))* & 
                         (1.0 + tanh( (y(j)-flame_locy_down)/sigma))
                 
    end do
  end do

  if( nz .gt. 1) then
    do i=2,nz
      zeta(:,:,i)=zeta(:,:,1)
    enddo
  endif

  endif ! istrat


  if( nx .gt. 1) then
    do i=2,nx
      zeta(i,:,:)=zeta(1,:,:)
    enddo
  endif
                                                                                                                    
!declare zeta,mix_loc_up,mix_loc_down,zthick
                                                                                                                    
  call zclookup(c,zeta, yspecies, temp, io)
                                                                                                                    
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


! there are two options here, the first is the same as the premixed cases
! simply varying velocity between the jet and coflow velocities across the 
! tanh function. 
! The second has a variation in the z direction so that the mass flux
! is the same as the phi=0.7 premixed case despite the changing density 
! with phi along the z direction.

  if(istrat.lt.0)then

! set local x-velocity based on conservation of momentum in x-direction
  rho_inlet(:) = c(1,:,1)
  call mpi_bcast(rho_inlet, ny, MPI_REAL8, 0, xcomm, ierr)
  rhou_inlet(:) = rhou_inlet(:)*rho_inlet(:)
  do j = 1, ny
    u(:,j,:,1)=rhou_inlet(j)/c(:,j,:)
  end do

 else

! set local x-velocity based on conservation of momentum in x-direction
  rho_inlet_yz(:,:) = c(1,:,:)
  call mpi_bcast(rho_inlet_yz, ny*nz, MPI_REAL, 0, xcomm, ierr)
                                                                                                                    

  do j = 1, ny
    call calc_zrho0(rho0(j),ceta(j),0.0,press_in,io)
    call calc_zrho0(rhoM(j),ceta(j),0.5,press_in,io)
    call calc_zrho0(rho1(j),ceta(j),1.0,press_in,io)

    do k = 1, nz
      rhou_inlet_yz(j,k) = rhou_inlet(j)*rhoM(j)/c(1,j,k)
      rhou_inlet_yz(j,k) = rhou_inlet_yz(j,k)*c(1,j,k)  ! multiply by density
    enddo

  enddo

  do k = 1, nz
    do j = 1, ny
      u(:,j,k,1)=rhou_inlet_yz(j,k)/c(:,j,k)
    enddo
  end do

 endif  !istrat

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
  end subroutine initialize_strat


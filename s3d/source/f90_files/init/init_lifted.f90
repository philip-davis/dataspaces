#include "globalDefines.h"
!----------------------------------------------------------------------
  subroutine initialize_lifted &
    ( io, yspecies, temp, pressure, u, uprofile )
!----------------------------------------------------------------------
! Written by Chun Sang Yoo
! To initialize a lifted flame.
! Based on Evatt Hawkes' v-flame code
!----------------------------------------------------------------------
  use param_m
  use topology_m
  use grid_m, only : x, y, z, xmin, xmax, ymin, ymax, zmin, zmax, xg
  use reference_m
  use chemkin_m, only : species_name
  use thermchem_m, only : avmolwt, calc_inv_avg_mol_wt, mixEnth, calc_temp
  
  use frozenfeed_m, only : vel_bar, feedVel
  use mixfrac_m, only: allocate_mixFrac_arrays, fuelMassfr, oxidMassfr
  use mixfrac_m, only: getStoichMixfr, stoichMixfr

  implicit none
!----------------------------------------------------------------------
  real, intent(inout), dimension(nx,ny,nz) :: temp, pressure
  real, intent(inout), dimension(nx,ny,nz,n_spec) :: yspecies
  real, intent(inout), dimension(nx,ny,nz,3) :: u

  integer, intent(in) :: io
  real :: half_width, decay_factor
  real, intent(out) :: uprofile(ny)

  character*100 :: filename
  integer, parameter :: io_lifted=204

  real u_inlet, press_in, co_flow, t_inlet, t_coflow, thickness, s, sigma
  real width_mf_vel, h_jet, h_coflow, totspec,alpha_xi
  real, dimension(ny) :: zprofile, zprofile_out, uprofile_out
  real, dimension(nx,ny) :: cprofile
  real, dimension(nx,ny) :: zprofile2d, uprofile2d, hprofile2d
  real, dimension(nx,ny,nz) :: hfield
  real zstoich, tstoich, totburnt
  real, dimension(n_spec) :: ystoich
  real, dimension(nx,ny) :: tburnt 
  real, dimension(nx,ny,n_spec) :: yburnt
  integer prof_spec, linear_enth, inflamelet
  real sigma2, Lx_heat, Lx_heat2
  real cp_H2, cp_air, Y_C2H4, t_max
  integer i, j, k
  real :: vel_bar_xinlet(ny, nz)

  real flame_locy, flame_locy_up, flame_locy_down, tan_alpha

!----------------------------------------------------------------------
! write header

  if(myid == 0) then
    call write_header(io,'-')
    write(io,*) 'initializing lifted flame ....'
    write(io,*)
  endif
!----------------------------------------------------------------------
! Read the input file
  filename = '../input/lifted.in'
  call inquire_about_input_file(filename, io)

  if(myid .eq. 0) then
    open(unit=io_lifted, file = trim(filename), status = 'old')

    !header
    read(io_lifted, *)

    !slot half width (input in mm)
    read(io_lifted, *) half_width
      !convert to SI
      half_width = half_width*1e-3
    
    !inflow velocity in m/s
    read(io_lifted, *) u_inlet
    
    !coflow velocity in m/s
    read(io_lifted, *) co_flow
    
    !inflow temperature in K
    read(io_lifted, *) t_inlet
    
    !coflow temperature in K
    read(io_lifted, *) t_coflow
    
    !Inflow profile decays to zero using this factor
    !Fraction of h, half-width. Non-dimensionalization is not needed.
    read(io_lifted, *) decay_factor
    
    !Pressure at inlet 
    read(io_lifted, *) press_in

    !Profile specification
    read(io_lifted, *) prof_spec

    !Width ratio (width_mf/width_vel)
    read(io_lifted, *) width_mf_vel

    !Temperature initialisation (0=linear temp, 1=linear enthalpy)
    read(io_lifted, *) linear_enth
    
    !Flamelet initialisation (0=none, 1=temperature patch, 
    ! 2=Burke-Schumann triangles, 3=read flamelet solution - not implemented)
    read(io_lifted, *) inflamelet

    close(io_lifted)
    
    !nondimensionalize
    half_width = half_width/l_ref
    u_inlet = u_inlet/a_ref
    co_flow = co_flow/a_ref
    t_inlet = t_inlet/t_ref
    t_coflow = t_coflow/t_ref
    press_in  = press_in*pres_atm/p_ref
  
  end if

! broadcast input data
  call MPI_Bcast(half_width,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(u_inlet,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(co_flow,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(t_inlet,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(t_coflow,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(decay_factor,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(press_in,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(prof_spec,1,MPI_INTEGER,0,gcomm,ierr)
  call MPI_Bcast(width_mf_vel,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(linear_enth,1,MPI_INTEGER,0,gcomm,ierr)
  call MPI_Bcast(inflamelet,1,MPI_INTEGER,0,gcomm,ierr)

  s = decay_factor * half_width
  sigma = 2.5*s

! set initial temperature and velocities
  temp(:,:,:) = t_coflow
  u(:,:,:,1) = co_flow
  u(:,:,:,2:3) = 0.0
  yspecies(:,:,:,:) = 0.0
  pressure(:,:,:) = press_in

! set the cprofile
!  do i = 1, nx
!    flame_locy = half_width - x(i)*tan_alpha
!    !Push flame_locy outside so that the flame begins outside h.
!    flame_locy = flame_locy+3.0*sigma
!    flame_locy_up = 0.5*(ymax+ymin) + flame_locy
!    flame_locy_down = 0.5*(ymax+ymin) - flame_locy
!    do j = 1, ny
!      c(i,j,1) = 1.0 -  &
!                 0.25* (1.0 - tanh( (y(j)-flame_locy_up)/sigma))* &
!                       (1.0 + tanh( (y(j)-flame_locy_down)/sigma))
!    end do
!  end do

  if(inflamelet.eq.0)then
    cprofile(:,:)=0.0
  elseif(inflamelet.ne.0)then

    tan_alpha=4.0 * half_width/(xmax-xmin)
    do i=1, nx
!    flame_locy = half_width - x(i)*tan_alpha
!    flame_locy =  0.5* half_width + (x(i)-0.5*(xmax+xmin))*tan_alpha
    flame_locy =  2.0* half_width 
    flame_locy_up   = 0.5*(ymax+ymin) + flame_locy
    flame_locy_down = 0.5*(ymax+ymin) - flame_locy

      do j=1, ny
        cprofile(i,j)=0.5*( 1.0+tanh((x(i)-0.40*(xmax+xmin))/(0.1*(xmax-xmin)) )  )   
! flame initialised at 40% of domain height

        cprofile(i,j)=cprofile(i,j) * &
                     (0.25* (1.0 - tanh( (y(j)-flame_locy_up)  /sigma)  )* &
                            (1.0 + tanh( (y(j)-flame_locy_down)/sigma)  )  )
! flame cut off outside +/- 1 jet width

      enddo
    enddo
!    cprofile(:,:)=0.0
  endif

! set the zprofile, consider zprofile the mixture fraction variation across the y
! direction at the inlet plane

  if(prof_spec.eq.0)then   !tanh profiles

 
    do j = 1, ny
      zprofile(j) = 0.5 * &
                    (    tanh((y(j)-0.5*(ymax+ymin) + half_width)/sigma) &
                       - tanh((y(j)-0.5*(ymax+ymin) - half_width)/sigma))
! specify an outlet profile for a fully developed planar jet using the formulae in Pope's book, page 137.
! <U>=U_o(x)f(xi)
! U_o(x)=U_o(0)/x^0.5
! xi=y/y_half(x)
! y_half(x)=H/2+x/10  ! 10% spreading rate
! f(xi)=sech^2(alpha.xi)=1/cosh(alpha.xi)
! cosh(x)=(exp(x)+exp(-x))/2
! alpha=0.5 ln(1+2^0.5)^2=0.88137

! alpha_xi = (0.88137*y(j)/(half_width+x(i)/10.0))
! sech^2(alpha.xi) = 2.0/(exp(alpha_xi)+exp(-alpha_xi))
! <U>=sech^2(alpha.xi) /(x(i))**0.5

!      alpha_xi = (0.88137*y(j)/(half_width+xg(nx_g)/10.0))
      alpha_xi = (0.88137*y(j)/(half_width+0.25*xg(nx_g)/10.0))
      zprofile_out(j) =  2.0/(exp(alpha_xi)+exp(-alpha_xi))/(xg(nx_g)/(2.0*half_width))**0.5
      uprofile(j) = zprofile(j)
      uprofile_out(j) = zprofile_out(j)
    enddo
  else !gaussian profile
    do j = 1, ny
      zprofile(j) = exp( -((y(j)-0.5*(ymax+ymin))/(half_width*width_mf_vel))**2)
      uprofile(j) = exp( -((y(j)-0.5*(ymax+ymin))/(half_width))**2)
      alpha_xi = (0.88137*y(j)/(half_width+xg(nx_g)/10.0))
      zprofile_out(j) =  2.0/(exp(alpha_xi)+exp(-alpha_xi))/(xg(nx_g))**0.5
      uprofile_out(j) = zprofile_out(j)
    enddo
  endif

  do j = 1, ny
  do i = 1, nx
    zprofile2d(i,j)=zprofile(j) 
    uprofile2d(i,j)=uprofile(j) + (uprofile_out(j)-uprofile(j))*(x(i)/xg(nx_g))**4.0
  enddo
  enddo

! set the species mass fractions using zprofile
  call allocate_mixFrac_arrays(1)

! find stoichiometric mixture fraction:
  call getStoichMixfr

  zstoich = stoichMixfr

! set the stoichiometric burnt composition

  do i=1,n_spec
    ystoich(i)=0.0 
  enddo
! set the CO2,H2O and N2 compositions at stoichiometry, everything else is zero.
! use n_spec to work out what mechanism we're dealing with

  if(n_spec.eq.9)then  !9 species hydrogen mechanism
!H2O=5th 
    ystoich(n_spec)=oxidMassfr(n_spec) + (fuelMassfr(n_spec)-oxidMassfr(n_spec))*zstoich
    ystoich(5) = 1.0-ystoich(n_spec)

  elseif(n_spec.eq.6)then  !6 species methane mechanism
!H2O=5th 
!CO2=4th
    ystoich(n_spec)=oxidMassfr(n_spec) + (fuelMassfr(n_spec)-oxidMassfr(n_spec))*zstoich
    ystoich(5) = (1.0-ystoich(n_spec))*(2.0*18.0/(2.0*18.0+1.0*44.0))
    ystoich(4) = 1.0-ystoich(n_spec)-ystoich(5)
  
  elseif(n_spec.eq.22)then  !22 species ethylene
!H2O=6th
!CO2=12th
    ystoich(n_spec)=oxidMassfr(n_spec) + (fuelMassfr(n_spec)-oxidMassfr(n_spec))*zstoich
    ystoich(6) = (1.0-ystoich(n_spec))*(2.0*18.0/(2.0*18.0+2.0*44.0))
    ystoich(12) = 1.0-ystoich(n_spec)-ystoich(6)

  elseif(n_spec.eq.58.or.n_spec.eq.52.or.n_spec.eq.53)then  !58 or 52 species n_heptane
!H2O=6th
!CO2=8th
    ystoich(n_spec)=oxidMassfr(n_spec) + (fuelMassfr(n_spec)-oxidMassfr(n_spec))*zstoich
    ystoich(6) = (1.0-ystoich(n_spec))*(8.0*18.0/(8.0*18.0+7.0*44.0))
    ystoich(8) = 1.0-ystoich(n_spec)-ystoich(6)
    
  elseif(n_spec.eq.30.or.n_spec.eq.31)then  !30 or 31 species DME
!H2O=7th
!CO2=18th
    ystoich(n_spec)=oxidMassfr(n_spec) + (fuelMassfr(n_spec)-oxidMassfr(n_spec))*zstoich
    ystoich(7) = (1.0-ystoich(n_spec))*(3.0*18.0/(3.0*18.0+2.0*44.0))
    ystoich(18) = 1.0-ystoich(n_spec)-ystoich(7)

  else
    write(io,*)'Error in init_lifted.f90: mechanism not known'
    stop
  endif



! if the linear_enth option is used the value of tstoich is only used to modify the velocity and does not 
! effect the initial temperture and enthalpy directly.

  tstoich = 2200.0/t_ref

  do j = 1, ny
  do i = 1, nx
    totspec=0.0
    totburnt=0.0
    do k = 1, n_spec-1
      yspecies(i,j,1,k) = oxidMassfr(k) + (fuelMassfr(k) - oxidMassfr(k)) * zprofile2d(i,j)
      totspec = totspec + yspecies(i,j,1,k)
      if(zprofile2d(i,j).le.zstoich)then
        yburnt(i,j,k)     = oxidMassfr(k) +(ystoich(k)-oxidMassfr(k))*zprofile2d(i,j)/zstoich
      else
        yburnt(i,j,k)     = ystoich(k) +(fuelMassfr(k)-ystoich(k))*(zprofile2d(i,j)-zstoich)/(1.0-zstoich)
      endif 

      totburnt = totburnt + yburnt(i,j,k)
    enddo

    if(n_spec.eq.53.or.n_spec.eq.31)then               !set species n_spec-1=zstoich*1e-7
      yspecies(i,j,1,n_spec-1)=zprofile2d(i,j)*1.0e-7
      totspec=totspec+yspecies(i,j,1,n_spec-1)
      yburnt(i,j,n_spec-1)=zprofile2d(i,j)*1.0e-7
      totburnt=totburnt+yburnt(i,j,n_spec-1)
    endif

    yspecies(i,j,1,n_spec) = 1.0 - totspec
    yburnt(i,j,n_spec) = 1.0 - totburnt
    temp(i,j,1) = t_coflow + (t_inlet - t_coflow) * zprofile2d(i,j)   !initially set temperature as straight line
    tburnt(i,j) = min(t_coflow+(Tstoich-t_coflow)*zprofile2d(i,j)/zstoich,  &
                    Tstoich+(T_inlet-Tstoich)*(zprofile2d(i,j)-zstoich)/(1.0-zstoich))
    u(i,j,1,1) = co_flow + (u_inlet - co_flow) * uprofile2d(i,j)
  enddo
  enddo


! simple and dirty check that all mass fractions are bounded by zero and one

  do j=1,ny
  do i=1,nx
    do k=1,n_spec
      if(yburnt(i,j,k).lt.0.0.or.yburnt(i,j,k).gt.1.0)write(io,*)'out of range init_lifted ',k,yburnt(i,j,k), zprofile2d(i,j)
    enddo
  enddo
  enddo

  if(inflamelet.eq.1)then
    do j=1,ny
      do i=1,nx
        !the unburnt velocity is scaled by the density ratio 
        !this line must come before tburnt is added to the temperature 
        u(i,j,1,1) = u(i,j,1,1) * ((1.0-cprofile(i,j))*temp(i,j,1) + cprofile(i,j)*tburnt(i,j)) / temp(i,j,1)
        temp(i,j,1) = (1.0-cprofile(i,j))*temp(i,j,1) + cprofile(i,j)*tburnt(i,j)
      enddo
    enddo
  elseif(inflamelet.eq.2)then
    do j=1,ny
      do i=1,nx
        do k=1,n_spec
          yspecies(i,j,1,k)=(1.0-cprofile(i,j))*yspecies(i,j,1,k) + cprofile(i,j)*yburnt(i,j,k)
        enddo
        !the unburnt velocity is scaled by the density ratio 
        !this line must come before tburnt is added to the temperature 
        u(i,j,1,1) = u(i,j,1,1) * ((1.0-cprofile(i,j))*temp(i,j,1) &
                    + cprofile(i,j)*tburnt(i,j)) / temp(i,j,1)                                                                
        temp(i,j,1) = (1.0-cprofile(i,j))*temp(i,j,1) + cprofile(i,j)*tburnt(i,j)
      enddo
    enddo
  endif


  if(linear_enth.eq.1)then
! find the enthalpy for the profile
    h_coflow = mixEnth(oxidMassfr, t_coflow)
    h_jet = mixEnth(fuelMassfr, t_inlet)
    
    do j = 1, ny
    do i = 1, nx
      hprofile2d(i,j) = h_coflow + (h_jet - h_coflow) * zprofile2d(i,j)
    enddo
    enddo
  endif
  
  call allocate_mixFrac_arrays(-1)
  
!C2H4   ! set C2H4(15th), O2(4th), and N2(22nd) profiles at inlet boundary
!C2H4     s = decay_factor * half_width
!C2H4     cp_H2 = 14.31 ! kJ/kg/K
!C2H4     cp_air = 1.182 ! kJ/kg/K
!C2H4     sigma = 2.5*s
!C2H4   !  Y_C2H4 = 0.20424
!C2H4     Y_C2H4 = 0.18
!C2H4     do j = 1, ny
!C2H4       yspecies(:,j,1,15) = Y_C2H4*0.5 * &
!C2H4                       (   tanh((y(j)-0.5*(ymax+ymin) + half_width)/sigma) &
!C2H4                         - tanh((y(j)-0.5*(ymax+ymin) - half_width)/sigma))
!C2H4       yspecies(:,j,1,22) = (1.0 - Y_C2H4)*0.5 * &
!C2H4                       (   tanh((y(j)-0.5*(ymax+ymin) + half_width)/sigma) &
!C2H4                         - tanh((y(j)-0.5*(ymax+ymin) - half_width)/sigma))
!C2H4       yspecies(:,j,1,4) = 0.233*(1.0-yspecies(:,j,1,15)-yspecies(:,j,1,22))
!C2H4       yspecies(:,j,1,22) = (1.0 - yspecies(:,j,1,15) - yspecies(:,j,1,4))
!C2H4       u(:,j,1,1) = co_flow + (u_inlet-co_flow)*0.5*                            &
!C2H4                  (       tanh((y(j)-0.5*(ymax+ymin) + half_width )/sigma)   &
!C2H4                        - tanh((y(j)-0.5*(ymax+ymin) - half_width )/sigma) )
!C2H4     enddo
!C2H4   
!C2H4     Lx_heat = 12.0/1000/l_ref
!C2H4     Lx_heat2 = 10.0/1000/l_ref
!C2H4   
!C2H4     t_max = 2300./t_ref
!C2H4     
!C2H4    do i = 1, nx
!C2H4      do j = 1, ny
!C2H4        temp(i,j,1) = t_coflow - (t_coflow-t_inlet)*0.5*                       &
!C2H4                   (       tanh((y(j)-0.5*(ymax+ymin) + half_width)/sigma)     &
!C2H4                         - tanh((y(j)-0.5*(ymax+ymin) - half_width)/sigma) )  ! &
!C2H4                !   * 0.5*(1.0-tanh((x(i)-Lx_heat2)/sigma))                    & 
!C2H4                !   + (t_max-t_coflow)*0.5*                                    &
!C2H4                !  ( tanh((y(j)-0.5*(ymax+ymin) + half_width + 5.0*s)/sigma)     &
!C2H4                !  - tanh((y(j)-0.5*(ymax+ymin) - half_width - 5.0*s)/sigma) )   &
!C2H4                !  * 0.5*(1.0+tanh((x(i)-Lx_heat)/sigma))
!C2H4      enddo
!C2H4    enddo
  
  if ( nz .gt. 1) then
    do k = 2, nz
      yspecies(:,:,k,:) = yspecies(:,:,1,:)
      temp(:,:,k) = temp(:,:,1)
      u(:,:,k,1) = u(:,:,1,1)
    enddo
  endif

  if(linear_enth.eq.1)then
      hfield(:,:,1) = hprofile2d(:,:)
    if(nz.gt.1)then
      do k = 2, nz
        hfield(:,:,k) = hfield(:,:,1)
      enddo
    endif
    
    call calc_temp( temp, hfield, u, yspecies ) !this neglects the kinetic energy
  endif

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
  end subroutine initialize_lifted


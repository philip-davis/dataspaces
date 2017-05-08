#include "globalDefines.h"
!=========================================================================================
  module compheat_m
!=========================================================================================
! module for compression heating parameters

  implicit none
!-----------------------------------------------------------------------------------------
! integers

  integer :: i_compheat = 0
  character*12 :: poly_mode, compheat_mode

! reals

  real :: RPM, CR, Rc
  real :: mass_intake
!-----------------------------------------------------------------------------------------
  contains
!========================================================================================
  subroutine initialize_compheat(io)
!========================================================================================
  use topology_m

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  character*100 filename
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing compression heating module...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! set file name and inquire

  filename='../input/compheat.in'
  call inquire_about_input_file(filename,io)
!----------------------------------------------------------------------------------------
! read file

  if(myid.eq.0) then

    open(unit=13,file=trim(filename),status='unknown')

    read(13,*) poly_mode
    read(13,*) compheat_mode
    read(13,*) RPM
    read(13,*) CR
    read(13,*) Rc

!   close file

    close(13)

!   check poly_mode

    if((trim(poly_mode).eq.'isentropic').or.  &
       (trim(poly_mode).eq.'isothermal').or.  &
       (trim(poly_mode).eq.'isobaric')) then
        continue
    else
        write(io,*) 'improper setting of polytropic process in compheat.in'
        write(io,*) 'poly_mode = ', trim(poly_mode)
        write(io,*) 'see routine initialize_compheat'
        term_status=1
    endif

  endif

  call check_term_status(io,0)
!----------------------------------------------------------------------------------------
! broadcast parameters

  call MPI_Bcast( RPM, 1, MPI_REAL8, 0, gcomm, ierr)
  call MPI_Bcast( CR, 1, MPI_REAL8, 0, gcomm, ierr)
  call MPI_Bcast( Rc, 1, MPI_REAL8, 0, gcomm, ierr)
  call MPI_Bcast( poly_mode, 12, MPI_CHARACTER, 0, gcomm, ierr)
  call MPI_Bcast( compheat_mode, 12, MPI_CHARACTER, 0, gcomm, ierr)

  if(myid == 0) then

    write(io,*) 'polytropic process = ',trim(poly_mode)
    write(io,*) 'compression heating mode = ',trim(compheat_mode)

    if(trim(compheat_mode).eq.'engine') then
      write(io,'(a7,f8.3)') ' RPM = ',RPM
      write(io,'(a7,f8.3)') ' CR  = ',CR
      write(io,'(a7,f8.3)') ' Rc  = ',Rc
    else
      write(io,*)
      write(io,*) 'improper setting for compression heating mode'
      write(io,*) '(compheat_mode) in compheat.in'
      write(io,*) 'see routine initialize_compheat'
      term_status=1
    endif

    call write_header(io,'-')

  endif

  call check_term_status(io,0)
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_compheat
!========================================================================================
  subroutine compheat(rhs,q,u,yspecies,temp,pressure,volum,gamma,avmolwt,time_accum,i_time)
!========================================================================================
  use topology_m
  use param_m, only : nx, ny, nz, nvar_tot, nsc

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real rhs(nx,ny,nz,nvar_tot)
  real q(nx,ny,nz,nvar_tot)
  real u(nx,ny,nz,3)
  real yspecies(nx,ny,nz,nsc)
  real temp(nx,ny,nz)
  real pressure(nx,ny,nz)
  real volum(nx,ny,nz)
  real gamma(nx,ny,nz)
  real avmolwt(nx,ny,nz)

  real time_accum

  integer i_time

! local declarations

  real ds
  integer L
!----------------------------------------------------------------------------------------
! calculate density source term

  if(trim(compheat_mode).eq.'engine') then
    call calc_density_source_term_engine(ds,temp,pressure,avmolwt,time_accum,i_time)
  else
    ds=0.0  
  endif
!----------------------------------------------------------------------------------------
! add source terms to rhs
!----------------------------------------------------------------------------------------
! momentum equations

  do L=1,3,1
    rhs(:,:,:,L) = rhs(:,:,:,L) + ds*u(:,:,:,L)
  enddo

! continuity

  rhs(:,:,:,4) = rhs(:,:,:,4) + ds

! energy equation

  if(trim(poly_mode).eq.'isentropic') then

    rhs(:,:,:,5) = rhs(:,:,:,5)  &
                 + ds*((q(:,:,:,5)*volum(:,:,:))+(pressure(:,:,:)*volum(:,:,:)))

  elseif(trim(poly_mode).eq.'isothermal') then

    rhs(:,:,:,5) = rhs(:,:,:,5)  &
                 + ds*(q(:,:,:,5)*volum(:,:,:))

  elseif(trim(poly_mode).eq.'isobaric') then

    rhs(:,:,:,5) = rhs(:,:,:,5)  &
                 + ds*((q(:,:,:,5)*volum(:,:,:))  &
                 + ((1.0/(1.0-gamma(:,:,:)))*pressure(:,:,:)*volum(:,:,:)))
  endif

! species equation

  do L=1,nsc,1
    rhs(:,:,:,L+5) = rhs(:,:,:,L+5) + ds*yspecies(:,:,:,L)
  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine compheat
!========================================================================================
  subroutine calc_density_source_term_engine(ds,temp,pressure,avmolwt,time_accum,i_time)
!========================================================================================
! calculates the density source for the simulation of
! compression through the addition of source terms to governing equations
! currently the density source has no spatial dependency, it is just point-wise
!----------------------------------------------------------------------------------------
  use topology_m
  use param_m
  use reference_m

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real ds, time_accum
  integer i_time
  real temp(nx,ny,nz)
  real pressure(nx,ny,nz)
  real avmolwt(nx,ny,nz)

! other declarations

  real constant_k
  real omega, pi
  real pres_intake, temp_intake, fact1
  real rho_intake
  real w, t, num, den, V_intake, theta_rad, Vc, t_end
!----------------------------------------------------------------------------------------
! calculate pi and omega (rad/s)

  pi=4.0*atan(1.0)
  omega=RPM*(2.0*pi/60.0)
!----------------------------------------------------------------------------------------
! calculate starting density (kg/m^3)

  if(i_time.eq.1) then  !this is dependent upon the time loop structure

!   set intake values

    if((nx.eq.1).and.(ny.eq.1).and.(nz.eq.1)) then
      temp_intake=temp(1,1,1)*t_ref
      pres_intake=pressure(1,1,1)*p_ref
    else
      write(6,*) 'density source not ready multi-dimensional runs'
      write(6,*) 'see routine calc_density_source_term_engine'
      call terminate_run(6,0)
    endif
!    write(6,*) 'temp_intake=',temp_intake
!    write(6,*) 'pres_intake=',pres_intake

!   set fact1 (as in inifie.f)

    fact1=univ_gascon*(1.0/a_ref**2)*t_ref

!   non-dimensionalize pres_intake and temp_intake

    pres_intake=pres_intake/p_ref
    temp_intake=temp_intake/t_ref

!   calculate non-dimenional density (as in inifie.f)
!   avmolwt must be before any considerable radical build-up!!!

    rho_intake=pres_intake/(fact1*temp_intake*avmolwt(1,1,1))

!   calculate dimensional density (kg/m^3)

    rho_intake=rho_intake*rho_ref

!   calculate mass_intake from initial conditions (matches SENKIN calculations)

    Vc=1.0
    theta_rad=pi
    V_intake=Vc*(1.0+0.5*(CR-1.0)                  &
           *(Rc+1.0-cos(theta_rad)-sqrt(Rc**2-(sin(theta_rad))**2)))
    mass_intake=rho_intake*V_intake

  endif
!----------------------------------------------------------------------------------------
! calculate dimensional density source (see SENKIN calculations)

  t_end=(60.0/RPM)+(60.0)/(2.0*RPM)

  t=(time_accum*time_ref)+(60.0)/(2.0*RPM)  !+added for 180 deg offset
  w=omega
  num=-((CR-1.0)*(w*sin(w*t)+((w*cos(w*t)*sin(w*t))           &
      /(sqrt((Rc)**2-(sin(w*t))**2)))))
  den=2.0*(1.0+(((CR-1.0)*(1.0+Rc-cos(w*t)-                   &
      sqrt((Rc)**2-(sin(w*t))**2)))/(2.0)))**2
  ds=mass_intake*num/den

! non-dimensionalize ds

  ds=ds*(time_ref/rho_ref)

! terminate at one cycle

  if(t.gt.t_end) then
    call terminate_run(6,1)
  endif

! write file

  if(myid == 0) then
    if(i_time.eq.1) then
      open(unit=124,file='../post/gnuplot/ds.dat',status='unknown')
    else
      write(124,125) t-(60.0)/(2.0*RPM), ds
    endif
  endif
  125 format(10(e12.5,1x))    
!----------------------------------------------------------------------------------------
  return
  end subroutine calc_density_source_term_engine
!----------------------------------------------------------------------------------------
  end module compheat_m

#include "globalDefines.h"
!==============================================================================
  subroutine initialize_ignition_test(io,yspecies,temp,pressure,u)
!==============================================================================
! initializes the following quantities for ignition:
!
!   yspecies - species mass fractions
!   temp     - temperature
!   pressure - pressure field
!   u        - mean velocity field
!------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz, n_spec, n_elem
  use param_m, only : vary_in_x, vary_in_y, vary_in_z

  use reference_m, only : t_ref, pres_atm, p_ref, l_ref

  use chemkin_m, only : species_name, element_name, ickwrk, rckwrk, molwt

  use grid_m, only : xmin, xmax, ymin, ymax, zmin, zmax

  use mixfrac_m

  implicit none
!------------------------------------------------------------------------------
! variables passed in

  real, intent(inout), dimension(nx,ny,nz) :: temp, pressure
  real, intent(inout), dimension(nx,ny,nz,n_spec) :: yspecies
  real, intent(inout), dimension(nx,ny,nz,3) :: u

  integer, intent(in) :: io

! eqRat declarations

  character*16 fuelname
  real :: eqRat

! temperature declarations

  real :: temp_max
  real :: temp_min
  real :: temp_var
  real :: temp_fac
  real :: Lxyz(3)         !domain lengths in each direction
  real :: L_max           !longest distance in domain

! pressure declarations

  real :: pres_init

! other declarations

  integer :: i,j,k,L
  character*100 :: filename
  logical :: exist, zero_d
!------------------------------------------------------------------------------
! check for zero-dimensions

  zero_d=.false.
  if((nx==1).and.(ny==1).and.(nz==1)) then
    zero_d=.true.
  endif
!------------------------------------------------------------------------------
! write header

  if(myid==0) then
    if(zero_d) then
      write(io,100) 'initializing for zero-dimensional ignition...'
    else
      write(io,100) 'initializing for multi-dimensional ignition...'
    endif
    write(io,100)
  endif
!------------------------------------------------------------------------------
! read ignition input file

  if (myid==0) then
    filename = '../input/ignition.in'          ! set the file name
    inquire(file=trim(filename),exist=exist)   ! see if the file exists
  endif

  call MPI_Bcast(exist, 1, MPI_LOGICAL, 0, gcomm, ierr)

  if(.not.exist) then   !does not exist
     if(myid==0) then
        write(io,*) 'the following input file does not exist:'
        write(io,*) trim(filename)
     endif
     call terminate_run(io,0)
  endif

  if(myid == 0) then
    open(unit=20, file=trim(filename), status='old', form='formatted')
    read(20,*)                  !fuel name
    read(20,*)  eqRat             ! equivalence ratio
    read(20,*)  pres_init       ! initial pressure (atm)
    read(20,*)  temp_max        ! maximum temperature (K)
    read(20,*)  temp_min        ! minimum temperature (K)
    read(20,*)  temp_var        ! variance for temperature gaussians
    read(20,*)  temp_fac        ! shift factor for temperature gaussians
    close(20)
  endif

! broadcast input file parameters

  call MPI_Bcast(eqRat,       1, MPI_REAL8,    0, gcomm, ierr)
  call MPI_Bcast(pres_init, 1, MPI_REAL8,    0, gcomm, ierr)
  call MPI_Bcast(temp_max,  1, MPI_REAL8,    0, gcomm, ierr)
  call MPI_Bcast(temp_min,  1, MPI_REAL8,    0, gcomm, ierr)
  call MPI_Bcast(temp_var,  1, MPI_REAL8,    0, gcomm, ierr)
  call MPI_Bcast(temp_fac,  1, MPI_REAL8,    0, gcomm, ierr)

! write to screen

  if(myid==0) then
    write(io,102) 'equivalence ratio = ',eqRat
    write(io,103) 'initial pressure = ',pres_init, ' atm'
    if(zero_d) then
      write(io,101) 'temperature = ',temp_max, ' K'
    else
      write(io,101) 'maximum temperature = ',temp_max, ' K'
      write(io,101) 'minimum temperature = ',temp_min, ' K'
      write(io,102) 'temperature variance = ',temp_var
      write(io,102) 'temperature shift factor = ',temp_fac
    endif
    write(io,100)
  endif
!------------------------------------------------------------------------------
! non-dimensionalize input temperature and pressure

  temp_max = temp_max / t_ref
  temp_min = temp_min / t_ref
  pres_init = pres_init * pres_atm / p_ref
!------------------------------------------------------------------------------
! set longest domain distance

  Lxyz(:)=0.0
  if(vary_in_x==1) Lxyz(1)=(xmax-xmin)*l_ref
  if(vary_in_y==1) Lxyz(2)=(ymax-ymin)*l_ref
  if(vary_in_z==1) Lxyz(3)=(zmax-zmin)*l_ref

  L_max=sqrt(Lxyz(1)**2 + Lxyz(2)**2 + Lxyz(3)**2)

! scale variance with longest domain length

  temp_var=L_max/temp_var
!------------------------------------------------------------------------------
! set pressure

  pressure=pres_init
!------------------------------------------------------------------------------
! set temperature

  if(zero_d) then
    temp=temp_max
  else
    call set_spike(temp,temp_max,temp_min,temp_var,temp_fac,io)
  endif
!------------------------------------------------------------------------------
! set velocity

  u(:,:,:,:)=0.0

!------------------------------------------------------------------------------
! set mixture parameters
  call allocate_mixFrac_arrays(1)       ! set up the mixture fraction module
  call getStoichMixfr                   ! initialize stoichiometric mixture fraction
  phi = eqRat

!------------------------------------------------------------------------------
! set species
  call phiToMixfrac( phi, mixfrac )     ! convert equivalence ratio to mixture fraction
  call mixfrToSpec( yspecies )          ! convert mixture fraction to species mass fractions

!------------------------------------------------------------------------------
! format statements

  100 format(3x,a)
  101 format(3x,a,f7.2,a)
  102 format(3x,a,1pe8.2,a)
  103 format(3x,a,f6.2,a)
  104 format(3x,a,a,a)
!------------------------------------------------------------------------------
  return
  end subroutine initialize_ignition_test
!==============================================================================
  subroutine set_spike(phi,maxphi,minphi,sigma,shift_fac,io)
!==============================================================================
! sets spike (gaussian distribution) in variable phi in arbitrary number
! of directions
!------------------------------------------------------------------------------
  use param_m
  use topology_m
  use grid_m
!------------------------------------------------------------------------------
  implicit none

! variables passed in

  real, intent(inout), dimension(nx,ny,nz) :: phi
  real, intent(inout) :: maxphi, minphi, sigma, shift_fac
  integer, intent(in) :: io

! local declarations

  real pi
  real maxPt(3), junk, facx, facy, facz, max, max_g, arg
  integer i,j,k
  real, dimension(nx,ny,nz) :: gauss
!------------------------------------------------------------------------------
! set pi

  pi=4.0*atan(1.0)

! set location

  maxPt = 0.0

  if (vary_in_x == 1) then
    maxPt(1) = (xmax-xmin) * shift_fac
  endif

  if (vary_in_y == 1) then
    maxPt(2) = (ymax-ymin) * shift_fac
  endif

  if (vary_in_z == 1) then
    maxPt(3) = (zmax-zmin) * shift_fac
  endif

! compute phi using a gaussian distribution

  facx = 0.0
  facy = 0.0
  facz = 0.0

  do k=1,nz
     do j=1,ny
        do i=1,nx

           if (vary_in_x == 1)   facx = x(i)-maxPt(1)
           if (vary_in_y == 1)   facy = y(j)-maxPt(2)
           if (vary_in_z == 1)   facz = z(k)-maxPt(3)

           junk = sqrt( facx**2 + facy**2 + facz**2 )

           arg = -junk**2 / (2.0*(sigma**2))

           if(abs(arg).gt.50.0) arg=-50.0  !catch underflow

           gauss(i,j,k) = 1.0/(sigma*sqrt(2.0*pi)) * exp(arg)

        enddo
     enddo
  enddo

  max = maxval(gauss)

  call MPI_Allreduce(max,max_g,1,MPI_REAL8,MPI_MAX,gcomm,ierr)

  gauss = gauss / max_g

! scale phi

  phi = minphi + (maxphi-minphi) * gauss
!------------------------------------------------------------------------------
  return
  end subroutine set_spike

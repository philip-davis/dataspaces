#include "globalDefines.h"
!========================================================================================
  subroutine initialize_opthinrad_test(io,yspecies,temp,pressure,u)
!========================================================================================
! initializes water at T = 1000 K, P = 1 atm for radiation test
!----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx, ny, nz, nsc
  use chemkin_m, only : species_name
  use reference_m
  use opthinrad_m

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real yspecies(nx,ny,nz,nsc+1), temp(nx,ny,nz), pressure(nx,ny,nz), u(nx,ny,nz,3)

  integer io

! local declarations

  integer L
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing optically thin radiation test...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! turn on radiation module and initialize

  i_opthinrad=1
  call initialize_opthinrad(io)
!----------------------------------------------------------------------------------------
! initialize primative variables for ambient air

  u=0.0
  temp=2000.0/t_ref
  pressure=pres_atm/p_ref

  yspecies=0.0
  do L=1,nsc,1
    if(trim(species_name(L)).eq.'H2O') yspecies(:,:,:,L)=1.0
    if(trim(species_name(L)).eq.'N2')  yspecies(:,:,:,L)=0.0
  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_opthinrad_test

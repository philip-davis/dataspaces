#include "globalDefines.h"
!========================================================================================
  subroutine initialize_turbulent_air_test(io,yspecies,temp,pressure,u)
!========================================================================================
! initializes ambient air (T = 300 K, P = 1 atm)
!----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx, ny, nz, nsc
  use chemkin_m, only : species_name
  use reference_m

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
    write(io,*) 'initializing turbulent air test...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! initialize primative variables for ambient air

  u=0.0
  temp=300.0/t_ref
  pressure=pres_atm/p_ref

  yspecies=0.0
  do L=1,nsc+1,1
    if(trim(species_name(L)).eq.'O2') yspecies(:,:,:,L)=0.233
    if(trim(species_name(L)).eq.'N2') yspecies(:,:,:,L)=0.767
  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_turbulent_air_test

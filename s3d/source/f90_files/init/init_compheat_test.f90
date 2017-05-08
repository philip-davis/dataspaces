#include "globalDefines.h"
!========================================================================================
  subroutine initialize_compheat_test(io,yspecies,temp,pressure,u)
!========================================================================================
! initializes compresstion heating test
! 
!----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx, ny, nz, nsc
  use chemkin_m, only : species_name
  use reference_m
  use compheat_m

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
    write(io,*) 'initializing compression heating test...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! initialize ignition case

!  call initialize_ignition_test(io,yspecies,temp,pressure,u)
!  call initialize_ignition_H2_test(io,yspecies,temp,pressure,u)
! gbansal  
  call initialize_hcci(io,yspecies,temp,pressure,u)
!----------------------------------------------------------------------------------------
! turn on compression heating module and initialize

  i_compheat=1
  call initialize_compheat(io)
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_compheat_test

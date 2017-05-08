#include "globalDefines.h"
!=========================================================================================
  module reference_m
!=========================================================================================
! module for reference variables

  implicit none
!-----------------------------------------------------------------------------------------
! reals

  real a_ref        !reference velocity (speed of sound) (m/s)
  real l_ref        !reference length (m)
  real rho_ref      !reference density (kg/m^3)
  real lambda_ref   !reference conductivity (W/m-K)
  real mu_ref       !reference viscosity (Pa-s)
  real t_ref        !reference temperature (K)
  real t_o          !freestream temperature (K)
  real p_ref        !reference pressure (Pa)
  real time_ref     !reference time (s)
  real cp_ref       !reference specific heat at constant pres (J/kg-K)
  real univ_gascon  !universal gas constant (J/mol-K)
  real g_ref        !reference ratio specific heats
  real pres_atm     !standard atmospheric pressure from Chemkin (Pa)

  real re         !acoustic Reynolds number = rho_ref * a_ref * l_ref / mu_ref
  real re_real    !convective Reynolds number = rho_ref * u_ref * l_ref / mu_ref
  real mach_no    !Mach number = u_ref / a_ref (note that u_ref is never calculated)

  real rr_ref  ! reference reaction_rate (rho*Y/time) 
  real hr_ref  ! reference heat release rate (cp*Temp*rr_ref)

  real pr      ! prandtl number belongs here, along with everything else

!-----------------------------------------------------------------------------------------
! Setting the reference variables in SI units (used to be read from s3d.in)

  parameter (mach_no = 0.001)
  parameter (re_real = 100.0)
  parameter (pr = 0.708)

  parameter (g_ref = 1.4)
  parameter (a_ref = 347.2)   ! m/s
  parameter (t_o   = 300.0)   ! K
  parameter (rho_ref = 1.1766)  ! Kg/m3
  parameter (lambda_ref = 26.14e-3)  ! W/m-s

  parameter (re = re_real / mach_no)
  parameter (univ_gascon = 8.31451)                   !J/mol-K
  parameter (t_ref = t_o * ( g_ref - 1 ))             !K
  parameter (cp_ref = (a_ref**2) / t_ref )            !J/kg-K
  parameter (l_ref = pr * lambda_ref / cp_ref * re / rho_ref / a_ref)   !m
  parameter (mu_ref = rho_ref*l_ref*a_ref)       !kg/m-s

  parameter (p_ref = (a_ref**2)*rho_ref)              !Pa
  parameter (time_ref=l_ref/a_ref)                    !sec

  parameter (rr_ref= rho_ref/time_ref)  !kg m^-3 s^-1 
  parameter (hr_ref = cp_ref*t_ref*rr_ref) ! J m^-3 s^-1
  parameter (pres_atm=1.01325e5) !in Pa. 

  !----------------------------------------------------------------------
  ! Reference quantities in cgs units

  real, parameter :: l_ref_cgs = l_ref*100.0 !cm
  real, parameter :: p_ref_cgs = p_ref*10.0  !in dynes/cm^2
  real, parameter :: rho_ref_cgs = rho_ref/1000.0 !in g/cm^3
  real, parameter :: rr_ref_cgs = rr_ref/1000.0 ! in g/cm^3 s^-1

  contains
!=========================================================================================
  subroutine initialize_reference(io,pr_transport)
  use topology_m, only : myid
  implicit none

  integer, intent(in):: io
  real, intent(out) :: pr_transport

  ! pr is also defined in transport_m and should be set here by passing as an argument
  pr_transport = pr

  if(myid .ne. 0) return

    write(io,*) 'initializing reference module...'
    write(io,*)

    write(io,*) 'the various reference values are as follows:'
    write(io,*)

    write(io,1) 'universal gas constant (J/mol-K)   = ',univ_gascon
    write(io,1) 'freestream temperature (K)         = ',t_o
    write(io,1) 'reference ratio of specifice heats = ',g_ref
    write(io,1) 'reference speed of sound (m/s)     = ',a_ref
    write(io,1) 'reference density (kg/m^3)         = ',rho_ref
    write(io,1) 'reference conductivity (W/m-K)     = ',lambda_ref
    write(io,1) 'reference temperature (K)          = ',t_ref
    write(io,1) 'reference pressure (Pa)            = ',p_ref
    write(io,1) 'standard atmospheric pressure (Pa) = ',pres_atm
    write(io,1) 'reference time (s)                 = ',time_ref
    write(io,1) 'reference specific heat (J/kg-K)   = ',cp_ref
    write(io,1) 'reference length (m)               = ',l_ref
    write(io,1) 'reference viscosity (Pa-s)         = ',mu_ref
    write(io,1) 'acoustic Reynolds number           = ',re
    write(io,1) 'Mach number                        = ',mach_no
    write(io,1) 'convective Reynolds number         = ',re*mach_no
    write(io,*)
    call write_header(io,'-')

  1 format(a40,1pe12.5)
  2 format(60('-'))

  return
  end subroutine initialize_reference
!-----------------------------------------------------------------------------------------
  end module reference_m

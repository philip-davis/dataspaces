#include "globalDefines.h"
! $Id:$
!----------------------------------------------------------------------
subroutine write_usdn(io)
use param_m
use variables_m, only: u, q
use reference_m, only: l_ref, a_ref, rho_ref, time_ref
use runtime_m, only: time
use topology_m
use grid_m
use premix_drvd_var_m
use stat_util_m
implicit none

integer, intent(in) :: io

integer i, j, k
real, dimension(nx, ny, nz) :: cprog, sd_uw , mag_grad, st
logical, dimension(nx, ny, nz) :: flag
real, dimension(nx, ny, nz, 3) :: normal, sdn, netspd
logical symm
character filename*200, time_ext*9

call calculate_progvar(cprog,io)
call calc_mag_grad_and_normal &
    (cprog, (/ -1.0, 0.0, 0.0 /), .true., mag_grad, normal)
!call calc_sd_uw(1.8/a_ref, 0.4261/rho_ref, .true., sd_uw)

!do k = 1, 3
!  netspd(:,:,:,k) = u(:,:,:,k) - sd_uw(:,:,:)*normal(:,:,:,k)
!end do

call calc_sdn_uw(sdn, flag)
netspd = sdn + u

if(myid .eq. 0) then
  write(time_ext, '(1pe9.3)') time*time_ref
  filename = '../post/tecplot/netspd-'//time_ext//'.tec'
  open(unit=625, file=filename, status='unknown')
  write(625, *) & 
      'variables = x[mm] y[mm] c grad sd sx sy sz st'
  write(625, 30) nx_g, ny_g
  write(625, 31) ((xg(i)*l_ref*1e3, i = 1, nx_g), j = 1, ny_g)
  write(625, 31) ((yg(j)*l_ref*1e3, i = 1, nx_g), j = 1, ny_g)
end if

call write_single_field(cprog, .true., 1.0)
call write_single_field(mag_grad, .true., 1.0/l_ref/1e3)
call write_single_field(sd_uw, .true., a_ref)

do k = 1, 3
  sdn(:,:,:,k) = sd_uw(:,:,:)*normal(:,:,:,k)
end do
netspd = u + sdn
st(:,:,:) = netspd(:,:,:,1)*netspd(:,:,:,1)+netspd(:,:,:,2)*netspd(:,:,:,2)
st = sqrt(st)

do k = 1, 3
  symm = .true. 
  if(k .eq. 2) symm = .false. 
  call write_single_field(netspd(:,:,:,k), symm, a_ref)
end do
call write_single_field(st, .true., a_ref)

if(myid .eq. 0) close(625)

30 format('zone t ="spd mean", i = ', i5, ', j = ', i5, ', f=block')
31 format(10(1pe12.5,1x))

contains

  !----------------------------------------
  subroutine write_single_field(field, symm2, ref)
  implicit none
  real, intent(in) :: field(nx, ny, nz)
  logical, intent(in) :: symm2
  real, intent(in) :: ref

  real, dimension(nx, ny, nz) :: wt
  logical, dimension(nx, ny, nz) :: ifcond
  real, dimension(nx, ny) :: field_mean

  wt = 1.0
  ifcond = .true. 
  call calc_z_mean(field, ifcond, wt, field_mean)
  !call calc_XYfld_Ysym(field_mean, symm2)
  call write_XYfld(625, field_mean, ref)

  return
  end subroutine write_single_field

end subroutine write_usdn

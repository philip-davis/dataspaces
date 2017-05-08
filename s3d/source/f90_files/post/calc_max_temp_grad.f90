#include "globalDefines.h"
subroutine calc_max_temp_grad(temp,max_temp_grad)

  use param_m 
  use topology_m
  use reference_m

  implicit none

  real, intent(in), dimension(nx,ny,nz)           :: temp
  real, intent(out)           :: max_temp_grad

  real, dimension(nx,ny,nz,3)         :: gradT
  real, dimension(nx,ny,nz)         :: abs_gradT

! ========================================================================================
  
  gradT = 0.0
  call computeScalarGradient( temp, gradT )

! dimensionalize gradT
  gradT = gradT*t_ref/l_ref   ! K/m

! Obtain the absoulte value of gradT
  abs_gradT = sqrt(gradT(:,:,:,1)**2.0+gradT(:,:,:,2)**2.0+gradT(:,:,:,3)**2.0)

  call MPI_Barrier( gcomm, ierr )  ! to ensure all processes have abs_gradT before applying Allreduce

! Perform reduction maximum operation 
  CALL MPI_Allreduce(maxval(abs_gradT), max_temp_grad, 1, MPI_DOUBLE_PRECISION, MPI_MAX, gcomm, ierr)

  return
end subroutine calc_max_temp_grad


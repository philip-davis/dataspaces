#include "globalDefines.h"
subroutine calc_avg_temp_grad(temp,avg_temp_grad)

  use param_m 
  use topology_m
  use reference_m

  implicit none

  real, intent(in), dimension(nx,ny,nz)           :: temp
  real, intent(out)           :: avg_temp_grad

  real, dimension(nx,ny,nz,3)         :: gradT
  real, dimension(nx,ny,nz)         :: abs_gradT
  real :: sum_abs_gradT, sum_abs_gradT_g
  integer :: i,j,k

! ========================================================================================
  
  gradT = 0.0
  call computeScalarGradient( temp, gradT )

! dimensionalize gradT
  gradT = gradT*t_ref/l_ref   ! K/m

! Obtain the absoulte value of gradT
  abs_gradT = sqrt(gradT(:,:,:,1)**2.0+gradT(:,:,:,2)**2.0+gradT(:,:,:,3)**2.0)

  sum_abs_gradT = 0.0
! Obtain sum of abs_gradT
  do i=1,nx
    do j=1,ny
       do k=1,nz
          sum_abs_gradT = sum_abs_gradT + abs_gradT(i,j,k)
       end do
    end do
  end do


  call MPI_Barrier( gcomm, ierr )  ! to ensure all processes have abs_gradT before applying Allreduce

! Perform reduction maximum operation 
  CALL MPI_Allreduce(sum_abs_gradT, sum_abs_gradT_g, 1, MPI_DOUBLE_PRECISION, MPI_SUM, gcomm, ierr)

! Average Temperature grad
  avg_temp_grad = sum_abs_gradT_g/(nx_g-1)**2

  return
end subroutine calc_avg_temp_grad


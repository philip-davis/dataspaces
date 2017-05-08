#include "globalDefines.h"
!============================================================================
subroutine calc_Sd(rr_r, Sd, U_n)
!============================================================================
#ifdef MIXAVG
  !-------------------------------------------------------------------!
  ! compute displacement speed                                        !
  !-------------------------------------------------------------------!
  use topology_m
  use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use grid_m, only : x, y, z, scale_1x, scale_1y, scale_1z
  use variables_m, only : u, yspecies, volum
  use transport_m, only : getDiffusionCoeff
  use thermchem_m, only : mixMW, avmolwt
  use work_m, only : tmp  => work1_1
  use work_m, only : tmp2 => work1_2
   
  implicit none 

  real, dimension(nx,ny,nz) :: rho, mag_grad_Ys, U_n, abs_grad_Ys, Sd, diff
  real, dimension(nx,ny,nz,3) :: grad_Ys, grad_mixMW, flame_normal_vec
  real, dimension(nx,ny,nz,n_spec) :: rr_r, Ds_mixavg
  real small, small_rr, small_fac, epsilon
  
  integer i, j, k, n, m
  
  small_fac = 1.0e-5
  epsilon = 1.0e-6

  rho = 1.0/volum 
  Ds_mixavg = getDiffusionCoeff()

  call computeScalarGradient(mixMW(:,:,:), grad_mixMW(:,:,:,:))
  do m = 1,3
    grad_mixMW(:,:,:,m) = grad_mixMW(:,:,:,m)*avmolwt(:,:,:)
  end do

  n = 4

  call computeScalarGradient(yspecies(:,:,:,n), grad_Ys(:,:,:,:))

  if (vary_in_x == 1) then
    tmp(:,:,:) = -Ds_mixavg(:,:,:,n) * (grad_Ys(:,:,:,1)  &
                 +yspecies(:,:,:,n)*grad_mixMW(:,:,:,1))
    call derivative_x( nx,ny,nz, tmp, tmp2, scale_1x, 1 )
    diff(:,:,:) = -tmp2
  else 
    diff(:,:,:) = 0.0
  endif

  if (vary_in_y == 1) then
    tmp(:,:,:) = -Ds_mixavg(:,:,:,n) * (grad_Ys(:,:,:,2) &
                 +yspecies(:,:,:,n)*grad_mixMW(:,:,:,2))
    call derivative_y( nx,ny,nz, tmp, tmp2, scale_1y, 1 )
    diff(:,:,:) = diff(:,:,:)-tmp2
  endif

  if (vary_in_z == 1) then
    tmp(:,:,:) = -Ds_mixavg(:,:,:,n) * (grad_Ys(:,:,:,3) &
                 +yspecies(:,:,:,n)*grad_mixMW(:,:,:,3))
    call derivative_z( nx,ny,nz, tmp, tmp2, scale_1z, 1 )
    diff(:,:,:) = diff(:,:,:)-tmp2
  endif

  mag_grad_Ys = sqrt(sum(grad_Ys**2,DIM=4))

  call MPI_ALLreduce(maxval(mag_grad_Ys),small,1,MPI_REAL8,MPI_MAX,gcomm,ierr)
  small=small*small_fac

  ! original Sd
  Sd(:,:,:) = (rr_r(:,:,:,n) + diff(:,:,:))/(mag_grad_Ys(:,:,:)+small)/rho(:,:,:)

  do i = 1,3
    flame_normal_vec(:,:,:,i) = -grad_Ys(:,:,:,i)/(mag_grad_Ys(:,:,:)+epsilon)
  enddo

! Local normal velocity
  if (vary_in_x == 1) then
    U_n(:,:,:) = u(:,:,:,1)*flame_normal_vec(:,:,:,1)
  else 
    U_n(:,:,:) = 0.0
  endif

  if (vary_in_y == 1) then
    U_n(:,:,:) = U_n(:,:,:) + u(:,:,:,2)*flame_normal_vec(:,:,:,2)
  endif

  if (vary_in_z == 1) then
    U_n(:,:,:) = U_n(:,:,:) + u(:,:,:,3)*flame_normal_vec(:,:,:,3)
  endif

#else
   write(*,*) 'calc_Sd only implemented for MIXAVG transport...'
   call terminate_run(io,0)
#endif
end subroutine calc_Sd

!============================================================================
subroutine calc_FlameIndex(FI)
!============================================================================
  use param_m, only : nx, ny, nz
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use variables_m, only : yspecies

  real, dimension(nx,ny,nz), intent(out) :: FI
  real, dimension(nx,ny,nz) :: mag_grad_YF, mag_grad_YO
  real, dimension(nx,ny,nz,3) :: grad_YF, grad_YO

  integer i

  call computeScalarGradient( yspecies(:,:,:,1), grad_YF )
  call computeScalarGradient( yspecies(:,:,:,2), grad_YO )

  FI = 0.0
  mag_grad_YF = 0.0
  mag_grad_YO = 0.0

  do i = 1,3
    mag_grad_YF(:,:,:) = mag_grad_YF(:,:,:) + grad_YF(:,:,:,i)**2
    mag_grad_YO(:,:,:) = mag_grad_YO(:,:,:) + grad_YO(:,:,:,i)**2
    FI(:,:,:) = FI(:,:,:) + grad_YF(:,:,:,i)*grad_YO(:,:,:,i)
  enddo

  mag_grad_YF = sqrt(mag_grad_YF)
  mag_grad_YO = sqrt(mag_grad_YO)

  FI = FI/(mag_grad_YF * mag_grad_YO + 1.0e-10)

end subroutine calc_FlameIndex

!============================================================================
subroutine calc_ScalDissRate(Chi)
!============================================================================
  use param_m, only : nx, ny, nz
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use variables_m, only : volum
  use transport_m
  use thermchem_m, only : cpmix
  use mixfrac_m, only : mixFrac

  real, dimension(nx,ny,nz), intent(out) :: Chi
  real, dimension(nx,ny,nz) :: rho, lambda
  real, dimension(nx,ny,nz,3) :: grad_mixfrac

  rho = 1.0/volum
  lambda = getThermalConductivity()

  call computeScalarGradient( mixFrac, grad_mixfrac )

  if (vary_in_x == 1) then
    Chi(:,:,:) = grad_mixfrac(:,:,:,1)**2
  else
    Chi(:,:,:) = 0.0
  endif

  if (vary_in_y == 1) then
    Chi(:,:,:) = Chi(:,:,:) + grad_mixfrac(:,:,:,2)**2
  endif

  if (vary_in_z == 1) then
    Chi(:,:,:) = Chi(:,:,:) + grad_mixfrac(:,:,:,3)**2
  endif

  Chi = 2.0*Chi*(lambda/rho/cpmix)

end subroutine calc_ScalDissRate

!============================================================================
subroutine calc_Da(rr_r,Da)
!============================================================================
#ifdef MIXAVG
  use topology_m
  use param_m, only : nx, ny, nz, n_spec
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use grid_m, only : scale_1x, scale_1y, scale_1z
  use variables_m, only : u, yspecies, temp, volum
  use reference_m
  use transport_m
  use thermchem_m, only : mixMW, avmolwt
  use work_m, only : tmp  => work1_1
  use work_m, only : tmp2 => work1_2

  real, dimension(nx,ny,nz), intent(out) :: Da
  real, dimension(nx,ny,nz,n_spec), intent(in)  :: rr_r

  real, dimension(nx,ny,nz)          :: rho
  real, dimension(nx,ny,nz,n_spec)   :: Ds_mixavg
  real, dimension(nx,ny,nz,3)        :: grad_Ys, grad_mixMW

 
  integer m, n
  real small, small_fac

  small_fac = 1.0e-5
  rho = 1.0/volum
  Ds_mixavg = getDiffusionCoeff()

  call computeScalarGradient(mixMW(:,:,:), grad_mixMW(:,:,:,:))
  do m = 1,3
    grad_mixMW(:,:,:,m) = grad_mixMW(:,:,:,m)*avmolwt(:,:,:)
  end do

!  SPECIES: do n=1,n_spec-1

   n = 4

     call computeScalarGradient( yspecies(:,:,:,n), grad_Ys(:,:,:,:))

     if (vary_in_x==1) then
        tmp(:,:,:) = -Ds_mixavg(:,:,:,n) * (grad_Ys(:,:,:,1)  &
                     +yspecies(:,:,:,n)*grad_mixMW(:,:,:,1))
        call derivative_x( nx,ny,nz, tmp, tmp2, scale_1x, 1 )
        Da(:,:,:) = -tmp2
     else
        Da(:,:,:) = 0.0
     endif

     if (vary_in_y==1) then
        tmp(:,:,:) = -Ds_mixavg(:,:,:,n) * (grad_Ys(:,:,:,2) &
                     +yspecies(:,:,:,n)*grad_mixMW(:,:,:,2))
        call derivative_y( nx,ny,nz, tmp, tmp2, scale_1y, 1 )
        Da(:,:,:) = Da(:,:,:)-tmp2
     endif

     if (vary_in_z==1) then
        tmp(:,:,:) = -Ds_mixavg(:,:,:,n) * (grad_Ys(:,:,:,3) &
                     +yspecies(:,:,:,n)*grad_mixMW(:,:,:,3))
        call derivative_z( nx,ny,nz, tmp, tmp2, scale_1z, 1 )
        Da(:,:,:) = Da(:,:,:)-tmp2
     endif

     call MPI_Allreduce(maxval(abs(Da)),small,1,MPI_REAL8,MPI_MAX,gcomm,ierr)
     small=small*small_fac

     Da(:,:,:) = abs(rr_r(:,:,:,n)) / (abs(Da(:,:,:))+small)

!  enddo SPECIES

#else 
     write(*,*) 'calc_Da is only implemented for mixture average transport'
     call terminate_run(io,0)
#endif
end subroutine calc_Da

!============================================================================
subroutine calc_VortTerms(vort,vort_term)
!============================================================================
  !-------------------------------------------------------------------!
  ! compute vortex terms of vorticity in z direction                 !
  !-------------------------------------------------------------------!
  use topology_m
  use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use grid_m, only : x, y, z, scale_1x, scale_1y, scale_1z
  use variables_m, only : u, volum, pressure
  use transport_m, only : getViscosity
  use work_m, only : tmp  => work1_1
  use work_m, only : tmp2 => work1_2
   
  implicit none 

  real, dimension(nx,ny,nz,3) :: vort
  real, dimension(nx,ny,nz,4) :: vort_term
  real, dimension(nx,ny,nz)   :: viscosity, rho

  integer i, j, k, n, m

! calculate vort_term(1) : vortex stretch
  if (vary_in_x==1) then
     call derivative_x( nx,ny,nz, u(:,:,:,3), tmp2, scale_1x, 1 )
     vort_term(:,:,:,1) = vort(:,:,:,1)*tmp2(:,:,:)
  else
     vort_term(:,:,:,1) = 0.0
  endif

  if (vary_in_y==1) then
     call derivative_y( nx,ny,nz, u(:,:,:,3), tmp2, scale_1y, 1 )
     vort_term(:,:,:,1) = vort_term(:,:,:,1)+vort(:,:,:,2)*tmp2
  endif

  if (vary_in_z==1) then
     call derivative_z( nx,ny,nz, u(:,:,:,3), tmp2, scale_1z, 1 )
     vort_term(:,:,:,1) = vort_term(:,:,:,1)+vort(:,:,:,3)*tmp2
  endif

! calculate vort_term(2) : expansion
  call computeDivergence(u,tmp) 
  vort_term(:,:,:,2) = -tmp(:,:,:)*vort(:,:,:,3)    ! expansion

! calculate vort_term(3) : diffusion
  viscosity=getViscosity() 
  if (vary_in_x==1) then
     call derivative_x( nx,ny,nz, vort(:,:,:,3), tmp2, scale_1x, 1 )
     tmp(:,:,:)=viscosity(:,:,:)*tmp2(:,:,:)
     call derivative_x( nx,ny,nz, tmp(:,:,:), tmp2, scale_1x, 1 )
     vort_term(:,:,:,3) = tmp2(:,:,:)
  else
     vort_term(:,:,:,3) = 0.0
  endif

  if (vary_in_y==1) then
     call derivative_y( nx,ny,nz, vort(:,:,:,3), tmp2, scale_1y, 1 )
     tmp(:,:,:)=viscosity(:,:,:)*tmp2(:,:,:)
     call derivative_y( nx,ny,nz, tmp(:,:,:), tmp2, scale_1y, 1 )
     vort_term(:,:,:,3) = vort_term(:,:,:,3)+tmp2(:,:,:)
  endif

  if (vary_in_z==1) then
     call derivative_z( nx,ny,nz, vort(:,:,:,3), tmp2, scale_1z, 1 )
     tmp(:,:,:)=viscosity(:,:,:)*tmp2(:,:,:)
     call derivative_z( nx,ny,nz, tmp(:,:,:), tmp2, scale_1z, 1 )
     vort_term(:,:,:,3) = vort_term(:,:,:,3)+tmp2(:,:,:)
  endif

! calculate vort_term(4) : baroclinic
  rho = 1.0/volum

  call derivative_x( nx,ny,nz, rho(:,:,:), tmp, scale_1x, 1 )
  call derivative_y( nx,ny,nz, pressure(:,:,:), tmp2, scale_1y, 1 )
  vort_term(:,:,:,4) = tmp(:,:,:)*tmp2(:,:,:)

  call derivative_y( nx,ny,nz, rho(:,:,:), tmp, scale_1y, 1 )
  call derivative_x( nx,ny,nz, pressure(:,:,:), tmp2, scale_1x, 1 )
  vort_term(:,:,:,4) = vort_term(:,:,:,4)-tmp(:,:,:)*tmp2(:,:,:)

  vort_term(:,:,:,4) = vort_term(:,:,:,4)/rho(:,:,:)**2

  return

end subroutine calc_VortTerms

!============================================================================
subroutine calc_ThermalDissRate_xy(gradT2_2D,gradT2_3D,DT)
!============================================================================
  use param_m, only : nx, ny, nz
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use variables_m, only : volum, temp
  use transport_m
  use thermchem_m, only : cpmix

  real, dimension(nx,ny,nz), intent(out) :: gradT2_2D, gradT2_3D, DT
  real, dimension(nx,ny,nz) :: rho, lambda
  real, dimension(nx,ny,nz,3) :: grad_temp

  rho = 1.0/volum
  lambda = getThermalConductivity()

  call computeScalarGradient( temp, grad_temp )

  if (vary_in_x == 1) then
    gradT2_2D(:,:,:) = grad_temp(:,:,:,1)**2
  else
    gradT2_2D(:,:,:) = 0.0
  endif

  if (vary_in_y == 1) then
    gradT2_2D(:,:,:) = gradT2_2D(:,:,:) + grad_temp(:,:,:,2)**2
  endif

  if (vary_in_z == 1) then
    gradT2_3D(:,:,:) = gradT2_2D(:,:,:) + grad_temp(:,:,:,3)**2
  endif

  DT=(lambda/rho/cpmix)

return
end subroutine calc_ThermalDissRate_xy

!============================================================================
subroutine calc_ThermalDissRate_yz(gradT2_2D,gradT2_3D,DT)
!============================================================================
  use param_m, only : nx, ny, nz
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use variables_m, only : volum, temp
  use transport_m
  use thermchem_m, only : cpmix

  real, dimension(nx,ny,nz), intent(out) :: gradT2_2D, gradT2_3D, DT
  real, dimension(nx,ny,nz) :: rho, lambda
  real, dimension(nx,ny,nz,3) :: grad_temp

  rho = 1.0/volum
  lambda = getThermalConductivity()

  call computeScalarGradient( temp, grad_temp )

  gradT2_2D(:,:,:) = 0.0
  gradT2_3D(:,:,:) = 0.0

  if (vary_in_y == 1) then
    gradT2_2D(:,:,:) = grad_temp(:,:,:,2)**2
  endif

  if (vary_in_z == 1) then
    gradT2_2D(:,:,:) = gradT2_2D(:,:,:) + grad_temp(:,:,:,3)**2
  endif

  if (vary_in_x == 1) then
    gradT2_3D(:,:,:) = gradT2_2D(:,:,:) + grad_temp(:,:,:,1)**2
  endif

  DT=(lambda/rho/cpmix)

return
end subroutine calc_ThermalDissRate_yz

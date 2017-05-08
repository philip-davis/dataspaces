#include "globalDefines.h"
! $Id: premix_drvd_var.f90,v 1.1.2.5 2006/08/18 02:44:47 rsankar Exp $
module premix_drvd_var_m
!----------------------------------------------------------------------
! For postprocessing Ramanan's bunsen flame
! Surface routines cut and pasted from Evatt's stuff
!----------------------------------------------------------------------
use param_m, only : nx, ny, nz, n_spec, nvar_tot
use runtime_m, only: run_title, tstep
implicit none
public 
real, private, parameter::small_fac=1e-5  !numerical fix for limit behaviour

contains
!----------------------------------------------------------------------
subroutine calculate_progvar(c,io)
use variables_m, only: yspecies
use zclookup_m, only: SpecToProg, PartProg

implicit none
integer, intent(in) :: io
real, intent(out), dimension(nx,ny,nz) :: c


 call SpecToProg(yspecies,io)
 c=PartProg

return
end subroutine calculate_progvar

!----------------------------------------------------------------------
!  !----------------------------------------------------------------------
!  subroutine calculate_progvar(c)
!  use variables_m, only: yspecies
!  implicit none
!  real, intent(out), dimension(nx,ny,nz) :: c
!  
!  ! Know Thy Chemistry! - Ramanan(5:32)
!  !----------------------------------------
!  ! Methane is species #9
!  ! Unburnt Y is 0.0392815 (from clookup.in)
!  ! c = 1.0 - yspecies(:,:,:,9)/0.0392815
!  !----------------------------------------
!  ! Oxygen is species #4
!  ! Unburnt YO2 is 0.223855. Burnt YO2 is 0.066943  (from clookup.in)
!  c = 1.0 - (yspecies(:,:,:,4)-0.066943)/(0.223855-0.066943)
!  !----------------------------------------
!  ! CO is species #10 CO2 is species#11
!  ! Burnt YCO+YCO2 is 0.10655 (from clookup.in)
!  !c = (yspecies(:,:,:,10)+yspecies(:,:,:,11))/0.10655
!  !----------------------------------------
!  
!  return
!  end subroutine calculate_progvar

!----------------------------------------------------------------------
subroutine calc_mag_grad (scalar, mag_grad)
implicit none
real, intent(in), dimension(nx,ny,nz) :: scalar 
real, intent(out), dimension(nx,ny,nz) :: mag_grad
real, dimension(nx,ny,nz,3) :: normal

call computeScalarGradient(scalar,normal)
mag_grad=sqrt(sum(normal**2,DIM=4))

return
end subroutine calc_mag_grad

!----------------------------------------------------------------------
subroutine calc_normal(scalar, norm_limit, flip, normal)
implicit none
real, intent(in), dimension(nx,ny,nz) :: scalar 
real, intent(in), dimension(3) :: norm_limit   !limiting value of normal
logical, intent(in) :: flip                    !flag for direction
real, intent(out), dimension(nx,ny,nz,3) :: normal     
real, dimension(nx, ny, nz) :: mag_grad

call calc_mag_grad_and_normal(scalar, norm_limit, flip, mag_grad, normal)
return
end subroutine calc_normal

!----------------------------------------------------------------------
subroutine calc_mag_grad_and_normal  &
      (scalar, norm_limit, flip, mag_grad, normal)
! -calculates the magnitude of the scalar gradient
! -also this routine calculates the scalar isosurface normals.
!  (sign convention depends on 'flip')
! 
! Routine calculates the normals for the local isosurface.  
! Where gradients tend to zero, a zero over zero limit is encountered.  
! To avoid this problem the normal is set to the user defined limits
use topology_m
implicit none

! declarations: input
real, intent(in), dimension(nx,ny,nz) :: scalar 
real, intent(in), dimension(3) :: norm_limit   !limiting value of normal
logical, intent(in) :: flip                    !flag for direction
real, intent(out), dimension(nx,ny,nz) :: mag_grad     
real, intent(out), dimension(nx,ny,nz,3) :: normal     

real :: small
integer m

call computeScalarGradient(scalar,normal)
mag_grad=sqrt(sum(normal**2,DIM=4))

! set small number
call MPI_Allreduce(maxval(mag_grad),small,1,MPI_REAL8,MPI_MAX,gcomm,ierr)
small=small*small_fac

! Usually the normal would point in the direction of increasing scalar. 
! Unless.... a flip is requested.
if(flip) normal = -normal
! calculate the unit normals, taking care of the zero/zero issue:
do m = 1, 3
  normal(:,:,:,m)=(normal(:,:,:,m)+norm_limit(m)*small) &
                  /(mag_grad(:,:,:)+small)
end do

return
end subroutine calc_mag_grad_and_normal

!----------------------------------------------------------------------
! subroutine calc_shape_factor(scalar, sf)
! use topology_m
! implicit none
! real, intent(in), dimension(nx, ny, nz) :: scalar
! real, intent(out), dimension(nx, ny, nz) :: sf
! 
! real, dimension(nx, ny, nz, 3) :: gradc
! real, dimension(nx, ny, nz) :: mag_grad
! real, dimension(nx, ny, nz, 3, 3) :: ggc
! real small
! integer m
! integer i, j, k
! 
! call computeScalarGradient(scalar, gradc)
! mag_grad=sqrt(sum(gradc**2,DIM=4))
! 
! ! set small number
! call MPI_Allreduce(maxval(mag_grad),small,1,MPI_REAL8,MPI_MAX,gcomm,ierr)
! small=small*small_fac
! 
! do m = 1, 3
!   call computeScalarGradient(gradc(:,:,:,m), ggc(:,:,:,:,m))
! end do
! 
! do k = 1, nz
!   do j = 1, ny
!     do i = 1, nx
!       if(mag_grad(i, j, k) .lt. small) then
!         sf(i, j, k) = 0.0
!       else
!         sf(i, j, k) = shape_factor & 
!         (gradc(i,j,k,1), gradc(i,j,k,2), gradc(i,j,k,3), mag_grad(i,j,k), &
!          ggc(i, j, k, 1, 1), ggc(i, j, k, 2, 1), ggc(i, j, k, 3, 1), &
!          ggc(i, j, k, 1, 2), ggc(i, j, k, 2, 2), ggc(i, j, k, 3, 2), &
!          ggc(i, j, k, 1, 3), ggc(i, j, k, 2, 3), ggc(i, j, k, 3, 3) )
!       end if
!     end do
!   end do
! end do
! 
! return
! 
!   contains
!   !----------------------------------------
!   ! code from Vaidya
!   !----------------------------------------
!   function shape_factor (Cx, Cy, Cz, del_c, &
!       Cxx, Cxy, Cxz, Cyx, Cyy, Cyz, Czx, Czy, Czz)
!   implicit none
!   real, intent(in) ::  Cx, Cy, Cz, del_c
!   real, intent(in) ::  Cxx, Cxy, Cxz, Cyx, Cyy, Cyz, Czx, Czy, Czz 
!   real :: shape_factor
! 
!   real gg, gg3
!   real rgg, rgg3
!   real h11, h12, h13, h21, h22, h23, h31, h32, h33
!   real aa, bb, cc, root
!   real xlam1, xlam2
! 
!   if(del_c .eq. 0.0) then
!     shape_factor = 0.0
!     return
!   end if
! 
!   gg  = del_c 
!   gg3 = del_c**3
! 
!   rgg  = 1.0/gg
!   rgg3 = 1.0/gg**3
! 
!   h11 = -Cxx*rgg + rgg3*(Cx*Cx*Cxx + Cx*Cy*Cxy + Cx*Cz*Cxz)
!   h12 = -Cxy*rgg + rgg3*(Cx*Cx*Cyx + Cx*Cy*Cyy + Cx*Cz*Cyz)
!   h13 = -Cxz*rgg + rgg3*(Cx*Cx*Czx + Cx*Cy*Cyz + Cx*Cz*Czz)
! 
!   h21 = -Cyx*rgg + rgg3*(Cy*Cx*Cxx + Cy*Cy*Cxy + Cy*Cz*Cxz)
!   h22 = -Cyy*rgg + rgg3*(Cy*Cx*Cyx + Cy*Cy*Cyy + Cy*Cz*Cyz)
!   h23 = -Cyz*rgg + rgg3*(Cy*Cx*Czx + Cy*Cy*Czy + Cy*Cz*Czz)
! 
!   h31 = -Czx*rgg + rgg3*(Cz*Cx*Cxx + Cz*Cy*Cxy + Cz*Cz*Cxz)
!   h32 = -Czy*rgg + rgg3*(Cz*Cx*Cyx + Cz*Cy*Cyy + Cz*Cz*Cyz)
!   h33 = -Czz*rgg + rgg3*(Cz*Cx*Czx + Cz*Cy*Czy + Cz*Cz*Czz)
! 
!   aa = -1.0
!   bb = h11 + h22 + h33
!   cc = h12*h21 + h13*h31 + h23*h32 - h11*h22 - h11*h33 - h22*h33
! 
!   root = bb*bb - 4.0*aa*cc
! 
!   if (root .lt. 0.0) root = 0.0
! 
!   root = sqrt(root)
!   xlam1 = (-bb + root)/(2.0*aa)
!   xlam2 = (-bb - root)/(2.0*aa)
! 
!   if(abs(xlam1).gt. abs(xlam2)) then
!     !swap such that xlam2 is greater
!     root = xlam1
!     xlam1 = xlam2
!     xlam2 = root
!   end if
! 
!   if(xlam2 .eq. 0.0) then
!     shape_factor = 0.0
!   else
!     shape_factor = xlam1/xlam2
!   end if
! 
!   RETURN
!   end function shape_factor
!  
! end subroutine calc_shape_factor

!----------------------------------------------------------------------
subroutine calc_normal_deriv(field, normal, deriv)
implicit none
real, intent(in), dimension(nx, ny, nz) :: field
real, intent(in), dimension(nx, ny, nz, 3) :: normal
real, intent(out), dimension(nx, ny, nz) :: deriv

real, dimension(nx, ny, nz, 3) :: gradient

call computeScalarGradient(field,gradient)
!Take the dot product with normal
deriv = normal(:,:,:,1)*gradient(:,:,:,1) &
      + normal(:,:,:,2)*gradient(:,:,:,2) &
      + normal(:,:,:,3)*gradient(:,:,:,3)
return
end subroutine calc_normal_deriv

!----------------------------------------------------------------------
subroutine calc_normal_strain(normal,strain)
! -calculates the normal fluid strain of isosurfaces of the scalar, given the 
!  normal vector to the isosurface
  use variables_m, only : u
  implicit none

  real, intent(in), dimension(nx,ny,nz,3) :: normal      !isosurface normal
  real, intent(out), dimension(nx,ny,nz) :: strain       !strain

  integer i,j
  real grad_u(nx,ny,nz,3,3)

  call computeVectorGradient(u,grad_u)

  strain=0.0
! now calculate the tensor sum ni nj du_i/dx_j (the normal strain)
  do i=1,3
    do j=1,3
      strain(:,:,:)=strain(:,:,:) + &
                    normal(:,:,:,i)*normal(:,:,:,j)*grad_u(:,:,:,j,i)
    enddo
  enddo

  return
end subroutine calc_normal_strain
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine calc_strain(normal,strain)
! -calculates the fluid strain of isosurfaces of the scalar, given the 
!  normal vector to the isosurface
  use variables_m, only : u
  implicit none

  real, intent(in), dimension(nx,ny,nz,3) :: normal      !isosurface normal
  real, intent(out), dimension(nx,ny,nz) :: strain       !strain

  integer i,j
  real grad_u(nx,ny,nz,3,3)

  call computeVectorGradient(u,grad_u)

  strain=0.0
! now calculate the tensor sum ni nj du_i/dx_j (the normal strain)
  do i=1,3
    do j=1,3
      strain(:,:,:)=strain(:,:,:) + &
                    normal(:,:,:,i)*normal(:,:,:,j)*grad_u(:,:,:,j,i)
    enddo
  enddo

! take away the normal strain from the dilatation to get tangential strain
  strain(:,:,:)=grad_u(:,:,:,1,1)+grad_u(:,:,:,2,2)+grad_u(:,:,:,3,3)-strain(:,:,:)

  return
end subroutine calc_strain
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine calculate_dissipation(big_u, lil_u, volum, epsln)
!----------------------------------------
! - routine calculates the dissipation of kinetic energy
! - Needs the big_u which is mean+prime velocities for calculating
!   the stress tensor.
! - Needs little u to calculate the gradient tensor, which is then 
!   contracted against the stress tensor.
!   Little u is big_u - favre_mean_u
!----------------------------------------
use transport_m, only: computestresstensor
implicit none

real, dimension(nx, ny, nz, 3), intent(in) :: big_u, lil_u
real, dimension(nx, ny, nz), intent(in) :: volum
real, dimension(nx, ny, nz), intent(out) :: epsln
real, dimension(nx, ny, nz, 3, 3) :: grad_u, tau

! Compute a grad_u based on the big_u
call computevectorgradient(big_u, grad_u)
tau = grad_u
call computeStressTensor(tau)

! Recompute grad_u based on the lil_u
call computevectorgradient(lil_u, grad_u)

! contract the stress with the velocity gradients to form the dissipation
! epsilon=tau_{ij} du_j/dx_i

  epsln(:,:,:)       = tau(:,:,:,1,1)*grad_u(:,:,:,1,1)    &
                       + tau(:,:,:,1,2)*grad_u(:,:,:,2,1)    &
                       + tau(:,:,:,1,3)*grad_u(:,:,:,3,1)    &
                       + tau(:,:,:,2,1)*grad_u(:,:,:,1,2)    &
                       + tau(:,:,:,2,2)*grad_u(:,:,:,2,2)    &
                       + tau(:,:,:,2,3)*grad_u(:,:,:,3,2)    &
                       + tau(:,:,:,3,1)*grad_u(:,:,:,1,3)    &
                       + tau(:,:,:,3,2)*grad_u(:,:,:,2,3)    &
                       + tau(:,:,:,3,3)*grad_u(:,:,:,3,3)    

! divide by density
  epsln(:,:,:)=epsln(:,:,:)*volum

return
end subroutine calculate_dissipation
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine calculate_dissipation_tensor(big_u, lil_u, tau)
!----------------------------------------
! - routine calculates the inverse of the dissipation tensor
! - Needs the big_u which is mean+prime velocities for calculating
!   the stress tensor.
! - Needs little u to calculate the gradient tensor
!   Little u is big_u - favre_mean_u
!----------------------------------------
use variables_m, only : q, temp, pressure, yspecies, volum, u
use transport_m, only: computestresstensor, computecoefficients, getViscosity
use thermchem_m, only: cpmix
implicit none

real, dimension(nx, ny, nz, 3), intent(in) :: big_u, lil_u
!real, dimension(nx, ny, nz), intent(in) :: volum
real, dimension(nx, ny, nz, 3, 3), intent(out) :: tau
real, dimension(nx, ny, nz, 3, 3) :: grad_u
real, dimension(nx, ny, nz) :: det, vis

integer m,n,i

! Compute grad_u based on the lil_u




!WARNING THIS IS TAKING THE WHOLE VELOCITY
!call computevectorgradient(lil_u, grad_u)
call computevectorgradient(u, grad_u)


#ifdef LEWIS
call computeCoefficients( Cpmix, Temp )
#endif
#ifdef MIXAVG
call computeCoefficients( pressure, Temp, yspecies, 1.0/volum )
#endif
vis(:,:,:)=getViscosity()


!  ! find the dissipation rate tensor, use tau to hold it.
!  ! contract the stress with the velocity gradients to form the dissipation
!  ! epsilon{ij}=tau_{ij} du_j/dx_i
!  
!    tau(:,:,:,1,1)       = tau(:,:,:,1,1)*grad_u(:,:,:,1,1)    
!    tau(:,:,:,1,2)       = tau(:,:,:,1,2)*grad_u(:,:,:,2,1)    
!    tau(:,:,:,1,3)       = tau(:,:,:,1,3)*grad_u(:,:,:,3,1)    
!    tau(:,:,:,2,1)       = tau(:,:,:,2,1)*grad_u(:,:,:,1,2)    
!    tau(:,:,:,2,2)       = tau(:,:,:,2,2)*grad_u(:,:,:,2,2)    
!    tau(:,:,:,2,3)       = tau(:,:,:,2,3)*grad_u(:,:,:,3,2)    
!    tau(:,:,:,3,1)       = tau(:,:,:,3,1)*grad_u(:,:,:,1,3)    
!    tau(:,:,:,3,2)       = tau(:,:,:,3,2)*grad_u(:,:,:,2,3)    
!    tau(:,:,:,3,3)       = tau(:,:,:,3,3)*grad_u(:,:,:,3,3)    
!  
!  ! divide by density
!    do n=1,3
!     do m=1,3
!      tau(:,:,:,m,n)=tau(:,:,:,m,n)*volum
!     enddo
!    enddo

    tau = 0.0
    do n=1,3
    do m=1,3
    do i=1,3
    tau(:,:,:,m,n)       = tau(:,:,:,m,n) + grad_u(:,:,:,i,m)*grad_u(:,:,:,i,n)
    enddo

    tau(:,:,:,m,n)       = tau(:,:,:,m,n) * 2.0 * vis(:,:,:) * volum(:,:,:)

!    invepsln(:,:,:,m,n)   = 1.0 / max(tau(:,:,:,m,n),1.0e-20)

    enddo 
    enddo




! find the determinant of the dissipation tensor

!   det(:,:,:) = tau(:,:,:,1,1)*(tau(:,:,:,3,3)*tau(:,:,:,2,2)-tau(:,:,:,3,2)*tau(:,:,:,2,3)) &
!              - tau(:,:,:,2,1)*(tau(:,:,:,3,3)*tau(:,:,:,1,2)-tau(:,:,:,3,2)*tau(:,:,:,1,3)) &
!              + tau(:,:,:,3,1)*(tau(:,:,:,2,3)*tau(:,:,:,1,2)-tau(:,:,:,2,2)*tau(:,:,:,1,3)) 
!   
!   ! find the inverse of the dissipation tensor
!   
!   invepsln(:,:,:,1,1)       = +(tau(:,:,:,3,3)*tau(:,:,:,2,2)-tau(:,:,:,3,2)*tau(:,:,:,2,3))
!   invepsln(:,:,:,1,2)       = -(tau(:,:,:,3,3)*tau(:,:,:,1,2)-tau(:,:,:,3,2)*tau(:,:,:,1,3))
!   invepsln(:,:,:,1,3)       = +(tau(:,:,:,2,3)*tau(:,:,:,1,2)-tau(:,:,:,2,2)*tau(:,:,:,1,3))
!   invepsln(:,:,:,2,1)       = -(tau(:,:,:,3,3)*tau(:,:,:,2,1)-tau(:,:,:,3,1)*tau(:,:,:,2,3))
!   invepsln(:,:,:,2,2)       = +(tau(:,:,:,3,3)*tau(:,:,:,1,1)-tau(:,:,:,3,1)*tau(:,:,:,1,3))
!   invepsln(:,:,:,2,3)       = -(tau(:,:,:,2,3)*tau(:,:,:,1,1)-tau(:,:,:,2,1)*tau(:,:,:,1,3))
!   invepsln(:,:,:,3,1)       = +(tau(:,:,:,3,2)*tau(:,:,:,2,1)-tau(:,:,:,3,1)*tau(:,:,:,2,2))
!   invepsln(:,:,:,3,2)       = -(tau(:,:,:,3,2)*tau(:,:,:,1,1)-tau(:,:,:,3,1)*tau(:,:,:,1,2))
!   invepsln(:,:,:,3,3)       = +(tau(:,:,:,2,2)*tau(:,:,:,1,1)-tau(:,:,:,2,1)*tau(:,:,:,1,2))
!   
!     do n=1,3
!      do m=1,3
!       invepsln(:,:,:,m,n)=invepsln(:,:,:,m,n)/det(:,:,:)
!      enddo
!     enddo

return
end subroutine calculate_dissipation_tensor
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Subroutines to calculate Sd and its individual terms
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! calculates the rate of change rho*DY/Dt
! given the linear constant weightings for species 
subroutine calc_rhoDYDt(weightings, DDt)
use variables_m, only : q, temp, pressure, yspecies, volum, u
use reference_m
use thermchem_m, only: cpmix
use transport_m, only: computecoefficients, getViscosity
#ifdef LEWIS
use transport_m, only: InvSc
#endif
#ifdef MIXAVG
use transport_m, only: computeSpeciesDiffFlux
#endif
use grid_m, only: scale_1x, scale_1y, scale_1z
use chemkin_m, only: reaction_rate
implicit none

real, intent(in), dimension(n_spec) :: weightings
real, intent(out), dimension(nx,ny,nz) :: DDt

real, dimension(nx,ny,nz,n_spec) :: rr_all, diffusion
real, dimension(nx,ny,nz) :: vis, flux, diff
#ifdef LEWIS
real, dimension(nx,ny,nz,3) :: gradY
#endif
#ifdef MIXAVG
real, dimension(nx,ny,nz,3) :: grad_T
real, dimension(nx,ny,nz,n_spec,3) :: gradYall
#endif

integer m, L

DDt=0.0

#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_all,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
call reaction_rate &
     (rr_all,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif

#ifdef MIXAVG
  call computeCoefficients(pressure,temp,yspecies,1.0/volum)

! we can use the getSpeciesDiffFlux routine
  do L=1,n_spec
  call computeScalarGradient(yspecies(:,:,:,L), gradYall(:,:,:,L,:))
  enddo
  call derivative_x(nx, ny, nz, temp(:,:,:), grad_T(:,:,:,1), scale_1x, 1)
  call derivative_y(nx, ny, nz, temp(:,:,:), grad_T(:,:,:,2), scale_1y, 1)
  call derivative_z(nx, ny, nz, temp(:,:,:), grad_T(:,:,:,3), scale_1z, 1)

! J_i = -D * rho * dY/dx_i
  call computeSpeciesDiffFlux( Temp, grad_T, Yspecies, gradYall, Pressure, 1.0/volum)

Loop_sp: do L=1,n_spec
  if(weightings(L).eq.0.0) cycle Loop_sp
  DDt = DDt + weightings(L)*rr_all(:,:,:,L)
  call derivative_x(nx, ny, nz, gradYall(:,:,:,L,1), flux, scale_1x, 1)
  DDt = DDt - weightings(L)*flux
  call derivative_y(nx, ny, nz, gradYall(:,:,:,L,2), flux, scale_1y, 1)
  DDt = DDt - weightings(L)*flux
  call derivative_z(nx, ny, nz, gradYall(:,:,:,L,3), flux, scale_1z, 1)
  DDt = DDt - weightings(L)*flux
enddo Loop_sp
#endif

#ifdef LEWIS
  call computeCoefficients(cpmix, temp)
  vis = getViscosity()

Loop_sp: do L=1,n_spec
  if(weightings(L).eq.0.0) cycle Loop_sp
  DDt = DDt + weightings(L)*rr_all(:,:,:,L)
  call computeScalarGradient(yspecies(:,:,:,L), gradY)
  DIRECTION: do m = 1, 3
    gradY(:,:,:,m) = -vis(:,:,:)*Invsc(L)*gradY(:,:,:,m)
  end do DIRECTION
  call derivative_x(nx, ny, nz, gradY(:,:,:,1), flux, scale_1x, 1)
  DDt = DDt - weightings(L)*flux
  call derivative_y(nx, ny, nz, gradY(:,:,:,2), flux, scale_1y, 1)
  DDt = DDt - weightings(L)*flux
  call derivative_z(nx, ny, nz, gradY(:,:,:,3), flux, scale_1z, 1)
  DDt = DDt - weightings(L)*flux
enddo Loop_sp
#endif

return
end subroutine calc_rhoDYDt

!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_sd_uw(sd_farfield, rho_0, flip, sd_uw)
!----------------------------------------------------------------------
! this routine calculates the unweighted displacement
! speed of an isosurface. 
! there is a 0/0 limit that occurs in the fresh and burnt gases.  
! this problem is avoided here by forcing the speed to 
! tend to a user specified limit value as the scalar gradient 
! and the rate of change vanishes.  it is recommended to use 
! the laminar flame speed for this limit value in premixed combustion. 
! in non-premixed combustion, a limit value of 0 is probably appropriate.
!----------------------------------------------------------------------
use variables_m, only : q, volum, yspecies
use topology_m
implicit none
real, intent(in) :: sd_farfield  !limit value of flame speed
real, intent(in) :: rho_0        !normalising density 
logical, intent(in) :: flip
real, intent(out), dimension(nx,ny,nz) :: sd_uw  !unweighted flame speed

real, dimension(nx,ny,nz) :: ddt
real, dimension(nx,ny,nz) :: mag_grad
real, dimension(n_spec) :: weight
real small               !fudge factor
                   
weight(:) = 0.0
weight(4) = 1.0
call calc_rhodydt(weight, ddt)
call calc_mag_grad(yspecies(:,:,:,4), mag_grad)

! set small number
call mpi_allreduce(maxval(mag_grad),small,1,mpi_real8,mpi_max,gcomm,ierr)
small=small*small_fac
if (flip) ddt = -ddt

! divide by the magnitude of the scalar gradient, 
! taking care of the zero/zero limit
sd_uw = volum*(ddt + sd_farfield*small*rho_0)/(mag_grad+small)

return
end subroutine calc_sd_uw

!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_sd(sd_farfield, rho_0, flip, sd,io)
!----------------------------------------------------------------------
! This routine calculates the density weighted displacement
! speed of an isosurface. 
! There is a 0/0 limit that occurs in the fresh and burnt gases.  
! This problem is avoided here by forcing the speed to 
! tend to a user specified limit value as the scalar gradient 
! and the rate of change vanishes.  It is recommended to use 
! the laminar flame speed for this limit value in premixed combustion. 
! In non-premixed combustion, a limit value of 0 is probably appropriate.

! modified 10Mar08 by Ed Richardosn to give the sd of the partially premixed 
! progress variable.

!----------------------------------------------------------------------
use variables_m, only : q, volum, yspecies
use topology_m
use zclookup_m
implicit none
real, intent(in) :: sd_farfield  !limit value of flame speed
real, intent(in) :: rho_0        !normalising density 
logical, intent(in) :: flip
integer, intent(in) :: io
real, intent(out), dimension(nx,ny,nz) :: sd  !density weighted flame speed

real, dimension(nx,ny,nz) :: DDt
real, dimension(nx,ny,nz) :: DDt_Z
real, dimension(nx,ny,nz) :: mag_grad
real, dimension(n_spec) :: weight
real small               !fudge factor

  select case (trim(run_title))
  case ('bunsen')

call calc_sd_uw(sd_farfield, rho_0, flip, sd)
sd = sd*q(:,:,:,4,1)/rho_0

   case default                                 ! partially premixed option

call initialize_zcpost(io,+1)

weight(:) = 0.0
weight(n_spec_prog) = 1.0
call calc_rhoDYDt(weight, DDt)

weight(:) = 0.0
weight(n_spec_mf) = 1.0
call calc_rhoDYDt(weight, DDt_Z)

call SpecToProg(yspecies,io)
call zcderiv_field(io)

! DDt(:,:,:) = yderiv_field(:,:,:,2)*DDt(:,:,:) + &
!              yderiv_field(:,:,:,1)*DDt_Z(:,:,:)

  DDt(:,:,:) = (DDt(:,:,:)-DDt_Z(:,:,:)*yderiv_field(:,:,:,1))/yderiv_field(:,:,:,2)

!call calc_mag_grad(yspecies(:,:,:,4), mag_grad)
call calc_mag_grad(PartProg(:,:,:),mag_grad)

! set small number
call MPI_Allreduce(maxval(mag_grad),small,1,MPI_REAL8,MPI_MAX,gcomm,ierr)
small=small*small_fac*rho_0
if (flip) DDt = -DDt

! divide by the magnitude of the scalar gradient, 
! taking care of the zero/zero limit
sd = (DDt + sd_farfield*small)/(mag_grad*rho_0+small)

end select

return
end subroutine calc_sd

!----------------------------------------------------------------------
subroutine calc_sd_comp_uw & 
         (sd_farfield, rho_0, flip, sd_rr_uw, sd_nd_uw, sd_curv_uw, sd_uw)
! compute the unweighted sd and its components:
! reaction, normal_diffusion and curvature.
! needs the far-field displacement speed
! to treat the division by zero limit.
use variables_m, only : q, temp, pressure, yspecies, volum, u
use reference_m
use topology_m
#ifdef LEWIS
use transport_m, only: getspcsdiffusivity
#endif
use chemkin_m, only: reaction_rate
implicit none
real, intent(in) :: sd_farfield
real, intent(in) :: rho_0        !normalising density 
logical, intent(in) :: flip
real, intent(out), dimension(nx,ny,nz) :: sd_rr_uw, sd_nd_uw, sd_curv_uw, sd_uw

real, dimension(nx,ny,nz) :: ddt, ddt_rr, ddt_nd, ddt_curv, ddt_rr_nd
real, dimension(nx,ny,nz) :: mag_grad, curv, diff
real, dimension(nx,ny,nz,3) :: normal
real, dimension(nx,ny,nz,n_spec) :: rr_all, diffusion
real, dimension(n_spec) :: weight
real small               !fudge factor

#ifdef MIXAVG
write(*,*)'calc_sd_comp_uw is not written for MIXAVG, needs attention'
#endif


! first compute the full dy/dt term
weight(:) = 0.0
weight(4) = 1.0
call calc_rhodydt(weight, ddt)

!calculate the curvature term
call calc_mag_grad_and_normal  &
      (yspecies(:,:,:,4), (/0.0, -1.0, 0.0/), .false., mag_grad, normal)
      ! dont flip the normal. it is done when calculating sd. 
call computedivergence(normal, curv)
#ifdef LEWIS
diff = getspcsdiffusivity(4)
#endif
ddt_curv = diff*curv

!calculate the reaction term
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_all,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
call reaction_rate(rr_all,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
ddt_rr = rr_all(:,:,:,4)

!calculate the normal diffusion term
ddt_nd = ddt - ddt_curv - ddt_rr


!calculate the unweighted sd components
! set small number
call mpi_allreduce(maxval(mag_grad),small,1,mpi_real8,mpi_max,gcomm,ierr)
small=small*small_fac

if(flip) then 
  ddt = -ddt
  ddt_curv = -ddt_curv
  ddt_rr = -ddt_rr
  ddt_nd = -ddt_nd
end if

! divide by the magnitude of the scalar gradient, 
! taking care of the zero/zero limit
sd_uw = volum*(ddt + sd_farfield*small*rho_0)/(mag_grad+small)
ddt_rr_nd = ddt_rr + ddt_nd
where(mag_grad.lt.small .and. ddt_rr_nd .gt. 0.0)
  sd_rr_uw = sd_uw*(ddt_rr/(ddt_rr_nd))
  sd_nd_uw = sd_uw*(ddt_nd/(ddt_rr_nd))
  sd_curv_uw = 0.0
elsewhere(mag_grad.lt.small .and. ddt_rr_nd .eq. 0.0)
  sd_rr_uw = sd_uw*0.5
  sd_nd_uw = sd_uw*0.5
  sd_curv_uw = 0.0
elsewhere
  sd_rr_uw = volum*ddt_rr/mag_grad
  sd_nd_uw = volum*ddt_nd/mag_grad
  sd_curv_uw = volum*ddt_curv/mag_grad
end where


return
end subroutine calc_sd_comp_uw

!----------------------------------------------------------------------
subroutine calc_sd_comp & 
         (reac_ff, nd_ff, rho_0, flip, sd_reac, sd_nd, sd_curv, &
            sd_mixf, sd_crss, sd_ddif, sd_uw,io)

! modified to give the displacement speed of the partially premixed
! progress variable. This leads to the addition of some new terms:
!
! Progress variable dissipation (this is zero and not returned)
! Mixture fraction dissipation
! Cross dissipation
! Differential diffusion
         
! Compute the unweighted sd and its components:
! reaction, normal diffusion and curvature.
! Needs the far-field values of reaction and normal diffusion components
! to treat the division by zero limit.
use variables_m, only : q, temp, pressure, yspecies, volum, u
use reference_m
use topology_m
#ifdef LEWIS
use transport_m, only: getSpcsDiffusivity
#endif
use chemkin_m, only: reaction_rate
use zclookup_m
implicit none
real, intent(in) :: reac_ff, nd_ff !far field values for division by zero limit
real, intent(in) :: rho_0        !normalising density 
logical, intent(in) :: flip
integer, intent(in) :: io
real, intent(out), dimension(nx,ny,nz) :: sd_reac, sd_nd, sd_curv, &
                              sd_mixf, sd_crss, sd_ddif, sd_uw

real, dimension(nx,ny,nz) :: DDt, DDt_Z
real, dimension(nx,ny,nz) :: mag_grad, curv, diff
real, dimension(nx,ny,nz,3) :: normal
real, dimension(:,:,:,:), allocatable :: rr_all, diffusion
real, dimension(n_spec) :: weight
real small               !fudge factor
real :: dirn=1.0  !factor to ensure flame speed is positive 

real, dimension(nx,ny,nz) :: ZZdiss,CCdiss,ZCdiss

  select case (trim(run_title))
  case ('bunsen')
  ! premixed option

! sd_farfield = reac_ff+nd_ff
call calc_sd_comp_uw &
         (reac_ff+nd_ff, rho_0, flip, sd_reac, sd_nd, sd_curv, sd_uw)
sd_uw = sd_uw*q(:,:,:,4,1)/rho_0
sd_reac = sd_reac*q(:,:,:,4,1)/rho_0
sd_nd = sd_nd*q(:,:,:,4,1)/rho_0
sd_curv = sd_curv*q(:,:,:,4,1)/rho_0

sd_mixf = 0.0
sd_crss = 0.0
sd_ddif = 0.0
  

   case default     !part premixed option

!! First compute the full DY/Dt term
!weight(:) = 0.0
!weight(4) = 1.0
!call calc_rhoDYDt(weight, DDt)

call initialize_zcpost(io, 1)

weight(:) = 0.0
weight(n_spec_prog) = 1.0
call calc_rhoDYDt(weight, DDt)
weight(:) = 0.0
weight(n_spec_mf) = 1.0E+7
call calc_rhoDYDt(weight, DDt_Z)

call SpecToProg(yspecies,io)
    
call zcderiv_field(io)
    

!   DDt(:,:,:) = yderiv_field(:,:,:,2)*DDt(:,:,:) + &
!                yderiv_field(:,:,:,1)*DDt_Z(:,:,:)      !24mar08 I think this was wrong

!try the following

   DDt(:,:,:) =   &
   (DDt(:,:,:)-yderiv_field(:,:,:,1)*DDt_Z(:,:,:))/yderiv_field(:,:,:,2)

!Calculate the reaction rate
allocate(rr_all(nx, ny, nz, n_spec))
allocate(diffusion(nx,ny,nz,n_spec))
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_all,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_all,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif

!sd_reac = rr_all(:,:,:,4)

sd_reac = rr_all(:,:,:,n_spec_prog)/yderiv_field(:,:,:,2)

deallocate(rr_all)
deallocate(diffusion)

!Calculate the curvature term

!call calc_mag_grad_and_normal  &
!      (yspecies(:,:,:,4), (/0.0, -1.0, 0.0/), .false., mag_grad, normal)
!      ! Dont flip the normal. It is done when calculating sd. 
!call computeDivergence(normal, curv)
!diff = getSpcsDiffusivity(4)
!sd_curv = diff*curv


call calc_mag_grad_and_normal  &
       (PartProg(:,:,:),  (/0.0, -1.0, 0.0/), .false., mag_grad, normal) 
      ! Dont flip the normal. It is done when calculating sd. 
call computeDivergence(normal, curv)

#ifdef LEWIS
diff = getSpcsDiffusivity(n_spec_prog)
#else
write(io,*) 'Mixture average formulation of calc_sd_comp not implemented'
#endif

sd_curv = diff*curv

!Calculate the Mixture fraction dissipation term

#ifdef LEWIS
     diff=getSpcsDiffusivity(n_spec_mf)/getSpcsDiffusivity(n_spec_prog)
#else
     write(io,*) 'Mixture average formulation of calc_sd_comp not implemented'
#endif
     call calc_ZCdiss(ZZdiss,CCdiss,ZCdiss,io)
     sd_mixf(:,:,:)=ZZdiss(:,:,:) &
                   *Yderiv_field(:,:,:,3) &
                   *diff &
                   /Yderiv_field(:,:,:,2) 

!Calculate the Cross dissipation term

#ifdef LEWIS
diff = 1.0 + getSpcsDiffusivity(n_spec_mf)/getSpcsDiffusivity(n_spec_prog)
#else
write(io,*) 'Mixture average formulation of calc_sd_comp not implemented'
#endif

sd_crss(:,:,:) = ZCdiss(:,:,:) &
                 *Yderiv_field(:,:,:,4) &
                 *diff &
                 /Yderiv_field(:,:,:,2) 

!Calculate the Differential Diffusion term

!sd_ddif = 0.0

#ifdef LEWIS
diff = getSpcsDiffusivity(n_spec_prog)/getSpcsDiffusivity(n_spec_mf) - 1.0
#else
write(io,*) 'Mixture average formulation of calc_sd_comp not implemented'
#endif

sd_ddif =  Yderiv_field(:,:,:,1) &
           * diff &
           * DDt_Z(:,:,:) &
           /Yderiv_field(:,:,:,2) 

!Calculate the normal diffusion term
sd_nd = DDt - sd_reac - sd_curv - sd_mixf - sd_crss - sd_ddif

!First calculate the density WEIGHTED sd. 
!And later get the unweighted values
!This is because the division by zero limit values are defined
!based on fresh gas conditions

! set small number
call MPI_Allreduce(maxval(mag_grad),small,1,MPI_REAL8,MPI_MAX,gcomm,ierr)
small=small*small_fac*rho_0
if(flip) dirn = -1.0

sd_uw = (dirn*DDt + (reac_ff+nd_ff)*small)/(mag_grad*rho_0+small)

sd_reac = (dirn*sd_reac + reac_ff*small)/(mag_grad*rho_0+small)
sd_nd =   (dirn*sd_nd   + nd_ff*small)  /(mag_grad*rho_0+small)
sd_curv = (dirn*sd_curv + 0.0)/(mag_grad*rho_0+small)
sd_mixf = (dirn*sd_mixf + 0.0)/(mag_grad*rho_0*volum+small)
sd_crss = (dirn*sd_crss + 0.0)/(mag_grad*rho_0*volum+small)
sd_ddif = (dirn*sd_ddif + 0.0)/(mag_grad*rho_0+small)

!Unweight them. Get back the local sd
sd_uw = sd_uw*rho_0*volum

sd_reac = sd_reac*rho_0*volum
sd_nd   = sd_nd*rho_0*volum
sd_curv = sd_curv*rho_0*volum
sd_mixf = sd_mixf*rho_0*volum
sd_crss = sd_crss*rho_0*volum
sd_ddif = sd_ddif*rho_0*volum

end select

return
end subroutine calc_sd_comp


!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_sdn_uw(sdn_uw, flag)
!----------------------------------------------------------------------
! this routine calculates the propagation velocity of iso-surface. 
! sd_uw * normal vector. 
! I wrote this routine for use in the tracer module. 
! So, I will not use any allreduces here.
!----------------------------------------------------------------------
use variables_m, only : q, volum, yspecies
use topology_m
use grid_m, only: delx
implicit none
real, intent(out), dimension(nx,ny,nz,3) :: sdn_uw  
logical, intent(out), dimension(nx, ny, nz) :: flag 
  !positions where gradient is too small are flagged. 

real, dimension(nx,ny,nz) :: ddt, mag_grad2
real, dimension(nx,ny,nz,3) :: grad
real, dimension(n_spec) :: weight
real small, small2 !fudge factor
integer k
                   
weight(:) = 0.0
weight(4) = 1.0 !O2
call calc_rhodydt(weight, ddt)

call computeScalarGradient(yspecies(:,:,:,4), grad)
mag_grad2=sum(grad**2,DIM=4)

! Max possible gradient is where YO2 varies from .22 to .06 in one delx
! Let the smallest allowed gradient be one millionth of it
small=1e-6*(0.223855-0.066943)/delx
small2 = small*small

flag = .false. 
where(mag_grad2 < small2) flag = .true. 

ddt = -ddt !O2 is consumed

do k = 1, 3
  where(.not. flag) 
    sdn_uw(:,:,:,k) = volum(:,:,:)*ddt(:,:,:)*grad(:,:,:,k)/mag_grad2(:,:,:)
  end where
end do

return
end subroutine calc_sdn_uw

!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine calc_sdn_mass( sdn_uw)
!----------------------------------------------------------------------
! this routine calculates the propagation velocity for tracer
! particles as u_x = -(1/rho)*d(rhoD)/dx so as to maintain uniform number
! per unit mass.  
! THIS ASSUMES ALL SPECIES HAVE UNITY LEWIS NUMBER.
! I wrote this routine for use in the tracer module. 
! So, I will not use any allreduces here.
!----------------------------------------------------------------------
use variables_m, only : q, volum, yspecies, temp, pressure, yspecies
use topology_m
use grid_m, only: delx
use transport_m, only : Pr, computeCoefficients, getViscosity
use thermchem_m, only : cpmix
implicit none
real, intent(out), dimension(nx,ny,nz,3) :: sdn_uw  

real, dimension(nx,ny,nz) :: Rho_alpha
real, dimension(nx,ny,nz,3) :: grad
integer k
                   
#ifdef MIXAVG
     call computeCoefficients(pressure,temp,yspecies,1.0/volum)
#endif
#ifdef LEWIS
  call computeCoefficients(cpmix, temp)
#endif

!The following returns rho * nu:
rho_alpha = getViscosity()

! assume Lewis number for mass = 1.0
! Prandtl number is read from s3d.in
! Sc = Pr * Le
! rhoD = rho*nu/Sc 

rho_alpha = rho_alpha/Pr

call computeScalarGradient(rho_alpha, grad)

do k = 1, 3
    sdn_uw(:,:,:,k) = - volum(:,:,:)*grad(:,:,:,k)
end do

return
end subroutine calc_sdn_mass

!----------------------------------------------------------------------
subroutine calc_sdn_rand( sdn_uw)
!----------------------------------------------------------------------
! this routine calculates the propagation velocity for tracer
! particles as u_x = sqrt(2D*tstep) * rnd.
! here tstep is the time step for the RK substep in question
! rnd is a normally distributed random variable with zero mean and unity 
! variance.

! THIS ASSUMES ALL SPECIES HAVE UNITY LEWIS NUMBER.
! I wrote this routine for use in the tracer module. 
! So, I will not use any allreduces here.
!----------------------------------------------------------------------
use variables_m, only : q, volum, yspecies, temp, pressure, yspecies
use topology_m
use grid_m, only: delx
use transport_m, only : Pr, computeCoefficients, getViscosity
use thermchem_m, only : cpmix
implicit none
real, intent(out), dimension(nx,ny,nz,3) :: sdn_uw  

real, dimension(nx,ny,nz) :: alpha, weiner
real, dimension(nx,ny,nz,3) :: grad
integer k
                   
#ifdef MIXAVG
     call computeCoefficients(pressure,temp,yspecies,1.0/volum)
#endif

#ifdef LEWIS
  ! RG: I'm not sure that cpmix and temp are set here; if not, need to call
  !     calc_temp from thermochem_m
  call computeCoefficients(cpmix, temp)
#endif

!The following returns rho * nu:
alpha = getViscosity()

! assume Lewis number for mass = 1.0
! Prandtl number is read from s3d.in
! Sc = Pr * Le
! rhoD = rho*nu/Sc 

alpha = alpha*volum/Pr

!multiply by 2.0*tstep and take the sqrt
alpha = sqrt(2.0*alpha)

!weiner
weiner = snorm()

do k = 1, 3
    sdn_uw(:,:,:,k) = alpha(:,:,:) * weiner(:,:,:)
end do

return

contains
REAL FUNCTION snorm()
!C**********************************************************************C
!C                                                                      C
!c     open source routine introduced by esr to return normally
!c     distributed random variables. It will be modified to use 
!c     "call random_number" and not ranf.
!C                                                                      C
!C     (STANDARD-)  N O R M A L  DISTRIBUTION                           C
!C                                                                      C
!C                                                                      C
!C**********************************************************************C
!C**********************************************************************C
!C                                                                      C
!C     FOR DETAILS SEE:                                                 C
!C                                                                      C
!C               AHRENS, J.H. AND DIETER, U.                            C
!C               EXTENSIONS OF FORSYTHE'S METHOD FOR RANDOM             C
!C               SAMPLING FROM THE NORMAL DISTRIBUTION.                 C
!C               MATH. COMPUT., 27,124 (OCT. 1973), 927 - 937.          C
!C                                                                      C
!C     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM 'FL'  C
!C     (M=5) IN THE ABOVE PAPER     (SLIGHTLY MODIFIED IMPLEMENTATION)  C
!C                                                                      C
!C     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C
!C     SUNIF.  The argument IR thus goes away.                          C
!C                                                                      C
!C**********************************************************************C
!C
      REAL a(32),d(31),t(31),h(31)
      REAL u, s, ustar, aa, w, f, tt, y, random
      INTEGER i
!C
!C     THE DEFINITIONS OF THE CONSTANTS A(K), D(K), T(K) AND
!C     H(K) ARE ACCORDING TO THE ABOVEMENTIONED ARTICLE
!C
      DATA a/0.0,.3917609E-1,.7841241E-1,.1177699,.1573107,.1970991,     &
          .2372021,.2776904,.3186394,.3601299,.4022501,.4450965,         &
          .4887764,.5334097,.5791322,.6260990,.6744898,.7245144,         &
          .7764218,.8305109,.8871466,.9467818,1.009990,1.077516,         &
          1.150349,1.229859,1.318011,1.417797,1.534121,1.675940,         &
          1.862732,2.153875/
      DATA d/5*0.0,.2636843,.2425085,.2255674,.2116342,.1999243,         &
          .1899108,.1812252,.1736014,.1668419,.1607967,.1553497,         &
          .1504094,.1459026,.1417700,.1379632,.1344418,.1311722,         &
          .1281260,.1252791,.1226109,.1201036,.1177417,.1155119,         &
          .1134023,.1114027,.1095039/
      DATA t/.7673828E-3,.2306870E-2,.3860618E-2,.5438454E-2,            &
           .7050699E-2,.8708396E-2,.1042357E-1,.1220953E-1,.1408125E-1,  &
           .1605579E-1,.1815290E-1,.2039573E-1,.2281177E-1,.2543407E-1,  &
           .2830296E-1,.3146822E-1,.3499233E-1,.3895483E-1,.4345878E-1,  &
           .4864035E-1,.5468334E-1,.6184222E-1,.7047983E-1,.8113195E-1,  &
           .9462444E-1,.1123001,.1364980,.1716886,.2276241,.3304980,     &
           .5847031/
      DATA h/.3920617E-1,.3932705E-1,.3950999E-1,.3975703E-1,            & 
           .4007093E-1,.4045533E-1,.4091481E-1,.4145507E-1,.4208311E-1,  &
           .4280748E-1,.4363863E-1,.4458932E-1,.4567523E-1,.4691571E-1,  &
           .4833487E-1,.4996298E-1,.5183859E-1,.5401138E-1,.5654656E-1,  &
           .5953130E-1,.6308489E-1,.6737503E-1,.7264544E-1,.7926471E-1,  &
           .8781922E-1,.9930398E-1,.1155599,.1404344,.1836142,.2790016,  &
           .7010474/
!C
!c   10 u = ranf()
   10 call random_number(random)
      u=random
      s = 0.0
      IF (u.GT.0.5) s = 1.0
      u = u + u - s
   20 u = 32.0*u
      i = int(u)
      IF (i.EQ.32) i = 31
      IF (i.EQ.0) GO TO 100
!C
!C                                START CENTER
!C
   30 ustar = u - float(i)
      aa = a(i)
   40 IF (ustar.LE.t(i)) GO TO 60
      w = (ustar-t(i))*h(i)
!C
!C                                EXIT   (BOTH CASES)
!C
   50 y = aa + w
      snorm = y
      IF (s.EQ.1.0) snorm = -y
      RETURN
!C
!C                                CENTER CONTINUED
!C
!esr   60 u = ranf()
   60 call random_number(random)
      u=random
      w = u* (a(i+1)-aa)
      tt = (0.5*w+aa)*w
      GO TO 80

   70 tt = u
!esr      ustar = ranf()
      call random_number(random)
      ustar=random
   80 IF (ustar.GT.tt) GO TO 50
!esr   90 u = ranf()
   90 call random_number(random)
      u=random
      IF (ustar.GE.u) GO TO 70
!esr      ustar = ranf()
         call random_number(random)
         ustar=random
      GO TO 40
!C
!C                                START TAIL
!C
  100 i = 6
      aa = a(32)
      GO TO 120

  110 aa = aa + d(i)
      i = i + 1
  120 u = u + u
      IF (u.LT.1.0) GO TO 110
  130 u = u - 1.0
  140 w = u*d(i)
      tt = (0.5*w+aa)*w
      GO TO 160

  150 tt = u
!cesr  160 ustar = ranf()
  160 call random_number(random)
      ustar=random
      IF (ustar.GT.tt) GO TO 50
!cesr  170 u = ranf()
  170 call random_number(random)  
      u=random
      IF (ustar.GE.u) GO TO 150
!cesr      u = ranf()                       
      call random_number(random)
      u=random
      GO TO 140                            
                                           
      return


end function snorm
end subroutine calc_sdn_rand
!----------------------------------------------------------------------
end module premix_drvd_var_m




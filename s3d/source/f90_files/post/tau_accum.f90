#include "globalDefines.h"
   SUBROUTINE ACCUMULATE_STEP(io,finish )

  use variables_m, only : q, u, temp, pressure, yspecies, volum
!  USE HDF5
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g, iselect
!  use s3d_hdf_interface_m  !no longer used
  use reference_m
  use chemkin_m, only: reaction_rate
  use zclookup_m
  use clookup_m
  use transport_m
  use thermchem_m
  use premix_drvd_var_m
  use runtime_m, only: run_title, tstep
   
  use timescales_m
   IMPLICIT NONE
   integer, intent(in) :: io
   logical, intent(in) :: finish

   real, dimension(nx,ny,nz) :: wt, sqdummy, dummy
   real, dimension(nx,ny,nz,n_spec) :: spdummy, diffusion
   logical, dimension(nx,ny,nz) :: cond

COUNTER=COUNTER+1.0        
    
allocate(YI(nxf,nyf,nzf,2))
allocate(YP(nxf,nyf,nzf,2))
allocate(UP(nxf,nyf,nzf,3))
allocate(WI(nxf,nyf,nzf,2))
allocate(WP(nxf,nyf,nzf,2))
allocate(PP(nxf,nyf,nzf))  ! pressure fluctuation (Reynolds)
allocate(Alpha(nx,ny,nz))
allocate(VISC(nx,ny,nz))

allocate(gradYP(nxf,nyf,nzf,2,3))
allocate(gradUP(nxf,nyf,nzf,3,3))
allocate(gradWP(nxf,nyf,nzf,2,3))
allocate(grad2YP(nxf,nyf,nzf,2,3,3))

!allocate(sumgradYM(nxf,nyf,nzf,2))
!allocate(YflucError(nxf,nyf,nzf,2))

allocate(dY2dY1(nxf,nyf,nzf))
  

wt=1.0
cond=.true.

YI = 0.0
YP = 0.0 
UP = 0.0
WI = 0.0
WP = 0.0
PP = 0.0
alpha = 0.0
visc = 0.0
!rhosl = 0.0
gradYP = 0.0
gradUP = 0.0 
gradWP = 0.0
grad2YP = 0.0

!sumgradYM = 0.0
!YflucError = 0.0
  if(myid.eq.0)write(io,*)'***** gradient allocation done *****'
        

! Find Yprime
! (YI is Y_instananeous, YF is Y_filtered = YM for Y_mean, YP is Y_prime)
 YI(:,:,:,1)=yspecies(:,:,:,tau_indx(1))*fac_indx(1)
 if(n_spec.le.15)then
! YI(:,:,:,2)=yspecies(:,:,:,tau_indx(2))*fac_indx(2)  !scaled to give MixFrac
 YI(:,:,:,2)=0.0233661+0.0318212*yspecies(:,:,:,tau_indx(2))*fac_indx(2)  !scaled to give MixFrac^M
 else
 YI(:,:,:,2)=0.078578666*yspecies(:,:,:,tau_indx(2))*fac_indx(2) 
if(myid.eq.0)write(io,*)'************* YI_2 set *************',YI(1,1,1,2),tau_indx(2),fac_indx(2)
 endif
! YI(:,:,:,3)=yspecies(:,:,:,tau_indx(3))*fac_indx(3)
 YP(:,:,:,:)=YI(:,:,:,:)-YM(:,:,:,:)

! Find Uprime
 UP(:,:,:,:)=U(:,:,:,:)-UM(:,:,:,:)

if(myid.eq.0)write(*,*)'tau_indx before finding wprime', tau_indx

! Find Wprime
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
 call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
 do i=1,2 !3
 WI(:,:,:,i)=rr_r(:,:,:,tau_indx(i))*fac_indx(i)
 enddo
 do i=1,2 !3
 WP(:,:,:,i)=WI(:,:,:,i)-WM(:,:,:,i)
 enddo
 
! Find PPrime (Pressure)
 PP(:,:,:)=Pressure(:,:,:)-PM(:,:,:)

 if(myid.eq.0)write(io,*)'***** Prime values found *****'
         

#ifdef MIXAVG
  call computeCoefficients(pressure,temp,yspecies,1.0/volum)
  rr_r  = getDiffusionCoeff()   !use rr_r as a dummy variable
  ALPHA(:,:,:) = rr_r(:,:,:,imf)*volum(:,:,:)
 VISC(:,:,:)  = getViscosity()
#endif
#ifdef LEWIS
  call computeCoefficients( cpmix, temp)
!  ALPHA(:,:,:) = getSpcsDiffusivity(imf)*volum  ! alpha=D_z due to unity lewis no. of MF.
  VISC(:,:,:)  = getViscosity()
  ALPHA=VISC*volum/Pr
#endif

 if(n_spec.le.15)then   !assumes it is the premixed or lo-strat case
 zcoeff(1)=0.2524
 zcoeff(2)=1.1933
 zcoeff(3)=0.4882
 zcoeff(4)=-1.8871
 zcoeff(5)=1.8294
 zcoeff(6)=-1.0367
 zcoeff(7)=0.2352
 else                   !assumes it is the hi-strat case
 zcoeff(1)=0.0
 zcoeff(2)=0.2514
 zcoeff(3)=-10.211
 zcoeff(4)=76.7
 zcoeff(5)=-155.62
 zcoeff(6)=127.85
 zcoeff(7)=-38.478
 endif

if(myid.eq.0)then  !check that the stoich value of rho_u.Sl comes out as 1.07
rhosl1val=0.0
if(n_spec.eq.15)then
do ic=0,6
rhosl1val=rhosl1val+zcoeff(ic+1)*1.0**ic
enddo
else !n_spec = 29
do ic=0,6
rhosl1val=rhosl1val+zcoeff(ic+1)*0.702**ic
enddo
endif
write(io,*) 'rho_unbrnt * Sl _stoich = ',rhosl1val, ' it should be close to 1.07.'
endif !myid


 if(myid.eq.0)write(io,*)'***** Alpha found *****'
       
 ! dYP
  do i=1,2!3
  call derivative_x(nx,ny,nz, YI(:,:,:,i), gradYP(:,:,:,i,1),scale_1x,1)
  call derivative_y(nx,ny,nz, YI(:,:,:,i), gradYP(:,:,:,i,2),scale_1y,1)
  call derivative_z(nx,ny,nz, YI(:,:,:,i), gradYP(:,:,:,i,3),scale_1z,1)
  enddo

  gradYP=gradYP-gradYM
 
  do i=1,3
  call derivative_x(nx,ny,nz, U(:,:,:,i), gradUP(:,:,:,i,1),scale_1x,1)
  call derivative_y(nx,ny,nz, U(:,:,:,i), gradUP(:,:,:,i,2),scale_1y,1)
  call derivative_z(nx,ny,nz, U(:,:,:,i), gradUP(:,:,:,i,3),scale_1z,1)
  enddo

  gradUP=gradUP-gradUM
 
 ! dWP
  do i=1,2!3
  call derivative_x(nx,ny,nz, WP(:,:,:,i), gradWP(:,:,:,i,1),scale_1x,1)
  call derivative_y(nx,ny,nz, WP(:,:,:,i), gradWP(:,:,:,i,2),scale_1y,1)
  call derivative_z(nx,ny,nz, WP(:,:,:,i), gradWP(:,:,:,i,3),scale_1z,1)
  enddo

    if(myid.eq.0)write(io,*)'***** Starting main filtering block *****'

!find dY2/dY1_laminar
if(run_title.eq.'bunsen')then
!  find the progress variable.
!  subroutine calculate_progvar(c)
!  call calculate_progvar(fieldx,io)
  fieldx=(temp*t_ref-800.0)/(2207.14894-800.0)
!  subroutine cgradlookup(c,n1,n2,grad,io)
  call cgradlookup(fieldx,tau_indx(2),tau_indx(1),dY2dY1,io)
  if(myid.eq.0)write(io,*)'Cgradlookup used to find species gradients'
else
  dY2dY1=1.0
endif

!conditional   do ic=1,ncond+1
!conditional !if(myid.eq.50)write(io,*)ic,'limit lower=',condmin+dcond*real(ic-2), yspecies(5,5,5,4)
!conditional !if(myid.eq.50)write(io,*)ic,'limit upper=',condmin+dcond*real(ic-1), yspecies(5,5,5,4)
!conditional 
!conditional !  !set cond here
!conditional !  where(yspecies(:,:,:,4).le.condmin+dcond*real(ic-2)  &
!conditional !  .and. yspecies(:,:,:,4).gt.condmin+dcond*real(ic-1)  )
!conditional !    cond=.true.
!conditional !  elsewhere
!conditional !    cond=.false.
!conditional !  endwhere
!conditional 
!conditional   if(ic.eq.1)cond=.true.
!conditional 
!conditional condmin=0.35D-7
!conditional condmax=0.45D-7
!conditional !condmin=0.45D-7
!conditional !condmax=0.55D-7
!conditional !condmin=0.55D-7
!conditional !condmax=0.65D-7
!conditional 
!conditional   if(myid.eq.0)write(io,*)'UNCONDITOINAL AVERAGING'
!conditional      cond=.true.
!conditional   !conditional !if(myid.eq.0)write(io,*)'CONDITIONAL AVERAGING BETWEEN',condmin*1.0D7,'and',condmax*1.0D7
!conditional   !conditional   where(yspecies(:,:,:,n_spec-2).gt.condmin  &
!conditional   !conditional   .and. yspecies(:,:,:,n_spec-2).lt.condmax  )
!conditional   !conditional     cond=.true.
!conditional   !conditional   elsewhere
!conditional   !conditional     cond=.false.
!conditional   !conditional   endwhere
!conditional   
!conditional   
!conditional     if(ic.eq.1)cond=.true.
!conditional   
!conditional    field(:,:,:) = 1.0
!conditional    call accum_planar(field, cond, wt, windowx, windowy, windowz, CFcond(:,:,:,ic), dummy,io)
!conditional   
!conditional    field(:,:,:) = q(:,:,:,4,1)
!conditional    call accum_planar(field, cond, wt, windowx, windowy, windowz, DFILcond(:,:,:,ic), dummy,io)
!conditional   
!conditional    field(:,:,:) = 1.0
!conditional    call accum_planar(field, cond, wt, windowx, windowy, windowz, Cpdfcond(:,:,:,ic), dummy,io)
!conditional   
!conditional    field(:,:,:) = q(:,:,:,4,1)
!conditional    call accum_planar(field, cond, wt, windowx, windowy, windowz, Dpdfcond(:,:,:,ic), dummy,io)
!conditional   
!conditional   
!conditional    do m=1,2
!conditional    do n=1,2
!conditional    fieldx=0.0
!conditional    fieldz=0.0
!conditional    do i=1,3
!conditional    fieldx=fieldx+(gradYM(:,:,:,m,i)+gradYP(:,:,:,m,i)) * &
!conditional                  (gradYM(:,:,:,n,i)+gradYP(:,:,:,n,i))
!conditional   ! fieldz=fieldz+(gradYM(:,:,:,1,i)+gradYP(:,:,:,1,i)) * &
!conditional   !               (gradYM(:,:,:,1,i)+gradYP(:,:,:,1,i))
!conditional    enddo
!conditional   
!conditional   ! if(myid.eq.50)write(io,*)ic,'fieldx,y,z',fieldx(5,5,5),fieldy(5,5,5),fieldz(5,5,5)
!conditional   ! where(abs(fieldy(:,:,:)).gt.1.0e-15.and.abs(fieldz(:,:,:)).gt.1.0e-15)
!conditional   ! field(:,:,:) =  fieldx(:,:,:)/fieldy(:,:,:)/fieldz(:,:,:)
!conditional   ! elsewhere
!conditional   ! field(:,:,:)=0.0
!conditional   ! endwhere
!conditional   
!conditional   ! where(abs(fieldz(:,:,:)).gt.1.0e-15)
!conditional   ! field(:,:,:) =  fieldx(:,:,:) /fieldz(:,:,:)
!conditional   ! elsewhere
!conditional   ! field(:,:,:)=0.0
!conditional   ! endwhere
!conditional   
!conditional    field=fieldx
!conditional   
!conditional    call accum_planar(field, cond, wt, windowx, windowy, windowz, gradCheck(:,:,:,m,n,ic), dummy,io)
!conditional    enddo
!conditional    enddo
!conditional   
!conditional   
!conditional   !  if(myid.eq.50)write(io,*)ic,'after accum grad cond'
!conditional     do n=1,2
!conditional     do m=1,2
!conditional     field(:,:,:) = YI(:,:,:,m)*YI(:,:,:,n)*q(:,:,:,4,1)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, YPYPcond(:,:,:,m,n,ic), dummy,io)
!conditional     enddo
!conditional     enddo
!conditional   
!conditional     do m=1,2
!conditional     field(:,:,:) = YI(:,:,:,m)*q(:,:,:,4,1)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, Ycond(:,:,:,m,ic), dummy,io)
!conditional     enddo
!conditional   
!conditional     do m=1,2
!conditional     field(:,:,:) = rr_r(:,:,:,tau_indx(m))*q(:,:,:,4,1)*q(:,:,:,4,1)*fac_indx(m)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, Wcond(:,:,:,m,ic), dummy,io)
!conditional     enddo
!conditional   
!conditional   !  Find cosAlpha, the angle between the gradY1 and gradY2. We take the conditional average of 
!conditional   !  cosAlpha and cosAlpha^2.
!conditional    fieldx=0.0
!conditional    fieldy=0.0
!conditional    fieldz=0.0
!conditional    do i=1,3
!conditional    fieldx=fieldx+(gradYM(:,:,:,1,i)+gradYP(:,:,:,1,i)) * &
!conditional                  (gradYM(:,:,:,1,i)+gradYP(:,:,:,1,i))
!conditional    fieldy=fieldy+(gradYM(:,:,:,1,i)+gradYP(:,:,:,1,i)) * &
!conditional                  (gradYM(:,:,:,2,i)+gradYP(:,:,:,2,i))
!conditional    fieldz=fieldz+(gradYM(:,:,:,2,i)+gradYP(:,:,:,2,i)) * &
!conditional                  (gradYM(:,:,:,2,i)+gradYP(:,:,:,2,i))
!conditional    enddo
!conditional   
!conditional     field(:,:,:) = fieldy(:,:,:)/sqrt(fieldx(:,:,:))/sqrt(fieldz(:,:,:))
!conditional     fieldx(:,:,:) = field(:,:,:)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, CosAlpha(:,:,:,1,ic), dummy,io)
!conditional   
!conditional     field(:,:,:) = fieldx(:,:,:)*fieldx(:,:,:)    
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, CosAlpha(:,:,:,2,ic), dummy,io)
!conditional   
!conditional   !  Find FlamNormGrad, the component of the GradY2 in the GradY1 direction, and its square.
!conditional   !  <(gradY2.gradY1/|gradY1|)^2 | zeta> 
!conditional   !
!conditional   !  1)      <gradY1.gradY2/|gradY1|>
!conditional   !  2)      <gradY1.gradY2/|gradY1|>)^2
!conditional   !  3)      <gradY1.gradY1/|gradY1|>
!conditional   !  4)      <gradY1.gradY1/|gradY1|>^2
!conditional   !  5)      <gradY1.gradY2/gradY1.gradY1>
!conditional   !  6)      <gradY1.gradY2/gradY1.gradY1>^2
!conditional   !  7)      <D1gradY1.gradY1>
!conditional   !  8)      <D2gradY2.gradY2>
!conditional   !  9)      <D2(gradY1.gradY2/|gradY1|)^2>
!conditional   !
!conditional   !  1)
!conditional    fieldx=0.0
!conditional    fieldy=0.0
!conditional    fieldz=0.0
!conditional    do i=1,3
!conditional    fieldx=fieldx+(gradYM(:,:,:,1,i)+gradYP(:,:,:,1,i)) * &
!conditional                  (gradYM(:,:,:,1,i)+gradYP(:,:,:,1,i))
!conditional    fieldy=fieldy+(gradYM(:,:,:,1,i)+gradYP(:,:,:,1,i)) * &
!conditional                  (gradYM(:,:,:,2,i)+gradYP(:,:,:,2,i))
!conditional    fieldz=fieldz+(gradYM(:,:,:,2,i)+gradYP(:,:,:,2,i)) * &
!conditional                  (gradYM(:,:,:,2,i)+gradYP(:,:,:,2,i))
!conditional    enddo
!conditional   
!conditional     field(:,:,:) = fieldy(:,:,:)/sqrt(abs(fieldx(:,:,:)))
!conditional     fieldz(:,:,:) = field(:,:,:)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,1,ic), dummy,io)
!conditional   !  2)
!conditional     field(:,:,:) = fieldz(:,:,:)*fieldz(:,:,:)    
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,2,ic), dummy,io)
!conditional   !  3)
!conditional     field(:,:,:) = fieldx(:,:,:)/sqrt(abs(fieldx(:,:,:)))
!conditional     fieldz(:,:,:) = field(:,:,:)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,3,ic), dummy,io)
!conditional   !  4)
!conditional     field(:,:,:) = fieldz(:,:,:)*fieldz(:,:,:)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,4,ic), dummy,io)
!conditional   !  5)
!conditional     fieldz = 1.0D-20
!conditional     field = fieldy/(fieldx+fieldz)
!conditional     fieldz = field
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,5,ic), dummy,io)
!conditional   !  6)
!conditional     field(:,:,:) = fieldz(:,:,:)*fieldz(:,:,:)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,6,ic), dummy,io)
!conditional   !  7)      <D1gradY1.gradY1>
!conditional     field = fieldx*alpha/invLe(1,1)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,7,ic), dummy,io)
!conditional    fieldz=0.0   ! need to find gradY2.gradY2 again since it was used for working space above.
!conditional    do i=1,3
!conditional    fieldz=fieldz+(gradYM(:,:,:,2,i)+gradYP(:,:,:,2,i)) * &
!conditional                  (gradYM(:,:,:,2,i)+gradYP(:,:,:,2,i))
!conditional    enddo
!conditional   !  8)      <D2gradY2.gradY2>
!conditional     field = fieldz*alpha/invLe(2,2)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,8,ic), dummy,io) 
!conditional   !  9)      <D2(gradY1.gradY2/|gradY1|)^2>
!conditional     field = fieldy/sqrt(abs(fieldx)) * fieldy/sqrt(abs(fieldx)) * alpha/invLe(2,2)
!conditional     call accum_planar(field, cond, wt, windowx, windowy, windowz, FlamNormGrad(:,:,:,8,ic), dummy,io) 
!conditional   
!conditional     enddo !ncond

! set cond to true for all the unconditional averages that follow.

 cond=.true.
!conditional  if(myid.eq.0)write(io,*)'CONDITIONAL AVERAGING BETWEEN',condmin*1.0D7,'and',condmax*1.0D7
!conditional    where(yspecies(:,:,:,n_spec-2).gt.condmin  &
!conditional    .and. yspecies(:,:,:,n_spec-2).lt.condmax  )
!conditional      cond=.true.
!conditional    elsewhere
!conditional      cond=.false.
!conditional    endwhere


! Counter
 field(:,:,:) = 1.0
  call accum_planar(field, cond, wt, windowx, windowy, windowz, CF, sqCF,io)

! RHO
 field(:,:,:) = q(:,:,:,4,1)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, DFIL, sqDFIL,io)

! ALPHA
 field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, AF, sqAF,io)
! VISC
 field(:,:,:) = q(:,:,:,4,1)*visc(:,:,:)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, VF, sqVF,io)

! error assessment
 do i=1,3
 do m=1,2
 field(:,:,:) = q(:,:,:,4,1)*(gradYP(:,:,:,m,i)+gradYM(:,:,:,m,i))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, sumrhogradYI(:,:,:,m,i), sqdummy,io)
 enddo
 enddo
 do i=1,3
 do m=1,2
 field(:,:,:) = gradYP(:,:,:,m,i)*gradYP(:,:,:,m,i)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, sumgradYP2(:,:,:,m,i), sqdummy,io)
 enddo
 enddo
               


 if(n_spec.eq.15.or.n_spec.eq.29)then  ! then the second species is MF and we can use it
! to find rho_sl as a function of mixture fraction, otherwise set rho_sl to the stoichiometric value.
  field=0.0
  do ic=0,6
    field(:,:,:)=field(:,:,:) + zcoeff(ic+1)*(1.0e7*yspecies(:,:,:,imf))**ic
  enddo
  field(:,:,:)=field(:,:,:)/rho_ref/a_ref

 elseif(n_spec.eq.13)then  !this is the premixed methane case
                           !Xi=0.5 corresponds to phi=0.7
  
  field=0.0
  do ic=0,6
    field(:,:,:)=field(:,:,:) + zcoeff(ic+1)*0.5**ic
  enddo
  field(:,:,:)=field(:,:,:)/rho_ref/a_ref

 else

  field=1.0747/rho_ref/a_ref  ! this is the stoichiometric value for methane-air at 800K
 endif


! RHOSL
 field(:,:,:) = q(:,:,:,4,1)*field(:,:,:)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, RHOSL, sqRHOSL,io)

! RHOU1
 do i=1,3
 field(:,:,:) = q(:,:,:,i,1)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, UFIL(:,:,:,i), sqUFIL(:,:,:,i),io)
 enddo
! RHOY1
 do i=1,2!3
  field(:,:,:) = YI(:,:,:,i)*q(:,:,:,4,1)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, YFIL(:,:,:,i), sqYFIL(:,:,:,i),io)
 enddo

if(run_title.eq.'bunsen')then
!  find the progress variable.
!  call calculate_progvar(fieldx)
  fieldx=(temp*t_ref-800.0)/(2207.14894-800.0)
  call clookup(fieldx,spdummy,dummy,io)
else
  spdummy=0.0
endif

! RHOY1_c, here the value of yspecies if found from the laminar look up table based on the local cprog
 do i=1,2!3
   field(:,:,:) = spdummy(:,:,:,tau_indx(i))*q(:,:,:,4,1)*fac_indx(i)
   call accum_planar(field, cond, wt, windowx, windowy, windowz, YcFIL(:,:,:,i), sqYcFIL(:,:,:,i),io)
 enddo

! RHOY1_c2
 do i=1,2
 do j=1,2
   field(:,:,:) = spdummy(:,:,:,tau_indx(j))*spdummy(:,:,:,tau_indx(i))*q(:,:,:,4,1)*fac_indx(i)*fac_indx(j)
   call accum_planar(field, cond, wt, windowx, windowy, windowz, YYcFIL(:,:,:,i,j), sqYYcFIL(:,:,:,i,j),io)
 enddo
 enddo
      
! KE
 do i=1,2!3
 field(:,:,:) = q(:,:,:,4,1)*YP(:,:,:,i)*YP(:,:,:,1)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, YPYPF(:,:,:,i,1), sqYPYPF(:,:,:,i,1),io)
 enddo
 do i=2,2!3
 field(:,:,:) = q(:,:,:,4,1)*YP(:,:,:,i)*YP(:,:,:,2)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, YPYPF(:,:,:,i,2), sqYPYPF(:,:,:,i,2),io)
 enddo
  
! EPS
 do j=1,2!3 !species loop
 field(:,:,:)= gradYP(:,:,:,j,1)*gradYP(:,:,:,1,1)  &
             + gradYP(:,:,:,j,2)*gradYP(:,:,:,1,2)  &
             + gradYP(:,:,:,j,3)*gradYP(:,:,:,1,3)
 field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)*field(:,:,:)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, AdYPdYPF(:,:,:,j,1), sqAdYPdYPF(:,:,:,j,1),io)
 enddo
 do j=2,2!3
 field(:,:,:)= gradYP(:,:,:,j,1)*gradYP(:,:,:,2,1)  &
             + gradYP(:,:,:,j,2)*gradYP(:,:,:,2,2)  &
             + gradYP(:,:,:,j,3)*gradYP(:,:,:,2,3)
 field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)*field(:,:,:)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, AdYPdYPF(:,:,:,j,2), sqAdYPdYPF(:,:,:,j,2),io)
 enddo

! EPS_MOD, found estimating gradY2 by dY2/dY1_lam * gradY1, this is used to estimate EPS for spec 2 and for the 
! cross dissipation of species 1 and species 2
 field(:,:,:)= gradYP(:,:,:,1,1)*gradYP(:,:,:,1,1)  &
             + gradYP(:,:,:,1,2)*gradYP(:,:,:,1,2)  &
             + gradYP(:,:,:,1,3)*gradYP(:,:,:,1,3)
! do the species 2 dissipation first: 
 field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)*field(:,:,:)*dY2dY1(:,:,:) * dY2dY1(:,:,:)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, AdYPdYPF_MOD(:,:,:,1), sqdummy(:,:,:),io)
! now do the cross dissipation 
 field(:,:,:)= gradYP(:,:,:,1,1)*gradYP(:,:,:,1,1)  &
             + gradYP(:,:,:,1,2)*gradYP(:,:,:,1,2)  &
             + gradYP(:,:,:,1,3)*gradYP(:,:,:,1,3)
 field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)*field(:,:,:)*dY2dY1(:,:,:) 
    call accum_planar(field, cond, wt, windowx, windowy, windowz, AdYPdYPF_MOD(:,:,:,2), sqdummy(:,:,:),io)

 !KEMECH
  field(:,:,:)=0.0
  do i=1,3
  field(:,:,:) = field(:,:,:)+q(:,:,:,4,1)*UP(:,:,:,i)*UP(:,:,:,i)
  enddo
  field=field*0.5
    call accum_planar(field, cond, wt, windowx, windowy, windowz, KEMECH, sqKEMECH,io)

  do j=1,3
  do i=1,3
    field(:,:,:)=0.5*q(:,:,:,4,1)*UP(:,:,:,i)*UP(:,:,:,j)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, KEMECH_tens(:,:,:,i,j), sqKEMECH_tens(:,:,:,i,j),io)
  enddo
  enddo

 
 !EPSMECH
  call calculate_dissipation(U,UP,volum,field)
  field(:,:,:)=field(:,:,:)*q(:,:,:,4,1)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, EPSMECH, sqEPSMECH,io)

  call calculate_dissipation_tensor(U, UP, inveps) 
  do n=1,3
   do m=1,3
    field(:,:,:)=inveps(:,:,:,m,n)*q(:,:,:,4,1)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, EPSMECH_tens(:,:,:,m,n), sqEPSMECH_tens(:,:,:,m,n),io)
   enddo
  enddo

 !UPVP
  field(:,:,:)=UP(:,:,:,1)*UP(:,:,:,2)*q(:,:,:,4,1)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, UPVP, sqUPVP, io)


 !NB, KEDISS would equal -2 DENS * EPS
 
 
 !KEDIFF
 do n=1,2
 do m=1,2

    field(:,:,:)=YP(:,:,:,m)*YP(:,:,:,n)
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)

    field=fieldx*q(:,:,:,4,1)*alpha(:,:,:)
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
    field=fieldy*q(:,:,:,4,1)*alpha(:,:,:)
    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
    field=fieldz*q(:,:,:,4,1)*alpha(:,:,:)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)

    field=fieldx+fieldy+fieldz
    call accum_planar(field, cond, wt, windowx, windowy, windowz, KEDIFF(:,:,:,m,n), sqKEDIFF(:,:,:,m,n),io)

 enddo
 enddo

 !KET1
 do n=1,2
 do m=1,2

    field(:,:,:)=q(:,:,:,4,1)*UP(:,:,:,1)*YP(:,:,:,m)*YP(:,:,:,n)
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
    field(:,:,:)=q(:,:,:,4,1)*UP(:,:,:,2)*YP(:,:,:,m)*YP(:,:,:,n)
    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
    field(:,:,:)=q(:,:,:,4,1)*UP(:,:,:,3)*YP(:,:,:,m)*YP(:,:,:,n)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)

    field=fieldx+fieldy+fieldz
    call accum_planar(field, cond, wt, windowx, windowy, windowz, KETRIP(:,:,:,m,n), sqKEDIFF(:,:,:,m,n),io)

 enddo
 enddo
 
 !KET1
!   do n=1,2
!   do m=1,2
!     field=0.0
! 
! !xdir
!    field(:,:,:)=U(:,:,:,1)*YI(:,:,:,m)*YI(:,:,:,n)
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=fieldz(:,:,:)
!    
!    field(:,:,:)=U(:,:,:,1)*YM(:,:,:,m)*YM(:,:,:,n)
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:) 
! 
!    field(:,:,:)=-U(:,:,:,1)*YI(:,:,:,m)*YM(:,:,:,n)
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-U(:,:,:,1)*YM(:,:,:,m)*YI(:,:,:,n)
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-UM(:,:,:,1)*YI(:,:,:,m)*YI(:,:,:,n)    !sign corrected here 10th oct08
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-UM(:,:,:,1)*YM(:,:,:,m)*YM(:,:,:,n)
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=UM(:,:,:,1)*YI(:,:,:,m)*YM(:,:,:,n)
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=UM(:,:,:,1)*YM(:,:,:,m)*YI(:,:,:,n)
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    fieldy=fieldx
! 
! !ydir
!    field(:,:,:)=U(:,:,:,2)*YI(:,:,:,m)*YI(:,:,:,n)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=fieldz(:,:,:)
!    
!    field(:,:,:)=U(:,:,:,2)*YM(:,:,:,m)*YM(:,:,:,n)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:) 
! 
!    field(:,:,:)=-U(:,:,:,2)*YI(:,:,:,m)*YM(:,:,:,n)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-U(:,:,:,2)*YM(:,:,:,m)*YI(:,:,:,n)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-UM(:,:,:,2)*YI(:,:,:,m)*YI(:,:,:,n)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-UM(:,:,:,2)*YM(:,:,:,m)*YM(:,:,:,n)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=UM(:,:,:,2)*YI(:,:,:,m)*YM(:,:,:,n)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=UM(:,:,:,2)*YM(:,:,:,m)*YI(:,:,:,n)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    fieldy=fieldy+fieldx
! 
! !zdir
!    field(:,:,:)=U(:,:,:,3)*YI(:,:,:,m)*YI(:,:,:,n)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=fieldz(:,:,:)
!    
!    field(:,:,:)=U(:,:,:,3)*YM(:,:,:,m)*YM(:,:,:,n)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:) 
! 
!    field(:,:,:)=-U(:,:,:,3)*YI(:,:,:,m)*YM(:,:,:,n)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-U(:,:,:,3)*YM(:,:,:,m)*YI(:,:,:,n)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-UM(:,:,:,3)*YI(:,:,:,m)*YI(:,:,:,n)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=-UM(:,:,:,3)*YM(:,:,:,m)*YM(:,:,:,n)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=UM(:,:,:,3)*YI(:,:,:,m)*YM(:,:,:,n)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    field(:,:,:)=UM(:,:,:,3)*YM(:,:,:,m)*YI(:,:,:,n)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=fieldx(:,:,:)+fieldz(:,:,:)
! 
!    fieldy=fieldy+fieldx
!    field= - fieldx   ! NOTE THE SIGN HERE
!    call accum_planar(field, cond, wt, windowx, windowy, windowz, KET1(:,:,:,m,n), sqKET1(:,:,:,m,n),io)
!
!  do i=1,3
!  field=-q(:,:,:,4,1)*UP(:,:,:,i)*YP(:,:,:,m)*YP(:,:,:,n)
!    call accum_planar(field, cond, wt, windowx, windowy, windowz, KET1b(:,:,:,m,n,i), sqKET1b(:,:,:,m,n,i),io)
!
!  enddo
!
!   enddo
!   enddo
 
 !KET2
  do i=1,3
  do m=1,2
    field(:,:,:)= q(:,:,:,4,1)*UP(:,:,:,i)*YP(:,:,:,m)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, KET2(:,:,:,m,i), sqKET2(:,:,:,m,i),io)
  enddo
  enddo
 
 !KET3
 
  do n=1,2
  do m=1,2
   field(:,:,:)= WI(:,:,:,m)*YP(:,:,:,n)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, KET3(:,:,:,m,n), sqKET3(:,:,:,m,n),io)
  enddo
  enddo
 
 !KET4
  do n=1,2
   do m=1,2
 
 !xdir
    field(:,:,:)=gradYP(:,:,:,m,1)+gradYM(:,:,:,m,1)
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)  ! fieldx is second deriv of YI
    field(:,:,:)=gradYM(:,:,:,m,1)
    call derivative_x(nx,ny,nz, fieldx(:,:,:), fieldy(:,:,:),scale_1x,1) ! fieldy is second deriv of YM
 
!    fieldz(:,:,:)=(fieldx(:,:,:)-fieldy(:,:,:))*YP(:,:,:,n)*q(:,:,:,4,1)*alpha(:,:,:)*(invLe(m,m)-invLe(m,n))

    fieldz=(fieldx*YI(:,:,:,n)-fieldy*YM(:,:,:,n))*q(:,:,:,4,1)*alpha*(invLe(m,m)-invLe(m,n))

 
 !ydir
    field(:,:,:)=gradYP(:,:,:,m,2)+gradYM(:,:,:,m,2)
    call derivative_y(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1y,1)  ! fieldx is second deriv of YI
    field(:,:,:)=gradYM(:,:,:,m,2)
    call derivative_y(nx,ny,nz, fieldx(:,:,:), fieldy(:,:,:),scale_1y,1) ! fieldy is second deriv of YM
 
!    fieldz(:,:,:)=fieldz(:,:,:)+(fieldx(:,:,:)-fieldy(:,:,:))*YP(:,:,:,n)*q(:,:,:,4,1)*alpha(:,:,:)*(invLe(m,m)-invLe(m,n))

    fieldz=fieldz +  &
      (fieldx*YI(:,:,:,n)-fieldy*YM(:,:,:,n))*q(:,:,:,4,1)*alpha*(invLe(m,m)-invLe(m,n))

 
 !zdir
    field(:,:,:)=gradYP(:,:,:,m,3)+gradYM(:,:,:,m,3)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1z,1)  ! fieldx is second deriv of YI
    field(:,:,:)=gradYM(:,:,:,m,3)
    call derivative_z(nx,ny,nz, fieldx(:,:,:), fieldy(:,:,:),scale_1z,1) ! fieldy is second deriv of YM
             
!    fieldz(:,:,:)=fieldz(:,:,:)+(fieldx(:,:,:)-fieldy(:,:,:))*YP(:,:,:,n)*q(:,:,:,4,1)*alpha(:,:,:)*(invLe(m,m)-invLe(m,n))

    fieldz=fieldz +  &
      (fieldx*YI(:,:,:,n)-fieldy*YM(:,:,:,n))*q(:,:,:,4,1)*alpha*(invLe(m,m)-invLe(m,n))
       
    field= fieldz
    call accum_planar(field, cond, wt, windowx, windowy, windowz, KET4(:,:,:,m,n), sqKET4(:,:,:,m,n),io)
   enddo
  enddo
                   
 !KET5
 
 do n=1,2
 do m=1,2
 
   field(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(invLe(m,m)-invLe(m,n))
   
 !xdir
   call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
   fieldz(:,:,:)=gradYP(:,:,:,m,1)*YP(:,:,:,n)*fieldx(:,:,:)
       
 !ydir
   call derivative_y(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1y,1)
   fieldz(:,:,:)=fieldz(:,:,:)+gradYP(:,:,:,m,2)*YP(:,:,:,n)*fieldx(:,:,:)
 
 !zdir
   call derivative_z(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1z,1)
   fieldz(:,:,:)=fieldz(:,:,:)+gradYP(:,:,:,m,2)*YP(:,:,:,n)*fieldx(:,:,:)
 
   field=fieldz 
    call accum_planar(field, cond, wt, windowx, windowy, windowz, KET5(:,:,:,m,n), sqKET5(:,:,:,m,n),io)
 enddo
 enddo

! Terms for the turbulent kinetic energy
!TKconv: 0.5.rho.up_k.up_k summed over k,=> Use KEMECH
!TK1:
 do m=1,3
 do n=1,3
  field(:,:,:)=q(:,:,:,4,1)*UP(:,:,:,m)*UP(:,:,:,n)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, TK1(:,:,:,m,n), sqTK1(:,:,:,m,n),io)
 enddo
 enddo
!TK2: rho.up_i.up_i.up_k  summed over i, record the three k components
 do n=1,3  !k components
   field(:,:,:)=    q(:,:,:,4,1)*(UP(:,:,:,1)*UP(:,:,:,1) &
                                 +UP(:,:,:,2)*UP(:,:,:,2) &
                                 +UP(:,:,:,3)*UP(:,:,:,3))*UP(:,:,:,n)
   call accum_planar(field, cond, wt, windowx, windowy, windowz, TK2(:,:,:,n), sqTK2(:,:,:,n),io)  
enddo
!TK3: up_k (in order to take the Reynolds average
 do n=1,3
   field(:,:,:)=UP(:,:,:,n)
   call accum_planar(field, cond, wt, windowx, windowy, windowz, TK3(:,:,:,n), sqTK3(:,:,:,n),io)       
 enddo
!TK4: PP.up_k
 do n=1,3
   field(:,:,:)=PP(:,:,:)*UP(:,:,:,n)
   call accum_planar(field, cond, wt, windowx, windowy, windowz, TK4(:,:,:,n), sqTK4(:,:,:,n),io)     
 enddo
!TK5
   field(:,:,:)=PP(:,:,:)*(gradUP(:,:,:,1,1)+gradUP(:,:,:,2,2)+gradUP(:,:,:,3,3))
   call accum_planar(field, cond, wt, windowx, windowy, windowz, TK5(:,:,:), sqTK5(:,:,:),io)
! End of TKE equation accumulation      

! Darbyshire accumulation
!UYflux:
 do m=1,2
 do n=1,3
  field(:,:,:)=q(:,:,:,4,1)*U(:,:,:,n)*YI(:,:,:,m)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, UYflux(:,:,:,n,m), sqTK1(:,:,:,m,n),io)
 enddo
 enddo
!UCflux
 do n=1,3
  field(:,:,:)=q(:,:,:,4,1)*U(:,:,:,n)*(1.0-YI(:,:,:,1)/max(1.0d-10,YI(:,:,:,2)))
  call accum_planar(field, cond, wt, windowx, windowy, windowz, UCflux(:,:,:,n), sqTK1(:,:,:,m,n),io)
 enddo
! 
!Y1_Y2M = rho *Y1/Y2
   field(:,:,:)=q(:,:,:,4,1)*YI(:,:,:,1)/max(1.0d-10,YI(:,:,:,2))
   call accum_planar(field, cond, wt, windowx, windowy, windowz, Y1_Y2M(:,:,:), sqY1_Y2M(:,:,:),io)
!sqY1_Y2M = rho *(Y1/Y2)**2
   field(:,:,:)=q(:,:,:,4,1)*(YI(:,:,:,1)/max(1.0d-10,YI(:,:,:,2)))**2
   call accum_planar(field, cond, wt, windowx, windowy, windowz, Y1_Y2sq(:,:,:), sqY1_Y2sq(:,:,:),io)
!YWM = rho *WI(1) * YI(1)
   field(:,:,:)=q(:,:,:,4,1)*YI(:,:,:,1)*WI(:,:,:,1)
   call accum_planar(field, cond, wt, windowx, windowy, windowz, YWM(:,:,:), sqYWM(:,:,:),io)
!EPSC = rho * D_1 * GradC * GradC,     C=1-Y1/Y2
 field(:,:,:)=(1.0-YI(:,:,:,1)/YI(:,:,:,2))-CM(:,:,:)
 call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
 call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
 call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
 field(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(fieldx(:,:,:)**2+fieldy(:,:,:)**2+fieldz(:,:,:)**2)
 call accum_planar(field, cond, wt, windowx, windowy, windowz, EPS_C(:,:,:), sqEPS_C(:,:,:),io)
! End of Darbyshire processing


! DISS
! do j=1,3 ! direction loop (k)
 do i=1,2!3 ! species loop
! call derivative_x(nx,ny,nz, gradYP(:,:,:,i,j), grad2YP(:,:,:,i,j,1),scale_1x,1)
! call derivative_y(nx,ny,nz, gradYP(:,:,:,i,j), grad2YP(:,:,:,i,j,2),scale_1y,1)
! call derivative_z(nx,ny,nz, gradYP(:,:,:,i,j), grad2YP(:,:,:,i,j,3),scale_1z,1)

! the three lines above were replaced with the following since it means we only take 
! derivatives of the instantaneous or the averaged fields rather than the 
! fluctuation field. Hopefully this is smoother.

!x dir.
! call derivative_x(nx,ny,nz, YI(:,:,:,i), field(:,:,:),scale_1x,1)
 field(:,:,:)=gradYP(:,:,:,i,1)+gradYM(:,:,:,i,1)
 call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
 call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
 call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
 grad2YP(:,:,:,i,1,1)=fieldx(:,:,:)
 grad2YP(:,:,:,i,1,2)=fieldy(:,:,:)
 grad2YP(:,:,:,i,1,3)=fieldz(:,:,:)
! call derivative_x(nx,ny,nz, YM(:,:,:,i), field(:,:,:),scale_1x,1)
 field(:,:,:)=gradYM(:,:,:,i,1)
 call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
 call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
 call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
 grad2YP(:,:,:,i,1,1)=grad2YP(:,:,:,i,1,1)-fieldx(:,:,:)
 grad2YP(:,:,:,i,1,2)=grad2YP(:,:,:,i,1,2)-fieldy(:,:,:)
 grad2YP(:,:,:,i,1,3)=grad2YP(:,:,:,i,1,3)-fieldz(:,:,:)

!y dir.
! call derivative_y(nx,ny,nz, YI(:,:,:,i), field(:,:,:),scale_1y,1)
 field(:,:,:)=gradYP(:,:,:,i,2)+gradYM(:,:,:,i,2)
 call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
 call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
 call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
 grad2YP(:,:,:,i,2,1)=fieldx(:,:,:)
 grad2YP(:,:,:,i,2,2)=fieldy(:,:,:)
 grad2YP(:,:,:,i,2,3)=fieldz(:,:,:)
! call derivative_y(nx,ny,nz, YM(:,:,:,i), field(:,:,:),scale_1y,1)
 field(:,:,:)=gradYM(:,:,:,i,2)
 call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
 call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
 call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
 grad2YP(:,:,:,i,2,1)=grad2YP(:,:,:,i,2,1)-fieldx(:,:,:)
 grad2YP(:,:,:,i,2,2)=grad2YP(:,:,:,i,2,2)-fieldy(:,:,:)
 grad2YP(:,:,:,i,2,3)=grad2YP(:,:,:,i,2,3)-fieldz(:,:,:)

!z dir.
! call derivative_z(nx,ny,nz, YI(:,:,:,i), field(:,:,:),scale_1z,1)
 field(:,:,:)=gradYP(:,:,:,i,3)+gradYM(:,:,:,i,3)
 call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
 call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
 call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
 grad2YP(:,:,:,i,3,1)=fieldx(:,:,:)
 grad2YP(:,:,:,i,3,2)=fieldy(:,:,:)
 grad2YP(:,:,:,i,3,3)=fieldz(:,:,:)
! call derivative_z(nx,ny,nz, YM(:,:,:,i), field(:,:,:),scale_1z,1)
 field(:,:,:)=gradYM(:,:,:,i,3)
 call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
 call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
 call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
 grad2YP(:,:,:,i,3,1)=grad2YP(:,:,:,i,3,1)-fieldx(:,:,:)
 grad2YP(:,:,:,i,3,2)=grad2YP(:,:,:,i,3,2)-fieldy(:,:,:)
 grad2YP(:,:,:,i,3,3)=grad2YP(:,:,:,i,3,3)-fieldz(:,:,:)

 enddo ! i=species
! enddo

 do i=1,3 ! direction loop
 do m=1,2!3 ! species loop
 do n=1,2!3 ! species loop
 field(:,:,:) = grad2YP(:,:,:,m,1,i)*grad2YP(:,:,:,n,1,i) &
              + grad2YP(:,:,:,m,2,i)*grad2YP(:,:,:,n,2,i) &
              + grad2YP(:,:,:,m,3,i)*grad2YP(:,:,:,n,3,i)
 field(:,:,:) = 2.0*q(:,:,:,4,1)*alpha(:,:,:)*alpha(:,:,:)*field(:,:,:)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, DISSvar(:,:,:,m,n,1), sqDISSvar(:,:,:,m,n,1),io)
 enddo
 enddo
 enddo

!    call mpi_barrier(gcomm,ierr) 
    if(myid.eq.0)write(io,*)'***** LHS filtered *****'
        
! ok up to here
  
! T1a
    do i=1,3 !direction loop
    do n=1,2!3 !species loop
    do m=1,2!3 !species loop
    field(:,:,:)= gradYP(:,:,:,m,1)*gradYP(:,:,:,n,1)  &
                + gradYP(:,:,:,m,2)*gradYP(:,:,:,n,2)  &
                + gradYP(:,:,:,m,3)*gradYP(:,:,:,n,3)
    field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)*field(:,:,:)*UP(:,:,:,i)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T1aVar(:,:,:,m,n,i), sqT1avar(:,:,:,m,n,i),io)
    enddo
    enddo
    enddo   ! we could economise using symmetry here.
   
!    call mpi_barrier(gcomm,ierr)
    if(myid.eq.0)write(io,*)'***** T1avar *****'
            
! T1b

 do i=1,3  !direction loop 
 do j=1,3  !direction loop - the direction of UP
 do m=1,2!3  !species loop
 field(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*gradYP(:,:,:,m,i)*UP(:,:,:,j)

! field(:,:,:)=q(:,:,:,4,1)*(gradYM(:,:,:,m,i)+gradYP(:,:,:,m,i))*U(:,:,:,j)


    call accum_planar(field, cond, wt, windowx, windowy, windowz, T1bVar(:,:,:,m,i,j), sqT1bVar(:,:,:,m,i,j),io)
 enddo
 enddo
 enddo


 do i=1,3  !direction loop
 do j=1,3  !direction loop - the direction of UP
 do m=1,2!3  !species loop
 field(:,:,:)=q(:,:,:,4,1)*(gradYM(:,:,:,m,i)+gradYP(:,:,:,m,j))*U(:,:,:,i)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, UdYVar(:,:,:,m,i,j), sqUdYVar(:,:,:,m,i,j),io)
 enddo
 enddo
 enddo

!    call mpi_barrier(gcomm,ierr)
    if(myid.eq.0)write(io,*)'***** T1bvar *****'
!    call mpi_barrier(gcomm,ierr)


! T2
! find the deriv of density
    call derivative_x(nx,ny,nz, q(:,:,:,4,1), fieldx(:,:,:),scale_1x,1)
    call derivative_y(nx,ny,nz, q(:,:,:,4,1), fieldy(:,:,:),scale_1y,1)
    call derivative_z(nx,ny,nz, q(:,:,:,4,1), fieldz(:,:,:),scale_1z,1)
   
   do n=1,2!3
   
    field(:,:,:)=fieldx(:,:,:)*gradYP(:,:,:,n,1)  &
                +fieldy(:,:,:)*gradYP(:,:,:,n,2)  &
                +fieldz(:,:,:)*gradYP(:,:,:,n,3)  
   
   do m=1,2!3

   fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(gradYP(:,:,:,m,1)+gradYM(:,:,:,m,1))*invLe(m,n)
   call derivative_x(nx,ny,nz, fieldx(:,:,:), fieldy(:,:,:),scale_1x,1)
   fieldz=fieldy
   fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(gradYP(:,:,:,m,2)+gradYM(:,:,:,m,2))*invLe(m,n)
   call derivative_y(nx,ny,nz, fieldx(:,:,:), fieldy(:,:,:),scale_1y,1)
   fieldz=fieldz+fieldy
   fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(gradYP(:,:,:,m,3)+gradYM(:,:,:,m,3))*invLe(m,n)
   call derivative_z(nx,ny,nz, fieldx(:,:,:), fieldy(:,:,:),scale_1z,1)
   fieldz=fieldz+fieldy
   fieldz=fieldz+WI(:,:,:,m)

   fieldz=fieldz*alpha*volum
   field=fieldz*field

    call accum_planar(field, cond, wt, windowx, windowy, windowz, T2Var(:,:,:,m,n,1), sqT2Var(:,:,:,m,n,1),io)
   
   enddo
   enddo





    
!       call mpi_barrier(gcomm,ierr)
          if(myid.eq.0)write(io,*)'***** T2var *****'
!       call mpi_barrier(gcomm,ierr)

! ok up to here

  ! T3a
  do n=1,2!3
  do m=1,2!3
  !x dirn:
  field(:,:,:)=q(:,:,:,4,1)*gradYP(:,:,:,n,1)* &
                            (  gradYP(:,:,:,m,1)*gradUP(:,:,:,1,1)  &
                             + gradYP(:,:,:,m,2)*gradUP(:,:,:,1,2)  &
                             + gradYP(:,:,:,m,3)*gradUP(:,:,:,1,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3aVar(:,:,:,m,n,1), sqT3aVar(:,:,:,m,n,1),io)
  
  !y dirn:
  field(:,:,:)=q(:,:,:,4,1)*gradYP(:,:,:,n,2)*  &
                            (  gradYP(:,:,:,m,1)*gradUP(:,:,:,2,1)  &
                             + gradYP(:,:,:,m,2)*gradUP(:,:,:,2,2)  &
                             + gradYP(:,:,:,m,3)*gradUP(:,:,:,2,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3aVar(:,:,:,m,n,1), sqT3aVar(:,:,:,m,n,1),io)
  
  
  !z dirn:
  field(:,:,:)=q(:,:,:,4,1)*gradYP(:,:,:,n,3)*  &
                            (  gradYP(:,:,:,m,1)*gradUP(:,:,:,3,1)  &
                             + gradYP(:,:,:,m,2)*gradUP(:,:,:,3,2)  &
                             + gradYP(:,:,:,m,3)*gradUP(:,:,:,3,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3aVar(:,:,:,m,n,1), sqT3aVar(:,:,:,m,n,1),io)
  enddo
  enddo
  
!         call mpi_barrier(gcomm,ierr)
            if(myid.eq.0)write(io,*)'***** T3avar *****'
!         call mpi_barrier(gcomm,ierr)
  
! T3b
do n=1,2!3
do m=1,2!3
!x dirn:
  field(:,:,:)=q(:,:,:,4,1) *gradYP(:,:,:,n,1) * &
                            (  gradYP(:,:,:,m,1)*gradUM(:,:,:,1,1)  &
                             + gradYP(:,:,:,m,2)*gradUM(:,:,:,1,2)  &
                             + gradYP(:,:,:,m,3)*gradUM(:,:,:,1,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3bVar(:,:,:,m,n,1), sqT3bVar(:,:,:,m,n,1),io)

!!y dirn:
 field(:,:,:)=q(:,:,:,4,1)*gradYP(:,:,:,n,2) *  &
                           (  gradYP(:,:,:,m,1)*gradUM(:,:,:,2,1)  &
                            + gradYP(:,:,:,m,2)*gradUM(:,:,:,2,2)  &
                            + gradYP(:,:,:,m,3)*gradUM(:,:,:,2,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3bVar(:,:,:,m,n,1), sqT3bVar(:,:,:,m,n,1),io)

!z dirn:
  field(:,:,:)=q(:,:,:,4,1)*gradYP(:,:,:,n,3) *  &
                            (  gradYP(:,:,:,m,1)*gradUM(:,:,:,3,1)  &
                             + gradYP(:,:,:,m,2)*gradUM(:,:,:,3,2)  &
                             + gradYP(:,:,:,m,3)*gradUM(:,:,:,3,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3bVar(:,:,:,m,n,1), sqT3bVar(:,:,:,m,n,1),io)
enddo
enddo
  
    
!       call mpi_barrier(gcomm,ierr)
          if(myid.eq.0)write(io,*)'***** T3bvar *****'
              

 ! T3c
 do m=1,2!3
 !x dirn:
 field(:,:,:)=q(:,:,:,4,1)*(  gradYP(:,:,:,m,1)*gradUP(:,:,:,1,1)  &
                            + gradYP(:,:,:,m,2)*gradUP(:,:,:,1,2)  &
                            + gradYP(:,:,:,m,3)*gradUP(:,:,:,1,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3cVar(:,:,:,m,1), sqT3cVar(:,:,:,m,1),io)
 
 !y dirn:
 field(:,:,:)=q(:,:,:,4,1)*(  gradYP(:,:,:,m,1)*gradUP(:,:,:,2,1)  &
                            + gradYP(:,:,:,m,2)*gradUP(:,:,:,2,2)  &
                            + gradYP(:,:,:,m,3)*gradUP(:,:,:,2,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3cVar(:,:,:,m,2), sqT3cVar(:,:,:,m,2),io)
 
 !z dirn:
 field(:,:,:)=q(:,:,:,4,1)*(  gradYP(:,:,:,m,1)*gradUP(:,:,:,3,1)  &
                            + gradYP(:,:,:,m,2)*gradUP(:,:,:,3,2)  &
                            + gradYP(:,:,:,m,3)*gradUP(:,:,:,3,3))
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T3cVar(:,:,:,m,3), sqT3cVar(:,:,:,m,3),io)
 enddo

    
!       call mpi_barrier(gcomm,ierr)
          if(myid.eq.0)write(io,*)'***** T3cvar *****'
              
  
  ! T4
  do i=1,3
  do n=1,2!3
  do m=1,2!3
   field(:,:,:)=alpha(:,:,:)*gradYP(:,:,:,m,i)*gradWP(:,:,:,n,i)
     call accum_planar(field, cond, wt, windowx, windowy, windowz, T4Var(:,:,:,m,n,1), sqT4Var(:,:,:,m,n,1),io)
  enddo
  enddo
  enddo
  
    
!       call mpi_barrier(gcomm,ierr)
          if(myid.eq.0)write(io,*)'***** T4var *****'
                
!   ! T5
!   do n=1,2!3
!   do m=1,2!3
!   
!   !x direction:
!    call derivative_x(nx,ny,nz, YI(:,:,:,n), fieldx(:,:,:),scale_1x,1)
!    fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(invLe(n,n)-invLe(m,n))*fieldx(:,:,:)
!    call derivative_x(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1x,1)
!   
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!   
!    field(:,:,:) = fieldx(:,:,:)*gradYP(:,:,:,m,1)  &
!                 + fieldy(:,:,:)*gradYP(:,:,:,m,2)  &
!                 + fieldz(:,:,:)*gradYP(:,:,:,m,3)
!    field(:,:,:) = field(:,:,:)*alpha(:,:,:)
!     call accum_planar(field, cond, wt, windowx, windowy, windowz, T5Var(:,:,:,m,n,1), sqT5Var(:,:,:,m,n,1),io)
!    
!   !y direction:
!    call derivative_y(nx,ny,nz, YI(:,:,:,n), fieldx(:,:,:),scale_1y,1)
!    fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(invLe(n,n)-invLe(m,n))*fieldx(:,:,:)
!    call derivative_y(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1y,1)
!   
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!   
!    field(:,:,:) = fieldx(:,:,:)*gradYP(:,:,:,m,1)  &
!                 + fieldy(:,:,:)*gradYP(:,:,:,m,2)  &
!                 + fieldz(:,:,:)*gradYP(:,:,:,m,3)
!    field(:,:,:) = field(:,:,:)*alpha(:,:,:)
!     call accum_planar(field, cond, wt, windowx, windowy, windowz, T5Var(:,:,:,m,n,1), sqT5Var(:,:,:,m,n,1),io)
!   
!   !z direction:
!    call derivative_z(nx,ny,nz, YI(:,:,:,n), fieldx(:,:,:),scale_1z,1)
!    fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(invLe(n,n)-invLe(m,n))*fieldx(:,:,:)
!    call derivative_z(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1z,1)
!   
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
!   
!    field(:,:,:) = fieldx(:,:,:)*gradYP(:,:,:,m,1)  &
!                 + fieldy(:,:,:)*gradYP(:,:,:,m,2)  &
!                 + fieldz(:,:,:)*gradYP(:,:,:,m,3)
!    field(:,:,:) = field(:,:,:)*alpha(:,:,:)
!     call accum_planar(field, cond, wt, windowx, windowy, windowz, T5Var(:,:,:,m,n,1), sqT5Var(:,:,:,m,n,1),io)
!   
!   enddo
!   enddo
! 

  ! T5
  do n=1,2!3
  do m=1,2!3
  
  !x direction:
   call derivative_x(nx,ny,nz, YI(:,:,:,n), fieldx(:,:,:),scale_1x,1)
   fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(invLe(n,n)-invLe(m,n))*fieldx(:,:,:)
   call derivative_x(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1x,1)

   fieldz=field

  !y direction:
   call derivative_y(nx,ny,nz, YI(:,:,:,n), fieldx(:,:,:),scale_1y,1) 
   fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(invLe(n,n)-invLe(m,n))*fieldx(:,:,:)
   call derivative_y(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1y,1)

   fieldz=fieldz+field

  !z direction:
   call derivative_z(nx,ny,nz, YI(:,:,:,n), fieldx(:,:,:),scale_1z,1)
   fieldx(:,:,:)=q(:,:,:,4,1)*alpha(:,:,:)*(invLe(n,n)-invLe(m,n))*fieldx(:,:,:)
   call derivative_z(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1z,1)

   fieldz=fieldz+field
   field=fieldz
  
   call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
   call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
   call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
  
   field(:,:,:) = fieldx(:,:,:)*gradYP(:,:,:,m,1)  &
                + fieldy(:,:,:)*gradYP(:,:,:,m,2)  &
                + fieldz(:,:,:)*gradYP(:,:,:,m,3)
   field(:,:,:) = field(:,:,:)*alpha(:,:,:)
    call accum_planar(field, cond, wt, windowx, windowy, windowz, T5Var(:,:,:,m,n,1), sqT5Var(:,:,:,m,n,1),io)
  
  enddo
  enddo


!    call mpi_barrier(gcomm,ierr) 
    if(myid.eq.0)write(io,*)'***** RHS filtered *****'
       
deallocate(YI)
deallocate(YP)
deallocate(UP)
deallocate(WI)
deallocate(WP)
deallocate(PP)
deallocate(ALPHA)
deallocate(VISC)

deallocate(gradYP)
deallocate(gradUP)
deallocate(gradWP)
deallocate(grad2YP)

deallocate(dY2dY1)
  END SUBROUTINE ACCUMULATE_STEP

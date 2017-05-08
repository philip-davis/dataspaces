#include "globalDefines.h"

!**********************************************************************
!*********************turbulent tranpsort analysis*********************
!***************************second pass********************************
!**********************************************************************

SUBROUTINE filter_field_2pss_TT(io,finish )
  use variables_m, only : q, u, temp, pressure, yspecies, volum
  USE HDF5
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g
!  use s3d_hdf_interface_m  !no longer used
  use reference_m
  use chemkin_m, only: reaction_rate
  use zclookup_m
  use transport_m
  use thermchem_m
  use premix_drvd_var_m

  IMPLICIT NONE

  INTEGER io
  
  INTEGER NXF,NYF,NZF, NXF_G, NYF_G, NZF_G

  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: filteredfield
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: DF, CF, AF, VF, DFIL, RHOSL
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: KEMECH, EPSMECH
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: UF,YF,WF, UFIL, YFIL, WFIL
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: YYF, YPYPF, UPYPF, T3cVar
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :, :) :: DISSvar, T1aVar, T1bVar, T2var, T3aVar, T3bVar, T4Var, T5var
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: KEDIFF, KET1, KET2, KET3, KET4, KET5
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: AdYPdYPF
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :) :: RR_R, DIFFUSION ! RR_R = d(rhoYi)/dt = /omega_i as required in the time scale equation.
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: FIELD, FIELDX, FIELDY, FIELDZ
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :) :: KE, EPS
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :) :: FLXLAM, FLXKEPS, FLXCNR, LCLU, LCLT, FLXDNS, FLXCGD
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :, :) :: CONV, DIFF, T1b, T3c
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: DENS
!  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :, :) :: DISS, T1a, T2, T3a, T3b, T4, T5
    !----------------------------------------------------------------------
    ! hdf declarations


    CHARACTER(LEN=20) :: groupname  ! Group name
    CHARACTER(LEN=20) :: fieldname  ! Field name
    CHARACTER(LEN=250) :: fielddesc  ! Very long field name
    CHARACTER(LEN=20) :: fieldunits  ! Field units
    CHARACTER(LEN=50) :: filename   ! File name

    CHARACTER(LEN=2) :: sid_ext
    CHARACTER(LEN=1) :: Y1id_ext, Y2id_ext, direction(3)
    CHARACTER(LEN=50) :: nomspec(3)

    LOGICAL :: overwrite
    INTEGER :: error
    REAL, ALLOCATABLE, DIMENSION(:,:) :: FX, FY, FZ
    REAL, ALLOCATABLE, DIMENSION(:) :: FXG, FYG, FZG
    real x2(nx/2), delta_fil_grid_x, delta_fil_grid_y,delta_fil_grid_z, delta, influence
    integer i2, i, j,k, m,n
    integer method

    integer, dimension(3) :: tau_indx
    real, dimension(3) :: fac_indx
    logical, intent(in) :: finish
    logical, save :: initialized=.false.
    REAL, SAVE, DIMENSION(3,3) :: invLe

    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ALPHA, VISC
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: YI, YP, UP, WI, WP
    REAL, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: YM, UM, WM
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: gradYP,gradUP,gradWP
    REAL, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) :: gradYM, gradUM
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: grad2YP

 
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: basics, basics_g
    REAL, ALLOCATABLE, DIMENSION(:) :: onedim, onedim_g
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: ke_eps, ke_eps_g
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: eps_eq, eps_eq_g
    
    REAL, DIMENSION(3) :: xplot, zplot, xval, zval
    INTEGER :: ic,id,ie,iy, ixplot,izplot, root_id
    INTEGER, DIMENSION(4) :: iorder

    REAL, DIMENSION(7) :: zcoeff
    REAL :: rhosl1val

    real :: writemin, writemax
    REAL :: tstep

    writemin=0.05
    writemax=0.95
!  xplot(1)=0.25
!  xplot(2)=0.50
!  xplot(3)=0.75 ! don't use 1.00
!  !ixplot=max(1,1+int(xplot(i)*real(nxf*npx-1))-xid*nxf)
!  
!  do i=1,3
!  ixplot=1+int(xplot(i)*real(nxf*npx-1))    
!  xval(i)=xg(ixplot)
!  !xval(i)=xplot(i)*real(nxf*npx-1)*(x(2)-x(1))
!  if(myid.eq.0)write(io,*)i,'xplot',xplot(i),'ixplot','xval',xval(i),nxf,npx
!  enddo
!  
!  zplot(1)=0.00
!  zplot(2)=0.25
!  zplot(3)=0.50
!  
!  do i=1,3
!  izplot=1+int(zplot(i)*real(nzf*npz-1))             
!  zval(i)=zg(izplot)
!  !zval(i)=zplot(i)*real(nzf*npz-1)*(z(2)-z(1))
!  enddo
!      
!  if(myid.eq.0)write(io,*)'xval,zval',xval*l_ref,zval*l_ref
    
    ! SPECIFY FILTERED GRID ======================================================================

! you MUST use nxf_g=nx_g or you will have BIG TROUBLE.

    nxf_g = nx*npx     ! set the filtered and unfiltered grids to the same numbers so that I can use the S3D derivatives.
    nyf_g = ny*npy     ! note that the filtered grid is uniform and there is a miss-match where there is grid stretching.
    nzf_g = nz*npz 

    if(n_spec==15)then
      tau_indx(1)=4   !this is the O2 mass fraction
      tau_indx(2)=14  !this is the passive progress variable
      tau_indx(3)=2
    else
      tau_indx(1)=4   !this is the O2 mass fraction
      tau_indx(2)=29  !this is the passive progress variable, NB this will not work for the part premixed case.
      tau_indx(3)=2
    endif
      fac_indx(1)=1.0
      fac_indx(2)=1.0e7
      fac_indx(3)=1.0                  



!    delta_fil_grid_x =2.4000e-2 / (nxf_g-1)!(8.925-7.59)/(nxf_g-1)*1e-3! 
!    delta_fil_grid_y =1.1980e-2 / (nyf_g-1)!(4.4525-1.57248)/(nyf_g-1)*1e-3!   ! this grid size is based on the unstretched spacing x (nxg-1).
!    delta_fil_grid_z= 0.7200e-2 / (nzf_g-1)!(5.445-4.665)/(nzf_g-1)*1e-3!
    
    delta_fil_grid_x = l_ref*(xg(nxf_g)-xg(1))/ (nxf_g-1)!
    delta_fil_grid_y = l_ref*(yg(nyf_g/2+1)-yg(nyf_g/2))
!    delta_fil_grid_y = l_ref*(yg(nyf_g)-yg(1))/ (nyf_g-1)!
    delta_fil_grid_z = l_ref*(zg(nzf_g)-zg(1))/ (nzf_g-1)!
    
!       delta_fil_grid_x =(8.925-7.59)/(nxf_g-1)*1e-3
!       delta_fil_grid_y =(4.4525-1.57248)/(nyf_g-1)*1e-3
!       delta_fil_grid_z= (5.445-4.665)/(nzf_g-1)*1e-3


    if( mod(nxf_g,npx).ne.0) then
       write(io,*) 'Filtered grid size is not perfectly divisible by npx in x-direction'
       stop
    else
       NXF = nxf_g/npx
    endif
    if( mod(nyf_g,npy).ne.0) then
       write(io,*) 'Filtered grid size is not perfectly divisible by npy in y-direction'
       stop
    else
       NYF = nyf_g/npy
    endif
    if( mod(nzf_g,npz).ne.0) then
       write(io,*) 'Filtered grid size is not perfectly divisible by npz in z-direction'
       stop
    else
       NZF = nzf_g/npz
    endif


    allocate(fxg(nxf_g))
    allocate(fyg(nyf_g))
    allocate(fzg(nzf_g))

    if(myid==0) then
       FXG = 0.0d0
       FYG = 0.0d0
       FZG = 0.0d0

       do i=1,nxf_g
          FXG(i) = (i-1)*delta_fil_grid_x !+ 7.59e-3
       enddo
       do j=1,nyf_g
          FYG(j) = (j-1)*delta_fil_grid_y - 0.599e-2   !offset used since domain is symmetrical in y.
       enddo
       do k=1,nzf_g
          FZG(k) = (k-1)*delta_fil_grid_z !+ 4.665e-3
       enddo
!       write(*,*) 'fzg=',fzg
       FXG = FXG/l_ref
       FYG = FYG/l_ref
       FZG = FZG/l_ref
    endif

    ! Broadcast FxyzG to all root processes - easiest just to broadcast to all
    CALL MPI_Bcast( FXG, nxf_g, MPI_DOUBLE_PRECISION, 0, gcomm, ierr)
    CALL MPI_Bcast( FYG, nyf_g, MPI_DOUBLE_PRECISION, 0, gcomm, ierr)
    CALL MPI_Bcast( FZG, nzf_g, MPI_DOUBLE_PRECISION, 0, gcomm, ierr)
                
    allocate(filteredfield(nxf,nyf,nzf))       
    allocate(field(nxf,nyf,nzf))
    allocate(fieldx(nxf,nyf,nzf))
    allocate(fieldy(nxf,nyf,nzf))
    allocate(fieldz(nxf,nyf,nzf))
    allocate(RR_R(nx,ny,nz,n_spec))    
    allocate(DIFFUSION(nx,ny,nz,n_spec))
    allocate(fx(nxf,npx*npy*npz))
    allocate(fy(nyf,npx*npy*npz))
    allocate(fz(nzf,npx*npy*npz))

    FX = 0
    FY = 0
    FZ = 0

    RR_R=0.0

    CALL MPI_Scatter( fxg, nxf, MPI_DOUBLE_PRECISION, FX(:,myid+1), nxf, MPI_DOUBLE_PRECISION, 0,xcomm, ierr)
    CALL MPI_Scatter( fyg, nyf, MPI_DOUBLE_PRECISION, FY(:,myid+1), nyf, MPI_DOUBLE_PRECISION, 0,ycomm, ierr)
    CALL MPI_Scatter( fzg, nzf, MPI_DOUBLE_PRECISION, FZ(:,myid+1), nzf, MPI_DOUBLE_PRECISION, 0,zcomm, ierr)

!    FX(:,myid+1) = x
!    FY(:,myid+1) = y
!    FZ(:,myid+1) = z
!
    deallocate(fxg)
    deallocate(fyg)
    deallocate(fzg)

xplot(1)=0.25
xplot(2)=0.50
xplot(3)=0.75 ! don't use 1.00
!ixplot=max(1,1+int(xplot(i)*real(nxf*npx-1))-xid*nxf)

do i=1,3
ixplot=1+int(xplot(i)*real(nxf*npx-1))    
xval(i)=xg(ixplot)
!xval(i)=xplot(i)*real(nxf*npx-1)*(x(2)-x(1))
if(myid.eq.0)write(io,*)i,'xplot',xplot(i),'ixplot','xval',xval(i),nxf,npx
enddo

zplot(1)=0.00
zplot(2)=0.25
zplot(3)=0.50

do i=1,3
izplot=1+int(zplot(i)*real(nzf*npz-1))             
zval(i)=zg(izplot)
!zval(i)=zplot(i)*real(nzf*npz-1)*(z(2)-z(1))
enddo
    
if(myid.eq.0)write(io,*)'xval,zval',xval*l_ref,zval*l_ref


    

    ! FILTER FIELD ===========================================================================
    method = 1 !1 For top hat, 2 for gaussian
!    delta = 0.10e-3 ! in m, will be normalized by lref inside generalized_filter
    delta = delta_fil_grid_x*9.0
    if(myid.eq.0)write(io,*)'filter full width = ',delta
! to include 27 points in the box filter make delta =(2+1)*the dns grid spacing.
! to include 125 points in the box filter make delta=(4+1)*the dns grid spacing.
    !delta = 0.3124e-3 ! in m, will be normalized by lref inside generalized_filter
    influence = 2.0*delta ! use to exclude calculation of points far from filtered grid point


   if(myid.eq.0)write(io,*)'***** initializing for the second pass *****'
   if(myid.eq.0)write(io,*)'tau_indx ', tau_indx
       
  if(.not. finish)then       

  if(.not.initialized) then 

  allocate(YF(nxf,nyf,nzf,n_spec))
  allocate(UF(nxf,nyf,nzf,3))
  allocate(WF(nxf,nyf,nzf,n_spec))

!  read YF,UF,WF from the HDF files, taking care to non-dimensionalise them.
  call read_filtfields(io,uf,yf,wf)

if(myid.eq.0)write(io,*) 'uf',uf(5,5,5,1)*a_ref


if(n_spec.eq.29)then   ! the TFL CH4 mechanism for non-premixed combustion plus mixture fraction.
invLe(1,1)=0.9001
invLe(2,2)=1.0000
invLe(3,3)=5.5555
else                    ! the 13 spec TFL CH4 mechanism for lean flames plus two mixture fractions.
invLe(1,1)=0.9259
invLe(2,2)=1.0000
invLe(3,3)=5.8824
endif

invLe(1,2)=0.5*(invLe(1,1)+invLe(2,2))
invLe(2,1)=invLe(1,2)
invLe(1,3)=0.5*(invLe(1,1)+invLe(3,3))
invLe(3,1)=invLe(1,3)
invLe(2,3)=0.5*(invLe(2,2)+invLe(3,3))
invLe(3,2)=invLe(2,3)     

   if(myid.eq.0)write(io,*)'invLe set'

  allocate(CF(nxf,nyf,nzf))
  allocate(DFIL(nxf,nyf,nzf))
  allocate(AF(nxf,nyf,nzf))
  allocate(VF(nxf,nyf,nzf))
  allocate(RHOSL(nxf,nyf,nzf))
  allocate(UFIL(nxf,nyf,nzf,3))
  allocate(YFIL(nxf,nyf,nzf,2))

if(myid.eq.0)write(io,*)'yfil allocated'
  
  allocate(YPYPF(nxf,nyf,nzf,2,2))  
  allocate(UPYPF(nxf,nyf,nzf,3,2))
  allocate(AdYpdYPF(nxf,nyf,nzf,2,2))
  

  allocate(KEMECH(nxf,nyf,nzf))
  allocate(EPSMECH(nxf,nyf,nzf))

if(myid.eq.0)write(io,*)'t5var allocated'

  allocate(YM(nxf,nyf,nzf,2))
  allocate(UM(nxf,nyf,nzf,3))
  allocate(WM(nxf,nyf,nzf,2))

  allocate(gradYM(nxf,nyf,nzf,2,3))
  allocate(gradUM(nxf,nyf,nzf,3,3))

  YM = 0.0
  UM = 0.0
  WM = 0.0

  gradYM = 0.0
  gradUM = 0.0

! convert YM from YO2 to progress variable at this point, 
! this is not exact since we should have averaged progress variable in the first place,
! however since we are looking on the lean side of stoichiometry, the burnt/unburnt YO2
! should be pretty linear in mixture fraction and it is ok to convert it here.
if(myid.eq.0)write(io,*)'Conversion of mean YO2 to mean progress variable has been hard wired'

if(n_spec.eq.15)then
  field(:,:,:)=YF(:,:,:,n_spec-2)*1.0E7
else
  field(:,:,:)=YF(:,:,:,n_spec-1)*1.0E7
endif

! sets YM(1) to progress variable based on YF(O2) and YF(mixfrac).
 call calc_YtoCfield(YM(:,:,:,1),YF(:,:,:,tau_indx(1)),field,io)                                              
! YM(:,:,:,1)=YF(:,:,:,tau_indx(1))*fac_indx(1)
 YM(:,:,:,2)=YF(:,:,:,tau_indx(2))*fac_indx(2)  !scaled to give MixFrac
! YM(:,:,:,3)=YF(:,:,:,tau_indx(3))*fac_indx(3)   

 UM(:,:,:,:)=UF(:,:,:,:)

 WM(:,:,:,1)=WF(:,:,:,tau_indx(1))*fac_indx(1)     
 WM(:,:,:,2)=WF(:,:,:,tau_indx(2))*fac_indx(2)
! WM(:,:,:,3)=WF(:,:,:,tau_indx(3))*fac_indx(3)

 !dYM
  do i=1,2!3
  call derivative_x(nx,ny,nz, YM(:,:,:,i), gradYM(:,:,:,i,1),scale_1x,1)
  call derivative_y(nx,ny,nz, YM(:,:,:,i), gradYM(:,:,:,i,2),scale_1y,1)
  call derivative_z(nx,ny,nz, YM(:,:,:,i), gradYM(:,:,:,i,3),scale_1z,1)
  enddo

  do i=1,3
  call derivative_x(nx,ny,nz, UM(:,:,:,i), gradUM(:,:,:,i,1),scale_1x,1)
  call derivative_y(nx,ny,nz, UM(:,:,:,i), gradUM(:,:,:,i,2),scale_1y,1)
  call derivative_z(nx,ny,nz, UM(:,:,:,i), gradUM(:,:,:,i,3),scale_1z,1)
  enddo



  deallocate(YF)
  deallocate(UF)
  deallocate(WF)
  
  CF = 0.0      ! these are the 'saved' variable counters
  DFIL = 0.0
  AF = 0.0
  VF = 0.0
  RHOSL = 0.0
!  WFIL = 0.0
  UFIL = 0.0
  YFIL = 0.0
!  YYF = 0.0
  YPYPF = 0.0
  UPYPF = 0.0
  AdYpdYPF = 0.0

  KEMECH = 0.0
  EPSMECH = 0.0

initialized=.true.

endif

    if(myid.eq.0)write(io,*)'***** Initialization complete for second filtering pass *****'
        
    
allocate(YI(nxf,nyf,nzf,2))
allocate(YP(nxf,nyf,nzf,2))
allocate(UP(nxf,nyf,nzf,3))
allocate(WI(nxf,nyf,nzf,2))
allocate(WP(nxf,nyf,nzf,2))
allocate(Alpha(nx,ny,nz))
allocate(VISC(nx,ny,nz))

allocate(gradYP(nxf,nyf,nzf,2,3))
allocate(gradUP(nxf,nyf,nzf,3,3))
allocate(gradWP(nxf,nyf,nzf,2,3))
allocate(grad2YP(nxf,nyf,nzf,2,3,3))


YI = 0.0
YP = 0.0 
UP = 0.0
WI = 0.0
WP = 0.0
alpha = 0.0
visc = 0.0
rhosl = 0.0
gradYP = 0.0
gradUP = 0.0 
gradWP = 0.0
grad2YP = 0.0

  if(myid.eq.0)write(io,*)'***** gradient allocation done *****'
        

! Find Yprime
! (YI is Y_instananeous, YF is Y_filtered = YM for Y_mean, YP is Y_prime)
 call SpecToProg(yspecies, io)   !this sets zetafield and PartProg
 YI(:,:,:,1)=PartProg(:,:,:)

! YI(:,:,:,1)=yspecies(:,:,:,tau_indx(1))*fac_indx(1)
 YI(:,:,:,2)=yspecies(:,:,:,tau_indx(2))*fac_indx(2)  !scaled to give MixFrac
! YI(:,:,:,3)=yspecies(:,:,:,tau_indx(3))*fac_indx(3)
 YP(:,:,:,:)=YI(:,:,:,:)-YM(:,:,:,:)

! Find Uprime
 UP(:,:,:,:)=U(:,:,:,:)-UM(:,:,:,:)

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

deallocate(rr_r)
deallocate(diffusion)
 if(myid.eq.0)write(io,*)'***** Prime values found *****'
         
  call computeCoefficients( cpmix, temp)
 ALPHA(:,:,:) = getSpcsDiffusivity(tau_indx(2))*volum  ! alpha=D_z due to unity lewis no. of MF.
 VISC(:,:,:)  = getViscosity()

 if(n_spec.eq.15)then
 zcoeff(1)=0.2524
 zcoeff(2)=1.1933
 zcoeff(3)=0.4882
 zcoeff(4)=-1.8871
 zcoeff(5)=1.8294
 zcoeff(6)=-1.0367
 zcoeff(7)=0.2352
 else
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
if(tau_indx(2).eq.13)then
do ic=0,6
rhosl1val=rhosl1val+zcoeff(ic+1)*1.0**ic
enddo
else !tau_2 = 28
do ic=0,6
rhosl1val=rhosl1val+zcoeff(ic+1)*0.702**ic
enddo
endif
write(io,*) 'rho_unbrnt * Sl _stoich = ',rhosl1val, ' it should be close to 1.07.'
endif !myid


 if(myid.eq.0)write(io,*)'***** Alpha found *****'
       
 
 ! CHECK IT COMPILES AND RUNS UP TO HERE.... seems ok.
 
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

! check it isn't the derivatives causing a segmentation fault.

! some variables such as UFIL are recalculated since it is not guaranteed that the user
! used the same DNS time steps to evaluate the quantities on the first 
! pass, in which case UM(:,1).ne.UFIL(:,1).


    if(myid.eq.0)write(io,*)'***** Starting main filtering block *****'
        

! Counter
 field(:,:,:) = 1.0
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)       
 CF(:,:,:)=CF(:,:,:)+filteredfield
! RHO
 field(:,:,:) = q(:,:,:,4,1)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
 DFIL(:,:,:)=DFIL(:,:,:)+filteredfield
! ALPHA
 field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
 AF(:,:,:)=AF(:,:,:)+filteredfield 
! VISC
 field(:,:,:) = q(:,:,:,4,1)*visc(:,:,:)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)
 VF(:,:,:)=VF(:,:,:)+filteredfield

 if(tau_indx(2).eq.13.or.tau_indx(2).eq.28)then  ! then the second species is MF and we can use it
! to find rho_sl as a function of mixture fraction, otherwise set rho_sl to the stoichiometric value.
  field=0.0
  do ic=0,6
    field(:,:,:)=field(:,:,:) + zcoeff(ic+1)*YI(:,:,:,2)**ic
  enddo
  field(:,:,:)=field(:,:,:)/rho_ref/a_ref
 else
  field=1.0747/rho_ref/a_ref  ! this is the stoichiometriv value for methane-air at 800K.
 endif
! RHOSL
 field(:,:,:) = q(:,:,:,4,1)*field(:,:,:)   !rhosl = RHO_u(zeta) *SL (zeta)
!subroutine calc_zrhofield(rho0,c,zeta,press_in,io)                                 
! subroutine SpecToProg(ysp,io) 
! call SpecToProg(yspecies, io)   !this sets zeta
 fieldz=0.0
 call calc_zrhofield(fieldx,fieldz,zetafield,pressure,io)
 fieldz=1.0
 call calc_zrhofield(fieldy,fieldz,zetafield,pressure,io)
  
 field(:,:,:)=field(:,:,:)*(1.0/fieldy(:,:,:)-1.0/fieldx(:,:,:)) ! now field= sl(zeta)*(tau(zeta)) where tau is (rhob/rhou-1)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)
 RHOSL(:,:,:)=RHOSL(:,:,:)+filteredfield
! RHOU1
 do i=1,3
 field(:,:,:) = q(:,:,:,i,1)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
 UFIL(:,:,:,i)=UFIL(:,:,:,i)+filteredfield
 enddo
! RHOY1
 do i=1,2!3
 field(:,:,:) = YI(:,:,:,i)*q(:,:,:,4,1)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
 YFIL(:,:,:,i)=YFIL(:,:,:,i)+filteredfield
 enddo
      
! KE
 do i=1,2!3
 field(:,:,:) = q(:,:,:,4,1)*YP(:,:,:,i)*YP(:,:,:,1)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
 YPYPF(:,:,:,i,1)=YPYPF(:,:,:,i,1)+filteredfield  
 enddo
 do i=2,2!3
 field(:,:,:) = q(:,:,:,4,1)*YP(:,:,:,i)*YP(:,:,:,2)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
 YPYPF(:,:,:,i,2)=YPYPF(:,:,:,i,2)+filteredfield  
 enddo
  
! EPS
 do j=1,2!3 !species loop
 field(:,:,:)= gradYP(:,:,:,j,1)*gradYP(:,:,:,1,1)  &
             + gradYP(:,:,:,j,2)*gradYP(:,:,:,1,2)  &
             + gradYP(:,:,:,j,3)*gradYP(:,:,:,1,3)
 field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)*field(:,:,:)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
 AdYPdYPF(:,:,:,j,1)=AdYPdYPF(:,:,:,j,1)+filteredfield
 enddo
 do j=2,2!3
 field(:,:,:)= gradYP(:,:,:,j,1)*gradYP(:,:,:,2,1)  &
             + gradYP(:,:,:,j,2)*gradYP(:,:,:,2,2)  &
             + gradYP(:,:,:,j,3)*gradYP(:,:,:,2,3)
 field(:,:,:) = q(:,:,:,4,1)*alpha(:,:,:)*field(:,:,:)
 CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
 AdYPdYPF(:,:,:,j,2)=AdYPdYPF(:,:,:,j,2)+filteredfield
 enddo

 !KEMECH
  field(:,:,:)=0.0
  do i=1,3
  field(:,:,:) = field(:,:,:)+q(:,:,:,4,1)*UP(:,:,:,i)*UP(:,:,:,i)
  enddo
  field=field*0.5
  CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)          
  KEMECH(:,:,:)=KEMECH(:,:,:)+filteredfield  
 
 
 !EPSMECH
 !  subroutine calculate_dissipation(big_u, lil_u, volum, epsln)
  call calculate_dissipation(U,UP,volum,field)
  field(:,:,:)=field(:,:,:)*q(:,:,:,4,1)
  CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)
  EPSMECH(:,:,:)=EPSMECH(:,:,:)+filteredfield
 
!UPYPF
 do j=1,2
 do i=1,3
   field(:,:,:) = q(:,:,:,4,1)*UP(:,:,:,i)*YP(:,:,:,j)
   CALL generalized_filter(io, field, filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)
   UPYPF(:,:,:,i,j)=UPYPF(:,:,:,i,j)+filteredfield   
 enddo
 enddo

deallocate(YI)
deallocate(YP)
deallocate(UP)
deallocate(WI)
deallocate(WP)
deallocate(ALPHA)
deallocate(VISC)

deallocate(gradYP)
deallocate(gradUP)
deallocate(gradWP)
deallocate(grad2YP)

 else !finish ! don't use this else until I have sorted the HDF reading.

if(myid.eq.0)write(io,*)'***** starting finish step *****'
        
    allocate(KE(nxf,nyf,nzf,2,2))
    allocate(EPS(nxf,nyf,nzf,2,2))

    allocate(FLXLAM(nxf,nyf,nzf,1,1))
    allocate(FLXKEPS(nxf,nyf,nzf,1,1))
    allocate(FLXCNR(nxf,nyf,nzf,1,1))
    allocate(FLXDNS(nxf,nyf,nzf,1,1))
    allocate(FLXCGD(nxf,nyf,nzf,1,1))
    allocate(LCLU(nxf,nyf,nzf,1,1))
    allocate(LCLT(nxf,nyf,nzf,1,1))
  
    allocate(DENS(nxf,nyf,nzf))

 AF(:,:,:)=AF(:,:,:)/CF(:,:,:)  !since we require a Reynolds average of </rho /alpha>
 VF(:,:,:)=VF(:,:,:)/DFIL(:,:,:)
 RHOSL(:,:,:)=RHOSL(:,:,:)/DFIL(:,:,:)
 DENS(:,:,:)=DFIL(:,:,:)/CF(:,:,:)
 do i=1,3
 UFIL(:,:,:,i)=UFIL(:,:,:,i)/DFIL(:,:,:)
 enddo
 do i=1,2!3
 YFIL(:,:,:,i)=YFIL(:,:,:,i)/DFIL(:,:,:)
 enddo
 
 
 do j=1,2!3
 do i=1,2!3
 KE(:,:,:,i,j)=YPYPF(:,:,:,i,j)/DFIL(:,:,:)
 enddo
 enddo
! KE(:,:,:,2,3)=KE(:,:,:,3,2)
 KE(:,:,:,1,2)=KE(:,:,:,2,1)
! KE(:,:,:,1,3)=KE(:,:,:,3,1)

 KEMECH(:,:,:)=KEMECH(:,:,:)/DFIL(:,:,:)
 EPSMECH(:,:,:)=EPSMECH(:,:,:)/DFIL(:,:,:)

!NB, it is possible to check that Y" is found ok, by alternatively evaluating KE using YY-Y.Y
 
 do j=1,2!3
 do i=1,2!3
 EPS(:,:,:,i,j)=AdYPdYPF(:,:,:,i,j)/DFIL(:,:,:)
 enddo
 enddo
! EPS(:,:,:,2,3)=EPS(:,:,:,3,2)  !this is required since we economised by not evaluating both during averaging.
 EPS(:,:,:,1,2)=EPS(:,:,:,2,1)  !make sure this is the right way around or you'll get rubbish!
! EPS(:,:,:,1,3)=EPS(:,:,:,3,1)

FLXLAM(:,:,:,1,1) = -AF(:,:,:) * (GRADYM(:,:,:,1,2)/CF(:,:,:))  !NB gradyf is already the derivative of a favre average
field(:,:,:)=0.09*KEMECH(:,:,:)*KEMECH(:,:,:)/(EPSMECH(:,:,:)+1.0e-10)  !the k-eps based turbulent diffusivity, with C_mu = 0.09
FLXKEPS(:,:,:,1,1)= - field(:,:,:) * (GRADYM(:,:,:,1,2)/CF(:,:,:))
field(:,:,:)= - UPYPF(:,:,:,2,2)/DFIL(:,:,:) / (1.0e-20+GRADYM(:,:,:,2,2))    ! the turbulent diffusivity based on the y direction flux of species 2, (add an option for the x flux of mixture fraction).
FLXCNR(:,:,:,1,1) = - field(:,:,:) * (GRADYM(:,:,:,1,2)/CF(:,:,:))
FLXDNS(:,:,:,1,1) = UPYPF(:,:,:,2,1)/DFIL(:,:,:)
FLXCGD(:,:,:,1,1) = YFIL(:,:,:,1)*(1.0-YFIL(:,:,:,1))*RHOSL(:,:,:) &
      *GRADYM(:,:,:,1,2)/(1.0E-10+ABS(GRADYM(:,:,:,1,2)))     !set direction normal to flame
LCLU(:,:,:,1,1) = abs(GRADUM(:,:,:,1,2)/(GRADYM(:,:,:,1,2)+1.0E-10)/(75.0/a_ref))
!assumes the velocity differential between jet core (100m/s) and free stream (25m/s) is constant.
!also assumes species 1 varies between 0.0 (unburned) and 1.0 (burnt).
LCLT(:,:,:,1,1) = abs(1.0/(GRADYM(:,:,:,1,2)+1.0E-10)/(0.09*(KEMECH(:,:,:)**1.5)/(EPSMECH(:,:,:)+1.0E-10)+1.0e-10))  !l_t = 0.09 k^1.5/eps


do j=1,3  ! zplot
do i=1,3  ! xplot

! x direction condition:
ixplot=max(1,1+int(xplot(i)*real(nxf*npx-1))-xid*nxf)
ixplot=max(min(ixplot,nxf),1)
! z direction condition:
izplot=max(1,1+int(zplot(j)*real(nzf*npz-1))-zid*nzf)
izplot=max(min(izplot,nzf),1)
if(myid.eq.0)write(io,*)'in side the loop at yid=',yid,'i,j',i,j


if(yid.eq.0 .and. &
   real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx)  .and.  &
   real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then

! write the stuff to a tecplot file

  write(y1id_ext,'(I1.1)') i
  write(y2id_ext,'(I1.1)') j
  nomspec(1)='O2'
  nomspec(2)='Xi'
  nomspec(3)='H'
  do ic=1,3
  write(direction(ic),'(I1.1)') ic
  enddo
  filename = '../post/yplots/fluxAnalysisX'//trim(Y1id_ext)//'Z'//trim(Y2id_ext)//'.tec'
 
!call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
if(myid.eq.0)write(io,*)'about to open the tecplot filed'
 
  open(unit=78,file=trim(filename),status='unknown')
!----------------------------------------------------------------------
!   write title of file

    write(78,*) 'title = "',trim(filename),'"'
!----------------------------------------------------------------------
!   write variables header
    write(78,*) 'variables = '
! write coordinates labels
    write(78,*) '"y (mm)"'
!   write variables labels
    write(78,*) '"u (m/s)"'
    write(78,*) '"v (m/s)"'
    write(78,*) '"w (m/s)"'
    write(78,*) '"y1"'
    write(78,*) '"y2"'
!    write(78,*) '"y3"'

!write the 2d variable labels
!ke
do ic=1,2!3
do id=1,2!3
  write(78,*) '"'//trim(nomspec(ic))//trim(nomspec(id))//'"'
enddo
enddo
!eps
do ic=1,2!3
do id=1,2!3
  write(78,*) '"eps_'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
enddo
enddo

  write(78,*) '"ke_mech (m^2/s^2)"'
  write(78,*) '"eps_mech (m^2/s^3)"'

  write(78,*) '"fluxlam (m/s)"'
  write(78,*) '"fluxkeps (m/s)"'
  write(78,*) '"fluxcnr (m/s)"'
  write(78,*) '"fluxdns (m/s)"'
  write(78,*) '"fluxcgd (m/s)"'
  write(78,*) '"lc/lu fluxkeps (m/s)"'
  write(78,*) '"lc/lt fluxkeps (m/s)"'
  write(78,*) '"lc/lu fluxcnr (m/s)"'
  write(78,*) '"lc/lt fluxcnr (m/s)"'
  write(78,*) '"lc/lu"'
  write(78,*) '"lc/lt"'
  write(78,*) '"lc"'
  write(78,*) '"lu"'
  write(78,*) '"lt"'  


!write the axis
    write(78,1) int(nyf*npy*writemax)-int(nyf*npy*writemin)+1
    write(78,9) (yg(iy)*l_ref*1e3, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))

endif
!endif
!endif


!loop over all the variables to be written, gather them, ifstatement, write them, cycle.

!velocity and species first

allocate(basics(3,3,nyf))
allocate(basics_g(3,3,nyf*npy))
basics=0.0
basics_g=0.0

do ic=1,3
  basics(1,ic,:)=ufil(ixplot,:,izplot,ic)   !ic= icomponent
enddo
do ic=1,2
  basics(2,ic,:)=yfil(ixplot,:,izplot,ic)
enddo
  if(myid.eq.0)write(io,*)'eps_eq set'
  call mpi_barrier(gcomm,ierr)   !probably this is not necessary.

  if(myid.eq.0)write(io,*)'***** about to gather *****'  

! find the root process of this gather, it is xid=xid, yid=0, zid=zid
root_id=0 
call MPI_Gather(basics,nyf*3*3,MPI_REAL8,basics_g,nyf*3*3,MPI_REAL8,root_id,ycomm,ierr)

if(yid.eq.0 .and. &
   real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx)  .and.  &
   real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then

 write(78,9) (basics_g(1,1,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
 write(78,9) (basics_g(1,2,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
 write(78,9) (basics_g(1,3,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
 write(78,9) (basics_g(2,1,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
 write(78,9) (basics_g(2,2,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
! write(78,9) (basics_g(2,3,iy), iy=int(nyf*npy*writemin,int(nyf*npy*writemax))

endif
deallocate(basics_g)

 if(myid.eq.0)write(io,*)'***** basics done, starting ke *****'
! ke
do ic=1,2!3
  do id=1,2!3
    basics(ic,id,:)=ke(ixplot,:,izplot,ic,id)  
  enddo
enddo
call write_basics(78, basics, xplot(i), zplot(j), 3, 3 ,writemin, writemax)

! eps
do ic=1,2!3
  do id=1,2!3
    basics(ic,id,:)=eps(ixplot,:,izplot,ic,id) 
  enddo
enddo
basics=basics/time_ref
call write_basics(78, basics, xplot(i), zplot(j), 3, 3, writemin, writemax )

if(myid.eq.0)write(io,*)'***** ke done, starting on flux quantities *****'
      
allocate(onedim(nyf));        onedim=0.0


!KEMECH
onedim(:)=kemech(ixplot,:,izplot)
onedim(:)=onedim(:)*a_ref*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!EPSMECH
onedim(:)=epsmech(ixplot,:,izplot)
onedim(:)=onedim(:)*a_ref*a_ref/time_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
        
!FLUXLAM
onedim(:)=flxlam(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
                   
!FLUXKEPS
onedim(:)=flxkeps(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
                  
!FLUXCNR
onedim(:)=flxcnr(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
      
!FLUXDNS
onedim(:)=flxdns(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!FLUXCGD
onedim(:)=flxCGD(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!LCLU FLUXKEPS
onedim(:)=lclu(ixplot,:,izplot,1,1)*flxkeps(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!LCLT FLUXKEPS
onedim(:)=lclt(ixplot,:,izplot,1,1)*flxkeps(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
                  
!LCLU FLUXCNR
onedim(:)=lclu(ixplot,:,izplot,1,1)*flxcnr(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!LCLT FLUXCNR
onedim(:)=lclt(ixplot,:,izplot,1,1)*flxcnr(ixplot,:,izplot,1,1)
onedim(:)=onedim(:)*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!LCLU 
onedim(:)=lclu(ixplot,:,izplot,1,1)
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!LCLT 
onedim(:)=lclt(ixplot,:,izplot,1,1)
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!lc,lu,lt
!lc
onedim(:)= abs(1.0/(GRADYM(ixplot,:,izplot,1,2)+1.0E-10))*l_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!lu
onedim(:)= abs(75.0/a_ref/(GRADUM(ixplot,:,izplot,1,2)+1.0E-10))*l_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!lt
onedim(:)= (0.09*(KEMECH(ixplot,:,izplot)**1.5)/(EPSMECH(ixplot,:,izplot)+1.0E-10)+1.0e-10) !l_t = 0.09 k^1.5/eps
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

deallocate(onedim)
deallocate(basics)    

enddo  !xplot(i)
enddo  !yplot(j)


  1 format(' zone t="stationary", i=',i5,', f=block')
  9 format(10(1pe12.5,1x))

!deallocate the variables used only in the finishing section
deallocate(KE)
deallocate(EPS)

!deallocate the saved variables
deallocate(CF)
deallocate(DFIL)
deallocate(AF)
deallocate(VF)
deallocate(RHOSL)
deallocate(UFIL)
deallocate(YFIL)
deallocate(YPYPF)
deallocate(UPYPF)
deallocate(AdYPdYPF)

deallocate(KEMECH)
deallocate(EPSMECH)

deallocate(FLXLAM)
deallocate(FLXKEPS)
deallocate(FLXCNR)
deallocate(FLXDNS)
deallocate(FLXCGD)
deallocate(LCLT)
deallocate(LCLU)

deallocate(YM)
deallocate(UM)
deallocate(WM)

deallocate(gradYM)
deallocate(gradUM)

initialized=.false.

endif  !finish

    deallocate(fx)
    deallocate(fy)
    deallocate(fz)
    deallocate(field)
    deallocate(fieldx)
    deallocate(fieldy)
    deallocate(fieldz)
    deallocate(filteredfield)
    
    if(myid.eq.0)write(io,*)'***** reached end of 2pss_TT routine ... phew! *****'

END SUBROUTINE FILTER_FIELD_2pss_TT

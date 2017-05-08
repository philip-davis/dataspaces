#include "globalDefines.h"
module timescales_m

  use variables_m, only : q, u, temp, pressure, yspecies, volum
!  USE HDF5
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g, iselect
!  use s3d_hdf_interface_m  !no longer used
  use reference_m
  use chemkin_m, only: reaction_rate
!  use zclookup_m
  use transport_m
  use thermchem_m
!  use premix_drvd_var_m

!  PRIVATE
  PUBLIC

! Public routines:
  public :: filter_field_2pss      
    
  
  INTEGER NXF,NYF,NZF, NXF_G, NYF_G, NZF_G

  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: filteredfield
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: DF, CF, AF, VF, DFIL, RHOSL,PF, PREY
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: KEMECH, EPSMECH, UPVP
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: KEMECH_tens, EPSMECH_tens
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: UF,YF,WF
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: UFIL, YFIL, WFIL, YcFIL
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: CFIL
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: YYF, YPYPF, T3cVar, YYcFIL
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :, :) :: DISSvar, T1aVar, T1bVar, UdYVar, T2var, T3aVar, T3bVar, T4Var, T5var, KET1b
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: KETRIP, KEDIFF, KET1, KET2, KET3, KET4, KET5
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: TK1  
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: sqTK1 
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: UYflux
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: UCflux
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: TK2, TK3, TK4  ! variables for the mechanical TKE equation that require three components at the accumulation statge
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: sqTK2, sqTK3, sqTK4
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: TKCONV, TK5! variables for the mechanical TKE equation that require single values at the accumulation statge 
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: sqTKCONV, sqTK5
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: Y1_Y2M,Y1_Y2sq,YWM,EPS_C  ! variables needed for the Darbyshire processing
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: sqY1_Y2M,sqY1_Y2sq,sqYWM,sqEPS_C  ! variables needed for the Darbyshire processing
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: AdYPdYPF
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: AdYPdYPF_MOD
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: DFILcond, CFcond,Dpdfcond,Cpdfcond
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: YPYPcond, gradCheck
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: Ycond, Wcond
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: CosAlpha, FlamNormGrad


! some variables for error analysis
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: sumrhogradYI, sumgradYP2
  
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :) :: sumgradYM, YflucError

! some variables to evaluate confidence intervals
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: sqDF, sqCF, sqAF, sqVF, sqDFIL, sqRHOSL
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :) :: sqKEMECH, sqEPSMECH, sqUPVP
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: sqKEMECH_tens, sqEPSMECH_tens
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: sqUF,sqYF,sqWF, sqUFIL, sqYFIL, sqYcFIL
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: sqYYF, sqYPYPF, sqT3cVar, sqYYcFIL
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :, :) :: sqDISSvar, sqT1aVar, sqT1bVar, sqUdYVar, sqT2var, sqT3aVar, sqT3bVar, sqT4Var, sqT5var, sqKET1b
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: sqKEDIFF, sqKET1, sqKET2, sqKET3, sqKET4, sqKET5
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :, :) :: sqAdYPdYPF
  REAL, SAVE :: COUNTER             ! this is the number of independent samples used in the averaging.

  REAL, ALLOCATABLE, DIMENSION(:, :, :, :) :: RR_R  ! RR_R = d(rhoYi)/dt = /omega_i as required in the time scale equation.
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: FIELD, FIELDX, FIELDY, FIELDZ
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :) :: INVEPS
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :) :: KE, EPS
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :) :: PDFCcond, PDFDcond
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: PDFCsum, PDFDsum
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :, :) :: CONV, DIFF, T1b, T3c
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: DENS

  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :, :) :: sqCONV, sqDIFF, sqT1b, sqT3c
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: sqDENS
!  REAL, ALLOCATABLE, DIMENSION(:, :, :, :, :, :) :: DISS, T1a, T2, T3a, T3b, T4, T5
    !----------------------------------------------------------------------
    ! hdf declarations


    CHARACTER(LEN=20) :: groupname  ! Group name
    CHARACTER(LEN=20) :: fieldname  ! Field name
    CHARACTER(LEN=250) :: fielddesc  ! Very long field name
    CHARACTER(LEN=20) :: fieldunits  ! Field units
    CHARACTER(LEN=50) :: filename   ! File name

    CHARACTER(LEN=2) :: sid_ext
    CHARACTER(LEN=1) :: Y1id_ext, Y2id_ext, direction(3)    !auto
    CHARACTER(LEN=8) :: Y1rl_ext, Y2rl_ext
    CHARACTER(LEN=50) :: nomspec(3)   !auto

    LOGICAL :: overwrite
    INTEGER :: error
    REAL, ALLOCATABLE, DIMENSION(:,:) :: FX, FY, FZ
    REAL, ALLOCATABLE, DIMENSION(:) :: FXG, FYG, FZG
    real  delta_fil_grid_x, delta_fil_grid_y,delta_fil_grid_z, delta, influence
    integer i2, i, j,k, m,n
    integer method

    integer, dimension(3) :: tau_indx  !auto
    real, dimension(3) :: fac_indx   !auto
    logical, save :: initialized=.false.
    REAL, SAVE, DIMENSION(3,3) :: invLe

    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ALPHA, VISC
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: YI, YP, UP, WI, WP
    REAL, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: YM, UM, WM
    REAL, ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: CM
    REAL, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::PP, PM   !reynolds mean pressure

    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: gradYP,gradUP,gradWP
    REAL, ALLOCATABLE, SAVE, DIMENSION(:,:,:,:,:) :: gradYM, gradUM    

    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: grad2YP

    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: dY2dY1

 
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: basics, basics_g, normbasics
    REAL, ALLOCATABLE, DIMENSION(:) :: onedim, onedim_g, normonedim
    REAL, ALLOCATABLE, DIMENSION(:,:) :: condonedim, condonedim_g, condbasics
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: ke_eps, ke_eps_g
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: eps_eq, eps_eq_g
    
    REAL, DIMENSION(3) :: xplot, zplot, xval, zval   !auto
    INTEGER :: ic,id,ie,iy, ixplot,izplot, root_id
    INTEGER, DIMENSION(4) :: iorder   !auto

    REAL, DIMENSION(7) :: zcoeff  !auto
    REAL :: rhosl1val

    real :: writemin, writemax
    
    integer :: imf
    integer,save :: ncond
    real,save :: condmin, condmax, dcond

! varaibles for planar averaging calls
!    real, dimension(nx,ny,nz) :: wt
!    real, dimension(nx,ny) :: fsum, wsum
!    logigal, dimension(nx,ny,nz) :: cond
    integer windowx,windowy,windowz !PDF ,windowycond

    integer icgd
   
!    window = 2
!    wt=1.0
!    cond=.true.

contains

SUBROUTINE filter_field_2pss(io,finish )
 IMPLICIT NONE 
 integer, intent(in) :: io
 logical, intent(in) :: finish

  if(.not.initialized) then
    call initialize_step(io,finish)
  endif
  
  if(.not.finish) then
    call accumulate_step(io,finish)
  else
    call finish_step(io,finish)
  endif
  

END SUBROUTINE filter_field_2pss



!**********************************************************************
!******************************second pass*****************************
!**********************************************************************

   SUBROUTINE INITIALIZE_STEP(io,finish )
   use runtime_m, only : run_title
   IMPLICIT NONE
   integer, intent(in) :: io
   logical, intent(in) :: finish

  
    writemin=0.05
    writemax=0.95
    
    ncond=1
!    condmin=0.2239
!    condmax=0.0669
    dcond=(condmax-condmin)/max(1.0,real(ncond-1))
    

!  !set cond here
!  cond=.false.
!  where(yspecies(:,:,:,4).lt.condmin+dcond*real(ic-1)  &
!  .and. yspecies(:,:,:,4).ge.condmin+dcond*real(ic)  )cond=.true.

    iselect=1
    icgd=1
    if(myid.eq.0)write(io,*)'iselect hardwired to equal :',iselect
    
    ! SPECIFY FILTERED GRID ======================================================================

! you MUST use nxf_g=nx_g or you will have BIG TROUBLE.

    nxf_g = nx*npx     ! set the filtered and unfiltered grids to the same numbers so that I can use the S3D derivatives.
    nyf_g = ny*npy     ! note that the filtered grid is uniform and there is a miss-match where there is grid stretching.
    nzf_g = nz*npz 

    if(myid.eq.0)write(io,*)'iselect=',iselect
    if(n_spec==15)then     !lo strat case
      imf=13
      if(myid.eq.0)write(io,*)'setting time scale analysis for lo-strat case:'
    elseif(n_spec==13)then   !premix case
!      imf=1  ! set it to H2 since there is no transporte mix-frac
!      imf=2  ! set it to H
!      imf=10 !set it to CO
       imf=5 ! set it to OH
      if(myid.eq.0)write(io,*)'setting time scale analysis for premix case:'
    elseif(n_spec==29)then   !hi strat case
      imf=28
      if(myid.eq.0)write(io,*)'setting time scale analysis for hi-strat case:'
    else
      write(io,*)'time scale routines not implemented for this chemistry'
      call terminate_run(io,0) 
    endif
              
    if(iselect.eq.1)then  
      tau_indx(1)=4   !O2
!      tau_indx(1)=9   !CH4
      tau_indx(2)=imf
      tau_indx(3)=1   !H2
      fac_indx(1)=1.0
if(trim(run_title).eq.'bunsen')then
      fac_indx(2)=1.0  !H2
else
      fac_indx(2)=1.0e7
endif
      fac_indx(3)=1.0
    elseif(iselect.eq.2)then
      tau_indx(1)=4
      tau_indx(2)=1
      tau_indx(3)=imf
      fac_indx(1)=1.0
      fac_indx(2)=1.0
      fac_indx(3)=1.0e7
    elseif(iselect.eq.3)then
      tau_indx(1)=imf
      tau_indx(2)=1
      tau_indx(3)=4
      fac_indx(1)=1.0e7
      fac_indx(2)=1.0
      fac_indx(3)=1.0
    endif
    
    if(myid.eq.0)write(io,*)'analysis for species ',tau_indx(1),' and ',tau_indx(2)

    delta_fil_grid_x = l_ref*(xg(nxf_g)-xg(1))/ (nxf_g-1)!
    delta_fil_grid_y = l_ref*(yg(nyf_g/2+1)-yg(nyf_g/2))
    delta_fil_grid_z = l_ref*(zg(nzf_g)-zg(1))/ (nzf_g-1)!
    
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
    allocate(inveps(nxf,nyf,nzf,3,3))
    allocate(RR_R(nx,ny,nz,n_spec))    
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

zplot(1)=0.50
zplot(2)=0.50
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

    windowx = 40
    windowy = 20
!PDF     windowycond = 20
    windowz = 20

   if(myid.eq.0)write(io,*)'***** initializing for the second pass *****'
   if(myid.eq.0)write(io,*)'tau_indx ', tau_indx
       
  COUNTER=0.0

  allocate(YF(nxf,nyf,nzf,n_spec))
  allocate(CFil(nxf,nyf,nzf))
  allocate(UF(nxf,nyf,nzf,3))
  allocate(WF(nxf,nyf,nzf,n_spec))
  allocate(PF(nxf,nyf,nzf))

!  read YF,UF,WF from the HDF files, taking care to non-dimensionalise them.
!  do ic=1,ncond+1
  call read_filtfields(io,uf(:,:,:,:),yf(:,:,:,:),wf(:,:,:,:),pf(:,:,:),cfil(:,:,:),1)
!  enddo

#ifdef LEWIS
invLe(1,1)=invSc(tau_indx(1))*Pr
invLe(2,2)=invSc(tau_indx(2))*Pr
invLe(3,3)=invSc(tau_indx(3))*Pr
#endif
#ifdef MIXAVG
invLe=1.0
if(myid.eq.0)write(io,*)'The time scale analysis is not derived for MIXAVG, unity Lewis numbers assumed.'
#endif

!  if(n_spec.eq.29)then   ! the TFL CH4 mechanism for non-premixed combustion plus mixture fraction.
!  invLe(1,1)=0.9001
!  invLe(2,2)=1.0000
!  invLe(3,3)=5.5555
!  else                    ! the 13 spec TFL CH4 mechanism for lean flames plus two mixture fractions.
!  invLe(1,1)=0.9259
!  invLe(2,2)=1.0000
!  invLe(3,3)=5.8824
!  endif

!invLe(1,2)=0.5*(invLe(1,1)+invLe(2,2))
invLe(1,2)=1.0
invLe(2,1)=invLe(1,2)
invLe(1,3)=0.5*(invLe(1,1)+invLe(3,3))
invLe(3,1)=invLe(1,3)
invLe(2,3)=0.5*(invLe(2,2)+invLe(3,3))
invLe(3,2)=invLe(2,3)     

   if(myid.eq.0)write(io,*)'invLe set'
       
!saved variable allocation
  allocate(CF(nxf,nyf,nzf))
  allocate(DFIL(nxf,nyf,nzf))
  allocate(AF(nxf,nyf,nzf))
  allocate(VF(nxf,nyf,nzf))
  allocate(RHOSL(nxf,nyf,nzf))
  allocate(PREY(nxf,nyf,nzf))
!  allocate(WFIL(nxf,nyf,nzf,2))
  allocate(UFIL(nxf,nyf,nzf,3))
  allocate(CFIL(nxf,nyf,nzf))
  allocate(YFIL(nxf,nyf,nzf,2))
  allocate(YcFIL(nxf,nyf,nzf,2))
  allocate(YYcFIL(nxf,nyf,nzf,2,2))

if(myid.eq.0)write(io,*)'yfil allocated'
  
!  allocate(YYF(nxf,nyf,nzf,2,2))
  allocate(YPYPF(nxf,nyf,nzf,2,2))  
  allocate(AdYpdYPF(nxf,nyf,nzf,2,2))
  allocate(AdYpdYPF_MOD(nxf,nyf,nzf,2))
  allocate(DISSvar(nxf,nyf,nzf,2,2,1))
  allocate(T1aVar(nxf,nyf,nzf,2,2,3))
  allocate(T1bVar(nxf,nyf,nzf,2,3,3))
  allocate(UdYVar(nxf,nyf,nzf,2,3,3))
  allocate(T2Var(nxf,nyf,nzf,2,2,1))
  allocate(T3aVar(nxf,nyf,nzf,2,2,1))
  allocate(T3bVar(nxf,nyf,nzf,2,2,1))
  allocate(T3cVar(nxf,nyf,nzf,2,3))
  allocate(T4Var(nxf,nyf,nzf,2,2,1))
  allocate(T5Var(nxf,nyf,nzf,2,2,1))

  allocate(KEDIFF(nxf,nyf,nzf,2,2))
  allocate(KETRIP(nxf,nyf,nzf,2,2))
  allocate(KET1(nxf,nyf,nzf,2,2))
  allocate(KET1b(nxf,nyf,nzf,2,2,3))
  allocate(KET2(nxf,nyf,nzf,2,3))
  allocate(KET3(nxf,nyf,nzf,2,2))
  allocate(KET4(nxf,nyf,nzf,2,2))
  allocate(KET5(nxf,nyf,nzf,2,2))

  allocate(TKCONV(nxf,nyf,nzf))
  allocate(TK1(nxf,nyf,nzf,3,3))
  allocate(TK2(nxf,nyf,nzf,3))
  allocate(TK3(nxf,nyf,nzf,3))
  allocate(TK4(nxf,nyf,nzf,3))
  allocate(TK5(nxf,nyf,nzf))
  allocate(sqTKCONV(nxf,nyf,nzf))
  allocate(sqTK1(nxf,nyf,nzf,3,3))
  allocate(sqTK2(nxf,nyf,nzf,3))
  allocate(sqTK3(nxf,nyf,nzf,3))
  allocate(sqTK4(nxf,nyf,nzf,3))
  allocate(sqTK5(nxf,nyf,nzf))

  allocate(UYflux(nxf,nyf,nzf,3,2))
  allocate(UCflux(nxf,nyf,nzf,3))

  allocate(Y1_Y2M(nxf,nyf,nzf))
  allocate(Y1_Y2sq(nxf,nyf,nzf))
  allocate(YWM(nxf,nyf,nzf))
  allocate(EPS_C(nxf,nyf,nzf))
  allocate(sqY1_Y2M(nxf,nyf,nzf))
  allocate(sqY1_Y2sq(nxf,nyf,nzf))
  allocate(sqYWM(nxf,nyf,nzf))
  allocate(sqEPS_C(nxf,nyf,nzf))
  
  allocate(gradCheck(nxf,nyf,nzf,2,2,ncond+1))
  allocate(YPYPcond(nxf,nyf,nzf,2,2,ncond+1))
  allocate(Ycond(nxf,nyf,nzf,2,ncond+1))
  allocate(Wcond(nxf,nyf,nzf,2,ncond+1))
  allocate(CosAlpha(nxf,nyf,nzf,2,ncond+1))
  allocate(FlamNormGrad(nxf,nyf,nzf,9,ncond+1))
  allocate(DFILcond(nxf,nyf,nzf,ncond+1))
  allocate(CFcond(nxf,nyf,nzf,ncond+1))
  allocate(Dpdfcond(nxf,nyf,nzf,ncond+1))
  allocate(Cpdfcond(nxf,nyf,nzf,ncond+1))

  allocate(KEMECH(nxf,nyf,nzf))
  allocate(KEMECH_tens(nxf,nyf,nzf,3,3))
  allocate(EPSMECH(nxf,nyf,nzf))
  allocate(EPSMECH_tens(nxf,nyf,nzf,3,3))
  allocate(UPVP(nxf,nyf,nzf))

! uncertainty counters

  allocate(sqCF(nxf,nyf,nzf))
  allocate(sqDFIL(nxf,nyf,nzf))
  allocate(sqAF(nxf,nyf,nzf))
  allocate(sqVF(nxf,nyf,nzf))
  allocate(sqRHOSL(nxf,nyf,nzf))
!  allocate(sqWFIL(nxf,nyf,nzf,2))
  allocate(sqUFIL(nxf,nyf,nzf,3))
  allocate(sqYFIL(nxf,nyf,nzf,2))
  allocate(sqYcFIL(nxf,nyf,nzf,2))
  allocate(sqYYcFIL(nxf,nyf,nzf,2,2))

 allocate(sqYPYPF(nxf,nyf,nzf,2,2))  
 allocate(sqAdYpdYPF(nxf,nyf,nzf,2,2))
 allocate(sqDISSvar(nxf,nyf,nzf,2,2,1))
 allocate(sqT1aVar(nxf,nyf,nzf,2,2,3))
 allocate(sqT1bVar(nxf,nyf,nzf,2,3,3))
 allocate(sqUdYVar(nxf,nyf,nzf,2,3,3))
 allocate(sqT2Var(nxf,nyf,nzf,2,2,1))
 allocate(sqT3aVar(nxf,nyf,nzf,2,2,1))
 allocate(sqT3bVar(nxf,nyf,nzf,2,2,1))
 allocate(sqT3cVar(nxf,nyf,nzf,2,3))
 allocate(sqT4Var(nxf,nyf,nzf,2,2,1))
 allocate(sqT5Var(nxf,nyf,nzf,2,2,1))

 allocate(sqKEDIFF(nxf,nyf,nzf,2,2))
 allocate(sqKET1(nxf,nyf,nzf,2,2))
 allocate(sqKET1b(nxf,nyf,nzf,2,2,3))
 allocate(sqKET2(nxf,nyf,nzf,2,3))
 allocate(sqKET3(nxf,nyf,nzf,2,2))
 allocate(sqKET4(nxf,nyf,nzf,2,2))
 allocate(sqKET5(nxf,nyf,nzf,2,2))

 allocate(sqKEMECH(nxf,nyf,nzf))
 allocate(sqKEMECH_tens(nxf,nyf,nzf,3,3))
 allocate(sqEPSMECH(nxf,nyf,nzf))
 allocate(sqEPSMECH_tens(nxf,nyf,nzf,3,3))
 allocate(sqUPVP(nxf,nyf,nzf))

!non-counter saved variables  
  allocate(gradYM(nxf,nyf,nzf,2,3))
  allocate(gradUM(nxf,nyf,nzf,3,3))
  allocate(YM(nxf,nyf,nzf,2))
  allocate(CM(nxf,nyf,nzf))
  allocate(UM(nxf,nyf,nzf,3))
  allocate(WM(nxf,nyf,nzf,2))
  allocate(PM(nxf,nyf,nzf))

!error assessment variables
  allocate(sumrhogradYI(nxf,nyf,nzf,2,3))
  allocate(sumgradYP2(nxf,nyf,nzf,2,3))
  
  YM = 0.0
  CM = 0.0
  UM = 0.0
  WM = 0.0
  PM = 0.0
  gradYM = 0.0
  gradUM = 0.0

  sumrhogradYI = 0.0
  sumgradYP2 = 0.0

 YM(:,:,:,1)=YF(:,:,:,tau_indx(1))*fac_indx(1)
! YM(:,:,:,2)=YF(:,:,:,tau_indx(2))*fac_indx(2)  !scaled to give MixFrac
if(n_spec.le.15)then
 YM(:,:,:,2)=0.0233661 + 0.0318212   * YF(:,:,:,tau_indx(2))*fac_indx(2)  !scaled to give MixFrac
else
 YM(:,:,:,2)=0.0       + 0.078578666 * YF(:,:,:,tau_indx(2))*fac_indx(2)
endif
! YM(:,:,:,3)=YF(:,:,:,tau_indx(3))*fac_indx(3)   

 CM(:,:,:)=CFil(:,:,:)

 UM(:,:,:,:)=UF(:,:,:,:)

 WM(:,:,:,1)=WF(:,:,:,tau_indx(1))*fac_indx(1)     
 WM(:,:,:,2)=WF(:,:,:,tau_indx(2))*fac_indx(2)
! WM(:,:,:,3)=WF(:,:,:,tau_indx(3))*fac_indx(3)

 PM(:,:,:)=PF(:,:,:)

 !dYM
  do i=1,2!3
  call derivative_x(nx,ny,nz, YM(:,:,:,i), gradYM(:,:,:,i,1),scale_1x,1)
  call derivative_y(nx,ny,nz, YM(:,:,:,i), gradYM(:,:,:,i,2),scale_1y,1)
  call derivative_z(nx,ny,nz, YM(:,:,:,i), gradYM(:,:,:,i,3),scale_1z,1)
  enddo

!if(gradYM(nx/2,ny/2,nz/2,1,3).eq.0.0)write(io,*)myid,'dYdz=0', gradYM(nx/2,ny/2,nz/2,1,1), gradYM(nx/2,ny/2,nz/2,1,2), gradYM(nx/2,ny/2,nz/2,1,3)&
!    ,YM(nx/2,ny/2,nz/2-1,1),YM(nx/2,ny/2,nz/2+1,1),YM(nx/2,ny/2+1,nz/2-1,1),YM(nx/2,ny/2-1,nz/2,1)

  do i=1,3
  call derivative_x(nx,ny,nz, UM(:,:,:,i), gradUM(:,:,:,i,1),scale_1x,1)
  call derivative_y(nx,ny,nz, UM(:,:,:,i), gradUM(:,:,:,i,2),scale_1y,1)
  call derivative_z(nx,ny,nz, UM(:,:,:,i), gradUM(:,:,:,i,3),scale_1z,1)
  enddo


  deallocate(YF)
  deallocate(CFil)
  deallocate(UF)
  deallocate(WF)
  deallocate(PF)
  
  CF = 0.0      ! these are the 'saved' variable counters
  DFIL = 0.0
  AF = 0.0
  VF = 0.0
  RHOSL = 0.0
  PREY = 0.0
!  WFIL = 0.0
  UFIL = 0.0
  YFIL = 0.0
  YcFIL = 0.0
  YYcFIL = 0.0
!  YYF = 0.0
  YPYPF = 0.0
  AdYpdYPF = 0.0
  AdYpdYPF_MOD = 0.0
  DISSvar = 0.0  
  T1aVar = 0.0
  T1bVar = 0.0
  UdYVar = 0.0
  T2Var  = 0.0
  T3aVar = 0.0
  T3bVar = 0.0
  T3cVar = 0.0
  T4Var  = 0.0
  T5Var  = 0.0

  gradCheck = 0.0
  YPYPcond = 0.0
  Ycond = 0.0
  Wcond = 0.0
  CosAlpha = 0.0
  FlamNormGrad = 0.0
  DFILcond = 0.0
  CFcond = 0.0
  Dpdfcond = 0.0
  Cpdfcond = 0.0

  KEDIFF = 0.0
  KETRIP = 0.0
  KET1 = 0.0
  KET1b = 0.0
  KET2 = 0.0
  KET3 = 0.0
  KET4 = 0.0
  KET5 = 0.0

  TKCONV=0.0
  TK1=0.0
  TK2=0.0
  TK3=0.0
  TK4=0.0
  TK5=0.0
  sqTKCONV=0.0
  sqTK1=0.0
  sqTK2=0.0
  sqTK3=0.0
  sqTK4=0.0
  sqTK5=0.0

  UYflux=0.0
  UCflux=0.0

  Y1_Y2M=0.0
  Y1_Y2SQ=0.0
  YWM=0.0
  EPS_C=0.0
  sqY1_Y2M=0.0
  sqY1_Y2SQ=0.0
  sqYWM=0.0
  sqEPS_C=0.0

  KEMECH = 0.0
  KEMECH_tens = 0.0
  EPSMECH = 0.0
  EPSMECH_tens = 0.0
  UPVP = 0.0

! uncertainty variables
  sqCF = 0.0      ! these are the 'saved' variable counters
  sqDFIL = 0.0
  sqAF = 0.0
  sqVF = 0.0
  sqRHOSL = 0.0
!  sqWFIL = 0.0
  sqUFIL = 0.0
  sqYFIL = 0.0
  sqYcFIL = 0.0
  sqYYcFIL = 0.0
!  sqYYF = 0.0
  sqYPYPF = 0.0
  sqAdYpdYPF = 0.0
  sqDISSvar = 0.0  
  sqT1aVar = 0.0
  sqT1bVar = 0.0
  sqUdYVar = 0.0
  sqT2Var  = 0.0
  sqT3aVar = 0.0
  sqT3bVar = 0.0
  sqT3cVar = 0.0
  sqT4Var  = 0.0
  sqT5Var  = 0.0

  sqKEDIFF = 0.0
  sqKET1 = 0.0
  sqKET1b= 0.0
  sqKET2 = 0.0
  sqKET3 = 0.0
  sqKET4 = 0.0
  sqKET5 = 0.0

  sqKEMECH = 0.0
  sqKEMECH_tens = 0.0
  sqEPSMECH = 0.0
  sqEPSMECH_tens = 0.0
  sqUPVP = 0.0

initialized=.true.

    if(myid.eq.0)write(io,*)'***** Initialization complete for second filtering pass *****'

   END SUBROUTINE INITIALIZE_STEP


end module timescales_m


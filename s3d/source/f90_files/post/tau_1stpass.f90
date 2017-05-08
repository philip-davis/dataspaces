#include "globalDefines.h"
SUBROUTINE filter_field_1pss(io,finish )

! to do:
! move some common parameters into the timescale module

  use variables_m, only : q, u, temp, pressure, yspecies, volum
!  USE HDF5
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g
!  use s3d_hdf_interface_m  !no longer used
  use reference_m
  use chemkin_m, only: reaction_rate
  use thermchem_m
  use stat_util_m, only: calc_z_mean_numden_window, calc_XYfld_Xmovsum, calc_XYfld_Ymovsum
  use runtime_m, only: tstep
  
  IMPLICIT NONE

  INTEGER io
  
  INTEGER NXF,NYF,NZF, NXF_G, NYF_G, NZF_G, ncond

  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: filteredfield, dummy
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :,:) :: DF, CF
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :,:) :: UF,YF,WF
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: CFIL   !CF = 1-Y1/Y2 (used in Darbyshire processing)
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :) :: RR_R  ! RR_R = d(rhoYi)/dt = /omega_i as required in the time scale equation.
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :) :: DIFFUSION
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: Pave  !PRESSURE
  REAL, ALLOCATABLE, SAVE, DIMENSION(:, :, :, :) :: Total  !Total, for Reynolds averages
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: FIELD, FIELDX, FIELDY, FIELDZ

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

    logical, intent(in) :: finish
    logical, save :: initialized=.false.
    REAL, SAVE, DIMENSION(3,3) :: invLe

    INTEGER :: ic,id,ie,iy

    real, dimension(3) :: xval,zval

! varaibles for planar averaging calls
    real, dimension(nx,ny,nz) :: wt
    real, dimension(nx,ny) :: fsum, wsum
    logical, dimension(nx,ny,nz) :: cond
    integer windowx, windowy, windowz
    integer iplanar
    real vsmall, condmin, condmax, dcond
    
    ! SPECIFY FILTERED GRID ======================================================================
   
    iplanar=1   !selects whether to use the generalized filter or the moving planar average
    windowx = 40
    windowy = 20
    windowz = 20
    wt=1.0
    cond=.true.
    ncond=1
    vsmall = 1.0e-20

    xval=-1.0
    zval=-1.0

! you MUST use nxf_g=nx_g or you will have BIG TROUBLE.

    nxf_g = nx*npx     ! set the filtered and unfiltered grids to the same numbers so that I can use the S3D derivatives.
    nyf_g = ny*npy     ! note that the filtered grid is uniform and there is a miss-match where there is grid stretching.
    nzf_g = nz*npz 

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
    allocate(dummy(nxf,nyf,nzf))
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
!
    deallocate(fxg)
    deallocate(fyg)
    deallocate(fzg)


    ! FILTER FIELD ===========================================================================
    method = 1 !1 For top hat, 2 for gaussian
    delta = delta_fil_grid_x*9.0
    if(myid.eq.0)write(io,*)'filter full width = ',delta
! to include 27 points in the box filter make delta =(2+1)*the dns grid spacing.
! to include 125 points in the box filter make delta=(4+1)*the dns grid spacing.
    !delta = 0.3124e-3 ! in m, will be normalized by lref inside generalized_filter
    influence = 2.0*delta ! use to exclude calculation of points far from filtered grid point

  !***************************************************************************
  !******************************* first pass ********************************
  !***************************************************************************

    if(.not.initialized) then
        
    allocate(DF(nxf,nyf,nzf,0:ncond))
    allocate(UF(nxf,nyf,nzf,3,0:ncond))
    allocate(YF(nxf,nyf,nzf,n_spec,0:ncond))
    allocate(CFIL(nxf,nyf,nzf,0:ncond))
    allocate(WF(nxf,nyf,nzf,n_spec,0:ncond))
    allocate(Pave(nxf,nyf,nzf,0:ncond))
    allocate(Total(nxf,nyf,nzf,0:ncond))
    DF = 0.0
    UF = 0.0
    YF = 0.0
    CFIL = 0.0
    WF = 0.0
    Pave = 0.0
    Total = 0.0

    initialized=.true.

    endif


  if(.not.finish)then

  if(myid.eq.0)write(io,*)'***** Evaluating filtered species, velocities and reaction rates *****'
              

! First time through we get the filtered U,Y,W fields and write them to an s3d output file ...
! also write the filtered fields to hdf for plotting.

! (once this is working you can easily convert it to filter all variables 
! at once, that may save some time, maybe not. just make field(:,:,:) multi-dimensional)
 
    ! Calculate the quantities to be filtered on the first pass:
    ! U(k), Y(i), W(i)

  condmin=0.2239
  condmax=0.0669
  dcond=(condmax-condmin)/max(1.0,real(ncond-1))
  if(myid.eq.0)write(io,*)'condmin,condmax,dcond',condmin,condmax,dcond

!  do ic=0,ncond
   do ic=1,ncond
!if(myid.eq.50)write(io,*)ic,'limit lower=',condmin+dcond*real(ic-1), yspecies(5,5,5,4)
!if(myid.eq.50)write(io,*)ic,'limit upper=',condmin+dcond*real(ic), yspecies(5,5,5,4)
!
!
!  !set cond here
!  where(yspecies(:,:,:,4).le.condmin+dcond*real(ic-1)  &
!  .and. yspecies(:,:,:,4).gt.condmin+dcond*real(ic)  )
!    cond=.true.
!  elsewhere
!    cond=.false.
!  endwhere
!
!  if(ic.eq.0)cond=.true.

if(myid.eq.0)write(io,*)' UNCONDITIONAL AVERAGING'
cond=.true.

!conditional  condmin=0.35D-7
!conditional  condmax=0.45D-7
!conditional  !condmin=0.45D-7
!conditional  !condmax=0.55D-7
!conditional  !condmin=0.55D-7
!conditional  !condmax=0.65D-7
!conditional  if(myid.eq.0)write(io,*)' CONDITIONAL AVERAGIN BETWEEN:',condmin*1.0D7,' and ',condmax*1.0D7
!conditional      where(yspecies(:,:,:,n_spec-2).gt.condmin   &
!conditional      .and. yspecies(:,:,:,n_spec-2).le.condmax)
!conditional        cond=.true.
!conditional     elsewhere
!conditional        cond=.false.
!conditional     endwhere


!DENS
! RHO
 field(:,:,:) = q(:,:,:,4,1)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, DF(:,:,:,ic), dummy,io)
!PRESS
 field(:,:,:) = pressure(:,:,:)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, Pave(:,:,:,ic), dummy,io)
!Total
 field(:,:,:) = 1.0
  call accum_planar(field, cond, wt, windowx, windowy, windowz, Total(:,:,:,ic), dummy,io)
  if(myid.eq.0)write(io,*)'after total accumulation'

!rhoV
   do i=1,3
  field(:,:,:) = q(:,:,:,i,1)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, UF(:,:,:,i,ic), dummy,io)
   enddo
!rhoY
  do i=1,n_spec-1
  field(:,:,:) = yspecies(:,:,:,i)*q(:,:,:,4,1)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, YF(:,:,:,i,ic), dummy,io)
  enddo
!rhoC
  field(:,:,:) = yspecies(:,:,:,9)*q(:,:,:,4,1)/&
     (0.0233661+1.0d7*yspecies(:,:,:,13)*0.0318212)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, CFIL(:,:,:,ic), dummy,io)
!rhoW
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif  
  do i=1,n_spec-1
  field(:,:,:) = rr_r(:,:,:,i)*q(:,:,:,4,1)
  call accum_planar(field, cond, wt, windowx, windowy, windowz, WF(:,:,:,i,ic), dummy,io)
  enddo

enddo !icond

!filt else
!filt 
!filt     CALL generalized_filter(io, q(:,:,:,4,1), filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)       
!filt     DF(:,:,:)=DF(:,:,:)+filteredfield
!filt     do i=1,3 !loop over directions
!filt     CALL generalized_filter(io, q(:,:,:,i,1), filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)
!filt     UF(:,:,:,i)=UF(:,:,:,i)+filteredfield
!filt     enddo
!filt     do i=1,n_spec-1
!filt     CALL generalized_filter(io, yspecies(:,:,:,i)*q(:,:,:,4,1), filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)
!filt     YF(:,:,:,i)=YF(:,:,:,i)+filteredfield
!filt     enddo
!filt ! find the reaction rate
!filt     call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
!filt     do i=1,n_spec
!filt     CALL generalized_filter(io, q(:,:,:,4,1)*rr_r(:,:,:,i), filteredfield, nxf, nyf, nzf, fx, fy, fz, method, delta, influence,xval,zval)
!filt     WF(:,:,:,i)=WF(:,:,:,i)+filteredfield
!filt     enddo
!filt 
!filt endif

 else   !finish.

    !COMPUTE THE AVERAGES:
!    do ic=0,ncond
    do ic=1,ncond
    do i=1,3
    UF(:,:,:,i,ic)=UF(:,:,:,i,ic)/max(DF(:,:,:,ic),vsmall)
    enddo
    do i=1,n_spec
    YF(:,:,:,i,ic)=YF(:,:,:,i,ic)/max(DF(:,:,:,ic),vsmall)
    CFIL(:,:,:,ic)=1.0-CFIL(:,:,:,ic)/max(DF(:,:,:,ic),vsmall)
    WF(:,:,:,i,ic)=WF(:,:,:,i,ic)/max(DF(:,:,:,ic),vsmall)
    enddo
    Pave(:,:,:,ic)=Pave(:,:,:,ic)/max(Total(:,:,:,ic),vsmall)
    enddo
 
   if(myid.eq.0)write(io,*)'***** Writing filtered species, velocities and reaction rates *****'
 

    ! WRITE OUTPUT FIELD TO HDF FILE
!    do i=1,3
!    write(sid_ext,'(I2.2)') i
!    filteredfield = UF(:,:,:,i) * a_ref
!    filename = '../post/UF'//trim(sid_ext)//'.h5'
!    call write_field(io, filteredfield, nxf, nyf, nzf, FX, FY, FZ, filename)
!    enddo
!    do i=1,n_spec
!    write(sid_ext,'(I2.2)') i
!    filteredfield = YF(:,:,:,i)
!    filename = '../post/YF'//trim(sid_ext)//'.h5'
!    call write_field(io, filteredfield, nxf, nyf, nzf, FX, FY, FZ, filename)
!    enddo
!    do i=1,n_spec
!     write(sid_ext,'(I2.2)') i
!    filteredfield = WF(:,:,:,i) * rr_ref
!    filename = '../post/WF'//trim(sid_ext)//'.h5'
!    call write_field(io, filteredfield, nxf, nyf, nzf, FX, FY, FZ, filename)
!    enddo


!do ic=0,ncond
do ic=1,ncond
if(myid.eq.50)write(io,*) 'yf before writing=',yf(5,5,5,4,ic)!*a_ref
!1pss !write UF,YF,WF to file
call write_filtfields(io,uf(:,:,:,:,ic),yf(:,:,:,:,ic),wf(:,:,:,:,ic),pave(:,:,:,ic),cfil(:,:,:,ic),ic)

!just for testing
yf(:,:,:,:,ic)=0.0
call read_filtfields(io,uf(:,:,:,:,ic),yf(:,:,:,:,ic),wf(:,:,:,:,ic),pave(:,:,:,ic),cfil(:,:,:,ic),ic)
if(myid.eq.50)write(io,*) 'yf',ic,yf(5,5,5,4,ic) !*a_ref
enddo  !icond
 
    deallocate(DF)
    deallocate(UF)
    deallocate(YF)
    deallocate(CFIL)
    deallocate(WF)
    deallocate(Pave)
    deallocate(Total)

    endif  !finish

    deallocate(RR_R)
    deallocate(DIFFUSION)
    deallocate(fx)
    deallocate(fy)
    deallocate(fz)
    deallocate(field)
    deallocate(fieldx)
    deallocate(fieldy)
    deallocate(fieldz)
    deallocate(dummy)
    deallocate(filteredfield)

 
  END SUBROUTINE FILTER_FIELD_1pss


#include "globalDefines.h"
!=========================================================================================
  module zclookup_m
!=========================================================================================
! module for table lookup for species and temperature in progress variable space
! assumes simple table with equally spaced increments
! Evatt Hawkes July 2003
!
! Ed Richardson February 2008
! modified for partially premixed combustion. table is in terms of progress variable
! and mixture fraction
! Ed Richardson March 2008
! modified to return the progress variable field as function of species array and 
! look up table. Please take care - definition of progress variable and mixture fraction
! can be case specific.

  implicit none
!-----------------------------------------------------------------------------------------
! variable declarations

  integer  nc_tab_pts                        !number of points in the c direction of table
  integer  nz_tab_pts                        !number of points in the z direction of table

  integer  imethod                           !calculation of progress variable in calcualtion
  parameter(imethod=2)                       !and post processing, (excludes initial and 
                                             !Boundary conditions). 
                                             !1=zclook table, 
                                             !2=Burke-Schumann,  Currently assumes lean methane stratified conditions (0.4<phi<1.0)
                                             !3=limits table.

  real Yu0,Yu1,Yb0,Yb1,Yust,Ybst,zstoich
!  parameter(Yu0 = 0.22756,Yu1 = 0.22016,Yb0 = 0.13410,Yb1 = 0.0)

  real :: cmin,cmax,delc                     !max, min, interval for c
  real :: zetamin,zetamax,delz                     !max, min, interval for z

  real, allocatable :: yspec_tab(:,:,:)        !tabulated species mass fraction
  real, allocatable :: temp_tab(:,:)           !tabulated temperature
  real, allocatable :: c_tab(:)                !tabulated c-values
  real, allocatable :: z_tab(:)                !tabulated z-values

  logical :: initialized_zclookup = .false.   !initialization flag
 

! define some new arrays which are kept initialized during calculation in order to return
! progress variable as function of Yspecies and /xi. Precompute required derivatives
! into into look up table.

  real, allocatable :: ylimit_tab(:,:)       !the max and min values of the species 
                                              !used for progress variable. (eg O2) as 
                                              !function of mix frac.
  integer :: n_spec_prog, n_spec_mf              !index of species used for prog var and for mix frac
!  real, public, allocatable, dimension(:,:,:) :: PartProg  
  real, allocatable, dimension(:,:,:) :: PartProg
  real, allocatable, dimension(:,:,:) :: ZetaField
  real, allocatable, dimension(:,:,:,:) :: Yderiv_field

  real, allocatable, dimension(:,:,:) :: Yderiv_tab
  real, allocatable, dimension(:) :: z_post
  real, allocatable, dimension(:) :: c_post

  logical :: initialized_zcpost = .false.     !initialization flag

  
!-----------------------------------------------------------------------------------------
  contains

!=========================================================================================
subroutine initialize_zclookup(io,flag)
!=========================================================================================
!allocates the necessary arrays for the lookup
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : n_spec
  use reference_m, only : t_ref

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed in
  integer, intent(in) :: io      !io unit
  integer, intent(in) :: flag    !operation flag: =+1 initialise =-1 clean up
!-----------------------------------------------------------------------------------------
! local declarations

  character*100 :: filename          !input filename
  integer :: io_cl=14                !io unit for input file 
  integer :: K,L,M                     !counters

  logical :: exist
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if((flag==+1).and.(.not.initialized_zclookup))then

    if(myid==0)then
      write(io,*) 'Initialising zc-table lookup ...'
      write(io,*)
    endif

!-----------------------------------------------------------------------------------------
!   set some parameters depending on the chemistry - this is hard coded for now.
!   be careful!!!!
!-----------------------------------------------------------------------------------------
!  parameter(Yu0 = 0.22756,Yu1 = 0.22016,Yb0 = 0.13410,Yb1 = 0.0)
if(n_spec.ne.29)then
Yu0 = 0.22756
Yu1 = 0.22016
Yb0 = 0.13410
Yb1 = 0.0
Yust = 0.22016
Ybst = 0.0
zstoich=1.0
else  ! the partially premixed stratified case:
Yu0 = 0.233
Yu1 = 2.14698495E-01 
Yb0 = 0.233
Yb1 = 0.0
Yust = 0.220147
Ybst = 0.0
zstoich=0.7023

endif



!-----------------------------------------------------------------------------------------
!   read the control file
!-----------------------------------------------------------------------------------------
    filename='../input/zclookup.in'
    if(myid.eq.0)then
      inquire(file=trim(filename),exist=exist)
    endif

    call MPI_Bcast(exist , 1, MPI_LOGICAL, 0, gcomm, ierr)

    if (.not.exist) then ! zclookup.in does not exist, let's look for clookup.in
      filename='../input/clookup.in'
    endif

    call inquire_about_input_file(filename,io)
    
!-----------------------------------------------------------------------------------------
!   myid==0 only opens and reads file
    if(myid.eq.0) then
!-----------------------------------------------------------------------------------------
!     open file

      open(unit=io_cl,file=trim(filename),status='old')
!-----------------------------------------------------------------------------------------
!     read header
      read(io_cl,*)
      read(io_cl,*)
      read(io_cl,*)

!     read number of points on table
      if(exist)then
      read(io_cl,*) nc_tab_pts,nz_tab_pts
      else  ! in this case it is premixed, our lookup file is clookup.in and we set nz_tab_pts=1
      read(io_cl,*) nc_tab_pts
      nz_tab_pts = 1
      endif

!     read max, min
      if(exist)then  ! the zclookup convention is to write the minimum then maximum.
      read(io_cl,*) cmin, cmax
      else  ! the clookup convention is to write the maximum value first, then the min.
      read(io_cl,*) cmax, cmin
      endif
!     allocate space for the table  
      allocate(temp_tab(nc_tab_pts,nz_tab_pts)); temp_tab=0.0;
      allocate(yspec_tab(n_spec,nc_tab_pts,nz_tab_pts)); yspec_tab=0.0;

!     read each line:
      do K=1,nz_tab_pts
        do L=1,nc_tab_pts
          read(io_cl,*)  temp_tab(L,K), (yspec_tab(M,L,K), M=1,n_spec)
        enddo
      enddo

!     close file
      close(io_cl)

      zetamax = 1.0
      zetamin = 0.0

    endif 

    

!   broadcast
    call MPI_Bcast(nc_tab_pts,1,MPI_INTEGER,0,gcomm,ierr)
    call MPI_Bcast(nz_tab_pts,1,MPI_INTEGER,0,gcomm,ierr)


!   allocate for other processes
    if(myid.ne.0)then
      allocate(temp_tab(nc_tab_pts,nz_tab_pts)); temp_tab=0.0;
      allocate(yspec_tab(n_spec,nc_tab_pts,nz_tab_pts)); yspec_tab=0.0;
    endif

!   broadcasts
    call MPI_Bcast(cmax   ,1             ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(cmin   ,1             ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(zetamin   ,1             ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(zetamax   ,1             ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(temp_tab ,nz_tab_pts*nc_tab_pts       ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(yspec_tab,nz_tab_pts*nc_tab_pts*n_spec,MPI_REAL8,0,gcomm,ierr)

!   nondimensionalise temperature
    temp_tab=temp_tab/t_ref

!   set interval in c-space
    delc=(cmax-cmin)/real(nc_tab_pts - 1)

!   allocate c entries
    allocate(c_tab(nc_tab_pts));c_tab=0.0;

!   set c entries:
    c_tab(1)=cmin
    do L=2,nc_tab_pts
      c_tab(L)=cmin+real(L)*delc
    enddo

!   set interval in z-space
    delz=(zetamax-zetamin)/real(max(1,nz_tab_pts - 1))

!   allocate z entries
    allocate (z_tab(nz_tab_pts));z_tab=0.0;

! set z entries:
    z_tab(1)=zetamin
    if(nz_tab_pts.gt.0)then
    do L=2,nz_tab_pts
      z_tab(L)=zetamin+real(L)*delz
    enddo
    endif

!   set initialisation flag
    initialized_zclookup=.true.

  elseif((flag==-1).and.(initialized_zclookup))then
    if(myid==0)then
      write(io,*) 'Shutting down zc-table lookup ...'
      write(io,*)
    endif

!   deallocate
    deallocate(c_tab)
    deallocate(z_tab)
    deallocate(temp_tab)
    deallocate(yspec_tab)

!   update initialisation flag
    initialized_zclookup = .false.

  else
    if(myid==0)then
      write(io,*) 'Error calling initialize_zclookup, zclookup_m'
      write(io,*) 'Expected flag =+1 for initialisation, -1 for clean-up'
      write(io,*) 'Flag= ', flag
      write(io,*) 'ABORTING!!!'
    endif
    call terminate_run(io,0)
  endif

!-----------------------------------------------------------------------------------------
  return
end subroutine initialize_zclookup
!-----------------------------------------------------------------------------------------



!=========================================================================================
subroutine initialize_zcpost(io,flag)
!=========================================================================================
!allocates the arrays to evaluate progress variable from the solution and some other 
!derivatives
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx,ny,nz,n_spec

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed in
  integer, intent(in) :: io      !io unit
  integer, intent(in) :: flag    !operation flag: =+1 initialise =-1 clean up
!-----------------------------------------------------------------------------------------
! local declarations

  integer :: K,L,M                 !counters
  real :: invdZ, invdC, invd2Z, invdZdC
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if((flag==+1).and.(.not.initialized_zcpost))then

    if(myid==0)then
      write(io,*) 'Initialising zc-post table ...'
      write(io,*)
    endif

    if(.not.initialized_zclookup) call initialize_zclookup(io,+1)


!   allocate
      allocate(ylimit_tab(3,nz_tab_pts)); ylimit_tab=0.0;
      allocate( PartProg(nx,ny,nz) );   PartProg=0.0;            
      allocate( Yderiv_field(nx,ny,nz,4)) ; Yderiv_field=0.0;
      allocate( ZetaField(nx,ny,nz)); ZetaField=0.0;

      allocate( Yderiv_tab(nc_tab_pts,nz_tab_pts,4)); Yderiv_tab=0.0;
      allocate( c_post(nc_tab_pts)); c_post=0.0;
      allocate( z_post(nz_tab_pts)); z_post=0.0;

    if(myid==0)then

    c_post=c_tab
    z_post=z_tab
!   set ylimit_tab
    if(n_spec==29)then   !TFL non-premixed methane with NOx chemistry
      n_spec_prog = 4  
      n_spec_mf = 28    
    elseif(n_spec==15)then  !TFL lean methane, 13 species and two passives
      n_spec_prog = 4
      n_spec_mf = 13
    elseif(n_spec==13)then  !TFL lean methane, 13 species no passives
      n_spec_prog = 4
      n_spec_mf = 13
    elseif(n_spec==6)then   !CERFACS 6-species mechanism
      n_spec_prog = 2 
      n_spec_mf = 6
    else
      write(io,*)'terminating in zcpost, un-known mechanism'
      call terminate_run(io,0)
    endif

    if(myid.eq.0)  write(io,*)'WARNING, the progress variable is given by species: ',n_spec_prog
    if(myid.eq.0)  write(io,*)'this is hard wired in zclookup_m'

    do K=1,NZ_tab_pts

      ylimit_tab(1,K)=  yspec_tab(n_spec_prog,1,K)          !min

      ylimit_tab(2,K)=  yspec_tab(n_spec_prog,nc_tab_pts,K)  !max

      ylimit_tab(3,K)=  z_tab(K)        !mf value

    enddo

    invdC=0.5/(c_tab(2)-c_tab(1))

    if(nz_tab_pts.gt.1)then
    invdZ=0.5/(z_tab(2)-z_tab(1))
    invd2Z=1.0/(z_tab(2)-z_tab(1))/(z_tab(2)-z_tab(1))
    invdZdC=invdZ*invdC
    else
    invdZ=1.0
    invd2Z=1.0
    invdZdC=1.0
    endif

!dY/dZ
    if(nz_tab_pts.gt.1)then
    if(nz_tab_pts.gt.2)then
    do L=2,nz_tab_pts-1
      do K=1,nc_tab_pts

        Yderiv_tab(K,L,1)=(Yspec_tab(n_spec_prog,K,L+1)-Yspec_tab(n_spec_prog,K,L-1))*invdZ

      enddo
    enddo
    endif

    do K=1,nc_tab_pts
      Yderiv_tab(K,1,1)=(Yspec_tab(n_spec_prog,K,2)-Yspec_tab(n_spec_prog,K,1))*2.0*invdZ
      Yderiv_tab(K,nz_tab_pts,1)=(Yspec_tab(n_spec_prog,K,nz_tab_pts)-Yspec_tab(n_spec_prog,K,nz_tab_pts-1))*2.0*invdZ
    enddo
    else
      Yderiv_tab(:,:,1)=0.0
    endif

!dY/dC
    do L=1,nz_tab_pts
      do K=2,nc_tab_pts-1

        Yderiv_tab(K,L,2)=(Yspec_tab(n_spec_prog,K+1,L)-Yspec_tab(n_spec_prog,K-1,L))*invdC

      enddo

      Yderiv_tab(1,L,2)=(Yspec_tab(n_spec_prog,2,L)-Yspec_tab(n_spec_prog,1,L))*2.0*invdC
      Yderiv_tab(nc_tab_pts,L,2)=(Yspec_tab(n_spec_prog,nc_tab_pts,L)-Yspec_tab(n_spec_prog,nc_tab_pts-1,L))*2.0*invdC

    enddo
    
!d2Y/dZ2
    if(nz_tab_pts.gt.2)then
    do L=2,nz_tab_pts-1
      do K=1,nc_tab_pts
        Yderiv_tab(K,L,3)=(Yspec_tab(n_spec_prog,K,L-1) &
                      -2.0*Yspec_tab(n_spec_prog,K,L)   &
                      +    Yspec_tab(n_spec_prog,K,L+1))*invd2Z
      enddo
    enddo
    else
    Yderiv_tab(:,:,3)=0.0
    endif

    do K=1,nc_tab_pts
      Yderiv_tab(K,1,3)=0.0
      Yderiv_tab(K,nz_tab_pts,3)=0.0
    enddo


!d2Y/dZdC    
    if(nz_tab_pts.gt.1)then
    do L=2,nz_tab_pts-1
      do K=2,nc_tab_pts-1
        Yderiv_tab(K,L,4)=(Yspec_tab(n_spec_prog,K+1,L+1) &
                        +  Yspec_tab(n_spec_prog,K-1,L-1) &
                        -  Yspec_tab(n_spec_prog,K+1,L-1) &
                        -  Yspec_tab(n_spec_prog,K-1,L+1))*invdZdC
      enddo
    enddo
    else
       Yderiv_tab(:,:,4)=0.0
    endif
    

    do L=1,nz_tab_pts
      Yderiv_tab(1,L,4)=0.0
      Yderiv_tab(nc_tab_pts,L,4)=0.0
    enddo
    do K=1,nc_tab_pts
      Yderiv_tab(K,1,4)=0.0
      Yderiv_tab(K,nz_tab_pts,4)=0.0
    enddo

    endif   !myid==0

!   broadcasts

    call MPI_Bcast(n_spec_prog   ,1             ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(n_spec_mf     ,1             ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(ylimit_tab    ,3*nz_tab_pts  ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(c_post        ,nc_tab_pts    ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(z_post        ,nz_tab_pts    ,MPI_REAL8,0,gcomm,ierr)

    call MPI_Bcast(Yderiv_tab,4*nc_tab_pts*nz_tab_pts, MPI_REAL8,0,gcomm,ierr)

!   set initialisation flag
    initialized_zcpost=.true.

!   deallocate initialized_zclookup
    call initialize_zclookup(io,-1)

  elseif((flag==+1).and.(initialized_zcpost))then

! do nothing

  elseif((flag==-1).and.(initialized_zcpost))then
    if(myid==0)then
      write(io,*) 'Shutting down zc-post table ...'
      write(io,*)
    endif

!   deallocate
    deallocate(ylimit_tab)
    deallocate(PartProg)
    deallocate(Yderiv_field)
    deallocate(ZetaField)
    deallocate(Yderiv_tab)
    deallocate(c_post)
    deallocate(z_post)

!   update initialisation flag
    initialized_zcpost = .false.

  else
    if(myid==0)then
      write(io,*) 'Error calling initialize_zcpost, zclookup_m'
      write(io,*) 'Expected flag =+1 for initialisation, -1 for clean-up'
      write(io,*) 'Flag= ', flag
      write(io,*) 'ABORTING!!!'
    endif
    call terminate_run(io,0)
  endif

!-----------------------------------------------------------------------------------------
  return
end subroutine initialize_zcpost
!-----------------------------------------------------------------------------------------
!=========================================================================================
subroutine SpecToProg(ysp,io)
!=========================================================================================
! calculates progress variable for given composition in s3d
!-----------------------------------------------------------------------------------------
  use param_m, only : nx,ny,nz,n_spec
  use topology_m
    
  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  real, dimension(nx,ny,nz,n_spec), intent(in) :: ysp     !species mass fraction  
  integer, intent(in)  :: io         !io unit
!-----------------------------------------------------------------------------------------
! local declarations

  integer i,j,k                ! counters 
  real Ymin,Ymax,Zeta,zfac
  integer iz_tab,invdelz

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_zcpost) call initialize_zcpost(io,+1)

! Progress variable defined as:
!    PartProg = (Y - Ymin(Z)) / (Ymax(Z)-Ymin(Z))

    if(nz_tab_pts.gt.1)then

    invdelz=1.0/(max(delz,1.0D-10))

    if(imethod.eq.1)then
    do k=1,nz
      do j=1,ny
        do i=1,nx     !this loop is in every tracer time step - can I improve it?

!! Find the mixture fraction
          if(n_spec_mf.lt.n_spec)then
          zeta=ysp(i,j,k,n_spec_mf)/1.0e-7   ! this is problem specific.
          ZetaField(i,j,k)=zeta
          else
          zeta = 0.0D0
          Zetafield(i,j,k)=zeta
          endif

!! Find the Ymin(Z) and Ymax(Z).
          
          iz_tab=floor((zeta-z_post(1))*invdelz)+1
          iz_tab=max(min(iz_tab,nz_tab_pts-1),1)
          zfac=(zeta-Ylimit_tab(3,iz_tab))*invdelz

          Ymin=   Ylimit_tab(1,iz_tab)*(1.0-zfac)  &
                + Ylimit_tab(1,iz_tab+1)*zfac
          Ymax=   Ylimit_tab(2,iz_tab)*(1.0-zfac)  &
                + Ylimit_tab(2,iz_tab+1)*zfac

!          if(Ymin.lt.Ymax)then
!          PartProg(i,j,k)=(ysp(i,j,k,n_spec_prog)-Ymin)/   &
!              max((Ymax-Ymin),1.0d-10)
!          elseif(Ymin.gt.Ymax)then
          PartProg(i,j,k)=(ysp(i,j,k,n_spec_prog)-Ymin)/   &
               min((Ymax-Ymin),-1.0d-10)
!          else
!            PartProg(i,j,k)=0.0
!          endif

! the max statement in the above is there for places close to pure oxidiser 
! where Ymax-Ymin tends to zero.

        enddo
      enddo
    enddo

    elseif(imethod.eq.2)then  !Burke-Schumann 

    do k=1,nz                                                                        
      do j=1,ny
        do i=1,nx 

!! Find the mixture fraction
          zeta=ysp(i,j,k,n_spec_mf)/1.0e-7   ! this is problem specific.
          ZetaField(i,j,k)=zeta

!! Find the Ymin(Z) and Ymax(Z).

!          Ymin=   Yu0+(Yu1-Yu0)*zeta
!          Ymax=   Yb0+(Yb1-Yb0)*zeta

          Ymin=Yust
          Ymax=Ybst

          if(zeta.lt.zstoich)then

          if (zstoich.gt.0.0)then
          Ymin=Yu0+(Yust-Yu0)*zeta/zstoich
          Ymax=Yb0+(Ybst-Yb0)*zeta/zstoich
          else
          Ymin=Yu0
          Ymax=Yb0
          endif

          elseif(zeta.gt.zstoich)then

          if (zstoich.lt.1.0)then
          Ymin=Yu1+(Yust-Yu1)*(1.0-zeta)/(1.0-zstoich)
          Ymax=Yb1+(Ybst-Yb1)*(1.0-zeta)/(1.0-zstoich)
          else
          Ymin=Yu1
          Ymax=Yb1
          endif

          endif


          PartProg(i,j,k)=(ysp(i,j,k,n_spec_prog)-Ymin)/   &
               min((Ymax-Ymin),-1.0d-10)

        enddo
      enddo
    enddo

    elseif(imethod.eq.3)then
    if(myid.eq.0)write(io,*)'limit table method to find Cprog is not implemented'
    stop

    endif
    
    else !nz_tab_pts.eq.1
        
    do k=1,nz
      do j=1,ny
        do i=1,nx     !this loop is in every tracer time step - can I improve it?

!! Find the mixture fraction
          if(n_spec_mf.lt.n_spec)then
          zeta=ysp(i,j,k,n_spec_mf)/1.0e-7   ! this is problem specific.
          ZetaField(i,j,k)=zeta
          else
          zeta = 0.0D0
          Zetafield(i,j,k)=zeta
          endif

!! Find the Ymin(Z) and Ymax(Z).
          
          iz_tab=1
          zfac=(zeta-Ylimit_tab(3,iz_tab))*invdelz

          Ymin=   Ylimit_tab(1,iz_tab)
          Ymax=   Ylimit_tab(2,iz_tab)

          PartProg(i,j,k)=(ysp(i,j,k,n_spec_prog)-Ymin)/   &
               min((Ymax-Ymin),-1.0d-10)

! the max statement in the above is there for places close to pure oxidiser 
! where Ymax-Ymin tends to zero.

        enddo
      enddo
    enddo

    endif  !nz_tab_pts = 1


    
!-----------------------------------------------------------------------------------------
  return
  end subroutine SpecToProg

!=========================================================================================
subroutine zcderiv_field(io)
!=========================================================================================
! does the lookup for a field of c-values
! uses simple linear interpolation
! assumes constant intervals
!-----------------------------------------------------------------------------------------
  use param_m, only : nx,ny,nz,n_spec
  use topology_m, only : myid

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  integer, intent(in) :: io !io unit
!  real, dimension(nx,ny,nz),        intent(in) :: c        !lookup parameter
!  real, dimension(nx,ny,nz),        intent(in) :: zeta     !lookup parameter

!  real, dimension(nx,ny,nz,n_spec), intent(out) :: ysp     !species mass fraction
!  real, dimension(nx,ny,nz),        intent(out) :: t       !temperature
!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k                   !grid counters
  integer :: ic_tab,iz_tab           !lookup entry
  real :: cfac,zfac                  !interpolating factor
  real :: invdelc,invdelz            !inverse c-interval
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_zcpost)then
!     call initialize_zcpost(io,+1)
    if(myid.eq.0)write(io,*)'You need to initialize zcpost AND call SpecToProg before'
    if(myid.eq.0)write(io,*)' zderiv_field.... ABORTING'
    call terminate_run(io,0)
  endif


  invdelc=1.0/(max(delc,1.0D-10))
  invdelz=1.0/(max(delz,1.0D-10))

if(nz_tab_pts.gt.1)then
  if(imethod.eq.1)then

  do k=1,nz
    do j=1,ny
      do i=1,nx

        ic_tab=floor((PartProg(i,j,k)-c_post(1))*invdelc)+1
        ic_tab=max(min(ic_tab,nc_tab_pts-1),1)
        cfac=(PartProg(i,j,k)-c_post(ic_tab))*invdelc

        iz_tab=floor((zetafield(i,j,k)-z_post(1))*invdelz)+1
        iz_tab=max(min(iz_tab,nz_tab_pts-1),1)
        zfac=(zetafield(i,j,k)-z_post(iz_tab))*invdelz

        yderiv_field(i,j,k,1)=   &
         +Yderiv_tab(ic_tab,iz_tab,1)*(1.0-cfac)*(1.0-zfac)   &
         +Yderiv_tab(ic_tab,iz_tab+1,1)*(1.0-cfac)*zfac   &
         +Yderiv_tab(ic_tab+1,iz_tab,1)*cfac*(1.0-zfac)   &
         +Yderiv_tab(ic_tab+1,iz_tab+1,1)*cfac*zfac

        yderiv_field(i,j,k,2)=   &
         +Yderiv_tab(ic_tab,iz_tab,2)*(1.0-cfac)*(1.0-zfac)   &
         +Yderiv_tab(ic_tab,iz_tab+1,2)*(1.0-cfac)*zfac   &
         +Yderiv_tab(ic_tab+1,iz_tab,2)*cfac*(1.0-zfac)   &
         +Yderiv_tab(ic_tab+1,iz_tab+1,2)*cfac*zfac

        yderiv_field(i,j,k,3)=   &
         +Yderiv_tab(ic_tab,iz_tab,3)*(1.0-cfac)*(1.0-zfac)   &
         +Yderiv_tab(ic_tab,iz_tab+1,3)*(1.0-cfac)*zfac   &
         +Yderiv_tab(ic_tab+1,iz_tab,3)*cfac*(1.0-zfac)   &
         +Yderiv_tab(ic_tab+1,iz_tab+1,3)*cfac*zfac
                                            

        yderiv_field(i,j,k,4)=   &
         +Yderiv_tab(ic_tab,iz_tab,4)*(1.0-cfac)*(1.0-zfac)   &
         +Yderiv_tab(ic_tab,iz_tab+1,4)*(1.0-cfac)*zfac   &
         +Yderiv_tab(ic_tab+1,iz_tab,4)*cfac*(1.0-zfac)   &
         +Yderiv_tab(ic_tab+1,iz_tab+1,4)*cfac*zfac

      enddo
    enddo
  enddo

  elseif(imethod.eq.2)then  !B-S limit
  do k=1,nz
    do j=1,ny
      do i=1,nx
        yderiv_field(i,j,k,1)=     &  !dYdZ
          PartProg(i,j,k)*(Yb1-Yb0)+(1.0-PartProg(i,j,k))*(Yu1-Yu0)
        yderiv_field(i,j,k,2)=     &  !dYdC
          Yb0-Yu0+zetafield(i,j,k)*((Yb1-Yb0)-(Yu1-Yu0)) 
        yderiv_field(i,j,k,3)=0.0     !d2YdZ2
        yderiv_field(i,j,k,4)=     &  !d2YdZdC
          (Yb1-Yb0)-(Yu1-Yu0) 
      enddo
    enddo
  enddo

  elseif(imethod.eq.3)then  !Limits table
    if(myid.eq.0)write(io,*)'imethod=',imethod,' not implemented'
    stop
  else  !error trapping
    if(myid.eq.0)write(io,*)'imethod=',imethod,' not implemented'
    stop
  endif
  
else !nz_tab_pts=1
    
  do k=1,nz
    do j=1,ny
      do i=1,nx

        ic_tab=floor((PartProg(i,j,k)-c_post(1))*invdelc)+1
        ic_tab=max(min(ic_tab,nc_tab_pts-1),1)
        cfac=(PartProg(i,j,k)-c_post(ic_tab))*invdelc

        yderiv_field(i,j,k,1)=0.0            !dYdZ
        yderiv_field(i,j,k,2)=   &
         +Yderiv_tab(ic_tab,1,2)*(1.0-cfac)  &
         +Yderiv_tab(ic_tab+1,1,2)*cfac      !dYdC 
        yderiv_field(i,j,k,3)=0.0            !d2YdZ2
        yderiv_field(i,j,k,4)=0.0            !d2YdZdC
      enddo
    enddo
  enddo

endif !nz_tab_pts=1


!-----------------------------------------------------------------------------------------
  return
end subroutine zcderiv_field
!-----------------------------------------------------------------------------------------
!=========================================================================================

!=========================================================================================
subroutine calc_zcdiss(ZZdiss,CCdiss,ZCdiss,io)
!=========================================================================================
! YOU MUST CALL SPECTOPROG BEFORE CALLING CALC_ZCDISS
! Evaluated mixture fraction dissipation, progress variable dissipation and 
! their cross dissipation
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx,ny,nz,n_spec, vary_in_x, vary_in_y, vary_in_z
!mixavg  use variables_m, only : q, u, temp, pressure, yspecies, volum
  use variables_m, only : temp, yspecies, volum, pressure  !lewis
  use reference_m
  use chemkin_m
  use transport_m
  use thermchem_m

  implicit none

  integer, intent(in) :: io
  real, dimension(nx,ny,nz), intent(out) :: ZZdiss,CCdiss,ZCdiss
  integer i,j,k
!  real, dimension(n_spec) :: Lewis
  real, dimension(nx,ny,nz,3) :: grad_mixfrac, grad_prog
  real, dimension(nx,ny,nz) :: rho   !, lambda
  real, dimension(nx,ny,nz) :: diffZ, diffC
  real, dimension(nx,ny,nz,n_spec) :: diffusivities
  
!  rho=1.0/volum  !mixavg
!  call computeCoefficients( pressure, temp, yspecies, rho)  !mixavg
!   call computeCoefficients(Cp, temp)  ! lewis, computes Lambda
!    lambda = getThermalConductivity()

    call computeScalarGradient( ZetaField, grad_mixfrac )
    call computeScalarGradient( PartProg, grad_prog     )
    
  if (vary_in_x == 1) then
    ZZdiss(:,:,:) = grad_mixfrac(:,:,:,1)**2
    CCdiss(:,:,:) = grad_prog(:,:,:,1)**2
    ZCdiss(:,:,:) = grad_mixfrac(:,:,:,1)*grad_prog(:,:,:,1)
  else
    ZZdiss(:,:,:) = 0.0
    CCdiss(:,:,:) = 0.0
    ZCdiss(:,:,:) = 0.0
  endif

  if (vary_in_y == 1) then
    ZZdiss(:,:,:) = ZZdiss(:,:,:) + grad_mixfrac(:,:,:,2)**2
    CCdiss(:,:,:) = CCdiss(:,:,:) + grad_prog(:,:,:,2)**2
    ZCdiss(:,:,:) = ZCdiss(:,:,:) + grad_mixfrac(:,:,:,2)*grad_prog(:,:,:,2)
  endif

  if (vary_in_z == 1) then
    ZZdiss(:,:,:) = ZZdiss(:,:,:) + grad_mixfrac(:,:,:,3)**2
    CCdiss(:,:,:) = CCdiss(:,:,:) + grad_prog(:,:,:,3)**2
    ZCdiss(:,:,:) = ZCdiss(:,:,:) + grad_mixfrac(:,:,:,3)*grad_prog(:,:,:,3)
  endif

  do i = 1, nx
    do j = 1, ny
      do k = 1, nz
        cpmix(i,j,k) = mixCp(yspecies(i,j,k,:),temp(i,j,k))
      enddo
    enddo
  enddo

  rho=1.0/volum  !mixavg
!  lambda = getThermalConductivity()  

#ifdef LEWIS
  call computeCoefficients( cpmix, temp)
  
  diffZ = getSpcsDiffusivity(n_spec_mf)/rho
  diffC = getSpcsDiffusivity(n_spec_prog)/rho

#endif
#ifdef MIXAVG

  call computeCoefficients(pressure,temp,yspecies,rho)
  diffusivities=getDiffusionCoeff()

  diffZ = diffusivities(:,:,:,n_spec_mf)/rho
  diffC = diffusivities(:,:,:,n_spec_prog)/rho

#endif

!ZZdiss = ZZdiss*(lambda/rho/cpmix)
!CCdiss = CCdiss*(lambda/rho/cpmix)
!ZCdiss = ZCdiss*(lambda/rho/cpmix)

ZZdiss = ZZdiss*diffZ
CCdiss = CCdiss*diffC
ZCdiss = ZCdiss*0.5*(diffC+diffZ)
if(nz_tab_pts.eq.1)then
    ZZdiss=0.0
    ZCdiss=0.0
endif

!-----------------------------------------------------------------------------------------
  return
  end subroutine calc_zcdiss
!-----------------------------------------------------------------------------------------

!=========================================================================================
!=========================================================================================
subroutine calc_CosZC(CosZC,io)
!=========================================================================================
! YOU MUST CALL SPECTOPROG BEFORE CALLING CALC_ZCDISS
! Evaluated mixture fraction dissipation, progress variable dissipation and 
! their cross dissipation
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx,ny,nz,n_spec, vary_in_x, vary_in_y, vary_in_z
!mixavg  use variables_m, only : q, u, temp, pressure, yspecies, volum
  use variables_m, only : temp, yspecies, volum, pressure  !lewis
  use reference_m
  use chemkin_m
  use transport_m
  use thermchem_m

  implicit none

  integer, intent(in) :: io
!  real, dimension(nx,ny,nz), intent(out) :: ZZdiss,CCdiss,ZCdiss
  real, dimension(nx,ny,nz) :: ZZdiss,CCdiss,ZCdiss
  real, dimension(nx,ny,nz), intent(out) :: CosZC
  integer i,j,k
!  real, dimension(n_spec) :: Lewis
  real, dimension(nx,ny,nz,3) :: grad_mixfrac, grad_prog
  real, dimension(nx,ny,nz) :: rho   !, lambda
  real, dimension(nx,ny,nz) :: diffZ, diffC

  real, dimension(nx,ny,nz,n_spec) :: diffusivities
  
call calc_zcdiss(ZZdiss,CCdiss,ZCdiss,io)

where(ZZdiss.gt.1.0E-10.and.CCdiss.gt.1.0E-10)
CosZC = ZCdiss/ZZdiss**0.5/CCdiss**0.5
elsewhere
CosZC = 0.0
endwhere

!-----------------------------------------------------------------------------------------
  return
  end subroutine calc_CosZC
!-----------------------------------------------------------------------------------------

!=========================================================================================
!=========================================================================================
subroutine zclookup(c,zeta,ysp,t,io)
!=========================================================================================
! does the lookup for a field of c-values
! uses simple linear interpolation
! assumes constant intervals
!-----------------------------------------------------------------------------------------
  use param_m, only : nx,ny,nz,n_spec

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  integer, intent(in) :: io !io unit
  real, dimension(nx,ny,nz),        intent(in) :: c        !lookup parameter
  real, dimension(nx,ny,nz),        intent(in) :: zeta     !lookup parameter

  real, dimension(nx,ny,nz,n_spec), intent(out) :: ysp     !species mass fraction
  real, dimension(nx,ny,nz),        intent(out) :: t       !temperature
!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k                   !grid counters
  integer :: ic_tab,iz_tab           !lookup entry
  real :: cfac,zfac                  !interpolating factor
  real :: invdelc,invdelz            !inverse c-interval
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_zclookup) call initialize_zclookup(io,+1)

!    do k=1,nz
!      write(*,*)'k',k,zeta(nx/2,ny/2,k),c(nx/2,ny/2,k)
!    enddo
!
!    do j=1,ny
!      write(*,*)'j',j,zeta(nx/2,j,nz/2),c(nx/2,j,nz/2)
!    enddo
!
!    do i=1,nx
!      write(*,*)'i',i,zeta(i,ny/2,nz/2),c(i,ny/2,nz/2)
!    enddo


!     zeta=0.0

  invdelc=1.0/(max(delc,1.0D-10))
  invdelz=1.0/(max(delz,1.0D-10))

if(nz_tab_pts.gt.1)then
  do k=1,nz
    do j=1,ny
      do i=1,nx
        
        ic_tab=floor((c(i,j,k)-cmin)*invdelc)+1
        ic_tab=max(min(ic_tab,nc_tab_pts-1),1)
        cfac=(c(i,j,k)-c_tab(ic_tab))*invdelc

        iz_tab=floor((zeta(i,j,k)-zetamin)*invdelz)+1
        iz_tab=max(min(iz_tab,nz_tab_pts-1),1)
        zfac=(zeta(i,j,k)-z_tab(iz_tab))*invdelz

! it would be better, and quite easy to use a linear interpolation using all four vertices.

        t(i,j,k)=   &
         +temp_tab(ic_tab,iz_tab)*(1.0-cfac)*(1.0-zfac)    &
         +temp_tab(ic_tab,iz_tab+1)*(1.0-cfac)*zfac  &
         +temp_tab(ic_tab+1,iz_tab)*cfac*(1.0-zfac)  &
         +temp_tab(ic_tab+1,iz_tab+1)*cfac*zfac
        ysp(i,j,k,:)=   &
         +yspec_tab(:,ic_tab,iz_tab)*(1.0-cfac)*(1.0-zfac)   &
         +yspec_tab(:,ic_tab,iz_tab+1)*(1.0-cfac)*zfac   &
         +yspec_tab(:,ic_tab+1,iz_tab)*cfac*(1.0-zfac)   &
         +yspec_tab(:,ic_tab+1,iz_tab+1)*cfac*zfac

      enddo
    enddo
  enddo
else !nz_tab_pts
  do k=1,nz
    do j=1,ny
      do i=1,nx
        
        ic_tab=floor((c(i,j,k)-cmin)*invdelc)+1
        ic_tab=max(min(ic_tab,nc_tab_pts-1),1)
        cfac=(c(i,j,k)-c_tab(ic_tab))*invdelc

! it would be better, and quite easy to use a linear interpolation using all four vertices.
        t(i,j,k)=   &
         +temp_tab(ic_tab,1)*(1.0-cfac)    &
         +temp_tab(ic_tab+1,1)*cfac
        ysp(i,j,k,:)=   &
         +yspec_tab(:,ic_tab,1)*(1.0-cfac)   &
         +yspec_tab(:,ic_tab+1,1)*cfac   

      enddo
    enddo
  enddo
endif !nz_tab_pts

!-----------------------------------------------------------------------------------------
  return
end subroutine zclookup
!-----------------------------------------------------------------------------------------

!=========================================================================================
subroutine zclookup_1val(c,zeta,ysp,t,io)
!=========================================================================================
! does the lookup for a single c-value
! uses simple linear interpolation
! assumes constant intervals
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  integer,                 intent(in) :: io       !io unit
  real,                    intent(in) :: c        !lookup parameter
  real,                    intent(in) :: zeta     !lookup parameter

  real, dimension(n_spec), intent(out) :: ysp     !species mass fraction
  real,                    intent(out) :: t       !temperature
!-----------------------------------------------------------------------------------------
! local declarations

  integer :: ic_tab                   !lookup entry
  integer :: iz_tab                   !lookup entry
  real :: cfac                       !interpolating factor
  real :: zfac                       !interpolating factor
  real :: invdelc                    !inverse c-interval
  real :: invdelz                    !inverse z-interval
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_zclookup) call initialize_zclookup(io,+1)

  invdelc=1.0/(max(delc,1.0D-10))
  invdelz=1.0/(max(delz,1.0D-10))


                                                                                                                                                             
        ic_tab=floor((c-cmin)*invdelc)+1
        ic_tab=max(min(ic_tab,nc_tab_pts-1),1)
        cfac=(c-c_tab(ic_tab))*invdelc
                                                                                                                                                             
if (nz_tab_pts.gt.1)then                                                                                                                                                             
        iz_tab=floor((zeta-zetamin)*invdelz)+1
        iz_tab=max(min(iz_tab,nz_tab_pts-1),1)
        zfac=(zeta-z_tab(iz_tab))*invdelz
                                                                                                                                                             
        t=   &
         +temp_tab(ic_tab,iz_tab)*(1.0-cfac)*(1.0-zfac)    &
         +temp_tab(ic_tab,iz_tab+1)*(1.0-cfac)*zfac  &
         +temp_tab(ic_tab+1,iz_tab)*cfac*(1.0-zfac)  &
         +temp_tab(ic_tab+1,iz_tab+1)*cfac*zfac
        ysp(:)=   &
         +yspec_tab(:,ic_tab,iz_tab)*(1.0-cfac)*(1.0-zfac)   &
         +yspec_tab(:,ic_tab,iz_tab+1)*(1.0-cfac)*zfac   &
         +yspec_tab(:,ic_tab+1,iz_tab)*cfac*(1.0-zfac)   &
         +yspec_tab(:,ic_tab+1,iz_tab+1)*cfac*zfac

else  !nz_tab_pts
        t=   &
         +temp_tab(ic_tab,1)*(1.0-cfac)    &
         +temp_tab(ic_tab+1,1)*cfac  
        ysp(:)=   &
         +yspec_tab(:,ic_tab,1)*(1.0-cfac)   &
         +yspec_tab(:,ic_tab+1,1)*cfac

endif !nz_tab_pts

!-----------------------------------------------------------------------------------------
  return
end subroutine zclookup_1val
!-----------------------------------------------------------------------------------------



!=========================================================================================
subroutine calc_zrho0(rho0,c,zeta,press_in,io)
!=========================================================================================
! calculates rho for c=0 in s3d
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec
  use chemkin_m, only : molwt_c
  use reference_m, only : a_ref,t_ref,univ_gascon

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  real,    intent(out) :: rho0       !fresh gas density
!  real,    intent(out) :: rhoL       !Burnt gas density
  real,    intent(in)  :: zeta       !lookup variable
  real,    intent(in)  :: c          !lookup variable
  real,    intent(in)  :: press_in   !pressure
  integer, intent(in)  :: io         !io unit
!-----------------------------------------------------------------------------------------
! local declarations

  real avmolwt !average inverse molecular weight
  real invdelz,invdelc
  real zfac,cfac
  integer iz_tab,ic_tab
  real, dimension(n_spec) :: ysp     !species mass fraction
  real                   :: t       !temperature


!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_zclookup) call initialize_zclookup(io,+1)

  invdelc=1.0/(max(delc,1.0D-10))
  invdelz=1.0/(max(delz,1.0D-10))

        ic_tab=floor((c-cmin)*invdelc)+1
        ic_tab=max(min(ic_tab,nc_tab_pts-1),1)
        cfac=(c-c_tab(ic_tab))*invdelc

if(nz_tab_pts.gt.1)then
        iz_tab=floor((zeta-zetamin)*invdelz)+1
        iz_tab=max(min(iz_tab,nz_tab_pts-1),1)
        zfac=(zeta-z_tab(iz_tab))*invdelz
 
        t=   &
         +temp_tab(ic_tab,iz_tab)*(1.0-cfac)*(1.0-zfac)    &
         +temp_tab(ic_tab,iz_tab+1)*(1.0-cfac)*zfac  &
         +temp_tab(ic_tab+1,iz_tab)*cfac*(1.0-zfac)  &
         +temp_tab(ic_tab+1,iz_tab+1)*cfac*zfac
        ysp(:)=   &
         +yspec_tab(:,ic_tab,iz_tab)*(1.0-cfac)*(1.0-zfac)   &
         +yspec_tab(:,ic_tab,iz_tab+1)*(1.0-cfac)*zfac   &
         +yspec_tab(:,ic_tab+1,iz_tab)*cfac*(1.0-zfac)   &
         +yspec_tab(:,ic_tab+1,iz_tab+1)*cfac*zfac

else !nz_tab_pts 
        t=   &
         +temp_tab(ic_tab,1)*(1.0-cfac)    &
         +temp_tab(ic_tab+1,1)*cfac  
        ysp(:)=   &
         +yspec_tab(:,ic_tab,1)*(1.0-cfac)   &
         +yspec_tab(:,ic_tab+1,1)*cfac
endif !nz_tab_pts
  avmolwt=sum(ysp(:)*molwt_c(:))

! set density in q-vector (equation of state)

  rho0=press_in/(univ_gascon*(1.0/a_ref**2)*t_ref*t*avmolwt)

!-----------------------------------------------------------------------------------------
  return
end subroutine calc_zrho0
!-----------------------------------------------------------------------------------------

!=========================================================================================
!=========================================================================================
subroutine calc_zrhofield(rho0,c,zeta,press_in,io)                                              
!=========================================================================================
! calculates rho for c=0 in s3d
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec,nx,ny,nz
  use chemkin_m, only : molwt_c
  use reference_m, only : a_ref,t_ref,univ_gascon

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  real,dimension(nx,ny,nz),    intent(out) :: rho0       !fresh gas density
!  real,    intent(out) :: rhoL       !Burnt gas density
  real,dimension(nx,ny,nz),    intent(in)  :: zeta       !lookup variable
  real,dimension(nx,ny,nz),    intent(in)  :: c          !lookup variable
  real,dimension(nx,ny,nz),    intent(in)  :: press_in   !pressure
  integer, intent(in)  :: io         !io unit
!-----------------------------------------------------------------------------------------
! local declarations

  real avmolwt !average inverse molecular weight
  real invdelz,invdelc
  real zfac,cfac
  integer iz_tab,ic_tab
  real, dimension(n_spec) :: ysp     !species mass fraction
  real                   :: t       !temperature
  integer i,j,k

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_zclookup) call initialize_zclookup(io,+1)

  invdelc=1.0/(max(1.0D-10,delc))
  invdelz=1.0/(max(delz,1.0D-10))

if(nz_tab_pts.gt.1)then
  do k=1,nz
    do j=1,ny
      do i=1,nx
                        
        ic_tab=floor((c(i,j,k)-cmin)*invdelc)+1
        ic_tab=max(min(ic_tab,nc_tab_pts-1),1)
        cfac=(c(i,j,k)-c_tab(ic_tab))*invdelc

        iz_tab=floor((zeta(i,j,k)-zetamin)*invdelz)+1
        iz_tab=max(min(iz_tab,nz_tab_pts-1),1)
        zfac=(zeta(i,j,k)-z_tab(iz_tab))*invdelz
 
        t=   &
         +temp_tab(ic_tab,iz_tab)*(1.0-cfac)*(1.0-zfac)    &
         +temp_tab(ic_tab,iz_tab+1)*(1.0-cfac)*zfac  &
         +temp_tab(ic_tab+1,iz_tab)*cfac*(1.0-zfac)  &
         +temp_tab(ic_tab+1,iz_tab+1)*cfac*zfac
        ysp(:)=   &
         +yspec_tab(:,ic_tab,iz_tab)*(1.0-cfac)*(1.0-zfac)   &
         +yspec_tab(:,ic_tab,iz_tab+1)*(1.0-cfac)*zfac   &
         +yspec_tab(:,ic_tab+1,iz_tab)*cfac*(1.0-zfac)   &
         +yspec_tab(:,ic_tab+1,iz_tab+1)*cfac*zfac

  avmolwt=sum(ysp(:)*molwt_c(:))

! set density in q-vector (equation of state)

  rho0(i,j,k)=press_in(i,j,k)/(univ_gascon*(1.0/a_ref**2)*t_ref*t*avmolwt)

      enddo
    enddo
  enddo
else !nz_tab_pts
  do k=1,nz
    do j=1,ny
      do i=1,nx
                        
        ic_tab=floor((c(i,j,k)-cmin)*invdelc)+1
        ic_tab=max(min(ic_tab,nc_tab_pts-1),1)
        cfac=(c(i,j,k)-c_tab(ic_tab))*invdelc

        t=   &
         +temp_tab(ic_tab,1)*(1.0-cfac)    &
         +temp_tab(ic_tab+1,1)*cfac
        ysp(:)=   &
         +yspec_tab(:,ic_tab,1)*(1.0-cfac)   &
         +yspec_tab(:,ic_tab+1,1)*cfac

  avmolwt=sum(ysp(:)*molwt_c(:))

! set density in q-vector (equation of state)

  rho0(i,j,k)=press_in(i,j,k)/(univ_gascon*(1.0/a_ref**2)*t_ref*t*avmolwt)

      enddo
    enddo
  enddo
endif !nz_tab_pts

!-----------------------------------------------------------------------------------------
  return
end subroutine calc_zrhofield
!-----------------------------------------------------------------------------------------

!=========================================================================================
!=========================================================================================
subroutine calc_YtoCfield(c,yinput,zeta,io)                                              
!=========================================================================================
! calculates rho for c=0 in s3d
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec,nx,ny,nz
!  use chemkin_m, only : molwt_c
!  use reference_m, only : a_ref,t_ref,univ_gascon

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  real,dimension(nx,ny,nz),    intent(out) :: c          !progress variable field
  real,dimension(nx,ny,nz),    intent(in)  :: zeta       !mixfrac field
  real,dimension(nx,ny,nz),    intent(in)  :: yinput     !field of species used to define c
  integer, intent(in)  :: io         !io unit
!-----------------------------------------------------------------------------------------
! local declarations

  real invdelz
  real zfac
  integer iz_tab
  integer i,j,k
  real cvar, yunburnt, yburnt

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_zclookup) call initialize_zclookup(io,+1)

! find the burnt and unburnt values of y(n_spec_prog)

  invdelz=1.0/(max(delz,1.0D-10))

if(nz_tab_pts.gt.1)then
  do k=1,nz
    do j=1,ny
      do i=1,nx

        iz_tab=floor((zeta(i,j,k)-zetamin)*invdelz)+1
        iz_tab=max(min(iz_tab,nz_tab_pts-1),1)
        zfac=(zeta(i,j,k)-z_tab(iz_tab))*invdelz
                                
        yunburnt=   &
         +yspec_tab(n_spec_prog,1,iz_tab)*(1.0-zfac) &
         +yspec_tab(n_spec_prog,1,iz_tab+1)*zfac

        yburnt=   &
         +yspec_tab(n_spec_prog,nc_tab_pts,iz_tab)*(1.0-zfac) &
         +yspec_tab(n_spec_prog,nc_tab_pts,iz_tab+1)*zfac

        cvar=abs(yinput(i,j,k)-yunburnt)/(abs(yburnt-yunburnt)+1.0E-10)

        c(i,j,k)=max(0.0,min(cvar,1.0))

      enddo
    enddo
  enddo
else !nz_tab_pts
  do k=1,nz
    do j=1,ny
      do i=1,nx

        yunburnt=   &
         +yspec_tab(n_spec_prog,1,1)

        yburnt=   &
         +yspec_tab(n_spec_prog,nc_tab_pts,1)

        cvar=abs(yinput(i,j,k)-yunburnt)/(abs(yburnt-yunburnt)+1.0E-10)

        c(i,j,k)=max(0.0,min(cvar,1.0))

      enddo
    enddo
  enddo
endif ! nz_tab_pts
!-----------------------------------------------------------------------------------------
  return
end subroutine calc_YtoCfield
!-----------------------------------------------------------------------------------------
!=========================================================================================
subroutine calc_ZtoRhoSl(zeta,rhosl,io)                                              
!=========================================================================================
! calculates nondimensional laminar flame * density speed as function of zeta based on 
! polynomial fits.
! This is chemistry specific so be careful.
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec,nx,ny,nz
!  use chemkin_m, only : molwt_c
  use reference_m, only : rho_ref, a_ref

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  real,dimension(nx,ny,nz),    intent(out) :: rhosl          !density * flame speed field
  real,dimension(nx,ny,nz),    intent(in)  :: zeta       !mixfrac field
  integer, intent(in)  :: io         !io unit
!-----------------------------------------------------------------------------------------
! local declarations

  integer i,j,k

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

if (n_spec.eq.15)then  ! assumes z=0, phi=0.41, z=1, phi=1.0, Tu=800K, methane
rhosl= 0.2352*zeta**6.0 - 1.0367*zeta**5.0 + 1.8294*zeta**4.0 - 1.8871*zeta**3.0 + 0.4882*zeta**2.0 + 1.1933*zeta + 0.2524

elseif(n_spec.eq.29 )then !assumes z=0, phi=0.0, z=1, phi=1.46, Tu=800K, methane 
rhosl= - 38.478*zeta**6.0 + 127.85* zeta**5.0 - 155.62 * zeta**4.0 + 76.7*zeta**3.0 - 10.211*zeta**2.0 + 0.2514*zeta

else   ! assumes phi=0.7, Tu=800K, (premixed)

rhosl = 0.821544

endif

!non-dimensionalize
rhosl = rhosl/a_ref/rho_ref

!-----------------------------------------------------------------------------------------
  return
end subroutine calc_ZtoRhoSl
!-----------------------------------------------------------------------------------------

!=========================================================================================
  end module zclookup_m


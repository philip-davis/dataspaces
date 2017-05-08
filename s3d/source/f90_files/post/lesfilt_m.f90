!=========================================================================================
  module lesfilt_m
!=========================================================================================
! module for les filtering of field variables
! Evatt Hawkes AUG 2003
! Evatt Hawkes partial update august 2009 for Obulesu
! new ghost zones util for basic scalar filtering.
! Added to main S3D repository Oct 2010.
! Suggested usage of simple/fast box filter:
! NB. it is necessary to deallocate the filter variables before resetting the filter size.
!>   if(initialized_lesfilt) call allocate_lesfilt(-1) 
!>   iftype=0
!>   ndelta=9
!>   if(.not.initialized_lesfilt) call initialize_lesfilt(io)
!>   if(myid==0)write(io,*)'filtering field with filter type',iftype,' and ndelta=',ndelta
!>   call filt_scalar(chi(:,:,:),filteredfield(:,:,:))
!
! restrictions:  -uniform grid, and delx=dely=delz for both grid and filter
!                -can do top-hat or gaussian
!                -filter result is reported at every dns result point

  implicit none
!-----------------------------------------------------------------------------------------
! variable declarations

  private
  
  logical :: initialized_lesfilt=.false. ! initialisation flag

  integer :: nfwidth                     ! computational extent of les filter
  
  integer :: ndelta                      ! filter width in units of dx
  
  integer :: iftype                      ! filter type : =0 box
                                         !               =1 gaussian

  real :: delta                          ! actual nondimensional filter size

  integer :: n_hw                        ! filter half-width

  integer :: n_pad_x,n_pad_y,n_pad_z     ! ghost-zone padding
  
  integer :: i_min_gz,i_max_gz           ! ghost-zone min and max
  integer :: j_min_gz,j_max_gz           ! ghost-zone min and max
  integer :: k_min_gz,k_max_gz           ! ghost-zone min and max
  

  integer, allocatable :: is(:),ie(:)    ! i index start and end arrays
  integer, allocatable :: js(:),je(:)    ! j index start and end arrays  
  integer, allocatable :: ks(:),ke(:)    ! k index start and end arrays 
  
  real, allocatable :: gfilt(:,:,:)      ! filter function 
  
  
  real, allocatable :: field_padded(:,:,:)    ! scalar field padded with ghost zone
  
  logical :: odd_delta                   ! true if ndelta is odd  

  integer :: nsgvol  !sub-grid volume in units of dx

  real :: fact_norm  ! multiplier for box filter


! for conditional filtering
  logical :: initialized_cndtl_lesfilt =.false.  !initialization flag  
  real, allocatable :: fact_norm_cndtl(:)        !multipliers for conditional odd-box filter
  integer, allocatable :: iflag_used(:,:,:)      !number of points used at each cell 
  real, allocatable :: cndtl_fact(:,:,:)         !number of points used at each cell 
  logical, allocatable :: cnd_flag_padded(:,:,:) !padded conditioning flag


! for test filtering
  logical :: initialized_test_lesfilt        ! initialisation flag
  integer :: nfwidtht                        ! computational extent of test filter
  integer :: ndeltat                         ! test filter width in units of dx
  real :: deltat                             ! actual nondimensional filter size
  integer :: n_hwt                           ! filter half-width
  integer :: n_pad_xt,n_pad_yt,n_pad_zt      ! ghost-zone padding
  integer :: i_min_gzt,i_max_gzt             ! ghost-zone min and max
  integer :: j_min_gzt,j_max_gzt             ! ghost-zone min and max
  integer :: k_min_gzt,k_max_gzt             ! ghost-zone min and max
  integer, allocatable :: ist(:),iet(:)      ! i index start and end arrays
  integer, allocatable :: jst(:),jet(:)      ! j index start and end arrays  
  integer, allocatable :: kst(:),ket(:)      ! k index start and end arrays   
  real, allocatable :: gfilt_t(:,:,:)        ! filter function 
  real, allocatable :: field_padded_t(:,:,:) ! scalar field padded with ghost zone
  logical :: odd_deltat                      ! true if ndelta is odd  
  integer :: nsgvolt                         ! sub-grid volume in units of dx
  real :: fact_normt                         ! multiplier for box filter

! for fast box filtering
  real, allocatable :: field_jk_sum(:)       ! plane sums
  integer, allocatable :: im1(:)             ! i-1
  integer, allocatable :: jm1(:)             ! j-1
  integer, allocatable :: km1(:)             ! k-1
  real, allocatable :: field_jk_sum_t(:)       ! plane sums


! declare publics
  public :: initialize_lesfilt           ! initialisation routine
  public :: allocate_lesfilt             ! allocation routine
  public :: initialized_lesfilt          ! initialisation flag
  public :: filt_scalar                  ! filters a scalar field 

!  public :: filt_vector                 ! filters a vector field 
  public :: filt_scalar_cndtl            ! filters a scalar field given a condition
  public :: initialize_cndtl_lesfilt     ! initialises conditional filtering
  public :: initialize_lesfilt_t         ! test filter initialisation routine
  public :: allocate_lesfilt_t           ! allocation routine
  public :: initialized_test_lesfilt     ! initialisation flag
  public :: filt_scalar_test             ! filters a scalar field 

  public :: nfwidth,ndelta,iftype,delta  ! public variables
  public :: nfwidtht,ndeltat,deltat      ! public variables

!-----------------------------------------------------------------------------------------
  contains


!=========================================================================================
subroutine allocate_lesfilt(flag)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! initialises the lesfilt module
!----------------------------------------------------------------------------------------
! modules used
  use param_m, only : nx,ny,nz

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  integer, intent(in) :: flag    ! operation flag: =1 allocate =-1 deallocate

!-----------------------------------------------------------------------------------------
! local declarations

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(flag==1)then
  
    allocate(is(nx));is=0
    allocate(ie(nx));is=0
    allocate(js(ny));js=0
    allocate(je(ny));js=0
    allocate(ks(nz));ks=0
    allocate(ke(nz));ks=0
    
    allocate(gfilt(-n_pad_x:n_pad_x,-n_pad_y:n_pad_y,-n_pad_z:n_pad_z));gfilt=0.  
    
!    allocate(field_padded(i_min_gz:i_max_gz,j_min_gz:j_max_gz,k_min_gz:k_max_gz)); field_padded=0.  
    
    allocate(fact_norm_cndtl(nsgvol));fact_norm_cndtl=0.
!    allocate(cnd_flag_padded(i_min_gz:i_max_gz,j_min_gz:j_max_gz,k_min_gz:k_max_gz))
!    allocate(iflag_used(nx,ny,nz));iflag_used=0
!    allocate(cndtl_fact(nx,ny,nz));cndtl_fact=0.

!   for fast box filtering
    allocate(field_jk_sum(i_min_gz:i_max_gz));field_jk_sum=0.0
    allocate(im1(nx));im1=0
    allocate(jm1(ny));jm1=0
    allocate(km1(nz));km1=0
        
  elseif(flag==-1)then
  
    deallocate(is)
    deallocate(ie)
    deallocate(js)
    deallocate(je)
    deallocate(ks)
    deallocate(ke)
    
    deallocate(gfilt)
    
!    deallocate(field_padded)
    
    deallocate(fact_norm_cndtl)
!    deallocate(cnd_flag_padded)
!    deallocate(iflag_used)
!    deallocate(cndtl_fact)

    deallocate(field_jk_sum)
    deallocate(im1)
    deallocate(jm1)
    deallocate(km1)

    initialized_lesfilt=.false.
    
    initialized_cndtl_lesfilt=.false.
    
  endif


  return
end subroutine allocate_lesfilt
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine initialize_lesfilt(io)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! initialises the lesfilt module
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
  use grid_m, only : delx,dely,delz

  use topology_m, only : myid, gcomm, ierr

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  integer, intent(in) :: io    ! io unit 

! output argument declarations

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k
  
  real :: pi
  real :: argx,argy,argz

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------


  call MPI_Barrier(gcomm,ierr)
! message
  if(myid==0)then
    write(io,*) 'Initialising filter module'
    write(io,*)
    call write_header(io,'-')
  endif

! set delta
! assuming uniform cubic grid
  delta=ndelta*delx

! for box filter - check if even or odd
  if(iftype==0)then
  
    if(mod(ndelta,2).eq.0)then
      ! filter is even
      odd_delta =.false.

      ! (re-)set filter calculation half-width
      nfwidth=ndelta+1	
      
    else
      !filter is odd
      odd_delta =.true.
      
      ! (re-)set filter calculation half-width
      nfwidth=ndelta     
       
    endif ! check of even delta
    
  endif  !check of box filter

 
! set ghost zone size
  n_hw=(nfwidth-1)/2
 
  if(vary_in_x==1)then
    n_pad_x=n_hw
  else
    n_pad_x=0
  endif
  if(vary_in_y==1)then
    n_pad_y=n_hw
  else
    n_pad_y=0
  endif
  if(vary_in_z==1)then
    n_pad_z=n_hw
  else
    n_pad_z=0
  endif 

! set ghostzone limits
  i_min_gz=1-n_pad_x
  i_max_gz=nx+n_pad_x
  
  j_min_gz=1-n_pad_y
  j_max_gz=ny+n_pad_y 
  
  k_min_gz=1-n_pad_z
  k_max_gz=nz+n_pad_z  
  
! sub-grid cell volume
  nsgvol=ndelta**numdim  
  
! allocate as required
  call allocate_lesfilt(1)  
  
! normalising factor for box and gaussian filter

  fact_norm=1.0/real(nsgvol)

! normalising factor for conditional odd-delta box filtering  
  do i=1,nsgvol
    fact_norm_cndtl(i)=1.0/real(i)
  enddo  
 
! set up start and end indices to avoid recalculation
  do i=1,nx 
    is(i)=i-n_pad_x
    ie(i)=i+n_pad_x
  enddo
  do j=1,ny 
    js(j)=j-n_pad_y
    je(j)=j+n_pad_y
  enddo
  do k=1,nz 
    ks(k)=k-n_pad_z
    ke(k)=k+n_pad_z
  enddo
 
! set filter function
  Filtertype : select case(iftype)
  
    case(0)
    ! filter is a box filter
    ! note : function is only used for edge nodes in case of even ndelta
    gfilt(:,:,:)=1.0

    if(.not.odd_delta)then
      if(vary_in_x==1)then
        gfilt(-n_pad_x,:,:)=gfilt(-n_pad_x,:,:)*0.5
        gfilt( n_pad_x,:,:)=gfilt( n_pad_x,:,:)*0.5
      endif
      if(vary_in_y==1)then
        gfilt(:,-n_pad_y,:)=gfilt(:,-n_pad_y,:)*0.5
        gfilt(:, n_pad_y,:)=gfilt(:, n_pad_y,:)*0.5
      endif
      if(vary_in_z==1)then
        gfilt(:,:,-n_pad_z)=gfilt(:,:,-n_pad_z)*0.5
        gfilt(:,:, n_pad_z)=gfilt(:,:, n_pad_z)*0.5
      endif
    endif
    
    if(myid==0)then
      write(io,*) 'filter is box, delta =', ndelta
    endif
      
    case(1)
    ! filter is gaussian
   
    pi=4.0*atan(1.0)
    do i=-n_pad_x,n_pad_x   
      do j=-n_pad_y,n_pad_y  
	do k=-n_pad_z,n_pad_z  
          argx=real(i)/real(ndelta)
          argx=-6.0*argx*argx
          argy=real(j)/real(ndelta)
          argy=-6.0*argy*argy
          argz=real(k)/real(ndelta)
          argz=-6.0*argz*argz
          gfilt(i,j,k)=(6.0/pi/delta/delta)**(real(numdim)/2.0)*exp(argx+argy+argz) &
	               *delx*dely*delz
        enddo
      enddo
    enddo
      
    if(myid==0)then
      write(io,*) 'filter is gaussian, delta =', ndelta
    endif
    
  end select Filtertype

! for fast box-filtering
  do i=1,nx
    im1(i)=i-1
  enddo

! for fast box-filtering
  do j=1,ny
    jm1(j)=j-1
  enddo

! for fast box-filtering
  do k=1,nx
    km1(k)=k-1
  enddo


! set initialization flag
  initialized_lesfilt =.true.

! header
  if(myid==0)then
    call write_header(io,'-')
  endif

  return
end subroutine initialize_lesfilt
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine filt_scalar(field_dns,field_les)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! filters a scalar field using a cubic (or square) top hat filter
!
! -filter restricted to uniform grid
! -filtered field is reported at every DNS grid node
! -filter handles parallelism by creation of ghost zone
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid, gcomm, ierr, neighbor
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
!  use ghost_zones_m, only : fill_ghost_zones
  use ghost_nice_m, only : ghostzone_real

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  real, dimension(nx,ny,nz), intent(in)  :: field_dns   ! dns field to be filtered

! output argument declarations

  real, dimension(nx,ny,nz), intent(out) :: field_les   ! filtered result

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k

  integer :: iflt

  real, dimension(i_min_gz:i_max_gz,j_min_gz:j_max_gz,k_min_gz:k_max_gz) :: field_pad
  real, dimension(i_min_gz:i_max_gz,j_min_gz:j_max_gz,k_min_gz:k_max_gz) :: tmp

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

! checks n_delta
  if(ndelta.le.1)then
!   no filtering
    field_les(:,:,:)=field_dns(:,:,:)
    return
  endif
  
! fill the ghost zone
!  call fill_ghost_zones(field_dns,                                        &
!                        n_pad_x,n_pad_x,n_pad_y,n_pad_y,n_pad_z,n_pad_z,  &
!                        field_padded) 


! debug
 ! call MPI_Barrier(gcomm,ierr)
 ! if(myid==0)then
   ! write(7,*) 'inside filt_scalar',i_min_gz,i_max_gz,j_min_gz,j_max_gz,k_min_gz,k_max_gz,nx,ny,nz
  !  write(myid+200,*)'I am here'
   ! call flush(myid+200)
  !endif
!call MPI_Barrier(gcomm,ierr)

  field_pad(1:nx,1:ny,1:nz)=field_dns(:,:,:)

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
  !  write(7,*) 'before ghostzone'
  !  call flush(7)
  endif
  call MPI_Barrier(gcomm,ierr)


  call ghostzone_real(field_pad, nx, ny, nz, n_pad_x, n_pad_x, &
                                                n_pad_y, n_pad_y, &
                                                n_pad_z, n_pad_z)

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
 !   write(7,*) 'after ghostzone'
 !   call flush(7)
  endif
 
! fill points for non-periodic boundaries
! adjust back to first point inside domain
  
!  if((periodic_x==0).and.(neighbor(1).lt.0))then
!    do i=i_min_gz,0
!      field_pad(i,:,:)= field_pad(1,:,:)
!    enddo
!  endif
!  if((periodic_x==0).and.(neighbor(2).lt.0))then
!    do i=nx+1,i_max_gz
!      field_pad(i,:,:)= field_pad(nx,:,:)
!    enddo
!  endif  
!  if((periodic_y==0).and.(neighbor(3).lt.0))then
!    do j=j_min_gz,0
!      field_pad(:,j,:)= field_pad(:,1,:)
!    enddo
!  endif
!  if((periodic_y==0).and.(neighbor(4).lt.0))then
!    do j=ny+1,j_max_gz
!      field_pad(:,j,:)= field_pad(:,ny,:)
!    enddo
!  endif  
!  if((periodic_z==0).and.(neighbor(5).lt.0))then
!    do k=k_min_gz,0
!      field_pad(:,:,k)= field_pad(:,:,1)
!    enddo
!  endif
!  if((periodic_z==0).and.(neighbor(6).lt.0))then
!    do k=nz+1,k_max_gz
!      field_pad(:,:,k)= field_pad(:,:,nz)
!    enddo
!  endif
  
  if((iftype==0).and.(odd_delta))then
 
!   this was a lot slower for larger filters.... 
!    do k=1,nz 
!      do j=1,ny 
!        do i=1,nx
!          field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
!        enddo
!      enddo
!    enddo

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
 !   write(7,*) 'filtering'
 !   call flush(7)
  endif


!!   fast box filtering - march in x-direction adding and subtracting planes
!    do k=1,nz 
!      do j=1,ny
!
!!       do the plane sums 
!        do iflt=is(1),ie(nx)
!          field_jk_sum(iflt)= sum(field_pad(iflt,js(j):je(j),ks(k):ke(k)))
!        enddo
!
!!       i=1
!        field_les(1,j,k) = sum(field_jk_sum(is(1):ie(1)))
!
!!       step in i
!        do i=2,nx
!          field_les(i,j,k)= field_les(im1(i),j,k) + field_jk_sum(ie(i))-field_jk_sum(is(im1(i)))
!        enddo
!
!      enddo
!    enddo


!   fast box filtering

!   x direction
    do k=ks(1),ke(nz)
      do j=js(1),je(ny)
        do i=1,nx
          tmp(i,j,k)=sum(field_pad(is(i):ie(i),j,k))
        enddo
      enddo
    enddo

!   y direction
    do k=ks(1),ke(nz)
      do i=1,nx
        do j=1,ny
          field_pad(i,j,k)=sum(tmp(i,js(j):je(j),k))
        enddo
      enddo
    enddo

!   z direction
    do j=1,ny
      do i=1,nx
        do k=1,nz
          field_les(i,j,k)=sum(field_pad(i,j,ks(k):ke(k)))
        enddo
      enddo
    enddo


! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
 !   write(7,*) 'before normalise'
 !   call flush(7)
  endif   

    ! normalise
    field_les(:,:,:)=field_les(:,:,:)*fact_norm


! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
!    write(7,*) 'finished filtering'
    call flush(7)
  endif   
    
  elseif((iftype==0).and.(.not.odd_delta))then 

    if(myid==0)then
      write(6,*) 'Sorry - even filter sizes not implemented - use filt_scalar_old'
    endif
    return
  
  else
  
    if(myid==0)then
      write(6,*) 'Sorry - non top-hat filters not implemented - use filt_scalar_old'
    endif
    return  
  
  endif  


! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
!    write(7,*) 'leaving'
    call flush(7)
  endif


  return
end subroutine filt_scalar
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine filt_scalar_slow(field_dns,field_les)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! filters a scalar field using a cubic (or square) top hat filter
!
! -filter restricted to uniform grid
! -filtered field is reported at every DNS grid node
! -filter handles parallelism by creation of ghost zone
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid, gcomm, ierr, neighbor
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
!  use ghost_zones_m, only : fill_ghost_zones
  use ghost_nice_m, only : ghostzone_real

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  real, dimension(nx,ny,nz), intent(in)  :: field_dns   ! dns field to be filtered

! output argument declarations

  real, dimension(nx,ny,nz), intent(out) :: field_les   ! filtered result

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k

  integer :: iflt

  real, dimension(i_min_gz:i_max_gz,j_min_gz:j_max_gz,k_min_gz:k_max_gz) :: field_pad

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

! checks n_delta
  if(ndelta.le.1)then
!   no filtering
    field_les(:,:,:)=field_dns(:,:,:)
    return
  endif
  
! fill the ghost zone
!  call fill_ghost_zones(field_dns,                                        &
!                        n_pad_x,n_pad_x,n_pad_y,n_pad_y,n_pad_z,n_pad_z,  &
!                        field_padded) 


! debug
 ! call MPI_Barrier(gcomm,ierr)
 ! if(myid==0)then
   ! write(7,*) 'inside filt_scalar',i_min_gz,i_max_gz,j_min_gz,j_max_gz,k_min_gz,k_max_gz,nx,ny,nz
  !  write(myid+200,*)'I am here'
   ! call flush(myid+200)
  !endif
!call MPI_Barrier(gcomm,ierr)

  field_pad(1:nx,1:ny,1:nz)=field_dns(:,:,:)

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
  !  write(7,*) 'before ghostzone'
  !  call flush(7)
  endif
  call MPI_Barrier(gcomm,ierr)


  call ghostzone_real(field_pad, nx, ny, nz, n_pad_x, n_pad_x, &
                                                n_pad_y, n_pad_y, &
                                                n_pad_z, n_pad_z)

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
 !   write(7,*) 'after ghostzone'
 !   call flush(7)
  endif
 
! fill points for non-periodic boundaries
! adjust back to first point inside domain
  
  if((periodic_x==0).and.(neighbor(1).lt.0))then
    do i=i_min_gz,0
      field_pad(i,:,:)= field_pad(1,:,:)
    enddo
  endif
  if((periodic_x==0).and.(neighbor(2).lt.0))then
    do i=nx+1,i_max_gz
      field_pad(i,:,:)= field_pad(nx,:,:)
    enddo
  endif  
  if((periodic_y==0).and.(neighbor(3).lt.0))then
    do j=j_min_gz,0
      field_pad(:,j,:)= field_pad(:,1,:)
    enddo
  endif
  if((periodic_y==0).and.(neighbor(4).lt.0))then
    do j=ny+1,j_max_gz
      field_pad(:,j,:)= field_pad(:,ny,:)
    enddo
  endif  
  if((periodic_z==0).and.(neighbor(5).lt.0))then
    do k=k_min_gz,0
      field_pad(:,:,k)= field_pad(:,:,1)
    enddo
  endif
  if((periodic_z==0).and.(neighbor(6).lt.0))then
    do k=nz+1,k_max_gz
      field_pad(:,:,k)= field_pad(:,:,nz)
    enddo
  endif
  
  if((iftype==0).and.(odd_delta))then

 
!   this was a lot slower for larger filters.... 
!    do k=1,nz 
!      do j=1,ny 
!        do i=1,nx
!          field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
!        enddo
!      enddo
!    enddo

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
 !   write(7,*) 'filtering'
 !   call flush(7)
  endif


!   fast box filtering - march in x-direction adding and subtracting planes
   do k=1,nz 
      do j=1,ny

!       do the plane sums 
        do iflt=is(1),ie(nx)
          field_jk_sum(iflt)= sum(field_pad(iflt,js(j):je(j),ks(k):ke(k)))
        enddo

!       i=1
        field_les(1,j,k) = sum(field_jk_sum(is(1):ie(1)))

!       step in i
        do i=2,nx
          field_les(i,j,k)= field_les(im1(i),j,k) + field_jk_sum(ie(i))-field_jk_sum(is(im1(i)))
        enddo

      enddo
    enddo


! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
 !   write(7,*) 'before normalise'
 !   call flush(7)
  endif   

    ! normalise
    field_les(:,:,:)=field_les(:,:,:)*fact_norm


! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
!    write(7,*) 'finished filtering'
    call flush(7)
  endif   
    
  elseif((iftype==0).and.(.not.odd_delta))then 

    if(myid==0)then
      write(6,*) 'Sorry - even filter sizes not implemented - use filt_scalar_old'
    endif
    return
  
  else
  
    if(myid==0)then
      write(6,*) 'Sorry - non top-hat filters not implemented - use filt_scalar_old'
    endif
    return  
  
  endif  


! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
!    write(7,*) 'leaving'
    call flush(7)
  endif


  return
end subroutine filt_scalar_slow
!-----------------------------------------------------------------------------------------

!=========================================================================================
subroutine filt_scalar_old(field_dns,field_les)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! filters a scalar field using a cubic (or square) top hat filter
!
! -filter restricted to uniform grid
! -filtered field is reported at every DNS grid node
! -filter handles parallelism by creation of ghost zone
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid, gcomm, ierr, neighbor
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
!  use ghost_zones_m, only : fill_ghost_zones

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  real, dimension(nx,ny,nz), intent(in)  :: field_dns   ! dns field to be filtered

! output argument declarations

  real, dimension(nx,ny,nz), intent(out) :: field_les   ! filtered result

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------


  if(myid==0) write(*,*) 'error, sub not ready'
  call terminate_run(7,0)

! checks n_delta
  if(ndelta.le.1)then
!   no filtering
    field_les(:,:,:)=field_dns(:,:,:)
    return
  endif
  
! fill the ghost zone
!  call fill_ghost_zones(field_dns,                                        &
!                        n_pad_x,n_pad_x,n_pad_y,n_pad_y,n_pad_z,n_pad_z,  &
!                        field_padded) 
  
! fill points for non-periodic boundaries
! adjust back to first point inside domain
  
  if((periodic_x==0).and.(neighbor(1).lt.0))then
    do i=i_min_gz,0
      field_padded(i,:,:)= field_padded(1,:,:)
    enddo
  endif
  if((periodic_x==0).and.(neighbor(2).lt.0))then
    do i=nx+1,i_max_gz
      field_padded(i,:,:)= field_padded(nx,:,:)
    enddo
  endif  
  if((periodic_y==0).and.(neighbor(3).lt.0))then
    do j=j_min_gz,0
      field_padded(:,j,:)= field_padded(:,1,:)
    enddo
  endif
  if((periodic_y==0).and.(neighbor(4).lt.0))then
    do j=ny+1,j_max_gz
      field_padded(:,j,:)= field_padded(:,ny,:)
    enddo
  endif  
  if((periodic_z==0).and.(neighbor(5).lt.0))then
    do k=k_min_gz,0
      field_padded(:,:,k)= field_padded(:,:,1)
    enddo
  endif
  if((periodic_z==0).and.(neighbor(6).lt.0))then
    do k=nz+1,k_max_gz
      field_padded(:,:,k)= field_padded(:,:,nz)
    enddo
  endif
  
  if((iftype==0).and.(odd_delta))then
  
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
          field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
        enddo
      enddo
    enddo
    
    ! normalise
    field_les(:,:,:)=field_les(:,:,:)*fact_norm
    
  elseif((iftype==0).and.(.not.odd_delta))then 
    
!   TEMPORARY - can be made faster by removing internal multiplications

    do k=1,nz 
      do j=1,ny 
        do i=1,nx
	
	  field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)) &
		       *gfilt(:,:,:))
				 
        enddo
      enddo
    enddo 

!    unfinished code .... to make faster ...
!    internal nodes
!    do k=1,nz 
!      do j=1,ny 
!        do i=1,nx
!	
!	  field_les(i,j,k)= sum(field_padded(is(i)+1:ie(i)-1,js(j)+1:je(j)-1,ks(k)+1:ke(k)-1) &
!		       *gfilt(-n_pad_x+1:n_pad_x-1,-n_pad_y+1:n_pad_y-1,-n_pad_z+1:n_pad_z-1))
!				 
!        enddo
!      enddo
!    enddo 

!    x-y faces etc    
!    do k=1,nz 
!      do j=1,ny 
!        do i=1,nx
!				      				      
!	    do jc=-n_pad_y,n_pad_y
!	      do ic=-n_pad_x,n_pad_x  
!                field_les(i,j,-n_pad_z)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)) &
!		                      *gfilt(:,:,:))
!	      enddo
!	    enddo
!	      	
!        enddo
!      enddo
!    enddo 
    
    ! normalise
    field_les(:,:,:)=field_les(:,:,:)*fact_norm
  
  else
    
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
          field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)) &
		                      *gfilt(:,:,:))	      	
        enddo
      enddo
    enddo  
  
  endif  


  return
end subroutine filt_scalar_old
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine initialize_cndtl_lesfilt(cnd_flag)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! initialises conditional filtering to save computation time for multiple filter ops
! having same condition
!
! MUST BE RE-INITIALISED FOR EACH DIFFERENT CONDITION
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid, gcomm, ierr, neighbor
  use param_m, only : nx,ny,nz, periodic_x,periodic_y,periodic_z, &
                      vary_in_x, vary_in_y, vary_in_z
!  use ghost_zones_m, only : fill_ghost_zones_logical


  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  logical, dimension(nx,ny,nz), intent(in) :: cnd_flag

! output argument declarations

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k
  
  real :: g_used

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(myid==0) write(*,*) 'error, sub not ready'
  call terminate_run(7,0)

! checks n_delta
  if(ndelta.le.1)then
!   no filtering

    do k=1,nz
      do j=1,ny
        do i=1,nx
	  if(cnd_flag(i,j,k))then	
            cndtl_fact(i,j,k)=1.0
	    iflag_used(i,j,k)=1
	  else	
            cndtl_fact(i,j,k)=0.
	    iflag_used(i,j,k)=0
	  endif
	enddo
      enddo	
    enddo  
    
    return
  endif

! fill the ghost zone
!  call fill_ghost_zones_logical(cnd_flag,                                  &
!                        n_pad_x,n_pad_x,n_pad_y,n_pad_y,n_pad_z,n_pad_z,  &
!                        cnd_flag_padded) 

! fill points for non-periodic boundaries
! adjust back to first point inside domain
  
  if((periodic_x==0).and.(neighbor(1).lt.0))then
    do i=i_min_gz,0
      cnd_flag_padded(i,:,:)= cnd_flag_padded(1,:,:)
    enddo
  endif
  if((periodic_x==0).and.(neighbor(2).lt.0))then
    do i=nx+1,i_max_gz
      cnd_flag_padded(i,:,:)= cnd_flag_padded(nx,:,:)
    enddo
  endif  
  if((periodic_y==0).and.(neighbor(3).lt.0))then
    do j=j_min_gz,0
      cnd_flag_padded(:,j,:)= cnd_flag_padded(:,1,:)
    enddo
  endif
  if((periodic_y==0).and.(neighbor(4).lt.0))then
    do j=ny+1,j_max_gz
      cnd_flag_padded(:,j,:)= cnd_flag_padded(:,ny,:)
    enddo
  endif  
  if((periodic_z==0).and.(neighbor(5).lt.0))then
    do k=k_min_gz,0
      cnd_flag_padded(:,:,k)= cnd_flag_padded(:,:,1)
    enddo
  endif
  if((periodic_z==0).and.(neighbor(6).lt.0))then
    do k=nz+1,k_max_gz
      cnd_flag_padded(:,:,k)= cnd_flag_padded(:,:,nz)
    enddo
  endif

  if((iftype==0).and.(odd_delta))then
  
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
				
	  iflag_used(i,j,k)=count(cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	  
	  if(iflag_used(i,j,k).gt.0)then
	    cndtl_fact(i,j,k)=fact_norm_cndtl(iflag_used(i,j,k))
	  else
	    cndtl_fact(i,j,k)=0.0
	  endif
          				
        enddo
      enddo
    enddo
    
    
  elseif((iftype==0).and.(.not.odd_delta))then 
    
!   TEMPORARY - can be made faster by removing internal multiplications

    do k=1,nz 
      do j=1,ny 
        do i=1,nx
	 
	  
	  g_used = sum(gfilt(:,:,:), MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))		
				
!	  iflag_used(i,j,k)=count(cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	  iflag_used(i,j,k)=int(g_used)
	
	  if(g_used.gt.0)then
	    cndtl_fact(i,j,k)=1.0/g_used
	  else
	    cndtl_fact(i,j,k)=0.0
	    iflag_used(i,j,k)=0
	  endif

	      	
        enddo
      enddo
    enddo 
  
  else
    
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
	 
	  
	  g_used = sum(gfilt(:,:,:), MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))		
				
!	  iflag_used(i,j,k)=count(cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	  iflag_used(i,j,k)=int(g_used*real(nsgvol))
	  	
	  if(g_used.gt.0)then
	    cndtl_fact(i,j,k)=1.0/g_used
	  else
	    cndtl_fact(i,j,k)=0.0
	    iflag_used(i,j,k)=0
	  endif

	      	
        enddo
      enddo
    enddo  
  
  endif  

  initialized_cndtl_lesfilt=.true.

  return
end subroutine initialize_cndtl_lesfilt
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine filt_scalar_cndtl(field_dns,field_les)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! filters a scalar field using a cubic (or square) top hat filter
! -uses conditioning to exclude given set of points
!  WARNING : FIRST MUST CALL INITIALIZE_CNDTL_LESFILt for each new condition!!!!!
!
! -filter restricted to uniform grid
! -filtered field is reported at every DNS grid node
! -filter handles parallelism by creation of ghost zone
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid, gcomm, ierr, neighbor
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
!  use ghost_zones_m, only : fill_ghost_zones

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  real, dimension(nx,ny,nz), intent(in)  :: field_dns   ! dns field to be filtered

! output argument declarations

  real, dimension(nx,ny,nz), intent(out) :: field_les     ! filtered result

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(myid==0) write(*,*) 'error, sub not ready'
  call terminate_run(7,0)

! checks n_delta
  if(ndelta.le.1)then
!   no filtering

    do k=1,nz
      do j=1,ny
        do i=1,nx
	  if(cnd_flag_padded(i,j,k))then	
            field_les(i,j,k)=field_dns(i,j,k)
	  else	
            field_les(i,j,k)=0.
	  endif
	enddo
      enddo	
    enddo  
    
    return
  endif

! fill the ghost zones
!  call fill_ghost_zones(field_dns,                                        &
!                        n_pad_x,n_pad_x,n_pad_y,n_pad_y,n_pad_z,n_pad_z,  &
!                        field_padded)
			

! fill points for non-periodic boundaries
! adjust back to first point inside domain
  
  if((periodic_x==0).and.(neighbor(1).lt.0))then
    do i=i_min_gz,0
      field_padded(i,:,:)= field_padded(1,:,:)
    enddo
  endif
  if((periodic_x==0).and.(neighbor(2).lt.0))then
    do i=nx+1,i_max_gz
      field_padded(i,:,:)= field_padded(nx,:,:)
    enddo
  endif  
  if((periodic_y==0).and.(neighbor(3).lt.0))then
    do j=j_min_gz,0
      field_padded(:,j,:)= field_padded(:,1,:)
    enddo
  endif
  if((periodic_y==0).and.(neighbor(4).lt.0))then
    do j=ny+1,j_max_gz
      field_padded(:,j,:)= field_padded(:,ny,:)
    enddo
  endif  
  if((periodic_z==0).and.(neighbor(5).lt.0))then
    do k=k_min_gz,0
      field_padded(:,:,k)= field_padded(:,:,1)
    enddo
  endif
  if((periodic_z==0).and.(neighbor(6).lt.0))then
    do k=nz+1,k_max_gz
      field_padded(:,:,k)= field_padded(:,:,nz)
    enddo
  endif

  if((iftype==0).and.(odd_delta))then
  
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
          field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)), &
	                        MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))				
	  
	  field_les(i,j,k)=field_les(i,j,k)*cndtl_fact(i,j,k)
				
        enddo
      enddo
    enddo
    
    
  elseif((iftype==0).and.(.not.odd_delta))then 
    
!   TEMPORARY - can be made faster by removing internal multiplications

    do k=1,nz 
      do j=1,ny 
        do i=1,nx

	  field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k))  &
		                *gfilt(:,:,:),                                     &
	                        MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	  	
          field_les(i,j,k)=field_les(i,j,k)*cndtl_fact(i,j,k)
	
				 
        enddo
      enddo
    enddo 
 
  
  else
    
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
	 
	  field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k))  &
		                *gfilt(:,:,:),                                     &
	                        MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	  	
          field_les(i,j,k)=field_les(i,j,k)*cndtl_fact(i,j,k)
	      	
        enddo
      enddo
    enddo  
  
  endif  


  return
end subroutine filt_scalar_cndtl
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine filt_scalar_cndtl_old(field_dns,flag_in,field_les,iflag_used)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! filters a scalar field using a cubic (or square) top hat filter
!
! -filter restricted to uniform grid
! -filtered field is reported at every DNS grid node
! -filter handles parallelism by creation of ghost zone
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid, gcomm, ierr, neighbor
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
!  use ghost_zones_m, only : fill_ghost_zones, fill_ghost_zones_logical

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  real, dimension(nx,ny,nz), intent(in)  :: field_dns   ! dns field to be filtered
  logical, dimension(nx,ny,nz), intent(in)  :: flag_in  ! logical containing which
                                                        ! values to use in conditional
							! filtering (T=use)

! output argument declarations

  real, dimension(nx,ny,nz), intent(out) :: field_les     ! filtered result
  integer, dimension(nx,ny,nz), intent(out) :: iflag_used ! number of points used to obtain
                                                          ! result
							  ! =0 did not obtain
							  ! (in this case field_les set 0)

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k
  real :: g_used
 

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(myid==0) write(*,*) 'error, sub not ready'
  call terminate_run(7,0)

! checks n_delta
  if(ndelta.le.1)then
!   no filtering

    do k=1,nz
      do j=1,ny
        do i=1,nx
	  if(flag_in(i,j,k))then	
            field_les(i,j,k)=field_dns(i,j,k)
	    iflag_used(i,j,k)=1
	  else	
            field_les(i,j,k)=0.
	    iflag_used(i,j,k)=0
	  endif
	enddo
      enddo	
    enddo  
    
    return
  endif

! fill the ghost zones
!  call fill_ghost_zones(field_dns,                                        &
!                        n_pad_x,n_pad_x,n_pad_y,n_pad_y,n_pad_z,n_pad_z,  &
!                        field_padded)
			

!  call fill_ghost_zones_logical(flag_in,                                  &
!                        n_pad_x,n_pad_x,n_pad_y,n_pad_y,n_pad_z,n_pad_z,  &
!                        cnd_flag_padded) 

! fill points for non-periodic boundaries
! adjust back to first point inside domain
  
  if((periodic_x==0).and.(neighbor(1).lt.0))then
    do i=i_min_gz,0
      field_padded(i,:,:)= field_padded(1,:,:)
      cnd_flag_padded(i,:,:)= cnd_flag_padded(1,:,:)
    enddo
  endif
  if((periodic_x==0).and.(neighbor(2).lt.0))then
    do i=nx+1,i_max_gz
      field_padded(i,:,:)= field_padded(nx,:,:)
      cnd_flag_padded(i,:,:)= cnd_flag_padded(nx,:,:)
    enddo
  endif  
  if((periodic_y==0).and.(neighbor(3).lt.0))then
    do j=j_min_gz,0
      field_padded(:,j,:)= field_padded(:,1,:)
      cnd_flag_padded(:,j,:)= cnd_flag_padded(:,1,:)
    enddo
  endif
  if((periodic_y==0).and.(neighbor(4).lt.0))then
    do j=ny+1,j_max_gz
      field_padded(:,j,:)= field_padded(:,ny,:)
      cnd_flag_padded(:,j,:)= cnd_flag_padded(:,ny,:)
    enddo
  endif  
  if((periodic_z==0).and.(neighbor(5).lt.0))then
    do k=k_min_gz,0
      field_padded(:,:,k)= field_padded(:,:,1)
      cnd_flag_padded(:,:,k)= cnd_flag_padded(:,:,1)
    enddo
  endif
  if((periodic_z==0).and.(neighbor(6).lt.0))then
    do k=nz+1,k_max_gz
      field_padded(:,:,k)= field_padded(:,:,nz)
      cnd_flag_padded(:,:,k)= cnd_flag_padded(:,:,nz)
    enddo
  endif

  if((iftype==0).and.(odd_delta))then
  
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
          field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)), &
	                        MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
				
	  iflag_used(i,j,k)=count(cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	  
	  if(iflag_used(i,j,k).gt.0)then
	    field_les(i,j,k)=field_les(i,j,k)*fact_norm_cndtl(iflag_used(i,j,k))
	  else
	    field_les(i,j,k)=0.0
	  endif
          				
        enddo
      enddo
    enddo
    
    
  elseif((iftype==0).and.(.not.odd_delta))then 
    
!   TEMPORARY - can be made faster by removing internal multiplications

    do k=1,nz 
      do j=1,ny 
        do i=1,nx

	  field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k))  &
		                *gfilt(:,:,:),                                     &
	                        MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	  
	  g_used = sum(gfilt(:,:,:), MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))		
				
	  iflag_used(i,j,k)=int(g_used)
	
	  if(g_used.gt.0)then
	    field_les(i,j,k)=field_les(i,j,k)/g_used
	  else
	    field_les(i,j,k)=0.0
	  endif		
				 
        enddo
      enddo
    enddo 
  
  else
    
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
	 
	  field_les(i,j,k)= sum(field_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k))  &
		                *gfilt(:,:,:),                                     &
	                        MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	  
	  g_used = sum(gfilt(:,:,:), MASK=cnd_flag_padded(is(i):ie(i),js(j):je(j),ks(k):ke(k)))		
				
	  iflag_used(i,j,k)=count(flag_in(is(i):ie(i),js(j):je(j),ks(k):ke(k)))
	
	  if(g_used.gt.0)then
	    field_les(i,j,k)=field_les(i,j,k)/g_used
	  else
	    field_les(i,j,k)=0.0
	  endif

	      	
        enddo
      enddo
    enddo  
  
  endif  


  return
end subroutine filt_scalar_cndtl_old
!-----------------------------------------------------------------------------------------



!=========================================================================================
subroutine allocate_lesfilt_t(flag)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! initialises the lesfilt module
!----------------------------------------------------------------------------------------
! modules used
  use param_m, only : nx,ny,nz

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  integer, intent(in) :: flag    ! operation flag: =1 allocate =-1 deallocate

!-----------------------------------------------------------------------------------------
! local declarations

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(flag==1)then
  
    allocate(ist(nx));ist=0
    allocate(iet(nx));ist=0
    allocate(jst(ny));jst=0
    allocate(jet(ny));jst=0
    allocate(kst(nz));kst=0
    allocate(ket(nz));kst=0
    
    allocate(gfilt_t(-n_pad_xt:n_pad_xt,-n_pad_yt:n_pad_yt,-n_pad_zt:n_pad_zt));gfilt_t=0.  
    
!    allocate(field_padded_t(i_min_gzt:i_max_gzt,j_min_gzt:j_max_gzt,k_min_gzt:k_max_gzt))

    allocate(field_jk_sum_t(i_min_gzt:i_max_gzt));field_jk_sum_t=0.0  
        
  elseif(flag==-1)then
  
    deallocate(ist)
    deallocate(iet)
    deallocate(jst)
    deallocate(jet)
    deallocate(kst)
    deallocate(ket)
    
    deallocate(gfilt_t)
    
!    deallocate(field_padded_t)   
     
    deallocate(field_jk_sum_t)   
    
    initialized_test_lesfilt=.false.
    
  endif


  return
end subroutine allocate_lesfilt_t
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine initialize_lesfilt_t(io)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! initialises the lesfilt module
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
  use grid_m, only : delx,dely,delz

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  integer, intent(in) :: io    ! io unit 

! output argument declarations

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k
  
  real :: pi
  real :: argx,argy,argz

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------
! message
  if(myid==0)then
    write(io,*) 'Initialising test filter module'
    write(io,*)
    call write_header(io,'-')
  endif


! set delta
! assuming uniform cubic grid
  deltat=ndeltat*delx

! for box filter - check if even or odd
  if(iftype==0)then
  
    if(mod(ndeltat,2).eq.0)then
      ! filter is even
      odd_deltat =.false.

      ! (re-)set filter calculation half-width
      nfwidtht=ndeltat+1	
      
    else
      !filter is odd
      odd_deltat =.true.
      
      ! (re-)set filter calculation half-width
      nfwidtht=ndeltat     
       
    endif ! check of even delta
    
  endif  !check of box filter

 
! set ghost zone size
  n_hwt=(nfwidtht-1)/2
 
  if(vary_in_x==1)then
    n_pad_xt=n_hwt
  else
    n_pad_xt=0
  endif
  if(vary_in_y==1)then
    n_pad_yt=n_hwt
  else
    n_pad_yt=0
  endif
  if(vary_in_z==1)then
    n_pad_zt=n_hwt
  else
    n_pad_zt=0
  endif 

! set ghostzone limits
  i_min_gzt=1-n_pad_xt
  i_max_gzt=nx+n_pad_xt
  
  j_min_gzt=1-n_pad_yt
  j_max_gzt=ny+n_pad_yt 
  
  k_min_gzt=1-n_pad_zt
  k_max_gzt=nz+n_pad_zt 
  
! sub-grid cell volume
  nsgvolt=ndeltat**numdim  
  
! allocate as required
  call allocate_lesfilt_t(1)  
  
! normalising factor for box and gaussian filter

  fact_normt=1.0/real(nsgvolt)
 
! set up start and end indices to avoid recalculation
  do i=1,nx 
    ist(i)=i-n_pad_xt
    iet(i)=i+n_pad_xt
  enddo
  do j=1,ny 
    jst(j)=j-n_pad_yt
    jet(j)=j+n_pad_yt
  enddo
  do k=1,nz 
    kst(k)=k-n_pad_zt
    ket(k)=k+n_pad_zt
  enddo
 
! set filter function
  Filtertype : select case(iftype)
  
    case(0)
    ! filter is a box filter
    ! note : function is only used for edge nodes in case of even ndelta
    gfilt(:,:,:)=1.0

    if(.not.odd_deltat)then
      if(vary_in_x==1)then
        gfilt_t(-n_pad_xt,:,:)=gfilt_t(-n_pad_xt,:,:)*0.5
        gfilt_t( n_pad_xt,:,:)=gfilt_t( n_pad_xt,:,:)*0.5
      endif
      if(vary_in_y==1)then
        gfilt_t(:,-n_pad_yt,:)=gfilt_t(:,-n_pad_yt,:)*0.5
        gfilt_t(:, n_pad_yt,:)=gfilt_t(:, n_pad_yt,:)*0.5
      endif
      if(vary_in_z==1)then
        gfilt_t(:,:,-n_pad_zt)=gfilt_t(:,:,-n_pad_zt)*0.5
        gfilt_t(:,:, n_pad_zt)=gfilt_t(:,:, n_pad_zt)*0.5
      endif
    endif
    
    if(myid==0)then
      write(io,*) 'filter is box, delta =', ndeltat
    endif
      
    case(1)
    ! filter is gaussian
   
    pi=4.0*atan(1.0)
    do i=-n_pad_xt,n_pad_xt   
      do j=-n_pad_yt,n_pad_yt  
	do k=-n_pad_zt,n_pad_zt  
          argx=real(i)/real(ndeltat)
          argx=-6.0*argx*argx
          argy=real(j)/real(ndeltat)
          argy=-6.0*argy*argy
          argz=real(k)/real(ndeltat)
          argz=-6.0*argz*argz
          gfilt_t(i,j,k)=(6.0/pi/deltat/deltat)**(real(numdim)/2.0)*exp(argx+argy+argz) &
	               *delx*dely*delz
        enddo
      enddo
    enddo
      
    if(myid==0)then
      write(io,*) 'filter is gaussian, delta =', ndeltat
    endif
    
  end select Filtertype

! set initialization flag
  initialized_test_lesfilt =.true.

! header
  if(myid==0)then
    call write_header(io,'-')
  endif

  return
end subroutine initialize_lesfilt_t
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine filt_scalar_test(field_dns,field_les)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! filters a scalar field using a cubic (or square) top hat filter
!
! -filter restricted to uniform grid
! -filtered field is reported at every DNS grid node
! -filter handles parallelism by creation of ghost zone
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid, gcomm, ierr, neighbor
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
!  use ghost_zones_m, only : fill_ghost_zones
  use ghost_nice_m, only : ghostzone_real

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  real, dimension(nx,ny,nz), intent(in)  :: field_dns   ! dns field to be filtered

! output argument declarations

  real, dimension(nx,ny,nz), intent(out) :: field_les   ! filtered result

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k, iflt
  
  real, dimension(i_min_gzt:i_max_gzt,j_min_gzt:j_max_gzt,k_min_gzt:k_max_gzt) :: field_pad_t
  real, dimension(i_min_gzt:i_max_gzt,j_min_gzt:j_max_gzt,k_min_gzt:k_max_gzt) :: tmp

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

!  if(myid==0) write(*,*) 'error, sub not ready'
!  call terminate_run(7,0)

! checks n_delta
  if(ndeltat.le.1)then
!   no filtering
    field_les(:,:,:)=field_dns(:,:,:)
    return
  endif
  
! fill the ghost zone
!  call fill_ghost_zones(field_dns,                                        &
!                        n_pad_xt,n_pad_xt,n_pad_yt,n_pad_yt,n_pad_zt,n_pad_zt,  &
!                        field_padded_t) 

  field_pad_t(1:nx,1:ny,1:nz)=field_dns(:,:,:)

  call ghostzone_real(field_pad_t, nx, ny, nz, n_pad_xt, n_pad_xt, &
                                                n_pad_yt, n_pad_yt, &
                                                n_pad_zt, n_pad_zt)

  
! fill points for non-periodic boundaries
! adjust back to first point inside domain
  
!  if((periodic_x==0).and.(neighbor(1).lt.0))then
!    do i=i_min_gzt,0
!      field_pad_t(i,:,:)= field_pad_t(1,:,:)
!    enddo
!  endif
!  if((periodic_x==0).and.(neighbor(2).lt.0))then
!    do i=nx+1,i_max_gzt
!      field_pad_t(i,:,:)= field_pad_t(nx,:,:)
!    enddo
!  endif  
!  if((periodic_y==0).and.(neighbor(3).lt.0))then
!    do j=j_min_gzt,0
!      field_pad_t(:,j,:)= field_pad_t(:,1,:)
!    enddo
!  endif
!  if((periodic_y==0).and.(neighbor(4).lt.0))then
!    do j=ny+1,j_max_gzt
!      field_pad_t(:,j,:)= field_pad_t(:,ny,:)
!    enddo
!  endif  
!  if((periodic_z==0).and.(neighbor(5).lt.0))then
!    do k=k_min_gzt,0
!      field_pad_t(:,:,k)= field_pad_t(:,:,1)
!    enddo
!  endif
!  if((periodic_z==0).and.(neighbor(6).lt.0))then
!    do k=nz+1,k_max_gzt
!      field_pad_t(:,:,k)= field_pad_t(:,:,nz)
!    enddo
!  endif
  
  if((iftype==0).and.(odd_deltat))then
  
!    do k=1,nz 
!      do j=1,ny 
!        do i=1,nx
!          field_les(i,j,k)= sum(field_padded_t(ist(i):iet(i),jst(j):jet(j),kst(k):ket(k)))
!        enddo
!      enddo
!    enddo

!!   fast box filtering - march in x-direction adding and subtracting planes
!    do k=1,nz 
!      do j=1,ny
!
!!       do the plane sums 
!        do iflt=ist(1),iet(nx)
!          field_jk_sum_t(iflt)= sum(field_pad_t(iflt,jst(j):jet(j),kst(k):ket(k)))
!        enddo
!
!!       i=1
!        field_les(1,j,k) = sum(field_jk_sum_t(ist(1):iet(1)))
!
!!       step in i
!        do i=2,nx
!          field_les(i,j,k) = field_les(im1(i),j,k)                         &
!                           + field_jk_sum_t(iet(i))-field_jk_sum_t(ist(im1(i)))
!        enddo
!
!      enddo
!    enddo

!   fast box filtering

!   x direction
    do k=kst(1),ket(nz)
      do j=jst(1),jet(ny)
        do i=1,nx
          tmp(i,j,k)=sum(field_pad_t(ist(i):iet(i),j,k))
        enddo
      enddo
    enddo

!   y direction
    do k=kst(1),ket(nz)
      do i=1,nx
        do j=1,ny
          field_pad_t(i,j,k)=sum(tmp(i,jst(j):jet(j),k))
        enddo
      enddo
    enddo

!   z direction
    do j=1,ny
      do i=1,nx
        do k=1,nz
          field_les(i,j,k)=sum(field_pad_t(i,j,kst(k):ket(k)))
        enddo
      enddo
    enddo

    
    ! normalise
    field_les(:,:,:)=field_les(:,:,:)*fact_normt
    
  elseif((iftype==0).and.(.not.odd_deltat))then 
    
!   TEMPORARY - can be made faster by removing internal multiplications

    do k=1,nz 
      do j=1,ny 
        do i=1,nx
	
	  field_les(i,j,k)= sum(field_pad_t(ist(i):iet(i),jst(j):jet(j),kst(k):ket(k)) &
		       *gfilt_t(:,:,:))
				 
        enddo
      enddo
    enddo 
    
    ! normalise
    field_les(:,:,:)=field_les(:,:,:)*fact_normt
  
  else
    
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
          field_les(i,j,k)= sum(field_pad_t(ist(i):iet(i),jst(j):jet(j),kst(k):ket(k)) &
		                      *gfilt_t(:,:,:))	      	
        enddo
      enddo
    enddo  
  
  endif  


  return
end subroutine filt_scalar_test
!-----------------------------------------------------------------------------------------

!=========================================================================================
subroutine filt_scalar_test_old(field_dns,field_les)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! filters a scalar field using a cubic (or square) top hat filter
!
! -filter restricted to uniform grid
! -filtered field is reported at every DNS grid node
! -filter handles parallelism by creation of ghost zone
!----------------------------------------------------------------------------------------
! modules used
  use topology_m, only : myid, gcomm, ierr, neighbor
  use param_m, only : nx,ny,nz, numdim, &
                      vary_in_x,vary_in_y,vary_in_z, &
                      periodic_x,periodic_y,periodic_z
!  use ghost_zones_m, only : fill_ghost_zones

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  real, dimension(nx,ny,nz), intent(in)  :: field_dns   ! dns field to be filtered

! output argument declarations

  real, dimension(nx,ny,nz), intent(out) :: field_les   ! filtered result

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(myid==0) write(*,*) 'error, sub not ready'
  call terminate_run(7,0)

! checks n_delta
  if(ndeltat.le.1)then
!   no filtering
    field_les(:,:,:)=field_dns(:,:,:)
    return
  endif
  
! fill the ghost zone
!  call fill_ghost_zones(field_dns,                                        &
!                        n_pad_xt,n_pad_xt,n_pad_yt,n_pad_yt,n_pad_zt,n_pad_zt,  &
!                        field_padded_t) 
  
! fill points for non-periodic boundaries
! adjust back to first point inside domain
  
  if((periodic_x==0).and.(neighbor(1).lt.0))then
    do i=i_min_gzt,0
      field_padded_t(i,:,:)= field_padded_t(1,:,:)
    enddo
  endif
  if((periodic_x==0).and.(neighbor(2).lt.0))then
    do i=nx+1,i_max_gzt
      field_padded_t(i,:,:)= field_padded_t(nx,:,:)
    enddo
  endif  
  if((periodic_y==0).and.(neighbor(3).lt.0))then
    do j=j_min_gzt,0
      field_padded_t(:,j,:)= field_padded_t(:,1,:)
    enddo
  endif
  if((periodic_y==0).and.(neighbor(4).lt.0))then
    do j=ny+1,j_max_gzt
      field_padded_t(:,j,:)= field_padded_t(:,ny,:)
    enddo
  endif  
  if((periodic_z==0).and.(neighbor(5).lt.0))then
    do k=k_min_gzt,0
      field_padded_t(:,:,k)= field_padded_t(:,:,1)
    enddo
  endif
  if((periodic_z==0).and.(neighbor(6).lt.0))then
    do k=nz+1,k_max_gzt
      field_padded_t(:,:,k)= field_padded_t(:,:,nz)
    enddo
  endif
  
  if((iftype==0).and.(odd_deltat))then
  
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
          field_les(i,j,k)= sum(field_padded_t(ist(i):iet(i),jst(j):jet(j),kst(k):ket(k)))
        enddo
      enddo
    enddo
    
    ! normalise
    field_les(:,:,:)=field_les(:,:,:)*fact_normt
    
  elseif((iftype==0).and.(.not.odd_deltat))then 
    
!   TEMPORARY - can be made faster by removing internal multiplications

    do k=1,nz 
      do j=1,ny 
        do i=1,nx
	
	  field_les(i,j,k)= sum(field_padded(ist(i):iet(i),jst(j):jet(j),kst(k):ket(k)) &
		       *gfilt_t(:,:,:))
				 
        enddo
      enddo
    enddo 
    
    ! normalise
    field_les(:,:,:)=field_les(:,:,:)*fact_normt
  
  else
    
    do k=1,nz 
      do j=1,ny 
        do i=1,nx
          field_les(i,j,k)= sum(field_padded(ist(i):iet(i),jst(j):jet(j),kst(k):ket(k)) &
		                      *gfilt_t(:,:,:))	      	
        enddo
      enddo
    enddo  
  
  endif  


  return
end subroutine filt_scalar_test_old
!-----------------------------------------------------------------------------------------



!-----------------------------------------------------------------------------------------
  end module lesfilt_m

!========================================================================================
  subroutine initialize_bomb(io,yspecies,temp,pressure,u)
!========================================================================================
! initializes sinusoidal profile for auto-ignition study
!----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g
  use param_m, only : n_spec, numdim, vary_in_x, vary_in_y, vary_in_z
  use reference_m
  use runtime_m, only : i_restart
  
  use mixfrac_m

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io
  real yspecies(nx,ny,nz,n_spec), temp(nx,ny,nz), pressure(nx,ny,nz), u(nx,ny,nz,3)
!----------------------------------------------------------------------------------------
! local declarations

  integer i, j, k, i_g, j_g, k_g  !counters
  
  integer :: io_bomb = 20  !io unit
  character*100 :: filename 
  
  real :: p_mean      ! initial mean pressure
  real :: temp_mean   ! initial mean temperature
  real :: temp_rms    ! initial rms temperature
  real :: phi_mean    ! initial phi

  real :: twopi, roottwo  !constants
  
  real :: t_funx(nx),t_funy(ny),t_funz(nz) !functional variations of temperature

!----------------------------------------------------------------------------------------
! return if restarting

  if(i_restart==1) return
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing bomb (stand back)'
    write(io,*)
  endif

!-----------------------------------------------------------------------------------------
! read the control file
!-----------------------------------------------------------------------------------------
  filename='../input/bomb.in'
  call inquire_about_input_file(filename,io)
!-----------------------------------------------------------------------------------------
! myid==0 only opens and reads file
  if(myid.eq.0) then
!-----------------------------------------------------------------------------------------
!   open file

    open(unit=io_bomb,file=trim(filename),status='old')
!-----------------------------------------------------------------------------------------
!   read header
    read(io_bomb,*)

!   read mean initial pressure  (atm)
    read(io_bomb,*) p_mean

!   read mean initial temperature  (K)
    read(io_bomb,*) temp_mean

!   read temperature rms (K)
    read(io_bomb,*) temp_rms
    
!   read phi (nondimensional)
    read(io_bomb,*) phi_mean

!   close file
    close(io_bomb)

  endif
  
!-----------------------------------------------------------------------------------------
! broadcast data
  call MPI_Bcast(p_mean,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(temp_mean,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(temp_rms,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(phi_mean,1,MPI_REAL8,0,gcomm,ierr)

! nondimensionalise
  p_mean=p_mean/p_ref*pres_atm           !from atm
  temp_mean=temp_mean/t_ref              !from K
  temp_rms=temp_rms/t_ref                !from K
  
!-----------------------------------------------------------------------------------------
! set initial temperature field

! here - multiplication of sinusoids
  
  twopi=8.0*atan(1.0)
  roottwo=sqrt(2.0)
  
  t_funx=1.0
  t_funy=1.0
  t_funz=1.0
  
  if(vary_in_x==1)then
    do i=1,nx
      i_g = i + xid*nx
      t_funx(i) = 0.5*(1.0-cos(twopi*i_g/(nx_g+1)))
    enddo  
  endif     
  
  if(vary_in_y==1)then  
    do j=1,ny
      j_g = j + yid*ny
      t_funy(j) = 0.5*(1.0-cos(twopi*j_g/(ny_g+1)))
    enddo      
  endif
    
  if(vary_in_z==1)then  
    do k=1,nz
      k_g = k + zid*nz
      t_funz(k) = 0.5*(1.0-cos(twopi*k_g/(nz_g+1)))
    enddo      
  endif
  
  do k=1,nz
    do j=1,ny
      do i=1,nx
        temp(i,j,k)=t_funx(i)*t_funy(j)*t_funz(k)
      enddo
    enddo
  enddo
  
  if(numdim==0)then
    temp(:,:,:) = temp_mean
  else  
    temp(:,:,:) = temp_mean + (temp(:,:,:)-0.5)*temp_rms*2.0*roottwo
  endif  
       
!-----------------------------------------------------------------------------------------
! set initial composition field

! evatt's mixfrac
!  call allocate_MixFrac_arrays( 1 )
!
!  call calc_mixfrac_coeffs
!  
!  phi(:,:,:) = phi_mean
!
!  call PhiToMixFrac
!  
!  call mixfrToSpec( yspecies(:,:,:,:) )
!
!  call allocate_MixFrac_arrays( -1 )

!------------------------------------------------------------------------------
! james mixfrac
! set mixture parameters
  call allocate_mixFrac_arrays(1)       ! set up the mixture fraction module
  call getStoichMixfr                   ! initialize stoichiometric mixture fraction

  phi(:,:,:)=phi_mean

!------------------------------------------------------------------------------
! set species
  call phiToMixfrac( phi, mixfrac )     ! convert equivalence ratio to mixture fraction
  call mixfrToSpec( yspecies )          ! convert mixture fraction to species mass fractions

  call allocate_MixFrac_arrays( -1 )
  

!--------------------------------------------------------------------------------
! set initial mean pressure

  pressure(:,:,:) = p_mean

!-----------------------------------------------------------------------------------------
! set initial velocities to zero

  u(:,:,:,:) = 0.0

!-----------------------------------------------------------------------------------------
  return
  end subroutine initialize_bomb

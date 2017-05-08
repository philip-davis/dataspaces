#include "globalDefines.h"
! $Id: turbulence_m.f90,v 1.2.2.6 2006/05/06 23:36:07 rsankar Exp $
!=========================================================================================
! VERSION 1.01
!    To do list:
!                 provide tools for checking the output (spectra, length scales)
!
! WARNING: This whole module assumes a uniform mesh.
!
!
!    Revision info:
!      NOV-2004:  Evatt Hawkes - adding Cray X1 compatibility
!                 preprocessor flag X1
!
!      AUG-2004:  Evatt Hawkes - minor bug fixed - calls to dconj and dreal
!                 were not portable
!
!      JUN-2004:  Evatt Hawkes (erhawke@ca.sandia.gov)
!                 generalising vkp to 3D and non-equal lengths and nodes of domain
!                 allowing switch for spectrum options of vkp and passot-pouquet
!                 adding some further turbulence scales information
! 
!      JULY-2003: Evatt Hawkes (erhawke@ca.sandia.gov)
!                 added comments and warnings
!                 fixed 3d turbulence generation
!                 main changes:
!                   1 corrected periodicity such that 1st node equals (n+1)th, not nth
!                   2 added missed symmetry conditon for j-direction when i,k=1
!                   3 corrected random number generation to employ the seed correctly
!                   4 corrected setting of nw such that nw is set to the max possible value
!                     for safety (this could be returned to an input parameter, but 
!                     requires some user understanding)
!                   5 removed erroneous call to vkp for 3d turbulence - vkp is for 2d
!                   6 added lines to set ke and up, required for the passot-pouquet spectra
!                   7 added a check of the spectral and physical energy content
!
!                 NOTE!!!! Release 1.01 had nw and nh defined in both param_m and here
!                          You need to remove them from param_m if it is still there!
!
!      (10-8-02)  Made by James Sutherland (sutherland@crsim.utah.edu)
!      Performed an overhaul of this module.
!      The velocity array is now passed into initialization routines.
!      Earlier versions passed an entire register of the solution vector
!      and did a lot of juggling between primitive and conserved variables in
!      the q vector.  That was awkward and confusing...
!
! VERSION 1.00
!=========================================================================================
  module turbulence_m
!=========================================================================================
! module for turbulence variables
! WARNING: may not work for non-uniform grids.

  implicit none
!-----------------------------------------------------------------------------------------
! integers

  integer i_turbulence    !switch for turbulence
  integer seed            !input seed for turbulence

! additions by Evatt Hawkes July 2003
  integer seed_size                    ! size of seed array for random_number call
  integer, allocatable :: new_seed(:)  ! seed array for intrinsic random_number call

  integer :: nw,nh      ! number of waves in turbulence
                        ! these dimension the velocity field in fourier space
                        ! nh = nw/2 +1 to save memory
                        ! since the velocity is real, the fourier field is symmetric
                        ! so only half the field need be stored
                        
! reals

  real df       !characteristic flame length scale (m)
  real sl       !characteristic flame velocity scale (m/s)
  real ltdf     !ratio of LT to flame length scale
  real upsl     !ratio of rms velocity fluctuations to flame velocity scale

  real up       !rms velocity (non-dimensional) defined as (u_ii)/numdim
  real dissip   !dissipation (non-dimensional) defined as up**3/lt

  real ke       !spectrum wave number defined as 2*pi/le
  real kd       !spectrum wave number defined as 2*pi/ld (only for vkp)

  real C_spect  !multuplier for the spectrum - depends on spectrum and dimensionality

  integer :: ispect = 0 !spectrum type = 0 vkp =1 passot-pouquet

!-----------------------------------------------------------------------------------------
  contains
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! superimpose isotropic turbulence onto the flow field
  subroutine init2_turb(io,q,u_prime)
  use topology_m
  use param_m, only : nx, ny, nz, nvar_tot, n_reg
  use variables_m, only : u
  use work_m, only : kinetic_E => work1_1     ! alias kinetic_E to this work array to save storage

!----------------------------------------------------------------------
  implicit none
!----------------------------------------------------------------------
! declarations passed in

  integer :: io
  real q(nx,ny,nz,nvar_tot,n_reg)             ! solution vector
  real, dimension(nx,ny,nz,3) :: u_prime

! local declarations
  integer :: L

! post processing checks
!  call calculate_spectra(u_prime,io)

!----------------------------------------------------------------------
! extract original velocity field

  do L=1,3
    u(:,:,:,L) = q(:,:,:,L,1)/q(:,:,:,4,1)
  enddo
!----------------------------------------------------------------------
! superpose the isotropic turbulent flow field on to mean field

! update the momentum equations
  do L=1,3
    q(:,:,:,L,1) = q(:,:,:,L,1) + u_prime(:,:,:,L) * q(:,:,:,4,1)
  enddo

! exctract kinetic energy of the mean field from the energy term
  kinetic_E = 0.5*( u(:,:,:,1)**2 + u(:,:,:,2)**2 + u(:,:,:,3)**2 )*q(:,:,:,4,1)
  q(:,:,:,5,1) = q(:,:,:,5,1) - kinetic_E

  kinetic_E = 0.5*( (u(:,:,:,1)+u_prime(:,:,:,1))**2 +  &
                    (u(:,:,:,2)+u_prime(:,:,:,2))**2 +  &
                    (u(:,:,:,3)+u_prime(:,:,:,3))**2 )*q(:,:,:,4,1)

! update total energy by adding in the kinetic energy contribution from the turbulence
  q(:,:,:,5,1) = q(:,:,:,5,1) + kinetic_E
!----------------------------------------------------------------------
! check the spectrum

! extract velocity field
  do L=1,3
    u(:,:,:,L) = q(:,:,:,L,1)/q(:,:,:,4,1)
  enddo

! Deallocated here, but allocated somewhere else!!
! Adding an if check to stop it from crashing. 
  if(allocated(new_seed)) deallocate( new_seed )

!----------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------
  return
  end subroutine init2_turb

!========================================================================================
  subroutine initialize_turbulence(io,q,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
!========================================================================================
! superimpose isotropic turbulence onto the flow field
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use param_m, only : nx, ny, nz, numdim, nvar_tot, n_reg, nx_g, ny_g, nz_g
  use reference_m, only : a_ref, l_ref
  use bc_m, only : nrf_x0, nrf_xl, nrf_y0, nrf_yl, nrf_z0, nrf_zl
  use variables_m, only : u
  use work_m, only : kinetic_E => work1_1     ! alias kinetic_E to this work array to save storage

  use work_m, only : work1_2
!-----------------------------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer :: io
  real q(nx,ny,nz,nvar_tot,n_reg)             ! solution vector
  real xmin, xmax, ymin, ymax, zmin, zmax     ! grid min and max locations 
                                              ! WARNING: some parts assume unifrom grid
                                              ! despite this argument
  real x(nx)                                  
  real re

! local declarations

  integer :: L,i,j,k
  real, dimension(nx,ny,nz,3) :: u_prime

!----------------------------------------------------------------------------------------
! set up the turbulence parameters and set the field
  call setup_turbulence(io,u_prime,xmin,xmax,ymin,ymax,zmin,zmax,x,re)

! now filter the field so it does not overlap the flame
!  call filter_u(u_prime)

! post processing checks
!  call calculate_spectra(u_prime,io)

!-----------------------------------------------------------------
! extract original velocity field

  do L=1,3
    u(:,:,:,L) = q(:,:,:,L,1)/q(:,:,:,4,1)
  enddo
!--------------------------------------------------------------------------------------
! superpose the isotropic turbulent flow field on to mean field

! update the momentum equations
  do L=1,3
    q(:,:,:,L,1) = q(:,:,:,L,1) + u_prime(:,:,:,L) * q(:,:,:,4,1)
  enddo

! exctract kinetic energy of the mean field from the energy term
  kinetic_E = 0.5*( u(:,:,:,1)**2 + u(:,:,:,2)**2 + u(:,:,:,3)**2 )*q(:,:,:,4,1)
  q(:,:,:,5,1) = q(:,:,:,5,1) - kinetic_E

  kinetic_E = 0.5*( (u(:,:,:,1)+u_prime(:,:,:,1))**2 +  &
                    (u(:,:,:,2)+u_prime(:,:,:,2))**2 +  &
                    (u(:,:,:,3)+u_prime(:,:,:,3))**2 )*q(:,:,:,4,1)

! update total energy by adding in the kinetic energy contribution from the turbulence
  q(:,:,:,5,1) = q(:,:,:,5,1) + kinetic_E
!----------------------------------------------------------------------------------------
! check the spectrum

! extract velocity field
  do L=1,3
    u(:,:,:,L) = q(:,:,:,L,1)/q(:,:,:,4,1)
  enddo

! deallocate temporaries
  deallocate( new_seed )

!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!-----------------------------------------------------------------------------------------     
  return
  end subroutine initialize_turbulence


!========================================================================================
  subroutine setup_turbulence(io,u_prime,xmin,xmax,ymin,ymax,zmin,zmax,x,re)
!========================================================================================
! read the input file, calculate turbulence parameters, and calculate the
! turbulence field
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use param_m, only : nx, ny, nz, numdim, nvar_tot, n_reg, nx_g, ny_g, nz_g
  use reference_m, only : a_ref, l_ref
  use bc_m, only : nrf_x0, nrf_xl, nrf_y0, nrf_yl, nrf_z0, nrf_zl


!-----------------------------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer, intent(in) :: io                   ! io unit
  real   , intent(in) :: xmin, xmax, ymin, ymax, zmin, zmax     
                                              ! grid min and max locations 
                                              ! WARNING: some parts assume unifrom grid
                                              ! despite this argument

  real, intent(in) :: x(nx)                   ! grid (see WARNING above)               
  real, intent(in) :: re                      ! non-dimensionaliation Re                 

  real, intent(out), dimension(nx,ny,nz,3) :: u_prime      ! turbulence field

!-----------------------------------------------------------------------------------------
! local declarations

!  for debug
!  real :: up_mean

!----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

! write header

  if(myid==0) then
    write(io,*) 'initializing turbulence field...'
    write(io,*)
  endif

!-- set number of waves (nw) and nh

!  if (numdim == 3) then
!    nw=int(min(nx_g,ny_g,nz_g)/2)-1
!  elseif(numdim == 2) then
!    if( vary_in_x==1 .and. vary_in_y==1 ) then
!      nw = int(min(nx_g,ny_g)/2)-1
!    elseif (vary_in_x==1 .and. vary_in_z==1) then
!      nw = int(min(nx_g,nz_g)/2)-1
!    elseif (vary_in_y==1 .and. vary_in_z==1) then
!      nw = int(min(ny_g,nz_g)/2)-1
!    endif
!  endif
!  nh = 1 + nw / 2

! Evatt Hawkes has altered this from the above July 2003
! think this is now correct for the periodicity case where the (n+1)th is the 1st node
! nh is the number of fourier modes allowed in the domain
! nw is the number of fourier coefficients  
! in the original incarnation of s3d, nw was read as an input parameter
! i think that the only restriction on nw is that it be less than the active minimum grid dim.
! hence nw can be set to a number less than it is here to save memory and computation
! here i have set it to the maximum allowable size for safety, otherwise
! the spectrum can be truncated.
! nh = 1+nw/2 because of the symmetry condition to save memory

  if (numdim == 3) then
    nw=min(nx_g,ny_g,nz_g)
  elseif(numdim == 2) then
    if( vary_in_x==1 .and. vary_in_y==1 ) then
      nw = min(nx_g,ny_g)
    elseif (vary_in_x==1 .and. vary_in_z==1) then
      nw = min(nx_g,nz_g)
    elseif (vary_in_y==1 .and. vary_in_z==1) then
      nw = min(ny_g,nz_g)
    endif
  endif
  nh = 1 + nw / 2
!-----------------------------------------------------------------------------------------
! read input file and set-up params
! Evatt Hawkes JUN 2004

  call get_turbulence_params(re,io,1)

!-----------------------------------------------------------------------------------------
! initialize seed for intrinsic random number call
! changed by Evatt Hawkes Jul 2003

  call random_seed(size=seed_size)
  allocate(new_seed(seed_size)); new_seed(:)=seed 
  call random_seed(put=new_seed)

!-----------------------------------------------------------------------------------------

! initialize turbulence
  u_prime = 0.0
  if(numdim.eq.3) then
    call turb_3d(xmin,xmax,ymin,ymax,zmin,zmax,x,seed,u_prime,io)
  elseif(numdim.eq.2) then
    call rndini2d(xmin,xmax,ymin,ymax,nx,ny,nz,x,seed,u_prime,io)
  endif

  return
  end subroutine setup_turbulence

!========================================================================================
  subroutine get_turbulence_params(re,io,iscrndmp)
!========================================================================================
! reads the input file
! sets spectrum and needed turbulence parameters
! dumps some information to the screen
!-----------------------------------------------------------------------------------------
  use topology_m
  use reference_m, only : a_ref, l_ref, time_ref
  use grid_m, only : xmin,xmax,ymin,ymax,zmin,zmax,delx,dely,delz
  use param_m, only : vary_in_x,vary_in_y,vary_in_z, numdim
  implicit none
!-----------------------------------------------------------------------------------------
! passed arg
  real, intent(in) :: re           !nondim reynolds number 
  integer, intent(in) :: io        !main io unit  
  integer, intent(in) :: iscrndmp  !whether or not to write turbulence info
!----------------------------------------------------------------------------------------- 
! local declarations
  character*100 :: filename
  character*3 :: spectrum_type
  logical :: exist

  real :: pi
  real :: lt     ! integral scale = up^3/eps
  real :: l11    ! autocorrelation integral scale
  real :: l11df  ! l11/df
  real :: eta_k  ! kolmogorov scale
  real :: re_t   ! turbulence Reynolds number
  real :: le, ld ! energy containing and dissipation length scales

!-----------------------------------------------------------------------------------------
! executable statements
!----------------------------------------------------------------------------------------- 
 
! set filename in inquire

  filename = '../input/turbulence.in'
  call inquire_about_input_file(filename,io)

! read and broadcast the turbulence parameters from the turbulence file

  if(myid == 0) then
    open( unit=20, file=filename, status='old', form='formatted' )
    read(20,*) df
    read(20,*) sl
    read(20,*) ltdf
    read(20,*) upsl
    read(20,*) seed
    read(20,*) spectrum_type
    close(20)
  endif

  call MPI_Bcast( df,   1, MPI_REAL8,   0, gcomm, ierr )  
  call MPI_Bcast( sl,   1, MPI_REAL8,   0, gcomm, ierr )  
  call MPI_Bcast( ltdf, 1, MPI_REAL8,   0, gcomm, ierr )  
  call MPI_Bcast( upsl, 1, MPI_REAL8,   0, gcomm, ierr )  
  call MPI_Bcast( seed, 1, MPI_INTEGER, 0, gcomm, ierr )  
  call MPI_Bcast( spectrum_type,3,MPI_CHARACTER, 0, gcomm, ierr )

!-----------------------------------------------------------------------------------------
! non-dimensionalize ignition velocity and length scales

  sl=sl/a_ref
  df=df/l_ref

!-----------------------------------------------------------------------------------------
! check spectrum type and calculate spectrum params
  select case (trim(spectrum_type))
  case ('vkp','VKP')
    if(myid==0)then 
      write(io,*) 'Turbulence Spectrum is Von Karman - Pao'
!      call vkp( ke, le, kd, ld, upsl, ltdf, re, sl, df, l11df, io)
    endif
    ispect = 0
  case ('pp','PP')
    if(myid==0)then
      write(io,*) 'Turbulence Spectrum is Passot-Pouquet'
      call calc_passot_pouquet_params(ke,le,upsl,ltdf,re,sl,df,l11df)
    endif
    ispect = 1
  case default
    if(myid==0) then
      write(io,*) 'unknown spectrum type: ', trim(spectrum_type)
      write(io,*) 'TERMINATING!'
     endif
     call terminate_run(io,0)  !must be called by all processors
  end select

!-----------------------------------------------------------------------------------------
! Broadcast the data needed
  call MPI_Bcast( ke,    1, MPI_REAL8, 0, gcomm, ierr)
  call MPI_Bcast( kd,    1, MPI_REAL8, 0, gcomm, ierr)
  
!-----------------------------------------------------------------------------------------
! screen dump of turbulence parameters
! Evatt Hawkes JUN 2004
! write turbulence parameters to io

! all processes can set these

! up
  up = upsl*sl

! integral scale
  lt = ltdf*df

! dissipation rate
  dissip = up**3/lt

! only myid == 0 does it - then broadcast later stuff that is needed
  if(myid==0.and.iscrndmp==1)then

! l11 integral scales
  l11 = l11df*df

! reynolds number
  re_t = up*lt*re

! kolmogorov scale
  eta_k = (re_t)**(-0.75)*ltdf*df

  write(io,*) 
  write(io,*) 'BASED ON YOUR REFERENCE VISCOSITY, the '
  write(io,*) 'initial turbulence parameters are as follows:'
  write(io,*)
  write(io,*) 'up/sl            =',upsl
  write(io,*) 'lt/df            =',ltdf
  write(io,*) 'l11/df           =',l11df
  write(io,*) 'eta_k/df         =',eta_k/df
  write(io,*) 'lt (cm)          =',lt*100.0*l_ref
  write(io,*) 'l11 (cm)         =',l11*100.*l_ref
  write(io,*) 'li22 (cm)        =',0.5*l11*100.*l_ref
  write(io,*) 'df (cm)          =',df*100.*l_ref
  write(io,*) 'eta_k(cm)        =',eta_k*100.0*l_ref
  write(io,*) 're_t             =',re_t
  write(io,*) 're_l11           =',re_t*l11df/ltdf
  write(io,*) 're_taylor        =',(re_t*l11df/ltdf)**0.5
  write(io,*) 'le for ke (cm)   =',le*l_ref*100.0
  write(io,*) 'ld for kd (cm)   =',ld*l_ref*100.0
  write(io,*) 'up (cm/sec)      =',up*a_ref*100.0
  write(io,*) 'eps (cm^2/sec^3) =',dissip*a_ref**3/l_ref*(100.0**2)
  write(io,*)

  write(io,*) 'various turbulence time scales are as follows:'
  write(io,*)
  write(io,*) 't_flame (sec)        =', df*l_ref/(sl*a_ref)
  write(io,*) 'tau/t_flame          =', ltdf/upsl
  write(io,*) 'tau=lt/up (sec)      =', lt/up*time_ref
  write(io,*) 't_l11=l11/up (sec)   =', l11*l_ref/(up*a_ref)
  write(io,*) 't_k (sec)            =', lt*time_ref/up*re_t**(-0.5)
  write(io,*)

! resolution of kolmogorv scale

  write(io,*)
  write(io,*) 'Based on that, your Kolmogorov scale eta_k is resolved as follows: '
  if(vary_in_x==1)then
    write(io,*) 'in the x-direction: ',eta_k/delx
    if(eta_k/delx.lt.1.0)then
      write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(io,*)'Kolmogorov scale may not be resolvable with this choice'
    endif
  endif
  if(vary_in_y==1)then
    write(io,*) 'in the y-direction: ',eta_k/dely
    if(eta_k/dely.lt.1.0)then
      write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(io,*)'Kolmogorov scale may not be resolvable with this choice'
    endif 
  endif
  if(vary_in_z==1)then
    write(io,*) 'in the z-direction: ',eta_k/delz
    if(eta_k/delz.lt.1.0)then
      write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(io,*)'Kolmogorov scale may not be resolvable with this choice'
    endif 
  endif
  write(io,*)

! checks on ld for vkp spectrum
  if(ispect==0)then
    if((vary_in_x==1).and.(ld/delx.lt.1.0))then
      write(io,*)
      write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(io,*)'Dissipation range scale may not be resolvable with this choice'
      write(io,*)'Problem is x-direction'
    endif
    if((vary_in_y==1).and.(ld/dely.lt.1.0))then
      write(io,*)
      write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(io,*)'Dissipation range scale may not be resolvable with this choice'
      write(io,*)'Problem is x-direction'
    endif
    if((vary_in_z==1).and.(ld/delz.lt.1.0))then
      write(io,*)
      write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(io,*)'Dissipation range scale may not be resolvable with this choice'
      write(io,*)'Problem is x-direction'
    endif 
    if(ke.gt.kd)then
      write(io,*)
      write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      write(io,*)'kd should be > ke'
      write(io,*)'i.e. ld<le'
      write(io,*)
    endif
  endif

! checks on L11, lt, and le
  if(vary_in_x==1)then
    if((le.ge.xmax-xmin).or.(lt.ge.(xmax-xmin)).or.(l11.ge.(xmax-xmin)))then
      write(io,*) 'Large length scales may not be represented with this choice'
      write(io,*) 'Problem is x-direction'
    endif
  endif
  if(vary_in_y==1)then
    if((le.ge.ymax-ymin).or.(lt.ge.(ymax-ymin)).or.(l11.ge.(ymax-ymin)))then
      write(io,*) 'Large length scales may not be represented with this choice'
      write(io,*) 'Problem is y-direction'
    endif
  endif
  if(vary_in_z==1)then
    if((le.ge.zmax-zmin).or.(lt.ge.(zmax-zmin)).or.(l11.ge.(zmax-zmin)))then
      write(io,*) 'Large length scales may not be represented with this choice'
      write(io,*) 'Problem is z-direction'
    endif
  endif

  if(vary_in_x.eq.1) then
    write(io,*) 'number of l11 eddies in x-direction =', (xmax-xmin)/(l11df*df)
  endif
            
  if(vary_in_y.eq.1) then
    write(io,*) 'number of l11 eddies in y-direction =', (ymax-ymin)/(l11df*df)
  endif
        
  if(vary_in_z.eq.1) then
    write(io,*) 'number of l11 eddies in z-direction =', (zmax-zmin)/(l11df*df)
  endif

  endif  !myid == 0

!-----------------------------------------------------------------------------------------
! Set spectrum constant
  if(ispect==0)then
!   case of vkp
    if(numdim==2)then
      C_spect = 1.5 * up**5 / dissip
    else
      C_spect = 1.5 * 1.5 * up**5 / dissip
    endif
  else
!   case of passot-pouquet
    pi = 4.0*atan(1.0) 
    if(numdim==2)then
      C_spect = 16.0 * sqrt(2.0/pi) * up**2 / ke * 2.0/3.0
    else
      C_spect = 16.0 * sqrt(2.0/pi) * up**2 / ke
    endif
  endif
  
  return
  end subroutine get_turbulence_params


!========================================================================================
  subroutine calc_passot_pouquet_params(ke,le,upsl,ltdf,re,sl,df,l11df)
!========================================================================================
! - routine calculates parameters for the passot-pouquet spectrum
!
! - The Passot-Pouquet spectrum is:
!
!   E(k) = C * (k/ke)**4 * exp(-2*(k/ke)**2)
!
!   FOR 2D:   C = sqrt(2/pi) * up**2 / ke * 32/3
!
!   FOR 3D:   C = sqrt(2/pi) * up**2 / ke
!
!   The difference between 2D and 3D is because the TKE = 1.5 up**2 in 3D and up**2 in 2D
!   The spectrum integrates to the TKE in both cases.
!
!   The quantity ke will be calculated in this routine.  This is the wavenumber at peak 
!   energy density.  The calculation is based on integration of the dissipation spectrum.
!   It assumes that the reference thermal conductivity and the input Pr provide the 
!   viscosity.  Be careful to use a good value for these or your turbulence parameters
!   will be off!  If you have a varying viscosity or if you are using mixture 
!   averaged transport you will experience some discrepancy from the predicted scales.
!   In any case you advised to check your initial field using post-processing.
!
!----------------------------------------------------------------------------------------
  use param_m, only : numdim

  implicit none
!----------------------------------------------------------------------------------------

! passed arguments:
  real, intent(in)  :: upsl   !up/sl
  real, intent(in)  :: ltdf   !lt/df
  real, intent(in)  :: re     !non-dimensionalisation Reynolds number
  real, intent(in)  :: sl     !characteristic velocity (perhaps related to flame)
  real, intent(in)  :: df     !characteristic length scale (perhaps related to flame)

  real, intent(out) :: ke     !peak energy density wavenumber
  real, intent(out) :: le     !length scale of peak energy density wavenumber
  real, intent(out) :: l11df  !l11/df

!----------------------------------------------------------------------------------------

! local declarations
  real :: lt      !integral scale = up^3/dissip
  real :: l11     !velocity autocorrelation length-scale
  real :: dissip  !dissipation = up^3/lt
  real :: up      !turbulence fluctuation defined as (u_ii)/numdim
  real :: pi

!----------------------------------------------------------------------------------------
! executable statements
!----------------------------------------------------------------------------------------
  pi = 4.0*atan(1.0) 

! from input
  up = upsl*sl

! from input
  lt = ltdf*df

! definition of lt
  dissip = up**3/lt

! calculate ke based on the dissipation
! from integration of 2*k^2*E(k)/re dk
  if(numdim==2)then
    ke = sqrt(dissip*4.0*re/(10.0*up*up))
  elseif(numdim==3)then
    ke = sqrt(dissip*4.0*re/(15.0*up*up))
  endif

! set le
  le = 2.0*pi/ke

! now calculate the l11 scales
!
! in 2D: L11 = 2/up**2 Integral_0^infinity E(k)/k dk
!
! in 3D: L11 = pi/2/up**2 Integral_0^infinity E(k)/k dk
!
! (the difference between 2D and 3D comes from integrating over 2 directions
!  in 3D and one direction in 2D) 
  if(numdim==2)then
    l11df = 8.0/3.0 * sqrt(2.0/pi) / ke / df
  else
    l11df = sqrt(2.0*pi) / ke / df
  endif

     
!----------------------------------------------------------------------------------------
  return
end subroutine calc_passot_pouquet_params
!----------------------------------------------------------------------------------------

!========================================================================================
  function energy_spectrum(mag_k) result(e_spec)
!========================================================================================
! - routine calculates the radial energy spectrum
!   physically the spectrum is the integrated energy in a thin shell of radius
!   k and infinitessimal thickness dk divided by dk.
!
! - currently Von-Karman Pao and Passot-Pouquet spectra are coded
! - adding a new spectrum requires an addition to this routine, plus you will
!   need to calculate the parameters of the spectrum from the input file parameters
!
! - The Von-Karman Pao spectrum is:
!
!   E(k) = C * (k/ke)**4 / ( 1.0 + (k/ke)**2 )**(17./6.) * exp (-2.25 *(k/kd)**(4.0/3.0))
!
!   FOR 2D: C = 1.5 * up**5 / dissip
!
!   FOR 3D: C = 1.5 * 1.5 * up**5 / dissip
!
!   ke is an "energy containing" length scale
!   kd is a "dissipation" length scale (related to Kolmogorov scale)
!
! - The Passot-Pouquet spectrum is:
!
!   E(k) = C * (k/ke)**4 * exp(-2*(k/ke)**2)
!
!   FOR 2D:   C = 16.0 * sqrt(2/pi) * up**2 / ke * 2/3
!
!   FOR 3D:   C = 16.0 * sqrt(2/pi) * up**2 / ke
!
!   ke is an "energy containing" length scale
!
!   The difference between 2D and 3D is because the TKE = 1.5 up**2 in 3D and up**2 in 2D
!   The spectrum integrates to the TKE in both cases.  Be careful in comparisons
!   with the literature that some spectra are normalised to integrate to up, and there
!   are different definitions of up and wavenumber out there!
!
!----------------------------------------------------------------------------------------
  use param_m, only : numdim
  implicit none
!----------------------------------------------------------------------------------------
! passed arguments:
  real, intent(in) :: mag_k       ! wavenumber magnitude

! result
  real :: e_spec                  ! energy spectrum
!----------------------------------------------------------------------------------------
! local declarations
  real :: rat_ke  !energy containing wavenumber ratio
  real :: rat_kd  !dissipation wavenumber ratio (only for vkp)
  real :: vk, pao
!----------------------------------------------------------------------------------------
! executable statements
!----------------------------------------------------------------------------------------

  rat_ke = mag_k/ke  ! ke is a module variable

  if(ispect==0)then
!   case of vkp spectrum
    rat_kd = mag_k/kd
    vk = C_spect*(rat_ke)**4 / ( 1.0 + (rat_ke)**2 )**(17./6.) !von-karman interpolation
    pao = exp (-2.25 *(rat_kd)**(4.0/3.0))                     !pao viscous correction
    e_spec = vk*pao
  else
!   case of passot - pouquet spectrum
    e_spec = C_spect*(rat_ke)**4 * exp (-2.0 * rat_ke**2)
  endif
     
!----------------------------------------------------------------------------------------
  return
end function energy_spectrum
!----------------------------------------------------------------------------------------

!=========================================================================================
  subroutine rndini2d(xmin,xmax,ymin,ymax,nx,ny,nz,x,seed,u,io)
!=========================================================================================
! compute the turbulence flow field based on the VKP energy spectrum.
!
! NOTE: This routine assumes that the 2-D plane is the XY plane.
! It will NOT WORK for YZ or XZ configurations (though this could be
! fixed without too much work - just some more logic)
!
!-----------------------------------------------------------------------------------------
!  use param_m, only : ny_g  ! really this should all be cleaned up...
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real, intent(in) :: xmax, xmin, ymin, ymax    !grid start and end locations
  integer, intent(in) :: nx,ny,nz               !grid dimensions
  real, intent(in) :: x(nx)                     !grid locations
  real u(nx,ny,nz,3)                            !velocity field

  integer seed, io                              !turbulence seed, io unit

! local declarations

  real    amp
  real    c
  real    e_spec, eps
  real    intl1, intl2
  real    kc, kx, ky
  real    mag_k
  real    pao
  real    pi
  real    ps
  real    r11, r22
  real    rat_k, rat2_k
  real    u1m,u2m
  real    urms
  real    vk

  complex i_cmplx
  complex u1k(nh,nw)                              !fourier space x-velocity
  complex u2k(nh,nw)                              !fourier space y-velocity

  integer i, j, k, l
  integer ii,   jj
  integer ivar, npoints

  real eps_new ! minumum filled wavenumber (Evatt 11-JUN-2004)
!-----------------------------------------------------------------------------------------
! Revisions and comments:
! 1. The routine computes the turbulence flow field
! based on the VKP energy spectrum:
! E(k) = A_k (Up^5/dissip) * (k/k_e)^4/(1+(k/k_e)^2)^(17/6) exp(-(3/2) a_k (k/k_d)^(4/3) )
! where A_k = 1.5
! a_k = 1.5
! Up  = rms velocity
! k_e = (in code Ke ) is the wave number associated
!  with most energetic scales
! k_d = (in code Kd ) is the wave number associated
!  with dissipation scales
!
! 2. The spectrum is specified inside a range Eps < k < Kc,
!   where Eps = is a very small number set to near zero (Ke / 1000, here).
! Kc  = cut-off scale.
!   Outside this range the spectrum E(k) is set to zero.
!
! 3. The descrete wave numbers are based on all possible descrete periods along
!   a given direction ranging from 0 to (N-1)*2*pi/L.
!
! 4. Since the range L is not specified exactly to yield
!   a multiple of 2*pi, a truncation of the ratio L/(2*pi)
!   is carried out to avoid out-of-range computations.
!-----------------------------------------------------------------------------------------
! set pi

  pi = 4.0 * atan ( 1.0 )
!-----------------------------------------------------------------------------------------
! error checking

!  Evatt Hawkes: this appears to be a legacy from when nw was read from an input file
!  so have cut it
!  if( nw .gt. ny_g/2 ) then
!    if( myid.eq.0 ) then
!      write(io,9350)
!    endif
!    call terminate_run(io,0)  !must be called by all processors
!  endif

! Evatt Hawkes JUN 2004 : previously ke and kd were entered as lengths
! and juggled back and forth between wavenumber and length - that was confusing
! transform ke and kd into a wavenumber (note: angular wavenumber)
!  ke = 2.0 * pi / ke
!  kd = 2.0 * pi / kd

! initialize field with homogeneous turbulence on top of laminar solution

  i_cmplx = ( 0.0, 1.0 )

! specify the cut-offs for the evaluation of spectrum
! eps means a `small number'

  eps = ke / 10000.0

! MODIFICATION OF EPS (Evatt 11-JUN-2004)
! want to allow no bigger waves than the smallest dimension
  eps_new = 2.0*pi/min((ymax-ymin),(xmax-xmin))

! Xmax-Xmin > -2 Ymin  ???note from evatt - did not understand this comment

! cut-off wavenumber - must be less than the Nyquist cut-off
! evatt has set this equal to the Nyquist cut-off by setting nh=nx_g/2+1
! i am not sure how general this is for varying box sizes
  kc = real ( nh - 1 ) * 2.0 * pi /max((xmax - xmin),(ymax-ymin))

! truncation to 8 digits only ???note from evatt - did not understand this comment
! Kc = 1.E-8*int(Kc * 1.e08)

! set constants for vkp spectrum
! Evatt Hawkes JUN 2004 - c no longer needed now there is a general spectrum call
!  c  = 1.5 * up**5 / dissip
  amp = 2.0 * pi / sqrt ( (xmax-xmin)*(ymax-ymin) )

! loop over fourier space
! note that all processes initialise the same 2D domain in fourier space
! i.e. there is no domain decomposition in fourier space!
  do i = 1, nh
    do j = 1, nw

!     set a random phase
      ps = 2.0 * pi * ( ran2 ( seed ) - 0.5 )

!     find local wavenumber
      kx = real ( i - 1 ) * 2. * pi / ( xmax - xmin )
      ky = real ( j - 1 ) * 2. * pi / ( ymax - ymin )

!     by convention negative wavenumbers are stored in reverse starting from nh+1
      if( j .gt. nh ) then
        ky = - real(nw+1-j)*2. * pi / ( ymax - ymin )
      endif

!     wavenumber magnitude
      mag_k = sqrt ( kx**2 + ky**2 )


!     Evatt Hawkes JUN 2004 - no longer needed now there is a general spectrum
!!     ratio of wavenumber to "energetic" wavenumber
!      rat_k = mag_k / ke
!
!!     ratio of wavenumber to "dissipation" wavenumber
!      rat2_k = mag_k / kd
!
!!     spectrum
!      vk = c * rat_k**4 / ( 1.0 + rat_k**2 )**(17./6.)
!      pao = exp ( -2.25 * rat2_k**(4.0/3.0) )
!      e_spec = vk * pao

      e_spec = energy_spectrum(mag_k)

!     intialise velocity
      u1k(i,j) = 0.0
      u2k(i,j) = 0.0

!     only set if within a sensible range
!     zero wavenumber not filled as this would result in a net velocity
!     wavenumbers greater than the cut-off wavenumber not filled

!        Evatt temporary mod 11-JUN-2004
!      if( eps .lt. mag_k .and. mag_k .lt. kc ) then
      if( eps_new .lt. mag_k .and. mag_k .lt. kc ) then

!       normal case
        if( eps .lt. abs(kx) .and. eps .lt. abs(ky)) then
!         set velocity from known magnitude from spectrum, and random phase
          u1k(i,j) = amp * sqrt ( e_spec * ky**2 / ( pi * mag_k**3 ) )  &
                   * exp ( i_cmplx * ps )
!         enforce continuity in fourier space (k.u=0)
          u2k(i,j) = - kx * u1k(i,j)/ky
        endif

!       dealing with small kx wavenumber
        if( abs(kx) .lt. eps .and. abs(ky) .gt. eps ) then
!         set velocity from known magnitude from spectrum, and random phase
          u1k(i,j) = amp * sqrt ( e_spec * ky**2 / ( pi * mag_k**3 ) )  &
                   * exp ( i_cmplx * ps )
!         since u2k=0 and kx=0 continuity is satisfied
        endif

!       dealing with small ky wavenumber
        if( abs(kx) .gt. eps .and. abs(ky) .lt. eps ) then
!         set velocity from known magnitude from spectrum, and random phase
          u2k(i,j) = amp * sqrt ( e_spec * kx**2 / ( pi * mag_k**3 ) )  &
                   * exp ( i_cmplx * ps )
!         since u1k=0 and ky=0 continuity is satisfied
        endif

      endif

    enddo
  enddo

! since transformed result is real, fourier space result must be conjugate symmetric
! enforce j-symmetry here for i=1.  i-symmetry is later enforced for all j in rin2dy
  do j = nh+1, nw
    u1k(1,j) = conjg ( u1k(1,nw+2-j) )
    u2k(1,j) = conjg ( u2k(1,nw+2-j) )
  enddo

! do a check that we actually achieved zero divergence in fourier space,
! i.e. that continuity will be satisfied in physical space
  do i = 1, nh
    do j = 1, nw

      kx = real ( i - 1 ) * 2.0 * pi / ( xmax - xmin )
      ky = real ( j - 1 ) * 2.0 * pi / ( ymax - ymin )

      if( j .gt. nh ) then
        ky =-real(nw+1-j) * 2.0 * pi / ( ymax - ymin )
      endif

      if(abs(real(kx*u1k(i,j)+ky*u2k(i,j))) .gt. 1.e-5 .or.         &
      abs(aimag(kx*u1k(i,j)+ky*u2k(i,j))) .gt. 1.e-5 ) then
        if(myid.eq.0) then
          write(io,4010) i,j, myid
        endif
      endif

    enddo
  enddo

! do the inverse transformations etc

  ivar = 1
  call rin2dy ( ivar, nx,ny,u1k, xmin, xmax, x, u(:,:,1,ivar) )

  ivar = 2
  call rin2dy ( ivar, nx,ny,u2k, xmin, xmax, x, u(:,:,1,ivar) )
!-----------------------------------------------------------------------------------------
! format statements

  4010 format(' non zero divergence in fourier space :',3i4)
!  Evatt Hawkes removed July 2003
!  9350 format(/' error: wrong specification of nw')
!-----------------------------------------------------------------------------------------
  return
  end subroutine rndini2d
!=========================================================================================
  subroutine rin2dy(ivar,nx,ny,uk,xmin,xmax,x,u)
!=========================================================================================
! initializes 2-D turbulence from u-field defined in fourier space
!-----------------------------------------------------------------------------------------
  use param_m, only : nx_g,ny_g  !should be generalised
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed    

  integer ivar
  integer, intent(in) :: nx,ny
  complex uk(nh,nw)               !the complex u-field in fourier space
  real xmin
  real xmax
  real x(nx)
  real u(nx,ny)                   !the field in physical space

! local declarations

  integer i, j, jj, l, ll, ione, nyt
  real pi
  real kx, kx0

  complex, allocatable, dimension(:) :: tmpout
  real, allocatable, dimension(:) :: table, work

  complex, allocatable, dimension(:,:) :: fcmplx
!-----------------------------------------------------------------------------------------
! allocate array space

  allocate(fcmplx(nh,ny_g))

#if defined(ARCH_T3E)
    allocate (table(2*ny_g))
    allocate (work(4*ny_g))
#else
#if defined(ARCH_SGI)
      allocate (table(2*(ny_g+15)))
#else
#if defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
        allocate (table(2*ny_g))
        allocate (work(2*ny_g))
#else
#if defined(ARCH_X1)
          allocate (table(100+8*ny_g))
          allocate (work(8*ny_g))
#endif
#endif
#endif
#endif
!-----------------------------------------------------------------------------------------
! set pi

  pi = 4.0 * atan ( 1.0 )
!-----------------------------------------------------------------------------------------
! do stuff

! initialise to zero
  do j=1,ny,1
    do i=1,nx,1
      u(i,j) = 0.0
    enddo
  enddo

  do j=1,ny_g,1
    do i=1,nh,1
      fcmplx(i,j) = 0.0
    enddo
  enddo

! the fourier space velocity is now transfered into a possibly larger array that is the
! correct size for the inverse fft
! elements going from nh+1 to nw are tranferred to the end of the array
! (from ny_g+nh+1-nw to ny_g)
! the convention is that the first node holds the "0" wavenumber
! the second node holds the "1" wavenumber, etc, up to the ny_g/2 wavenumber
! after which the -ny/2+1 wavenumber is held, followed by the -ny/2+2,
! up to the -1 wavenumber held in the ny_g node
! hence this deals with non-square domains (remember that nw was based on min(nx,ny,nz))
! also nw can be set less than min(nx,ny,nz) if desired, although at the time of
! writing this is hard-wired for safety such that nw=min(nx_g,ny_g)
!
  do i = 1, nh
    do j = 1, nw
      if ( j .le. nh ) then
        fcmplx(i,j) = uk(i,j)
      else
        fcmplx(i,ny_g+j-nw) = uk(i,j)
      end if
    enddo
  enddo

! direction of transform
  ione = 1

! allocate workspace
#if defined(ARCH_T3E) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
    allocate(tmpout(ny_g))
#endif

! initialise fft table
  call fft_cinit(ny_g,table)

! do the inverse fft pencil by pencil in the y-direction
! note that no commnication is required because each process contains the 
! whole fourier field
  do i=1,nh
    call fft_complex( ione, ny_g, table, work, fcmplx(i,:), tmpout)
  enddo

! deallocate workspace
  deallocate(table)

#if defined(ARCH_T3E) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
    deallocate(tmpout)
    deallocate(work)
#endif


  nyt = ny !probably a leftover from the old code where periodicity was 1=ny, where nyt=nym

! do the remaining inverse ft in the x-direction
! the inverse ft is done "manually" using the nx**2 operation 
! rather than using an fft, an nx log(nx) operation
! this is to save memory, by applying the symmetry condition
! loop over x
  do l = 1, nx
    do j = 1, nyt

!     domain decomposition for real-space velocity
      jj = j + yid*ny
      ll = l + xid*nx

!     separate treatment for the nh-1 wavenumber
!     symmetry is not applied
!     (nh-1 wavenumber has identical fourier coefficient to -(nh-1))
      kx0 = real ( nh - 1 ) * 2.0 * pi * real(ll-1)/real(nx_g)

      u(l,j) = real(fcmplx(1,jj)) + real(fcmplx(nh,jj))*cos(kx0)     &
                - aimag(fcmplx(nh,jj)) * sin(kx0)

!     loop over k
      do i = 2, nh-1

!       set wavenumber
        kx = real ( i - 1 ) * 2.0 * pi * real(ll-1)/real(nx_g)

!       increment the ft - factor of 2.0 is for the symmetry
        u(l,j) = u(l,j) + 2.0 * ( real(fcmplx(i,jj))*cos(kx)     &
                  - aimag(fcmplx(i,jj)) * sin(kx) )

      enddo

    enddo
  enddo

  call MPI_Barrier(gcomm,ierr)

!-----------------------------------------------------------------------------------------
! deallocate remaining arrays

  deallocate(fcmplx)
!-----------------------------------------------------------------------------------------
  return
  end subroutine rin2dy
!=========================================================================================

!=========================================================================================
  real function ran2(idum)
!=========================================================================================
! computes random numbers for both 2D and 3D turbulence
!
! contains platform-independent calls to random number generators
! (note from Evatt: not sure who entered this comment, and not sure that it is true.) 
! using F90 intrinsic function random_number
!
! currently the seed idum is not used in this routine because the intrinsic function 
! random_number does not require this as an input argument.  
! idum is remains here since commonly used machine independent random number generators 
! employ a seed as an argument.
!
! the seed required by the random_number is set elsewhere using the intrinsic 
! function random_seed
!
!-----------------------------------------------------------------------------------------
  implicit none

! declarations passed in

  integer idum

! local declarations

  real(kind=4) rannum
!-----------------------------------------------------------------------------------------
! calculate random number
  call random_number(rannum)
  ran2=rannum
!-----------------------------------------------------------------------------------------
  return
  end function ran2
!-----------------------------------------------------------------------------------------


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! Turbulence generation for 3D. Rewritten by Ramanan Sankaran.
subroutine turb_3d(xmin, xmax, ymin,ymax,zmin,zmax,xx,iseed,u,io)
use param_m, only: nx, ny, nz, nx_g, ny_g, nz_g
use topology_m
implicit none
!----------------------------------------
real, intent(in) :: xmin, xmax, ymin, ymax, zmin, zmax
real, intent(in) :: xx(nx) !dummy, not used.
integer, intent(in) :: iseed
real, intent(out) :: u(nx, ny, nz, 3)
integer, intent(in) :: io

!----------------------------------------
real pi
complex :: uk(nx, ny, nz, 3) 
real ps1, ps2, psr
real delta, kc
real tke_l, tke
integer i, j, k, m
integer ir, jr, kr

real kx, ky, kz, k2, mag_k
real e_spec, amp
complex ak, bk
complex i_cmplx

real, allocatable, dimension(:) :: table, work
complex, allocatable, dimension(:) :: tmpout, q, qg

new_seed(:) = iseed
call random_seed(put=new_seed)
!JET has a starting problem with random numbers
do i = 1, 1e6
  call random_number(psr)
end do

pi = 4.0*atan(1.0)
i_cmplx = (0.0, 1.0)

!----------------------------------------
delta = max( (xmax-xmin)/real(nx_g-1), &
             (ymax-ymin)/real(ny_g-1), &
             (zmax-zmin)/real(nz_g-1) )
kc = pi/delta
amp = sqrt((2.0*pi)**3/( (xmax-xmin)*(ymax-ymin)*(zmax-zmin) ) )
!----------------------------------------
uk(:,:,:,:) = 0.0

!----------------------------------------
loopk: do k = 1, nz_g
  loopj: do j = 1, ny_g
    !----------------------------------------
    !Do only half the i's due to conjugate symmetry
    loopi: do i = 1, 1+nx_g/2
      call random_number(psr); psr = 2.0*pi*(psr-0.5)
      call random_number(ps1); ps1 = 2.0*pi*(ps1-0.5)
      call random_number(ps2); ps2 = 2.0*pi*(ps2-0.5)
      !----------------------------------------
      ! Dont fill the origin. It will add a mean velocity.
      if (i.eq.1 .and. j.eq.1 .and. k.eq.1) cycle loopi
      !----------------------------------------
      ! Dont fill the pi-wavenumber.
      ! When the number of grid points is even, there is a point at `pi'
      ! Since -pi = pi, conjugate symmetry here needs logic. 
      ! I have not coded that logic here.
      ! This wave number corresponds to kc. I leave it at zero.
      ! Ok to directly cycle loopj and loopk here.
      if( 2*int(nx_g/2) == nx_g .and. i .eq. 1+nx_g/2) cycle loopi
      if( 2*int(ny_g/2) == ny_g .and. j .eq. 1+ny_g/2) cycle loopj
      if( 2*int(nz_g/2) == nz_g .and. k .eq. 1+nz_g/2) cycle loopk
      !----------------------------------------
      ! find out if this i, j, k belongs to this processor.
      ! If not dont do anything.
      if ( i.le.xid*nx .or. i.gt.(xid+1)*nx .or. &
           j.le.yid*ny .or. j.gt.(yid+1)*ny .or. &
           k.le.zid*nz .or. k.gt.(zid+1)*nz     ) cycle loopi
      !----------------------------------------
      ir = i - xid*nx
      jr = j - yid*ny
      kr = k - zid*nz
      !----------------------------------------
      kx = real(i-1)*2.0*pi/(xmax-xmin)
      if(j <= 1+ny_g/2) then
        ky = real(j-1)*2.0*pi/(ymax-ymin)
      else
        ky = -real(ny_g+1-j)*2.0*pi/(ymax-ymin)
      end if
      if(k <= 1+nz_g/2) then
        kz = real(k-1)*2.0*pi/(zmax-zmin)
      else
        kz = -real(nz_g+1-k)*2.0*pi/(zmax-zmin)
      end if
      k2 = sqrt(kx**2+ky**2)
      mag_k = sqrt(kx**2+ky**2+kz**2)
      !----------------------------------------
      ! Dont fill wave numbers larger than the cutoff
      if (mag_k .gt. kc) cycle loopi
      !----------------------------------------
      e_spec = energy_spectrum(mag_k)
      ak = amp*sqrt(e_spec/(2.0*pi*mag_k**2))*exp(i_cmplx*ps1)*cos(psr)
      bk = amp*sqrt(e_spec/(2.0*pi*mag_k**2))*exp(i_cmplx*ps2)*sin(psr)
      !----------------------------------------
      if(k2 .lt. ke/1e4) then
        uk(ir,jr,kr,1) = (ak+bk)/sqrt(2.0)
      else
        uk(ir,jr,kr,1) = (ak*mag_k*ky+bk*kx*kz)/(mag_k*k2)
      endif
      if(k2 .lt. ke/1e4) then
        uk(ir,jr,kr,2) = (bk-ak)/sqrt(2.0)
      else
        uk(ir,jr,kr,2) = (bk*ky*kz-ak*mag_k*kx)/(mag_k*k2)
      endif
      uk(ir,jr,kr,3) = -(bk*k2)/mag_k
    end do loopi
  end do loopj
end do loopk

!----------------------------------------
! i = 1 plane needs special handling. 
if(xid .eq. 0) then
  call conjugate_yzplane(uk(1,:,:,1))
  call conjugate_yzplane(uk(1,:,:,2))
  call conjugate_yzplane(uk(1,:,:,3))
end if

!----------------------------------------
!----------------------------------------
! Check for spectral energy content
if(myid==0)then
  write(io,*)
  write(io,*) 'Performing Energy Check for 3D turbulence initialization'
  write(io,*) 'Spectral and Physical Energy Content Should Be Equal'
endif

!Missing factor of 0.5 accounted through conjugate symmetry
tke_l = sum(real(uk*conjg(uk)))
! But i = 1 already has conjugate symmetry applied.
tke_l = tke_l - 0.5*sum(real(uk(1,:,:,:)*conjg(uk(1,:,:,:))))
call MPI_Reduce(tke_l,tke,1,MPI_REAL8,MPI_SUM,0,gcomm,ierr)
if(myid==0) write(io,*) 'tke/1.5up**2 spectral =', tke/1.5/up/up

! Now do the FFTS
!----------------------------------------
! Allocate arrays
#if defined(ARCH_T3E)
  allocate (table(2*max(nx_g,ny_g,nz_g)))
  allocate (work (4*max(nx_g,ny_g,nz_g)))
#elif defined(ARCH_SGI)
  allocate (table(2*(max(nx_g,ny_g,nz_g)+15)))
#elif defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
  allocate (table(2*max(nx_g,ny_g,nz_g)))
  allocate (work (2*max(nx_g,ny_g,nz_g)))
#elif defined(ARCH_X1)
  allocate (table(100+8*max(nx_g,ny_g,nz_g)))
  allocate (work(8*max(nx_g,ny_g,nz_g)))
#endif

#if defined(ARCH_T3E) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
  allocate(tmpout(max(nx_g,ny_g,nz_g)))
#endif

allocate(q(max(nx,ny,nz)))
allocate(qg(max(nx_g,ny_g,nz_g)))

!----------------------------------------
! First in the y-direction
call fft_cinit(ny_g, table)
do k = 1, nz_g
  do i = 1, 1+nx_g/2
    !----------------------------------------
    ! find out if this i, j, k belongs to this processor.
    ! If not dont do anything.
    if ( i.le.xid*nx .or. i.gt.(xid+1)*nx .or.  &
         k.le.zid*nz .or. k.gt.(zid+1)*nz     ) cycle
    ir = i - xid*nx
    kr = k - zid*nz
    do m = 1, 3
      q(1:ny) = uk(ir,1:ny,kr,m)
      call mpi_gather(q, 2*ny, MPI_REAL8, qg, 2*ny, MPI_REAL8, 0, ycomm, ierr)
      if(yid .eq. 0) call fft_complex(1, ny_g, table, work, qg, tmpout)
      call mpi_scatter(qg, 2*ny, MPI_REAL8, q, 2*ny, MPI_REAL8, 0, ycomm, ierr)
      uk(ir,1:ny,kr,m) = q(1:ny)
    end do
  end do
end do


!----------------------------------------
! Next in the z-direction
call fft_cinit(nz_g, table)
do j = 1, ny_g
  do i = 1, 1+nx_g/2
    !----------------------------------------
    ! find out if this i, j, k belongs to this processor.
    ! If not dont do anything.
    if ( i.le.xid*nx .or. i.gt.(xid+1)*nx .or.  &
         j.le.yid*ny .or. j.gt.(yid+1)*ny     ) cycle
    ir = i - xid*nx
    jr = j - yid*ny
    do m = 1, 3
      q(1:nz) = uk(ir,jr,1:nz,m)
      call mpi_gather(q, 2*nz, MPI_REAL8, qg, 2*nz, MPI_REAL8, 0, zcomm, ierr)
      if(zid .eq. 0) call fft_complex(1, nz_g, table, work, qg, tmpout)
      call mpi_scatter(qg, 2*nz, MPI_REAL8, q, 2*nz, MPI_REAL8, 0, zcomm, ierr)
      uk(ir,jr,1:nz,m) = q(1:nz)
    end do
  end do
end do

!----------------------------------------
! Last is the x-direction. Conjugate symmetry is done here.
call fft_cinit(nx_g, table)
do k = 1, nz_g
  do j = 1, ny_g
    !----------------------------------------
    ! find out if this i, j, k belongs to this processor.
    ! If not dont do anything.
    if ( j.le.yid*ny .or. j.gt.(yid+1)*ny .or.  &
         k.le.zid*nz .or. k.gt.(zid+1)*nz     ) cycle
    jr = j - yid*ny
    kr = k - zid*nz
    do m = 1, 3
      q(1:nx) = uk(1:nx,jr,kr,m)
      call mpi_gather(q, 2*nx, MPI_REAL8, qg, 2*nx, MPI_REAL8, 0, xcomm, ierr)
      if(xid .eq. 0) then
        do i = 2+nx_g/2, nx_g
          qg(i) = conjg(qg(nx_g+2-i))
        end do
        call fft_complex(1, nx_g, table, work, qg, tmpout)
      end if
      call mpi_scatter(qg, 2*nx, MPI_REAL8, q, 2*nx, MPI_REAL8, 0, xcomm, ierr)
      uk(1:nx,jr,kr,m) = q(1:nx)
    end do
  end do
end do

!At this point, uk should be purely real.
u = real(uk)

!----------------------------------------
!Checking for any imaginary uk values
tke_l = maxval(abs(aimag(uk)))
call MPI_Reduce(tke_l,tke,1,MPI_REAL8,MPI_MAX,0,gcomm,ierr)
if(myid .eq. 0) write(io, *) 'Maximum imaginary uk is ', tke

!----------------------------------------
!Checking kinetic energy
tke_l = 0.5*sum(u*u)
call MPI_Reduce(tke_l,tke,1,MPI_REAL8,MPI_SUM,0,gcomm,ierr)
if(myid==0) &
  write(io,*) 'tke/1.5up**2 physical =', tke/1.5/up/up/real(nx_g*ny_g*nz_g)

return

contains
!----------------------------------------
subroutine conjugate_yzplane(f)
implicit none
complex, intent(inout) :: f(ny, nz)

complex :: fg3(ny, nz, yz_pes), fg(ny_g, nz_g)
integer j, k

!Gather the whole array to yz_id=0. 
!Doing it without derived data type.
call mpi_gather(f, 2*ny*nz, MPI_REAL8, fg3, 2*ny*nz,MPI_REAL8,0,yz_comm,ierr)

if(yz_id .eq. 0) then
  do k = 0, zpes-1
    do j = 0, ypes-1
      fg(j*ny+1:(j+1)*ny, k*nz+1:(k+1)*nz) = fg3(1:ny,1:nz,k*ypes+j+1)
    end do
  end do
  !Apply conjugate symmetry 
  do k = 2+nz_g/2, nz_g
    fg(1, k) = conjg(fg(1, nz_g+2-k))
    do j = 2, ny_g
      fg(j,k) = conjg(fg(ny_g+2-j,nz_g+2-k))
    end do
  end do
  !Treat the k=1 line
  do j = 2+ny_g/2, ny_g
    fg(j,1) = conjg(fg(ny_g+2-j,1))
  end do
  do k = 0, zpes-1
    do j = 0, ypes-1
      fg3(1:ny,1:nz,k*ypes+j+1) = fg(j*ny+1:(j+1)*ny, k*nz+1:(k+1)*nz)
    end do
  end do
end if

call mpi_scatter(fg3, 2*ny*nz, MPI_REAL8, f, 2*ny*nz,MPI_REAL8,0,yz_comm,ierr)

return
end subroutine conjugate_yzplane

end subroutine turb_3d
!----------------------------------------------------------------------

end module turbulence_m

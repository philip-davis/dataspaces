#include "globalDefines.h"
!$Id:!
!=========================================================================================
  module chemkin_m
!=========================================================================================
! module for chemkin variables
! ifdef options to use TianFeng Lu's mechanisms which require diffusion terms in the 
! reaction rate evaluation added 18Feb2009.

  implicit none
!-----------------------------------------------------------------------------------------
! integers

  integer leni          !length of chemkin integer work array
  integer lenr          !length of chemkin real work array
  integer lenc          !length of chemkin character work array
  integer n_elements    !number of elements in reaction mechanism
  integer n_species     !number of species in reaction mechanism
  integer n_reactions   !number of reactions in reaction mechanism

! arrays

  integer,  allocatable, dimension(:) :: ickwrk       !chemkin integer work array
  real,     allocatable, dimension(:) :: rckwrk       !chemkin real work array
  character*16, allocatable, dimension(:) :: cckwrk   !chemkin character work array

! character arrays

  character*16, allocatable :: element_name(:)        !element names
  character*16, allocatable :: species_name(:)        !species names

! real arrays

  real, allocatable :: molwt(:)         !molecular weight
  real, allocatable :: molwt_c(:)       !inverse molecular weight

#ifdef GETRATES_NEEDS_DIFFUSION
! real scalars
  real tchem_fast  ! the shortest time scale retained by the stiffness reduction routine 
#endif

  integer, parameter :: MAXVL=24 !Used in getrates
!-----------------------------------------------------------------------------------------
  contains
!=========================================================================================
  subroutine allocate_chemkin_arrays(flag)
!=========================================================================================
! allocate chemkin arrays
!-----------------------------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed

  integer flag
!-----------------------------------------------------------------------------------------
! chemkin arrays

  if(flag.eq.1) then

    allocate(ickwrk(leni));           ickwrk=0
    allocate(rckwrk(lenr));           rckwrk=0.0
    allocate(cckwrk(lenc));           cckwrk=' '

!   note that element_name, species_name, molwt, and molwt_c are initialized individually

  elseif(flag.eq.-1) then

    deallocate(ickwrk)
    deallocate(rckwrk)
    deallocate(cckwrk)

    deallocate(element_name)
    deallocate(species_name)

    deallocate(molwt)
    deallocate(molwt_c)

  endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine allocate_chemkin_arrays
!========================================================================================
  subroutine initialize_chemkin(io,myid,ierr,gcomm)
!========================================================================================
! This routine initializes the chemkin work arrays based on the chem.asc file.
!
! Notes:
! 1. The chem.asc file is an unformatted file. Be careful, some versions of
!    Chemkin require binary link files (chem.bin). This DNS code would not be
!    compatible with such versions.
!
!----------------------------------------------------------------------------------------
  implicit none
!-----------------------------------------------------------------------------------------
  include 'mpif.h'
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io,myid,ierr,gcomm

! local declarations

  integer iflag, ifile
  character*100 filename
  logical exist
  integer i
!-----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing chemkin module...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! set filename and inquire

  filename='../input/chem.asc'
  call inquire_about_input_file(filename,io)
!----------------------------------------------------------------------------------------
! initialize chemkin arrays on myid=0

  if(myid.eq.0) then

!   set ifile

    ifile=1

!   write header

    write(io,*) 'opening chemkin link file...'

!   open file

    open(unit=ifile,file=trim(filename),form='formatted',status='old')

!   set chemkin work array lengths

    write(io,*) 'setting chemkin work array lengths...'

    call cklen(ifile,io,leni,lenr,lenc,iflag)

    if(iflag.gt.0) then
      write(io,*) 'error setting chemkin work array lengths'
      write(io,*) 'iflag = ',iflag
      write(io,*) 'see routines cklen and initialize_chemkin'
    endif

  endif

  call MPI_Bcast(iflag,1,MPI_INTEGER,0,gcomm,ierr)

  if(iflag.gt.0) call terminate_run(io,0)  !must be called by all processors
!----------------------------------------------------------------------------------------
! all processors do this...

! broadcast chemkin work array lengths

  call MPI_Bcast(leni    ,1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(lenr    ,1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(lenc    ,1, MPI_INTEGER, 0, gcomm, ierr)

! allocate chemkin arrays

  if (myid==0) write(io,*) 'allocating chemkin work arrays...'

  call allocate_chemkin_arrays(1)

  if (myid == 0) then

!   fill chemkin work arrays

    write(io,*) 'filling chemkin work arrays based on chemkin link file...'

    call ckinit(leni,lenr,lenc,ifile,io,ickwrk,rckwrk,cckwrk,iflag)

!   check for error

    if(iflag.gt.0) then
      write(io,*) 'error initializing chemkin work arrays'
      write(io,*) 'iflag = ',iflag
      write(io,*) 'see routine ckinit and chemkin_init'
    endif

!   close chem.asc file

    close(ifile)

  endif

  call MPI_Bcast(iflag,1,MPI_INTEGER,0,gcomm,ierr)

  if(iflag.gt.0) call terminate_run(io,0)  !must be called by all processors
!----------------------------------------------------------------------------------------
! broadcast chemkin work arrays

  call MPI_Bcast(ickwrk, leni,    MPI_INTEGER,   0, gcomm, ierr)
  call MPI_Bcast(rckwrk, lenr,    MPI_REAL8,     0, gcomm, ierr)
  call MPI_Bcast(cckwrk, lenc*16, MPI_CHARACTER, 0, gcomm, ierr)

! call routine to set ckstrt.h parameters on other processes
! this must be done in an F77 routine for compatability reasons with the header file
  
  call ckHeaderSetup( )
!----------------------------------------------------------------------------------------
! get number of elements, species, and reactions

  call set_number_elem_spec_reac(n_elements,n_species,n_reactions,myid,io)

! allocate species and element names

  allocate(element_name(n_elements));     element_name=' '
  allocate(species_name(n_species));      species_name=' '

! set element names

  do i=1,n_elements,1
    element_name(i)=cckwrk(i)
  enddo

! set species names

  do i=1,n_species,1
    species_name(i)=trim(cckwrk(i+n_elements))
  enddo

! write names to output file

  if(myid.eq.0) then

    write(io,*) 'the following was extracted from the ../input/chem.asc file:'
    write(io,*)

    write(io,*) 'element names are as follows:'
    write(io,*) 'index   name'

    do i=1,n_elements,1
      write(io,'(1x,i3,5x,a16)') i, element_name(i)
    enddo
    write(io,*)

    write(io,*) 'species names are as follows:'
    write(io,*) 'index   name'

    do i=1,n_species,1
      write(io,'(1x,i3,5x,a16)') i, species_name(i)
    enddo
    write(io,*)

  endif
!----------------------------------------------------------------------------------------
! allocate molecular weights

  allocate(molwt(n_species));       molwt=0.0
  allocate(molwt_c(n_species));     molwt_c=0.0

! set molecular weights from chemkin arrays

  call ckwt(ickwrk,rckwrk,molwt)    !kg/kmole or g/mole
  molwt=molwt/1000.0                !convert to kg/mole

! write molecular weights

  if(myid.eq.0) then

    write(io,*) 'species molecular weights are as follows:'
    write(io,*) 'index   name  value (kg/mol)'

    do i=1,n_species,1
      write(io,'(1x,i3,5x,a16,1x,1pe12.5)') i, species_name(i), molwt(i)
    enddo
    write(io,*)

  endif

! set inverse molecular weights

  molwt_c(:)=1.0/molwt(:)


!----------------------------------------------------------------------------------------
! read in parameters that are specific to the stiffness removal
#ifdef GETRATES_NEEDS_DIFFUSION
  filename='../input/stiff.in'
  call inquire_about_input_file(filename,io)
!----------------------------------------------------------------------------------------
! read stiff parameters on myid=0

  if(myid.eq.0) then

!   set ifile
    ifile=1

!   write header
    write(io,*) 'opening stiff.in ...'

!   open file
    open(unit=ifile,file=trim(filename),form='formatted',status='old')

    read(1,*) tchem_fast

!   leave tchem_fast in SI units

! end of read

  close(1)

  endif
!-----------------------------------------------------------------------------------------
! broadcast mode

  call MPI_Bcast(tchem_fast  ,  1, MPI_REAL8    , 0, gcomm, ierr)
  if(myid.eq.1)write(io,*)'Fastest chemical timescale is: ',tchem_fast,' (s)'
#endif
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_chemkin

!Changes 
! - by Ramanan - 01/06/05
! Redefined to call the newer reaction_rate routine with bounds set to everything
!=========================================================================================
#ifdef GETRATES_NEEDS_DIFFUSION
  subroutine reaction_rate(rr_r,temp,pressure,yspecies, &
                             diffusion,dt,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  subroutine reaction_rate(rr_r,temp,pressure,yspecies, g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
!=========================================================================================
! driver for autogetrates
! routine basically performs appropriate
! unit conversions before and after call to getrates
!-----------------------------------------------------------------------------------------
  use param_m, only : nx,ny,nz
#ifdef VECTORVERSION
  use variables_m, only: q
#endif

  implicit none

  real, intent(out), dimension(nx,ny,nz,n_species) :: rr_r
  real, intent(in), dimension(nx,ny,nz)            :: temp, pressure
  real, intent(in), dimension(nx,ny,nz,n_species)  :: yspecies

#ifdef GETRATES_NEEDS_DIFFUSION
  real, intent(inout) :: g_ref, rho_ref, a_ref, l_ref, t_o, dt
  real, intent(in), dimension(nx,ny,nz,n_species)  :: diffusion
#else
  real, intent(in) :: g_ref, rho_ref, a_ref, l_ref, t_o
#endif

#ifdef VECTORVERSION
! local declarations
  real rateconv
  integer n
                                                                                                                                                         
  rateconv = l_ref * 1.0e6 / (rho_ref * a_ref)
  call vecrates(pressure, temp, yspecies, q(:,:,:,4,1), rr_r)
  do n = 1, n_species
    rr_r(:,:,:,n) = rr_r(:,:,:,n)*rateconv*molwt(n)
  end do
#else
#ifdef USE_ACML_VEC_CHEM
#ifdef GETRATES_NEEDS_DIFFUSION  
  call reaction_rate_vec &
      (rr_r,temp,pressure,yspecies,diffusion,dt,g_ref,rho_ref,a_ref,l_ref,t_o, &
       1, nx*ny*nz)
#else      
  call reaction_rate_vec &
      (rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o, &
       1, nx*ny*nz)
#endif
       
#else
#ifdef GETRATES_NEEDS_DIFFUSION  
  call reaction_rate_bounds &
      (rr_r,temp,pressure,yspecies,diffusion,dt,g_ref,rho_ref,a_ref,l_ref,t_o, &
       1, nx, 1, ny, 1, nz)
#else      
  call reaction_rate_bounds &
      (rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o, &
       1, nx, 1, ny, 1, nz)
#endif

#endif
#endif
!----------------------------------------------------------------------------------------
  return
  end subroutine reaction_rate


! New routine by Ramanan Sankaran - 01/06/05
! Sometimes we dont need to compute the reaction rates at all points .
! But just within specified limits, like say the boundary plane alone.
! In those situations we can specify the bounds and compute for those points alone.
! For example, the RHS module, when using ARK integrator needs the rr_r 
! only at the boundaries. It was being unnecessarily computed everywhere.
! That is why a new subroutine was needed.
! It is not good to have many different versions of a subroutine
! So i have redefined the old reaction_rate subroutine to call this new routine
! with the bounds set to the whole domain.
! This might have been better done with optional arguments.
! Then we wont need two subroutines.
!=========================================================================================
#ifdef GETRATES_NEEDS_DIFFUSION
  subroutine reaction_rate_bounds &
      (rr_r,temp,pressure,yspecies,diffusion,dt,g_ref,rho_ref,a_ref,l_ref,t_o,  &      
       ixl, ixu, jyl, jyu, kzl, kzu)
#else      
  subroutine reaction_rate_bounds &
      (rr_r,temp,pressure,yspecies, g_ref,rho_ref,a_ref,l_ref,t_o,  &
       ixl, ixu, jyl, jyu, kzl, kzu)
#endif
!=========================================================================================
! driver for autogetrates
! routine basically performs appropriate
! unit conversions before and after call to getrates
!-----------------------------------------------------------------------------------------
  implicit none

  integer, intent(in) :: ixl, ixu, jyl, jyu, kzl, kzu
  real, intent(in), dimension(ixl:ixu,jyl:jyu,kzl:kzu) :: temp, pressure
  real, intent(in), dimension(ixl:ixu,jyl:jyu,kzl:kzu,n_species)  :: yspecies
  real, intent(out), dimension(ixl:ixu,jyl:jyu,kzl:kzu,n_species) :: rr_r

  real, intent(in) :: g_ref, rho_ref, a_ref, l_ref, t_o

! local declarations

  real, dimension(n_species) :: rr_r1   ! temporary array
  integer i, j, k
  real tconv, pconv, rateconv

! Added by Ramanan - 04/07/05
! Due to the seaborg IBM executing malloc and free we are manually creating an array
! to use for that purpose. Hoping to reduce some CPU load.
  real, dimension(n_species) :: yspec

#ifdef GETRATES_NEEDS_DIFFUSION
  real, intent(in), dimension(ixl:ixu,jyl:jyu,kzl:kzu,n_species) :: diffusion
  real, intent(in) :: dt
  real timeconv, diffconv
  real, dimension(n_species) :: diff
#endif


!----------------------------------------------------------------------------------------
! set conversion factors
!----------------------------------------------------------------------------------------
! T_nondimensional = T_dimensional/ [(gamma_inf -1)* T_inf]
! where gamma_inf = g_ref, and T_inf=to.  Therefore, T_dimensional
! will be in degrees Kelvin.

  tconv = (g_ref-1.0)*t_o

! Pressure_nondimensional = pressure_dimensional/(rho_ref * a_ref^2)
! Therefore pressure_dimensional will be in units of kg/(m s^2)
! The factor of 10 is to convert from SI to CGS units

  pconv = rho_ref*(a_ref**2)*10.0

! non-dimensionalize reaction rate by converting to SI from CGS (1e6/1e3) and
! then multiplying by l_ref/(rho_ref*a_ref)
! there is an extra 1000 in the numerator for the molecular weight conversion

  rateconv = l_ref * 1.0e6 / (rho_ref * a_ref)

#ifdef GETRATES_NEEDS_DIFFUSION
! time_nondimensional = time_dimensional/ (l_ref/a_ref)  

  timeconv = l_ref/a_ref

! diffusion = rho*dY/dt [kg/m^3/sec]
! The factor of 1/1000 is to convert from SI to CGS

  diffconv = rho_ref/timeconv/1.0e3

#endif
!----------------------------------------------------------------------------------------
! get reaction rate from getrates and convert units

  do k = kzl,kzu
    do j = jyl,jyu
      do i = ixl, ixu

        yspec(:) = yspecies(i, j, k, :)
#ifdef GETRATES_NEEDS_DIFFUSION
        diff(:)  = diffusion(i,j,k,:)
#endif

#ifndef USE_ACML_VEC_CHEM
#ifdef GETRATES_NEEDS_DIFFUSION
        call getrates(pressure(i,j,k)*pconv,temp(i,j,k)*tconv,  &
                      yspec,diff*diffconv,max(tchem_fast,dt*timeconv),ickwrk,rckwrk,rr_r1)
#else
        call getrates(pressure(i,j,k)*pconv,temp(i,j,k)*tconv,  &
                      yspec,ickwrk,rckwrk,rr_r1)
#endif
#endif
        rr_r(i,j,k,:) = rr_r1(:) * rateconv * molwt(:)

      enddo
    enddo
  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine reaction_rate_bounds
!-----------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------
! Call a vector getrates 
!=========================================================================================
#ifdef GETRATES_NEEDS_DIFFUSION
  subroutine reaction_rate_vec &
      (rr_r,temp,pressure,yspecies,diffusion,dt, g_ref,rho_ref,a_ref,l_ref,t_o,  &
       ml, mu)
#else      
  subroutine reaction_rate_vec &
      (rr_r,temp,pressure,yspecies, g_ref,rho_ref,a_ref,l_ref,t_o,  &
       ml, mu)
#endif
!=========================================================================================
! driver for autogetrates
! routine basically performs appropriate
! unit conversions before and after call to getrates
!-----------------------------------------------------------------------------------------
  implicit none

  integer, intent(in) :: ml, mu
  real, intent(in), dimension(ml:mu) :: temp, pressure
  real, intent(in), dimension(ml:mu,n_species)  :: yspecies
  real, intent(out), dimension(ml:mu,n_species) :: rr_r

  real, intent(in) :: g_ref, rho_ref, a_ref, l_ref, t_o

! local declarations
! Vary the stride lengths - Ramanan
!  integer, parameter :: ic = 20 
! Commented out because nathan specified it as a parameter MAXVL
  integer :: nu

  real, dimension(1:MAXVL,n_species) :: rr_r1   ! temporary a vnm.	3WSXrray
  real, dimension(1:MAXVL) :: ptemp,ttemp ! temporary array
  integer i, m
  real tconv, pconv, rateconv

  real, dimension(1:MAXVL,n_species) :: yspec
  
#ifdef GETRATES_NEEDS_DIFFUSION
  real, intent(in), dimension(ml:mu,n_species) :: diffusion
  real, intent(out) :: dt
  real timeconv, diffconv
  real, dimension(1:MAXVL,n_species) :: diff
#endif

!  integer tid, nthread
!  integer, external :: omp_get_thread_num, omp_get_num_threads

!----------------------------------------------------------------------------------------
! set conversion factors
!----------------------------------------------------------------------------------------
! T_nondimensional = T_dimensional/ [(gamma_inf -1)* T_inf]
! where gamma_inf = g_ref, and T_inf=to.  Therefore, T_dimensional
! will be in degrees Kelvin.

  tconv = (g_ref-1.0)*t_o

! Pressure_nondimensional = pressure_dimensional/(rho_ref * a_ref^2)
! Therefore pressure_dimensional will be in units of kg/(m s^2)
! The factor of 10 is to convert from SI to CGS units

  pconv = rho_ref*(a_ref**2)*10.0

! non-dimensionalize reaction rate by converting to SI from CGS (1e6/1e3) and
! then multiplying by l_ref/(rho_ref*a_ref)
! there is an extra 1000 in the numerator for the molecular weight conversion

  rateconv = l_ref * 1.0e6 / (rho_ref * a_ref)
  
#ifdef GETRATES_NEEDS_DIFFUSION
! time_nondimensional = time_dimensional/ (l_ref/a_ref)  

  timeconv = l_ref/a_ref

! diffusion = rho*dY/dt [kg/m^3/sec]
! The factor of 1/1000 is to convert from SI to CGS

  diffconv = rho_ref/timeconv/1.0e3
#endif
!----------------------------------------------------------------------------------------
! get reaction rate from getrates and convert units
! openmp for getrates -Ramanan (02/08)
!!!$omp parallel do default(shared), private(m, nu, yspec, ptemp, ttemp, rr_r1), schedule(static, 48)
  do m = ml, mu, MAXVL
    nu = min(MAXVL, mu-m+1)
    ptemp(1:nu) = pressure(m:m+nu-1)*pconv
    ttemp(1:nu) = temp(m:m+nu-1)*tconv

    do i = 1, n_species
      yspec(1:nu, i) = yspecies(m:m+nu-1,i)
    end do
#ifdef GETRATES_NEEDS_DIFFUSION
    do i = 1, n_species
      diff(1:nu, i)  = diffusion(m:m+nu-1,i)
    end do
#endif

! Check to see if this routine will be linked in so we don't get undefs if
! we're not using it
#ifdef USE_ACML_VEC_CHEM
#ifdef GETRATES_NEEDS_DIFFUSION
    call getrates_i(ptemp,ttemp,nu,yspec,diff*diffconv,max(tchem_fast,dt*timeconv),ickwrk,rckwrk,rr_r1)
#else
    call getrates_i(ptemp,ttemp,nu,yspec,ickwrk,rckwrk,rr_r1)
#endif
    do i = 1, n_species
      rr_r(m:m+nu-1,i) = rr_r1(1:nu,i)*rateconv*molwt(i)
    end do
#endif      
  end do
!!!$omp end parallel do
!----------------------------------------------------------------------------------------
  return
  end subroutine reaction_rate_vec
!-----------------------------------------------------------------------------------------

  end module chemkin_m

#include "globalDefines.h"
!========================================================================================
  subroutine write_temporal_data(io,iopen)
!========================================================================================
! routine writes a time series of data extracted from the savefiles
! for example, the maximum value of a derived field
! routine writes tecplot file.
! Author - Edward Richardson (1/11/10)
! Modified from Ramanan's write_tecplot_skip
!----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec
  use reference_m
  use grid_m, only : x, y, z
  use variables_m, only : q, u, temp, pressure, yspecies, volum !volum added by ERH
  use chemkin_m, only : species_name, n_species, element_name, n_elements, reaction_rate
#ifdef MIXAVG
  use transport_m, only : computeCoefficients,getDiffusionCoeff   !MixAvg
#endif
#ifdef LEWIS
  use transport_m, only : computeCoefficients                     !Lewis
#endif

! only for old thermchem (comment for new thermchem):
!  use thermchem_m, only : rr_r
  use thermchem_m, only: Cpmix

  use runtime_m, only : run_title, time, i_time, tstep

  use work_m, only : vort_mag => work1_1
  use work_m, only : hr => work1_2

  use mixfrac_m, only : mixfrac, specToMixfr
  use lesfilt_m

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io       !output unit
  integer iopen    !switch for new file (1) /append (2) filename 'temporal.tec'

! local declarations

  integer i,j,k,L
  real l_conv_cgs, a_conv_cgs, rho_conv_cgs, t_conv_cgs, dil_conv_cgs, hr_conv_cgs
  real rr_conv_cgs
  real p_conv_atm

  real, dimension(nx,ny,nz,3) :: vort
  real, dimension(nx,ny,nz) :: dil, work1, work2

! comment out for old thermchem
  real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion
  real, dimension(nx,ny,nz) :: filteredfield, chi

  real, dimension(nx_g) :: x_g
  real, dimension(ny_g) :: y_g
  real, dimension(nz_g) :: z_g
 
  real :: max_temp_grad, avg_temp_grad 
  real :: max_cprog,max_cprog_g, max_crr
  real, dimension(nx,ny,nz) :: cprog, crr
! need to declare
  real :: max_crr1,c_max_crr  
  integer :: iproc_max
  real, dimension(npes) :: max_crr_g,c_max_crr_g

  character*9 time_ext
  character*100 filename,filename_short

  integer nx_w, ny_w, nz_w, mskp

!----------------------------------------------------------------------------------------
if(myid.eq.0)then
!----------------------------------------------------------------------------------------
! if iopen.eq.1 then processor zero opens a new file and write the header
if(iopen.eq.1)then 

     open(unit=197,file='../post/tecplot/temporal.tec')
!     write(197,*) ' Temporal data '

!!   write variables labels
!
!    write(78,*) '"u (cm/s)"'
!    write(78,*) '"v (cm/s)"'
!!    write(78,*) '"w (cm/s)"'
!!    write(78,*) '"rho (g/cm^3)"'
!    write(78,*) '"T (K)"'
!    write(78,*) '"P (atm)"'
!!    write(78,*) '"dil (1/s)"'
!!    write(78,*) '"vort_x (1/s)"'
!!    write(78,*) '"vort_y (1/s)"'
!!    write(78,*) '"vort_z (1/s)"'
!!    write(78,*) '"vort mag (1/s)"'
!    write(78,*) '"heat release (erg/cm^3/s)"'
!    do L=1,n_species,1
!!      if(trim(species_name(L)).eq.'CH4')then
!        write(78,*) '"Y '//trim(species_name(L))//'"'
!!      endif
!    enddo
!    do L=1,n_species,1
!        write(78,*) '"RR '//trim(species_name(L))//' (1/s)"'
!    enddo
!     write(78,*) '"Z (-)"'
!!Mixfrac     write(78,*) '"Chi (1/s)"'
!!Filtering     write(78,*) '"Chi filtered 9pt (1/s)"'
!

!----------------------------------------------------------------------------------------
! if iopen.eq.2 then processor zero reopens the old file
else ! iopen
     open(unit=197,file='../post/tecplot/temporal.tec',access='append')
endif !iopen
endif ! myid=0



!----------------------------------------------------------------------------------------
! set reference values in CGS

  l_conv_cgs=l_ref*100.0
  a_conv_cgs=a_ref*100.0
  t_conv_cgs=t_ref
  rho_conv_cgs=rho_ref*(1000.0/100.0**3)
  dil_conv_cgs=(a_ref/l_ref)
  p_conv_atm=p_ref/pres_atm
! Added by Evatt Hawkes 21-AUG-02 for converting reaction rate
  rr_conv_cgs=rho_conv_cgs*a_conv_cgs/l_conv_cgs  ! g/cm^3/s
!  rr_conv_cgs=a_conv_cgs/l_conv_cgs              ! 1/s (remember to multilply rr by volum)
  hr_conv_cgs= cp_ref * t_ref * 1.0e4 * rr_conv_cgs ! [erg/cm^3/s] 

!----------------------------------------------------------------------------------------
! compute field variables

! calculate dilitation

  call computeDivergence(u,dil)

! calculate vorticity

  call calc_vorticity(vort,vort_mag,u)

! calculate reaction rate 

#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion=0.0
  tstep=1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif

! calculate heat release

! new thermchem_m:
  call calc_heat_release(rr_r,hr)

! old thermchem_m:
!  call calc_heat_release(hr)

! calcuate scalar dissipation rate
! calcuate scalar dissipation rate
#ifdef MIXAVG
  call computeCoefficients( pressure, temp, yspecies, q(:,:,:,4,1) )
#endif
#ifdef LEWIS
  call computeCoefficients( Cpmix, temp) 
#endif
  call calc_ScalDissRate(chi)

!! gauravb - calculate maximum temperature gradient
! 
!   call calc_max_temp_grad(temp,max_temp_grad)
!   call calc_avg_temp_grad(temp,avg_temp_grad)
!   if(myid.eq.0) then 
!     open(unit=198,file='max_temp_grad.dat',access='append')
!     write(198,1020) time*time_ref, max_temp_grad, avg_temp_grad
!     close(198)
!   end if
!1020 format (3(1pe13.6))


    cprog=0.0
    crr=0.0

    do L=1,n_species,1
      if(  trim(species_name(L)).eq.'H2' .or. &
           trim(species_name(L)).eq.'H2O'.or. &
           trim(species_name(L)).eq.'CO' .or. &
           trim(species_name(L)).eq.'CO2'      )then
       if(myid.eq.0) write(io,*)'including species ',species_name(L),yspecies(1,1,1,L)
        cprog(:,:,:) = cprog(:,:,:)+yspecies(:,:,:,L)
        crr(:,:,:)   = crr(:,:,:)+rr_r(:,:,:,L)
      endif
    enddo !L species

! find location of maximum heat release 
   max_crr=0.0
   do k=1,nz
     do j=1,ny
       do i=1,nx
         if(crr(i,j,k).gt.max_crr)then
           max_crr=crr(i,j,k)
           c_max_crr = cprog(i,j,k)
         endif
       enddo
     enddo
   enddo
   call MPI_Gather(c_max_crr,1,MPI_REAL8,c_max_crr_g,1,MPI_REAL8,0,gcomm,ierr)
   call MPI_Gather(max_crr,1,MPI_REAL8,max_crr_g,1,MPI_REAL8,0,gcomm,ierr)
! not processor zero finds the which processor had the maximum value of max_crr, and gets the corresponding valud of c_max_rr
   if(myid.eq.0)then
! we compare the gathered values of max_crr to the local value on processor zero
     do i=1,npes
       if(max_crr_g(i).ge.max_crr)then
         iproc_max = i
         max_crr = max_crr_g(i)
       endif
     enddo
! record the value of progress variable at the location (out of the whole domain) corresponding to highest reaction rate.
     c_max_crr = c_max_crr_g(iproc_max)
     max_crr1 = max_crr
   endif

    max_cprog = maxval(cprog(:,:,:))
    CALL MPI_Allreduce(max_cprog, max_cprog_g, 1, MPI_REAL8, MPI_MAX, gcomm,ierr) 
    max_crr = maxval(crr(:,:,:))
    CALL MPI_Allreduce(max_crr, max_crr_g(1), 1, MPI_REAL8, MPI_MAX, gcomm,ierr) 

! check that the all reduce produced the same value for the maximum value of crr.
    if(myid.eq.0)write(io,*)'check the max_crr1,',max_crr1,' equals ',max_crr_g(1)


! write the time, maximum progress variable, and maximum progress variable reaction rate.
!    if(myid.eq.0)write(197,3) time*time_ref,max_cprog_g,max_crr_g(1)*rr_ref
! write the time, progress variable at location of maximum progress variable reaction rate, and maximum progress variable reaction rate.
    if(myid.eq.0)write(197,3) time*time_ref,c_max_crr,max_crr1*rr_ref


 
!filtering ! Initialize filtering
!filtering ! NB. it is necessary to deallocate the filter variables before resetting the filter size.
!filtering   if(initialized_lesfilt) call allocate_lesfilt(-1) 
!filtering   iftype=0
!filtering   ndelta=9
!filtering   if(.not.initialized_lesfilt) call initialize_lesfilt(io)
!filtering   if(myid==0)write(io,*)'filtering field with filter type',iftype,' and ndelta=',ndelta
!filtering   call filt_scalar(chi(:,:,:),filteredfield(:,:,:))
!filtering   call write_tecplot_single_skip(78,filteredfield(:,:,:),1.0/time_ref,iskip)



!----------------------------------------------------------------------------------------
! close files
if(myid.eq.0)close(197)   ! only proc. zero opened it so only proc zero closes it
!----------------------------------------------------------------------------------------
! formatting statements

  3 format(3(1pe12.5,1x))
  4 format(4(1pe12.5,1x))
  5 format(5(1pe12.5,1x))
  6 format(6(1pe12.5,1x))
  7 format(7(1pe12.5,1x))
  8 format(8(1pe12.5,1x))
  9 format(9(1pe12.5,1x))
  10 format(10(1pe12.5,1x))

!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif

!----------------------------------------------------------------------------------------
  return
  end subroutine write_temporal_data
!========================================================================================

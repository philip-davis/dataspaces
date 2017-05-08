#include "globalDefines.h"
!=========================================================================================
  module clookup_m
!=========================================================================================
! module for table lookup for species and temperature in progress variable space
! assumes simple table with equally spaced increments
! Evatt Hawkes July 2003

  implicit none
!-----------------------------------------------------------------------------------------
! variable declarations

  integer  n_tab_pts                         !number of points in the table

  real :: cmin,cmax,delc                     !max, min, interval for c

  real, allocatable :: yspec_tab(:,:)        !tabulated species mass fraction
  real, allocatable :: temp_tab(:)           !tabulated temperature
  real, allocatable :: c_tab(:)              !tabulated c-values

  logical :: initialized_clookup = .false.   !initialization flag
  
!-----------------------------------------------------------------------------------------
  contains

!=========================================================================================
subroutine initialize_clookup(io,flag)
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
  integer :: L,M                     !counters
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if((flag==+1).and.(.not.initialized_clookup))then

    if(myid==0)then
      write(io,*) 'Initialising c-table lookup ...'
      write(io,*)
    endif

!-----------------------------------------------------------------------------------------
!   read the control file
!-----------------------------------------------------------------------------------------
    filename='../input/clookup.in'
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
      read(io_cl,*) n_tab_pts

!     read max, min
      read(io_cl,*) cmax, cmin

!     allocate space for the table  
      allocate(temp_tab(n_tab_pts)); temp_tab=0.0;
      allocate(yspec_tab(n_spec,n_tab_pts)); yspec_tab=0.0;

!     read each line:
      do L=1,n_tab_pts
        read(io_cl,*)  temp_tab(L), (yspec_tab(M,L), M=1,n_spec)
      enddo

!     close file
      close(io_cl)

    endif 

!   broadcast
    call MPI_Bcast(n_tab_pts,1,MPI_INTEGER,0,gcomm,ierr)

!   allocate for other processes
    if(myid.ne.0)then
      allocate(temp_tab(n_tab_pts)); temp_tab=0.0;
      allocate(yspec_tab(n_spec,n_tab_pts)); yspec_tab=0.0;
    endif

!   broadcasts
    call MPI_Bcast(cmax   ,1             ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(cmin   ,1             ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(temp_tab ,n_tab_pts       ,MPI_REAL8,0,gcomm,ierr)
    call MPI_Bcast(yspec_tab,n_tab_pts*n_spec,MPI_REAL8,0,gcomm,ierr)

!   nondimensionalise temperature
    temp_tab=temp_tab/t_ref

!   set interval in c-space
    delc=(cmax-cmin)/real(n_tab_pts - 1)

!   allocate c entries
    allocate(c_tab(n_tab_pts));c_tab=0.0

!   set c entries:
    c_tab(1)=cmin
    do L=2,n_tab_pts
      c_tab(L)=cmin+real(L)*delc
    enddo

!   set initialisation flag
    initialized_clookup=.true.

  elseif((flag==-1).and.(initialized_clookup))then
    if(myid==0)then
      write(io,*) 'Shutting down c-table lookup ...'
      write(io,*)
    endif

!   deallocate
    deallocate(c_tab)
    deallocate(temp_tab)
    deallocate(yspec_tab)

!   update initialisation flag
    initialized_clookup = .false.

  else
    if(myid==0)then
      write(io,*) 'Error calling initialize_clookup, clookup_m'
      write(io,*) 'Expected flag =+1 for initialisation, -1 for clean-up'
      write(io,*) 'Flag= ', flag
      write(io,*) 'ABORTING!!!'
    endif
    call terminate_run(io,0)
  endif

!-----------------------------------------------------------------------------------------
  return
end subroutine initialize_clookup
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine clookup(c,ysp,t,io)
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

  real, dimension(nx,ny,nz,n_spec), intent(out) :: ysp     !species mass fraction
  real, dimension(nx,ny,nz),        intent(out) :: t       !temperature
!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k                   !grid counters
  integer :: i_tab                   !lookup entry
  real :: cfac                       !interpolating factor
  real :: invdelc                    !inverse c-interval
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_clookup) call initialize_clookup(io,+1)

  invdelc=1.0/delc

  do k=1,nz
    do j=1,ny
      do i=1,nx
        
        i_tab=floor((c(i,j,k)-cmin)*invdelc)+1
        i_tab=max(min(i_tab,n_tab_pts-1),1)
        cfac=(c(i,j,k)-c_tab(i_tab))*invdelc

        t(i,j,k)=temp_tab(i_tab)+(temp_tab(i_tab+1)-temp_tab(i_tab))*cfac
        ysp(i,j,k,:)=yspec_tab(:,i_tab)+(yspec_tab(:,i_tab+1)-yspec_tab(:,i_tab))*cfac

      enddo
    enddo
  enddo

! clean up c-table
! only if not using after this
!  call initialize_clookup(io,-1)

!-----------------------------------------------------------------------------------------
  return
end subroutine clookup
!-----------------------------------------------------------------------------------------

!=========================================================================================
subroutine clookup_1val(c,ysp,t,io)
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

  real, dimension(n_spec), intent(out) :: ysp     !species mass fraction
  real,                    intent(out) :: t       !temperature
!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i_tab                   !lookup entry
  real :: cfac                       !interpolating factor
  real :: invdelc                    !inverse c-interval
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_clookup) call initialize_clookup(io,+1)

  invdelc=1.0/delc


  i_tab=floor((c-cmin)*invdelc)+1
  i_tab=max(min(i_tab,n_tab_pts-1),1)
  cfac=(c-c_tab(i_tab))*invdelc

  t=temp_tab(i_tab)+(temp_tab(i_tab+1)-temp_tab(i_tab))*cfac
  ysp(:)=yspec_tab(:,i_tab)+(yspec_tab(:,i_tab+1)-yspec_tab(:,i_tab))*cfac

!-----------------------------------------------------------------------------------------
  return
end subroutine clookup_1val
!-----------------------------------------------------------------------------------------



!=========================================================================================
subroutine calc_rho0(rho0,rhoL,press_in,io)
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
  real,    intent(out) :: rhoL       !Burnt gas density
  real,    intent(in)  :: press_in   !pressure
  integer, intent(in)  :: io         !io unit
!-----------------------------------------------------------------------------------------
! local declarations

  real avmolwt !average inverse molecular weight

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_clookup) call initialize_clookup(io,+1)

  avmolwt=sum(yspec_tab(:,1)*molwt_c(:))

! set density in q-vector (equation of state)

  rho0=press_in/(univ_gascon*(1.0/a_ref**2)*t_ref*temp_tab(1)*avmolwt)

  avmolwt=sum(yspec_tab(:,n_tab_pts)*molwt_c(:))

  rhoL=press_in/(univ_gascon*(1.0/a_ref**2)*t_ref*temp_tab(n_tab_pts)*avmolwt)

!-----------------------------------------------------------------------------------------
  return
end subroutine calc_rho0
!-----------------------------------------------------------------------------------------

!=========================================================================================
subroutine cgradlookup(c,n1,n2,grad,io)
!=========================================================================================
! does the lookup for a field of c-values and returns the derivative
! of species 1 with respect to species 2.
! uses simple linear interpolation
! assumes constant intervals
!-----------------------------------------------------------------------------------------
  use param_m, only : nx,ny,nz,n_spec

  implicit none

!-----------------------------------------------------------------------------------------
! declarations passed
  integer, intent(in) :: io !io unit
  integer, intent(in) :: n1,n2 !species indices

  real, dimension(nx,ny,nz),        intent(in) :: c        !lookup parameter

  real, dimension(nx,ny,nz),        intent(out) :: grad    !dY1/dY2
!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k                   !grid counters
  integer :: i_tab                   !lookup entry
  real :: cfac                       !interpolating factor
  real :: invdelc                    !inverse c-interval
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

  if(.not.initialized_clookup) call initialize_clookup(io,+1)

  invdelc=1.0/delc

  do k=1,nz
    do j=1,ny
      do i=1,nx
        
        i_tab=floor((c(i,j,k)-cmin)*invdelc)+1
        i_tab=max(min(i_tab,n_tab_pts-1),1)
        cfac=(c(i,j,k)-c_tab(i_tab))*invdelc

!        t(i,j,k)=temp_tab(i_tab)+(temp_tab(i_tab+1)-temp_tab(i_tab))*cfac
!        ysp(i,j,k,:)=yspec_tab(:,i_tab)+(yspec_tab(:,i_tab+1)-yspec_tab(:,i_tab))*cfac

        grad(i,j,k)=(yspec_tab(n1,i_tab+1)-yspec_tab(n1,i_tab)) / &
                    (yspec_tab(n2,i_tab+1)-yspec_tab(n2,i_tab)+1.0e-20)

      enddo
    enddo
  enddo

! clean up c-table
! only if not using after this
!  call initialize_clookup(io,-1)

!-----------------------------------------------------------------------------------------
  return
end subroutine cgradlookup
!-----------------------------------------------------------------------------------------
!=========================================================================================
  end module clookup_m

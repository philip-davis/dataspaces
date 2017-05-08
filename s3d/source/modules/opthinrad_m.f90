#include "globalDefines.h"
!=========================================================================================
  module opthinrad_m
!=========================================================================================
! module for optically thin radiation variables

  implicit none
!-----------------------------------------------------------------------------------------
! integers

  integer :: i_opthinrad = 0  !switch for optically thin radiation
  integer :: i_count_rad      !number of species radiation data sets in opthinrad.in file

! integer arrays

  integer, allocatable :: index_rad(:)    !indexing array

! real arrays

  real, allocatable :: as(:,:)    !radiation coefficient array
!-----------------------------------------------------------------------------------------
  contains
!=========================================================================================
  subroutine allocate_opthinrad_arrays(flag)
!=========================================================================================
! allocate opthinrad arrays
!-----------------------------------------------------------------------------------------
  use param_m, only : nsc

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed

  integer flag
!-----------------------------------------------------------------------------------------
! opthinrad arrays

  if(flag.eq.1) then

    allocate(as(nsc,6));        as=0.0
    allocate(index_rad(nsc));   index_rad=0

  elseif(flag.eq.-1) then

    if(i_opthinrad.eq.1) then

      deallocate(as)
      deallocate(index_rad)

    endif

  endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine allocate_opthinrad_arrays
!=========================================================================================
  subroutine initialize_opthinrad(io)
!=========================================================================================
! routine reads opthinrad file and broadcasts parameters
!----------------------------------------------------------------------------------------
  use topology_m
  use chemkin_m, only : cckwrk
  use param_m, only : nsc, n_elem

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  character*100 filename
  logical exist
  character*16 species_name
  integer ii, k, j
!-----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing opthinrad module...'
    write(io,*)
  endif
!-----------------------------------------------------------------------------------------
! return if no opthinrad

  if(i_opthinrad.ne.1) then
     if(myid==0) then
       write(io,*) 'note: optically thin radiation is turned off!'
        call write_header(io,'-')
     endif
     return
  endif
!-----------------------------------------------------------------------------------------
! allocate arrays

  call allocate_opthinrad_arrays(1)
!-----------------------------------------------------------------------------------------
! set file name and inquire

  filename='../input/opthinrad.in'
  call inquire_about_input_file(filename,io)
!-----------------------------------------------------------------------------------------
! open and read file

  if(myid.eq.0) then

    write(io,*) 'reading optically thin radiation data from ',trim(filename)
    write(io,*)

!   open file

    open(unit=1,file=trim(filename),status='old')

!   zero parameters array

    as=0.0

!   read number of data sets

    read(1,*) i_count_rad

!   read data

    do j = 1, i_count_rad

      read(1,*) species_name
      read(1,*) (as(j,k), k=1,6)

      do ii=n_elem+1,n_elem+nsc,1
        if(trim(cckwrk(ii)).eq.trim(species_name)) then
          index_rad(j) = ii-n_elem
        endif
      enddo

    enddo

    close(1)

  endif
!-----------------------------------------------------------------------------------------
! broadcast opthinrad parameters

  call MPI_Bcast(as,       size(as),       MPI_REAL8  ,0,gcomm,ierr)
  call MPI_Bcast(index_rad,size(index_rad),MPI_INTEGER,0,gcomm,ierr)
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_opthinrad
!=========================================================================================
  subroutine opthinrad(rad_term,yspecies,pressure,temp)
!=========================================================================================
! computes optically-thin radiation source term for energy equation
!-----------------------------------------------------------------------------------------
  use param_m
  use chemkin_m, only : molwt_c
  use reference_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real pressure(nx,ny,nz)                             ! input
  real temp(nx,ny,nz)                                 ! input
  real yspecies(nx,ny,nz,nsc+1)                       ! input
  real rad_term(nx,ny,nz)                             ! output (work array)

! local declarations

  real x_mol(nsc+1)
  real dkp(nsc)
  real ckp, rad_conv, pconv, sigma_rad, t_inf
  real sum

  integer i, j, k, isc
!-----------------------------------------------------------------------------------------
! set data

  data sigma_rad /5.670E-8/
  t_inf = t_o  !ambient temperature
!-----------------------------------------------------------------------------------------
! set reference pressure and energy

  pconv = p_ref/pres_atm  !conversion factor
  rad_conv = l_ref / rho_ref / a_ref**3.0
!-----------------------------------------------------------------------------------------
! compute species mole fraction

  do i = 1, nx*ny*nz

    rad_term(i,1,1) = 0.0
    sum = 0.0
    ckp = 0.0

    do isc = 1, nsc+1
      x_mol(isc) = yspecies(i,1,1,isc) * molwt_c(isc)
      sum = sum + x_mol(isc)
    enddo

    do isc = 1, nsc+1
      x_mol(isc) = x_mol(isc) / sum
    enddo

    do j = 1, i_count_rad

      dkp(j) = as(j,6)

      do k = 5, 1, -1
        dkp(j) = dkp(j)*temp(i,1,1)*t_ref + as(j,k)
      enddo

      if(index_rad(j).gt.0) then
        ckp = ckp + x_mol(index_rad(j))*pressure(i,1,1)*pconv*dkp(j)
      endif

    enddo

    rad_term(i,1,1) = - 4.0 * sigma_rad * ckp * ((temp(i,1,1)*t_ref)**4.0 - t_inf**4.0)

!   non-dimensionalization

    rad_term(i,1,1) = rad_term(i,1,1) * rad_conv

  enddo
!-----------------------------------------------------------------------------------------
  return
  end subroutine opthinrad
!----------------------------------------------------------------------------------------
  end module opthinrad_m

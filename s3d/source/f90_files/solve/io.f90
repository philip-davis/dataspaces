#include "globalDefines.h"
!----------------------------------------------------------------------
!  modifications
!  early 2005: Ramanan Sankaran added capability for io from directories
!                               instead of tar-balls
!  17-02-2005: Evatt Hawkes.  Bug fix of new io capability - bug on restarts
!  14-05-2008: Ed Richardson. Impemented Ramanan's changes for blocking output
!                               see !quad tags.
!  23-09-2008: Ed Richardson. Merged the io.f90 for tracers with the io.f90 for 
!                               morphing/interpolating/extruding on to arbitrary 
!                               grid/processor topology. At present the tracer data
!                               do not get morphed which forces the user to keep 
!                               npy and npz constant.
!=========================================================================================
#ifndef SAVEFILEINSEPDIR
  #error This code can only savefileinsepdir. Not capable of tarring.
#endif
!=========================================================================================
  subroutine read_input(io)
!=========================================================================================
! routine reads input file s3d.in for basic run parameters and broadcasts them
!-----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : run_mode
  use param_m, only : nx_g, ny_g, nz_g, npx, npy, npz
  use param_m, only : vary_in_x, vary_in_y, vary_in_z
  use param_m, only : periodic_x, periodic_y, periodic_z
  use param_m, only : iorder, iforder

  use runtime_m, only : i_restart, i_time_end, i_time_save, time_save_inc, time_save
  use runtime_m, only : run_title, i_write, i_time_mon, i_time_res, i_time_tec

  use turbulence_m, only : i_turbulence

  use bc_m, only : nrf_x0, nrf_xl, nrf_y0, nrf_yl, nrf_z0, nrf_zl, relax_ct

  use grid_m, only : xmin, ymin, zmin, xmax, ymax, zmax
  use grid_m, only : unif_grid_x, unif_grid_y, unif_grid_z
  use grid_m, only : min_grid_x, min_grid_y, min_grid_z

  use transport_m, only : pr

  use filter_m, only : i_time_fil

  use reference_m, only : g_ref, a_ref, t_o, rho_ref, lambda_ref, mach_no, re_real

  use thermchem_m, only : i_react

  use param_m, only : io_method

  !use tracer_m, only: tracer_ctrl

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  integer tracer_ctrl ! dummy to replace module variable
  character*100 filename
  logical exist
  integer tracer_dummy
  real ref_dummy


!-----------------------------------------------------------------------------------------
! set file name and inquire

  filename='../input/s3d.in'
  call inquire_about_input_file(filename,io)
!-----------------------------------------------------------------------------------------
! open and read file

  if(myid.eq.0) then
!-----------------------------------------------------------------------------------------
! open file

  open(unit=1,file=trim(filename),status='old')
!-----------------------------------------------------------------------------------------
! read mode

  read(1,*)
  read(1,*)
  read(1,*)

  read(1,*) run_mode
! For compatibility with old version, where `mode' was 0 or 1
  if(run_mode == '0') run_mode = 'solve'
  if(run_mode == '1') run_mode = 'post'
!-----------------------------------------------------------------------------------------
! read grid dimension parameters

  read(1,*)
  read(1,*)
  read(1,*)

  read(1,*) nx_g
  read(1,*) ny_g
  read(1,*) nz_g
  read(1,*) npx
  read(1,*) npy
  read(1,*) npz
!-----------------------------------------------------------------------------------------
! read run-time parameters

  read(1,*)
  read(1,*)
  read(1,*)

  read(1,*) i_write
  read(1,*) i_restart
  read(1,*) i_time_end
  read(1,*) i_time_save
  read(1,*) time_save_inc
!-----------------------------------------------------------------------------------------
! read geometry parameters

  read(1,*)
  read(1,*)
  read(1,*)

  read(1,*) run_title
  read(1,*) vary_in_x
  read(1,*) vary_in_y
  read(1,*) vary_in_z
  read(1,*) periodic_x
  read(1,*) periodic_y
  read(1,*) periodic_z
  read(1,*) unif_grid_x
  read(1,*) unif_grid_y
  read(1,*) unif_grid_z
  read(1,*) min_grid_x
  read(1,*) min_grid_y
  read(1,*) min_grid_z
  read(1,*) i_turbulence
  read(1,*) nrf_x0
  read(1,*) nrf_xl
  read(1,*) nrf_y0
  read(1,*) nrf_yl
  read(1,*) nrf_z0
  read(1,*) nrf_zl
  read(1,*) relax_ct
!-----------------------------------------------------------------------------------------
! read physical parameters

  read(1,*)
  read(1,*)
  read(1,*)

  read(1,*) xmin
  read(1,*) ymin
  read(1,*) zmin
  read(1,*) xmax
  read(1,*) ymax
  read(1,*) zmax
  ! Reference variables are set in that module as parameters. 
  ! The dummy read statements are here to not break s3d.in
  read(1,*) ref_dummy
  read(1,*) ref_dummy
  read(1,*) ref_dummy
!-----------------------------------------------------------------------------------------
! read numerics switches parameters

  read(1,*)
  read(1,*)
  read(1,*)

  read(1,*) i_react
  read(1,*) iorder
  read(1,*) i_time_mon
  read(1,*) i_time_res
  read(1,*) i_time_tec
  read(1,*) iforder
  read(1,*) i_time_fil
!-----------------------------------------------------------------------------------------
! read required reference values

  read(1,*)
  read(1,*)
  read(1,*)

  read(1,*) ref_dummy
  read(1,*) ref_dummy
  read(1,*) ref_dummy
  read(1,*) ref_dummy
  read(1,*) ref_dummy
!-----------------------------------------------------------------------------------------
! tracer control
  read(1,*)                 ! 3 comment lines
  read(1,*)
  read(1,*)

  read(1,*) tracer_dummy   !this line causes some problem on Jaguar
  !tracer_ctrl=tracer_dummy
  !bug???  read(1,*) tracer_ctrl
  !tracer_ctrl=0
!-----------------------------------------------------------------------------------------
! I/O Control
  read(1,*)                 ! 3 comment lines
  read(1,*)
  read(1,*)

  read(1,*) io_method

!-----------------------------------------------------------------------------------------

! end of read

  close(1)

  endif
!-----------------------------------------------------------------------------------------
! broadcast mode

  call MPI_Bcast(run_mode, 20, MPI_CHARACTER, 0, gcomm, ierr)
!-----------------------------------------------------------------------------------------
! broadcast grid parameters

  call MPI_Bcast(nx_g    , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(ny_g    , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(nz_g    , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(npx     , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(npy     , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(npz     , 1, MPI_INTEGER, 0, gcomm, ierr)
!-----------------------------------------------------------------------------------------
! broadcast run-time parameters

  call MPI_Bcast(i_write        , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(i_restart      , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(i_time_end     , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(i_time_save    , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(time_save_inc  , 1, MPI_REAL8  , 0, gcomm, ierr)
!-----------------------------------------------------------------------------------------
! broadcast geometry parameters

  call MPI_Bcast(run_title   , 20, MPI_CHARACTER, 0, gcomm, ierr)
  call MPI_Bcast(vary_in_x   ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(vary_in_y   ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(vary_in_z   ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(periodic_x  ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(periodic_y  ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(periodic_z  ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(unif_grid_x ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(unif_grid_y ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(unif_grid_z ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(min_grid_x  ,  1, MPI_REAL8    , 0, gcomm, ierr)
  call MPI_Bcast(min_grid_y  ,  1, MPI_REAL8    , 0, gcomm, ierr)
  call MPI_Bcast(min_grid_z  ,  1, MPI_REAL8    , 0, gcomm, ierr)
  call MPI_Bcast(i_turbulence,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(nrf_x0      ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(nrf_xl      ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(nrf_y0      ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(nrf_yl      ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(nrf_z0      ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(nrf_zl      ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(relax_ct    ,  1, MPI_REAL8    , 0, gcomm, ierr)
!-----------------------------------------------------------------------------------------
! broadcast physical parameters

  call MPI_Bcast(xmin   , 1, MPI_REAL8 , 0, gcomm, ierr)
  call MPI_Bcast(ymin   , 1, MPI_REAL8 , 0, gcomm, ierr)
  call MPI_Bcast(zmin   , 1, MPI_REAL8 , 0, gcomm, ierr)
  call MPI_Bcast(xmax   , 1, MPI_REAL8 , 0, gcomm, ierr)
  call MPI_Bcast(ymax   , 1, MPI_REAL8 , 0, gcomm, ierr)
  call MPI_Bcast(zmax   , 1, MPI_REAL8 , 0, gcomm, ierr)
!-----------------------------------------------------------------------------------------
! broadcast numerics parameters

  call MPI_Bcast(i_react      ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(iorder       ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(i_time_mon   ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(i_time_res   ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(i_time_tec   ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(iforder      ,  1, MPI_INTEGER  , 0, gcomm, ierr)
  call MPI_Bcast(i_time_fil   ,  1, MPI_INTEGER  , 0, gcomm, ierr)
!-----------------------------------------------------------------------------------------
! broadcast required tracer values

  call MPI_Bcast(tracer_ctrl  , 1, MPI_INTEGER , 0, gcomm, ierr)
!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
! broadcast io method control

  call MPI_Bcast(io_method,   1, MPI_INTEGER, 0, gcomm, ierr)
!-----------------------------------------------------------------------------------------

! set time_save equal to time_save_inc initially

  time_save = time_save_inc

  return
  end subroutine read_input

!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine write_savefile(io,precsn)
use topology_m, only : myid, gcomm, ierr, npes, zid, zpes, zcomm !quad
use runtime_m, only : i_time, run_title, time
use reference_m, only : time_ref
use param_m, only : nx, ny, nz, n_spec  !quad

! wkliao: paramaters related to MPI-IO, pnetcdf, and hdf5
use param_m, only : io_method
#ifdef MPIIO
use mpi_io_m,  only : mpi_file_io
use pnetcdf_m, only : pnetcdf_write
use hdf5_m,    only : hdf5_write
#endif

implicit none
integer,intent(in) :: io
character, intent(in) :: precsn ! s or d

character*5 :: myid_ext*5, time_ext*10, filename*100, dirname*100  !5sf
integer mz
logical exist
character*100 tarcmd, copycmd1, copycmd2, delcmd
!-----------------------------------------------------------------------------------------
! Added by Ramanan to measure I/O rate
 integer time_start(8), time_end(8)
 real io_size
 real time_taken

call MPI_Barrier( gcomm, ierr )

write(time_ext,'(1pe10.4)') time*time_ref       
write(myid_ext, '(I5.5)') myid


! wkliao: set a different file name for MPI-IO, pnetcdf, and HDF5 methods
! wkliao: no subdirectories are created if io_method > 0
if (io_method .EQ. 1) then
    filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.mpi'
elseif (io_method .EQ. 2) then
    filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.nc'
elseif (io_method .EQ. 3) then
    filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.h5'
elseif (io_method .EQ. 4) then
    filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.bp'
elseif (io_method .EQ. 5) then
    filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.bp'
else
dirname = '../data/'//precsn//'-'//trim(time_ext)//'/'

!----------------------------------------
if(myid == 0) then
#ifdef SYSTEMCALLWONTWORK
  call makedirectory(trim(dirname)//char(0))
#else
  call execute_command( 'mkdir -p '//trim(dirname))
#endif
end if

!All processes need to wait for the directory to be created
call MPI_Barrier( gcomm, ierr )

filename=trim(dirname)//'field.'//myid_ext

endif


!----------------------------------------
call MPI_Barrier(gcomm, ierr)
if(myid.eq.0) then
  write(io,100) 'writing save    files for: i_time = ',&
              i_time, ', time = ',time*time_ref,' (sec)'
  write(io, *) 'precision is ', precsn
  call date_and_time(values=time_start(:))
end if
100 format(1x,a,i7,a,1pe10.4,a)  !5sf

!----------------------------------------

if (io_method .GT. 0) then
    ! first delete the file if exists
    if (myid == 0) then
        inquire(file=trim(filename),exist=exist)
        if (exist) then
            tarcmd = 'rm -f '//trim(filename)
            call execute_command( trim(tarcmd) )
        endif
    endif
    if (io_method .EQ. 1) then
#ifdef MPIIO
        call mpi_file_io(filename, 'w')
#endif
    elseif (io_method .EQ. 2) then
#ifdef MPIIO
        call pnetcdf_write(filename)
#endif
    elseif (io_method .EQ. 3) then
#ifdef MPIIO
        call hdf5_write(filename)
#endif
    elseif (io_method .EQ. 5) then
#ifdef ADIOS
        call readwrite_savefile_data_adios(filename,'w')
#endif
    endif
else

#ifdef BLOCKIO
! I/O blocking by Ramanan
loop_z: do mz = 0, zpes-1
   call MPI_Barrier(zcomm, ierr)
   if(zid .ne. mz) cycle
   open(unit=201,file=trim(filename),status='unknown',form='unformatted')
   if(precsn == 's') then
      call readwrite_singprec_data(io,201,'w')
   else
      call readwrite_savefile_data(io,201,'w')
   end if
end do loop_z
close(201)
#else
   open(unit=201,file=trim(filename),status='unknown',form='unformatted')
   if(precsn == 's') then
      call readwrite_singprec_data(io,201,'w')
   else
      call readwrite_savefile_data(io,201,'w')
   endif
   
   close(201)

#endif

endif

!----------------------------------------
call MPI_Barrier( gcomm, ierr )
!I/O timers by Ramanan.
if(myid .eq. 0) then
  call date_and_time(values=time_end(:))
  call write_header(io,'-')
  write(io,251) time_start(6),time_start(7),time_start(8)
  write(io,252) time_end(6),time_end(7),time_end(8)
  io_size = real(nx*ny*nz*8*(5+n_spec))*1e-6
  write(io, 255) io_size
  io_size = io_size*real(npes)
  write(io, 256) io_size
  time_taken = &
      60.0*real(time_end(6)-time_start(6))&
        +  real(time_end(7)-time_start(7))&
    +0.001*real(time_end(8)-time_start(8))
  write(io,253) time_taken
  write(io,254) io_size/time_taken
  call write_header(io,'-')
end if
if(myid.eq.0) call write_header(io,'-')
call write_log
return

251 format('IO Started at ', I2.2, ':', I2.2, '.', I3.3)
252 format('IO Finished at ', I2.2, ':', I2.2, '.', I3.3)
253 format('Time taken = ', 1F15.3, 's')
254 format('Total IO Rate = ', 1F15.3, 'MB/s')
255 format('Per-thread IO = ', F15.1, 'MB')
256 format('Total IO = ', F15.1, 'MB')

contains
  !----------------------------------------
  subroutine write_log
  implicit none
  logical :: exist

  if(myid .ne. 0) return

  filename='../data/'//trim(run_title)//'.savefile.log'
  inquire(file=trim(filename),exist=exist)

  if(exist) then
    open(unit=1,file=trim(filename),status='old',position='append')
  else
    open(unit=1,file=trim(filename),status='unknown')
    write(1,*) 'time(sec)    timestep   precision'
    write(1,52)
  endif

  if (io_method .EQ. 1) then
     filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.mpi'
  elseif (io_method .EQ. 2) then
     filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.nc'
  elseif (io_method .EQ. 3) then
     filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.h5'
  elseif (io_method .EQ. 4) then
      filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.bp'
  elseif (io_method .EQ. 5) then
      filename = '../data/'//trim(run_title)//'.'//time_ext//'.field.bp'
  else

     filename = trim(run_title)//'.'//time_ext//'.tar'

  endif


  write(1,50) time*time_ref, i_time, precsn
  close(1)

  50 format(1pe10.4, 6x, i7, 6x, a1)  !5sf
  52 format(40('-'))
  end subroutine write_log
 !----------------------------------------
end subroutine write_savefile
!----------------------------------------------------------------------
! reads data file for post-processing and restarting

! WARNING.

! There is no redistribution of tracer data yet so you are 
! constrained by the topology in which you wrote your tracer data.
! But it is possible to change the code to do that in due course.

subroutine read_savefile(io, precsn, dirtime)
use topology_m
!use tracer_m
use runtime_m, only : time

!interp ***********************
use variables_m, only: temp, pressure, yspecies, u 
use param_m, only : nsc, nx_g, ny_g, nz_g
use grid_m, only: delx
use frozenfeed_m, only : write_frozenfeed_data
!interp ***********************

use runtime_m, only : run_title

use param_m, only : io_method
use reference_m, only : time_ref
#ifdef MPIIO
use mpi_io_m,  only : mpi_file_int_io, mpi_io_set_filetype, &
                      mpi_io_set_filetype_interp
use pnetcdf_m, only : pnetcdf_read
use hdf5_m,    only : hdf5_read
#endif

implicit none
integer, intent(in) :: io
character, intent(in), optional :: precsn
character(*), intent(in), optional :: dirtime

character :: dirname*150, filetype, tracerdir*150, filename*150
integer xpu, ypu, zpu  !Topology of data
integer facx, facy, facz 
integer mx, my, mz  !The counterpart of nx, ny, nz
integer i

logical exist
character*10 :: time_ext
character*10 :: dirtime2


!interp ****************
integer mx_g,my_g,mz_g
integer npts,nspec
integer, dimension(9) :: topol_src,topol_int,topol_dst
integer dest_id,n
integer, dimension(40000) :: src_id  ! this is very bad, think of another way,
!  the dimensin of src_id equals the maximum allowable number of processors
!  in the source data.
integer, dimension(3) :: lo,hi
integer, dimension(3) :: is_src,ie_src,is_int,ie_int,is_dst,ie_dst

real pout_src, time_src, tstep_src, time_save_src
real pout_int, time_int, tstep_int, time_save_int
real pout, tstep, time_save !!!!!!!, time

real, allocatable :: yspeciessrc(:,:,:,:)
real, allocatable :: usrc(:,:,:,:)
real, allocatable :: pressuresrc(:,:,:)
real, allocatable :: tempsrc(:,:,:)

real, allocatable :: yspeciesint(:,:,:,:)
real, allocatable :: uint(:,:,:,:)
real, allocatable :: pressureint(:,:,:)
real, allocatable :: tempint(:,:,:)

real, allocatable :: xysurf(:,:,:,:),xysurf_g(:,:,:,:)
real, allocatable :: yzsurf(:,:,:,:),yzsurf_g(:,:,:,:)
real, allocatable :: xzsurf(:,:,:,:),xzsurf_g(:,:,:,:)

character :: feedcase*20
character :: refinecase*20

integer isymm(3),src_off(3),org_off(3),lo_dst(3),hi_dst(3)
integer ixgl,ixgr,ixbndl,ixbndr,j,k,ix
integer kid(3),igl(3),igr(3),ibndl(3),ibndr(3)

nspec=nsc+1
!interp ****************

!----------------------------------------
!call MPI_Barrier( gcomm, ierr )

call get_topologyint ! Get the morph/topology of data

! ALLOCATE the yspeciessrc
  allocate(yspeciessrc(topol_src(7),topol_src(8),topol_src(9),nsc+1));  yspeciessrc=0.0
  allocate(usrc(topol_src(7),topol_src(8),topol_src(9),3));  usrc=0.0
  allocate(pressuresrc(topol_src(7),topol_src(8),topol_src(9)));  pressuresrc=0.0
  allocate(tempsrc(topol_src(7),topol_src(8),topol_src(9)));  tempsrc=0.0

! ALLOCATE the yspeciesint 
  allocate(yspeciesint(topol_int(7),topol_int(8),topol_int(9),nsc+1));  yspeciesint=0.0
  allocate(uint(topol_int(7),topol_int(8),topol_int(9),3));  uint=0.0
  allocate(pressureint(topol_int(7),topol_int(8),topol_int(9)));  pressureint=0.0
  allocate(tempint(topol_int(7),topol_int(8),topol_int(9)));  tempint=0.0

! compare topol_src and topol_dst and see whether any morphing or interpolation is needed

  if(trim(feedcase).ne.'feeddata')then
    call set_dirname ! Decide where to read the data from
    read(dirtime2,'(1pe10.4)')time  !esr: this is not an ideal way to set the time since 
!                                         there can be loss of precision, but hopefully its better 
!                                         than restarting the calculation with time=zero.
    time=time/time_ref
  endif

if (io_method .EQ. 1.and. trim(feedcase).ne.'feeddata') then

   filename = '../data/'//trim(run_title)//'.'//dirtime2//'.field.mpi'
   inquire(file=trim(filename), exist=exist)
   if(exist) then

      if(myid == 0) write(io,*) 'Reading collectively to intermediate arrays'
#ifdef MPIIO

      call mpi_io_set_filetype(-1)  
      call mpi_io_set_filetype_interp(topol_int(7),topol_int(8),topol_int(9),  &  
           topol_int(4),topol_int(5),topol_int(6),1)
      call mpi_file_int_io(yspeciesint,uint,pressureint,tempint,  &
               topol_int(7),topol_int(8),topol_int(9),filename)
      call mpi_io_set_filetype_interp(topol_int(7),topol_int(8),topol_int(9),  &  
           topol_int(4),topol_int(5),topol_int(6),-1)
      call mpi_io_set_filetype(1)  
#endif

      call InterpLocal(myid)
   else
      call MPI_Barrier(gcomm,ierr)
      write(io,*) 'the following restart file does not exist'
      write(io,*) trim(filename)
      call MPI_Barrier(gcomm,ierr)
      call terminate_run(io,0)
   endif

elseif (io_method .EQ. 2.and. trim(feedcase).ne.'feeddata') then
   filename = '../data/'//trim(run_title)//'.'//dirtime2//'.field.nc'

   inquire(file=trim(filename), exist=exist)
   if(exist) then
#ifdef MPIIO
      call pnetcdf_read(filename)
#endif
   else
      call MPI_Barrier(gcomm,ierr)
      write(io,*) 'the following restart file does not exist'
      write(io,*) trim(filename)
      call MPI_Barrier(gcomm,ierr)
      call terminate_run(io,0)
   endif

elseif (io_method .EQ. 3.and. trim(feedcase).ne.'feeddata') then
   filename = '../data/'//trim(run_title)//'.'//dirtime2//'.field.h5'

   inquire(file=trim(filename), exist=exist)
   if(exist) then
#ifdef MPIIO
      call hdf5_read(filename)
#endif
   else
      call MPI_Barrier(gcomm,ierr)
      write(io,*) 'the following restart file does not exist'
      write(io,*) trim(filename)
      call MPI_Barrier(gcomm,ierr)
      call terminate_run(io,0)
   endif

elseif ( (io_method .EQ. 4.or. io_method.eq.5) .and. trim(feedcase).ne.'feeddata') then
   filename = '../data/'//trim(run_title)//'.'//dirtime2//'.field.bp'
   inquire(file=trim(filename), exist=exist)
   if(exist) then
      if(myid == 0) write(io,*) 'Reading to intermediate arrays with from', filename

#ifdef ADIOS
      call read_savefile_data_adios( filename, topol_int(7), topol_int(8), topol_int(9), &
                                     yspeciesint, uint, pressureint, tempint )
#endif


      if( topol_int(4) == nx_g .and. topol_int(5) == ny_g .and. topol_int(6) == nz_g ) then
          if( myid == 0 ) then
              write(io,*) 'Copying data to work arrays'
          endif
          yspecies = yspeciesint
          u = uint
          pressure = pressureint
          temp = tempint
      else
          if(myid == 0) then
              write(io,*) 'Calling InterpLocal'
          endif
          call InterpLocal(myid)
      endif

      call MPI_Barrier(gcomm,ierr)
   else
      call MPI_Barrier(gcomm,ierr)
      write(io,*) 'the following restart file does not exist'
      write(io,*) trim(filename)
      call MPI_Barrier(gcomm,ierr)
      call terminate_run(io,0)
   endif
else  !io_method will be old fashioned fortran

  if(trim(feedcase).ne.'feeddata')then
    call set_dirname ! Decide where to read the data from
  endif

! ALLOCATE the extrusion boundaries
  allocate(xysurf(topol_dst(7),topol_dst(8),nsc+6,2));  xysurf=0.0
  allocate(yzsurf(topol_dst(8),topol_dst(9),nsc+6,2));  yzsurf=0.0
  allocate(xzsurf(topol_dst(7),topol_dst(9),nsc+6,2));  xzsurf=0.0

  allocate(xysurf_g(topol_dst(7),topol_dst(8),nsc+6,2));  xysurf_g=0.0
  allocate(yzsurf_g(topol_dst(8),topol_dst(9),nsc+6,2));  yzsurf_g=0.0
  allocate(xzsurf_g(topol_dst(7),topol_dst(9),nsc+6,2));  xzsurf_g=0.0

!IN MORPH THIS WAS INSIDE A LOOP OVER DESTINATION PROCESSES, HERE
! SINCE WE ARE PARALLEL THIS IS ALREADY HAPPENING FOR EACH DESTINATION
! PROCESS.

    call ijk_to_n(topol_dst(1:3),(/xid,yid,zid/),dest_id)

    src_off(:) = 0

!-- Which global indices are contained by this destination block?

!esr can I move the following into subroutines to tidy up?
    select case (trim(refinecase))
    case('cutter')
    call getIndxBnds(topol_dst, dest_id, lo_dst, hi_dst)

    isymm(1)=0
    isymm(2)=1
    isymm(3)=0
    
! I should make isymm into a vector so I can put some of this stuff in to loops.
    do i=1,3

      src_off(i)=org_off(i)    ! origin offsets in terms of src grid points
                               ! read them from intmorph.in
                               ! these should be positive.
        
      if(isymm(i).eq.1)then
        src_off(i)=(topol_src(3+i)-topol_dst(3+i))/2
        
        if(real((topol_src(3+i)-topol_dst(3+i))/2) .ne. &
          (real(topol_src(3+i))-real(topol_dst(3+i)))/2.0) &
          write(io, *) 'Error - add/remove an even number of points in symmetric-directions'
      endif
    enddo

! convert to global index in the source domain.

    do i=1,3

      lo(i)=lo_dst(i)+src_off(i)        ! add off set for symmetric addition/reductions
      hi(i)=hi_dst(i)+src_off(i)

      lo(i)=min0(topol_src(3+i),lo(i))  ! this check is necessary when adding domain
      hi(i)=min0(topol_src(3+i),hi(i))  ! limit lo,hi to the max,min of of the source domain.

      lo(i)=max0(1,lo(i))
      hi(i)=max0(1,hi(i))
            
    enddo

    case ('refiner')

    call getIndxBnds(topol_int, dest_id, lo, hi)
        
    case default  ! 'refiner' is the default

    call getIndxBnds(topol_int, dest_id, lo, hi)
    
    endselect 

!-- get list of source IDs that share/overlap this block
    call getId( topol_src, lo, hi, src_id, npts )

!-- check if lo,hi are outside of the source domain, if so, set npts=0
    do i=1,3
      if(topol_src(3+i).gt.1)then
      if(lo(i).eq.topol_src(3+i).or.hi(i).eq.1)npts=0
      endif
    enddo

! check that for processors 1 and 3 npts=0

    do n=1,npts

! read that src file into yspeciessrc, etc...
      call read_from_fileint(src_id(n))

! determine global indicies for intersection of the two blocks
    select case (trim(refinecase))
    case('cutter')

    call intersect_cutter(topol_src, src_id(n), topol_dst, dest_id, lo, hi ) 

    case('refiner') ! 'refiner' is the default
    call intersect(topol_src, src_id(n), topol_int, dest_id, lo, hi )
    
    case default ! 'refiner' is the default
    if(myid.eq.0)write(*,*)'WARNING: refinecase not recognised, Grid interpolation assumed'
    call intersect(topol_src, src_id(n), topol_int, dest_id, lo, hi )

    endselect
    
    is_src(:) = getLocIdx( topol_src, lo(:)+src_off(:))
    ie_src(:) = getLocIdx( topol_src, hi(:)+src_off(:))
    is_int(:) = getLocIdx( topol_int, lo(:))
    ie_int(:) = getLocIdx( topol_int, hi(:))

    call S3DBinDataCopynewInt(is_src,ie_src,is_int,ie_int)
                           
    enddo

    if(trim(refinecase).eq.'cutter')then

      call extruder   

    endif    ! endif(refinecase.eq.cutter)

    call InterpLocal(dest_id)

     if(trim(feedcase).eq.'feeddata')then
       call write_frozenfeed_data(io, u)
     endif


! DEALLOCATE the yspeciesint 
  deallocate(yspeciesint)
  deallocate(uint)
  deallocate(pressureint)
  deallocate(tempint)

  deallocate(yspeciessrc)
  deallocate(usrc)
  deallocate(pressuresrc)
  deallocate(tempsrc)

  deallocate(xysurf)
  deallocate(yzsurf)
  deallocate(xzsurf)
  
  deallocate(xysurf_g)
  deallocate(yzsurf_g)
  deallocate(xzsurf_g)
!merge***********************

endif


call MPI_Barrier(gcomm,ierr)
   !if(tracer_restart) call read_tracer_savefile(io, tracerdir)
if(myid.eq.0) call write_header(io,'-')

return
contains
  !----------------------------------------
  !----------------------------------------
  ! Decide where to read the data from
  subroutine set_dirname
  use runtime_m, only: run_title
  implicit none
  character :: myid_ext*5, logname*100
  character :: prec2, dummy*10  !, dirtime2*10   !5sf
  integer idummy

  write(myid_ext, '(I5.5)') myid
  !First option - read from input string directory
  if(present(dirtime)) then
    dirname='../data/'//precsn//'-'//trim(dirtime)
    dirtime2=dirtime
    filetype=precsn
    return
  end if

  !Second option - look for the last 'restart' file in logfile
  !restart files have to be 'd'ouble precision
  logname = '../data/'//trim(run_title)//'.savefile.log'
  call inquire_about_input_file(logname,io)
  if(myid == 0) then
    open(unit=64, file=logname, status='old', form='formatted')
    ! chew up the header lines of this file
    read(64,*)
    read(64,*)
    dirtime2 = ''
    do
      read(64,50, end=60) dummy, idummy, prec2
      50 format(a10, 6x, i7, 6x, a1)
      if(prec2 .eq. 'd') dirtime2 = dummy
    enddo
    60 continue 
    close(64)
  end if
  call MPI_Bcast(dirtime2, 10, MPI_CHARACTER, 0, gcomm, ierr )
  dirname='../data/d-'//trim(dirtime2)
  !tracerdir = '../data/tracer-'//trim(dirtime2)
  filetype='d'

  return
  end subroutine set_dirname

  !----------------------------------------
  !----------------------------------------
  subroutine get_topology
  use param_m, only: nx, ny, nz
  implicit none
  character*150 filename
  logical exist

  !check for 'unmorph.in' in the following order
  if(myid .eq. 0) then
    filename = trim(dirname)//'/unmorph.in'
    inquire(file=trim(filename), exist=exist)
    if(.not. exist) then
      filename = '../input/unmorph.in'
      inquire(file=trim(filename), exist=exist)
    end if

    if(exist) then
      open(unit=83, file=filename, status='old', form='formatted')
      read(83, *) xpu
      read(83, *) ypu
      read(83, *) zpu
      close(83)
    else
      xpu = xpes; ypu = ypes; zpu = zpes
    end if
  end if
  call MPI_Bcast(xpu, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(ypu, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(zpu, 1, MPI_INTEGER, 0, gcomm, ierr )

  facx = xpu/xpes
  facy = ypu/ypes
  facz = zpu/zpes
  if(facx*xpes.ne.xpu .or. facy*ypes.ne.ypu .or. facz*zpes.ne.zpu) then
    if(myid .eq. 0) then
      write(io, *) 'xpu = ', xpu, ', ypu = ', ypu, ', zpu = ', zpu
      write(io, *) 'facx = ', facx, ', facy = ', facy, ', facz = ', facz
      write(io, *) 'Error: unable to handle topology in unmorph.in'
    end if
    call terminate_run(io,0)
  end if
  mx = nx/facx
  my = ny/facy
  mz = nz/facz

  return
  end subroutine get_topology

  !----------------------------------------
  !----------------------------------------
  subroutine read_from_file
  use runtime_m, only: time, tstep, time_save
  use bc_m, only: pout
!  use variables_m, only: temp, pressure, yspecies, u
  use param_m, only: n_spec
  implicit none
  character*150 filename
  integer i, j, k, fileid
  character*5 fileid_ext
  logical exist, missing, missing_g

  missing = .false.
  loop_k: do k = 0, facz-1
    do j = 0, facy-1
      do i = 0, facx-1
        fileid = (xid*facx+i) + (yid*facy+j)*xpu + (zid*facz+k)*xpu*ypu
        write(fileid_ext, '(I5.5)') fileid
        filename = trim(dirname)//'/field.'//trim(fileid_ext)
        inquire(file=trim(filename), exist=exist)
        if(.not. exist) then
          write(*,*) 'missing files', xid, yid, zid, fileid, trim(dirname)
          missing = .true. 
          exit loop_k
        end if
        open(unit=201, file=trim(filename), status='old', form='unformatted')
        read(201) time
        read(201) tstep
        read(201) time_save
        call read_block2( &
             yspecies(mx*i+1:mx*i+mx,my*j+1:my*j+my,mz*k+1:mz*k+mz,:), n_spec)
        call read_block(temp(mx*i+1:mx*i+mx,my*j+1:my*j+my,mz*k+1:mz*k+mz))
        call read_block(pressure(mx*i+1:mx*i+mx,my*j+1:my*j+my,mz*k+1:mz*k+mz))
        call read_block2(u(mx*i+1:mx*i+mx,my*j+1:my*j+my,mz*k+1:mz*k+mz,:), 3)
        read(201) pout
        close(201)
      end do
    end do
  end do loop_k
  call mpi_allreduce(missing,missing_g,1,MPI_LOGICAL,MPI_LOR,gcomm,ierr)
  if(missing_g) then
    if(myid .eq. 0) write(io, *) 'Error: Some field files are missing'
    call terminate_run(io, 0)
  end if

  return
  end subroutine read_from_file

  !----------------------------------------
  !----------------------------------------
  subroutine read_block(field)
  implicit none
  real, intent(out) :: field(mx, my, mz)

  real (kind=4) :: dummy4(mx, my, mz) 

  if(filetype .eq. 's') then
    read(201) dummy4
    field = dummy4
  else
    read(201) field
  end if

  return
  end subroutine read_block

  !----------------------------------------
  !----------------------------------------
  ! To get around the blocked file format on RAM
  subroutine read_block2(field,mk)
  implicit none
  real, intent(out) :: field(mx, my, mz, mk)
  integer, intent(in) :: mk

  real (kind=4) :: dummy4(mx, my, mz, mk) 
  integer m

  if(filetype .eq. 's') then
    do m = 1, mk
      call read_block(field(:,:,:,m))
    end do
  else
    read(201) field
  end if

  return
  end subroutine read_block2



!===============================================================================

  subroutine getIndxBnds( topol, id, lo, hi )
    implicit none
    ! this routine calculates the global block indices
    ! for a given topology (topol) and block id (id)
    ! here topol is a topology, with variables npes(x),(y),(z),
    ! npts_g(x),(y),(z),npts_l(x),(y),(z)

!    type(Topology), intent(in) :: topol
    integer, dimension(9) :: topol
    integer, intent(in) :: id
    integer, dimension(3), intent(out) :: lo, hi
    integer, dimension(3) :: dirId
    integer :: i

    call n_to_ijk( topol(1:3), id, dirId )

    do i=1,3
       lo(i) = 1 + dirId(i) * topol(6+i)
       hi(i) = lo(i) + topol(6+i) - 1
    enddo

    return
  end subroutine getIndxBnds

!===============================================================================

  subroutine getId( topol, lo, hi, idlist, npts )
    ! this routine determines a list of block IDs (idlist) which overlap
    ! the global index region bracketed by (lo), (hi)
    ! for a given topology (topol)
    ! also returns how many ids were found (npts)
    implicit none
!    type(Topology), intent(in) :: topol
    integer, dimension(9), intent(in) :: topol
    integer, dimension(3), intent(in) :: lo, hi
    integer, dimension(:), intent(out) :: idlist
    integer, intent(out) :: npts
    integer :: i,j,k,n
    integer, dimension(3) :: idlo, idhi   ! indexed 0 to n-1

    idlist = -1

    do i=1,3
       ! which ids in this direction?
       idlo(i) = (lo(i)-1) / topol(6+i)
       idhi(i) = (hi(i)-1) / topol(6+i)
    enddo

  ! determine the number of points in the array
    npts = 1
    do i=1,3
       npts = npts * (1 + idhi(i)-idlo(i))
    enddo

    n=0
    do k= idlo(3),idhi(3)
       do j= idlo(2),idhi(2)
          do i= idlo(1),idhi(1)
             n = n+1
             call ijk_to_n( topol(1:3), (/i,j,k/), idlist(n) )
          enddo
       enddo
    enddo

! the above doesn't allow for zero source processors in the 
! destination dataset.

  !-- Error checking...
    if(n /= npts) then
       write(*,*)'PROBLEMS in getId'
       stop
    else
       do i=1,npts
          if( idlist(i) < 0 ) then
             write(*,*)'Invalid ID detected in getId at point: ',i
             stop
          endif
       enddo
    endif

    return
  end subroutine getId


!===============================================================================

  subroutine intersect( topol_1, id_1, topol_2, id_2, lo, hi )
    ! given the topology of two domains, along with the sequentially-ordered
    ! block IDs, this routine determines the domain of intersection (union)
    implicit none
!    type(Topology), intent(in) :: topol_1, topol_2
    integer, dimension(9), intent(in) :: topol_1,topol_2
    integer, intent(in) :: id_1, id_2
    integer, dimension(3), intent(out) :: lo, hi

    integer, dimension(3) :: lo_1, hi_1, lo_2, hi_2
    integer :: i

    call getIndxBnds( topol_1, id_1, lo_1, hi_1 )
    call getIndxBnds( topol_2, id_2, lo_2, hi_2 )

    do i=1,3
       lo(i) = max( lo_1(i), lo_2(i) )
       hi(i) = min( hi_1(i), hi_2(i) )
    enddo

    return
  end subroutine intersect

!===============================================================================

  subroutine intersect_cutter( topol_1, id_1, topol_2, id_2, lo, hi )
    ! given the topology of two domains, along with the sequentially-ordered
    ! block IDs, this routine determines the domain of intersection (union)
    ! The cutter version takes care of situations with symmetric addition
    ! or removal of domain.
    implicit none
!    type(Topology), intent(in) :: topol_1, topol_2
    integer, dimension(9), intent(in) :: topol_1,topol_2
    integer, intent(in) :: id_1, id_2
    integer, dimension(3), intent(out) :: lo, hi

    integer, dimension(3) :: lo_1, hi_1, lo_2, hi_2
    integer :: i

    call getIndxBnds( topol_1, id_1, lo_1, hi_1 )
    call getIndxBnds( topol_2, id_2, lo_2, hi_2 )

! in the case of symmetric addition/cutting there is an offset (of half the 
! addition/reduction) between the source (1) and destination (2) counts.
    
!esr23sept08    do i=1,3
!esr23sept08      lo(i) = max( lo_1(i), lo_2(i) )
!esr23sept08      hi(i) = min( hi_1(i), hi_2(i) )
!esr23sept08    enddo

    do i=1,3
!esr23sept08      if(isymm(i).eq.1)then
        lo(i) = max( lo_1(i) - src_off(i), lo_2(i))
        hi(i) = min( hi_1(i) - src_off(i), hi_2(i))
!esr23sept08      endif
    enddo

    return
  end subroutine intersect_cutter
!===============================================================================

  subroutine ijk_to_n( extent, ix_l, ix_g )
    ! given the domain extents and the local (i,j,k) index,
    ! this returns the GLOBAL (n) index
    !
    ! assumes i is the fastest varying index...
    implicit none
    integer, dimension(3), intent(in) :: extent
    integer, dimension(3), intent(in) :: ix_l
    integer, intent(out) :: ix_g
    integer :: i, itmp

    !!  ix_g = ix_l(1) + ix_l(2)*extent(1) + ix_l(3)*extent(1)*extent(2) + ...

    ix_g = 0
    itmp = 1
    do i=1,3
       ix_g = ix_g + ix_l(i) * itmp
       itmp = itmp * extent(i)
    enddo

    return
  end subroutine ijk_to_n

!===============================================================================

  subroutine n_to_ijk( extent, ix_g, ix_l )
    ! given the domain extents and the global index,
    ! this returns the local (i,j,k) index
    implicit none
    integer, dimension(3), intent(in) :: extent
    integer, intent(in) :: ix_g
    integer, dimension(3), intent(out) :: ix_l

    ix_l(3) = ix_g / (extent(1)*extent(2))
    ix_l(2) = ( mod( ix_g, extent(1)*extent(2) ) ) / extent(1)
    ix_l(1) = mod( mod( ix_g, extent(1)*extent(2) ), extent(1) )

    return
  end subroutine n_to_ijk

!===============================================================================

  function getLocIdx( topol, ix_glob ) result( ix_loc )
    ! given the global (i,j,k) index, this returns the local (i,j,k) index
    ! which depends on the processor topology
    implicit none
!    type(Topology), intent(in) :: topol
    integer, dimension(9) :: topol
    integer, dimension(3), intent(in)  :: ix_glob
    integer, dimension(3) :: ix_loc
    integer :: i

    do i=1,3
       ix_loc(i) = mod(ix_glob(i), topol(6+i))
       if( ix_loc(i) == 0 )  ix_loc(i) = topol(6+i)
    enddo

  end function getLocIdx 
!===============================================================================

  subroutine S3DBinDataCopynewInt(is_src, ie_src, is_int, ie_int)

  ! copies the specified index range of the source data
  ! to the specified index range of the intermediate data

  !ed I think 'is' and 'ie' are the global index, therfore I must convert
  ! to local indices - this needs to be done outside this routine...

    implicit none
    integer, dimension(3), intent(in) :: is_src, ie_src, is_int, ie_int

    select case (trim(feedcase))
    case('domaindata')

    pout_int      = pout_src
    time_int      = time_src
    tstep_int     = tstep_src
    time_save_int = time_save_src

    pressureint(is_int(1):ie_int(1),is_int(2):ie_int(2),      &
                is_int(3):ie_int(3)) =                        &
         pressuresrc(is_src(1):ie_src(1),is_src(2):ie_src(2),  &
                is_src(3):ie_src(3))
    tempint(is_int(1):ie_int(1),is_int(2):ie_int(2),      &
                is_int(3):ie_int(3)) =       &
         tempsrc(is_src(1):ie_src(1),is_src(2):ie_src(2),  &
                is_src(3):ie_src(3))
    uint(is_int(1):ie_int(1),is_int(2):ie_int(2),      &
                is_int(3):ie_int(3),:) =       &
         usrc(is_src(1):ie_src(1),is_src(2):ie_src(2),  &
                is_src(3):ie_src(3),:)
    yspeciesint(is_int(1):ie_int(1),is_int(2):ie_int(2),      &
                is_int(3):ie_int(3),:) =       &
         yspeciessrc(is_src(1):ie_src(1),is_src(2):ie_src(2),  &
                is_src(3):ie_src(3),:)
    case('feeddata')

    uint(is_int(1):ie_int(1),is_int(2):ie_int(2),      &
                is_int(3):ie_int(3),:) =       &
         usrc(is_src(1):ie_src(1),is_src(2):ie_src(2),  &
                is_src(3):ie_src(3),:)

    endselect
                                                                                                                                
  end subroutine S3DBinDataCopynewInt

!===============================================================================


  subroutine interpLocal( dest_id )
  
  ! given the destination processor index, this routine interpolates the 
  ! data held in "intData" onto the destination topology and writes it 
  ! into "destData". This version is 3D, could make 2D and 1D versions if required.
  ! Note, this should not modify the values where the same point exists in new and old grids.
                 
  !30 Aug 07, there is a problem at the block boundaries where the nodes in the nint'th cell 
  !don't get given a value.

  implicit none
  integer, intent(in) :: dest_id

  integer :: ix, iy, iz, ii, ij, ik, i, iL, iPoint, ifx, ify, ifz
  integer :: ii1,ij1,ik1
  integer, dimension(3) :: ninte, ndest, iblokLeft
  integer, dimension(topol_dst(7)+topol_dst(8)+topol_dst(9)) :: iLeft
  real, dimension(topol_dst(7)+topol_dst(8)+topol_dst(9)) :: diLeft
  real, dimension(topol_dst(7),topol_dst(8),topol_dst(9),3) :: dpress, dtemp
  real, dimension(topol_dst(7),topol_dst(8),topol_dst(9),3,3) :: du
  real, dimension(topol_dst(7),topol_dst(8),topol_dst(9),nspec,3) :: dys

  real, dimension(3) :: denom_int, denom_dest
  real :: xPoint, dummy
  real :: ci0j0k0,ci1j0k0,ci0j1k0,ci0j0k1,ci1j1k0,ci0j1k1,ci1j0k1,ci1j1k1
  integer, dimension(3) :: intlo, inthi, destlo, desthi
  real, dimension(3) :: xintlo, xinthi, xdestlo, xdesthi

!need to find iLeft and diLeft

  call getIndxBnds(topol_int,dest_id,intlo,inthi)
  call getIndxBnds(topol_dst,dest_id,destlo,desthi)

  do i=1,3
    xintlo(i)=real(intlo(i)-1)/real(max0(topol_int(3+i)-1,1))
    xinthi(i)=real(inthi(i)-1)/real(max0(topol_int(3+i)-1,1))
    xdestlo(i)=real(destlo(i)-1)/real(max0(topol_dst(3+i)-1,1))
    xdesthi(i)=real(desthi(i)-1)/real(max0(topol_dst(3+i)-1,1))
  enddo

  do i=1,3
    ninte(i)=topol_int(6+i)
    ndest(i)=topol_dst(6+i)
  enddo

  iblokLeft(1)=0
  iblokLeft(2)=ndest(1)
  iblokLeft(3)=ndest(1)+ndest(2)

!(1) Find the 'left' coordinate in the intermediate grid for the point i,j,k in the dest grid

!(2) express interpolated value as a combination of its 8 surrounding vertices.


!(1) find the index for the vertex of the original grid on the left of the interpolation point on the 
!    final grid, the three indicies are stored in a one dimensional array 'iLeft'. The ratio of the 
!    distance between the 'Left' point and the interpolation point to the grid size is stored in 
!    'diLeft()'. Again all three coordinates go into the 1D array 'diLeft()'.

!    since both int and dest grids are uniform in terms of the stretch vector s, we can do 
!    this directly without searching through the data. Note that the strech function x(s)
!    must be identical for both grids for this to be ok. (ie. the finest grid size must be
!    proportional to 1/(nx-1)). At the moment this must be changed manually in grid_m.f90
!    as well as changing the grid dimesnions in input/s3d.in.

  do i=1,3

    denom_int(i) = 1.0/max(1.0,real(ninte(i)-1))
    denom_dest(i)= 1.0/max(1.0,real(ndest(i)-1))

    do iPoint=1,ndest(i)
      xPoint = xdestlo(i)+real(iPoint-1)*denom_dest(i)*(xdesthi(i)-xdestlo(i))
        iL=1+ifix((xPoint-xintlo(i))*real(ninte(i)-1)/(xinthi(i)-xintlo(i)))
      iL=max0(iL,1)
      iL=min0(iL,ninte(i))
      iLeft(iPoint+iblokLeft(i))=iL
      diLeft(iPoint+iblokLeft(i))=xPoint- &
        (xintlo(i)+real(iL-1)*denom_int(i)*(xinthi(i)-xintlo(i)))
!warning, diLeft can be negative.

    enddo
  enddo



!(2) alternative method for finding the interpolate...
!    write(*,*)'Second order interpolation using eight vertices'

  do i=1,3
    do iPoint=1,ndest(i)
      diLeft(iPoint+iblokLeft(i)) = &
      diLeft(iPoint+iblokLeft(i))*real(topol_int(i+3)-1)
    enddo
  enddo

  do iz=1,ndest(3)
    do iy=1,ndest(2)
      do ix=1,ndest(1)

        ii=(ix+iblokLeft(1))
        ij=(iy+iblokLeft(2))
        ik=(iz+iblokLeft(3))
        ii1=(iblokLeft(1)+min0(ndest(1),ix+1))
        ij1=(iblokLeft(2)+min0(ndest(2),iy+1))
        ik1=(iblokLeft(3)+min0(ndest(3),iz+1))

        ci0j0k0=(1.-diLeft(ii))*(1.-diLeft(ij))*(1.-diLeft(ik))
        ci1j0k0=diLeft(ii)*(1.-diLeft(ij))*(1.-diLeft(ik))
        ci0j1k0=(1.-diLeft(ii))*diLeft(ij)*(1.-diLeft(ik))
        ci0j0k1=(1.-diLeft(ii))*(1.-diLeft(ij))*diLeft(ik)
        ci1j1k0=diLeft(ii)*diLeft(ij)*(1.-diLeft(ik))
        ci0j1k1=(1.-diLeft(ii))*diLeft(ij)*diLeft(ik)
        ci1j0k1=diLeft(ii)*(1.-diLeft(ij))*diLeft(ik)
        ci1j1k1=diLeft(ii)*diLeft(ij)*diLeft(ik)

        ii=iLeft(ix+iblokLeft(1))
        ij=iLeft(iy+iblokLeft(2))
        ik=iLeft(iz+iblokLeft(3))
        ii1=iLeft(iblokLeft(1)+min0(ndest(1),ix+1))
        ij1=iLeft(iblokLeft(2)+min0(ndest(2),iy+1))
        ik1=iLeft(iblokLeft(3)+min0(ndest(3),iz+1))
        
        select case (trim(feedcase))
        case('domaindata')

        pressure(ix,iy,iz)=                   &
            ci0j0k0*pressureint(ii,ij,ik)         &
           +ci1j0k0*pressureint(ii1,ij,ik)        &
           +ci0j1k0*pressureint(ii,ij1,ik)        &
           +ci0j0k1*pressureint(ii,ij,ik1)        &
           +ci1j1k0*pressureint(ii1,ij1,ik)       &
           +ci0j1k1*pressureint(ii,ij1,ik1)       &
           +ci1j0k1*pressureint(ii1,ij,ik1)       &
           +ci1j1k1*pressureint(ii1,ij1,ik1)

        temp(ix,iy,iz)=                   &
            ci0j0k0*tempint(ii,ij,ik)         &
           +ci1j0k0*tempint(ii1,ij,ik)        &
           +ci0j1k0*tempint(ii,ij1,ik)        &
           +ci0j0k1*tempint(ii,ij,ik1)        &
           +ci1j1k0*tempint(ii1,ij1,ik)       &
           +ci0j1k1*tempint(ii,ij1,ik1)       &
           +ci1j0k1*tempint(ii1,ij,ik1)       &
           +ci1j1k1*tempint(ii1,ij1,ik1)

        u(ix,iy,iz,:)=                  &
            ci0j0k0*uint(ii,ij,ik,:)        &
           +ci1j0k0*uint(ii1,ij,ik,:)       &
           +ci0j1k0*uint(ii,ij1,ik,:)       &
           +ci0j0k1*uint(ii,ij,ik1,:)       &
           +ci1j1k0*uint(ii1,ij1,ik,:)      &
           +ci0j1k1*uint(ii,ij1,ik1,:)      &
           +ci1j0k1*uint(ii1,ij,ik1,:)      &
           +ci1j1k1*uint(ii1,ij1,ik1,:)

        yspecies(ix,iy,iz,:)=                   &
            ci0j0k0*yspeciesint(ii,ij,ik,:)         &
           +ci1j0k0*yspeciesint(ii1,ij,ik,:)        &
           +ci0j1k0*yspeciesint(ii,ij1,ik,:)        &
           +ci0j0k1*yspeciesint(ii,ij,ik1,:)        &
           +ci1j1k0*yspeciesint(ii1,ij1,ik,:)       &
           +ci0j1k1*yspeciesint(ii,ij1,ik1,:)       &
           +ci1j0k1*yspeciesint(ii1,ij,ik1,:)       &
           +ci1j1k1*yspeciesint(ii1,ij1,ik1,:)

        case('feeddata')

        u(ix,iy,iz,:)=                  &
            ci0j0k0*uint(ii,ij,ik,:)        &
           +ci1j0k0*uint(ii1,ij,ik,:)       &
           +ci0j1k0*uint(ii,ij1,ik,:)       &
           +ci0j0k1*uint(ii,ij,ik1,:)       &
           +ci1j1k0*uint(ii1,ij1,ik,:)      &
           +ci0j1k1*uint(ii,ij1,ik1,:)      &
           +ci1j0k1*uint(ii1,ij,ik1,:)      &
           +ci1j1k1*uint(ii1,ij1,ik1,:)

        endselect
      enddo
    enddo
  enddo

120 continue

  pout      = pout      !_int
  time      = time      !_int
  tstep     = tstep     !_int
  time_save = time_save !_int

  end subroutine interpLocal
 
             
  !----------------------------------------
  subroutine get_topologyint
  use param_m, only: nx, ny, nz
  implicit none
  character*150 filename
  logical exist

  !check for 'intmorph.in' in the following order
  if(myid .eq. 0) then
    filename = trim(dirname)//'/intmorph.in'
    inquire(file=trim(filename), exist=exist)
    if(.not. exist) then
      filename = '../input/intmorph.in'
      inquire(file=trim(filename), exist=exist)
    end if

    if(exist) then
      open(unit=83, file=filename, status='old', form='formatted')
      read(83, *) xpu
      read(83, *) ypu
      read(83, *) zpu
      read(83, *) mx_g
      read(83, *) my_g
      read(83, *) mz_g
      read(83, *) feedcase
      read(83, *) refinecase
      read(83, *) org_off(1)
      read(83, *) org_off(2)
      read(83, *) org_off(3)
      close(83)
    else
      xpu = xpes; ypu = ypes; zpu = zpes
    end if
  end if

  call MPI_Bcast(xpu, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(ypu, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(zpu, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(mx_g, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(my_g, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(mz_g, 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(feedcase, 20, MPI_CHARACTER, 0, gcomm, ierr)
  call MPI_Bcast(refinecase, 20, MPI_CHARACTER, 0, gcomm, ierr)
  call MPI_Bcast(org_off(1), 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(org_off(2), 1, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast(org_off(3), 1, MPI_INTEGER, 0, gcomm, ierr )
        

!  facx = xpu/xpes
!  facy = ypu/ypes
!  facz = zpu/zpes
!  if(facx*xpes.ne.xpu .or. facy*ypes.ne.ypu .or. facz*zpes.ne.zpu) then
!    if(myid .eq. 0) then
!      write(io, *) 'xpu = ', xpu, ', ypu = ', ypu, ', zpu = ', zpu
!      write(io, *) 'facx = ', facx, ', facy = ', facy, ', facz = ', facz
!      write(io, *) 'Error: unable to handle topology in unmorph.in'
!    end if
!    call terminate_run(io,0)
!  end if
!  mx = nx/facx
!  my = ny/facy
!  mz = nz/facz

! perform some chcks on the compatibility of specified topology.

  mx = mx_g/xpu
  my = my_g/ypu
  mz = mz_g/zpu

  topol_src(1)=xpu
  topol_src(2)=ypu
  topol_src(3)=zpu
  topol_src(4)=mx*xpu
  topol_src(5)=my*ypu
  topol_src(6)=mz*zpu
  topol_src(7)=mx
  topol_src(8)=my
  topol_src(9)=mz

  select case (trim(refinecase))
  case('cutter')
  
  topol_int(1)=xpes
  topol_int(2)=ypes
  topol_int(3)=zpes
  topol_int(4)=nx*xpes
  topol_int(5)=ny*ypes
  topol_int(6)=nz*zpes
  topol_int(7)=nx
  topol_int(8)=ny
  topol_int(9)=nz

  case('refiner')

  topol_int(1)=xpes
  topol_int(2)=ypes
  topol_int(3)=zpes
  topol_int(4)=mx*xpu
  topol_int(5)=my*ypu
  topol_int(6)=mz*zpu
  topol_int(7)=mx*xpu/xpes
  topol_int(8)=my*ypu/ypes
  topol_int(9)=mz*zpu/zpes
                    
  case default

  if(myid.eq.0)write(*,*)'WARNING: cut/refine flag not recognised, using refine'
  
  topol_int(1)=xpes
  topol_int(2)=ypes
  topol_int(3)=zpes
  topol_int(4)=mx*xpu
  topol_int(5)=my*ypu
  topol_int(6)=mz*zpu
  topol_int(7)=mx*xpu/xpes
  topol_int(8)=my*ypu/ypes
  topol_int(9)=mz*zpu/zpes
  
  endselect

  topol_dst(1)=xpes
  topol_dst(2)=ypes
  topol_dst(3)=zpes
  topol_dst(4)=nx*xpes
  topol_dst(5)=ny*ypes
  topol_dst(6)=nz*zpes
  topol_dst(7)=nx
  topol_dst(8)=ny
  topol_dst(9)=nz

  select case (trim(refinecase))
  case ('refiner')
    
  if(topol_src(7).ne.topol_src(4)/topol_src(1) .or.  &
     topol_src(8).ne.topol_src(5)/topol_src(2) .or.  &
     topol_src(9).ne.topol_src(6)/topol_src(3)) then 
     if(myid.eq.0)then
       write(io,*) 'xpu = ', xpu, ', ypu = ', ypu, ', zpu = ', zpu
       write(io, *) 'mx = ', mx, ', my = ', my, ', mz = ', mz
       write(io, *) 'Error: unable to handle source topology in unmorph.in'
     endif
  endif

  if(topol_int(4).ne.topol_int(7)*topol_int(1) .or.  &
    topol_int(5).ne.topol_int(8)*topol_int(2) .or.   &
    topol_int(6).ne.topol_int(9)*topol_int(3)) then  
    if(myid.eq.0)then
      write(io,*) 'xpes = ', xpes, ', ypes = ', ypes, ', zpes = ', zpes
      write(io, *) 'nx = ', real(mx*xpu)/real(xpes), ', ny = ', real(my*ypu)/real(ypes), &
        ', nz = ', real(mz*zpu)/real(zpes)
      write(io, *) 'Error: unable to handle intermediate topology'
    endif
  endif

  if(topol_dst(7).ne.topol_dst(4)/topol_dst(1) .or. &
   topol_dst(8).ne.topol_dst(5)/topol_dst(2) .or.   &
   topol_dst(9).ne.topol_dst(6)/topol_dst(3)) then
    if(myid.eq.0)then
      write(io,*) 'xpes = ', xpes, ', ypes = ', ypes, ', zpes = ', zpes
      write(io, *) 'nx = ', nx, ', ny = ', ny, ', nz = ', nz
      write(io, *) 'Error: unable to handle desination topology'
    endif
  endif
           
  case ('cutter')           

  if(myid.eq.0) then
   write(io, *) 'No checks on compatable topologies implemented for'
   write(io, *) 'COOKIE CUTTER option as yet.'
   write(io, *) 'NB, By default, COOKIE CUTTER is symmetric in y-direction'
   write(io, *) 'but a-symmetric in x and z-directions'
  endif
  endselect
                                                     
  return
  end subroutine get_topologyint

  !----------------------------------------

 !----------------------------------------
  subroutine extruder
!  use param_m
  implicit none


      kid(1)=xid
      kid(2)=yid
      kid(3)=zid

    call MPI_Barrier(gcomm,ierr)

! 1) find the global x value of the ix=1 point ixgl,ixgr,ixbndl,ixbndr

!    ixgl=xid*topol_dst(7)+1
!    ixgr=xid*topol_dst(7)+topol_dst(7)
    do k=1,3
    igl(k)=kid(k)*topol_dst(6+k)+1
    igr(k)=kid(k)*topol_dst(6+k)+topol_dst(6+k)
    enddo


! ORDER X,Y,Z


! X direction

   k=1


! X2) find if igl is in this processor
        

    if((igr(k).gt.1-src_off(k)) .and. (igl(k).le.(1-src_off(k)))) then ! the lower boundary is in this processor
        ibndl(k)=(1-src_off(k))-kid(k)*topol_dst(6+k) !the local index for the lower boundary

    do i=1,3
      yzsurf(:,:,i,1)=uint(ibndl(k),:,:,i)
    enddo
    yzsurf(:,:,4,1)=pressureint(ibndl(k),:,:)
    yzsurf(:,:,5,1)=tempint(ibndl(k),:,:)
    do i=1,nsc+1
      yzsurf(:,:,i+5,1)=yspeciesint(ibndl(k),:,:,i)
    enddo

    endif   ! lower boundary

    if((igr(k).ge.topol_src(3+k)-src_off(k)).and.(igl(k).lt.topol_src(3+k)-src_off(k)) )then
        ibndr(k)= topol_src(3+k)-src_off(k)-kid(k)*topol_dst(6+k)  ! the local index for the upper boundary

    do i=1,3
      yzsurf(:,:,i,2)=uint(ibndr(k),:,:,i)
    enddo
    yzsurf(:,:,4,2)=pressureint(ibndr(k),:,:)
    yzsurf(:,:,5,2)=tempint(ibndr(k),:,:)
    do i=1,nsc+1
      yzsurf(:,:,i+5,2)=yspeciesint(ibndr(k),:,:,i)
    enddo

    endif   ! upper boundary

! X3) all reduce (summation along k'th direction)


    call MPI_Barrier(gcomm,ierr)


    do i=1,nsc+6
      do j=1,2
        CALL MPI_allreduce(yzsurf(:,:,i,j),yzsurf_g(:,:,i,j),topol_dst(8)*topol_dst(9),MPI_REAL8,MPI_SUM,xcomm,ierr)
      enddo
    enddo  

    call MPI_Barrier(gcomm,ierr)

! X4) if you are outside the orignial source data, overwrite with the boundary data

   do ix=1,topol_dst(7)
     if(xid*topol_dst(7)+ix .lt. (1-src_off(1)))then
         write(*,*)'this should not have been realised !!!!!'
       do i=1,3
         uint(ix,:,:,i)=yzsurf_g(:,:,i,1)
       enddo
       pressureint(ix,:,:)=yzsurf_g(:,:,4,1)
       tempint(ix,:,:)=yzsurf_g(:,:,5,1)
       do i=1,nsc+1
         yspeciesint(ix,:,:,i)=yzsurf_g(:,:,5+i,1)
       enddo
     endif
     if((xid*topol_dst(7)+ix).gt.(topol_src(4)-src_off(1)))then
       do i=1,3
         uint(ix,:,:,i)=yzsurf_g(:,:,i,2)
       enddo
       pressureint(ix,:,:)=yzsurf_g(:,:,4,2)
       tempint(ix,:,:)=yzsurf_g(:,:,5,2)
       do i=1,nsc+1
         yspeciesint(ix,:,:,i)=yzsurf_g(:,:,5+i,2)
       enddo
     endif
   enddo


! Y direction

   k=2


! Y2) find if igl is in this processor
        

    if((igr(k).gt.1-src_off(k)) .and. (igl(k).le.(1-src_off(k)))) then ! the lower boundary is in this processor
        ibndl(k)=(1-src_off(k))-kid(k)*topol_dst(6+k) !the local index for the lower boundary


    do i=1,3
      xzsurf(:,:,i,1)=uint(:,ibndl(k),:,i)
    enddo
    xzsurf(:,:,4,1)=pressureint(:,ibndl(k),:)
    xzsurf(:,:,5,1)=tempint(:,ibndl(k),:)
    do i=1,nsc+1
      xzsurf(:,:,i+5,1)=yspeciesint(:,ibndl(k),:,i)
    enddo


    endif   ! lower boundary

    if((igr(k).ge.topol_src(3+k)-src_off(k)).and.(igl(k).lt.topol_src(3+k)-src_off(k)) )then
        ibndr(k)= topol_src(3+k)-src_off(k)-kid(k)*topol_dst(6+k)  ! the local index for the upper boundary

    do i=1,3
      xzsurf(:,:,i,2)=uint(:,ibndr(k),:,i)
    enddo
    xzsurf(:,:,4,2)=pressureint(:,ibndr(k),:)
    xzsurf(:,:,5,2)=tempint(:,ibndr(k),:)
    do i=1,nsc+1
      xzsurf(:,:,i+5,2)=yspeciesint(:,ibndr(k),:,i)
    enddo

    endif   ! upper boundary

! Y3) all reduce (summation along k'th direction)


    call MPI_Barrier(gcomm,ierr)

    do i=1,nsc+6
      do j=1,2
        CALL MPI_allreduce(xzsurf(:,:,i,j),xzsurf_g(:,:,i,j),topol_dst(7)*topol_dst(9),MPI_REAL8,MPI_SUM,ycomm,ierr)
      enddo
    enddo

    call MPI_Barrier(gcomm,ierr)

! Y4) if you are outside the orignial source data, overwrite with the boundary data

   do ix=1,topol_dst(8)
     if(yid*topol_dst(8)+ix .lt. (1-src_off(2)))then
       do i=1,3
         uint(:,ix,:,i)=xzsurf_g(:,:,i,1)
       enddo
       pressureint(:,ix,:)=xzsurf_g(:,:,4,1)
       tempint(:,ix,:)=xzsurf_g(:,:,5,1)
       do i=1,nsc+1
         yspeciesint(:,ix,:,i)=xzsurf_g(:,:,5+i,1)
       enddo
     endif
     if((yid*topol_dst(8)+ix).gt.(topol_src(5)-src_off(2)))then
       do i=1,3
         uint(:,ix,:,i)=xzsurf_g(:,:,i,2)
       enddo
       pressureint(:,ix,:)=xzsurf_g(:,:,4,2)
       tempint(:,ix,:)=xzsurf_g(:,:,5,2)
       do i=1,nsc+1
         yspeciesint(:,ix,:,i)=xzsurf_g(:,:,5+i,2)
       enddo
     endif
   enddo


! Z direction

   k=3


! Z2) find if igl is in this processor
        

    if((igr(k).gt.1-src_off(k)) .and. (igl(k).le.(1-src_off(k)))) then ! the lower boundary is in this processor
        ibndl(k)=(1-src_off(k))-kid(k)*topol_dst(6+k) !the local index for the lower boundary

    do i=1,3
      xysurf(:,:,i,1)=uint(:,:,ibndl(k),i)
    enddo
    xysurf(:,:,4,1)=pressureint(:,:,ibndl(k))
    xysurf(:,:,5,1)=tempint(:,:,ibndl(k))
    do i=1,nsc+1
      xysurf(:,:,i+5,1)=yspeciesint(:,:,ibndl(k),i)
    enddo

    endif   ! lower boundary

    if((igr(k).ge.topol_src(3+k)-src_off(k)).and.(igl(k).lt.topol_src(3+k)-src_off(k)) )then
        ibndr(k)= topol_src(3+k)-src_off(k)-kid(k)*topol_dst(6+k)  ! the local index for the upper boundary

    do i=1,3
      xysurf(:,:,i,2)=uint(:,:,ibndr(k),i)
    enddo
    xysurf(:,:,4,2)=pressureint(:,:,ibndr(k))
    xysurf(:,:,5,2)=tempint(:,:,ibndr(k))
    do i=1,nsc+1
      xysurf(:,:,i+5,2)=yspeciesint(:,:,ibndr(k),i)
    enddo

    endif   ! upper boundary

! Z3) all reduce (summation along k'th direction)

    call MPI_Barrier(gcomm,ierr)
   
    do i=1,nsc+6
      do j=1,2
        CALL MPI_allreduce(xysurf(:,:,i,j),xysurf_g(:,:,i,j),topol_dst(7)*topol_dst(8),MPI_REAL8,MPI_SUM,zcomm,ierr)
      enddo
    enddo

    call MPI_Barrier(gcomm,ierr)


! Z4) if you are outside the orignial source data, overwrite with the boundary data

   do ix=1,topol_dst(9)
     if(zid*topol_dst(9)+ix .lt. (1-src_off(3)))then
       do i=1,3
         uint(:,:,ix,i)=xysurf_g(:,:,i,1)
       enddo
       pressureint(:,:,ix)=xysurf_g(:,:,4,1)
       tempint(:,:,ix)=xysurf_g(:,:,5,1)
       do i=1,nsc+1
         yspeciesint(:,:,ix,i)=xysurf_g(:,:,5+i,1)
       enddo
     endif
     if((zid*topol_dst(9)+ix).gt.(topol_src(6)-src_off(3)))then
       do i=1,3
         uint(:,:,ix,i)=xysurf_g(:,:,i,2)
       enddo
       pressureint(:,:,ix)=xysurf_g(:,:,4,2)
       tempint(:,:,ix)=xysurf_g(:,:,5,2)
       do i=1,nsc+1
         yspeciesint(:,:,ix,i)=xysurf_g(:,:,5+i,2)
       enddo
     endif
   enddo

  return
  end subroutine extruder

  !----------------------------------------

  !----------------------------------------
  subroutine read_from_fileint(tag)
 !Read the source data block into yspeciessrc etc.
  use runtime_m, only: time, tstep, time_save
  use bc_m, only: pout
!  use variables_m, only: temp, pressure, yspecies, u
  use param_m, only: n_spec
  implicit none
  character*150 filename
  integer i, j, k, fileid
  character*5 fileid_ext
  logical exist
  logical missing, missing_g
  integer, intent(in) :: tag
  integer tag_yz
  integer ijk(3)
  real dummy_delx
  real (kind=4) :: u4(my,mz,3)

  missing = .false.
  select case (trim(feedcase))
  case ('domaindata')
                
        write(fileid_ext, '(I5.5)') tag
        filename = trim(dirname)//'/field.'//trim(fileid_ext)
        inquire(file=trim(filename), exist=exist)
        if(.not. exist) then
          missing = .true. 
          write(*,*) "Missing file: ",filename
          goto 202
        end if
        open(unit=201, file=trim(filename), status='old', form='unformatted')
        read(201) time
        read(201) tstep
        read(201) time_save
        call read_block2( &
     yspeciessrc(1:topol_src(7),1:topol_src(8),1:topol_src(9),:), n_spec)
        call read_block(tempsrc(1:topol_src(7),1:topol_src(8),1:topol_src(9)))
        call read_block(pressuresrc(1:topol_src(7),1:topol_src(8),1:topol_src(9)))
        call read_block2(usrc(1:topol_src(7),1:topol_src(8),1:topol_src(9),:), 3)
        read(201) pout
        close(201)
  case ('feeddata')

        if(topol_dst(1).eq.1)then    !xpes=1

        write(fileid_ext, '(I5.5)') tag
        filename = '../feedin/frozenfeed/field.'//trim(fileid_ext)
        inquire(file=trim(filename), exist=exist)
        if(.not. exist) then
          missing = .true.
          goto 202
        end if
        open(unit=201, file=trim(filename), status='old', form='unformatted')    
                
        read(201) my, mz
        read(201) mx
                
        if(mx.ne.topol_src(7) .or. &
           my.ne.topol_src(8) .or. &
           mz.ne.topol_src(9))then
        if(myid.eq.0) write(*,*)'npts in morph.in does not match frozenfeed:', &
           topol_src(7),topol_src(8),topol_src(9),mx,my,mz
        endif
        read(201) dummy_delx

        do i = mx, 1, -1
         read(201) u4
         usrc(i,:,:,:)=u4(:,:,:)
        enddo
        close(201)

        else   !xpes.ne.1
        call n_to_ijk(topol_src(1:3),tag,ijk)
        write(*,*)'i,j,k',ijk
        i=ijk(1)
        j=ijk(2)
        k=ijk(3)
        call ijk_to_n((/1,topol_src(2),topol_src(3)/),(/0,j,k/),tag_yz)
        write(*,*)'myid=',myid,'tag',tag,'tag_yz',tag_yz,'xid',xid
        call MPI_Barrier( gcomm, ierr )

        write(fileid_ext, '(I5.5)') tag_yz
        filename = '../feedin/frozenfeed/field.'//trim(fileid_ext)
        inquire(file=trim(filename), exist=exist)
        if(.not. exist) then
          missing = .true.
          goto 202
        end if
        open(unit=201, file=trim(filename), status='old', form='unformatted')

        read(201) my, mz
        read(201) mx

        if(mx.ne.topol_src(4) .or. &
           my.ne.topol_src(8) .or. &
           mz.ne.topol_src(9))then
        if(myid.eq.0) write(*,*)'npts in morph.in does not match frozenfeed:', &
           topol_src(4),topol_src(8),topol_src(9),mx,my,mz
        endif
        read(201) dummy_delx


! be careful of reading all at the same time.

        do j = topol_src(1)-1,xid,-1
          do i = topol_src(7), 1, -1
            read(201) u4
            usrc(i,:,:,:)=u4(:,:,:)
          enddo
        enddo

        close(201)

        endif
 
  endselect
 
202    continue

!esr I have removed the following error check since this routine does not 
!    get called an equal number of times on all processes there fore it 
!    can cause the code to hang, not sure how to check the code otherwise.

!esr  call mpi_allreduce(missing,missing_g,1,MPI_LOGICAL,MPI_LOR,gcomm,ierr)
!esr  if(missing_g) then
!esr    if(myid .eq. 0) write(io, *) 'Error: Some field files are missing'
!esr    call terminate_run(io, 0)
!esr  end if

  return
  end subroutine read_from_fileint

  !----------------------------------------
  !----------------------------------------
  subroutine read_blockint(field)
  implicit none
  real, intent(out) :: field(mx, my, mz)

  real (kind=4) :: dummy4(mx, my, mz) 

  if(filetype .eq. 's') then
    read(201) dummy4
    field = dummy4
  else
    read(201) field
  end if

  return
  end subroutine read_blockint

  !----------------------------------------
  !----------------------------------------
  ! To get around the blocked file format on RAM
  subroutine read_block2int(field,mk)
  implicit none
  real, intent(out) :: field(mx, my, mz, mk)
  integer, intent(in) :: mk

  real (kind=4) :: dummy4(mx, my, mz, mk) 
  integer m

  if(filetype .eq. 's') then
    do m = 1, mk
      call read_block(field(:,:,:,m))
    end do
  else
    read(201) field
  end if

  return
  end subroutine read_block2int
end subroutine read_savefile

!----------------------------------------------------------------------
! reads and writes data for save file in double precision
subroutine readwrite_savefile_data(io,io_savefile,input)
use topology_m, only : myid
use runtime_m, only : time, tstep, time_save
use variables_m, only : temp, pressure, yspecies, u
use bc_m, only : pout

implicit none
integer io, io_savefile
character*1 input

if(input .ne. 'r' .and. input .ne. 'w') then
  if(myid.eq.0) then
    write(io,*) 'improper setting for variable input'
    write(io,*) 'in routine readwrite_savefile_data'
  endif
  call terminate_run(io,0)  !must be called by all processors
  return
end if

if(input.eq.'w') then
  write(io_savefile) time
  write(io_savefile) tstep
  write(io_savefile) time_save
  write(io_savefile) yspecies
  write(io_savefile) temp
  write(io_savefile) pressure
  write(io_savefile) u
  write(io_savefile) pout
elseif(input.eq.'r') then
  read(io_savefile) time
  read(io_savefile) tstep
  read(io_savefile) time_save
  read(io_savefile) yspecies
  read(io_savefile) temp
  read(io_savefile) pressure
  read(io_savefile) u
  read(io_savefile) pout
endif


return
end subroutine readwrite_savefile_data
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! reads and writes data for save file in single precision
subroutine readwrite_singprec_data(io,io_savefile,input)
use topology_m, only : myid
use param_m, only: nx, ny, nz, n_spec
use runtime_m, only : time, tstep, time_save
use variables_m, only : temp, pressure, yspecies, u
use bc_m, only : pout

implicit none
integer io, io_savefile
character*1 input
real(kind=4) :: dummy1(nx, ny, nz)

integer m

if(input .ne. 'r' .and. input .ne. 'w') then
  if(myid.eq.0) then
    write(io,*) 'improper setting for variable input'
    write(io,*) 'in routine readwrite_singprec_data'
  endif
  call terminate_run(io,0)  !must be called by all processors
  return
end if

if(input.eq.'w') then
  write(io_savefile) time
  write(io_savefile) tstep
  write(io_savefile) time_save
  do m = 1, n_spec
    dummy1 = yspecies(:,:,:,m); write(io_savefile) dummy1
  end do
  dummy1 = temp    ; write(io_savefile) dummy1
  dummy1 = pressure; write(io_savefile) dummy1
  do m = 1, 3
    dummy1 = u(:,:,:,m); write(io_savefile) dummy1
  end do
  write(io_savefile) pout
elseif(input.eq.'r') then
  read(io_savefile) time
  read(io_savefile) tstep
  read(io_savefile) time_save
  do m = 1, n_spec
    read(io_savefile) dummy1; yspecies(:,:,:,m) = dummy1
  end do
  read(io_savefile) dummy1; temp     = dummy1 
  read(io_savefile) dummy1; pressure = dummy1 
  do m = 1, 3
    read(io_savefile) dummy1; u(:,:,:,m) = dummy1
  end do
  read(io_savefile) pout
endif

return
end subroutine readwrite_singprec_data
!----------------------------------------------------------------------
!----------------------------------------------------------------------
  subroutine write_header(io,char_in)
  implicit none

  integer io
  character*1 char_in

  character*1 char(78)

  char(:)=char_in
  write(io,*) char(:)

  return
  end subroutine write_header

!=========================================================================================
  subroutine inquire_about_input_file(filename,io)
!=========================================================================================
! routine inquires about the existence of an input file
! and terminates the run cleanly if it doesn't exist
! this routine MUST be called from outside of any if(myid.eq.0) statements
! routine works best when filename passed in character*100 (which is the convention)
!-----------------------------------------------------------------------------------------
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  character*100 filename
  integer io

! local declarations

  logical exist
!-----------------------------------------------------------------------------------------
! inquire about existence of file for myid=0

  if(myid.eq.0) then

    inquire(file=trim(filename),exist=exist)

    if(.not.exist) then   !does not exist

      write(io,*) 'the following input file does not exist:'
      write(io,*) trim(filename)

      term_status=1

    endif

  endif

! check termination status and terminate if file doesn't exist

  call check_term_status(io,0)
!----------------------------------------------------------------------------------------
  return
  end subroutine inquire_about_input_file
!----------------------------------------------------------------------
! For calls left over in old code. 
! For new code use write(...'I(5.5)') directly. - Ramanan
subroutine set_file_extension_string(file_ext, num, max)
implicit none
character(*) file_ext
integer num, max
character d

write(d, '(i1)') max
write(file_ext,'(I'//d//'.'//d//')') num

end subroutine set_file_extension_string
!----------------------------------------------------------------------
subroutine write_date_and_time(dat,tim,io)
implicit none
integer io
character*1 dat(8), tim(10)

write(io,1) dat(5),dat(6),dat(7),dat(8),dat(1),dat(2),dat(3),dat(4)
write(io,2) tim(1),tim(2),tim(3),tim(4),tim(5),tim(6)

1 format(1x,'date = ',a1,a1,'/',a1,a1,'/',a1,a1,a1,a1)
2 format(1x,'time = ',a1,a1,':',a1,a1,':',a1,a1)
return
end subroutine write_date_and_time
!=========================================================================================
  subroutine monitor(io,temp_max_out,pres_max_out, &
                     q, qnx, qny, qnz, qnvar_tot, qn_reg)
!=========================================================================================
! routine monitors global minimum and maximum values
! of velocity, density, temperature, pressure, and yspecies
! for runtime ("on-the-fly") diagnostics
!
! routine outputs information in dimensional form
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx, ny, nz, n_reg, n_spec
  use variables_m
  use runtime_m, only : i_time, i_time_mon, run_title, time, i_restart, time_restart
  use reference_m, only : a_ref, rho_ref, t_ref, p_ref, pres_atm, time_ref
  use chemkin_m, only : species_name

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io
  real temp_max_out, pres_max_out

  integer, intent(in) :: qnx, qny, qnz, qnvar_tot, qn_reg
  real, intent(in), dimension(qnx,qny,qnz,qnvar_tot,qn_reg) :: q !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)
! local declarations

  real u_min(3), u_max(3)
  real rho_min, rho_max
  real t_min, t_max
  real p_min, p_max
  real y_min(n_spec), y_max(n_spec)
  real mass, mom_x, mom_y

  real u_min_l(3), u_max_l(3)
  real rho_min_l, rho_max_l
  real t_min_l, t_max_l
  real p_min_l, p_max_l
  real y_min_l(n_spec), y_max_l(n_spec)
  real mass_l, mom_x_l, mom_y_l

  real a_conv_cgs, rho_conv_cgs, p_conv_atm

  integer i, j, n
  character*100 filename
  logical openfile
  integer total
  character*10 title(11+n_spec)
  character*10 units(11+n_spec)
  character*2 ext
  logical exist
!----------------------------------------------------------------------------------------
! set conversion factors

  a_conv_cgs=a_ref*100.0
  rho_conv_cgs=rho_ref*(1000.0/100.0**3)
  p_conv_atm=p_ref/pres_atm
!----------------------------------------------------------------------------------------
! set total

  total=11+n_spec
!----------------------------------------------------------------------------------------
! set title and units

  title( 1)='C01-i_time'; units( 1) = ' '
  title( 2)='C02-time  '; units( 2) = '(sec)'
  title( 3)='C03-u     '; units( 3) = '(cm/s)'
  title( 4)='C04-v     '; units( 4) = '(cm/s)'
  title( 5)='C05-w     '; units( 5) = '(cm/s)'
  title( 6)='C06-rho   '; units( 6) = '(g/cm^3)'
  title( 7)='C07-T     '; units( 7) = '(K)'
  title( 8)='C08-P     '; units( 8) = '(atm)'
  title( 9)='C09-Mass  '; units( 9) = 'g/cm^3'
  title(10)='C10-X Mom '; units(10) = ' '
  title(11)='C11-Y Mom '; units(11) = ' '

  do i=1,n_spec
    n=i+11
    write(ext,'(I2.2)') n
    title(n)='C'//ext//'-'//trim(species_name(i))
    units(n)=' '
  enddo
!-----------------------------------------------------------------------------------------
! calculate min and max velocity values

  do i=1,3,1

    u_min_l(i)=minval(u(:,:,:,i))
    u_max_l(i)=maxval(u(:,:,:,i))

    call MPI_Allreduce(u_min_l(i),u_min(i),1,MPI_REAL8,MPI_MIN,gcomm,ierr)
    call MPI_Allreduce(u_max_l(i),u_max(i),1,MPI_REAL8,MPI_MAX,gcomm,ierr)

    u_min(i)=u_min(i)*a_conv_cgs  !cm/sec
    u_max(i)=u_max(i)*a_conv_cgs  !cm/sec

  enddo
!-----------------------------------------------------------------------------------------
! calculate min and max density values

  rho_min_l=minval(q(:,:,:,4,1))
  rho_max_l=maxval(q(:,:,:,4,1))

  call MPI_Allreduce(rho_min_l,rho_min,1,MPI_REAL8,MPI_MIN,gcomm,ierr)
  call MPI_Allreduce(rho_max_l,rho_max,1,MPI_REAL8,MPI_MAX,gcomm,ierr)

  rho_min=rho_min*rho_conv_cgs   !g/cm^3
  rho_max=rho_max*rho_conv_cgs   !g/cm^3
!-----------------------------------------------------------------------------------------
! calculate min and max temperature values

  t_min_l=minval(temp(:,:,:))
  t_max_l=maxval(temp(:,:,:))

  call MPI_Allreduce(t_min_l,t_min,1,MPI_REAL8,MPI_MIN,gcomm,ierr)
  call MPI_Allreduce(t_max_l,t_max,1,MPI_REAL8,MPI_MAX,gcomm,ierr)

  t_min=t_min*t_ref   !K
  t_max=t_max*t_ref   !K
!-----------------------------------------------------------------------------------------
! calculate min and max pressure values

  p_min_l=minval(pressure(:,:,:))
  p_max_l=maxval(pressure(:,:,:))

  call MPI_Allreduce(p_min_l,p_min,1,MPI_REAL8,MPI_MIN,gcomm,ierr)
  call MPI_Allreduce(p_max_l,p_max,1,MPI_REAL8,MPI_MAX,gcomm,ierr)

  p_min=p_min*p_conv_atm   !atm
  p_max=p_max*p_conv_atm   !atm

!-- calculate total mass in domain
  mass_l = sum(q(:,:,:,4,1))
  call MPI_Allreduce(mass_l,mass,1,MPI_REAL8,MPI_SUM,gcomm,ierr)
  mass = mass*rho_ref  

!-- calculate net X momentum in domain
  mom_x = maxval(abs(q(:,:,:,1,1)))
  mom_x_l = sum(q(:,:,:,1,1))/max(mom_x,1.0)
  call MPI_Allreduce(mom_x_l,mom_x,1,MPI_REAL8,MPI_SUM,gcomm,ierr)
  mom_x = mom_x * rho_ref*a_ref

!-- Calculate net Y momentum in domain
  mom_y = maxval(abs(q(:,:,:,2,1)))
  mom_y_l = sum(q(:,:,:,2,1))/max(mom_y,1.0)
  call MPI_Allreduce(mom_y_l,mom_y,1,MPI_REAL8,MPI_SUM,gcomm,ierr)
  mom_y = mom_y * rho_ref*a_ref
!-----------------------------------------------------------------------------------------
! calculate min and max yspecies values

  do i=1,n_spec,1

    y_min_l(i)=minval(yspecies(:,:,:,i))
    y_max_l(i)=maxval(yspecies(:,:,:,i))

    call MPI_Allreduce(y_min_l(i),y_min(i),1,MPI_REAL8,MPI_MIN,gcomm,ierr)
    call MPI_Allreduce(y_max_l(i),y_max(i),1,MPI_REAL8,MPI_MAX,gcomm,ierr)

  enddo
!-----------------------------------------------------------------------------------------
! set values to be passed out

  temp_max_out=t_max
  pres_max_out=p_max
!-----------------------------------------------------------------------------------------
! write minimum data to output file

  if(myid.eq.0) then

!   open file

    filename='../data/'//trim(run_title)//'.min.dat'
    inquire(file=trim(filename),exist=exist)

    if(exist) then

      open(unit=1,file=trim(filename),status='old',position='append')

      if((i_restart==1).and.(i_time.eq.1)) then
        write(1,*)
        write(1,'(a37,1pe10.4,a6)') '# run was restarted here from time = ',  &
                                    time_restart,' (sec)'      !5sf
      endif

    else

      open(unit=1,file=trim(filename),status='unknown')
      write(1,1) (title(i), i=1,total,1)
      write(1,1) (units(i), i=1,total,1)

    endif

!   write data

    write(1,2) i_time, time*time_ref, (u_min(i), i=1,3,1),     &
        rho_min, t_min, p_min, mass, mom_x, mom_y,  &
        (y_min(j), j=1,n_spec,1)

!   close file

    close(1)

  endif
!-----------------------------------------------------------------------------------------
! write maximum data to output file

  if(myid.eq.0) then

!   open file

    filename='../data/'//trim(run_title)//'.max.dat'
    inquire(file=trim(filename),exist=exist)

    if(exist) then

      open(unit=1,file=trim(filename),status='old',position='append')

      if((i_restart==1).and.(i_time.eq.1)) then
        write(1,*)
        write(1,'(a37,1pe10.4,a6)') '# run was restarted here from time = ',  &
                                    time_restart,' (sec)'   !5sf
      endif

    else

      open(unit=1,file=trim(filename),status='unknown')
      write(1,1) (title(i), i=1,total,1)
      write(1,1) (units(i), i=1,total,1)

    endif

!   write data

    write(1,2) i_time, time*time_ref, (u_max(i), i=1,3,1),     &
        rho_max, t_max, p_max, mass, mom_x, mom_y,         &
        (y_max(j), j=1,n_spec,1)

!   close file

    close(1)

  endif
!----------------------------------------------------------------------------------------
! format statements

1 format('#',1x,1000(a10,3x))
2 format( 2x,i10,1x,1000(1pe11.3e3,2x))
!----------------------------------------------------------------------------------------
  return
  end subroutine monitor
!=========================================================================================
  subroutine tar_input_files(io)
!=========================================================================================
! routine tars input files after initialization for record of run parameters
! places tar file in data directory
!
! PC users must have tar.exe and cygwin1.dll files in path directory
!-----------------------------------------------------------------------------------------
  use topology_m, only : myid
  use runtime_m, only : run_title

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  character*100 tarcmd, copycmd1, copycmd2, delcmd
!-----------------------------------------------------------------------------------------
  if(myid == 0) then

    write(io,*) 'tarring input files for record of run parameters...'
    write(io,*) 'tar file is located at ../data/'//trim(run_title)//'.in.tar'
    write(io,*) 

#if PC
      copycmd1 = 'copy ..\input\chem.asc ..\run'
      copycmd2 = 'copy ..\input\*.in ..\run'
      tarcmd = 'tar -cf ..\data\'//trim(run_title)//'.in.tar *.in chem.asc'
      delcmd = 'del *.in chem.asc'
#else
      copycmd1 = 'cp ../input/chem.asc ../run'
      copycmd2 = 'cp ../input/*.in ../run'
      tarcmd = 'tar -cf ../data/'//trim(run_title)//'.in.tar *.in chem.asc'
      delcmd = 'rm *.in chem.asc'
#endif

      call execute_command( trim(copycmd1) )
      call execute_command( trim(copycmd2) )
      call execute_command( trim(tarcmd) )
      call execute_command( trim(delcmd) )

    call write_header(io,'-')

  endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine tar_input_files
!=========================================================================================
  subroutine generate_active_file(io)
!=========================================================================================
! routine generates active input file for changing a few key parameters
! while the code is running
!-----------------------------------------------------------------------------------------
#ifndef BUILD_LIBS3D
  use topology_m, only : myid, term_status
  use runtime_m, only : run_title, i_time_end, i_time_save, time_save_inc
  use runtime_m, only : i_time_mon, i_time_res, i_time_tec
  use filter_m, only : i_time_fil
  use rk_m, only : cont_switch, cfl_switch, tstep_min, tstep_max, tstep_init
  use rk_m, only : rk_rtol, rk_atol
  use rk_m, only : i_time_cont

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  character*100 filename
!-----------------------------------------------------------------------------------------
! generate active.in file

  if(myid.eq.0) then

!   write header

    write(io,*) 'generating active.in file...'
    write(io,*)

!   open file

    filename='../input/active.in'
    open(unit=56,file=trim(filename),status='unknown')

!   begin general parameters

    write(56,1) '0-continue, 1-terminate soon with savefiles         (term_status) ',  &
                term_status
    write(56,1) 'ending time step                                     (i_time_end) ',  &
                i_time_end
    write(56,1) 'frequency to save fields in restart files           (i_time_save) ',  &
                i_time_save
    write(56,2) 'time period to save fields in restart files       (time_save_inc) ',  &
                time_save_inc
    write(56,1) 'frequency to monitor min/max files and active        (i_time_mon) ',  &
                i_time_mon
    write(56,1) 'frequency to check resolution; set < 0 for no check  (i_time_res) ',  &
                i_time_res
    write(56,1) 'frequency write current tecplot file                 (i_time_tec) ',  &
                i_time_tec
    write(56,1) 'frequency to filter solution vector                  (i_time_fil) ',  &
                i_time_fil

!   begin controller parameters

    if(cont_switch==0) then

    write(56,2) 'initial/constant timestep (sec)                      (tstep_init) ',  &
                tstep_init

    else

    write(56,2) 'minimum timestep for controller (sec)                 (tstep_min) ',  &
                tstep_min
    write(56,2) 'maximum timestep for controller (sec)                 (tstep_max) ',  &
                tstep_max
    write(56,2) 'relative Runge-Kutta error tolerance                    (rk_rtol) ',  &
                rk_rtol
    write(56,2) 'absolute Runge-Kutta error tolerance                    (rk_atol) ',  &
                rk_atol
    write(56,1) 'controller cfl check, 0=off, 1=on                    (cfl_switch) ',  &
                cfl_switch

    endif

    write(56,1) 'timestep frequency to write controller info         (i_time_cont) ',  &
                i_time_cont

!   close file

    close(56)

!   write header

    call write_header(io,'-')

  endif
!----------------------------------------------------------------------------------------
! format statements

  1 format(a66,i12)
  2 format(a66,1pe12.5)
!----------------------------------------------------------------------------------------
#endif BUILD_LIBS3D
  return
  end subroutine generate_active_file
!=========================================================================================
  subroutine read_active_file(io)
!=========================================================================================
#ifndef BUILD_LIBS3D
! routine reads active input file and changes key parameters
! while the code is running
!
!
! BUG FIX Evatt Hawkes
! The previous code (whole subroutine commented out below) did not compile on seaborg.
! The new code is less efficient but more portable.
!
!-----------------------------------------------------------------------------------------
  use topology_m
  use runtime_m, only : run_title, i_time_end, i_time_save, time_save_inc
  use runtime_m, only : i_time_mon, i_time_res, i_time, i_time_tec
  use filter_m, only : i_time_fil
  use rk_m, only : cont_switch, cfl_switch, tstep_min, tstep_max, tstep_init
  use rk_m, only : rk_rtol, rk_atol
  use rk_m, only : i_time_cont

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  character*100 filename
  logical exist
  character*66 dummy  !length specific to format statement in routine generate_active_file

  integer, parameter :: n_intgrs=9, n_reals=6

  real, dimension(n_reals) :: r
  integer, dimension(n_intgrs) :: i

!-----------------------------------------------------------------------------------------
! return if not desired

  if(mod(i_time,i_time_mon).ne.0) return
!-----------------------------------------------------------------------------------------
! check for existence of active.in file

  if(myid.eq.0) then

    filename='../input/active.in'
    inquire(file=trim(filename),exist=exist)

    if(exist) then   !continue with read

      continue

    else      !generate and return

      write(io,*) 'active.in file does not exist...'
      write(io,*) 'regenerating active.in file...'
      call generate_active_file(io)
      return

    endif

  endif
!-----------------------------------------------------------------------------------------
! read active.in file

!jcs
!!$  time_save_inc=0.0
!!$  i_time_save=0

  if(myid.eq.0) then

!   open file

    filename='../input/active.in'
    open(unit=56,file=trim(filename),status='unknown',err=10)

!   begin general parameters

    read(56,1) dummy, term_status
    read(56,1) dummy, i_time_end
    read(56,1) dummy, i_time_save
    read(56,2) dummy, time_save_inc
    read(56,1) dummy, i_time_mon
    read(56,1) dummy, i_time_res
    read(56,1) dummy, i_time_tec
    read(56,1) dummy, i_time_fil

!   begin controller parameters

    if(cont_switch==0) then
      read(56,2) dummy, tstep_init
    else
      read(56,2) dummy, tstep_min
      read(56,2) dummy, tstep_max
      read(56,2) dummy, rk_rtol
      read(56,2) dummy, rk_atol
      read(56,1) dummy, cfl_switch
    endif

    read(56,1) dummy, i_time_cont
    goto 20

10  write(io,*) 'there is an error reading the active.in file...'
    write(io,*) 'proceeding without update of active variables...'

20  continue

    close(56)

  endif
!----------------------------------------------------------------------------------------
! broadcast parameters

  i(1) = term_status;    r(1) = time_save_inc
  i(2) = i_time_end ;    r(2) = tstep_init
  i(3) = i_time_save;    r(3) = tstep_min
  i(4) = i_time_mon ;    r(4) = tstep_max
  i(5) = i_time_res ;    r(5) = rk_rtol
  i(6) = i_time_tec ;    r(6) = rk_atol
  i(7) = i_time_fil
  i(8) = cfl_switch
  i(9) = i_time_cont

  call MPI_Bcast( i, n_intgrs, MPI_INTEGER, 0, gcomm, ierr )
  call MPI_Bcast( r, n_reals,  MPI_REAL8, 0, gcomm, ierr )

  term_status = i(1);     time_save_inc = r(1)
  i_time_end  = i(2);     tstep_init    = r(2)
  i_time_save = i(3);     tstep_min     = r(3)
  i_time_mon  = i(4);     tstep_max     = r(4)
  i_time_res  = i(5);     rk_rtol       = r(5)
  i_time_tec  = i(6);     rk_atol       = r(6)
  i_time_fil  = i(7);
  cfl_switch  = i(8);
  i_time_cont = i(9);
!----------------------------------------------------------------------------------------
! terminate if desired

  if(term_status.eq.1) call terminate_run(io,1)
  return
!----------------------------------------------------------------------------------------
! format statements

  1 format(a66,i12)
  2 format(a66,1pe12.5)
!----------------------------------------------------------------------------------------
#endif   BUILD_LIBS3D
  return
  end subroutine read_active_file


!----------------------------------------------------------------------
! New routine by Ramanan. 02/16/05
! This routine will execute the command using either `system' or `ishell'. 
! Gets rid of some of the #if preprocessor commands from the code.
!----------------------------------------------------------------------
  subroutine execute_command(cmd)
  
  character(*), intent(in)  :: cmd

#if defined(ARCH_SGI) || defined(ARCH_SP2) || defined(ARCH_OPTERON)
      call system(trim(cmd))
#endif

#if defined(ARCH_T3E) || defined(ARCH_X1)
      call ishell(trim(cmd))
#endif

  end subroutine execute_command
!----------------------------------------------------------------------

#ifdef ADIOS
!=========================================================================================
  subroutine readwrite_savefile_data_adios(filename,input)
!=========================================================================================
! reads and writes specific data for save file
! this routine exists so that the reading and writing are both in one location
!-----------------------------------------------------------------------------------------
  use topology_m, only : myid

  use runtime_m, only : time, tstep, time_save
  use variables_m, only : temp, pressure, yspecies, u
  use bc_m, only : qx_bc, qy_bc, qz_bc, pout
  use topology_m,  only : npes, mypx, mypy, mypz, gcomm
  use param_m,     only : nx, ny, nz, nx_g, ny_g, nz_g

  use chemkin_m,   only : species_name, n_species, molwt, element_name, n_elements
  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io, io_savefile
  character*1 input
  character*100 filename

! ADIOS stuff
  integer*8 adios_handle, adios_groupsize, adios_totalsize, adios_buf_size,&
            adios_group_handle
  integer adios_err
  integer*8, dimension(3) :: start, readsize

  ! local
  character*100 fieldname
  integer L
  integer  lx, ly, lz, gx, gy, gz
!-----------------------------------------------------------------------------------------
! write data

  ! local sizes are nx ny nz
  ! global offsets are
  lx = mypx*nx
  ly = mypy*ny
  lz = mypz*nz
  
  ! global sizes 
  gx = nx_g
  gy = ny_g
  gz = nz_g
  
  if(input.eq.'w') then

    if( myid == 0 ) then
        write(*,*) 'Writing data with adios to ', trim(filename)
    endif
    call adios_open( adios_handle, trim('restart')//CHAR(0), trim(filename)//CHAR(0), &
                     'w'//CHAR(0), adios_err)
    if( myid == 0) then
        write(*,*) 'Adios ierr_adios:', adios_err
    endif

    adios_groupsize = 4 &
    + 4 &
    + 4 &
    + 4 &
    + 4 &
    + 4 &
    + 4 &
    + 4 &
    + 4 &
    + 8 &
    + 8 &
    + 8 &
    + 8 &
    + 8 * (nz) * (ny) * (nx) &
    + 8 * (nz) * (ny) * (nx) &
    + 8 * nz * ny * nx *3 &
    + 8 * nz * ny * nx * n_species
    call adios_group_size (adios_handle, adios_groupsize, adios_totalsize, gcomm, adios_err)
    call adios_write (adios_handle, "gx"//char(0), gx, adios_err)
    call adios_write (adios_handle, "gy"//char(0), gy, adios_err)
    call adios_write (adios_handle, "gz"//char(0), gz, adios_err)
    call adios_write (adios_handle, "lx"//char(0), lx, adios_err)
    call adios_write (adios_handle, "ly"//char(0), ly, adios_err)
    call adios_write (adios_handle, "lz"//char(0), lz, adios_err)
    call adios_write (adios_handle, "nx"//char(0), nx, adios_err)
    call adios_write (adios_handle, "ny"//char(0), ny, adios_err)
    call adios_write (adios_handle, "nz"//char(0), nz, adios_err)
    call adios_write (adios_handle, "time"//char(0), time, adios_err)
    call adios_write (adios_handle, "tstep"//char(0), tstep, adios_err)
    call adios_write (adios_handle, "time_save"//char(0), time_save, adios_err)
    call adios_write (adios_handle, "pout"//char(0), pout, adios_err)
    call adios_write (adios_handle, "temp"//char(0), temp, adios_err)
    call adios_write (adios_handle, "pressure"//char(0), pressure, adios_err)
    !!
    call adios_write (adios_handle, "uvel"//char(0), u(:,:,:,1), adios_err)
    call adios_write (adios_handle, "vvel"//char(0), u(:,:,:,2), adios_err)
    call adios_write (adios_handle, "wvel"//char(0), u(:,:,:,3), adios_err)
    !
    DO L=1,n_species
        call adios_write (adios_handle, trim(species_name(L))//char(0), &
        yspecies(:,:,:,L), adios_err)
    ENDDO

    !#include "gwrite_restart.fh"
    call adios_close(adios_handle,adios_err)



  elseif(input.eq.'r') then


    call adios_fopen (adios_handle, trim(filename)//CHAR(0), gcomm, adios_err)
    call adios_gopen (adios_handle, adios_group_handle, trim('restart')//CHAR(0), adios_err)

    ! Scalars
    start(1) = 0
    readsize(1) = 1

    call adios_read_var (adios_group_handle, "time"//char(0), &
                         start(1), readsize(1), time, adios_err)
    call adios_read_var (adios_group_handle, "tstep"//char(0), &
                         start(1), readsize(1), tstep, adios_err)
    call adios_read_var (adios_group_handle, "time_save"//char(0), &
                         start(1), readsize(1), time_save, adios_err)
    call adios_read_var (adios_group_handle, "pout"//char(0), &
                         start(1), readsize(1), pout, adios_err)

    ! Field variables
    start(1)=lz
    start(2)=ly
    start(3)=lx

    readsize(1)=nz
    readsize(2)=ny
    readsize(3)=nx

    call adios_read_var (adios_group_handle, "temp"//char(0), &
                         start, readsize, temp, adios_err)
    call adios_read_var (adios_group_handle, "pressure"//char(0), &
                         start, readsize, pressure, adios_err)
    !!
    call adios_read_var (adios_group_handle, "uvel"//char(0), &
                         start, readsize, u(:,:,:,1), adios_err)
    call adios_read_var (adios_group_handle, "vvel"//char(0), &
                         start, readsize, u(:,:,:,2), adios_err)
    call adios_read_var (adios_group_handle, "wvel"//char(0), &
                         start, readsize, u(:,:,:,3), adios_err)
    !
    DO L=1,n_species
        call adios_read_var (adios_group_handle, trim(species_name(L))//char(0), &
                             start, readsize, yspecies(:,:,:,L), adios_err)
    ENDDO

    call adios_gclose( adios_group_handle, adios_err )
    call adios_fclose( adios_handle, adios_err )
    write(io, *) 'Read restart information from ', trim(filename)
else

    if(myid.eq.0) then
      write(io,*) 'improper setting for variable input'
      write(io,*) 'in routine readwrite_savefile_data'
    endif
    call terminate_run(io,0)  !must be called by all processors

  endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine readwrite_savefile_data_adios
!=========================================================================================

!=========================================================================================
  subroutine read_savefile_data_adios( filename, nxint, nyint, nzint, yspeciesint, uint, &
                                       pressureint, tempint)
!=========================================================================================
! reads and writes specific data for save file
! this routine exists so that the reading and writing are both in one location
!-----------------------------------------------------------------------------------------
  use topology_m, only : myid

  use runtime_m, only : time, tstep, time_save
  use variables_m, only : temp, pressure, yspecies, u
  use bc_m, only : qx_bc, qy_bc, qz_bc, pout
  use topology_m,  only : npes, mypx, mypy, mypz, gcomm

  use chemkin_m,   only : species_name, n_species, molwt, element_name, n_elements
  use param_m, only : nsc
  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in
  integer io, io_savefile
  character*100 filename
  integer, intent(in) :: nxint, nyint, nzint

  real, dimension(nxint,nyint,nzint,nsc+1), intent(out) :: yspeciesint
  real, dimension(nxint,nyint,nzint,3), intent(out) :: uint
  real, dimension(nxint,nyint,nzint), intent(out) :: pressureint
  real, dimension(nxint,nyint,nzint), intent(out) :: tempint

! ADIOS stuff
  integer*8 adios_handle, adios_groupsize, adios_totalsize, adios_buf_size,&
            adios_group_handle
  integer adios_err

  ! local
  character*100 fieldname
  integer L
  integer  lx, ly, lz, gx, gy, gz
!-----------------------------------------------------------------------------------------
! write data

  ! local sizes are nx ny nz
  
    integer*8, dimension(3) :: start, readsize

    call adios_fopen (adios_handle, trim(filename)//CHAR(0), gcomm, adios_err)
    call adios_gopen (adios_handle, adios_group_handle, trim('restart')//CHAR(0), adios_err)

    ! Scalars
    start(1) = 0
    readsize(1) = 1

    !call adios_read_var (adios_group_handle, "time"//char(0), &
    !                     start(1), readsize(1), time, adios_err)
    !call adios_read_var (adios_group_handle, "tstep"//char(0), &
    !                     start(1), readsize(1), tstep, adios_err)
    !call adios_read_var (adios_group_handle, "time_save"//char(0), &
    !                     start(1), readsize(1), time_save, adios_err)
    !call adios_read_var (adios_group_handle, "pout"//char(0), &
    !                     start(1), readsize(1), pout, adios_err)

    ! Field variables
    ! global offsets are
    start(3) = nxint * mypx
    start(2) = nyint * mypy
    start(1) = nzint * mypz

    readsize(3)=nxint
    readsize(2)=nyint
    readsize(1)=nzint

    call adios_read_var (adios_group_handle, "temp"//char(0), &
                         start, readsize, tempint, adios_err)
    call adios_read_var (adios_group_handle, "pressure"//char(0), &
                         start, readsize, pressureint, adios_err)
    !!
    call adios_read_var (adios_group_handle, "uvel"//char(0), &
                         start, readsize, uint(:,:,:,1), adios_err)
    call adios_read_var (adios_group_handle, "vvel"//char(0), &
                         start, readsize, uint(:,:,:,2), adios_err)
    call adios_read_var (adios_group_handle, "wvel"//char(0), &
                         start, readsize, uint(:,:,:,3), adios_err)
    !
    DO L=1,n_species
        call adios_read_var (adios_group_handle, trim(species_name(L))//char(0), &
                             start, readsize, yspeciesint(:,:,:,L), adios_err)
    ENDDO

    call adios_gclose( adios_group_handle, adios_err )
    call adios_fclose( adios_handle, adios_err )
    if( myid == 0) write(io, *) 'Read restart information to intermediate variables from ', trim(filename)
!-----------------------------------------------------------------------------------------
  return
  end subroutine read_savefile_data_adios
!=========================================================================================
#endif

#include "globalDefines.h"
module par_filter_m
! =============================================================================
! Routines for spatially filtering DNS fields
! Author - Ray Grout (2008)
!
! Notes:
! - Control options by setting values in par_filter_param
! - Very efficient for output on small grids; for output on every 
!   point of the input grid Evatt's routines are probably better
! - Ed Richardson has made a derivative of this to do conditional filtering; if 
!   that's what you want to do get in touch with him and save some time...
! =============================================================================

  public

  ! ==== Variables for filter computed from par_filter_control variables ====
  ! Call subroutine refresh_filter_params to reinit these values
  REAL, PRIVATE :: filinflu, delta
  REAL, PRIVATE :: G1 
  INTEGER, PRIVATE :: method
  INTEGER, PRIVATE :: nxf, nyf, nzf
  REAL, PRIVATE :: deltafx, deltafy, deltafz
  REAL, PRIVATE, ALLOCATABLE, DIMENSION(:,:) :: FX, FY, FZ
  REAL, PRIVATE, ALLOCATABLE, DIMENSION(:,:) :: XG, YG, ZG
  !  ==== End filter values computed from par_filter_control variables ====

  ! Input parameters for filter control
  TYPE :: par_filter_control

    ! use to exclude calculation of points far from filtered grid point
    REAL :: filinflu1

    REAL :: delta1
    INTEGER :: nxf_g 
    INTEGER :: nyf_g
    INTEGER :: nzf_g
    real :: delta_fil_grid_x 
    real :: delta_fil_grid_y 
    real :: delta_fil_grid_z 
    
    real :: start_fil_grid_x 
    real :: start_fil_grid_y 
    real :: start_fil_grid_z 

    integer :: n_fil_grid_x 
    integer :: n_fil_grid_y 
    integer :: n_fil_grid_z 

    real :: delta_expansion_fac_x = 1.0
    real :: delta_expansion_fac_y = 1.0
    real :: delta_expansion_fac_z = 1.0
  END type par_filter_control

  TYPE :: par_filter_output_control
     ! These are for external control
     logical :: output_uvw = .false.
     logical :: output_rho = .false.
     logical :: output_temp = .false.
     logical :: output_spc = .false.
     logical :: output_rates = .false.
     logical :: output_dilitation = .false.
     logical :: output_vorticity = .false.
     logical :: output_mixfrac = .false.
     logical :: output_chi = .false.
     logical :: output_chi_sep = .false.
     logical :: output_pressure = .false.

     CHARACTER(LEN=50) :: filename   ! File name
  END type par_filter_output_control

  type(par_filter_output_control) :: write_filtered_param
  type(par_filter_control) :: par_filter_param



  contains

  ! ============================================================================ 
  ! Reinitialize filter control variables from parameter object
  ! ============================================================================ 
  SUBROUTINE refresh_par_filter_params(io)
    use reference_m, only : l_ref
    use param_m, only : npx, npy, npz, nx, ny, nz
    use topology_m 
    use grid_m, only : x, y, z
    IMPLICIT NONE
    integer io
    REAL, ALLOCATABLE, DIMENSION(:) :: FXG, FYG, FZG
    integer i, j, k, id

    ! Scalars -----------------------------------------------------------------
    filinflu = par_filter_param%filinflu1/l_ref
    delta = par_filter_param%delta1/l_ref
    deltafx = delta * par_filter_param%delta_expansion_fac_x
    deltafy = delta * par_filter_param%delta_expansion_fac_y
    deltafz = delta * par_filter_param%delta_expansion_fac_z

    if( method == 1 ) then
       G1 = 1.0/deltafx/deltafy/deltafz ! Constant for box filter
    else if(method == 2) then
       G1 = sqrt( 6.0d0 / 3.14149 / delta**6 ) ! Constant for Gaussian
    else
       write(*,*) 'Method must be 1 (box) or 2 (Gaussian)'
       stop
    endif


    ! Grid ---------------------------------------------------------------------
    if( mod(par_filter_param%nxf_g,npx).ne.0) then
       write(io,*) &
          'Filtered grid size is not perfectly divisible by npx in x-direction'
       stop
    else
       NXF = par_filter_param%nxf_g/npx
    endif

    if( mod(par_filter_param%nyf_g,npy).ne.0) then
       write(io,*) &
          'Filtered grid size is not perfectly divisible by npy in y-direction'
       stop
    else
       NYF = par_filter_param%nyf_g/npy
    endif

    if( mod(par_filter_param%nzf_g,npz).ne.0) then
       write(io,*) &
          'Filtered grid size is not perfectly divisible by npz in z-direction'
       stop
    else
       NZF = par_filter_param%nzf_g/npz
    endif


    IF( ALLOCATED(XG) ) DEALLOCATE(XG )
    IF( ALLOCATED(YG) ) DEALLOCATE(YG )
    IF( ALLOCATED(ZG) ) DEALLOCATE(ZG )
    allocate(XG(nx,npx*npy*npz)) ! To store x/y/z grid from all nodes
    allocate(YG(ny,npx*npy*npz))
    allocate(ZG(nz,npx*npy*npz))
    allocate(fxg(par_filter_param%nxf_g))
    allocate(fyg(par_filter_param%nyf_g))
    allocate(fzg(par_filter_param%nzf_g))

    if(myid==0) then
       FXG = 0.0d0
       FYG = 0.0d0  
       FZG = 0.0d0

       do i=1,par_filter_param%nxf_g
          FXG(i) = (i-1)*par_filter_param%delta_fil_grid_x &
                   + par_filter_param%start_fil_grid_x  
       enddo

       do j=1,par_filter_param%nyf_g
          FYG(j) = (j-1)*par_filter_param%delta_fil_grid_y &
                   + par_filter_param%start_fil_grid_y  
       enddo

       do k=1,par_filter_param%nzf_g
          FZG(k) = (k-1)*par_filter_param%delta_fil_grid_z &
                   + par_filter_param%start_fil_grid_z  
       enddo

       FXG = FXG/l_ref
       FYG = FYG/l_ref
       FZG = FZG/l_ref
    endif

    ! Broadcast FxyzG to all root processes - easiest just to broadcast to all
    CALL MPI_Bcast( FXG, par_filter_param%nxf_g, &
                    MPI_DOUBLE_PRECISION, 0, gcomm, ierr)
    CALL MPI_Bcast( FYG, par_filter_param%nyf_g, &
                    MPI_DOUBLE_PRECISION, 0, gcomm, ierr)
    CALL MPI_Bcast( FZG, par_filter_param%nzf_g, &
                    MPI_DOUBLE_PRECISION, 0, gcomm, ierr)
    
    IF( ALLOCATED(fx) ) DEALLOCATE( fx )
    IF( ALLOCATED(fy) ) DEALLOCATE( fy )
    IF( ALLOCATED(fz) ) DEALLOCATE( fz )

    allocate(fx(nxf,npx*npy*npz))
    allocate(fy(nyf,npx*npy*npz))
    allocate(fz(nzf,npx*npy*npz))

    FX = 0
    FY = 0
    FZ = 0

    CALL MPI_Scatter( fxg, nxf, MPI_DOUBLE_PRECISION, &
                      FX(:,myid+1), nxf, MPI_DOUBLE_PRECISION, 0,xcomm, ierr)
    CALL MPI_Scatter( fyg, nyf, MPI_DOUBLE_PRECISION, &
                      FY(:,myid+1), nyf, MPI_DOUBLE_PRECISION, 0,ycomm, ierr)
    CALL MPI_Scatter( fzg, nzf, MPI_DOUBLE_PRECISION, &
                      FZ(:,myid+1), nzf, MPI_DOUBLE_PRECISION, 0,zcomm, ierr)


    deallocate(fxg)
    deallocate(fyg)
    deallocate(fzg)


    ! Sync ouput grid across processes
    DO id = 0,npx*npy*npz-1
       CALL MPI_Bcast( FX(:,id+1), nxf, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
       CALL MPI_Bcast( FY(:,id+1), nyf, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
       CALL MPI_Bcast( FZ(:,id+1), nzf, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
    ENDDO



    ! Move global input grid to all 
    XG(:,myid+1) = x
    YG(:,myid+1) = y
    ZG(:,myid+1) = z
    DO id = 0,npx*npy*npz-1
       CALL MPI_Bcast( XG(:,id+1), nx, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
       CALL MPI_Bcast( YG(:,id+1), ny, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
       CALL MPI_Bcast( ZG(:,id+1), nz, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
    ENDDO


  END SUBROUTINE

  ! ============================================================================ 
  ! Set filter input parameters (eventually read from input file)
  ! ============================================================================ 
  SUBROUTINE init_par_filter(io)

  use topology_m

  IMPLICIT NONE

  integer io
  real rscratch

  ! Open input file to read control parameters
  IF(myid == 0) THEN
    OPEN(UNIT=20, file='../input/par_filter.in',status='old',form='formatted')

    READ(20,*)
    READ(20,*)
    READ(20,*)

    READ(20,*) method 
    READ(20,*) par_filter_param%delta1 
    READ(20,*) rscratch
    par_filter_param%filinflu1= rscratch*par_filter_param%delta1
    READ(20,*) par_filter_param%delta_fil_grid_x
    READ(20,*) par_filter_param%delta_fil_grid_y
    READ(20,*) par_filter_param%delta_fil_grid_z

    READ(20,*) par_filter_param%start_fil_grid_x
    READ(20,*) par_filter_param%start_fil_grid_y
    READ(20,*) par_filter_param%start_fil_grid_z

    READ(20,*) par_filter_param%nxf_g
    READ(20,*) par_filter_param%nyf_g
    READ(20,*) par_filter_param%nzf_g

    READ(20,*)
    READ(20,*)
    READ(20,*)

    READ(20,*) write_filtered_param%output_uvw
    READ(20,*) write_filtered_param%output_rho
    READ(20,*) write_filtered_param%output_temp
    READ(20,*) write_filtered_param%output_spc
    READ(20,*) write_filtered_param%output_mixfrac
    READ(20,*) write_filtered_param%output_chi
    READ(20,*) write_filtered_param%output_chi_sep
    READ(20,*) write_filtered_param%output_pressure
    READ(20,*) write_filtered_param%output_rates
    READ(20,*) write_filtered_param%output_dilitation
    READ(20,*) write_filtered_param%output_vorticity

    close(20)
  ENDIF

    CALL MPI_Bcast( method, 1, MPI_INTEGER, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%delta1, 1, MPI_DOUBLE_PRECISION, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%filinflu1, 1, MPI_DOUBLE_PRECISION, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%delta_fil_grid_x, 1, MPI_DOUBLE_PRECISION, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%delta_fil_grid_y, 1, MPI_DOUBLE_PRECISION, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%delta_fil_grid_z, 1, MPI_DOUBLE_PRECISION, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%start_fil_grid_x, 1, MPI_DOUBLE_PRECISION, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%start_fil_grid_y, 1, MPI_DOUBLE_PRECISION, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%start_fil_grid_z, 1, MPI_DOUBLE_PRECISION, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%nxf_g, 1, MPI_INTEGER, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%nyf_g, 1, MPI_INTEGER, 0, gcomm, ierr )
    CALL MPI_Bcast( par_filter_param%nzf_g, 1, MPI_INTEGER, 0, gcomm, ierr )


    CALL MPI_Bcast( write_filtered_param%output_uvw, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_rho, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_temp, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_spc, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_mixfrac, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_chi, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_chi_sep, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_pressure, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_rates, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_dilitation, 1, MPI_LOGICAL, 0, gcomm, ierr )
    CALL MPI_Bcast( write_filtered_param%output_vorticity, 1, MPI_LOGICAL, 0, gcomm, ierr )

  call refresh_par_filter_params(io)
  if( myid == 0 ) then
    write(io,*) &
    'refresh_par_filter_params must be called if control params changed'
  endif

  END SUBROUTINE

  ! ============================================================================ 
  ! Driver routine to test filter
  ! ============================================================================ 
SUBROUTINE filter_field(io)
  use variables_m
#ifdef HDF5
  use hdf5
#endif
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g
  use reference_m
  use runtime_m, only : run_title, time, i_time, tstep
  use variables_m, only : q, u, temp, pressure, yspecies
  use chemkin_m, only : species_name, n_species, reaction_rate
  use runtime_m, only : run_title, time, i_time
  use work_m, only : hr => work1_2
  IMPLICIT NONE

  real p_conv_atm
  INTEGER io
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: filteredfield, numer, denom, cond
  real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion

  LOGICAL :: overwrite
  INTEGER :: error
  real x2(nx/2)
  integer i2, i, j,k,l

  character*9 time_ext
  character*100 filename_short
  CHARACTER(LEN=8) :: dsetname   ! Dataset name
  CHARACTER(LEN=20) :: fieldname  ! Field name
  CHARACTER(LEN=20) :: groupname  ! Group name
  CHARACTER(LEN=50) :: filename   ! File name
  CHARACTER(LEN=250) :: fielddesc  ! Very long field name
  CHARACTER(LEN=20) :: fieldunits  ! Field units

    p_conv_atm=p_ref/pres_atm
    ! Specify filename on root, broadcast
    IF (myid==0) then  
       !   set time stamp 
       write(time_ext,'(1pe9.3)') time*time_ref 
       filename_short=trim(run_title)//'_filtered.'//trim(time_ext)//'.h5'
       filename='../post/hdf5/'//trim(filename_short)
       filename=trim(filename)

       write(io,*) 'Writing filtered data to ', filename

       ! make sure directory exists
#ifdef SYSTEMCALLWONTWORK
       call makedirectory('../post/hdf5'//char(0))
#else
       call execute_command( 'mkdir ../post/hdf5')
#endif
    ENDIF
    
    call MPI_Barrier(gcomm, ierr) ! Wait for directory to be created
    call MPI_Bcast(filename,50, MPI_CHARACTER, 0, gcomm, ierr)

    ! Store filename for later external calls to add fields    
    write_filtered_param%filename = filename 

  allocate(numer(nxf,nyf,nzf))
  allocate(denom(nxf,nyf,nzf))
  allocate(cond(nx,ny,nz))
  allocate(filteredfield(nxf,nyf,nzf))
    
    numer = 0
    denom = 0
    cond = 1.0

#ifndef HDF5
    if(myid==0) write(io,*) 'Not writing out filtered field because not build with HDF5 support'
#endif

    overwrite = .true.

    if( write_filtered_param%output_uvw ) then
      fieldname = "u"
      fielddesc = "Velocity component"
      fieldunits = "m/s"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, u(:,:,:,1), cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredfield = numer/denom * a_ref
      end where

#ifdef HDF5
      call write_field(io, filteredfield, filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif


      overwrite = .false.
      fieldname = "v"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, u(:,:,:,2), cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredfield = numer/denom * a_ref
      end where
#ifdef HDF5
      call write_field(io, filteredfield, filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif


      fieldname = "w"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, u(:,:,:,3), cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredfield = numer/denom * a_ref
      end where
#ifdef HDF5
      call write_field(io, filteredfield, filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif
    endif

    if( write_filtered_param%output_rho ) then
      fieldname = "rho"
      fielddesc = "Density"
      fieldunits = "g/cm^3"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, q(:,:,:,4,1), cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredfield = numer/denom * rho_ref
      end where

#ifdef HDF5
      call write_field(io, filteredfield, filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif
      overwrite = .false.
    endif

    if( write_filtered_param%output_temp ) then
      fieldname = "Temp"
      fielddesc = "Temperature"
      fieldunits = "K"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, temp, cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredfield = numer/denom * t_ref
      end where

#ifdef HDF5
      call write_field(io, filteredfield, filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif
      overwrite = .false.
    endif

    if( write_filtered_param%output_pressure ) then
      fieldname = "P"
      fielddesc = "Pressure"
      fieldunits = "atm"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, pressure, cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredfield = numer/denom * p_conv_atm
      end where

#ifdef HDF5
      call write_field(io, filteredfield, filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif
      overwrite = .false.
    endif


    if( write_filtered_param%output_spc ) then
       fielddesc = "Species mass fraction"
       fieldunits = "kg/kg_mix"
       DO L=1,n_species
          write(fieldname,*)'"Y '//trim(species_name(L))//'"'
          numer = 0.0d0
          denom = 0.0d0
          cond = 1.0
          CALL generalized_filter(io, yspecies(:,:,:,L), cond, cond, numer, denom)
          call mpi_barrier(gcomm, ierr)
          where(denom .ne. 0) 
            filteredfield = numer/denom 
          end where

#ifdef HDF5
          call write_field(io, filteredfield, filename, groupname, fieldname, &
          fielddesc, fieldunits, overwrite)
#endif
          overwrite = .false.
       ENDDO
    endif

    if( write_filtered_param%output_rates ) then
      fielddesc = "Species reaction rates"
      fieldunits = "1/s"
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
      DO L=1,n_species
        write(fieldname,*)'"RR '//trim(species_name(L))//'"'
        numer = 0.0d0
        denom = 0.0d0
        cond = 1.0
        CALL generalized_filter(io, rr_r(:,:,:,L), cond, cond, numer, denom)
        call mpi_barrier(gcomm, ierr)
        where(denom .ne. 0) 
          filteredfield = numer/denom 
        end where

#ifdef HDF5
        call write_field(io, filteredfield, filename, groupname, fieldname, &
        fielddesc, fieldunits, overwrite)
#endif
        overwrite = .false.
      ENDDO
      call calc_heat_release(rr_r,hr)
      fielddesc = "Heat release rate"
      fieldunits = "J/m^3/s"
      fieldname = "hrr"
      numer = 0.0d0
      cond = 1.0
      denom = 0.0d0
      CALL generalized_filter(io, hr, cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredfield = numer/denom * (-1.0*hr_ref)
      end where

#ifdef HDF5
      call write_field(io, filteredfield, filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif
    endif

    deallocate(cond)
    deallocate(numer)
    deallocate(denom)
    deallocate(filteredfield)
    
  END SUBROUTINE FILTER_FIELD

  ! ============================================================================ 
  ! Driver routine to filter frozenfeed data
  ! ============================================================================ 
SUBROUTINE filter_frozenfeed(io)
  use variables_m
#ifdef HDF5
  use hdf5
#endif
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g
  use reference_m
  use runtime_m, only : run_title, time, i_time, tstep
  use variables_m, only : q, u, temp, pressure, yspecies
  use chemkin_m, only : species_name, n_species, reaction_rate
  use runtime_m, only : run_title, time, i_time
  use work_m, only : hr => work1_2
  IMPLICIT NONE

  real p_conv_atm
  INTEGER io
  REAL, ALLOCATABLE, DIMENSION(:, :, :) :: numer, denom, cond
  REAL, ALLOCATABLE, DIMENSION(:, :, :, :) :: filteredvelocities
  real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion

  LOGICAL :: overwrite
  INTEGER :: error
  real x2(nx/2)
  integer i2, i, j,k,l

  character*9 time_ext
  character*100 filename_short
  CHARACTER(LEN=8) :: dsetname   ! Dataset name
  CHARACTER(LEN=20) :: fieldname  ! Field name
  CHARACTER(LEN=20) :: groupname  ! Group name
  CHARACTER(LEN=50) :: filename   ! File name
  CHARACTER(LEN=250) :: fielddesc  ! Very long field name
  CHARACTER(LEN=20) :: fieldunits  ! Field units

    p_conv_atm=p_ref/pres_atm
    ! Specify filename on root, broadcast
    IF (myid==0) then  
       !   set time stamp 
       write(time_ext,'(1pe9.3)') time*time_ref 
       filename_short=trim(run_title)//'_filtered.'//trim(time_ext)//'.h5'
       filename='../post/hdf5/'//trim(filename_short)
       filename=trim(filename)

       write(io,*) 'Writing filtered data to ', filename

       ! make sure directory exists
#ifdef SYSTEMCALLWONTWORK
       call makedirectory('../post/hdf5'//char(0))
#else
       call execute_command( 'mkdir ../post/hdf5')
#endif
    ENDIF
    
    call MPI_Barrier(gcomm, ierr) ! Wait for directory to be created
    call MPI_Bcast(filename,50, MPI_CHARACTER, 0, gcomm, ierr)

    ! Store filename for later external calls to add fields    
    write_filtered_param%filename = filename 

  allocate(numer(nxf,nyf,nzf))
  allocate(denom(nxf,nyf,nzf))
  allocate(cond(nx,ny,nz))
  allocate(filteredvelocities(nxf,nyf,nzf,3))
    
    numer = 0
    denom = 0
    cond = 1.0

#ifndef HDF5
    if(myid==0) write(io,*) 'Not writing out filtered field because not build with HDF5 support'
#endif

    overwrite = .true.

    if( write_filtered_param%output_uvw ) then
      fieldname = "u"
      fielddesc = "Velocity component"
      fieldunits = "m/s"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, u(:,:,:,1), cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredvelocities(:,:,:,1) = numer/denom * a_ref
      end where

#ifdef HDF5
      call write_field(io, filteredvelocities(:,:,:,1), filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif


      overwrite = .false.
      fieldname = "v"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, u(:,:,:,2), cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredvelocities(:,:,:,2) = numer/denom * a_ref
      end where
#ifdef HDF5
      call write_field(io, filteredvelocities(:,:,:,2), filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif


      fieldname = "w"
      numer = 0.0d0
      denom = 0.0d0
      cond = 1.0
      CALL generalized_filter(io, u(:,:,:,3), cond, cond, numer, denom)
      call mpi_barrier(gcomm, ierr)
      where(denom .ne. 0) 
        filteredvelocities(:,:,:,3) = numer/denom * a_ref
      end where
#ifdef HDF5
      call write_field(io, filteredvelocities(:,:,:,3), filename, groupname, fieldname, &
      fielddesc, fieldunits, overwrite)
#endif
    endif

    deallocate(cond)
    deallocate(numer)
    deallocate(denom)
    deallocate(filteredvelocities)
    
  END SUBROUTINE FILTER_FROZENFEED


SUBROUTINE GENERALIZED_FILTER( io, field, weight, cond, numer, denom )
  ! ==================================================================================================
  ! GENERALIZED FILTER ROUTINE - RAY GROUT , APRIL 2008
  ! INPUT:
  !      io          UNIT FOR MESSAGES
  !      field       INPUT FIELD 
  !      filfield    OUTPUT FIELD WITH DIMENSIONS (nxf, nyf, nzf)
  !      fx, fy, fz  OUTPUT GRID ON EACH PROC SHOULD BE STORED IN fx(:,myid+1)
  !      method      1 FOR BOX FILTER, 2 FOR GAUSSIAN
  !      delta       FILTER WIDTH
  !      filinflu    CUTOFF DISTANCE FOR CALCULATION OF CONTRIBUTION
  !                  (USE DELTA/2 FOR OPTIMAL BOX FILTER CALCULATION)
  ! ==================================================================================================




  ! MODULES AND VARIABLES ============================================================================
    use topology_m
    use param_m, only : nx, ny, nz, npx, npy, npz
    use reference_m
    use grid_m, only : x, y, z
  
    IMPLICIT NONE

    ! DECLARATIONS PASSED IN
    REAL, DIMENSION(nx,ny,nz) :: field
    REAL, DIMENSION(nx,ny,nz) :: weight
    REAL, DIMENSION(nx,ny,nz) :: cond
    REAL, DIMENSION(nxf,nyf,nzf) :: numer
    REAL, DIMENSION(nxf,nyf,nzf) :: denom
    REAL, DIMENSION(nxf,nyf,nzf) :: filfield ! This is a working variable now
    INTEGER, intent(in) :: io

    ! SCRATCH / WORKING VARIABLES
    REAL, DIMENSION(nxf,nyf,nzf) :: filfield_norm
    REAL, DIMENSION(nxf,nyf,npx*npy*npz) :: contrib
    REAL, DIMENSION(nxf,nyf,npx*npy*npz) :: contrib_norm_fac

    


    INTEGER i,j,k, ifil,jfil,kfil, id, expstart, expstop,nexp ! counters


    REAL XVAL(3), ZVAL(3), XINFLU, ZINFLU ! For trimming portion of filter grid
                                          ! to be evaluated

    ! These take part in array calculations to let compiler vectorize easily
    real r(nxf), G(nx), Gtemp(nx)
    logical computecontrib(nxf)

    real deltax(nx), deltay(ny), deltaz(nz) ! This is distance from filter grid point for kernel

    real dx(nx), dy(ny), dz(nz), dv(nx,ny,nz)  ! This is for differential

    
    
    ! SETUP WORKING GRIDS AND DIFFERENTIAL VOLUME =====================================================


    ! Compute dv for each node on this proc, referring to neighbours as necessary at edges
    do i=2,nx-1
       dx(i) = 0.5d0*( x(i+1) - x(i-1) )
    enddo
    if( neighbor(1) >= 0) then
       dx(1) = 0.5d0*(x(2) - XG(nx,neighbor(1)+1) )
    else
       dx(1) = x(2) - x(1) 
    endif
    if( neighbor(2) >= 0) then
       dx(nx) = 0.5d0*(XG(1,neighbor(2)+1) - x(nx-1))
    else
       dx(nx) = x(nx) - x(nx-1)
    endif

      

    do i=2,ny-1
       dy(i) = 0.5d0*( y(i+1) - y(i-1) )
    enddo
    if( neighbor(3) >= 0) then
       dy(1) = 0.5d0*(y(2) - YG(ny,neighbor(3)+1))
    else
       dy(1) = y(2) - y(1)          
    endif
    if( neighbor(4) >=0 ) then
       dy(ny) = 0.5*(YG(1,neighbor(4)+1) - y(ny-1) )
    else
       dy(ny) = y(ny) - y(ny-1)   
    endif



    
    do i=2,nz-1
       dz(i) = 0.5d0*( z(i+1) - z(i-1) )
    enddo
    if( neighbor(5) >=0) then
       dz(1) = 0.5*(z(2) - ZG(nz,neighbor(5)+1))
    else
       dz(1) = z(2) - z(1) 
    endif
    if( neighbor(6) >=0) then
       dz(nz) = 0.5*(ZG(1,neighbor(6)+1) - z(nz-1))
    else
       dz(nz) = z(nz) - z(nz-1)
    endif

    

    
    do k=1,nz
       do j=1,ny
          do i=1,nx
             dv(i,j,k) =dx(i) * dy(j) * dz(k)
          enddo
       enddo
    enddo


    ! COMPUTE CONTRIBUTION TO FILTERED GRID POINTS FROM LOCAL POINTS ============================
    DO kfil =1,nzf ! Loop over k-planes in output grid so that working buffer needs to only be of size nxf*nyf
       contrib_norm_fac = 0
       contrib = 0

       DO id = 0, npx*npy*npz-1 ! Loop over contribution to all other processes bit of output grid
          DO jfil = 1, nyf  ! Loop over output grid j-planes
             
             ! Check to see how close (conservatively) a point here could be to a bit of the output
             ! grid on proc id
             r = ( FX(:,id+1) - x((nx/2)) )**2 &
                  + ( FY(jfil,id+1)- y((ny/2)) )**2 &
                  + ( FZ(kfil,id+1) - z((nz/2)) )**2 

             r = sqrt(r) - sqrt( (x(nx)-x(1))**2 + (y(ny) - y(1))**2 + (z(nz) - z(1))**2 ) 

             WHERE( r < 0 )
                r = 0
             ELSEWHERE
                r = r / 1.732 ! dx = dy = dz with same r = r/sqrt(3)
             END WHERE

             WHERE( r > filinflu )
                computecontrib =  .FALSE.
             ELSEWHERE
                computecontrib = .TRUE.
             END WHERE

! check whether the point is within one filter half width plus one stencil half width.
! we assume that the stencil half width is four times the distance between output grid points
! since the output grid is usually equal or coarser than the input grid.
!if(xval(1).gt.0.0)then
!xinflu=5.0*(x(2)-x(1))+deltafx/2.0
!xinflu=xinflu*xinflu
!WHERE( (FX(:,id+1)-xval(1))**2.0.gt. xinflu .and. &
!       (FX(:,id+1)-xval(2))**2.0.gt. xinflu .and. &
!       (FX(:,id+1)-xval(3))**2.0.gt. xinflu ) 
!computecontrib = .FALSE.
!END WHERE
!endif
!
!
!zinflu=5.0*(z(2)-z(1))+deltafz/2.0
!zinflu=zinflu*zinflu
!IF(    zval(1).gt.0.0 .and. &
!       (FZ(kfil,id+1)-zval(1))**2.0.gt. zinflu .and. &
!       (FZ(kfil,id+1)-zval(2))**2.0.gt. zinflu .and. &
!       (FZ(kfil,id+1)-zval(3))**2.0.gt. zinflu ) THEN 
!computecontrib(:) = .FALSE.
!ENDIF

             
   

computecontrib = .true.
             DO ifil = 1,nxf ! Loop over output i-planes
                
                IF( computecontrib(ifil) ) THEN ! Check to see if it's worth computing contrib

                   deltaz = ABS( FZ(kfil,id+1) - z )
                   deltay = ABS( FY(jfil,id+1) - y )
                   deltax = ABS( FX(ifil,id+1) - x )

                   IF( method == 1) THEN 
                      ! For box filter. If things get slow, comment this bit (including the if) when using gaussian
                      ! especially for fine output grids
                      G = 0
                      DO k=1,nz
                         if( deltaz(k) .LE. deltafz/2 ) THEN
                            DO j=1,ny
                               IF(deltay(j) .LE. deltafy/2 ) THEN
                                  WHERE (deltax  .LE. deltafx/2 )
                                     G = 1
                                  ELSEWHERE
                                     G = 0
                                  END WHERE
                                  G = G*dv(:,j,k)*G1
                                  
                                  contrib(ifil,jfil, id+1) = contrib(ifil,jfil, id+1) + SUM(G * field(:,j,k) )
                                  contrib_norm_fac(ifil,jfil, id+1) = contrib_norm_fac(ifil,jfil, id+1) + SUM(G) 
                               ENDIF
                            ENDDO
                         ENDIF
                      ENDDO
                   ELSE IF (method == 2) THEN
                      ! For Gaussian filter. If things get slow, comment this bit (including the if) when using box filter
                      ! especially for fine output grids
                      G = 0
                      DO k=1,nz
                         if( deltaz(k) .le. filinflu ) THEN
                            DO j=1,ny
                               if(deltay(j) .le. filinflu ) THEN
#ifdef ACMLAVAIL
                                  ! Vec exponential calc option ---------------
                                  expstart = 0
                                  expstop = 0
                                  G = 0.0d0
                                  do i=1,nx
                                     if( deltax(i) .le. filinflu ) then
                                        expstart = i
                                        exit
                                     endif
                                  enddo
                                  do i=nx,1,-1
                                     if( deltax(i) .le. filinflu ) then
                                        expstop = i
                                        exit
                                     endif
                                  enddo
                                  nexp = (expstop-expstart+1)
                                  !write(*,*)myid,'id',expstart,expstop,nexp
                                  if( expstop .ne. 0  .and. expstart.ne.0 ) then
                                     Gtemp(1:nexp) = -6.0d0 / delta**2 * (deltaz(k)**2 + deltay(j)**2 + deltax(expstart:expstop)**2)
                                     call vrda_exp( nexp, Gtemp, G(expstart:expstop) )
                                  endif
                                  
!                                  ! End vec exponential calc option ---------------
#else

                                 ! Std exponential calc option ---------------
                                  WHERE (deltax .le. filinflu )
                                     G = exp ( -6.0d0 / delta**2 * (deltaz(k)**2 + deltay(j)**2 + deltax**2 ) )
                                  ELSEWHERE 
                                     G = 0
                                  END WHERE
                                  ! End std exponential calc option ---------------
#endif

                                  G = G*dv(:,j,k)*G1*weight(:,j,k)*cond(:,j,k)
                                  
                                  contrib(ifil,jfil, id+1) = contrib(ifil,jfil, id+1) + SUM(G * field(:,j,k) )
                                  contrib_norm_fac(ifil,jfil, id+1) = contrib_norm_fac(ifil,jfil, id+1) + SUM(G) 
                               ENDIF
                            ENDDO
                         ENDIF
                      ENDDO
                   END IF
                   
                ENDIF ! end if ( computercontrib(ifil) )

             ENDDO ! Loop over ifil
          ENDDO ! Loop over jfil
       ENDDO ! Loop over id

       ! Wait for all contributions to be available before we start reduction
       call mpi_barrier(gcomm,ierr)

       DO id=0,npx*npy*npz-1
          CALL MPI_Reduce( contrib(:,:,id+1), filfield(:,:,kfil),&
               nxf*nyf, MPI_DOUBLE_PRECISION, MPI_SUM, id, gcomm, ierr)

          CALL MPI_Reduce( contrib_norm_fac(:,:,id+1), filfield_norm(:,:,kfil),&
               nxf*nyf, MPI_DOUBLE_PRECISION, MPI_SUM, id, gcomm, ierr)

       ENDDO


    ENDDO
    
    ! Should be synced up here anyway after global reduction but make sure
    call mpi_barrier(gcomm,ierr) 

!    write(*,*)myid, 'max filfield_norm=',MAXVAL(filfield_norm)
!    write(*,*)myid, 'min filfield_norm=',MINVAL(filfield_norm)


!    where(filfield_norm < 1e-32)
!       filfield_norm = 1
!    end where
!    filfield = filfield / filfield_norm
    numer = numer + filfield
    denom = denom + filfield_norm +1e-50

    filfield = numer/denom
    !write(*,*)myid, 'max numer/denom=',MAXVAL(filfield)
    !write(*,*)myid, 'min numer/denom=',MINVAL(filfield)


END SUBROUTINE generalized_filter


#ifdef HDF5
SUBROUTINE write_field(io, field,  filename, groupname, fieldname, &
                       fielddesc, fieldunits, overwrite)

  USE hdf5
  use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
  use topology_m
  use reference_m

  IMPLICIT NONE
  ! INPUT VARIABLES
  INTEGER io
  REAL, intent(in):: field(nxf,nyf,nzf)
  REAL :: buffer(nxf,nyf,nzf)
  CHARACTER(LEN=8) :: dsetname   ! Dataset name
  CHARACTER(LEN=20) :: fieldname  ! Field name
  CHARACTER(LEN=20) :: groupname  ! Group name
  CHARACTER(LEN=50) :: filename   ! File name
  CHARACTER(LEN=250) :: fielddesc  ! Very long field name
  CHARACTER(LEN=20) :: fieldunits  ! Field units
  
  LOGICAL :: overwrite
  INTEGER :: error
  
  
  ! HDF variables
  INTEGER(HID_T) :: file_id       ! File identifier 
  INTEGER(HID_T) :: dset_id,x_id,y_id,z_id       ! Dataset identifier 
  INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
  INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
  INTEGER(HSIZE_T), DIMENSION(3) :: dimsf, blockdim, offset  ! Dataset dimensions: global, local, local offset
  INTEGER(HSIZE_T), DIMENSION(3) :: dimschk
  INTEGER OS(3)
  INTEGER(HSIZE_T), DIMENSION(3) :: stride, count
  INTEGER, DIMENSION(NPX) :: COUNTX
  INTEGER, DIMENSION(NPY) :: COUNTY
  INTEGER, DIMENSION(NPZ) :: COUNTZ
  INTEGER, DIMENSION(npx*npy*npz) :: COUNTXG, COUNTYG, COUNTZG
  INTEGER pcounter, countpx, countpy, countpz
  INTEGER :: rank = 3 ! Dataset rank 
  real, dimension(nxf,nyf,nzf) :: grid_store
  integer req(MPI_STATUS_SIZE)
  ! counters
  INTEGER i,j,k
  !----------------------------------------------------------------------

  dimsf(1) = nxf*npx
  dimsf(2) = nyf*npy
  dimsf(3) = nzf*npz



    ! Dimensions of output block on this process
    blockdim(1) = nxf
    blockdim(2) = nyf
    blockdim(3) = nzf

    
    call MPI_Allgather( blockdim(1), 1, MPI_INTEGER, countxg, 1, MPI_INTEGER, gcomm, ierr)
    call MPI_Allgather( blockdim(2), 1, MPI_INTEGER, countyg, 1, MPI_INTEGER, gcomm, ierr)
    call MPI_Allgather( blockdim(3), 1, MPI_INTEGER, countzg, 1, MPI_INTEGER, gcomm, ierr)
    

    offset = 0
    dimschk = 0
    ! setup countx/y/z in grid topology coordinates.
    DO pcounter = 0,npx*npy*npz-1
       countpz = pcounter/(npx*npy)
       countpx = MOD(pcounter-(countpz*npx*npy), npx)
       countpy = (pcounter-(countpz*npx*npy))/npx
              
       if( countpx == mypx ) then
          if( countpy == mypy ) then
             countz(countpz+1) = countzg(pcounter+1)
          endif
          if( countpz == mypz ) then
             county(countpy+1) = countyg(pcounter+1)
          endif
       endif
       if(countpy == mypy .and. countpz == mypz) then
          countx(countpx+1) =countxg(pcounter+1)
       endif
    ENDDO
!    write(*,*) 'myid,countxyz=',myid,countx,county,countz
    ! Count up grid points on other processes, check total number
    DO pcounter = 1, npx
       IF(pcounter <= mypx ) then
          offset(1) = offset(1) + countx(pcounter)
       ENDIF
       dimschk(1) = dimschk(1) + countx(pcounter)
    ENDDO
    
    DO pcounter = 1, npy
       IF(pcounter <= mypy ) then
          offset(2) = offset(2) + county(pcounter)
       ENDIF
       dimschk(2) = dimschk(2) + county(pcounter)
    ENDDO
    
    DO pcounter = 1, npz
       IF(pcounter <= mypz ) then
          offset(3) = offset(3) + countz(pcounter)
       ENDIF
       dimschk(3) = dimschk(3) + countz(pcounter)
    ENDDO
    
!    write(*,*) 'myid,offset=',myid,offset
    ! Get global file dimensions (use count as scracth space)
    CALL MPI_Allreduce(dimschk(1), count(1), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
    CALL MPI_Allreduce(dimschk(2), count(2), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
    CALL MPI_Allreduce(dimschk(3), count(3), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
    
    if(myid==0) then
       do i = 1,3
       if (dimsf(i).ne.count(i)) then
          write(*,*) 'error, count mismatch: dimsf/dimschk=',dimsf, count
       endif
       enddo
    endif
    
  ! End compute offset 


  

    
  ! Do output on root only
    
    if(myid==0) then

       CALL h5open_f(error) 
       if( overwrite ) then
          CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
          write(io,*) 'Created filename:',filename
          CALL h5screate_simple_f(rank, dimsf, filespace, error)

          CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
               dset_id, error)
          fieldname = 'x'    
          CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
               x_id, error)
          fieldname = 'y'    
          CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
               y_id, error)
          fieldname = 'z'    
          CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
               z_id, error)
       else

          CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error )
          IF(error == -1 ) THEN
             IF(myid.eq.0) write(io,*)'Could not open ',filename,'trying to create'
             CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
             IF( error == -1 ) THEN
                IF(myid==0) write(io,*)'Could not create ', filename
             ENDIF
          ENDIF
          CALL h5screate_simple_f(rank, dimsf, filespace, error)
          CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
               dset_id, error)
       endif

       CALL h5screate_simple_f(rank, blockdim, memspace, error)    ! create memory space
       stride = 1
       count = 1
       CALL h5dget_space_f(dset_id, filespace, error)

       ! First dump what's here
       CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, blockdim, error, &
            stride, count)          

       !       write(*,*)'offset=',offset
       !       write(*,*)'blockdim=',blockdim
       !       write(*,*)'stride=',stride
       !       write(*,*)'count=',count
       !       write(*,*)'field(1,1,1)=',field(1,1,1)

       CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, field, dimsf, error, &
            file_space_id = filespace, mem_space_id = memspace)


       if( overwrite ) then
          DO k=1,nzf
             DO j=1,nyf
                grid_store(:,j,k) = fx(:,myid+1)*l_ref*1.0e3
             ENDDO
          ENDDO
          CALL h5dwrite_f(x_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
               file_space_id = filespace, mem_space_id = memspace)

          DO k=1,nzf
             do j=1,nyf
                DO i=1,nxf
                   grid_store(i,j,k) = fy(j,myid+1)*l_ref*1.0e3
                enddo
             ENDDO
          ENDDO
          CALL h5dwrite_f(y_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
               file_space_id = filespace, mem_space_id = memspace)

          do k = 1,nzf
             DO j=1,nyf
                DO i=1,nxf
                   grid_store(i,j,k) = fz(k,myid+1)*l_ref*1.0e3
                enddo
             ENDDO
          ENDDO
          CALL h5dwrite_f(z_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
               file_space_id = filespace, mem_space_id = memspace)
       endif

       !       write(*,*)'bd, nx/y/z f',blockdim, nxf, nyf, nzf
       DO pcounter=1,npx*npy*npz-1
          CALL MPI_Recv(os,rank, MPI_INTEGER, pcounter, 15, gcomm, req, ierr)
          offset=os
          !          write(*,*) 'root recieved offset from',pcounter,offset

          CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
               , MPI_DOUBLE_PRECISION, pcounter, 16, gcomm, req, ierr)

          CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, blockdim, error, &
               stride, count)          

          CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
               file_space_id = filespace, mem_space_id = memspace)

          if( overwrite ) then
             CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
                  , MPI_DOUBLE_PRECISION, pcounter, 26, gcomm, req, ierr)
             CALL h5dwrite_f(x_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
                  file_space_id = filespace, mem_space_id = memspace)
             !
             CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
                  , MPI_DOUBLE_PRECISION, pcounter, 36, gcomm, req, ierr)
             CALL h5dwrite_f(y_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
                  file_space_id = filespace, mem_space_id = memspace)
             !
             CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
                  , MPI_DOUBLE_PRECISION, pcounter, 46, gcomm, req, ierr)
             CALL h5dwrite_f(z_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
                  file_space_id = filespace, mem_space_id = memspace)
          endif
          !
       ENDDO


       CALL h5dclose_f(dset_id, error)
       CALL h5sclose_f(filespace, error)
       CALL h5sclose_f(memspace, error)
       CALL h5fclose_f(file_id, error)
       CALL h5close_f(error)

    ELSE
       os = offset
       CALL MPI_Send( os, rank, MPI_INTEGER, 0, 15, gcomm,req,  ierr)

       CALL MPI_Send( field, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
            , 0, 16, gcomm, req,  ierr)

       if( overwrite ) then
          DO k=1,nzf
             DO j=1,nyf
                grid_store(:,j,k) = fx(:,myid+1)*l_ref*1.0e3
             ENDDO
          ENDDO
          CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
               , 0, 26, gcomm,req,  ierr)

          DO k=1,nzf
             do j=1,nyf
                DO i=1,nxf
                   grid_store(i,j,k) = fy(j,myid+1)*l_ref*1.0e3
                ENDDO
             enddo
          ENDDO

          CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
               , 0, 36, gcomm,  req, ierr)
          do k=1,nzf
             DO j=1,nyf
                DO i=1,nxf
                   grid_store(i,j,k) = fz(k,myid+1)*l_ref*1.0e3
                ENDDO
             ENDDO
          enddo
          CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
               , 0, 46, gcomm,  req, ierr)
          !


       endif
    endif
END SUBROUTINE WRITE_FIELD
#endif
SUBROUTINE write_basics(io, basics, xpl, zpl, n1, n2 , writemin, writemax)

  use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
  use topology_m
  use reference_m

  IMPLICIT NONE
  integer io,n1,n2
!  real, allocatable, dimension(:,:,:) :: basics, basics_g
  real, dimension(n1,n2,ny) :: basics
  real, dimension(n1,n2,ny*npy) :: basics_g
  real xpl,zpl, writemin, writemax

  integer ic,id,iy

call MPI_Gather(basics,ny*n1*n2,MPI_REAL8,basics_g,ny*n1*n2,MPI_REAL8,0,ycomm,ierr)

if(yid.eq.0 .and. &
   real(xid*nx).le.xpl*real(nx*npx).and.real((xid+1)*nx).gt.xpl*real(nx*npx)  .and.  &
   real(zid*nz).le.zpl*real(nz*npz).and.real((zid+1)*nz).gt.zpl*real(nz*npz))then

do ic=1,2!3
do id=1,2!3
 write(io,9) (basics_g(ic,id,iy), iy=int(ny*npy*writemin),int(ny*npy*writemax))
enddo
enddo

endif

  9 format(10(1pe12.5,1x))
RETURN
END SUBROUTINE WRITE_BASICS

!-------------------------------------------------------------------

SUBROUTINE write_onedim(io, onedim, xpl, zpl , writemin, writemax)

  use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
  use topology_m
  use reference_m
      
  IMPLICIT NONE
  integer io,n1,n2
  real, dimension(ny) :: onedim
  real, dimension(ny*npy) :: onedim_g
  real xpl,zpl, writemin, writemax

  integer ic,id,iy
  
call MPI_Gather(onedim,ny,MPI_REAL8,onedim_g,ny,MPI_REAL8,0,ycomm,ierr)

if(yid.eq.0 .and. &
   real(xid*nx).le.xpl*real(nx*npx).and.real((xid+1)*nx).gt.xpl*real(nx*npx)  .and.  &
   real(zid*nz).le.zpl*real(nz*npz).and.real((zid+1)*nz).gt.zpl*real(nz*npz))then
   write(78,9) (onedim_g(iy), iy=int(ny*npy*writemin),int(ny*npy*writemax))
endif
           
  9 format(10(1pe12.5,1x))
RETURN
END SUBROUTINE WRITE_ONEDIM

!-------------------------------------------------------------------

subroutine write_filtfields(io,uf,yf,wf)
use topology_m, only : myid, gcomm, ierr
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
use reference_m, only : time_ref

implicit none
integer,intent(in) :: io

character*5 :: myid_ext*5, filename*100, dirname*100

real uf(nx*ny*nz*3),yf(nx*ny*nx*n_spec),wf(nx*ny*nx*n_spec)

call MPI_Barrier( gcomm, ierr )

write(myid_ext, '(I5.5)') myid
dirname = '../post/filtfields/'

!----------------------------------------
filename=trim(dirname)//'field.'//myid_ext
open(unit=201,file=trim(filename),status='unknown',form='unformatted')
  call readwrite_filtfile_data(io,201,'w',uf,yf,wf)
close(201)

!----------------------------------------
call MPI_Barrier( gcomm, ierr )
return
end subroutine write_filtfields


!***********************************************
!***********************************************

subroutine read_filtfields(io,uf,yf,wf)
use topology_m, only : myid, gcomm, ierr
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
use reference_m, only : time_ref
  
implicit none
integer,intent(in) :: io

character*5 :: myid_ext*5, filename*100, dirname*100

real uf(nx*ny*nz*3),yf(nx*ny*nx*n_spec),wf(nx*ny*nx*n_spec)

call MPI_Barrier( gcomm, ierr )

write(myid_ext, '(I5.5)') myid
dirname = '../post/filtfields/'

!----------------------------------------
filename=trim(dirname)//'field.'//myid_ext
open(unit=201,file=trim(filename),status='unknown',form='unformatted')
  call readwrite_filtfile_data(io,201,'r',uf,yf,wf)
close(201)

!----------------------------------------
call MPI_Barrier( gcomm, ierr )
return
end subroutine read_filtfields


! reads and writes data for save file in double precision
subroutine readwrite_filtfile_data(io,io_savefile,input,uf,yf,wf)
use topology_m, only : myid
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
!use runtime_m, only : time, tstep, time_save
!use variables_m, only : temp, pressure, yspecies, u
!use bc_m, only : pout

implicit none
integer io, io_savefile
character*1 input

real uf(nx*ny*nz*3),yf(nx*ny*nz*n_spec),wf(nx*ny*nz*n_spec)


if(input .ne. 'r' .and. input .ne. 'w') then
  if(myid.eq.0) then
    write(io,*) 'improper setting for variable input'
    write(io,*) 'in routine readwrite_savefile_data'
  endif
  call terminate_run(io,0)  !must be called by all processors
  return
end if

if(input.eq.'w') then
  write(io_savefile) uf
  write(io_savefile) yf
  write(io_savefile) wf
elseif(input.eq.'r') then
  read(io_savefile) uf
  read(io_savefile) yf
  read(io_savefile) wf
endif

return
end subroutine readwrite_filtfile_data

end module par_filter_m

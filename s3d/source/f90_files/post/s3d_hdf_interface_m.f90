#include "globalDefines.h"
module s3d_hdf_interface_m
  ! ==================================================================================
  ! Module for writing hdf5 files from S3D for postprocessing
  ! Author - Ray Grout (2008)
  ! Control options by setting values in write_hdf5_param
  ! Usage:
  ! Driver routine needs to " use s3d_hdf_interface "
  ! Set fields to output by setting write_hdf5_parm%output_xxxx = .true 
  ! Current options for xxxx are: uvw, rho, temp, spc, rates, dilitation, vorticity
  ! Control number of points output by setting 
  !         write_hdf5_parm%output_iskip = number of points to skip on output
  ! After setting options, call:
  ! write_hdf5(io, ierror) 
  ! ierror < 0 on exit if error encountered
  ! To add extra fields, after call to write_hdf5(io, ierror), call:
  !
  ! call write_hdf_field( io, write_hdf5_param%filename, write_hdf5_param%extragroup, \
  !  fieldname, fielddesc, fieldunits, field, scaling, error, .false.)
  !
  ! where:
  ! CHARACTER(LEN=20) :: fieldname  ! Field name
  ! CHARACTER(LEN=250) :: fielddesc  ! Very long field name
  ! CHARACTER(LEN=20) :: fieldunits  ! Field units
  ! INTEGER error ! Error < 0 if error detected
  ! scaling ! Scale factor

  ! ==================================================================================

  ! TODO LIST:
  ! -> ADD PADDING TO MAKE SURE OUTPUTFLAG IS RECTANGULAR IF KEEP_3D_STRUCTURE IS SET
  ! -> ADD FLAG FOR ZLIB COMPRESSION IF DATA CAN USE CHUNKED STORAGE
  ! 8. DOUBLE CHECK SCALING FACTORS
  ! 9. CHECK OPERATIONAL TEST CASES:
  !   A. OUTPUT ARBITRARY BLOCK / PLANE / PENCIL IN 3D/1D STRUCTURE
  !   B. OUTPUT ARBITRARY SUBDOMAIN (SURFACE) WITH 3D STRUCTURE + PADDING
  !   C. OUTPUT ARBITRARY SUBDOMAIN (SURFACE) AS 1D STRUCTURE
  ! 10. SORT OUT INTERFACE FOR ADDING ARBITRARY EXTRA FIELDS

  TYPE :: hdf_output_control
     ! These are for external control
     logical :: output_uvw = .false.
     logical :: output_rho = .false.
     logical :: output_temp = .false.
     logical :: output_spc = .false.
     logical :: output_rates = .false.
     logical :: output_dilitation = .false.
     logical :: output_hrr = .false.
     logical :: output_vorticity = .false.
     logical :: output_mixfrac = .false.
     logical :: output_chi = .false.
     logical :: output_chi_sep = .false.
     logical :: output_pressure = .false.
     logical :: keep_3d_structure = .true.
     integer :: iskip = 4  
     integer :: precision = 4
     logical :: output_flag_manual = .false.
     logical :: output_rect_subset = .false.

     integer :: xmin, xmax, ymin, ymax, zmin, zmax

     CHARACTER(LEN=50) :: filename   ! File name
     CHARACTER(LEN=20) :: extragroup = 'Additional'  ! Group name
  END type hdf_output_control

  type(hdf_output_control) :: write_hdf5_param
  INTEGER, ALLOCATABLE, DIMENSION(:, :, :) :: outputflagmanual
  LOGICAL outputflagmanual_allocated 
contains

  !----------------------------------------------------------------------
  subroutine write_hdf5(io,ierror)
    !----------------------------------------------------------------------
    ! Write out an hdf5 file using parallel hdf5
    ! Ray Grout ( 21 March 08)
    ! Format of function calls modelled on Ramanan's / Evatt's write_tecplot
    !----------------------------------------------------------------------
    ! S3D Modules
    use topology_m
    use param_m, only : nx, ny, nz, n_spec
    use reference_m
    use grid_m, only : x, y, z, scale_1x, scale_1y, scale_1z, xg, yg, zg
    use variables_m, only : q, u, temp, pressure, yspecies
    use chemkin_m, only : species_name, n_species, reaction_rate
    use runtime_m, only : run_title, time, i_time, tstep
    use work_m, only : vort_mag => work1_1
    use work_m, only : hr => work1_2
    use mixfrac_m
    
    implicit none
    !----------------------------------------------------------------------
    ! DECLARATIONS PASSED IN

    integer io       ! Output unit
    integer ierror   ! Error on exit

    ! LOCAL DECLARATIONS

    integer i,j,k,L
    real p_conv_atm

    real, dimension(nx,ny,nz,3) :: vort
    real, dimension(nx,ny,nz) :: dil

    real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion

    real, dimension(nx,ny,nz) :: grid_store, chi, scratch
    character*9 time_ext
    character*100 filename_short

    !----------------------------------------------------------------------
    ! hdf declarations

    CHARACTER(LEN=8) :: dsetname   ! Dataset name
    CHARACTER(LEN=20) :: groupname  ! Group name
    CHARACTER(LEN=20) :: fieldname  ! Field name
    CHARACTER(LEN=250) :: fielddesc  ! Very long field name
    CHARACTER(LEN=20) :: fieldunits  ! Field units
    CHARACTER(LEN=50) :: filename   ! File name

    LOGICAL :: overwrite
    INTEGER :: error

    !----------------------------------------------------------------------


    ! Return if zero dimensions
    p_conv_atm=p_ref/pres_atm
    IF((nx==1).AND.(ny==1).AND.(nz==1)) THEN
       RETURN
    ELSE
       IF(myid.eq.0) then
          write(io,*) 'Writing hdf files for:'
          write(io,'(a10,i7)') ' i_time = ',i_time
          write(io,'(a8,1pe9.3,a6)') ' time = ',time*time_ref,' (sec)'
          write(io,*) 'write_hdf5_param%iskip = ',write_hdf5_param%iskip
       ENDIF
    ENDIF



    ! Specify filename on root, broadcast
    IF (myid==0) then  
       !   set time stamp 
       write(time_ext,'(1pe9.3)') time*time_ref 
       filename_short=trim(run_title)//'.'//trim(time_ext)//'.h5'
       filename='../post/hdf5/'//trim(filename_short)
       filename=trim(filename)

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
    write_hdf5_param%filename = filename 


    ! Call write_hdf_field for fields to output. Always write out grid

    groupname = "/primary/grid"
    fieldname = "x"
    fielddesc = "coordinate"
    fieldunits = "mm"
    overwrite = .true.
    DO k=1,nz
       DO j=1,ny
          grid_store(:,j,k) = x(:)
       ENDDO
    ENDDO
    CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, grid_store, l_ref*1e3, error,overwrite)
    IF(error < 0 ) then
       write(io,*) 'id',myid, 'raised an error during writing '
       GOTO 9999
    ENDIF



    overwrite = .false.
    fieldname = "y"
    DO k=1,nz
       DO i=1,nx
          grid_store(i,:,k) = y(:)
       ENDDO
    ENDDO
    CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, grid_store, l_ref*1e3, error,overwrite)
    IF(error < 0 ) GOTO 9999

    fieldname = "z"
    DO j=1,ny
       DO i=1,nx
          grid_store(i,j,:) = z(:)
       ENDDO
    ENDDO
    CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, grid_store, l_ref*1e3, error,overwrite)
    IF(error < 0 ) GOTO 9999

    ! Write out these quantities only if specified in write_hdf5_param

    IF(write_hdf5_param%output_uvw) then
       groupname = "/primary/flow"
       fieldname = "u"
       fielddesc = "Velocity component"
       fieldunits = "m/s"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, u(:,:,:,1), a_ref, error,overwrite)
       IF(error < 0 ) GOTO 9999

       fieldname = "v"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, u(:,:,:,2), a_ref, error,overwrite)
       IF(error < 0 ) GOTO 9999

       fieldname = "w"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, u(:,:,:,3),a_ref, error, overwrite)
       IF(error < 0 ) GOTO 9999
    ENDif

    IF(write_hdf5_param%output_rho) then
       groupname = "/primary/flow"
       fieldname = "rho"
       fielddesc = "Density"
       fieldunits = "(g/cm^3)"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, q(:,:,:,4,1), rho_ref, error,overwrite)
       IF(error < 0 ) GOTO 9999
    ENDIF

    IF(write_hdf5_param%output_temp) then
       groupname = "/primary/flow"
       fieldname = "Temp"
       fielddesc = "Temperature"
       fieldunits = "K"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, temp,t_ref, error,overwrite)
       IF(error < 0 ) GOTO 9999
    ENDIF

    IF(write_hdf5_param%output_pressure) then
       groupname = "/primary/flow"
       fieldname = "P"
       fielddesc = "Pressure"
       fieldunits = "atm"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, pressure,p_conv_atm, error,overwrite)
       IF(error < 0 ) GOTO 9999
    ENDIF

    IF(write_hdf5_param%output_spc) then
       groupname = "/primary/composition"
       fielddesc = "Species mass fraction"
       fieldunits = "kg/kg_mix"
       DO L=1,n_species
          write(fieldname,*)'"Y '//trim(species_name(L))//'"'
          CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, yspecies(:,:,:,L), 1.0, error,overwrite)
          IF(error < 0 ) GOTO 9999
       ENDDO
    ENDIF


    IF(write_hdf5_param%output_mixfrac) then
       groupname = "/derived"
       fielddesc = "Mixture fraction"
       fieldunits = "none"
       fieldname = 'mixfrac'
       call specToMixfr(yspecies)
       if(myid==0)write (*,*)'Calculated mixture fraction'
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, mixFrac, 1.0, error,overwrite)
       if(myid==0)write (*,*)'Wrote mixture fraction'
          IF(error < 0 ) GOTO 9999

    ENDIF

    IF(write_hdf5_param%output_chi) then
       groupname = "/derived"
       fielddesc = "Scalar dissipation"
       fieldunits = ""
       fieldname = 'chi'
       if(myid==0)write (*,*)'Calculating chi'
       CALL computeScalarDissipationRate(io, chi)
       if(myid==0)write (*,*)'Calculated chi'
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, chi, 1/time_ref, error,overwrite)
          IF(error < 0 ) GOTO 9999

    ENDIF

    IF(write_hdf5_param%output_chi_sep) then
       groupname = "/derived"
       fielddesc = "Mixture fraction gradient (x)"
       fieldunits = ""

       fieldname = 'gradz_x'
       if(myid==0)write (*,*)'Calculating grad mf x'
       call derivative_x(nx,ny,nz,mixFrac,scratch,scale_1x,1)
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, scratch, 1.0, error,overwrite)
       IF(error < 0 ) GOTO 9999

       fieldname = 'gradz_y'
       if(myid==0)write (*,*)'Calculating grad mf y'
       call derivative_y(nx,ny,nz,mixFrac,scratch,scale_1y,1)
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, scratch, 1.0, error,overwrite)
       IF(error < 0 ) GOTO 9999

       fieldname = 'gradz_z'
       if(myid==0)write (*,*)'Calculating grad mf z'
       call derivative_z(nx,ny,nz,mixFrac,scratch,scale_1z,1)
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, scratch, 1.0, error,overwrite)
       IF(error < 0 ) GOTO 9999

    ENDIF


    IF(write_hdf5_param%output_rates) then
       groupname = "/derived/rates"
       fielddesc = "Species reaction rate"
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
          CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, rr_r(:,:,:,L), 1.0,  error,overwrite)
          IF(error < 0 ) GOTO 9999
       ENDDO
       call calc_heat_release(rr_r,hr)
       fielddesc = "Heat release rate"
       fieldunits = "J/m^3/s"
       fieldname = "hrr"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, hr, -hr_ref,  error,overwrite)
       IF(error < 0 ) GOTO 9999
    ENDIF

    IF(write_hdf5_param%output_dilitation) then
       call computeDivergence(u,dil)
       fieldunits = "1/s"
       groupname = "/derived"
       fielddesc = "Dilitation"
       fieldname = "Dilitation"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, dil, 1.0/time_ref,  error,overwrite)
       IF(error < 0 ) GOTO 9999
    ENDIF

    IF(write_hdf5_param%output_vorticity) then
       fieldunits = "1/s"
       call calc_vorticity(vort,vort_mag,u)
       fieldunits = "1/s"
       groupname = "/derived"
       fielddesc = "Vorticity, x component"
       fieldname = "vort_x"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, vort(:,:,:,1), 1.0/time_ref,  error,overwrite)
       IF(error < 0 ) GOTO 9999
       fielddesc = "Vorticity, y component"
       fieldname = "vort_y"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, vort(:,:,:,2), 1.0/time_ref,  error,overwrite)
       IF(error < 0 ) GOTO 9999
       fielddesc = "Vorticity, z component"
       fieldname = "vort_z"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, vort(:,:,:,3), 1.0/time_ref,  error,overwrite)
       IF(error < 0 ) GOTO 9999
       fielddesc = "Vorticity magnitude"
       fieldname = "vort_mag"
       CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, vort_mag, 1.0/time_ref,  error,overwrite)
       IF(error < 0 ) GOTO 9999
    ENDIF

    IF(write_hdf5_param%output_hrr) then
      IF(.not. write_hdf5_param%output_rates) then
        groupname = "/derived/rates"

#ifdef GETRATES_NEEDS_DIFFUSION
        diffusion = 0.0
        tstep = 1.0
        call reaction_rate(rr_r,temp,pressure,yspecies,&
        diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
        call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
        call calc_heat_release(rr_r,hr)
      ENDIF

        fielddesc = "Heat release rate"
        fieldunits = "J/m^3/s"
        fieldname = "hrr"
        CALL write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits, hr, -hr_ref,  error,overwrite)
        IF(error < 0 ) GOTO 9999
    ENDIF
    ierror = 0
    return

9999 CONTINUE
    ierror = error
    return

9   format(10(1pe12.5,1x))
    !!----------------------------------------------------------------------
    return

  END SUBROUTINE write_hdf5

  subroutine write_hdf_field( io, filename, groupname, fieldname, fielddesc, fieldunits,  unscaledfield, scaling, error, overwrite)

    ! S3D Modules
    use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec, npx, npy, npz
    use topology_m
    ! Parallel HDF5
    USE hdf5 ! This module contains all necessary modules for hdf5

    implicit none    

    ! Input arguments
    CHARACTER(LEN=50), INTENT(IN) :: filename   ! File name
    CHARACTER(LEN=20), INTENT(IN) :: groupname  ! Group name
    CHARACTER(LEN=20), INTENT(IN) :: fieldname  ! Field name
    CHARACTER(LEN=250), INTENT(IN) :: fielddesc  ! Very long field name
    CHARACTER(LEN=20), INTENT(IN) :: fieldunits  ! Field units
    REAL, DIMENSION(NX,NY,NZ), INTENT(IN) :: unscaledfield ! Field data
    REAL, INTENT(IN) :: scaling
    INTEGER, INTENT(IN) :: io
    LOGICAL, INTENT(IN) :: overwrite
    INTEGER, INTENT(OUT) :: error 
    INTEGER, DIMENSION(nx, ny,nz) :: outputflag 

    ! Local variables
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: field ! Field data

    ! HDF variables
    CHARACTER(LEN=8) :: dsetname   ! Dataset name
    INTEGER(HID_T) :: file_id       ! File identifier 
    INTEGER(HID_T) :: primary_group, derived_group, group_id       ! group identifiers
    INTEGER(HID_T) :: dset_id       ! Dataset identifier 
    INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
    INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
    INTEGER(HID_T) :: plist_id      ! Property list identifier 
    INTEGER(HSIZE_T), DIMENSION(3) :: dimsf  ! Dataset dimensions
    ! in the file.
    INTEGER(HSIZE_T), DIMENSION (3) :: dimsfi 
    INTEGER(HSIZE_T), DIMENSION(3) :: chunk_dims  ! Chunks dimensions
    INTEGER(HSIZE_T),  DIMENSION(3) :: count  
    INTEGER(HSSIZE_T), DIMENSION(3) :: offset, offset_mem
    INTEGER(HSIZE_T),  DIMENSION(3) :: stride
    INTEGER(HSIZE_T),  DIMENSION(3) :: blockdim
    INTEGER :: rank = 3 ! Dataset rank 

    ! Counters
    INTEGER pcounter, ifull,jfull,kfull, countpx, countpy, countpz
    INTEGER ifield, jfield, kfield
    INTEGER iglobal, jglobal, kglobal

    ! For data exchange of number of data points per process
    INTEGER, DIMENSION(NPX) :: COUNTX
    INTEGER, DIMENSION(NPY) :: COUNTY
    INTEGER, DIMENSION(NPZ) :: COUNTZ
    INTEGER, DIMENSION(npx*npy*npz) :: COUNTXG, COUNTYG, COUNTZG

    !    write(io,*)'Writing field:',fieldname,'to file:',filename,'using write_hdf5_param%iskip=',write_hdf5_param%iskip

    ! Setup outputflag
    IF( write_hdf5_param%output_flag_manual ) THEN
       ! Check to make sure outpuflagmanual is allocated, then copy it
       IF( outputflagmanual_allocated == .TRUE. ) THEN
          outputflag = outputflagmanual
       ELSE 
          IF (myid == 0 ) THEN
             WRITE(io,*) 'outputflagmanual not allocated'
             WRITE(io,*) 'call set_outputflagmanual(outputflagmanual,1)'
          ENDIF
          GOTO 9999
       ENDIF
    ELSE IF( write_hdf5_param%output_rect_subset ) THEN
       ! Setup outputflag based on limits in global coordinate
       outputflag = 0
       DO kfull = 1,nz
          DO jfull = 1,ny
             DO ifull = 1,nx
                iglobal = ifull + mypx*nx
                jglobal = jfull + mypy*ny
                kglobal = kfull + mypz*nz

                if( ( iglobal >= write_hdf5_param%xmin ) .AND. ( iglobal <= write_hdf5_param%xmax ) ) THEN
                   if( ( jglobal >= write_hdf5_param%ymin ) .AND. ( jglobal <= write_hdf5_param%ymax ) ) THEN
                      if( ( kglobal >= write_hdf5_param%zmin ) .AND. ( kglobal <= write_hdf5_param%zmax ) ) THEN
                         outputflag(ifull,jfull,kfull) = 1
                      ENDIF
                   ENDIF
                ENDIF
             ENDDO
          ENDDO
       ENDDO

    ELSE
       ! Output entire field
       outputflag = 1
    END IF

    IF( write_hdf5_param%keep_3d_structure ) then
       ! TODO: Need to add code here to pad outputflag so that block to output is rectangular
       ! This only needs to be done if outputflagmanual = .TRUE.
       ! Haven't been able to think of an efficient way to do this yet
       IF ( write_hdf5_param%output_flag_manual ) THEN
          ! Look through outputflag to find extrema of selected region
          WRITE(io,*) 'Not sure yet how to keep_3d_structure and output_flag_manual'
          WRITE(io,*) 'If region is rectangular, try using output_rect_subset'
          error = -5
       ENDIF
       ! Setup flag for which points to output on this node - adjust for skiping
       FORALL  (ifull=1:nx,jfull=1:ny,kfull=1:nz, mod(ifull,write_hdf5_param%iskip).ne.0 .or. mod(jfull,write_hdf5_param%iskip).ne.0 .or. mod(kfull,write_hdf5_param%iskip).ne.0 )
          outputflag(ifull,jfull,kfull) = 0
       END FORALL

       ! Each process calculates how many points it's going to write in the 
       ! x/y/z directions, tells other processes along that direction, then
       ! uses the information from the other processes to find it's offset
       ! This might be dangerous if outputflag array isn't marked out in a rectangular form
       blockdim(1) = maxval(SUM(outputflag, dim=1))
       blockdim(2) = maxval(SUM(outputflag, dim=2))
       blockdim(3) = maxval(SUM(outputflag, dim=3) )


       call MPI_Allgather( blockdim(1), 1, MPI_INTEGER, countxg, 1, MPI_INTEGER, gcomm, ierr)
       call MPI_Allgather( blockdim(2), 1, MPI_INTEGER, countyg, 1, MPI_INTEGER, gcomm, ierr)
       call MPI_Allgather( blockdim(3), 1, MPI_INTEGER, countzg, 1, MPI_INTEGER, gcomm, ierr)


       offset = 0
       dimsf = 0

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

       DO pcounter = 1, npx
          IF(pcounter <= mypx ) then
             offset(1) = offset(1) + countx(pcounter)
          ENDIF
          dimsf(1) = dimsf(1) + countx(pcounter)
       ENDDO

       DO pcounter = 1, npy
          IF(pcounter <= mypy ) then
             offset(2) = offset(2) + county(pcounter)
          ENDIF
          dimsf(2) = dimsf(2) + county(pcounter)
       ENDDO

       DO pcounter = 1, npz
          IF(pcounter <= mypz ) then
             offset(3) = offset(3) + countz(pcounter)
          ENDIF
          dimsf(3) = dimsf(3) + countz(pcounter)
       ENDDO

       !            IF (myid==0) then
       !               write(io,*)' countx=',countx
       !               write(io,*)' county=',countx
       !               write(io,*)' countz=',countz
       !               write(io,*)' offset=',offset
       !               write(io,*)' dimsf=',dimsf
       !               write(io,*)' nx,ny,nz=',nx,ny,nz
       !            ENDIF



       ! Get global file dimensions
       CALL MPI_Allreduce(dimsf(1), dimsfi(1), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       CALL MPI_Allreduce(dimsf(2), dimsfi(2), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       CALL MPI_Allreduce(dimsf(3), dimsfi(3), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       dimsf = dimsfi
       !       write(io,*)'mypx,y,z=',mypx,mypy,mypz
       !       call mpi_finalize(error)
       !       stop
!       IF( blockdim(1) == 0 .or. blockdim(2)==0 .or. blockdim(3) ==0 ) then
!          write(io,*)'on myid=',myid,'blockdim=',blockdim, 'this is trouble!!!!'
!       ENDIF


       ! Copy field into buffer before output and apply scaling factor
       ALLOCATE( field(blockdim(1),blockdim(2),blockdim(3)) )

       ifield =1
       jfield =1
       kfield =1
       error = 0
       DO kfull=1,nz
          IF(MAXVAL(outputflag(:,:,kfull) )  > 0) THEN
             jfield =1
             DO jfull=1,ny
                IF(MAXVAL(outputflag(:,jfull,kfull)) >0 ) THEN
                   ifield = 1
                   DO ifull =1,nx
                      IF(outputflag(ifull,jfull,kfull) == 1 ) then
                         ! This test could be important if outputflag is oddly shaped, but it's expensive too
                         !                         IF(ifield>blockdim(1) .or. jfield>blockdim(2) .or. kfield>blockdim(3)) then
                         !                            write(io,*) ' Have overrun the END of output array on proc ',myid
                         !                            write(io,*) ' ifield,jfield,kfield=',ifield,jfield,kfield
                         !                            error = -1
                         !                            EXIT
                         !                         ENDIF
                         field(ifield,jfield,kfield) = unscaledfield(ifull,jfull,kfull)*scaling
                         ifield = ifield +1
                      ENDIF
                   ENDDO
                   jfield = jfield + 1
                ENDIF
             ENDDO
             kfield = kfield + 1
          ENDIF
       ENDDO
       CALL SYNC_ERROR(error)
       IF (error < 0) GOTO 9999

    ELSE     !   IF( keep_3d_structure ) 

       ! Setup flag for which points to output on this node - adjust for skiping
       FORALL  (ifull=1:nx,jfull=1:ny,kfull=1:nz, MOD(ifull,write_hdf5_param%iskip).ne.0 .or. MOD(jfull,write_hdf5_param%iskip).ne.0 .or. MOD(kfull,write_hdf5_param%iskip).ne.0 )
          outputflag(ifull,jfull,kfull) = 0
       END FORALL

       blockdim(1) = SUM(outputflag)
       blockdim(2) = 1
       blockdim(3) = 1
       call MPI_Allgather( blockdim(1), 1, MPI_INTEGER, countxg, 1, MPI_INTEGER, gcomm, ierr)

       offset = 0
       dimsf = 0
       DO pcounter = 1, npx*npy*npz
          IF(pcounter <= myid ) THEN
             offset(1) = offset(1) + countxg(pcounter)
          ENDIF
          dimsf(1) = dimsf(1) + countxg(pcounter)
       ENDDO
       dimsf(2) = 1
       dimsf(3) = 1
       dimsfi = dimsf

       ! Copy field to output buffer and apply scaling factor
       ALLOCATE( field(blockdim(1),1,1) )
       ifield =1
       DO kfull = 1,nz
          DO jfull = 1,ny
             DO ifull = 1,nx
                if( outputflag(ifull,jfull,kfull) == 1 ) then
                   IF(ifield>blockdim(1)) then
                      write(io,*) ' Have overrun the END of output array on proc ',myid
                      write(io,*) ' ifield=',ifield
                      error = -1
                      EXIT
                   ENDIF
                   field(ifield,1,1) = unscaledfield(ifull,jfull,kfull)*scaling
                   ifield = ifield + 1
                ENDIF
             ENDDO
          ENDDO
       ENDDO
       CALL SYNC_ERROR(error)
       IF (error < 0) GOTO 9999

    ENDIF



    !----------------------------------------------------------------------
    ! Initialize HDF5 library/fortran interface, setup file/dataset

    CALL h5open_f(error) 
    IF (error < 0) GOTO 9999

    ! 
    ! Setup file access property list with parallel I/O access.
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
    IF (error < 0) GOTO 9999

    CALL h5pset_fapl_mpio_f(plist_id, gcomm, MPI_INFO_NULL, error)
    IF (error < 0) GOTO 9999


    !
    ! Select group for this field. If it couldn't be opened, suspect it DOesn't exist and try to add it
    IF( overwrite) THEN
       CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)
       IF (error < 0) GOTO 9999

    ELSE
       CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error, access_prp = plist_id)
       IF(error == -1 ) THEN
          IF(myid.eq.0) write(io,*)'Could not open ',filename,'trying to create'
          CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)
          IF( error == -1 ) THEN
             IF(myid==0) write(io,*)'Could not create ', filename
          ENDIF
       ENDIF
       IF (error < 0) GOTO 9999
    ENDIF
    CALL h5pclose_f(plist_id, error)

    
    
    ! Create groups if new file
    CALL h5gopen_f(file_id, "/primary", group_id, error);
    IF(error < 0 ) THEN 
       CALL h5gcreate_f (file_id, "/primary", group_id, error)
       IF(error < 0) THEN 
          IF(myid==0) THEN
             write(io,*) 'Could not create group'
             write(io,*) 'Tried to create group: /primary'
             write(io,*) 'Failed with error: ',error
             write(io,*) 'It could be that this group already exists'
          ENDIF
          GOTO 9999
       ENDIF
    ENDIF

    CALL h5gclose_f(group_id,error)

    CALL h5gopen_f(file_id, "/primary/flow", group_id, error);
    IF(error < 0 ) THEN 
       CALL h5gcreate_f (file_id, "/primary/flow", group_id, error)
       IF(error < 0) THEN 
          IF(myid==0) THEN
             write(io,*) 'Could not create group'
             write(io,*) 'Tried to create group: /primary/flow'
             write(io,*) 'Failed with error: ',error
             write(io,*) 'It could be that this group already exists'
          ENDIF
          GOTO 9999
       ENDIF
    ENDIF
    CALL h5gclose_f(group_id,error)

    CALL h5gopen_f(file_id, "/primary/composition", group_id, error);
    IF(error < 0 ) THEN 
       CALL h5gcreate_f (file_id, "/primary/composition", group_id, error)
       IF(error < 0) THEN 
          IF(myid==0) THEN
             write(io,*) 'Could not create group'
             write(io,*) 'Tried to create group: /primary/composition'
             write(io,*) 'Failed with error: ',error
             write(io,*) 'It could be that this group already exists'
          ENDIF
          GOTO 9999
       ENDIF
    ENDIF
    CALL h5gclose_f(group_id,error)

    CALL h5gopen_f(file_id, "/derived", group_id, error);
    IF(error < 0 ) THEN 
       CALL h5gcreate_f (file_id, "/derived", group_id, error)
       IF(error < 0) THEN 
          IF(myid==0) THEN
             write(io,*) 'Could not create group'
             write(io,*) 'Tried to create group: /derived '
             write(io,*) 'Failed with error: ',error
             write(io,*) 'It could be that this group already exists'
          ENDIF
          GOTO 9999
       ENDIF
    ENDIF
    CALL h5gclose_f(group_id,error)

    CALL h5gopen_f(file_id, "/case", group_id, error);
    IF(error < 0 ) THEN 
       CALL h5gcreate_f (file_id, "/case", group_id, error)
       IF(error < 0) THEN 
          IF(myid==0) THEN
             write(io,*) 'Could not create group'
             write(io,*) 'Tried to create group: /case'
             write(io,*) 'Failed with error: ',error
             write(io,*) 'It could be that this group already exists'
          ENDIF
          GOTO 9999
       ENDIF
    ENDIF
    CALL h5gclose_f(group_id,error)


    CALL h5gopen_f(file_id, groupname, group_id, error);
    IF(error < 0 ) THEN 
       CALL h5gcreate_f (file_id, groupname, group_id, error)
       IF(error < 0) THEN 
          IF(myid==0) THEN
             write(io,*) 'Could neither open nor create group'
             write(io,*) 'Tried to create group: ', groupname
             write(io,*) 'Failed with error: ',error
          ENDIF
          GOTO 9999
       ENDIF
    ENDIF

    ! Create filespace
    CALL h5screate_simple_f(rank, dimsf, filespace, error)

    ! Check to see if block dimensions are the same on all processors, use offset_mem as temporary storage
    CALL MPI_Allreduce(blockdim(1), offset_mem(1), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
    CALL MPI_Allreduce(blockdim(1), chunk_dims(1), 1, MPI_INTEGER, MPI_MIN, gcomm, error)

    CALL MPI_Allreduce(blockdim(2), offset_mem(2), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
    CALL MPI_Allreduce(blockdim(2), chunk_dims(2), 1, MPI_INTEGER, MPI_MIN, gcomm, error)

    CALL MPI_Allreduce(blockdim(3), offset_mem(3), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
    CALL MPI_Allreduce(blockdim(3), chunk_dims(3), 1, MPI_INTEGER, MPI_MIN, gcomm, error)
    IF( offset_mem(1) == chunk_dims(1) .AND. offset_mem(2) == chunk_dims(2) .AND. offset_mem(3) == chunk_dims(3) ) then
       ! In this case make a chunked dataset
       chunk_dims = blockdim
       CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
       CALL h5pset_chunk_f(plist_id, rank, chunk_dims, error)
!       chunk_dims = -1

    ELSE
       CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
       chunk_dims = -1
    endif


    ! Select precision for filespace
    IF(write_hdf5_param%precision.eq.8) THEN
       CALL h5dcreate_f(group_id, fieldname, H5T_NATIVE_DOUBLE, filespace, &
            dset_id, error, plist_id)
       IF (error < 0 ) write(io,*) 'Error from h5dcreate_f'
       IF (error < 0 ) GOTO 9999
    ELSE IF(write_hdf5_param%precision.eq.4) THEN
       CALL h5dcreate_f(group_id, fieldname, H5T_NATIVE_REAL, filespace, &
            dset_id, error, plist_id)
       IF (error < 0 ) write(io,*) 'Error from h5dcreate_f'
       IF (error < 0 ) GOTO 9999
    ELSE
       error = -1 ! Set error code - unknown precision
       WRITE(io,*) 'Unknown precision = ',write_hdf5_param%precision
       WRITE(IO,*) 'Valid values are 8 and 4 '
       IF (error < 0 ) GOTO 9999
    END IF

    !    CALL h5sclose_f(filespace, error)

    ! Create memory space
    IF (MINVAL(blockdim) > 0 ) THEN
       CALL h5screate_simple_f(rank, blockdim, memspace, error)    !
    ELSE
       CALL h5screate_simple_f(rank, blockdim+1, memspace, error)    !
       CALL h5sselect_none_f(memspace,error)
    ENDIF
    IF (error < 0 ) write(io,*) 'Error creating memspace'

    ! Select hyperslab in the file.
    stride = 1
    count = 1
    
    !    IF(myid==0) then
    !       write(io,*)'stride=',stride
    !       write(io,*)'count=',count
    !       write(io,*)'blockdim=',blockdim
    !       write(io,*)'offset=',offset
    !       write(io,*)'dimsfi=',dimsfi
    !       write(io,*)'dimsf=',dimsf
    !    END IF

    CALL h5dget_space_f(dset_id, filespace, error)
    IF(error < 0) write(io,*) ' Error',error,'in h5dget_space_f, id',myid

    ! Do it like this for non-chunked data
    IF(chunk_dims(1) == -1 ) THEN
       IF (MINVAL(blockdim) > 0 ) THEN 
          CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, blockdim, error, &
               stride, count)          
       ELSE
          CALL h5sselect_none_f(filespace,error)
       ENDIF
       IF(error < 0) write(io,*) ' Error',error,'in h5sselect_hyperslab_f, id',myid
       IF (error < 0 ) GOTO 9999

       ! Write the dataset independently for non-chunked data
       CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, field, dimsfi, error, &
            file_space_id = filespace, mem_space_id = memspace)
       
    ELSE
       ! Or like this for chunked data
       ! N.B. blockdim == chunk_dims here
       ! For chunked data can do a collective write
       CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, error, &
            stride, blockdim)         
       IF(error < 0) write(io,*) ' Error',error,'in h5sselect_hyperslab_f, id',myid
       IF (error < 0 ) GOTO 9999

       ! Create property list for collective dataset write
       CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
       IF (error < 0 ) GOTO 9999
       CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
       IF (error < 0 ) GOTO 9999

       CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, field, dimsfi, error, &
            file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)

       
    ENDIF


    


    ! Close dataspaces, dataset, property list, group, and file

    CALL h5sclose_f(filespace, error)
    CALL h5sclose_f(memspace, error)

    CALL h5dclose_f(dset_id, error)

    CALL h5pclose_f(plist_id, error)

    CALL h5gclose_f(group_id,error)

    CALL h5fclose_f(file_id, error)

    
    ! Close FORTRAN interfaces and HDF5 library, deallocate scratch space
    CALL h5close_f(error)
    DEALLOCATE( field )
    

    RETURN

9999 CONTINUE
    WRITE(io,*) 'write_hdf_field encountered an error'
    RETURN
    
  END SUBROUTINE write_hdf_field


  SUBROUTINE SET_OUTPUTFLAGMANUAL( fieldflag, allocflag )

    INTEGER allocflag
    INTEGER fieldflag(nx,ny,nz)

    IF( ALLOCFLAG == 1) THEN
       IF( outputflagmanual_allocated) THEN
          outputflagmanual = fieldflag
       ELSE
          ALLOCATE( outputflagmanual(nx,ny,nz) )
          outputflagmanual_allocated = .TRUE.
          outputflagmanual = fieldflag
       ENDIF
    ELSE IF(ALLOCFLAG == -1) THEN
       IF( outputflagmanual_allocated ) THEN
          DEALLOCATE( outputflagmanual )
          outputflagmanual_allocated = .FALSE.
       ENDIF
    ENDIF


  END SUBROUTINE SET_OUTPUTFLAGMANUAL

  SUBROUTINE SYNC_ERROR(error)
    ! Call this so that if error < 0 on any process it becomes -1 on all
    USE topology_m
    IMPLICIT NONE

    INTEGER error, gerror
    INTEGER mpierror

    CALL MPI_Allreduce(error, gerror, 1, MPI_INTEGER, MPI_MIN, gcomm, mpierror)

    if (gerror < 0 ) error = -1

    RETURN
  END SUBROUTINE SYNC_ERROR
  
  SUBROUTINE initialize_hdf5_interface(io)
    use topology_m
    
    IF(myid == 0) THEN
       OPEN(UNIT=20, file='../input/hdf_output.in',status='old',form='formatted')
       
       READ(20,*)
       READ(20,*)
       READ(20,*)
       READ(20,*) write_hdf5_param%output_uvw
       READ(20,*) write_hdf5_param%output_rho
       READ(20,*) write_hdf5_param%output_temp
       READ(20,*) write_hdf5_param%output_spc
       READ(20,*) write_hdf5_param%output_mixfrac
       READ(20,*) write_hdf5_param%output_chi
       READ(20,*) write_hdf5_param%output_chi_sep
       READ(20,*) write_hdf5_param%output_pressure
       READ(20,*) write_hdf5_param%output_rates
       READ(20,*) write_hdf5_param%output_dilitation
       READ(20,*) write_hdf5_param%output_vorticity
       READ(20,*) write_hdf5_param%output_hrr
       READ(20,*)
       READ(20,*)
       READ(20,*)
       READ(20,*) write_hdf5_param%keep_3d_structure
       READ(20,*) write_hdf5_param%iskip
       READ(20,*) write_hdf5_param%precision
       READ(20,*)
       READ(20,*)
       READ(20,*)
       READ(20,*) write_hdf5_param%output_flag_manual
       READ(20,*) write_hdf5_param%output_rect_subset
       READ(20,*) write_hdf5_param%xmin
       READ(20,*) write_hdf5_param%xmax
       READ(20,*) write_hdf5_param%ymin
       READ(20,*) write_hdf5_param%ymax
       READ(20,*) write_hdf5_param%zmin
       READ(20,*) write_hdf5_param%zmax
       
       CLOSE(20)

       write(*,*) 'Initialized hdf5 interface'
    END IF

       CALL MPI_Bcast( write_hdf5_param%output_uvw, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_rho, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_temp, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_spc, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_mixfrac, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_chi, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_chi_sep, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_pressure, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_rates, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_dilitation, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_vorticity, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_hrr, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%keep_3d_structure, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%iskip, 1, MPI_INTEGER, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%precision, 1, MPI_INTEGER, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_flag_manual, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%output_rect_subset, 1, MPI_LOGICAL, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%xmin, 1, MPI_INTEGER, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%xmax, 1, MPI_INTEGER, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%ymin, 1, MPI_INTEGER, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%ymax, 1, MPI_INTEGER, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%zmin, 1, MPI_INTEGER, 0, gcomm, ierr )
       CALL MPI_Bcast( write_hdf5_param%zmax, 1, MPI_INTEGER, 0, gcomm, ierr )

  END SUBROUTINE initialize_hdf5_interface


  !----------------------------------------------------------------------
  subroutine read_hdf5(io,ierror,dirtime)
    !----------------------------------------------------------------------
    ! Write out an hdf5 file using parallel hdf5
    ! Ray Grout ( 21 March 08)
    ! Format of function calls modelled on Ramanan's / Evatt's write_tecplot
    !----------------------------------------------------------------------
    ! S3D Modules
    use topology_m
    use param_m, only : nx, ny, nz, n_spec
    use reference_m
    use grid_m
    use variables_m, only : q, u, temp, pressure, yspecies
    use chemkin_m, only : species_name, n_species, reaction_rate
    use runtime_m, only : run_title, time, i_time
    use work_m, only : vort_mag => work1_1
    use work_m, only : hr => work1_2
    use mixfrac_m
       
    implicit none
    !----------------------------------------------------------------------
    ! DECLARATIONS PASSED IN

    integer io       ! Output unit
    integer ierror   ! Error on exit
    character dirtime*9
    ! LOCAL DECLARATIONS

    integer i,j,k,L
    real p_conv_atm

    real, dimension(nx,ny,nz,3) :: vort
    real, dimension(nx,ny,nz) :: dil

    real, dimension(nx,ny,nz,n_spec) :: rr_r

    real, dimension(nx,ny,nz) :: grid_store, chi, scratch
    character*9 time_ext
    character*100 filename_short

    !----------------------------------------------------------------------
    ! hdf declarations

    CHARACTER(LEN=8) :: dsetname   ! Dataset name
    CHARACTER(LEN=20) :: groupname  ! Group name
    CHARACTER(LEN=20) :: fieldname  ! Field name
    CHARACTER(LEN=250) :: fielddesc  ! Very long field name
    CHARACTER(LEN=20) :: fieldunits  ! Field units
    CHARACTER(LEN=50) :: filename   ! File name

    LOGICAL :: overwrite
    INTEGER :: error

    !----------------------------------------------------------------------


    ! Return if zero dimensions
    p_conv_atm=p_ref/pres_atm
    IF((nx==1).AND.(ny==1).AND.(nz==1)) THEN
       RETURN
    ELSE
       IF(myid.eq.0) then
          write(io,*) 'Reading hdf files...:'
       ENDIF
    ENDIF



    ! Specify filename on root, broadcast
    IF (myid==0) then  
       !   set time stamp 
       filename_short=trim(run_title)//'.'//trim(dirtime)//'.h5'
       filename='../data/hdf5/'//trim(filename_short)
       write(io,*) 'Reading:',filename
       filename=trim(filename)
    ENDIF
    

    call MPI_Barrier(gcomm, ierr) ! Wait for directory to be created
    call MPI_Bcast(filename,50, MPI_CHARACTER, 0, gcomm, ierr)
    
    
    
    ! Call read_hdf_field for fields to read.

    groupname = "/primary/grid"
    fieldname = "x"
    CALL read_hdf_field( io, filename, groupname, fieldname, grid_store, l_ref*1e3, error)
    scale_1x = 1
    call derivative_x(nx,ny,nz,grid_store,scratch,scale_1x,1)
    x = grid_store(:,1,1)
    scale_1x = 1 / scratch(:,1,1)

    call mpi_allreduce( x(1), xmin, 1, MPI_DOUBLE_PRECISION, MPI_MIN, gcomm, error)
    call mpi_allreduce( x(nx), xmax, 1, MPI_DOUBLE_PRECISION, MPI_MAX, gcomm, error)

    IF(error < 0 ) then
       write(io,*) 'id',myid, 'raised an error during reading '
       GOTO 9999
    ENDIF

    scratch = 0
    groupname = "/primary/grid"
    fieldname = "y"
    CALL read_hdf_field( io, filename, groupname, fieldname, grid_store, l_ref*1e3, error)
    y = grid_store(1,:,1)
    scale_1y = 1
    call derivative_y(nx,ny,nz,grid_store,scratch,scale_1y,1)
    scale_1y = 1 / scratch(1,:,1)

    call mpi_allreduce( y(1), ymin, 1, MPI_DOUBLE_PRECISION, MPI_MIN, gcomm, error)
    call mpi_allreduce( y(ny), ymax, 1, MPI_DOUBLE_PRECISION, MPI_MAX, gcomm, error)

    fieldname = "z"
    CALL read_hdf_field( io, filename, groupname, fieldname,  grid_store, l_ref*1e3, error)
    IF(error < 0 ) GOTO 9999
    z = grid_store(1,1,:) 
    scale_1z = 1
    call derivative_z(nx,ny,nz,grid_store,scratch,scale_1z,1)
    scale_1z = 1 / scratch(1,1,:)
    !    write(*,*) 'scale_1z=',scale_1z
    call mpi_allreduce( z(1), zmin, 1, MPI_DOUBLE_PRECISION, MPI_MIN, gcomm, error)
    call mpi_allreduce( z(nz), zmax, 1, MPI_DOUBLE_PRECISION, MPI_MAX, gcomm, error)


    call MPI_AllGather(x,nx,MPI_DOUBLE_PRECISION,xg,nx,MPI_DOUBLE_PRECISION,xcomm,ierror)
    call MPI_AllGather(y,ny,MPI_DOUBLE_PRECISION,yg,ny,MPI_DOUBLE_PRECISION,ycomm,ierror)
    call MPI_AllGather(z,nz,MPI_DOUBLE_PRECISION,zg,nz,MPI_DOUBLE_PRECISION,zcomm,ierror)


    call MPI_Barrier(gcomm, ierr) ! Wait for directory to be created
    if(myid==0) write(*,*) 'Read grid'

    groupname = "/primary/flow"
    fieldname = "u"
    
    CALL read_hdf_field( io, filename, groupname, fieldname, u(:,:,:,1), a_ref, error)
    IF(error < 0 ) GOTO 9999
    
    fieldname = "v"
    CALL read_hdf_field( io, filename, groupname, fieldname, u(:,:,:,2), a_ref, error)
    IF(error < 0 ) GOTO 9999

    fieldname = "w"
    CALL read_hdf_field( io, filename, groupname, fieldname, u(:,:,:,3),a_ref, error)
    IF(error < 0 ) GOTO 9999
    

    call MPI_Barrier(gcomm, ierr) ! Wait for directory to be created
    if(myid==0) write(*,*) 'Read velocity'

    groupname = "/primary/flow"
    fieldname = "Temp"
    CALL read_hdf_field( io, filename, groupname, fieldname, temp,t_ref, error)
    IF(error < 0 ) GOTO 9999

    
    groupname = "/primary/flow"
    fieldname = "P"
    CALL read_hdf_field( io, filename, groupname, fieldname,pressure,p_conv_atm, error)
    IF(error < 0 ) GOTO 9999
    

    call MPI_Barrier(gcomm, ierr) ! Wait for directory to be created
    if(myid==0) write(*,*) 'Read flow'
    groupname = "/primary/composition"
    DO L=1,n_species
       write(fieldname,*)'"Y '//trim(species_name(L))//'"'
       CALL read_hdf_field( io, filename, groupname, fieldname,yspecies(:,:,:,L), 1.0, error)
       if(myid==0) write(*,*) 'Id 0 read species', trim(species_name(L))
       IF(error < 0 ) GOTO 9999
       
    ENDDO
    
    call MPI_Barrier(gcomm, ierr) ! Wait for directory to be created
    if(myid==0) write(*,*) 'Read species'
    ierror = 0
    return

9999 CONTINUE
    ierror = error
    return

9   format(10(1pe12.5,1x))
    !!----------------------------------------------------------------------
    return

  END SUBROUTINE read_hdf5

  subroutine read_hdf_field( io, filename, groupname, fieldname,  unscaledfield, scaling, error)

    ! S3D Modules
    use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec, npx, npy, npz
    use topology_m
    ! Parallel HDF5
    USE hdf5 ! This module contains all necessary modules for hdf5

    implicit none    

    ! Input arguments
    CHARACTER(LEN=50), INTENT(IN) :: filename   ! File name
    CHARACTER(LEN=20), INTENT(IN) :: groupname  ! Group name
    CHARACTER(LEN=20), INTENT(IN) :: fieldname  ! Field name
    REAL, DIMENSION(NX,NY,NZ), INTENT(OUT) :: unscaledfield ! Field data
    REAL, INTENT(IN) :: scaling
    INTEGER, INTENT(IN) :: io
    INTEGER, INTENT(OUT) :: error 
    INTEGER, DIMENSION(nx, ny,nz) :: outputflag 

    ! Local variables
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: field ! Field data

    ! HDF variables
    CHARACTER(LEN=8) :: dsetname   ! Dataset name
    INTEGER(HID_T) :: file_id       ! File identifier 
    INTEGER(HID_T) :: primary_group, derived_group, group_id       ! group identifiers
    INTEGER(HID_T) :: dset_id       ! Dataset identifier 
    INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
    INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
    INTEGER(HID_T) :: plist_id      ! Property list identifier 
    INTEGER(HSIZE_T), DIMENSION(3) :: dimsf  ! Dataset dimensions
    ! in the file.
    INTEGER(HSIZE_T), DIMENSION (3) :: dimsfi 
    INTEGER(HSIZE_T), DIMENSION(3) :: chunk_dims  ! Chunks dimensions
    INTEGER(HSIZE_T),  DIMENSION(3) :: count  
    INTEGER(HSSIZE_T), DIMENSION(3) :: offset, offset_mem
    INTEGER(HSIZE_T),  DIMENSION(3) :: stride
    INTEGER(HSIZE_T),  DIMENSION(3) :: blockdim
    INTEGER :: rank = 3 ! Dataset rank 

    ! Counters
    INTEGER pcounter, ifull,jfull,kfull, countpx, countpy, countpz
    INTEGER ifield, jfield, kfield
    INTEGER iglobal, jglobal, kglobal

    ! For data exchange of number of data points per process
    INTEGER, DIMENSION(NPX) :: COUNTX
    INTEGER, DIMENSION(NPY) :: COUNTY
    INTEGER, DIMENSION(NPZ) :: COUNTZ
    INTEGER, DIMENSION(npx*npy*npz) :: COUNTXG, COUNTYG, COUNTZG

    ! Each process calculates how many points it's going to read in the 
    ! x/y/z directions, tells other processes along that direction, then
    ! uses the information from the other processes to find it's offset
    ! This might be dangerous if outputflag array isn't marked out in a rectangular form
       blockdim(1) = nx
       blockdim(2) = ny
       blockdim(3) = nz


       call MPI_Allgather( blockdim(1), 1, MPI_INTEGER, countxg, 1, MPI_INTEGER, gcomm, ierr)
       call MPI_Allgather( blockdim(2), 1, MPI_INTEGER, countyg, 1, MPI_INTEGER, gcomm, ierr)
       call MPI_Allgather( blockdim(3), 1, MPI_INTEGER, countzg, 1, MPI_INTEGER, gcomm, ierr)


       offset = 0
       dimsf = 0

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

       DO pcounter = 1, npx
          IF(pcounter <= mypx ) then
             offset(1) = offset(1) + countx(pcounter)
          ENDIF
          dimsf(1) = dimsf(1) + countx(pcounter)
       ENDDO

       DO pcounter = 1, npy
          IF(pcounter <= mypy ) then
             offset(2) = offset(2) + county(pcounter)
          ENDIF
          dimsf(2) = dimsf(2) + county(pcounter)
       ENDDO

       DO pcounter = 1, npz
          IF(pcounter <= mypz ) then
             offset(3) = offset(3) + countz(pcounter)
          ENDIF
          dimsf(3) = dimsf(3) + countz(pcounter)
       ENDDO

       ! Get global file dimensions
       CALL MPI_Allreduce(dimsf(1), dimsfi(1), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       CALL MPI_Allreduce(dimsf(2), dimsfi(2), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       CALL MPI_Allreduce(dimsf(3), dimsfi(3), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       dimsf = dimsfi


       ! Read field into buffer before 
       ALLOCATE( field(blockdim(1),blockdim(2),blockdim(3)) )





    !----------------------------------------------------------------------
    ! Initialize HDF5 library/fortran interface, setup file/dataset

    CALL h5open_f(error) 
    IF (error < 0) GOTO 9999

    ! 
    ! Setup file access property list with parallel I/O access.
    !
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
    IF (error < 0) GOTO 9999

    CALL h5pset_fapl_mpio_f(plist_id, gcomm, MPI_INFO_NULL, error)
    IF (error < 0) GOTO 9999


    !
    ! Open file
       CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error, access_prp = plist_id)
       IF(error == -1 ) THEN
          IF(myid.eq.0) write(io,*)'Could not open ',filename
       ENDIF
       IF (error < 0) GOTO 9999
       CALL h5pclose_f(plist_id, error)

    ! Open group
       CALL h5gopen_f(file_id, groupname, group_id, error);
       IF(error < 0 ) THEN 
          IF(myid==0) THEN
             write(io,*) 'Could not open group', groupname
             write(io,*) 'Failed with error: ',error
          ENDIF
          GOTO 9999
       ENDIF

    ! Open dataset
       CALL h5dopen_f(group_id, fieldname, dset_id, error);
       IF(error < 0 ) THEN 
          IF(myid==0) THEN
             write(io,*) 'Could not open group', groupname
             write(io,*) 'Failed with error: ',error
          ENDIF
          GOTO 9999
       ENDIF

       ! Create filespace
       CALL h5screate_simple_f(rank, dimsf, filespace, error)
       
       ! Check to see if block dimensions are the same on all processors, use offset_mem as temporary storage
       CALL MPI_Allreduce(blockdim(1), offset_mem(1), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       CALL MPI_Allreduce(blockdim(1), chunk_dims(1), 1, MPI_INTEGER, MPI_MIN, gcomm, error)
       
       CALL MPI_Allreduce(blockdim(2), offset_mem(2), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       CALL MPI_Allreduce(blockdim(2), chunk_dims(2), 1, MPI_INTEGER, MPI_MIN, gcomm, error)
       
       CALL MPI_Allreduce(blockdim(3), offset_mem(3), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
       CALL MPI_Allreduce(blockdim(3), chunk_dims(3), 1, MPI_INTEGER, MPI_MIN, gcomm, error)

!       IF( offset_mem(1) == chunk_dims(1) .AND. offset_mem(2) == chunk_dims(2) .AND. offset_mem(3) == chunk_dims(3) ) then
!          ! In this case use a chunked dataset
!          chunk_dims = blockdim
!          CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
!          CALL h5pset_chunk_f(plist_id, rank, chunk_dims, error)
!          !       chunk_dims = -1
!          
!       ELSE
!          CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
!          chunk_dims = -1
!       endif
!       
       

       ! Create memory space
       IF (MINVAL(blockdim) > 0 ) THEN
          CALL h5screate_simple_f(rank, blockdim, memspace, error)    !
       ELSE
          CALL h5screate_simple_f(rank, blockdim+1, memspace, error)    !
          CALL h5sselect_none_f(memspace,error)
       ENDIF
       IF (error < 0 ) write(io,*) 'Error creating memspace'

       ! Select hyperslab in the file.
       stride = 1
       count = 1
       
       !    IF(myid==0) then
       !       write(io,*)'stride=',stride
       !       write(io,*)'count=',count
       !       write(io,*)'blockdim=',blockdim
       !       write(io,*)'offset=',offset
       !       write(io,*)'dimsfi=',dimsfi
       !       write(io,*)'dimsf=',dimsf
       !    END IF
       
       !CALL h5dget_space_f(dset_id, filespace, error)
       !IF(error < 0) write(io,*) ' Error',error,'in h5dget_space_f, id',myid

       ! Do it like this for non-chunked data
       IF(chunk_dims(1) == -1 ) THEN
          IF (MINVAL(blockdim) > 0 ) THEN 
             CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, blockdim, error, &
                  stride, count)          
          ELSE
             CALL h5sselect_none_f(filespace,error)
          ENDIF
          IF(error < 0) write(io,*) ' Error',error,'in h5sselect_hyperslab_f, id',myid
          IF (error < 0 ) GOTO 9999
          
       ! Write the dataset independently for non-chunked data
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, field, dimsfi, error, &
               file_space_id = filespace, mem_space_id = memspace)
       ELSE
          ! Or like this for chunked data
          ! N.B. blockdim == chunk_dims here
          ! For chunked data can do a collective write
          CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, count, error, &
            stride, blockdim)         
          IF(error < 0) write(io,*) ' Error',error,'in h5sselect_hyperslab_f, id',myid
          IF (error < 0 ) GOTO 9999
          
          ! Create property list for collective dataset write
          CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
          IF (error < 0 ) GOTO 9999
          CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
          IF (error < 0 ) GOTO 9999
          

          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, field, dimsfi, error, &
               file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)
          
          
       ENDIF
       

    


    ! Close dataspaces, dataset, property list, group, and file

    CALL h5sclose_f(filespace, error)
    CALL h5sclose_f(memspace, error)

    CALL h5dclose_f(dset_id, error)

    CALL h5pclose_f(plist_id, error)

    CALL h5gclose_f(group_id,error)

    CALL h5fclose_f(file_id, error)

    
    ! Close FORTRAN interfaces and HDF5 library
    CALL h5close_f(error)

    ! Scale the field
      unscaledfield = field /scaling

       ! deallocate scratch space
    DEALLOCATE( field )
    

    RETURN

9999 CONTINUE
    WRITE(io,*) 'read_hdf_field encountered an error'
    RETURN
    
  END SUBROUTINE read_hdf_field

END MODULE s3d_hdf_interface_m

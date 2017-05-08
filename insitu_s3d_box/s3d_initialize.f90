subroutine s3d_initialize( provided_communicator, io  )
  
    use param_m
    use topology_m
    use chemkin_m
    use runtime_m

    use reference_m, only : initialize_reference  !routine reference

    use filter_m, only : initialize_filter, allocate_filter_arrays  !routine references

    use rk_m, only : initialize_rk, allocate_rk_arrays  !routine references

    use thermchem_m, only : initialize_thermchem        !routine reference
    use thermchem_m, only : allocate_thermchem_arrays   !routine reference

    use variables_m, only : allocate_variables_arrays  !routine reference

    use work_m, only : allocate_work_arrays  !routine reference

    use grid_m, only : initialize_grid  !routine reference
    use grid_m, only : allocate_grid_arrays  !routine reference

    use bc_m, only : initialize_bc  !routine reference
    use bc_m, only : allocate_bc_arrays  !routine reference

    use transport_m, only : pr

    use transport_m, only : initialize_transport  !routine reference
    use transport_m, only : allocate_transport_arrays  !routine reference

    use derivative_m, only : initialize_derivative  !routine reference
    use derivative_m, only : allocate_derivative_arrays  !routine reference

!HK. For insitu_analytics
    use mixfrac_m, only: allocate_mixfrac_arrays, calculated_mixfrac_coeffs, calc_mixfrac_coeffs

    implicit none

    !----------------------------------------------------------------------------------------
    ! This needs to be a fortran communicator
    !   ( can convert c communictor to fortran with c MPI: f_comm = MPI_Comm_c2f( c_comm ) )
    integer, intent(in) :: provided_communicator

    integer, intent(out) :: io          ! io unit based on s3d input
  
    ! Locals
    character nodeid_str*18, rank_ext*5
    integer ierr_adios, L
  
    !----------------------------------------------------------------------------------------
    ! initialization of MPI parameters from above
    gcomm = provided_communicator
    call MPI_Comm_rank(gcomm, myid, ierr)
    call MPI_Comm_size(gcomm, npes, ierr)

    write(rank_ext,'(i5.5)') myid
    nodeid_str = 'Rank '//trim(rank_ext)//' is node'
    !call print_xtnodeid(trim(nodeid_str)//char(0))
    ! write(*,*) nodeid_str
  
    !----------------------------------------------------------------------------------------
    ! read data from s3d.in
    call read_input(6) 
    !----------------------------------------------------------------------------------------
    ! set output to screen or file
    if(i_write.eq.0) then
        io=6
    else
        io=7
        if(myid.eq.0) then
            open(unit=io,file='../data/'//trim(run_title)//'.out', status='REPLACE')
        endif
    endif
    !----------------------------------------------------------------------------------------
    ! write header

    if(myid.eq.0) then
        call write_header(io,'=')
        write(io,*) 'Hello, and welcome to S3D!'
        call write_header(io,'=')
        call writeopts()
    endif
    !----------------------------------------------------------------------------------------
    ! get start date and time and write to file
    if(myid.eq.0) then
        call write_header(io,'-')
        call date_and_time(dat_1,tim_1)
        write(io,*) 'this run started at:'
        write(io,*)
        call write_date_and_time(dat_1,tim_1,io)
        call write_header(io,'-')
    endif
    !----------------------------------------------------------------------------------------
    ! initialize primary modules
    !----------------------------------------------------------------------------------------
    ! initialize chemkin module
    call initialize_chemkin(io,myid,ierr,gcomm)
    ! initialize adios
#ifdef ADIOS

    if( myid == 0 ) then
        DO L=1,n_species
            call add_species_to_xml( L, trim(species_name(L))//char(0) );
        enddo
    endif

    ! wait for root to finish updating xml file
    call MPI_Barrier(gcomm, ierr )
    !call terminate_run(io,0)
    call adios_init( "s3d.xml"//CHAR(0), ierr_adios )
    if( myid == 0 ) then
        if (ierr_adios< 0 ) then
            write (*,*) "adios broken"
        else
            write(*,*) "Initialized ADIOS; ", ierr_adios
        endif
    endif
#endif

    !----------------------------------------------------------------------------------------
    ! initialize param module (must be done after initializing chemkin module)
    call initialize_param(io,myid,ierr,gcomm)
    !----------------------------------------------------------------------------------------
    ! intialize topology module (must be done after initializing param module)
    call initialize_topology(io,nx,ny,nz,npx,npy,npz,iorder,iforder)

    ! sync processors
    call MPI_Barrier(gcomm,ierr)
    !----------------------------------------------------------------------------------------
    ! Setup work arrays - s3d scratch
    call allocate_work_arrays(1)
    !----------------------------------------------------------------------------------------
    !Initialize solver modules  
    call initialize_reference(io,pr)
    call initialize_rk(io)
    call initialize_transport(io)
    call initialize_filter(io)
    call initialize_derivative(io)
    call initialize_thermchem(io)
    call initialize_grid(io)
    call initialize_bc(io)

!HK. Setup some initialisation in the mixfrac module
!HK. This is required only for insitu_analytics which 
!HK. needs mixture fraction and chi from primitive vars
    call allocate_mixFrac_arrays(1)
    if(.not.calculated_mixfrac_coeffs) call calc_mixfrac_coeffs

    call flush(io)
    call MPI_Barrier(gcomm,ierr)

end subroutine


subroutine s3d_finalize(io)
    use param_m
    use topology_m
    use chemkin_m
    use runtime_m

    use filter_m, only : allocate_filter_arrays  !routine references
    use rk_m, only : allocate_rk_arrays  !routine references
    use thermchem_m, only : allocate_thermchem_arrays   !routine reference
    use variables_m, only : allocate_variables_arrays  !routine reference
    use work_m, only : allocate_work_arrays  !routine reference
    use grid_m, only : allocate_grid_arrays  !routine reference
    use bc_m, only : allocate_bc_arrays  !routine reference
    use transport_m, only : allocate_transport_arrays  !routine reference
    use derivative_m, only : allocate_derivative_arrays  !routine reference
!HK.
    use mixfrac_m, only: allocate_mixFrac_arrays

    implicit none

      !----------------------------------------------------------------------------------------
      ! Arguments
    integer, intent(in) :: io          !output io unit
    !----------------------------------------------------------------------------------------
    ! Locals
    character nodeid_str*18, rank_ext*5
    integer ierr_adios, L
    !----------------------------------------------------------------------------------------
    ! get final date and time and write to file
    if(myid.eq.0) then
        call write_header(io,'-')
        write(io,*) 'S3D started at:'
        write(io,*)
        call write_date_and_time(dat_1,tim_1,io)
        write(io,*)
        write(io,*) 'S3D ended at:'
        write(io,*)
        call date_and_time(dat_2,tim_2)
        call write_date_and_time(dat_2,tim_2,io)
    endif
    !----------------------------------------------------------------------------------------
    ! write closing statement
    if(myid.eq.0) then
        call write_header(io,'=')
        write(io,*) 'Program finished normally, goodbye!'
        call write_header(io,'=')
    endif

      !----------------------------------------------------------------------------------------
    ! close output file if it exists

    if(i_write.ne.0) then
        if(myid.eq.0) close(io)
    endif
      !----------------------------------------------------------------------------------------
    ! clean up io
#ifdef ADIOS
    if( myid == 0 ) write(io,*) 'finalizing adios'
    call adios_finalize( myid, ierr_adios )
#endif
    !----------------------------------------------------------------------------------------
    ! deallocate arrays
    call allocate_derivative_arrays(-1)
    call allocate_filter_arrays(-1)
    call allocate_thermchem_arrays(-1)
    call allocate_transport_arrays(-1)
    call allocate_rk_arrays(-1)
    call allocate_variables_arrays(-1)
    call allocate_work_arrays(-1)
    call allocate_grid_arrays(-1)
    call allocate_bc_arrays(-1)

    call allocate_chemkin_arrays(-1)
!HK.
    call allocate_mixFrac_arrays(-1)

    call MPI_Barrier(gcomm,ierr)
    call flush(io)
    call MPI_Barrier(gcomm,ierr)
  
    !----------------------------------------------------------------------------------------
    ! S3D is clean, return to driver for MPI finalization
end subroutine s3d_finalize



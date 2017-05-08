!===============================================================================!
!    Program: s3d
!    File:    $HeadURL$
!
!    S3D was developed at SNL (CRF) under the direction of Dr. J.H. Chen
! 
!    This file
!    authored by:   Ray W. Grout (National Renewable Energy Laboratory)
!                   ray.grout@nrel.gov
!
!    Modified by:   
!
!             On:   04Oct2011
!
!    Description:   Implement unit test for temperature newton solve routines
!
!
!    Last Revision: $Rev$
!    Revision by:   $Author$
!    Date:          $Date$
!===============================================================================
program calc_temp_test

    ! These are real s3d modules
    use topology_m
    use reference_m, only : l_ref, t_ref
    use thermchem_m, only : initialize_thermchem        !routine reference
    use thermchem_m, only : allocate_thermchem_arrays   !routine reference
    use thermchem_m
    use chemkin_m, only : initialize_chemkin

    ! This is a stripped down version in tests/thermchem/__test__param_m.f90
    use param_m

    implicit none

    character(10) buffer

    ! These are only used as dummy variables in init routines
    integer :: n_elem, n_reac, ntr

    ! Grid setup for test purposes
    nx = 38
    ny = 38
    nz = 38

    call getarg(1, buffer)
    read(buffer, *)npx
    npy = npx
    npz = npx

    nx_g = nx*npx
    ny_g = ny*npy
    nz_g = nz*npz

    vary_in_x = 1; vary_in_y = 1; vary_in_z = 1

    ! Set derivative and filter order in param_m
    iorder = 8
    iforder = 10

    ! Setup MPI, Cartesian MPI grid, etc.
    call initialize_topology(6, nx, ny, nz, &
                             npx, npy, npz, &
                             iorder, iforder )


    !-- initialize thermo and chemistry module (must be done after reference module)
    call initialize_chemkin(6, myid, ierr, gcomm)

    ! set n_spec (n_elem, n_reac not used in this test)
    call set_number_elem_spec_reac(n_elem, n_spec, n_reac, myid, 6)

    nsc = n_spec - 1      !set number of chemical species for DNS purposes
    nvar_tot = nsc + 5    !number of species + 5 (density, energy, momentum)

    ! Setup thermchem module - build cp table, allocate variables, etc.
    call initialize_thermchem(6)

    ! Run test - will "stop 1" if it fails
    write(*,*) 'Thermochem test ...'
    call thermochem_test
    write(*,*) '... test successful.'

    call terminate_run(6, 0)

end program calc_temp_test

subroutine thermochem_test
    use topology_m
    use reference_m, only : l_ref, t_ref
    use thermchem_m, only : initialize_thermchem        !routine reference
    use thermchem_m, only : allocate_thermchem_arrays   !routine reference
    use thermchem_m
    use param_m, only : nx, ny, nz, n_spec

    implicit none
    
    ! These are inputs to calc_temp - set them up with test data
    real :: yspecies(nx,ny,nz,n_spec)
    real :: e0(nx,ny,nz) 
    real :: u(nx,ny,nz,3)

    ! U
    real, dimension(nx,ny,nz) :: temp, &
                                 temp_recovered
    
    real :: enthmix, r_gas

    real :: test_temp 
    integer i,j,k, L
    integer, parameter :: ntemp = 10
    
    ! Setup test composition
    !yspecies = 0.0d0
    !yspecies(:,:,:,n_spec) = 1.0d0
    yspecies = 1.0/n_spec


    ! Setup test mol wt
    call calc_inv_avg_mol_wt(yspecies,avmolwt)

    ! Sweep through temperature range
    do L = 1, ntemp
        test_temp = 300 + 2100/(ntemp)*L

        ! Setup test  temperature
        temp = test_temp /  t_ref
        !write(*,*) "nondim temp: ", temp(1,1,1), t_ref


        do k=1,nz,1
            do j=1,ny,1
                do i=1,nx,1

                    r_gas=avmolwt(i,j,k)*Ru
                    enthmix = mixEnth( yspecies(i,j,k,:), temp(i,j,k) )
                    e0(i,j,k)=( enthmix-r_gas*temp(i,j,k) )

                enddo
            enddo
        enddo


        ! Keep this as zero since we didn't add it in above 
        ! ( it gets subtracted off in calc_temp )
        u = 0.0

        ! This is initial guess for temperature
        temp_recovered = 4.0

        ! Compute temperature from mixture composition and energy
        call calc_temp(temp_recovered, e0, u, yspecies)


        if(  maxval( temp_recovered*t_ref - test_temp ) > 1.0e-6 ) then
            write(*,*) 'recovered: ', temp_recovered(1,1,1)*t_ref
            write(*,*) 'error is: ', temp_recovered(1,1,1)*t_ref - test_temp
            stop 1
        endif
        if( myid == 0 ) write(*,*) 'Checked temp ', test_temp
        
    enddo
    
end subroutine thermochem_test



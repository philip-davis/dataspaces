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
!    Description:   Implement unit test for derivative routines
!
!
!    Last Revision: $Rev$
!    Revision by:   $Author$
!    Date:          $Date$
!===============================================================================
program deriv_test

    ! These are real s3d modules
    use topology_m
    use derivative_m
    use grid_m
    use reference_m, only : l_ref

    ! This is a stripped down version in tests/stubs/__test__param_m.f90
    use param_m

  
    implicit none

    integer i
    character(10) buffer

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

    ! Set derivative and filter order in param_m
    iorder = 8
    iforder = 10

    nxm_g  = max(1,nx_g - 1)   
    nym_g  = max(1,ny_g - 1)   
    nzm_g  = max(1,nz_g - 1)   

    vary_in_x = 1; vary_in_y = 1; vary_in_z = 1
    periodic_x = 0; periodic_y = 0; periodic_z = 0

    ! Grid physical extents
    xmin=0.0
    xmax=3.14159 * 100.0 * l_ref

    ymin=0.0
    ymax=3.14159 * 100.0 * l_ref

    zmin=0.0
    zmax=3.14159 * 100.0 * l_ref

    unif_grid_x = 1; unif_grid_y = 1; unif_grid_z=1

    ! Setup MPI, Cartesian MPI grid, etc.
    call initialize_topology(6, nx, ny, nz, &
                             npx, npy, npz, &
                             iorder, iforder )


    ! Setup grid - scale arrays for stretched grid 
    ! used in derivatives, coordinates useful for
    ! generating test data

    call initialize_grid(6)

    !do i=1,nx
    !    write(*,*) i, x(i)
    !enddo

    ! Allocate derivative arrays
    call initialize_derivative(6)

    ! Run test - will "stop 1" if it fails
    write(*,*) 'Derivative test ...'
    call derivative_xyz_test
    write(*,*) '... test successful.'

    call terminate_run(6, 0)

end program deriv_test


subroutine derivative_xyz_test
    use topology_m
    use param_m
    use grid_m

    implicit none

    ! Number of test fields to generate and differentiate
    integer, parameter :: nderiv = 8
    real :: xshift, yshift, zshift
  
    ! Test fails if local rms error for any process exceeds these
    real, parameter :: xtol = 0.1
    real, parameter :: ytol = 0.1
    real, parameter :: ztol = 0.1

    ! Test fields, derivative return value, analytic value for comparision
    real, dimension(nx,ny,nz,nderiv) :: operand
    real, dimension(nx,ny,nz) ::  deriv_result
    real, dimension(nx,ny,nz,nderiv) ::  analy_dx, analy_dy, analy_dz

    ! Work variables
    integer :: i,j,k,n
    real :: err, erms



    ! Setup operand to be test field with analytic derivative
    do n = 1, nderiv
     
        xshift = n*(xmax - xmin)*0.1
        yshift = n*(ymax - ymin)*0.1
        zshift = n*(zmax - zmin)*0.1
     
        do k = 1, nz
            do j = 1, ny
                do i = 1, nx
                    !operand(i,j,k)  = sin(x(i))*cos( 2.0*y(j) )*sin( 3.0*z(k) )
                    !analy_dx(i,j,k) = cos(x(i))*cos( 2.0*y(j) )*sin( 3.0*z(k) )
                    !analy_dy(i,j,k) = sin(x(i)) &
                    !                      *(-2.0)*sin( 2.0*y(j) ) &
                    !                      *sin( 3.0*z(k) )
                    !analy_dz(i,j,k) = sin(x(i)) &
                    !                      *cos( 2.0*y(j) ) &
                    !                      *3.0d0*cos( 3.0*z(k) )

                    !operand(i,j,k,n)  = sin(x(i))*sin(y(j))*sin(z(k))
                    !analy_dx(i,j,k,n) = cos(x(i))*sin(y(j))*sin(z(k))
                    !analy_dy(i,j,k,n) = sin(x(i))*cos(y(j))*sin(z(k))
                    !analy_dz(i,j,k,n) = sin(x(i))*sin(y(j))*cos(z(k))

                    operand(i,j,k,n)  = sin(x(i)-xshift)*sin(y(j)-yshift)*sin(z(k)-zshift)
                    analy_dx(i,j,k,n) = cos(x(i)-xshift)*sin(y(j)-yshift)*sin(z(k)-zshift)
                    analy_dy(i,j,k,n) = sin(x(i)-xshift)*cos(y(j)-yshift)*sin(z(k)-zshift)
                    analy_dz(i,j,k,n) = sin(x(i)-xshift)*sin(y(j)-yshift)*cos(z(k)-zshift)
              
                enddo
            enddo
        enddo
     
    enddo


    !do n = 1, nderiv

    !    call derivative_x_post( nx, ny, nz, &
    !                            operand(:,:,:,n), n, 'test-1' )

    !    call derivative_x_send( nx, ny, nz, operand(:,:,:,n), n, 'test-1' )

    !enddo
    do n = nderiv, 1, -1
        call derivative_x_post( nx, ny, nz, &
                                operand(:,:,:,n), n, 'test-1' )
    enddo

    do n = 1, nderiv
        call derivative_x_send( nx, ny, nz, operand(:,:,:,n), n, 'test-1' )
    enddo


    do n = 1, nderiv
     
        call derivative_x_calc_buff( nx, ny, nz, operand(:,:,:,n), deriv_result, &
                                     scale_1x, 1, n )

        ! Compare to analytic solution
        erms = 0.0d0
        do k = 1, nz
            do j = 1, ny
                do i = 1, nx
              
                    err = ( deriv_result(i,j,k) - analy_dx(i,j,k,n) )
                    erms = erms + err*err
              
                enddo
            enddo
        enddo
        erms = sqrt( erms )

        !write(*,*) 'n ', n, 'erms_x = ', erms
        if( erms > xtol ) then
            write(*,*) 'n ', n, 'erms_x = ', erms
            !  print comparision
            open(unit=10, file='deriv_x_test.out', status='unknown')
            do i = 1, nx
                write(10,'(4e25.10)') x(i), deriv_result(i,ny/2,nz/2), &
                                      operand(i,ny/2,nz/2,n), analy_dx(i,ny/2,nz/2,n)
            enddo
            close(10)
            stop 1
        endif
     
    enddo

    do n = nderiv, 1, -1
        call derivative_y_post( nx, ny, nz, &
        operand(:,:,:,n), n, 'test-1' )
    enddo

    do n = 1, nderiv
        call derivative_y_send( nx, ny, nz, operand(:,:,:,n), n, 'test-1' )
    enddo

  
    do n = 1, nderiv

        call derivative_y_calc_buff( nx, ny, nz, &
                                     operand(:,:,:,n), deriv_result, &
                                     scale_1y, 1, n )

        ! Compare to analytic solution
        erms = 0.0d0
        do k = 1, nz
            do j = 1, ny
                do i = 1, nx
                    err = ( deriv_result(i,j,k) - analy_dy(i,j,k,n) )
                    erms = erms + err*err
                enddo
               !write(*,*) j, k, 'erms=', erms
            enddo
        enddo
        erms = sqrt( erms )


        if( erms > ytol ) then
            write(*,*) 'erms_y = ', erms
            open(unit=10, file='deriv_y_test.out', status='unknown')
            do j = 1, ny
                write(10,'(4e25.10)') y(j), deriv_result(nx/2,j,nz/2), &
                                      operand(nx/2,j,nz/2,n), analy_dy(nx/2,j,nz/2,n)
            enddo
            close(10)
            stop 1
        endif
     
    enddo


    do n = nderiv, 1, -1
     
        call derivative_z_post( nx, ny, nz, &
                                operand(:,:,:,n), n, 'test-1' )

    enddo
    do n = 1, nderiv
        call derivative_z_send( nx, ny, nz, operand(:,:,:,n), n, 'test-1' )

    enddo

  
    do n= 1, nderiv
     
        call derivative_z_calc_buff( nx, ny, nz, operand(:,:,:,n), deriv_result, &
                                     scale_1z, 1, n )

        ! Compare to analytic solution
        erms = 0.0d0
        do k = 1, nz
            do j = 1, ny
                do i = 1, nx
                    err = ( deriv_result(i,j,k) - analy_dz(i,j,k,n) )
                    erms = erms + err*err
                enddo
            enddo
        enddo
        erms = sqrt( erms )

        if( erms > ztol ) then
            write(*,*) 'erms_z = ', erms
            open(unit=10, file='deriv_z_test.out', status='unknown')
            do k = 1, nz
                write(10,'(4e25.10)') z(k), deriv_result(nx/2,ny/2,k), &
                                      operand(nx/2,ny/2,k,n), analy_dy(nx/2,ny/2,k,n)
            enddo
            close(10)
            stop 1
        endif

    enddo

  
end subroutine derivative_xyz_test

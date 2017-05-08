subroutine initialize_hcci( io, yspecies, temp, pressure, u )
!------------------------------------------------------------------------------!
! This subroutine initializes the following quantities:                        !
!                                                                              !
!   yspecies - Species mass fractions                                          !
!   temp     - Temperature                                                     !
!   pressure - pressure field                                                  !
!   u        - Mean velocity field                                             !
!                                                                              !
! It is currently set up to do a mixture fraction spectrum (2D or 3D)          ! 
!   and a temperature spectrum (2D or 3D)                                      !
!------------------------------------------------------------------------------!
  use param_m
  use topology_m
  use runtime_m, only : run_title
  use reference_m
  use chemkin_m, only : species_name, molwt
  use thermchem_m 
  use grid_m, only : ymax, ymin, xmax, xmin

  use mixfrac_m
!------------------------------------------------------------------------------
  implicit none
!------------------------------- Passed Variables -----------------------------

  real, intent(inout), dimension(nx,ny,nz) :: temp, pressure
  real, intent(inout), dimension(nx,ny,nz,n_spec) :: yspecies
  real, intent(inout), dimension(nx,ny,nz,3) :: u

  integer, intent(in) :: io

!------------------------------ Local Variables -------------------------------
  character*30 :: filename

  logical :: exist, err

  integer :: i,j,k,L,n

! variables read from input file

  real :: fuel_temp, oxid_temp          ! initial temp of fuel and oxidizer streams
  real :: press                         ! initial pressure of the system (assumed constant)
  character*10 :: scalarProfile         ! specifies mode for mixture fraction initialization
  real :: f_mean, f_rms                 ! mixture fraction average and rms
  real :: ke_mix(4)                     ! first two elements are the dominant length scale for
                                        ! mixture fraction fluctuations and the range of variation
                                        ! from that length scale
  integer :: seed                       ! seed for random number generator

  character*10 :: tempProfile           ! specifies mode for temperature initialization
  real :: t_mean, t_rms                 ! temperature average and rms
  real :: t_gamp                        ! amplitude of superimposed gaussian
  real :: ke_temp(4)                    ! first two elements are the dominant length scale for
                                        ! temperature fluctuations and the range of variation
                                        ! from that length scale
  integer :: seedt                      ! seed for random number generator

! variables for scalar mixing layer initialization

  real :: arg, inc, dfdymax, a, LL, fmax, fmin
  integer :: jj

! variables for temperature solve
  real :: h_o, h_f, h_mix, h_old, h_new         ! enthalpies (temperature solve)
  real :: t_old, t_new                          ! temperatures in solve
  real :: f_old, f_new, df                      ! function to zero in solve
  real :: avgmolwt

  integer :: iter
  real, parameter :: tol = 1e-4         ! tolerance for temperature solve
  integer, parameter :: maxiter = 100   ! maximum iterations for temp solve
  logical :: converged                  ! convergence flag

!---------------------------- Executable Statements ----------------------------

  if (myid == 0) then
     write(io,*) '**********************************'
     write(io,*) 'INITIALIZING scalar variation case '
     write(io,*) '**********************************'
  endif

!-------------------------------------------------------------------------------
!----------------------------- READ the INPUT FILE -----------------------------

  if (myid==0) then
     filename = '../input/' // trim(run_title) // '.in'         ! set the file name
     inquire(file=trim(filename),exist=exist)                   ! see if the file exists
  endif

  call MPI_Bcast( exist, 1, MPI_LOGICAL, 0, gcomm, ierr )

  if(.not.exist) then   !does not exist
     if(myid==0) then
        write(io,*) 'the following input file does not exist:'
        write(io,*) trim(filename)
     endif
     call terminate_run(io,0)
  endif

  if (myid == 0) then
     open( unit=20, file=trim(filename), status='old', form='formatted' )

   ! read the variables from the file

     read( 20,* )  fuel_temp       ! initial temp of fuel stream   (K)
     read( 20,* )  oxid_temp       ! initial temp of fuel stream   (K)
     read( 20,* )  press           ! initial pressure of the system (assumed constant)  (atm)

     read( 20,* )  scalarProfile   ! mode for scalar initialization
     read( 20,* )  f_mean          ! mixture fraction average
     read( 20,* )  f_rms           ! mixture fraction rms
     ke_mix = 0.0
     read( 20,* )  ke_mix(1)       ! dominant length scale of fluctuations (m)
     read( 20,* )  ke_mix(2)       ! range of variations of this length scale (m)
     read( 20,* )  seed            ! seed for random number generator

     read( 20,* )  tempProfile     ! mode for temp initialization
     read( 20,* )  t_mean          ! temperature average
     read( 20,* )  t_rms           ! temperature rms
     read( 20,* )  t_gamp          ! temperature amplitude of gaussian
     ke_temp = 0.0
     read( 20,* )  ke_temp(1)      ! dominant length scale of fluctuations (m)
     read( 20,* )  ke_temp(2)      ! range of variations of this length scale (m)
     read( 20,* )  seedt            ! seed for random number generator
     close( 20 )

     ke_mix = ke_mix/l_ref
		 ke_temp = ke_temp/l_ref

  endif

! broadcast these parameters - we could pack them first and then broadcast - save time...
  call MPI_Bcast( fuel_temp, 1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( oxid_temp, 1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( press,     1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( scalarProfile,10,MPI_CHARACTER,0,gcomm, ierr )
  call MPI_Bcast( f_mean,    1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( f_rms,     1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( ke_mix,    4, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( seed,      1, MPI_INTEGER,0,gcomm, ierr )
  call MPI_Bcast( tempProfile,10,MPI_CHARACTER,0,gcomm, ierr )
  call MPI_Bcast( t_mean,    1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( t_rms,     1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( t_gamp,    1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( ke_temp,    4, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( seedt,      1, MPI_INTEGER,0,gcomm, ierr )

! Non-dimensionalize these quantities
  fuel_temp = fuel_temp / t_ref
  oxid_temp = oxid_temp / t_ref
  press = press*101325.0 / p_ref

	t_mean = t_mean / t_ref
	t_rms = t_rms / t_ref
	t_gamp= t_gamp/ t_ref

!-------------------------------------------------------------------------------------
!--------------- Initialize mixfrac_m and set pure stream compositions ---------------

  call allocate_mixfrac_arrays( 1 )

!-------------------------------------------------------------------------------------
!----------------------------------- Set PRESSURE ------------------------------------

  pressure = press      ! uniform pressure throughout the domain

!-------------------------------------------------------------------------------------
!------------------------------- Set MIXTURE FRACTION --------------------------------
  select case (trim(scalarProfile))
  
  case ('spectrum')     ! initialize the mixture fraction from a spectrum 

     if(myid==0) then
        write(io,*)''
        write(io,*)'Setting mixture fraction profile - SPECTRUM'
     endif
     if (numdim == 1) then
        do i=1,nx
           mixfrac(i,1,1) = real( (myid*nx)+i  -1) / real(nx_g-1)
        enddo
     elseif (numdim == 2) then
        call rndini2d_mix( seed, ke_mix, f_mean, f_rms, io, mixfrac(:,:,1))
     elseif (numdim == 3) then
        call rndini3d_mix( seed, ke_mix, f_mean, f_rms, io, mixfrac )
     endif

  case default

     if(myid==0) then
        write(io,*)''
        write(io,*)'Setting mixture fraction profile - UNIFORM at mean mix. frac.'
     endif
     mixfrac = f_mean

  end select
!------------------------------ END set mixture fraction -----------------------------

!    call MPI_Barrier(gcomm,ierr)

!-------------------------------------------------------------------------------------
!------------------------------ Set SPECIES - UNREACTED ------------------------------

!  calculate stoichiometric mixture fraction and write to screen

  call getStoichMixfr
  if(myid.eq.0) then
     write(io,*) 'stoichiometric mixture fraction = ',stoichMixfr
     write(io,*)
  endif

! calculate species mass fractions based on the local mixture fraction
! gbansal/ for premixed HCCI case, just directly give species
  call mixfrToSpec( yspecies )
  
!  yspecies=0.0
!    do L = 1,nsc+1,1
!     if(trim(species_name(L)).eq.'O2') yspecies(:,:,:,L)=0.2255
!     if(trim(species_name(L)).eq.'N2') yspecies(:,:,:,L)=0.7421
!     if(trim(species_name(L)).eq.'CH3OCH3') yspecies(:,:,:,L)=0.0324
!  enddo



!----------------------------- END set species - unreacted ---------------------------


!-----------------------------------------------------------------------------------------
!--------------------------------- Set Temperature ---------------------------------------
  select case (trim(tempProfile))
!  
  case ('spectrum')     ! initialize the mixture fraction from a spectrum 
!
     if(myid==0) then
        write(io,*)''
        write(io,*)'Setting temperature profile - SPECTRUM'
     endif
     if (numdim == 1) then
          if (myid == 0) then
					   write(io,*) 'Dont know what to do for 1-D, switching to uniform temperature'
					end if
          temp = t_mean
     elseif (numdim == 2) then
        call rndini2d_temp( seedt, ke_temp, t_mean, t_rms, t_gamp, io, temp(:,:,1))
     elseif (numdim == 3) then
        call rndini3d_temp( seedt, ke_temp, t_mean, t_rms, t_gamp, io, temp )
! gbansal - t_gamp is not written yet for 3D
     endif
!
  case default
!
     if(myid==0) then
        write(io,*)''
        write(io,*)'Setting temperature profile - UNIFORM at mean temperature'
     endif
     temp = t_mean
  end select
!-------------------------------- END set Temperature ------------------------------------


!    call MPI_Barrier(gcomm,ierr)
!-----------------------------------------------------------------------------------------
!--------------------------------- Set Mean Velocities -----------------------------------

  u(:,:,:,1) = 0.0/a_ref        ! mean u-velocity set to zero
  u(:,:,:,2) = 0.0/a_ref        ! mean v-velocity set to zero
  u(:,:,:,3) = 0.0/a_ref        ! mean w-velocity set to zero

!-------------------------------- END set mean velocities --------------------------------


! deallocate mixture fraction module memory - don't do this if you need the info later!

!  call allocate_mixfrac_arrays( -1 )

  return
end subroutine initialize_hcci

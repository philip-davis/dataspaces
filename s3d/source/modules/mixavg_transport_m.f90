#include "globalDefines.h"
module transport_m

  !---------------------------------------------------------------------------------------
  !                               'transport_m'
  !                   Sean Smith and James Sutherland, June 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! This module provides tools to compute the diffusive terms in the Navier-
  ! Stokes equations for multicomponent flow.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !**************** ALL ARGUMENTS MUST BE PASSED IN CGS UNITS ****************************
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! USAGE:
  !     1. Call "initialize_transport" routine once to set up the module.
  !        This is the main usage, it will return every thing that is routinely needed.
  !        The other steps are for specific retrieval.
  !
  !     2. Call "computeCoefficients" routine to set mu, lambda
  !        This MUST be called BEFORE the remainder of the routines...
  !
  !     3. Call "computeSpeciesDiffFlux" routine to set diffusion velocities.
  !        This MUST be called BEFORE computeing the heat flux.
  !
  !     4. Call "computeStressTensor" routine to set stress tensor.
  !
  !     5. Call "computeHeatFlux" routine to set heat flux vector.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               mu*Cp                 lambda                   mu
  !         Pr = --------      Le_i = ------------     Sc_i = ---------
  !               lambda               rho*Cp*D_i              rho*D_i
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! All of the variables on the right hand side of the equations above are 
  ! created in this module except rho and Cp which are passed in. Then the
  ! dimensionless variables above along with the the diffusive flux terms are each
  ! calculated individually.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! DEPENDANCIES:
  !    param_m     - module specifying many parameters for S3D.  The only
  !                  information required from this module is the physical
  !                  grid size [nx,ny,nz], and the number of species [n_spec]
  !
  !    topology_m  - module containing parallelization information.  This is
  !                  only required in the initialization routine.
  !
  !    thermchem_m - module containing thermodynamic data.
  !
  !    chemkin_m   - module containing chemkin information.
  !
  !    grid_m      - module containing information about the grid spacing. This is
  !                  only required in computeSpeciesDiffFlux in oreder to get gradients.
  !
  !    rk_m        - module containing information about the Runge_Kutta method. This is
  !                  only used in getDiffusiveFluxTerms in order to lag the coefficients.
  !
  !---------------------------------------------------------------------------------------

  implicit none

  private

  real, public :: Pr ! The Prandtl number is not used in this module, but must be
               ! contained here in order for this module to be compatable with the code.

  ! Switch values are defined in mixavg.in:
  logical :: baro_switch          ! Switch indicates if baro-diffusion is calculated
  logical :: thermDiff_switch     ! Switch indicates if thermal diffusion is calculated
  logical, public :: lagging_switch      ! indicates if transport coefficients are lagged
  integer, public :: lag_steps           ! indicates the number of time-steps to lag

  integer, public :: flux_version_johnmc ! indicates which flux computation to use: 0 is usual other is johnmc's
  integer, public :: heatflux_version_johnmc ! indicates which heatflux computation to use
  integer, public :: mcavis_version_johnmc ! indicates which heatflux computation to use
                                                          !MIXTURE AVERAGE:

  real, allocatable, dimension(:,:,:)   :: viscosity      ! viscosity
  real, allocatable, dimension(:,:,:)   :: lambda         ! thermal conductivity
  real, allocatable, dimension(:,:,:,:) :: Ds_mixavg      ! diffususion coefficients
  real, allocatable, dimension(:,:,:,:) :: Rs_therm_diff  ! thermal diffusion ratios
  real :: rdummy

  ! Transport working arrays (it may be important to make these public later):
  integer :: limcwrk                      ! length of the transport integer work array
  integer :: lrmcwrk                      ! length of the transport real    work array
  integer, allocatable, dimension(:) :: imcwrk          ! transport integer work array
  real,    allocatable, dimension(:) :: rmcwrk          ! transport real    work array

  ! Local flag:
  logical, public :: initialized_trans = .false.


  ! Public routines:
  public :: initialize_transport       ! initialize the transport module
  public :: computeCoefficients        ! set thermal conductivity and viscosity
  public :: computeSpeciesDiffFlux     ! calculate the species diffusion velocities
  public :: computeStressTensor        ! calculate the stress tensor
  public :: computeHeatFlux            ! calculate the heat flux vector
  public :: allocate_transport_arrays  ! allocate/deallocate viscosity,lambda,diffusion
                                       ! coefficients and the transport work arrays

  public :: getViscosity           ! access function. Returns viscosity in CGS units.
  public :: getThermalConductivity ! access function. Returns lambda in CGS units.
  public :: getDiffusionCoeff      ! access function. diffusion coefficients in CGS units.
  public :: getLewis               ! function. Returns Le_i's, from rho, Temp, Ys in cgs.
  public :: getSchmidt             ! function. Returns Sc_i's, given rho in cgs units.
  public :: getPrandtl             ! function. Returns Pr, given Temp and Ys in cgs units.


!!$=======================================================================================


contains


!!$=======================================================================================


  function getViscosity()
    use param_m,  only : nx,ny,nz
    real, dimension(nx,ny,nz) :: getViscosity
    getViscosity = viscosity
    return
  end function getViscosity


!!$=======================================================================================


  function getThermalConductivity()
    use param_m,  only : nx,ny,nz
    real, dimension(nx,ny,nz) :: getThermalConductivity
    getThermalConductivity = lambda
    return
  end function getThermalConductivity


!!$=======================================================================================


  function getDiffusionCoeff()
    use param_m, only : nx,ny,nz,n_spec
    real, dimension(nx,ny,nz,n_spec) :: getDiffusionCoeff
    getDiffusionCoeff = Ds_mixavg
    return
  end function getDiffusionCoeff


!!$=======================================================================================


  function getLewis( Temp, Ys )
    use param_m,  only : nx,ny,nz,n_spec
    use chemkin_m,  only : ickwrk, rckwrk
    use reference_m, only: cp_ref

!    real, dimension(nx,ny,nz),        intent(in) :: rho      ! Density (in cgs units)
    real, dimension(nx,ny,nz),        intent(in) :: Temp     ! Temperature (in cgs units)
    real, dimension(nx,ny,nz,n_spec), intent(in) :: Ys       ! Species mass fractions
    real, dimension(nx,ny,nz)        :: Cp    ! Constant pressure heat capacity (per mass)
    real, dimension(nx,ny,nz,n_spec) :: getLewis   ! Lewis numbers ( lambda/rho*Cp*Ds )
    real, dimension(nx,ny,nz)        :: tmp        ! temporary array
    integer :: i,j,k

    ! Get Cp from chemkin using Temp and Ys:
    do k = 1,nz
       do j = 1,ny
          do i = 1,nx
             call ckcpbs(Temp(i,j,k), Ys(i,j,k,:), ickwrk, rckwrk, Cp(i,j,k))
          enddo
       enddo
    enddo

    !Changed by Ramanan -01/24/05
    !Ds_mixavg now stores \rho*D. 
    ! Added by ramanan - 02/10/05. Cp is in CGS, while everything else is non-dimensional.
    Cp = Cp*1e-4/cp_ref
    ! Calculate Lewis numbers:
    tmp = lambda / Cp
    !tmp = lambda / (rho * Cp)
    do i = 1,n_spec
       getLewis(:,:,:,i) =  tmp /  Ds_mixavg(:,:,:,i)
    enddo

    return
  end function getLewis


!!$=======================================================================================


  function getSchmidt( )
    use param_m,  only : nx,ny,nz,n_spec

!    real, intent(in), dimension(nx,ny,nz) :: rho      ! Density in cgs units
    real    :: tmp(nx,ny,nz), getSchmidt(nx,ny,nz,n_spec)
    integer :: i

    !Changed by Ramanan -01/24/05
    !Ds_mixavg now stores \rho*D. 

    ! Calculate Schmidt number:
    !tmp = viscosity / rho

    do i = 1,n_spec
       getSchmidt(:,:,:,i) =  viscosity /  Ds_mixavg(:,:,:,i)
    enddo

    return
  end function getSchmidt


!!$=======================================================================================


  function getPrandtl( Temp, Ys )
    use param_m,  only: nx,ny,nz,n_spec
    use chemkin_m,  only : ickwrk, rckwrk
    use reference_m, only: cp_ref

    real, intent(in), dimension(nx,ny,nz)        :: Temp       ! Temperature in cgs units.
    real, intent(in), dimension(nx,ny,nz,n_spec) :: Ys         ! Mass fractions
    real, dimension(nx,ny,nz) :: Cp, getPrandtl
    integer :: i,j,k

    ! Get Cp from chemkin using Temp and Ys:
    do k = 1,nz
       do j = 1,ny
          do i = 1,nx
             call ckcpbs(Temp(i,j,k), Ys(i,j,k,:), ickwrk, rckwrk, Cp(i,j,k))
          enddo
       enddo
    enddo

    ! Added by ramanan - 02/10/05. Cp is in CGS, while everything else is non-dimensional.
    cp = cp*1e-4/cp_ref

    ! Calculate Prandtl number:
    getPrandtl = ( Cp * viscosity ) / lambda

  end function getPrandtl


!!$=======================================================================================


  subroutine initialize_transport( io )

    !--------------------------------------------------------------------------------
    ! This routine initializes the transport module.
    !--------------------------------------------------------------------------------

    use topology_m,  only: myid, ierr, gcomm

    implicit none

    include 'mpif.h'

    integer, intent(in) :: io   ! output unit number (it is open and ready to print to)
    character(len=100)  :: filename
    integer, parameter :: linkmc=22          ! file input/tran.asc unit number
    integer :: lout             ! file output unit number
    integer :: iflag1, iflag2   ! transport error flags

    ! Chemkin transport variables:
    real    :: RU, PATMOS, SMALL
    integer :: NKK, NO, NLITE, INLIN, IKTDIF,                             &
               IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,            &
               NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,             &
               NCINT, NBIND, NEOK, NSGM, NAST, NBST, NCST, NXL,           &
               NR, NWRK, K3
    real    :: VERS, PREC
    integer :: KERR, LENI, LENR
    COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF,          &
                    IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,            &
                    NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,             &
                    NCINT, NBIND, NEOK, NSGM, NAST, NBST, NCST, NXL,           &
                    NR, NWRK, K3
    COMMON /MCCONS/ VERS, PREC, KERR, LENI, LENR


    ! Write header for transport initialzation:
    if (myid == 0) then
       write(unit=io,fmt=*) 'initializing transport module...'
       write(unit=io,fmt=*) ''
    endif

    !`````````````````````````````````````````````````````````````````````````````````````
    !! Read mixture averaged input file !!

    ! Check that input/mixavg.in exists:
    filename = '../input/mixavg.in'
    call inquire_about_input_file(filename, io)


    if (myid == 0) then
       write(unit=io,fmt=*) 'opening mixavg.in ...'
       open(unit=21, file=filename, status='old', form='formatted')

       read(unit=21,fmt=*)
       read(unit=21,fmt=*) baro_switch
       read(unit=21,fmt=*) thermDiff_switch
       read(unit=21,fmt=*)
       read(unit=21,fmt=*) lagging_switch
       read(unit=21,fmt=*) lag_steps
       read(unit=21,fmt=*) flux_version_johnmc 
       read(unit=21,fmt=*) heatflux_version_johnmc
       read(unit=21,fmt=*) mcavis_version_johnmc

       close(unit=21)

     ! Warning message from Evatt
       if(lagging_switch)then
         write(io,*) 'Lagging is switched on' 
         write(io,*) 'WARNING - with coefficient lagging, theoretical'
         write(io,*) 'temporal convergence order may not be observed'
         write(io,*) 
       endif

    endif

    call MPI_Bcast(baro_switch,      1, MPI_LOGICAL, 0, gcomm, ierr)
    call MPI_Bcast(thermDiff_switch, 1, MPI_LOGICAL, 0, gcomm, ierr)
    call MPI_Bcast(lagging_switch,   1, MPI_LOGICAL, 0, gcomm, ierr)
    call MPI_Bcast(lag_steps,        1, MPI_INTEGER, 0, gcomm, ierr)
    call MPI_Bcast(flux_version_johnmc,        1, MPI_INTEGER, 0, gcomm, ierr)
    call MPI_Bcast(heatflux_version_johnmc,    1, MPI_INTEGER, 0, gcomm, ierr)
    call MPI_Bcast(mcavis_version_johnmc,      1, MPI_INTEGER, 0, gcomm, ierr)

    !`````````````````````````````````````````````````````````````````````````````````````


    !`````````````````````````````````````````````````````````````````````````````````````
    !! Initialize chemkin transport !!

    ! lout is the variable name common to chemkin, its value is 'io'
    lout=io

    ! Check that input/tran.asc exists:
    filename = '../input/tran.asc'
    call inquire_about_input_file(filename, io)

    ! Initialize transport arrays on  myid = 0
    if (myid == 0) then
       ! Write header that tran.asc is being opened and open it:
       write(unit=io,fmt=*) 'opening transport link file...'
       open(unit=linkmc, status='old', file=filename, form='formatted')

       ! Write header for lengths of the transport working arrays and find them:
       write(unit=io,fmt=*) 'setting lengths of transport work arrays...'
       call mclen(linkmc, lout, limcwrk, lrmcwrk, iflag1)

       ! Check for error
       if (iflag1 > 0) then
          write(unit=io,fmt=*) 'error setting lengths of transport work arrays'
          write(unit=io,fmt=*) 'iflag=', iflag1
          write(unit=io,fmt=*) 'see routine mclen and initialze_transport'
       endif

    endif

    call MPI_Bcast(iflag1,1,MPI_INTEGER,0,gcomm,ierr)

    ! If there was an error terminate the run on all processors:
    if (iflag1 > 0) call terminate_run(io,0)


    ! Brodcast the lengths of the transport work arrays:
    call MPI_Bcast(limcwrk,1,MPI_INTEGER,0,gcomm,ierr)
    call MPI_Bcast(lrmcwrk,1,MPI_INTEGER,0,gcomm,ierr)


    ! Allocate space for the transport working arrays with header:
    if(myid == 0) then
       write(unit=io,fmt=*) 'allocating transport work arrays...'
    endif
    call allocate_transport_arrays(1)

    ! Create transport work arrays (with header) on one processor:
    if (myid == 0) then
       write(unit=io,fmt=*) 'creating transport work arrays from transport link file...'
       call mcinit(linkmc, lout, limcwrk, lrmcwrk, imcwrk, rmcwrk, iflag2)

       ! Check for error
       if (iflag2 > 0) then
          write(unit=io,fmt=*) 'error creating transport work arrays'
          write(unit=io,fmt=*) 'iflag=', iflag2
          write(unit=io,fmt=*) 'see routine mcinit and initialze_transport'
       endif

       ! close the link file (../input/tran.asc):
       close(linkmc)

       call write_header(io,'-')

    endif

    ! Broadcast transport library COMMON blocks:
    call MPI_Bcast(RU,  3, MPI_REAL8,  0,gcomm,ierr)
    call MPI_Bcast(NKK, 32,MPI_INTEGER,0,gcomm,ierr)
    call MPI_Bcast(VERS,2, MPI_REAL8,  0,gcomm,ierr)
    call MPI_Bcast(KERR,3, MPI_INTEGER,0,gcomm,ierr)

    ! Broadcast error flag
    call MPI_Bcast(iflag2,1,MPI_INTEGER,0,gcomm,ierr)

    ! Brodcast the transport work arrays:
    call MPI_Bcast(imcwrk,limcwrk,MPI_INTEGER,0,gcomm,ierr)
    call MPI_Bcast(rmcwrk,lrmcwrk,MPI_REAL8,0,gcomm,ierr)

    ! If there was an error terminate the run on all processors:
    if (iflag2 > 0) call terminate_run(io,0)

    !`````````````````````````````````````````````````````````````````````````````````````


    ! Indicate that trasport has been initialized:
    initialized_trans = .true.

  end subroutine initialize_transport


!!$=======================================================================================


  subroutine allocate_transport_arrays( flag )

    use param_m,  only: nx,ny,nz,n_spec

    implicit none

    integer, intent(in) :: flag

    if (flag == 1) then

       allocate(viscosity(nx,ny,nz))
       allocate(lambda(nx,ny,nz))
       allocate(Ds_mixavg(nx,ny,nz,n_spec))
       if (thermDiff_switch) then
          allocate(Rs_therm_diff(nx,ny,nz,n_spec))
       endif

       allocate(imcwrk(limcwrk))
       allocate(rmcwrk(lrmcwrk))

    elseif (flag == -1 ) then

       deallocate(viscosity)
       deallocate(lambda)
       deallocate(Ds_mixavg)
       if (thermDiff_switch) then
          deallocate(Rs_therm_diff)
       endif

       deallocate(imcwrk)
       deallocate(rmcwrk)

    endif

  end subroutine allocate_transport_arrays


!!$=======================================================================================

#ifdef VECTORVERSION

!Evatt Hawkes 23-SEP-2005
!new subroutine for vector arch.
!pulled just the ij loops inside the routines mcavis, mcacon and mcadif as a compromise
!between full grid loop vector operations and excessive memory.  macavis and mcacon 
!can be fully vectorized without much additional memeory.  however mcadif contains 
!kk*kk arrays which can be big.
!
!actually also we save some flops here.  worth trying for non-vector arch too.
!

subroutine computeCoefficients( press, temp, Ys, rho )

    
    !--------------------------------------------------------------------------------
    ! Calculate thermal conductivity, viscosity and diffusion coefficients at each
    ! grid point.
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! These values are retrived from chemkin-transport.
    !--------------------------------------------------------------------------------
    
    ! Use of mixMW and molwt in the chemkin module must be made possible.
    use chemkin_m,    only: molwt_c         ! Inverse of the molecular weights
    use thermchem_m,  only: mixMW           ! Mixture average molecular weight
    use param_m,      only: nx, ny, nz, n_spec
    use reference_m,  only: rho_ref, a_ref, l_ref, t_ref, p_ref

    real, intent(in) :: Press(nx,ny,nz)     ! Pressure
    real, intent(in) :: Temp(nx,ny,nz)      ! Temperature
    real, intent(in) :: Ys(nx,ny,nz,n_spec) ! Species mass fractions
    real, dimension(nx,ny,nz),        intent(in) :: rho      ! Density
    real :: Xs(nx,ny,nz,n_spec)              ! Species mole fractions
    integer :: i, j, k, m
    real inv_lam_ref, inv_vis_ref, inv_dif_ref, my_p_ref  !reference quantities    
    real :: this_temp(nx,ny,nz), this_pres(nx,ny,nz)  !dimensional temp. and press.
       

    !additions by Ramanan Sankaran 01/04/05
    inv_lam_ref = rho_ref*a_ref*a_ref*a_ref*l_ref/t_ref
    inv_lam_ref = 1e-5/inv_lam_ref  !1e-5 for converting from CGS to SI

    inv_vis_ref = rho_ref*a_ref*l_ref
    inv_vis_ref = 0.1/inv_vis_ref   !0.1 for converting from CGS to SI

    inv_dif_ref = a_ref*l_ref
    inv_dif_ref = 1e-4/inv_dif_ref  !1e-4 for converting from CGS to SI

    my_p_ref = p_ref*10.0 ! 10.0 for CGS units

    ! Zeroeing these arrays seems unnecessary - Ramanan 01/04/05
    !lambda    = 0.0
    !viscosity = 0.0
    !Ds_mixavg = 0.0

!   if we wanted we could take the log here of t and pass that.
    this_temp(:,:,:) = temp(:,:,:)*t_ref
    this_pres(:,:,:) = press(:,:,:)*my_p_ref

!   convert from mass fraction to mole fraction
    do m = 1,n_spec
      Xs(:,:,:,m) = mixMW(:,:,:) * Ys(:,:,:,m) * molwt_c(m)
    enddo

    do k = 1,nz
      call mcacon_allij(this_temp(:,:,k), Xs(:,:,k,:), rmcwrk, lambda(:,:,k))
    enddo         
             
    do k = 1,nz
      call mcavis_new_allij(this_temp(:,:,k), Xs(:,:,k,:), rmcwrk, viscosity(:,:,k))
    enddo         

    do k = 1,nz
      call mcadif_allij(this_pres(:,:,k), this_temp(:,:,k), Xs(:,:,k,:), rmcwrk, Ds_mixavg(:,:,k,:))
    enddo         
 
!   this loop is not vectorized 
    do k = 1,nz
       do j = 1,ny
          do i = 1,nx

             ! Retrive the thermal diffusion ratios (for the Soret affect):
             if (thermDiff_switch) then
                call mcatdr(this_temp(i,j,k), Xs(i,j,k,:), imcwrk, rmcwrk, Rs_therm_diff(i,j,k,:))
             endif

          enddo
       enddo
    enddo
    
    lambda = lambda*inv_lam_ref
    viscosity = viscosity*inv_vis_ref
    Ds_mixavg = Ds_mixavg*inv_dif_ref

    !Added by ramanan - 01/24/05
    ! Ds_mixavg is now \rho*D
    do i = 1, n_spec
      Ds_mixavg(:,:,:,i) = Ds_mixavg(:,:,:,i)*rho(:,:,:)
    end do

    !thermal diffusion is a ratio - no need to worry about units

    return
  end subroutine computeCoefficients
#else
  subroutine computeCoefficients( press, temp, Ys, rho )

    !--------------------------------------------------------------------------------
    ! Calculate thermal conductivity, viscosity and diffusion coefficients at each
    ! grid point.
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! These values are retrived from chemkin-transport.
    !--------------------------------------------------------------------------------
    
    ! Use of mixMW and molwt in the chemkin module must be made possible.
    use chemkin_m,    only: molwt_c         ! Inverse of the molecular weights
    use thermchem_m,  only: mixMW           ! Mixture average molecular weight
    use param_m,      only: nx, ny, nz, n_spec
    use reference_m,  only: rho_ref, a_ref, l_ref, t_ref, p_ref

    real, intent(in) :: Press(nx,ny,nz)     ! Pressure
    real, intent(in) :: Temp(nx,ny,nz)      ! Temperature
    real, intent(in) :: Ys(nx,ny,nz,n_spec) ! Species mass fractions
    real, dimension(nx,ny,nz),        intent(in) :: rho      ! Density
    real    :: Xs(n_spec)                   ! Species mole fractions
    integer :: i, j, k, m
    real inv_lam_ref, inv_vis_ref, inv_dif_ref, my_p_ref  !reference quantities
    real this_temp, this_pres

    !additions by Ramanan Sankaran 01/04/05
    inv_lam_ref = rho_ref*a_ref*a_ref*a_ref*l_ref/t_ref
    inv_lam_ref = 1e-5/inv_lam_ref  !1e-5 for converting from CGS to SI

    inv_vis_ref = rho_ref*a_ref*l_ref
    inv_vis_ref = 0.1/inv_vis_ref   !0.1 for converting from CGS to SI

    inv_dif_ref = a_ref*l_ref
    inv_dif_ref = 1e-4/inv_dif_ref  !1e-4 for converting from CGS to SI

    my_p_ref = p_ref*10.0 ! 10.0 for CGS units

    ! Zeroeing these arrays seems unnecessary - Ramanan 01/04/05
    !lambda    = 0.0
    !viscosity = 0.0
    !Ds_mixavg = 0.0

    do k = 1,nz
       do j = 1,ny
          do i = 1,nx

             this_temp = Temp(i,j,k)*t_ref
             this_pres = Press(i,j,k)*my_p_ref

             ! Convert from mass fraction to mole fraction
             do m = 1,n_spec
                Xs(m) = mixMW(i,j,k) * Ys(i,j,k,m) * molwt_c(m)
             enddo

             ! Retrieve the thermal conductivity from chemkin transport:
             call mcacon(this_temp, Xs(:), rmcwrk, lambda(i,j,k))

             ! Retrieve the viscosity from chemkin transport:
             call mcavis_new(this_temp, Xs(:), rmcwrk, viscosity(i,j,k))

             ! Retrieve the diffusion coefficients from chemkin transport:
             call mcadif(this_pres, this_temp, Xs, rmcwrk, Ds_mixavg(i,j,k,:))

             ! Retrive the thermal diffusion ratios (for the Soret affect):
             if (thermDiff_switch) then
                call mcatdr(this_temp, Xs, imcwrk, rmcwrk, Rs_therm_diff(i,j,k,:))
             endif

          enddo
       enddo
    enddo
    lambda = lambda*inv_lam_ref
    viscosity = viscosity*inv_vis_ref
    Ds_mixavg = Ds_mixavg*inv_dif_ref

    !Added by ramanan - 01/24/05
    ! Ds_mixavg is now \rho*D
    do i = 1, n_spec
      Ds_mixavg(:,:,:,i) = Ds_mixavg(:,:,:,i)*rho(:,:,:)
    end do

    !thermal diffusion is a ratio - no need to worry about units

    return
  end subroutine computeCoefficients
#endif  



!!$=======================================================================================

!----------------------------------------------------------------------
! Notes by Ramanan Sankaran - 01/13/05
! The gradients being computed in this routine needed to be non-dimensionalized
! by the CGS length scale
! The unit of molecular weight does not matter because it appears as a ratio W/\bar{W}
!----------------------------------------------------------------------

  subroutine computeSpeciesDiffFlux( Temp, grad_T, Ys, grad_Ys, Press, rho)
!Changes 
! - Ramanan Sankaran 01/05/05
! Rather than store diffflux in a separate array, 
! the modified version will overwrite grad_Ys
!  Just so the code is readable, i have a pointer diffFlux => grad_Ys
!  But remember that both refer to the same memory location

    !--------------------------------------------------------------------------------
    ! J_i = -D * rho * dY/dx_i
    !
    ! NOTE that the diffusion fluxes of all species must sum to zero.
    !      To ensure this, we compute the diffusion flux of the last
    !      species using this constraint.
    !--------------------------------------------------------------------------------

    use param_m,      only: nx, ny, nz, n_spec, vary_in_x, vary_in_y, vary_in_z
    use chemkin_m,    only: molwt
    use thermchem_m,  only: avmolwt, mixMW
    use grid_m,       only: scale_1x, scale_1y, scale_1z
    use reference_m,  only: l_ref

    real, intent(in),  dimension(nx,ny,nz)          :: Temp
    real, intent(in),  dimension(nx,ny,nz,3)        :: grad_T
    real, intent(in),  dimension(nx,ny,nz,n_spec)   :: Ys
    real, intent(inout),  target, dimension(nx,ny,nz,n_spec,3) :: grad_Ys
    real, intent(in),  dimension(nx,ny,nz)          :: Press
    real, intent(in),  dimension(nx,ny,nz)          :: rho
    real, pointer, dimension(:,:,:,:,:) :: diffFlux

    real, dimension(nx,ny,nz,3)           :: grad_mixMW
    real, allocatable, dimension(:,:,:,:) :: grad_P
    integer :: m,n

    diffFlux => grad_Ys

    ! The driving force for diffusion is not mass fraction, but mole fraction.
    ! Since the gradient of the mass fraction, grad_Ys, is already known, the expresion
    ! was manipulated so only the gradient of the mixture molecular weight, grad_mixMW,
    ! needs to be calculated, and not the gradient of the mole fraction, grad_Xs, for
    ! each species.

    ! compute grad_mixMW
    grad_mixMW = 0.0
    if (vary_in_x == 1) then
       call derivative_x( nx,ny,nz, mixMW, grad_mixMW(:,:,:,1), scale_1x, 1 )
    endif
    if (vary_in_y == 1) then
       call derivative_y( nx,ny,nz, mixMW, grad_mixMW(:,:,:,2), scale_1y, 1 )
    endif
    if (vary_in_z == 1) then
       call derivative_z( nx,ny,nz, mixMW, grad_mixMW(:,:,:,3), scale_1z, 1 )
    endif

    ! Change by ramanan - 01/24/05
    ! multiplying this here, just once, avoids doing it n_spec times 
    ! inside the SPECIES loop.
    do m = 1,3
     grad_mixMW(:,:,:,m) = grad_mixMW(:,:,:,m)*avmolwt(:,:,:)
    end do

    ! compute grad_P
    if (baro_switch) then
       allocate(grad_P(nx,ny,nz,3))
       grad_P = 0.0
       if (vary_in_x == 1) then
          call derivative_x( nx,ny,nz, Press, grad_P(:,:,:,1), scale_1x, 1 )
       endif
       if (vary_in_y == 1) then
          call derivative_y( nx,ny,nz, Press, grad_P(:,:,:,2), scale_1y, 1 )
       endif
       if (vary_in_z == 1) then
          call derivative_z( nx,ny,nz, Press, grad_P(:,:,:,3), scale_1z, 1 )
       endif
    endif

    ! Changed by Ramanan - 01/24/05
    ! Ds_mixavg is now \rho*D 
    !
    !grad_P/press and avmolwt*grad_T/Temp can be optimized by division before the loop.
    ! compute diffusive flux for species n in direction m.
    diffFlux(:,:,:,n_spec,:) = 0.0


 if(flux_version_johnmc == 0) then
    DIRECTION: do m=1,3
       SPECIES: do n=1,n_spec-1

          if (baro_switch) then
             ! driving force includes gradient in mole fraction and baro-diffusion:
             diffFlux(:,:,:,n,m) = - Ds_mixavg(:,:,:,n) * ( grad_Ys(:,:,:,n,m)  &
                                   + Ys(:,:,:,n) * ( grad_mixMW(:,:,:,m)    &
                                   + (1 - molwt(n)*avmolwt) * grad_P(:,:,:,m)/Press))
          else
             ! driving force is just the gradient in mole fraction:
             diffFlux(:,:,:,n,m) = - Ds_mixavg(:,:,:,n) * ( grad_Ys(:,:,:,n,m)  &
                                   + Ys(:,:,:,n) * grad_mixMW(:,:,:,m) )
          endif

          ! Add thermal diffusion:
          if (thermDiff_switch) then
             diffFlux(:,:,:,n,m) = diffFlux(:,:,:,n,m)    &
                  - Ds_mixavg(:,:,:,n) * Rs_therm_diff(:,:,:,n) * molwt(n) &
                  * avmolwt * grad_T(:,:,:,m) / Temp
          endif

          ! compute contribution to nth species diffusive flux
          ! this will ensure that the sum of the diffusive fluxes is zero.
          diffFlux(:,:,:,n_spec,m) = diffFlux(:,:,:,n_spec,m) - diffFlux(:,:,:,n,m)

       enddo SPECIES
    enddo DIRECTION

  else
        call diffflux_proc_looptool(nx, ny, nz, n_spec, baro_switch, thermDiff_switch,  &
             diffFlux, grad_Ys, grad_mixMW, Ys, rdummy, Press, Ds_mixavg, &
             avmolwt, molwt, rdummy, grad_T, Temp)     
  endif

    if (baro_switch) then
       deallocate(grad_P)
    endif

    return
  end subroutine computeSpeciesDiffFlux

!!$=======================================================================================


  subroutine computeStressTensor( grad_u)

!----------------------------------------------------------------------
!Rewritten by Ramanan Sankaran - 01/05/05
!  To save memory this function was rewritten.
!  Now Tau is returned overwriting grad_u
!  Just so the code is readable, i have a pointer tau=>grad_u
!  But remember that , now, both are the same memory locations
!  That is why we need to momentarily store as grad_u_tmp
!----------------------------------------------------------------------
!
    !--------------------------------------------------------------------------------
    ! given the velocity gradient tensor, this routine computes the stress tensor
    ! for a newtonian fluid
    !
    ! In index notation, the stress tensor is written as:
    !
    !  tau_ij = mu [ d(u_i)/dx_j + d(u_j)/dx_i ]
    !         - del_ij [ 2/3(mu-K) * d(u_k)/dx_k ]
    !
    ! where:
    !    mu  =  viscosity
    !    K   =  bulk viscosity (ASSUMED TO BE ZERO HERE - true for monatomic gases)
    !    del_ij = Kronicker delta (1 if i=j, else 0)
    !--------------------------------------------------------------------------------

    use param_m,  only: nx,ny,nz

    real, intent(inout),  target, dimension(nx,ny,nz,3,3) :: grad_u
    real, pointer, dimension(:,:,:,:,:) :: tau
#ifdef VECTORVERSION
    real grad_u_tmp(nx,ny,nz,3,3)
    real sumterm(nx, ny, nz)
#else
    real grad_u_tmp(3,3)
    real sumterm
#endif
    integer :: i,j,k,m,n

    tau => grad_u

#ifdef VECTORVERSION
    grad_u_tmp = grad_u

    sumterm = 0.0
    do n = 1, 3
      sumterm(:,:,:) = sumterm(:,:,:) + grad_u_tmp(:,:,:,n,n)
    end do

    ROW: do m=1,3
       tau(:,:,:,m,m) = 2.0*viscosity(:,:,:) * (grad_u_tmp(:,:,:,m,m) - sumterm(:,:,:)/3.0)

       ! copmute the rest of the columns in this row, using
       ! symmetry for elements to the left of the diagonal.
       ! really we don't need to store the whole thing, but 
       ! this makes life much easier...
       COLUMN: do n=m+1,3
          tau(:,:,:,m,n) = viscosity(:,:,:) * ( grad_u_tmp(:,:,:,m,n) + grad_u_tmp(:,:,:,n,m) )
          tau(:,:,:,n,m) = tau(:,:,:,m,n)
       enddo COLUMN
    enddo ROW
#else
    ! compute the diagonal element

    do k = 1, nz
      do j = 1, ny
         do i = 1, nx
           !Store the grad_u at this point - 
           !we will later overwrite in the original location
           grad_u_tmp(:,:) = grad_u(i,j,k,:,:)

           sumterm = 0.0
           do n=1,3
              sumterm = sumterm + grad_u_tmp(n,n)
           enddo

           ROW: do m=1,3
              tau(i,j,k,m,m) = 2.0*viscosity(i,j,k) * (grad_u_tmp(m,m) - sumterm/3.0)
       
              ! copmute the rest of the columns in this row, using
              ! symmetry for elements to the left of the diagonal.
              ! really we don't need to store the whole thing, but 
              ! this makes life much easier...
              COLUMN: do n=m+1,3
                 tau(i,j,k,m,n) = viscosity(i,j,k) * ( grad_u_tmp(m,n) + grad_u_tmp(n,m) )
                 tau(i,j,k,n,m) = tau(i,j,k,m,n)
              enddo COLUMN
           enddo ROW
        end do !i
      end do ! j
    end do !k
#endif

    return
  end subroutine computeStressTensor

!!$==================================================================================


!!$=======================================================================================


  subroutine computeHeatFlux( grad_T, enthalpy, diffFlux)
!Rewritten by Ramanan Sankaran - 01/05/05
!  To save memory this function was rewritten.
!  Now heatFlux is returned overwriting grad_T
!  Just so the code is readable, i have a pointer heatFlux=>grad_T
!  But remember that , now, both are the same memory locations
!----------------------------------------------------------------------


    !--------------------------------------------------------------------------------
    ! Compute the heat flux vector given by:
    !
    !    q_i = -lambda * dT/dx_i + SUM[ h_j*J_i,j ]
    !
    ! With
    !   mu      - viscosity
    !   lambda  - mixture thermal conductivity
    !   dT/dx_i - temperature gradient in direction i
    !   h_j     - enthalpy of species j
    !   J_i,j   - diffusion flux of species j in direction i
    !--------------------------------------------------------------------------------

    use param_m,  only: nx,ny,nz,n_spec

    real, intent(inout),  target, dimension(nx,ny,nz,3)        :: grad_T
    real, intent(in),  dimension(nx,ny,nz,n_spec)   :: enthalpy
    real, intent(in),  dimension(nx,ny,nz,n_spec,3) :: diffFlux
    real, pointer, dimension(:,:,:,:)        :: heatFlux

    integer :: m,n

    heatFlux => grad_T

    if (heatflux_version_johnmc == 0) then
    DIRECTION: do m=1,3
       heatFlux(:,:,:,m) = -lambda(:,:,:) * grad_T(:,:,:,m)

     ! add in the mass diffusion component.
       SPECIES: do n=1,n_spec
          heatFlux(:,:,:,m) = heatFlux(:,:,:,m) + enthalpy(:,:,:,n)*diffFlux(:,:,:,n,m)
       enddo SPECIES

    enddo DIRECTION
    else
      call computeHeatFlux_looptool( grad_T, enthalpy, diffFlux, &
       lambda, heatFlux, nx, ny, nz, n_spec)
    endif

    return
  end subroutine computeHeatFlux


!!$=======================================================================================

! New subroutine by Ramanan Sankaran - 01/24/05
! This routine is based on the mcavis from chemkin's transport library
! This routine was written for speed gains. 
! The old one had O(K^2) divisions and square roots.
! This new routine does O(K) operations and are stored in memory.
  subroutine mcavis_new(t, x, rmcwrk, vismix)
!
!  START PROLOGUE
!
!  SUBROUTINE MCAVIS (T, X, RMCWRK, VISMIX)
!  Returns mixture viscosity, given temperature and species mole
!  fractions.  It uses modification of the Wilke semi-empirical
!  formulas.
!
!  INPUT
!  T         - Real scalar, temperature.
!                 cgs units, K
!  X(*)      - Real array, mole fractions of the mixture;
!              dimension at least KK, the total species count.
!  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
!
!  OUTPUT
!  VISMIX    - Real scalar, mixture viscosity.
!                 cgs units, gm/cm*s
!
!  END PROLOGUE
!

  implicit none

  real zero, one, eight
  real RU, PATMOS, SMALL 
  integer NKK, NO, NLITE, INLIN, IKTDIF, &
       IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM, &
       NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT, &
       NCINT, NBIND, NEOK, NSGM, NAST, NBST, &
       NCST, NXL, NR, NWRK, K3

  PARAMETER (ZERO=0.0, ONE=1.0, EIGHT=8.0)
  COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF, &
                  IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM, &
                  NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT, &
                  NCINT, NBIND, NEOK, NSGM, NAST, NBST, &
                  NCST, NXL, NR, NWRK, K3
  real, intent(in):: X(*) 
  real, intent(inout):: RMCWRK(*)
  real, intent(in):: t 
  real, intent(out):: vismix

  !local declarations
  real, dimension(nkk) :: sqrt_eta, invsqrt_eta, quad_w, invquad_w
  real alogt, sumo, sumi
  integer i,j,k
  integer kstrt

  !In the following call, the species molecular weights are stored
  !in RMCWRK(NWT) and the pure species viscosities are in
  !RMCWRK(NVIS)

  if (mcavis_version_johnmc == 0) then
  ALOGT = LOG(T)
  IF (NO .EQ. 4) THEN
    CALL MCEVAL4 (ALOGT, NKK, RMCWRK(NETA), RMCWRK(NVIS))
  ELSE
    CALL MCEVAL (ALOGT, NKK, NO, RMCWRK(NETA), RMCWRK(NVIS))
  ENDIF

  DO K = 1, NKK
    sqrt_eta(k) = EXP(0.5*RMCWRK(NVIS+K-1))
    RMCWRK(NVIS+K-1) = sqrt_eta(k)*sqrt_eta(k)
    invsqrt_eta(k) = 1.0/sqrt_eta(k)
  END DO
  !Modified by Mark Fahey, ORNL
  !Fission the loop
  !This might be better on other platforms too.
  DO K = 1, NKK
    quad_w(k) = sqrt(sqrt(rmcwrk(nwt+k-1)))
    invquad_w(k) = 1.0/quad_w(k)
  END DO

  SUMO = ZERO
  DO K = 1, NKK

    SUMI = ZERO
    DO J = 1, NKK
       SUMI = SUMI + X(J) *   &
              (ONE + sqrt_eta(k)*invsqrt_eta(j)*quad_w(j)*invquad_w(k))**2 * &
              SQRT( RMCWRK(NWT+J-1)/ ( RMCWRK(NWT+J-1)+ RMCWRK(NWT+K-1) ) )
    END DO
    SUMO = SUMO + X(K) * RMCWRK(NVIS+K-1) / SUMI
  END DO

  VISMIX = SUMO * SQRT(EIGHT)
  else
    call mcavis_new_looptool(t, x, rmcwrk, vismix)
  endif

!     end of SUBROUTINE MCAVIS_new
      RETURN
      END subroutine mcavis_new
!----------------------------------------------------------------------
                                                                     


#ifdef VECTORVERSION
! New subroutines by Evatt Hawkes SEP-22-2005
!
! same as chemkin routines but pulling the i,j loops inside the routine
! for better vectorizing.

  subroutine mcavis_new_allij(t, x, rmcwrk, vismix)
!
!  START PROLOGUE
!
!  SUBROUTINE MCAVIS (T, X, RMCWRK, VISMIX)
!  Returns mixture viscosity, given temperature and species mole
!  fractions.  It uses modification of the Wilke semi-empirical
!  formulas.
!
!  INPUT
!  T         - Real scalar, temperature.
!                 cgs units, K
!  X(*)      - Real array, mole fractions of the mixture;
!              dimension at least KK, the total species count.
!  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
!
!  OUTPUT
!  VISMIX    - Real scalar, mixture viscosity.
!                 cgs units, gm/cm*s
!
!  END PROLOGUE
!
  use param_m, only : nx,ny,n_spec

  implicit none

  real zero, one, eight
  real RU, PATMOS, SMALL 
  integer NKK, NO, NLITE, INLIN, IKTDIF, &
       IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM, &
       NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT, &
       NCINT, NBIND, NEOK, NSGM, NAST, NBST, &
       NCST, NXL, NR, NWRK, K3

  PARAMETER (ZERO=0.0, ONE=1.0, EIGHT=8.0)
  COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF, &
                  IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM, &
                  NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT, &
                  NCINT, NBIND, NEOK, NSGM, NAST, NBST, &
                  NCST, NXL, NR, NWRK, K3
                  
  real, intent(in):: X(nx,ny,n_spec) 
  real, intent(inout):: RMCWRK(*)
  real, intent(in):: t(nx,ny) 
  real, intent(out):: vismix(nx,ny)

  !local declarations
  real, dimension(nx,ny,nkk) :: sqrt_eta, invsqrt_eta
  real, dimension(nkk) :: quad_w, invquad_w
  real, dimension(nx,ny,nkk) :: viscspec
  real alogt(nx,ny), sumo(nx,ny), sumi(nx,ny)
  integer i,j,k
  integer kstrt

  !In the following call, the species molecular weights are stored
  !in RMCWRK(NWT) and the pure species viscosities are in
  !RMCWRK(NVIS)

  ALOGT(:,:) = LOG(T(:,:))
  IF (NO .EQ. 4) THEN
    CALL MCEVAL4_allij (ALOGT(:,:), NKK, RMCWRK(NETA), viscspec(:,:,:))
  ELSE
    CALL MCEVAL_allij (ALOGT(:,:), NKK, NO, RMCWRK(NETA), viscspec(:,:,:))
  ENDIF

  sqrt_eta(:,:,:) = EXP(0.5*viscspec(:,:,:))
  viscspec(:,:,:)  = sqrt_eta(:,:,:)*sqrt_eta(:,:,:)
  invsqrt_eta(:,:,:) = 1.0/sqrt_eta(:,:,:)

  DO K = 1, NKK
    quad_w(k) = sqrt(sqrt(rmcwrk(nwt+k-1)))
    invquad_w(k) = 1.0/quad_w(k)
  END DO

  SUMO(:,:) = ZERO
  DO K = 1, NKK

    SUMI(:,:) = ZERO
    DO J = 1, NKK
       SUMI(:,:) = SUMI(:,:) + X(:,:,J) *   &
              (ONE + sqrt_eta(:,:,k)*invsqrt_eta(:,:,j)*quad_w(j)*invquad_w(k))**2 * &
              SQRT( RMCWRK(NWT+J-1)/ ( RMCWRK(NWT+J-1)+ RMCWRK(NWT+K-1) ) )
    END DO
    SUMO(:,:) = SUMO(:,:) + X(:,:,K) * viscspec(:,:,k) / SUMI(:,:)
  END DO

  VISMIX(:,:) = SUMO(:,:) * SQRT(EIGHT)

!     end of SUBROUTINE MCAVIS_new
      RETURN
      END subroutine mcavis_new_allij


!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
      SUBROUTINE MCACON_allij (T, X, RMCWRK, CONMIX)
!
!  START PROLOGUE
!
!  SUBROUTINE MCACON (T, X, RMCWRK, CONMIX)
!  Returns the mixture thermal conductivity given temperature and
!  species mole fractions.
!
!  INPUT
!  T         - Real scalar, temperature.
!                 cgs units, K
!  X(*)      - Real array, mole fractions of the mixture;
!              dimension at least KK, the total species count.
!  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
!
!  OUTPUT
!  CONMIX    - Real scalar, mixture thermal conductivity.
!                 cgs units, erg/cm*K*s
!
!  END PROLOGUE
!

  use param_m, only : nx,ny,n_spec
  implicit none

! parameters
  real zero, one
  PARAMETER (ZERO=0.0, ONE=1.0) 

 ! chemkin rubbish

  real RU, PATMOS, SMALL
  integer NKK, NO, NLITE, INLIN, IKTDIF, &  
          IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM,  &
          NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT,  &
          NCINT, NBIND, NEOK, NSGM, NAST, NBST,  &
          NCST, NXL, NR, NWRK, K3

      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF, &
                      IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM, &
                      NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT, &
                      NCINT, NBIND, NEOK, NSGM, NAST, NBST, &
                      NCST, NXL, NR, NWRK, K3
  ! args
                      
  real, intent(inout) :: RMCWRK(*)
  real, intent(in)  :: t(nx,ny)
  real, intent(in)  :: x(nx,ny,n_spec)
  real, intent(out) :: conmix(nx,ny)

  ! locals
  
  real :: alogt(nx,ny)
  real :: conspec(nx,ny,n_spec)
  real :: sum(nx,ny),sumr(nx,ny)
  integer k
  
!
!     In the following call, the pure species conductivities are in
!     RMCWRK(NXI)
!
      ALOGT(:,:) = LOG(T(:,:))
      IF (NO .EQ. 4) THEN
        CALL MCEVAL4_allij (ALOGT(:,:), NKK, RMCWRK(NLAM), conspec(:,:,:))
      ELSE
        CALL MCEVAL_allij (ALOGT(:,:), NKK, NO, RMCWRK(NLAM), conspec(:,:,:))
      ENDIF
!
      SUM(:,:) = ZERO
      SUMR(:,:) = ZERO      
      conspec(:,:,:) = EXP(conspec(:,:,:))
      DO 100 K = 1, NKK
         SUM(:,:) =  SUM(:,:)  + X(:,:,K)*conspec(:,:,k)
         SUMR(:,:) = SUMR(:,:) + X(:,:,K)/conspec(:,:,k)
  100 CONTINUE
!
      CONMIX(:,:) = 0.5 * (SUM(:,:) + ONE/SUMR(:,:))
!
!     end of SUBROUTINE MCACON_allij
      RETURN
      END SUBROUTINE MCACON_allij




!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
      SUBROUTINE MCADIF_allij (P, T, X, RMCWRK, D)
!
!  START PROLOGUE
!
!  SUBROUTINE MCADIF (P, T, X, RMCWRK, D)
!  Returns mixture-averaged diffusion coefficients given pressure,
!  temperature, and species mole fractions.
!
!  INPUT
!  P         - Real scalar, pressure.
!                 cgs units, dynes/cm**2
!  T         - Real scalar, temperature.
!                 cgs units, K
!  X(*)      - Real array, mole fractions of the mixture;
!              dimension at least KK, the total species count.
!  RMCWRK(*) - Real workspace array; dimension at least LENRMC.
!
!  OUTPUT
!  D(*)      - Real array, mixture diffusion coefficients;
!              dimension at least KK, the total species count.
!                 cgs units, cm**2/s
!
!  END PROLOGUE
!
  use param_m, only : nx,ny, n_spec
  implicit none
  
!  chemkin stuff 

   real RU, PATMOS, SMALL
   integer NKK, NO, NLITE, INLIN, IKTDIF, &
           IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM, &
           NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT, &
           NCINT, NBIND, NEOK, NSGM, NAST, NBST, &
           NCST, NXL, NR, NWRK, K3

      COMMON /MCMCMC/ RU, PATMOS, SMALL, NKK, NO, NLITE, INLIN, IKTDIF, &
                      IPVT, NWT, NEPS, NSIG, NDIP, NPOL, NZROT, NLAM, &
                      NETA, NDIF, NTDIF, NXX, NVIS, NXI, NCP, NCROT, &
                      NCINT, NBIND, NEOK, NSGM, NAST, NBST, &
                      NCST, NXL, NR, NWRK, K3
                      
!   args
    real, intent(in)  :: p(nx,ny)
    real, intent(in)  :: t(nx,ny)
    real, intent(in)  :: x(nx,ny,n_spec)
    real, intent(in)  :: rmcwrk(*)
    real, intent(out) :: d(nx,ny,n_spec)
   
!   locals
    real :: pfac(nx,ny)
    integer :: k
!
      CALL MCEDIF_allij (T(:,:), NO, NKK, X(:,:,:), RMCWRK(NDIF), &
                         RMCWRK(NWT), SMALL, RMCWRK(NXX), &
                         RMCWRK(NBIND), D(:,:,:))
!
      PFAC(:,:) = PATMOS / P(:,:)
      DO 100 K = 1, NKK
         D(:,:,K) = D(:,:,K) * PFAC(:,:)
  100 CONTINUE
!
!     end of SUBROUTINE MCADIF_allij
      RETURN
      END SUBROUTINE MCADIF_allij
!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
      SUBROUTINE MCEDIF_allij(T, NO, KK, X, COFD, WT, SMALL, XX, DJK, D)
!
!  START PROLOGUE
!
!  SUBROUTINE MCEDIF(T, NO, KK, X, COFD, WT, SMALL, XX, DJK, D)
!  This subroutine is used internally to compute the mixture
!  diffusion coefficients; normally not called by the package user.
!
!  INPUT
!  T          - Real scalar, temperature.
!                  cgs units, K
!  NO         - Integer scalar, order of fit.
!  KK         - Integer scalar, total species count.
!  X(*)       - Real array, mole fractions of the mixture;
!               dimension at least KK, the total species count.
!  COFD(*,*,*)- Real three-dimensional array, coefficients of the
!               fits for binary diffusion coefficients;
!               dimension at least NO for the first dimension,
!               the fit order, and at least KK, the total species
!               count, for both the second and last dimensions.
!  WT(*)      - Real array, species molecular weights;
!               dimension at least KK, the total species count.
!  SMALL      - Real scalar, a small number added to all mole fractions
!               before computing the mixture diffusion coefficients;
!               this process avoids an undefined situation when a pure
!               species condition is approached.
!  XX(*)      - Real array, mole fractions plus SMALL to avoid the
!               problem of a pure species;
!               dimension at least KK, the total species count.
!  RMCWRK(*)  - Real workspace array; dimension at LENRMC.
!
!  OUTPUT
!  D(*)       - Real array, mixture diffusion coefficients.
!                  cgs units, cm**2/s.
!  DJK(*,*)   - Real matrix, binary diffusion coefficients;
!               dimension at least KK, the total species count, for
!               both dimensions.
!                  cgs units, cm**2/s
!
!  END PROLOGUE
!
   use param_m, only : nx,ny,n_spec
   implicit none

!  parameters
   real, parameter :: zero = 0.0
   
!  args
   real,    intent(in)  :: t(nx,ny)
   integer, intent(in)  :: no,kk
   real,    intent(in)  :: x(nx,ny,n_spec)
   real,    intent(in)  :: cofd(no,n_spec,n_spec)
   real,    intent(in)  :: wt(n_spec)
   real,    intent(in)  :: small
   real,    intent(in)  :: xx         !not used
   real,    intent(in)  :: djk        !not used
   real,    intent(out) :: d(nx,ny,n_spec)
   
!  locals      
   real :: xxvec(nx,ny,n_spec)         !instead of xx
   real :: djkvec(nx,ny,n_spec,n_spec) !instead of DJK
   real :: wtm(nx,ny) 
   real :: alogt(nx,ny),sumxw(nx,ny),sumxod(nx,ny)
   integer :: k,j,i

      ALOGT(:,:) = LOG(T(:,:))
!
! Special Case for K = 1 - return the self-diffusion coefficient
!
      IF (KK .EQ. 1) THEN
        CALL MCEVAL_allij (ALOGT(:,:), 1, NO, COFD(1,1,1), djkvec(:,:,1,1))
        D(:,:,1) = EXP(djkvec(:,:,1,1))
        RETURN
      ENDIF
!
! Use the fact that DJK is symmetric to cut the work down by 1/2
!  - also we don't need the self-diffusion coefficient evaluated
!
              
      IF (NO .EQ. 4) THEN
!  BUG FIX by evatt - only if we are not exploiting dij symmetry
!  if we want to return to not using the symmetry, must loop from 1
!        DO 90 K = 1, KK
        DO 90 K = 2, KK
#ifdef VECTORVERSION
!  Added by Mark Fahey, ORNL.
!  Manually inline for Cray.
!  BUG FIX by evatt - only if we are not exploiting dij symmetry
!  if we want to return to not using the symmetry, must loop to KK
!           DO J = 1, KK  
           DO J = 1, K-1          
             djkvec(:,:,J,K) = (((COFD(4,J,K) * ALOGT(:,:)) &
                             + COFD(3,J,K)) * ALOGT(:,:) +  &                            
                               COFD(2,J,K)) * ALOGT(:,:) + COFD(1,J,K)
           ENDDO
#else
         CALL MCEVAL4_allij (ALOGT(:,:), K-1, COFD(1,1,K), djkvec(:,:,1,K) )
#endif
 90     CONTINUE
      ELSE
!  BUG FIX by evatt - only if we are not exploiting dij symmetry
!  if we want to return to not using the symmetry, must loop from 1
!        DO 100 K = 1, KK
        DO 100 K = 2, KK
#ifdef VECTORVERSION
!  Added by Mark Fahey, ORNL.
!  Manually inline for Cray.
!  BUG FIX by evatt - only if we are not exploiting dij symmetry
!  if we want to return to not using the symmetry, must loop to KK
!          DO J = 1, KK
          DO J = 1, K-1
            djkvec(:,:,J,K) = COFD(NO,J,K)
          ENDDO
          DO I = 1, NO-1
            DO J = 1, K-1
              djkvec(:,:,J,K) = djkvec(:,:,J,K) * ALOGT(:,:) + COFD(NO-I,J,K)
            ENDDO
          ENDDO
#else
          CALL MCEVAL (ALOGT(:,:), K-1, NO, COFD(1,1,K), djkvec(:,:,1,K) )
#endif
100     CONTINUE
      ENDIF

!
! Fill in the entire DJK, only after the exponential !
! - actually, evaluate and store the inverse of the
!   binary diffusion coefficient - this is what's needed.
!
!#ifdef VECTORVERSION
!! Added by Mark Fahey, ORNL.
!! If DJK is symmetric, can we do something more clever
!! now that the ij grid loop is inside this results in slower code
!! by about 5% according to tests by evatt - therefore go back to
!! symmetry exploitation.  to put this back in, note the bug fixes above
!
!      DO K = 1, KK
!         DO J = 1, KK
!            djkvec(:,:,J,K) = EXP(-djkvec(:,:,J,K))
!         ENDDO
!      ENDDO
!
!      DO K = 1, KK
!         djkvec(:,:,K,K) = ZERO
!      ENDDO
!#else
      DO 150 K = 1, KK
         DO 140 J = 1, K-1
            djkvec(:,:,J,K) = EXP(-djkvec(:,:,J,K))
            djkvec(:,:,K,J) = djkvec(:,:,J,K)
  140    CONTINUE
         djkvec(:,:,K,K) = ZERO
  150 CONTINUE
!#endif



      WTM(:,:) = ZERO
      DO 175 K = 1, KK
         WTM(:,:) = WTM(:,:) + WT(K)*X(:,:,K)
         xxvec(:,:,K) = MAX (X(:,:,K), SMALL)
  175 CONTINUE
!
      DO 300 K = 1, KK
!
         SUMXW(:,:)  = - xxvec(:,:,K) * WT(K)
         SUMXOD(:,:) = ZERO
!
         DO 200 J = 1, KK
             SUMXW(:,:)  = SUMXW(:,:)  + xxvec(:,:,J)*WT(J)
             SUMXOD(:,:) = SUMXOD(:,:) + xxvec(:,:,J)*djkvec(:,:,J,K)
  200    CONTINUE
!
         D(:,:,K) = SUMXW(:,:)/(WTM(:,:)*SUMXOD(:,:))
  300 CONTINUE
!
!     end of SUBROUTINE MCEDIF_allij
      RETURN
      END SUBROUTINE MCEDIF_allij

!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
      SUBROUTINE MCEVAL_allij (TF, KK, NO, COF, VAL)
!
!  START PROLOGUE
!
!  SUBROUTINE MCEVAL (TF, KK, NO, COF, VAL)
!  This subroutine uses Horner's algorithm to evaluate a polynomial
!  fit.  This routine is not normally called by the package user.
!
!  INPUT
!  TF        - Real scalar, independent variable of fit;
!              either temperature or log(temperature).
!  KK        - Integer scalar, total species count.
!  NO        - Integer scalar, order of fit.
!  COF(*,*)  - Real matrix, fit coefficients;
!              dimension exactly NO for the first dimension and at
!              least KK for the second.
!              COF(N,K) is the Nth coefficient of a property fit for
!              species K.
!
!  OUTPUT
!  VAL(*)    - Real array, evaluations of the fit at TF;
!              dimension at least KK, the total species count.
!
!  END PROLOGUE
!
      use param_m, only : nx,ny,n_spec
      implicit none

!     args
      integer no,kk
      real, intent(in)  :: tf(nx,ny)
      real, intent(out) :: val(nx,ny,n_spec)
      real, intent(in)  :: cof(no,n_spec)

!     locals
      integer nom1,k,i
      
!
      NOM1 = NO-1
!
      DO 10 K = 1, KK
        VAL(:,:,K) = COF(NO,K)
   10 CONTINUE
      DO 200 I = 1, NOM1
        DO 150 K = 1, KK
          VAL(:,:,K) = VAL(:,:,K) * TF(:,:) + COF(NO-I,K)
  150   CONTINUE
200   CONTINUE
!
!     end of SUBROUTINE MCEVAL_allij
      RETURN
      END SUBROUTINE MCEVAL_allij
!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
      SUBROUTINE MCEVAL4_allij (TF, KK, COF, VAL)
!
!  START PROLOGUE
!
!  SUBROUTINE MCEVAL4 (TF, KK, COF, VAL)
!  This subroutine uses Horner's algorithm to evaluate a polynomial;
!  the polynomial fit is hard-coded for order four polynomials, NO=4.
!  This routine is not normally called by the package user.
!
!  INPUT
!  TF       - Real scalar, independent variable of fit;
!             either temperature or log(temperature).
!  KK       - Integer scalar, total species count.
!  NO       - Integer scalar, order of fit.
!  COF(*,*) - Real matrix, fit coefficients;
!             dimension exactly NO for the first dimension, and at
!             least KK, the total species count, for the second;
!             COF(N,K) is the Nth coefficient of a property fit for
!             species K.
!
!  OUTPUT
!  VAL(*)   - Real array, evaluations of the fit at TF;
!             dimension at least KK, the total species count.
!
!  END PROLOGUE
!
      use param_m, only : nx,ny,n_spec
      implicit none
!     parameters
      integer, parameter :: no = 4

!     args
      integer kk
      real, intent(in)  :: tf(nx,ny)
      real, intent(out) :: val(nx,ny,n_spec)
      real, intent(in)  :: cof(no,n_spec)
      
!     locals
      integer k      

      DO 10 K = 1, KK
        VAL(:,:,K) = (((COF(4,K) * TF(:,:)) + COF(3,K)) * TF(:,:) + COF(2,K)) &
                                              * TF(:,:) + COF(1,K)
10    CONTINUE
!
!     end of SUBROUTINE MCEVAL4_allij
      RETURN
      END SUBROUTINE MCEVAL4_allij

!end vector version
#endif

end module transport_m


!=========================================================================================
!=========================================================================================


!----------------------------------------------------------------------
! Changes 
! 01/03/05 - Ramanan Sankaran
!----------------------------------------------------------------------
! Changed to non-dimensional units to avoid to and fro conversion
! and save flops
! This new version uses non-dimensional units
! Arguments are passed in as non-dimensional and 
! the resulting fluxes are non-dimensional too.
! Ignore older remarks about CGS units.
! Newer dummy arguments are added for interchangeability 
! with the lewis transport module
!----------------------------------------------------------------------
! Fluxes are now stored in the same memory location as the gradients
!----------------------------------------------------------------------

subroutine getDiffusiveFluxTerms( grad_u, Temp, grad_T, Ys, grad_Ys, Press, rho, & 
                                  cpmix, h_i)
  !----------------------------------------------------------------------------------
  ! Interface to the transport package.  This interface is for the "new"
  ! transport scheme in S3D. This method assumes mixture averaged values for
  ! thermal conductivity, viscosity, and diffusion coefficients (diagonal Fick matrix).
  ! These values are computed at each point in the domain for each evaluation of
  ! the right hand side.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! NOTE: All quantities in this routine are assumed to be in CGS units!!!
  !       That means that you must dimensionalize all quantities before passing
  !       them into this routine, and then nondimensionalize after returning.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! INPUT:
  !       grad_u  - velocity gradient tensor                    [1/s]
  !       Temp    - temperature                                 [K]
  !       grad_T  - Temperature gradient                        [K/cm]
  !       Ys	  - Species mass fraction                       []
  !       grad_Ys - species mass fraction gradient              [1/cm]
  !       Press   - Pressure                                    [atm]
  !       rho     - mass density                                [g/cm^3]
  !       h_i     - species enthalpies                          [erg/g]
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! OUTPUT: NOT REAL ARGUMENTS, STORED IN INPUT ARRAYS - RAMANAN(01/05/05)
  !       tau     - stress tensor - stored in grad_u            [g/(cm*s^2)]
  !       diffFlux - species mass diffusion fluxes - stored in  grad_Ys [g/(cm^2*s)]
  !       heatFlux - heat flux vector                           [erg/(cm^2*s)]
  !----------------------------------------------------------------------------------

  use transport_m
  use param_m,    only: nx,ny,nz,n_spec
#ifndef BUILD_LIBS3D
  use rk_m,       only: nstage
#endif
  use runtime_m,  only: i_time

  implicit none

  real, intent(inout), dimension(nx,ny,nz,3,3)       :: grad_u
  real, intent(in), dimension(nx,ny,nz)           :: Temp, Press, rho
  real, intent(in), dimension(nx,ny,nz)           :: cpmix !not used
  real, intent(inout), dimension(nx,ny,nz,3)         :: grad_T
  real, intent(in), dimension(nx,ny,nz,n_spec)    :: Ys, h_i
  real, intent(inout), dimension(nx,ny,nz,n_spec,3)  :: grad_Ys


  integer, save :: stage_counter=1
  integer, save :: next_calc_time=1
  integer       :: i,j,k,m,n

!!$  if(initialized_trans == .false.)  call initialize_transport(6)

  ! Set mu, lambda and Ds:
  ! If lag is on then it only updates once per 'lag_steps' number of time-steps:
#ifdef BUILD_LIBS3D
     call computeCoefficients( Press, Temp, Ys, rho )
#else
  if (.not. lagging_switch) then
     call computeCoefficients( Press, Temp, Ys, rho )
  elseif ((stage_counter == 1) .and. (i_time == next_calc_time)) then
     call computeCoefficients( Press, Temp, Ys, rho )
     stage_counter  = stage_counter + 1
     next_calc_time = next_calc_time + lag_steps
  elseif (stage_counter == nstage) then
     stage_counter = 1
  else
     stage_counter = stage_counter + 1
 endif
#endif

  call computeStressTensor( grad_u)                         ! calculate stress tensor
  call computeSpeciesDiffFlux( Temp, grad_T, Ys, grad_Ys, Press, rho)
                                                            ! calculate diffusion fluxes
  call computeHeatFlux( grad_T, h_i, grad_Ys)   ! calcualte heat flux

  return
end subroutine getDiffusiveFluxTerms

#include "globalDefines.h"
! Modified 5-9-03 to allow product stream compositions to be predicted
! from the mixture fraction under the assumption of complete combustion
! with the following products: CO2, H2O, N2, AR

! BUG FIX
! Evatt Hawkes 11-AUG-2004
! GetStoichMixFr called in initialisation before initialized set to .true.
! resulted in allocation of arrays twice in some cases
! Evatt Hawkes 22-NOV-2004
! Elemental subroutines were not portable - removed

! BUG FIX
! David Lignell 26-AUG-2005
! In specToMixfr(), at the end where the mixture fraction was forced to 
! range from 0 to 1.  The rescaling was wrong unless the mixture fraction
! already ranges from 0 to 1.  On one processor its okay, or on many if the 
! mixture fraction varies from 0 to 1 on the processor.  Otherwise, the 
! f bounds vary and the scaling gets goofed up.  I just delete the whole section

module mixFrac_m
!
! this module contains tools to compute the mixture fraction given the 
! species mass fraction or vice-versa
! 
! Currently set up for THREE-DIMENSIONS (spatial)
!
! Dependent on CHEMKIN - must be able to access the chemkin library.
!

  implicit none
  
  private

  
!----------------------- Variable Declarations ------------------------

 !---- declare PUBLIC variables ----
  
  real, public, allocatable, dimension(:,:,:) ::  mixFrac, phi
  real, public, allocatable, dimension(:,:,:,:) :: elem_mixFrac
  real, public :: stoichMixfr
  real, public, allocatable, dimension(:) :: fuelMassfr, oxidMassfr    
! pure stream compositions
  real, public, allocatable, dimension(:) :: fuelMolefr, oxidMolefr    
! pure stream compositions
  real, public, allocatable, dimension(:) :: fuelElemMassfr, oxidElemMassfr
  
  real,    public    :: Tfuel = -1.0, Toxid = -1.0                 ! dol added
  logical, public :: Tinitialized = .false. 

! additions Evatt Hawkes
  real, public,allocatable :: massfr_to_mixfrac_coeffs(:)        !conversion coeffs
  logical, public :: calculated_mixfrac_coeffs = .false.         !flag for coeffs calculation
  real, public :: mixfrac_const = 0.0                                    !-beta1/(beta2-beta1)

 !---- declare PRIVATE variables ----
                !!!---- be sure to set these ---!!!

  logical :: initialized = .false., init_betas = .false., init_stoich=.false.
  real, target :: beta1, beta2
  real, target, allocatable, dimension(:) :: elem_mass_frac, elem_mole_frac, gamma, elem_wt
  integer, target, allocatable, dimension(:,:) :: aa    ! species elemental composition matrix
  real, allocatable, dimension(:) :: xs_stoich_prod, ys_stoich_prod, r_sc, p_sc, sc

!---------------------- Routine Declarations -----------------------

 !---- declare PUBLIC routines ----
  public :: allocate_mixFrac_arrays     ! allocates arrays and initializes stuff
  public :: mixfrToSpec                 ! converts mix frac to species mass frac
  public :: specToMixfr                 ! converts species mass frac to mix frac
  public :: getStoichMixfr              ! computes Stoichiometric mixture fraction
!!$  public :: specToElemMixfr             ! computes elemental mixture fractions
  public :: massToMole                  ! converts mass fraction to mole fraction
  public :: moleToMass                  ! converts mole fraction to mass fraction
  public :: mixFracToPhi                ! converts mixture fraction to equivalence ratio
  public :: phiToMixFrac                ! converts equivalence ratio to mixture fraction
  public :: getProductComposition
  public :: getProductComposition2
  public :: phiBarlow
  public :: massToMoleAll
  public :: getBeta, getGamma, getali, getWl

! additions Evatt Hawkes
  public :: calc_mixfrac_coeffs         ! calculates coeffs for mass fr to mix fr conv.


 !---- declare PRIVATE routines ----
  private :: set_pure_stream_composition ! routine to set pure stream compositions
  private :: computeBeta                 ! function to compute the coupling function (beta)
  private :: computeElemMassFrac         ! routine to compute elemental mass fractions
  private :: setStoichiometry            ! sets stoichiometric coefficients etc.

!============================================================================  




contains




!============================================================================
!--------------------------  Public Routines  -------------------------------
!============================================================================


!============================================================================
  function getBeta( streamnum )
    implicit none
    real, pointer :: getBeta
    integer, intent(in) :: streamnum   ! 0->oxidizer, 1->fuel
    select case(streamnum)
    case(0)  ! oxidizer
       getBeta => beta1
    case(1)  ! fuel
       getBeta => beta2
    end select
    return
  end function getBeta
!============================================================================
  function getGamma( elemnum )
    implicit none
    real, pointer :: getGamma
    integer, intent(in) :: elemnum
    getGamma => gamma(elemnum)
    return
  end function getGamma
!============================================================================
  function getali( elem, spec )
    implicit none
    integer, pointer :: getali
    integer, intent(in) :: elem, spec
    getali => aa(elem,spec)
    return
  end function getali
!============================================================================
  function getWl( elemnun )
    implicit none
    real, pointer :: getWl
    integer, intent(in) :: elemnun
    getWl => elem_wt(elemnun)
    return
  end function getWl
!============================================================================

  subroutine allocate_mixFrac_arrays( mode )
    
    use chemkin_m, only : ickwrk, rckwrk, cckwrk, element_name, n_elements, n_species
    use param_m, only : nx,ny,nz
#if SERIAL
    integer, parameter :: myid=0
#else
    use topology_m, only : myid
#endif
    integer, intent(in) :: mode
    integer :: i
  
    if ( mode == 1 .and. .not. initialized ) then

       allocate( fuelMassfr(n_species), oxidMassfr(n_species) )
       allocate( fuelMolefr(n_species), oxidMolefr(n_species) )
       allocate( fuelElemMassfr(n_elements), oxidElemMassfr(n_elements) )
       allocate( mixFrac(nx,ny,nz) );   mixFrac=0.0
       allocate( phi(nx,ny,nz) );       phi=0.0
!!$       allocate( elem_MixFrac(nx,ny,nz,n_elements) )
       allocate( aa(n_elements,n_species), elem_wt(n_elements) )
       allocate( gamma(n_elements), elem_mass_frac(n_elements) )
       allocate( elem_mole_frac(n_elements) )

     ! set element atomic weights

       call CKAWT( ickwrk, rckwrk, elem_wt )    ! get the atomic mass of the elements  
       elem_wt = elem_wt / 1000                 ! convert to Kg/mol
       
     ! set species element composition matrix

       call CKNCF( n_elements, ickwrk, rckwrk, aa )      ! fill the elemental composition matrix

     ! set pure stream information
       
       call set_pure_stream_composition

     ! set gamma - weighting factors for computing the mixture fraction
     ! set as "Bilger's" mixture fraction.  See "Structure and Propagation of Methanol-Air
     ! Triple Flames", Echekki and Chen, Combustion and Flame vol 114 p 231-245 (1998)

       do i=1,n_elements
          gamma(i) = 0.
          select case (trim(element_name(i)))
          case ('c', 'C')
             gamma(i) = 2.0/elem_wt(i)
          case ('h', 'H')
             gamma(i) = 1.0/(2.0*elem_wt(i))
          case ('o', 'O')
             gamma(i) = -1.0/elem_wt(i)
          end select
       enddo

!      additions Evatt Hawkes
       allocate(massfr_to_mixfrac_coeffs(n_species)); massfr_to_mixfrac_coeffs=0.0

       initialized = .true.

       call getStoichMixfr  ! initializes beta_0 and beta_1 for later use.

       if (myid==0) write(*,*) 'Mixture fraction module successfully initialized...'

    elseif (mode == -1) then

       if (initialized) then
          deallocate( fuelMassfr, oxidMassfr, mixFrac, phi )
!!$          deallocate( elem_MixFrac )
          deallocate( fuelElemMassfr, oxidElemMassfr )
          deallocate( aa, elem_wt, gamma )
          deallocate( elem_mass_frac )
! deallocations added by esr 28/10/2010
         deallocate( fuelMolefr, oxidMolefr )
         deallocate( elem_mole_frac)

!         additions Evatt Hawkes
          deallocate( massfr_to_mixfrac_coeffs )
          initialized = .false.





       endif

    else
       if (mode /= 1 .and. mode /= -1) then
          if (myid==0) write(*,*) 'ERROR in allocate_MixFrac_arrays: expected 1 or -1 and received', mode
       endif

    endif
    
  end subroutine allocate_mixFrac_arrays

!=================================================================================

  subroutine calc_mixfrac_coeffs
    !-------------------------------------------------------------------!
    ! -calulates the coefficients for computing mixture fraction        !
    ! -streamlines the above procedures for calculating mixture fraction!             
    ! -Author : Evatt Hawkes                                            !
    !          (based on routines from James Sutherland)                !
    !                                                                   !
    ! converts species mass fractions to a mixture fraction (Bilger)    !
    !                                                                   !
    !            beta - beta2     beta2 is evaluated in the air stream  !
    ! MixFrac = --------------    beta1 is evaluated in the fuel stream !
    !            beta1 - beta2                                          !
    !                                                                   !
    ! ISSUES:                                                           !
    !   1. Must specify the composition of pure streams.                !
    !   2. Must specify coefficients for the coupling function (gammas) !
    !-------------------------------------------------------------------!

    use chemkin_m, only : n_elements, n_species

    real massfr_basis(n_species)
    real :: beta
    integer :: L
  !--------------------------- Executable Statements ---------------------------

  ! idiot - proofing

    if (.not. initialized) then
       call allocate_MixFrac_arrays( 1 )
    endif

    if (.not. init_betas) then
       call getStoichMixfr      ! this will set beta1 and beta2
    endif

    mixfrac_const = - beta1 / (beta2 - beta1)

!   loop over each basis vector
    do L=1,n_species
!     set the basis vector
      massfr_basis=0.0
      massfr_basis(L)=1.0

!     compute the coupling function, beta
      beta = computeBeta( massfr_basis(:), n_elements, n_species )

!     compute the mixture fraction
      massfr_to_mixfrac_coeffs(L) = (beta) / (beta2 - beta1)

    enddo

    calculated_mixfrac_coeffs=.true.

  end subroutine calc_mixfrac_coeffs

!=================================================================================

  subroutine mixfrToSpec( specMassFrac )
    !-------------------------------------------------------------------!
    ! converts the mixture fraction to species mass fractions           !
    !-------------------------------------------------------------------!

    use param_m, only : nx,ny,nz
    use chemkin_m, only : n_species

    real, intent(out), dimension(nx,ny,nz,n_species) :: specMassFrac
    integer :: i,j,k,n
  !--------------------------- Executable Statements ---------------------------

  ! idiot - proofing

    if (.not. initialized) then
       call allocate_MixFrac_arrays( 1 )
    endif

  ! loop over all grid points

    do k=1,nz
       do j=1,ny
          do i=1,nx
             do n=1,n_species
                specMassFrac(i,j,k,n) = MixFrac(i,j,k) * fuelMassfr(n)    &
                                        + (1.0-MixFrac(i,j,k)) * oxidMassfr(n)
             enddo
          enddo
       enddo
    enddo

  end subroutine mixfrToSpec

!===============================================================================================

  subroutine specToMixfr( massfr )
    !-------------------------------------------------------------------!
    ! converts species mass fractions to a mixture fraction (Bilger)    !
    !                                                                   !
    !            beta - beta2     beta2 is evaluated in the air stream  !
    ! MixFrac = --------------    beta1 is evaluated in the fuel stream !
    !            beta1 - beta2                                          !
    !                                                                   !
    ! ISSUES:                                                           !
    !   1. Must specify the composition of pure streams.                !
    !   2. Must specify coefficients for the coupling function (gammas) !
    !-------------------------------------------------------------------!
  
    use param_m, only : nx, ny, nz
    use chemkin_m, only : n_elements, n_species

    real, intent(in), dimension(nx,ny,nz,n_species) :: massfr

!    real :: beta
!    integer :: i,j,k
    integer :: L

  !--------------------------- Executable Statements ---------------------------

  ! idiot - proofing

    if (.not. initialized) then
       call allocate_mixFrac_arrays( 1 )
    endif

!  James' method involved lots of unnecessary calculations

!    if (.not. init_betas) then
!       call getStoichMixfr      ! this will set beta1 and beta2
!    endif
!
!  ! loop over all grid points
!    do k=1,nz
!       do j=1,ny
!          do i=1,nx
!
!           !-- compute the coupling function, beta
!             beta = computeBeta( massfr(i,j,k,:), n_elements, n_species )
!
!           !-- compute the mixture fraction
!             mixFrac(i,j,k) = (beta - beta1) / (beta2 - beta1)
!
!          enddo
!       enddo
!    enddo

!   Evatt Hawkes additions - only calculate the constants once

    if (.not. calculated_mixfrac_coeffs) then
       call calc_mixfrac_coeffs
    endif

!   add constant:
    MixFrac = mixfrac_const

!   loop over each species
    do L=1,n_species
      MixFrac = MixFrac + massfr(:,:,:,L)*massfr_to_mixfrac_coeffs(L)
    enddo

    ! bound
     mixfrac=min(max(mixfrac,0.0),1.0)

    return

  end subroutine specToMixfr

!===============================================================================================

  subroutine getStoichMixfr
    !-------------------------------------------------------------------!
    ! Compute the stoichiometric mixture fraction for the system        !
    !                                                                   !
    !            beta - beta2     beta2 is evaluated in the air stream  !
    ! MixFrac = --------------    beta1 is evaluated in the fuel stream !
    !            beta1 - beta2                                          !
    !                                                                   !
    ! stoichiometric when beta=0                                        !
    !                                                                   !
    ! this assumes a choice for the atomic mass fraction weighting      !
    ! factors (when you build the beta's) - this should be checked...   !
    !                                                                   !
    !-------------------------------------------------------------------!
    use chemkin_m, only : n_elements, n_species

  ! idiot - proofing

    if (.not. initialized) then
       call allocate_MixFrac_arrays( 1 )
    endif

    beta1 = computeBeta( oxidMassfr, n_elements, n_species )
    beta2 = computeBeta( fuelMassfr, n_elements, n_species )

    init_betas = .true.

    stoichMixfr = -beta1 / (beta2-beta1)
    
  end subroutine getStoichMixfr

!===============================================================================================

  subroutine specToElemMixfr( massfr )
    !-------------------------------------------------------------------!
    ! convert species mass fractions to elemental mass fractions        !
    !-------------------------------------------------------------------!

    use chemkin_m, only : n_elements, n_species
    use param_m, only : nx,ny,nz
    
    real, intent(in), dimension(n_species,nx,ny,nz) :: massfr
    integer :: i, j, k, n

  !--------------------------- Executable Statements ---------------------------

  ! idiot - proofing

    if (.not. initialized) then
       call allocate_MixFrac_arrays( 1 )
    endif

  ! loop over all grid points

    do k=1,nz
       do j=1,ny
          do i=1,nx

             call computeElemMassFrac( massfr(i,j,k,:) )

           ! loop over all elements

             do n=1,n_elements

              ! compute the elemental mixture fraction
                elem_MixFrac(i,j,k,n) = (elem_mass_frac(n) - oxidElemMassfr(n))    &
                            / (fuelElemMassfr(n) - oxidElemMassfr(n))
             enddo
          enddo
       enddo
    enddo

  end subroutine specToElemMixfr

!===============================================================================================

  subroutine getProductComposition( f, prodComp, form )
    !--------------------------------------------------------------------------!
    ! Given the mixture fraction, this routine obtains the product composition !
    ! assuming complete combustion. 'form' specifies whether the output is     !
    ! 'mass' or 'mole' fractions, if it does not exist it returns mass         !
    ! fractions.                                                               !
    !--------------------------------------------------------------------------!
    use chemkin_m, only : species_name, element_name, n_species, n_elements
    implicit none
    real, intent(in) :: f
    real, intent(out), dimension(n_species) :: prodComp
    character*4, intent(in), optional :: form
    real :: equiv, rjunk
    integer :: i
    real, dimension(n_species) :: tmp

    if( .not. init_stoich ) call setStoichiometry


    if( equiv == 1 ) then    ! stoichiometric
       prodComp = ys_stoich_prod
    elseif(equiv < 1) then  ! fuel-lean (excess air)
       prodComp = ys_stoich_prod + (StoichMixfr-f)/f*oxidMassfr
    else                ! fuel-rich (excess fuel)
       prodComp = ys_stoich_prod + (StoichMixfr-f)/(f-1.0)*fuelMassfr
    endif
    rjunk=sum(prodComp)
    prodComp=ProdComp/rjunk  ! this is mass fractions

    ! return mass fractions be default.  Return mole fractions if requested.
    if(present(form)) then
       select case (form)
       case( 'mole','MOLE','Mole' )
          tmp = prodComp
          call massToMole( tmp, prodComp )
       case( 'mass', 'MASS', 'Mass' )
          return
       end select
    else
       ! default is mass fraction
       return
    end if

    return
  end subroutine getProductComposition

!===============================================================================================

 subroutine getProductComposition2( f, prodComp )
    !--------------------------------------------------------------------------!
    ! David Lignell version
    ! Given the mixture fraction, this routine obtains the product composition !
    ! assuming complete combustion.    
    ! This assumes mass fraction output                                        !
    !--------------------------------------------------------------------------!
    use chemkin_m, only : species_name, element_name, n_species, n_elements
    implicit none
    real, intent(in) :: f
    real, intent(out), dimension(n_species) :: prodComp  ! mass frac
    real :: equiv, rjunk
    integer :: i
    real, dimension(n_species) :: tmp, ystprod
    real :: yprodt

    if( .not. init_stoich ) call setStoichiometry

    call moleToMass(xs_stoich_prod, ystprod)
    

    if(f <= stoichMixFr) then     ! lean
   
      yprodt = f/stoichMixFr
      prodComp = yprodt * ystprod + (1-yprodt) * OxidMassfr
   
    else                           ! rich
      yprodt = (1-f)/(1-stoichMixFr) 
      prodComp = yprodt * ystprod + (1-yprodt) * FuelMassfr

    endif
    
 end subroutine getProductComposition2



!============================================================================
!---------------------  Private Subroutines & Functions  --------------------
!============================================================================  



  real function computeBeta( massfr, n_elements, n_species )
    !-----------------------------------------------------------!
    ! compute the coupling function for the mixture fraction    !
    !-----------------------------------------------------------!

    integer, intent(in) :: n_elements, n_species
    real, intent(in), dimension(n_species) :: massfr

    integer :: n

  ! compute the elemental mass fractions
    call computeElemMassFrac( massfr )

  ! compute the coupling function, beta
    computeBeta = 0.0
    do n=1,n_elements
       computeBeta = computeBeta + gamma(n) * elem_mass_frac(n)
    enddo

    return

  end function computeBeta

!============================================================================  

  subroutine computeElemMassFrac( massfr )
    !-----------------------------------------------------------!
    ! compute the elemental mass fractions (elem_mass_frac)     !
    !                                                           !
    ! INPUT:                                                    !
    !   massfr - species mass fraction array (n_species)        !
    !-----------------------------------------------------------!
    use chemkin_m, only : molwt, n_elements, n_species

    real, dimension(n_species), intent(in) :: massfr

    integer :: m, n

  ! compute the elemental mass fractions
    do n=1,n_elements
       elem_mass_frac(n) = 0.0
       do m=1,n_species
          elem_mass_frac(n) = elem_mass_frac(n) +                       &
               aa(n,m) * elem_wt(n) * massfr(m) / molwt(m)
       enddo
    enddo

  end subroutine computeElemMassFrac

!==============================================================================================

  subroutine setStoichiometry
    !---------------------------------------------------------------------------!
    ! compute stoichiometric coefficients for species assuming that the only    !
    ! products of combustion include                                            !
    !   CO2  H2O  N2  AR                                                        !
    ! also compute product composition at stoichiometric conditions             !
    !---------------------------------------------------------------------------!
    use chemkin_m, only : n_species, n_elements, species_name, element_name
    implicit none
    real, dimension(n_species) :: ys_stoich, xs_stoich
    real, dimension(n_elements) :: elem_stoich
    real :: rjunk, rn, pn
    integer :: CO2_s, H2O_s, N2_s, Ar_s, O2_s
    integer :: C_e, H_e, N_e, O_e, Ar_e
    integer :: i

    allocate( xs_stoich_prod(n_species), ys_stoich_prod(n_species), r_sc(n_species), p_sc(n_species), sc(n_species) )

    !-- identify indices of "products" and some key reactants
    CO2_s=-1;  H2O_s=-1;  N2_s=-1;  Ar_s=-1;
    do i=1,n_species
       select case( trim(species_name(i)) )
       case('O2')
          O2_s = i
       case ('CO2')
          CO2_s = i
       case('H2O')
          H2O_s = i
       case('N2')
          N2_s = i
       case('Ar')
          Ar_s = i
       end select
    enddo
    !-- Identify indices of elements
    C_e=-1;  H_e=-1;  N_e=-1;  O_e=-1;  Ar_e=-1
    do i=1,n_elements
       select case( trim(element_name(i)) )
       case ('C')
          C_e = i
       case('H')
          H_e = i
       case('O')
          O_e = i
       case('N')
          N_e = i
       case('Ar')
          Ar_e = i
       end select
    enddo

  !-- set the stoichiometric composition (mole and mass fractions)
    ys_stoich = stoichMixfr*fuelMassfr + (1.0-stoichMixfr)*oxidMassfr
    call massToMole( ys_stoich, xs_stoich )

  !-- set elemental mole fractions at stoichiometric conditions
    elem_stoich = matmul(aa,xs_stoich)
    rjunk = sum(elem_stoich)
    elem_stoich = elem_stoich / rjunk

  !-- set the product composition at stoichiometric conditions
  ! This assumes that the only available product species are: CO2, H2O, N2
    xs_stoich_prod = 0.0
    if( CO2_s > 0 ) xs_stoich_prod(CO2_s) = elem_stoich(C_e)
    if( H2O_s > 0 ) xs_stoich_prod(H2O_s) = elem_stoich(H_e)/2.0
    if( N2_s  > 0 ) xs_stoich_prod( N2_s) = elem_stoich(N_e)/2.0
    if( AR_s  > 0 ) xs_stoich_prod( AR_s) = elem_stoich(Ar_e)
    rjunk = sum(xs_stoich_prod)
    xs_stoich_prod = xs_stoich_prod/rjunk
    call moleToMass( xs_stoich_prod, ys_stoich_prod )

  !-- Set the stoichiometric coefficients (r_sc, p_sc for reactants and products)
    where(xs_stoich == 0.0)
       r_sc = 0.0
    elsewhere
       r_sc = 1.0
    end where
    where( xs_stoich_prod == 0.0 )
       p_sc = 0.0
    elsewhere
       p_sc = 1.0
    end where

  !-- Determine stoichiometry coefficients
    ! rn -> number of each element in reactant stream
    ! pn -> number of each element in product  stream

    ! Carbon balance to set CO2 coefficient (assuming CO2 is only product containing C)
    if( C_e > 0 ) then
       rn =  sum(aa(C_e,:) * r_sc)
       p_sc(CO2_s) = p_sc(CO2_s) * rn/aa(C_e,CO2_s)
    endif
    ! Hydrogen balance to set H2O coefficient (assuming H2O is only product containing H)
    if( H_e > 0 ) then
       rn =  sum(aa(H_e,:) * r_sc)
       p_sc(H2O_s) = p_sc(H2O_s) * rn/aa(H_e,H2O_s)
    endif
    ! Oxygen balance to set O2 coefficient
    if( O_e > 0 ) then
       rn = sum(aa(O_e,:) * r_sc)
       pn = sum(aa(O_e,:) * p_sc)
       rjunk = pn - rn + aa(O_e,O2_s)      ! add present in reactants
       r_sc(O2_s) = r_sc(O2_s) * rjunk/aa(O_e,O2_s)
    endif
    ! Nitrogen balance to set N2 coefficient
    if( N_e > 0.0 ) then
       rn = sum(aa(N_e,:)*r_sc)
       pn = sum(aa(N_e,:)*r_sc)
       rjunk = rn - pn + aa(N_e,N2_s)
       p_sc(N2_s) = p_sc(N2_s) * rjunk/aa(N_e,N2_s)
    endif
    ! Argon balance to set Ar coefficient (Argon is inert)
    if( Ar_e > 0.0 ) p_sc(Ar_s) = r_sc(Ar_s)
    
    sc = r_sc + p_sc    ! final stoichiometric coefficients.
    rn = sum(r_sc)      ! number of moles of products
    pn = sum(p_sc)      ! number of moles of reactants

    init_stoich = .true.

    return
  end subroutine setStoichiometry

!==============================================================================================

  subroutine set_pure_stream_composition
    !-------------------------------------------------------------!
    ! set pure stream compositions - required for input to OPPST  !
    ! as well as for mixture fraction calculations                !
    !                                                             !
    ! This routine reads an input file called "streams.in" to set !
    ! the pure stream compositions.  This file should contain     !
    ! certain keywords as described below.                        !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Compositions may be entered in mass or mole fractions, with !
    ! species names specified, followed by the mass or mole       !
    ! fraction.                                                   !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! KEYWORDS:                                                   !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! FUEL - specifies that fuel stream compositions is being set !
    ! OXID - specifies that oxid stream composition is being set  !
    ! /    - specifies a comment line                             !
    ! !    - specifies a comment line                             !
    ! END  - end of FUEL or OXID block                            !
    ! MASS - specifies that mass fractions are entered            !
    ! MOLE - specifies that mole fractions are entered            !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! Example File:                                               !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! MOLE                                                        !
    ! FUEL                                                        !
    ! H2    0.5                                                   !
    ! N2    0.5                                                   !
    ! END                                                         !
    ! OXID                                                        !
    ! O2    0.21                                                  !
    ! N2    0.79                                                  !
    ! END                                                         !
    !-------------------------------------------------------------!
    use chemkin_m, only : n_species, species_name, ickwrk, rckwrk
#if SERIAL
    integer, parameter :: myid=0
#else
    use topology_m !, only : myid, ierr
#endif
    
    integer, parameter :: infile=11
    integer :: io = 6
    integer :: j, n
    real :: sumfrac, comp
    real, dimension(n_species) :: fuelComp,  oxidComp
    character*16 :: name
    character*30 :: line, subs(2)
    logical :: assigned, assigned_fuel, assigned_oxid, inputMoleFr
    logical :: exists, lerr
  !--------------------------------- Executable Statements ---------------------------------

  ! initialization

    fuelComp=0.0;               oxidComp=0.0
    assigned_fuel=.false.;      assigned_oxid=.false.
    assigned=.false.;           inputMoleFr=.false.

  ! set pure stream mass fractions for mixture fraction calc...

    if (myid==0) inquire( file='../input/streams.in', exist=exists )
#if SERIAL
#else
    call MPI_Bcast( exists, 1, MPI_LOGICAL, 0, gcomm, ierr )
#endif
    if (.not. exists) then
       if (myid==0) then
          write(io,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(io,*)'FATAL ERROR: input file "../input/streams.in" does not exist!'
          write(io,*)'terminating run... (set_initial_conditions in mixFracTools.f90)'
          write(io,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
       endif
#if SERIAL
#else
       call MPI_Finalize( ierr )
#endif
       stop
    endif

    if (myid==0) then

       open( unit=infile, file='../input/streams.in', form='formatted', status='old' )

     ! start reading the file to get pure stream information...

       do

          assigned = .false.
        ! read the first line and decide what to do...
          read(infile,*) name

          select case (trim(name))

          case ('FUEL')              ! read fuel stream composition
             do 
                read(infile,'(A)') line

                call cksubs(line,6,30,subs,j,lerr)
                name = trim(subs(1))

                if(trim(name)=='END') then
                   if(assigned)  assigned_fuel = .true.    ! at least one fuel component was specified
                   exit
                elseif ( trim(name) /= '!' .and. trim(name) /= '/' ) then
                   call ckxnum(subs(2),1,6,j,comp,lerr)
                   if(lerr) then
                      assigned = .false.
                   else
                      assigned = .true.
                   endif
                   call ckcray(name,n_species,species_name,6,1,j,n,lerr)
                   if(lerr) assigned=.false.
                   fuelComp(j) = comp
                endif

                if (.not. assigned) then
                   write(io,*)
                   write(io,*)'ERROR assigning FUEL stream composition! ABORTING...'
                   stop
                endif
             enddo

          case ('OXID')              ! read oxidizer stream composition
             do
                read(infile,'(A)') line

                call cksubs(line,6,30,subs,j,lerr)
                name = trim(subs(1))

                if(trim(name)=='END') then
                   if(assigned)  assigned_oxid = .true.    ! at least one fuel component was specified
                   exit
                elseif ( trim(name) /= '!' .and. trim(name) /= '/' ) then
                   call ckxnum(subs(2),1,6,j,comp,lerr)
                   if(lerr) then
                      assigned = .false.
                   else
                      assigned = .true.
                   endif
                   call ckcray(name,n_species,species_name,6,1,j,n,lerr)
                   if(lerr) assigned = .false.
                   oxidComp(j) = comp
                endif

                if (.not. assigned) then
                   write(io,*)
                   write(io,*)'ERROR assigning OXID stream composition! ABORTING...'
                   stop
                endif
             end do
     
          case ('/')         ! comment line - do nothing
          case ('!')         ! comment line - do nothing
          case ('MOLE')
             inputMoleFr = .true.
          case ('MASS')
             inputMoleFr = .false.
          case ('TFUEL')
            read(infile,*) Tfuel
          case ('TOXID') 
             read(infile,*) Toxid
          case default
          end select

          if (Tfuel > 0 .and. Toxid > 0) Tinitialized = .true.
          if (assigned_fuel .and. assigned_oxid) exit
          


       enddo

       close( infile )

    endif  ! (myid==0)

#if SERIAL
#else
    call MPI_Bcast( fuelComp, n_species, MPI_REAL8, 0, gcomm, ierr )
    call MPI_Bcast( oxidComp, n_species, MPI_REAL8, 0, gcomm, ierr )
    call MPI_Bcast( inputMoleFr,  1,   MPI_LOGICAL, 0, gcomm, ierr )
    call MPI_Bcast( fuelComp, n_species, MPI_REAL8, 0, gcomm, ierr )
    call MPI_Bcast( Tfuel, 1, MPI_REAL8, 0, gcomm, ierr)
    call MPI_Bcast( Toxid, 1, MPI_REAL8, 0, gcomm, ierr)

#endif
  ! normalize mole fractions if neccessary

    sumfrac = sum(fuelComp)
    if (sumfrac > 1.000001 .or. sumfrac < 0.999999) then
       if (myid==0) write(io,*)'NORMALIZING FUEL COMPOSITION...'
       fuelComp = fuelComp/sumfrac
    endif

    sumfrac = sum(oxidComp)
    if (sumfrac > 1.000001 .or. sumfrac < 0.999999) then
       if (myid==0) write(io,*)'NORMALIZING OXIDIZER COMPOSITION...'
       oxidComp = oxidComp/sumfrac
    endif

  ! set mass and mole fractions as appropriate  

    if (inputMoleFr) then
       call CKXTY( fuelComp, ickwrk, rckwrk, fuelMassfr )
       call CKXTY( oxidComp, ickwrk, rckwrk, oxidMassfr )
       fuelMolefr = fuelComp
       oxidMolefr = oxidComp
    else
       fuelMassfr = fuelComp
       oxidMassfr = oxidComp
       call CKYTX( fuelMassfr, ickwrk, rckwrk, fuelMolefr )
       call CKYTX( oxidMassfr, ickwrk, rckwrk, oxidMolefr )
    endif

  ! set pure stream elemental composition
    
    call computeElemMassFrac( fuelMassfr );  fuelElemMassfr = elem_mass_frac
    call computeElemMassFrac( oxidMassfr );  oxidElemMassfr = elem_mass_frac

  end subroutine set_pure_stream_composition

!=================================================================================

  subroutine massToMole( specMassFrac, specMoleFrac )
    !-------------------------------------------------------------------!
    ! converts species mass fractions to species mole fraction          !
    !-------------------------------------------------------------------!

    use chemkin_m, only : n_species, molwt

    real, intent(in),  dimension(n_species) :: specMassFrac
    real, intent(out), dimension(n_species) :: specMoleFrac
    real :: molwt_mix
    integer :: i,j,k,L
  !--------------------------- Executable Statements ---------------------------

  ! loop over all grid points

             molwt_mix=0.0
             do L=1,n_species,1
               molwt_mix = molwt_mix + specMassFrac(L)/molwt(L)
             enddo
             molwt_mix = 1.0 / molwt_mix

             do L=1,n_species,1
               specMoleFrac(L) = specMassFrac(L)*molwt_mix/molwt(L)
             enddo
  end subroutine massToMole

!=================================================================================

  subroutine massToMoleAll( specMassFrac, specMoleFrac )
    !-------------------------------------------------------------------!
    ! converts species mass fractions to species mole fraction          !
    !-------------------------------------------------------------------!

    use param_m, only : nx,ny,nz
    use chemkin_m, only : n_species, molwt

    real, intent(in),  dimension(nx,ny,nz,n_species) :: specMassFrac
    real, intent(out), dimension(nx,ny,nz,n_species) :: specMoleFrac
    real :: molwt_mix
    integer :: i,j,k,L
  !--------------------------- Executable Statements ---------------------------

  ! loop over all grid points

    do k=1,nz
       do j=1,ny
          do i=1,nx

             molwt_mix=0.0
             do L=1,n_species,1
               molwt_mix = molwt_mix + specMassFrac(i,j,k,L)/molwt(L)
             enddo
             molwt_mix = 1.0 / molwt_mix

             do L=1,n_species,1
               specMoleFrac(i,j,k,L) = specMassFrac(i,j,k,L)*molwt_mix/molwt(L)
             enddo

          enddo
       enddo
    enddo

  end subroutine massToMoleAll

!============================================================================  

  subroutine moleToMass( specMoleFrac, specMassFrac )
    !-------------------------------------------------------------------!
    ! converts species mole fractions to species mass fraction          !
    !-------------------------------------------------------------------!

    use chemkin_m, only : n_species, molwt

    real, intent(in), dimension(n_species) :: specMoleFrac
    real, intent(out), dimension(n_species) :: specMassFrac
    real :: molwt_mix
    integer :: i,j,k,L
  !--------------------------- Executable Statements ---------------------------

  ! loop over all grid points

    

             molwt_mix=0.0
             do L=1,n_species,1
               molwt_mix = molwt_mix + specMoleFrac(L)*molwt(L)
             enddo

             do L=1,n_species,1
               specMassFrac(L) = specMoleFrac(L)*molwt(L)/molwt_mix
             enddo

         

  end subroutine MoleToMass

!============================================================================  

  subroutine moleToMassAll( specMoleFrac, specMassFrac )
    !-------------------------------------------------------------------!
    ! converts species mole fractions to species mass fraction          !
    !-------------------------------------------------------------------!

    use param_m, only : nx,ny,nz
    use chemkin_m, only : n_species, molwt

    real, intent(in), dimension(nx,ny,nz,n_species) :: specMoleFrac
    real, intent(out), dimension(nx,ny,nz,n_species) :: specMassFrac
    real :: molwt_mix
    integer :: i,j,k,L
  !--------------------------- Executable Statements ---------------------------

  ! loop over all grid points

    do k=1,nz
       do j=1,ny
          do i=1,nx

             molwt_mix=0.0
             do L=1,n_species,1
               molwt_mix = molwt_mix + specMoleFrac(i,j,k,L)*molwt(L)
             enddo

             do L=1,n_species,1
               specMassFrac(i,j,k,L) = specMoleFrac(i,j,k,L)*molwt(L)/molwt_mix
             enddo

          enddo
       enddo
    enddo

  end subroutine MoleToMassAll

!=================================================================================


!  BUG fix 22-NOV-2004 Evatt Hawkes elemental subroutines not portable
!
!  elemental subroutine mixFracToPhi( f, equiv )
!    !-------------------------------------------------------------------!
!    ! converts mixture fraction to equivalence ratio                    !
!    !                                                                   !
!    !          Z / ( 1 - Z )                                            !
!    ! Phi = -------------------                                         !
!    !       Z_st / ( 1 - Z_st )                                         !
!    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!    ! INPUT:    f   -> mixture fraction                                 !
!    ! OUTPUT: equiv -> equivalence ratio                                !
!    !-------------------------------------------------------------------!
!    real, intent(in) :: f
!    real, intent(out) :: equiv
!    equiv = f*(1.0-stoichMixfr) / (stoichMixfr*(1.0-f))
!    return
!  end subroutine mixFracToPhi
!
!!=================================================================================
!
!  elemental subroutine phiToMixFrac( equiv, f )
!    !-------------------------------------------------------------------!
!    ! converts equivalence ratio to mixture fraction                    !
!    !                                                                   !
!    !         Z_st * Phi                                                !
!    ! Z = --------------------                                          !
!    !     1 + Z_st * (Phi - 1)                                          !
!    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!    ! INPUT:    f   -> mixture fraction                                 !
!    ! OUTPUT: equiv -> equivalence ratio                                !
!    !-------------------------------------------------------------------!
!    implicit none
!    real, intent(in) :: equiv
!    real, intent(out) :: f
!    f = (stoichMixfr*equiv) / (1.0+stoichMixfr*(equiv-1.0))
!    return
!  end subroutine phiToMixFrac


  subroutine mixFracToPhi( f, equiv )
    !-------------------------------------------------------------------!
    ! converts mixture fraction to equivalence ratio                    !
    !                                                                   !
    !          Z / ( 1 - Z )                                            !
    ! Phi = -------------------                                         !
    !       Z_st / ( 1 - Z_st )                                         !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! INPUT:    f   -> mixture fraction                                 !
    ! OUTPUT: equiv -> equivalence ratio                                !
    !-------------------------------------------------------------------!
    use param_m, only : nx,ny,nz
    implicit none
    real, dimension(nx,ny,nz), intent(in) :: f
    real, dimension(nx,ny,nz), intent(out) :: equiv
    equiv = f*(1.0-stoichMixfr) / (stoichMixfr*(1.0-f))
    return
  end subroutine mixFracToPhi

!=================================================================================

  subroutine phiToMixFrac( equiv, f )
    !-------------------------------------------------------------------!
    ! converts equivalence ratio to mixture fraction                    !
    !                                                                   !
    !         Z_st * Phi                                                !
    ! Z = --------------------                                          !
    !     1 + Z_st * (Phi - 1)                                          !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! INPUT:    f   -> mixture fraction                                 !
    ! OUTPUT: equiv -> equivalence ratio                                !
    !-------------------------------------------------------------------!
    use param_m, only : nx,ny,nz
    implicit none
    real, dimension(nx,ny,nz), intent(in) :: equiv
    real, dimension(nx,ny,nz), intent(out) :: f
    f = (stoichMixfr*equiv) / (1.0+stoichMixfr*(equiv-1.0))
    return
  end subroutine phiToMixFrac
  
!=================================================================================

  subroutine phiBarlow( massfr, equiv )
    !-------------------------------------------------------------------!
    ! Computes the equivalence ratio based on local information only,   !
    ! this is independent of boundary conditions                        !
    !                                                                   !
    !    Phi=AFR_st/AFR                                                 !
    !    AFR_st = fn( Y_C/Y_H)                                          !
    !                                                                   !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    ! OUTPUT: equiv -> equivalence ratio                                !
    !-------------------------------------------------------------------!
    use param_m, only : nx,ny,nz
    use chemkin_m, only : n_elements, n_species, element_name
    implicit none
    real, dimension(nx,ny,nz,n_species), intent(in) :: massfr
    real, dimension(nx,ny,nz), intent(out) :: equiv
    real EW_OtoEW_C, EW_OtoEW_H, NtoO, HtoC,YC,YO,YH,YN,AFRst,AFR
    integer indx(4), i,j,k

! idiot proofing

      if (.not. initialized) then
        call allocate_mixFrac_arrays( 1 )
      endif

!    AFR_st=(Y_O + Y_N)/(Y_C + Y_H)
! This equation has four unknowns, we close the system by:
! 1) taking the ratio of Y_O/Y_N from the DNS.
! 2) taking hte ratio of Y_C/Y_H from the DNS.
! 3) the sum of mass fractions equals 1.
! 4) for stoichiometry Y_O = 0.5Y_C(Elem_wt(O)/Elem_wt(C)) + 2Y_H(Elem_wt(O)/Elem_wt(H))

! This translates in to:
! Y_N = Y_O * (Y_N/Y_O)_DNS
! Y_H = Y_C * (Y_H/Y_C)_DNS
! Y_O = Y_C * (0.5*(Elem_wt(O)/Elem_wt(C) + 2*(Elem_wt(O)/Elem_wt(H)*(Y_H/Y_C)_DNS)
! Y_C = 1 / 
!  {(1+(Y_N/Y_O)_DNS)*[0.5*(Elem_wt(O)/Elem_wt(C) + 2*(Elem_wt(O)/Elem_wt(H)*(Y_H/Y_C)_DNS] 
!      + (1+(Y_H/Y_C)_DNS)}

! order the elements: O,H,C,N.
     do i=1,n_elements
       select case (trim(element_name(i)))
       case ('o', 'O')
       indx(1)=i
       case ('h', 'H')
       indx(2)=i
       case ('c', 'C')
       indx(3)=i
       case ('n', 'N')
       indx(4)=i
       end select
     enddo

    EW_OtoEW_C = elem_wt(indx(1))/elem_wt(indx(3))
    EW_OtoEW_H = elem_wt(indx(1))/elem_wt(indx(2))

    do k=1,nz
       do j=1,ny
          do i=1,nx

             call computeElemMassFrac( massfr(i,j,k,:) )

             NtoO=elem_mass_frac(indx(4))/elem_mass_frac(indx(1))
             HtoC=elem_mass_frac(indx(2))/elem_mass_frac(indx(3))

             YC = (1+NtoO)*(2.0*EW_OtoEW_C + 0.5*EW_OtoEW_H*HtoC) + (1+HtoC)
             YC = 1.0/YC
             YO = YC * (2.0*EW_OtoEW_C + 0.5*EW_OtoEW_H*HtoC)
             YH = YC * HtoC
             YN = YO * NtoO
             
             AFRst=(YO+YN)/(YC+YH)
             AFR  =(elem_mass_frac(indx(1))+elem_mass_frac(indx(4))) / &
                   (elem_mass_frac(indx(2))+elem_mass_frac(indx(3)))
             equiv(i,j,k)=AFRst/AFR

          enddo
       enddo
    enddo

    return
  end subroutine phiBarlow

!============================================================================  

end module mixFrac_m

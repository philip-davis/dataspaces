#include "globalDefines.h"

module transport_m
!----------------------------------------------------------------------
! Changes 
! 01/03/05 - Ramanan Sankaran
!----------------------------------------------------------------------
! Changed to non-dimensional units to avoid to and fro conversion
! and save flops
! This new version uses non-dimensional units
! Arguments are passed in as non-dimensional and 
! the resulting fluxes are non-dimensional too.
!----------------------------------------------------------------------
! ***** IGNORE OLDER REMARKS ABOUT CGS UNITS. ******
!----------------------------------------------------------------------

!----------------------------------------------------------------------
! Changed the argument list to be consistent with mixture averaged transport
!----------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !                      Author: James Sutherland
  !                      Date:   April, 2002
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! This module provides tools to compute the diffusive terms in the Navier-
  ! Stokes equations for multicomponent flow.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !**************** ALL ARGUMENTS MUST BE PASSED IN CGS UNITS ******************
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! USAGE:
  !     1. Call "initialize_transport" routine once to set up the module
  !
  !     2. Call "computeCoefficients" routine to set mu, lambda
  !        This MUST be called BEFORE the remainder of the routines...
  !
  !     3. Call "computeSpeciesDiffFlux" routine to set diffusion velocities.
  !        This MUST be called BEFORE computing the heat flux.
  !
  !     4. Call "computeStressTensor" routine to set stress tensor.
  !
  !     5. Call "computeHeatFlux" routine to set heat flux vector.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !        mu*Cp                 lambda                   mu
  !  Pr = --------      Le_i = ------------     Sc_i = ---------
  !        lambda               rho*Cp*D_i              rho*D_i
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Lewis numbers are read from a file called "lewis.in" which should reside
  ! in the directory (../input).  They may be listed in any order (see below).
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Thermal conductivity is calculated as a function of T and Cp, as suggested
  ! by Smooke & Giovangigli  {Lect. Notes Phys. 284:1-28 (1991)}.
  !
  ! Then, given rho, Cp, and Le, all other transport coefficients D_i, mu,
  ! may be calculated from the Pr, Le_i, Sc_i relationships given above.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! DEPENDANCIES:
  !    param_m    - module specifying many parameters for S3D.  The only
  !                 information required from this module is the physical
  !                 grid size [nx,ny,nz], and the number of species [n_spec]
  !
  !    topology_m - module containing parallelization information.  This is
  !                 only required in the initialization routine.
  !
  !    chemkin_m  - module containing chemkin information.  Used in the 
  !                 initialization routine to get species names.
  !-----------------------------------------------------------------------------

  use param_m, only : nx,ny,nz,n_spec
  use reference_m, only: rho_ref, a_ref, t_ref, l_ref, cp_ref
  implicit none

  private

  real, public :: Pr    ! mixture prandtl number (read from s3d.in - see io.f90)
  !Added by Ramanan Sankaran - 01/03/05
  real :: lambda_coeff  !Coefficient for the temperature dependent lambda

  real, allocatable, dimension(:,:,:) :: viscosity      ! viscosity
  real, allocatable, dimension(:,:,:) :: lambda         ! thermal conductivity
  real, allocatable, dimension(:)     :: Le             ! species Lewis numbers
  !Changed Sc to InvSc to save flops - Ramanan - 01/03/05
  real, public, allocatable, dimension(:)     :: InvSc          ! inverse species Schmidt numbers

  logical, public :: initialized_transport = .false.

  public :: initialize_transport        ! initialize the transport module
  public :: computeCoefficients         ! set thermal conductivity and viscosity
  public :: computeSpeciesDiffFlux      ! calculate the species diffusion velocities
  public :: computeStressTensor         ! calculate the stress tensor
  public :: computeHeatFlux             ! calculate the heat flux vector
  public :: allocate_transport_arrays

  public :: getViscosity                ! access function.  Returns viscosity in CGS units.
  public :: getThermalConductivity      ! access function.  Returns lambda in CGS units.

  public :: getSpcsDiffusivity

!!$==================================================================================


contains

!----------------------------------------------------------------------
! Returns the diffusivity (rho*D) of species with index `m'
function getSpcsDiffusivity(m) result (diff)
use param_m, only : nx,ny,nz
real, dimension(nx,ny,nz) :: diff
integer, intent(in) :: m

diff = getViscosity()
diff = diff*InvSc(m)
return
end function getSpcsDiffusivity


!----------------------------------------------------------------------


  function getViscosity()
    use param_m, only : nx,ny,nz
    real, dimension(nx,ny,nz) :: getViscosity
    getViscosity = viscosity
    return
  end function getViscosity


!!$==================================================================================


  function getThermalConductivity()
    use param_m, only : nx,ny,nz
    real, dimension(nx,ny,nz) :: getThermalConductivity
    getThermalConductivity = lambda
    return
  end function getThermalConductivity


!!$==================================================================================


  subroutine initialize_transport(io)
    !----------------------------------------------------------------------
    ! This routine initializes the transport module.  It reads the lewis
    ! numbers from ../input/lewis.in
    !----------------------------------------------------------------------
    use topology_m
    use chemkin_m, only : species_name

    integer :: io, i,j
    real :: le_in
    character*16  :: species_name_in
    character*100 :: filename

    call allocate_transport_arrays(1)   ! allocate necessary space.

    !--------------- Read Lewis Numbers from File --------------

    filename='../input/lewis.in'
    call inquire_about_input_file(filename,io)

    if(myid.eq.0) then
       write(io,*) 'initializing transport module...'
       write(io,*)
       write(io,*) 'reading lewis numbers from ',trim(filename)
       write(io,*)

       open(unit=1,file=trim(filename),status='old')

       ! read lewis numbers (can be in any order)

       do i=1,n_spec,1
          read(1,*,end=2) le_in, species_name_in
          do j=1,n_spec,1
             if(trim(species_name(j)).eq.trim(species_name_in)) then
                Le(j)=le_in
             endif
          enddo
       enddo

       close(1)

       ! error checking for unspecified lewis numbers

       if(minval(Le).le.0.0) then
          write(io,*) 'an improper lewis number was specified in lewis.in'
          write(io,*) 'for the following species:'
          do j=1,n_spec,1
             if(Le(j).le.0.0) write(io,*) species_name(j), Le(j)
          enddo
          term_status=1
       endif

    endif

    call check_term_status(io,0)        ! check for errors on file open

    if(myid.eq.0) then       ! write to lewis numbers to io
       write(io,*) 'lewis numbers are as follows:'
       write(io,*)
       do i=1,n_spec,1
          write(io,9) Le(i), trim(species_name(i))
       enddo
    endif

    ! broadcast lewis numbers
    call MPI_Bcast(Le,n_spec,MPI_REAL8,0,gcomm,ierr)

    if(myid.eq.0) then
       call write_header(io,'-')
    endif

    ! set Schmidt Numbers
    InvSc(:) = 1.0/(Le(:)*Pr)

    !Added by Ramanan Sankaran - 01/03/05
    !Coefficient for Lambda
    ! A = 2.58e-4  r = 0.7
    lambda_coeff = ((t_ref/298.0) ** 0.7) * &
                   2.58e-4 * 1e4 * cp_ref * 1e-5 / &
                   (rho_ref * a_ref * a_ref * a_ref * l_ref / t_ref)

    initialized_transport = .true.

    return

!!$-------------------------------------------------------------------

!!$ format statements
9   format(f6.3,1x,a10)

!!$ ERROR CONDITIONS - this will execute by process 0 if there is an
!!$ error reading the lewis number file.  All other processes will call
!!$ "check_term_status" above.  Thus, if there is an error reading the file, 
!!$ all processes will detect that.
2   continue
    write(io,*) 'there are not enough lewis numbers in the file lewis.in'
    write(io,*) 'for all of the species in the specified reaction mechanism'
    term_status=1
    call check_term_status(io,0)

  end subroutine initialize_transport


!!$==================================================================================


  subroutine computeCoefficients( Cp, T )
  ! Modified by Ramanan Sankaran - 01/03/05
  ! Computes lambda and viscosity in non-dimensional quantities
    !---------------------------------------------------------------------------
    ! Calculate the thermal conductivity and viscosity at each grid point
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! lambda = Cp*A*(T/To)^r    [erg/(cm*K*s)]
    !
    !             A = 2.58e-4           [g/(cm*s)]
    !             r = 0.7
    !             Cp = SUM{Cp_i*Y_i}    [erg/(g*K)]
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !   see Smooke & Giovangigli, "Lect. Notes Phys." 384:1-28 (1991)
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! mu = Pr*lambda/Cp          [g/(cm*s)]
    !---------------------------------------------------------------------------
    real, intent(in), dimension(nx,ny,nz) :: Cp, T
!** Constants A and r=0.7 are used earlier when lambda_coeff is set

!    real, parameter :: A = 2.58e-4   ! g/(cm*s)
!    real, parameter :: r = 0.7
!    real, parameter :: To = 298.0
!!integer :: i,j
!!integer, parameter :: DELTA=5    ! number of grid points over which viscosity increases by a factor of fmax
!!real, parameter :: fmax=10.0     ! factor to increase viscosity over DELTA grid points

!    lambda    = Cp*A*(T/To)**r
    lambda = 1.0

    viscosity = Pr*lambda/Cp

! increase viscosity near outflow boundaries to help stability.
!!$    do j=1,DELTA
!!$       i = nx-DELTA+j;
!!$       viscosity(i,:,:) = viscosity(i,:,:) * exp(real(j)/real(DELTA)*log(fmax));
!!$       viscosity(j,:,:) = viscosity(j,:,:) * exp(real(DELTA-j+1)/real(DELTA)*log(fmax));
!!$       i = ny-DELTA+j;
!!$       viscosity(:,i,:) = viscosity(:,i,:) * exp(real(j)/real(DELTA)*log(fmax));
!!$       viscosity(:,j,:) = viscosity(:,j,:) * exp(real(DELTA-j+1)/real(DELTA)*log(fmax));
!!$    enddo

    return
  end subroutine computeCoefficients


!!$==================================================================================


  subroutine computeSpeciesDiffFlux( grad_Ys)
!----------------------------------------------------------------------
!Changes 
! - Ramanan Sankaran 01/05/05
! Rather than store diffflux in a separate array, 
! the modified version will overwrite grad_Ys
!  Just so the code is readable, i have a pointer diffFlux => grad_Ys
!  But remember that both refer to the same memory location
! 

    !---------------------------------------------------------------------------
    ! J_i = -D * dY/dx_i
    !     = mu/Sc * dY/dx_i
    !     = mu/(Le*Pr) * dY/dx_i
    !
    ! NOTE that the diffusion fluxes of all species must sum to zero.
    !      To ensure this, we compute the diffusion flux of the last
    !      species using this constraint.
    !---------------------------------------------------------------------------
    real, intent(inout),  target, dimension(nx,ny,nz,n_spec,3) :: grad_Ys
    real, pointer, dimension(:,:,:,:,:) :: diffFlux
    integer :: m,n
    integer i, j, k

    diffFlux => grad_Ys

    diffFlux(:,:,:,n_spec,:) = 0.0

    DIRECTION: do m=1,3
       SPECIES: do n=1,n_spec-1

          ! compute diffusive flux for species n in direction m.
          diffFlux(:,:,:,n,m) = -viscosity(:,:,:)*InvSc(n) * grad_Ys(:,:,:,n,m)

          ! compute contribution to nth species diffusive flux
          ! this will ensure that the sum of the diffusive fluxes is zero.
          diffFlux(:,:,:,n_spec,m) = diffFlux(:,:,:,n_spec,m) - diffFlux(:,:,:,n,m)

       enddo SPECIES
    enddo DIRECTION
    return
  end subroutine computeSpeciesDiffFlux


!----------------------------------------------------------------------
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
       COLUMN: do n=m+1,3
          tau(:,:,:,m,n) = viscosity(:,:,:) * ( grad_u_tmp(:,:,:,m,n) + grad_u_tmp(:,:,:,n,m) )
          tau(:,:,:,n,m) = tau(:,:,:,m,n)
       enddo COLUMN
    enddo ROW
#else
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

  subroutine computeHeatFlux( grad_T, enthalpy, diffFlux)
!Rewritten by Ramanan Sankaran - 01/05/05
!  To save memory this function was rewritten.
!  Now heatFlux is returned overwriting grad_T
!  Just so the code is readable, i have a pointer heatFlux=>grad_T
!  But remember that , now, both are the same memory locations
!----------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! Compute the heat flux vector given by:
    !
    !    q_i = -mu*Cp/Pr * dT/dx_i + SUM[ h_j*J_i,j ]
    !        = -lambda   * dT/dx_i + SUM[ h_j*J_i,j ]
    !
    ! With
    !   mu      - viscosity
    !   lambda  - mixture thermal conductivity
    !   Cp      - mixture heat capacity
    !   dT/dx_i - temperature gradient in direction i
    !   h_j     - enthalpy of species j
    !   J_i,j   - diffusion flux of species j in direction i
    !---------------------------------------------------------------------------
    real, intent(inout),  target, dimension(nx,ny,nz,3)        :: grad_T
    real, intent(in),  dimension(nx,ny,nz,n_spec)   :: enthalpy
    real, intent(in),  dimension(nx,ny,nz,n_spec,3) :: diffFlux
    real, pointer, dimension(:,:,:,:)        :: heatFlux
    integer :: m,n

    heatFlux => grad_T

    DIRECTION: do m=1,3
       heatFlux(:,:,:,m) = -lambda(:,:,:) * grad_T(:,:,:,m)

     ! add in the mass diffusion component.
       SPECIES: do n=1,n_spec
          heatFlux(:,:,:,m) = heatFlux(:,:,:,m) + enthalpy(:,:,:,n)*diffFlux(:,:,:,n,m)
       enddo SPECIES

    enddo DIRECTION

    return
  end subroutine computeHeatFlux


!!$==================================================================================


  subroutine allocate_transport_arrays(mode)
    implicit none
    integer, intent(in) :: mode
    select case (mode)
    case(1)  ! allocate space
       allocate( viscosity(nx,ny,nz), lambda(nx,ny,nz), Le(n_spec), InvSc(n_spec) )
    case(-1) ! deallocate space
       deallocate( viscosity, lambda, Le, InvSc )
    end select
    return
  end subroutine allocate_transport_arrays


!!$==================================================================================


end module transport_m




!====================================================================================
!====================================================================================



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
! with the mixture average transport module
!----------------------------------------------------------------------
! Changed the argument list to be consistent with mixture averaged transport
!----------------------------------------------------------------------
! Fluxes are now stored in the same memory location as the gradients
!----------------------------------------------------------------------


subroutine getDiffusiveFluxTerms & 
            ( grad_u, T, grad_T, Ys, grad_Ys, Press, rho, Cp, h_i)
  !-----------------------------------------------------------------------------
  ! Interface to the transport package.  This interface is for the "old"
  ! transport scheme in S3D, where constant Le is assumed, along with 
  ! temperature dependent thermal conductivity and viscosity.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! NOTE: All quantities in this routine are assumed to be in CGS units!!!
  !       That means that you must dimensionalize all quantities before passing
  !       them into this routine, and then nondimensionalize after returning.
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! INPUT:
  !       grad_u  - velocity gradient tensor                    [1/s]
  !       T       - temperature                                 [K]
  !       grad_T  - Temperature gradient                        [K/cm]
  !       grad_Ys - species mass fraction gradient              [1/cm]
  !       rho     - mass density                                [g/cm^3]
  !       Cp      - mixture heat capacity at constant pressure  [erg/(g*K)]
  !       h_i     - species enthalpies                          [erg/g]
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! OUTPUT: NOT REAL ARGUMENTS, STORED IN INPUT ARRAYS - RAMANAN(01/05/05)
  !       tau     - stress tensor                               [g/(cm*s^2)]
  !       diffFlux - species mass diffusion fluxes              [g/(cm^2*s)]
  !       heatFlux - heat flux vector                           [erg/(cm^2*s)]
  !-----------------------------------------------------------------------------
  use transport_m
  use param_m, only : nx,ny,nz,n_spec

  implicit none

  real, intent(inout), dimension(nx,ny,nz,3,3)      :: grad_u
  real, intent(in), dimension(nx,ny,nz    )      :: Press  ! not used
  real, intent(in), dimension(nx,ny,nz    )      :: T, rho, Cp
  real, intent(inout), dimension(nx,ny,nz,3  )      :: grad_T
  real, intent(in), dimension(nx,ny,nz,n_spec )  :: Ys ! Not used
  real, intent(in), dimension(nx,ny,nz,n_spec )  :: h_i
  real, intent(inout), dimension(nx,ny,nz,n_spec,3) :: grad_Ys

!  if(.not. initialized_transport)  call initializeTransport(6)

  call computeCoefficients( Cp, T )                         ! set mu, lambda
  call computeStressTensor( grad_u)                         ! calculate stress tensor
  call computeSpeciesDiffFlux( grad_Ys)                     ! calculate diffusion fluxes
  call computeHeatFlux( grad_T, h_i, grad_Ys)   ! calcualte heat flux

  return
end subroutine getDiffusiveFluxTerms

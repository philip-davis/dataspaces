#include "globalDefines.h"
!=========================================================================================
  subroutine calc_heat_release(rr_r,hr)
!=========================================================================================
! computes the non-dimensional heat release
! Evatt Hawkes March 2010 - fixing constant cp assumption
!-----------------------------------------------------------------------------------------
  use param_m, only : nx, ny, nz, n_spec

!  Evatt Hawkes March 2010
!  use thermchem_m, only : h_chem
  use thermchem_m, only : calc_specEnth_allpts
  use variables_m, only : temp

  implicit none
!-----------------------------------------------------------------------------------------
  real, intent(in) :: rr_r(nx,ny,nz,n_spec)
  real, intent(out) :: hr(nx,ny,nz)

! local declarations

  integer i, j, k, L

! Evatt Hawkes March 2010
  real :: hs(nx,ny,nz,n_spec)

!-----------------------------------------------------------------------------------------

! Evatt Hawkes March 2010

  call calc_specEnth_allpts(temp, hs)
  
! zero heat release

  hr = 0.0
 
  do L = 1, n_spec
! Evatt Hawkes March 2010 
!    hr(:,:,:) = hr(:,:,:) + rr_r(:,:,:,L) * h_chem(L)
    hr(:,:,:) = hr(:,:,:) + rr_r(:,:,:,L) * hs(:,:,:,L)
  enddo
!-----------------------------------------------------------------------------------------
  return
  end

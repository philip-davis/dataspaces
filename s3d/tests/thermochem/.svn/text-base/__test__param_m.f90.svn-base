!===============================================================================!
!    Program: s3d
!    File:    $HeadURL$
!
!    S3D was developed at SNL (CRF) under the direction of Dr. J.H. Chen
! 
!    This file
!    authored by:   Ray W. Grout (National Renewable Energy Laboratory)
!                           ray.grout@nrel.gov
!
!    Modified by:   
!
!                 On:   04Oct2011
!
!    Description:   Stripped down parameter module from s3d for building unit tests
!
!
!    Last Revision: $Rev$
!    Revision by:   $Author$
!    Date:              $Date$
!===============================================================================

module param_m

  implicit none
  integer nx          !local number of grid points in x-direction
  integer ny          !local number of grid points in y-direction
  integer nz          !local number of grid points in z-direction

  integer nx_g        !global number of grid points in x-direction
  integer ny_g        !global number of grid points in y-direction
  integer nz_g        !global number of grid points in z-direction

  integer iorder      !order of derivatives
  integer iforder     !order of filter

  integer nxm_g       !miscelaneous for compatibility checks
  integer nym_g       !miscelaneous for compatibility checks
  integer nzm_g       !miscelaneous for compatibility checks

  integer vary_in_x   !switch to turn on x-direction
  integer vary_in_y   !switch to turn on y-direction
  integer vary_in_z   !switch to turn on z-direction

  integer periodic_x  !switch for periodicity in x-direction
  integer periodic_y  !switch for periodicity in y-direction
  integer periodic_z  !switch for periodicity in z-direction

  integer npx         !number of processors in x-direction
  integer npy         !number of processors in y-direction
  integer npz         !number of processors in z-direction

  integer n_spec
  integer nsc
  integer nvar_tot

end module param_m

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
!    Description:   Modified version of s3d write_header to reduce unit test dependencies
!
!
!    Last Revision: $Rev$
!    Revision by:   $Author$
!    Date:              $Date$
!===============================================================================

subroutine write_header(io,char_in)
  implicit none

  integer io
  character*1 char_in

  character*1 char(78)

  char(:)=char_in
  write(io,*) char(:)

  return
end subroutine write_header


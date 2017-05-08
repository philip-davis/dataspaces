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
!    Description:   Modified version of s3d terminate_run to reduce unit test dependencies
!
!
!    Last Revision: $Rev$
!    Revision by:   $Author$
!    Date:              $Date$
!===============================================================================

!=========================================================================================
subroutine terminate_run(io,flag)
  !=========================================================================================
  ! routine terminates run cleanly with respect to MPI and writing savefiles
  ! this routine MUST be called from outside any if(myid.eq.0) statements
  ! in other words, it should be called from all processors
  !-----------------------------------------------------------------------------------------


  implicit none
  include 'mpif.h'
  !-----------------------------------------------------------------------------------------
  ! declarations passed in

  integer io    !io unit
  integer flag  !0 for simple termination, 1 for termination with write savefile
  !-----------------------------------------------------------------------------------------
  integer ierr, myid
  ! simple termination


  call MPI_Barrier(MPI_COMM_WORLD,ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD,myid,ierr)

  if(myid==0) then
     write(io,*) 'terminating run with a simple termination...'
     write(io,*)
  endif


  call MPI_Finalize(ierr)

  stop

  !-----------------------------------------------------------------------------------------
  return
end subroutine terminate_run


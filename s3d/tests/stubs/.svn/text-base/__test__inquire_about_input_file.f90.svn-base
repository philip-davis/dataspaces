  subroutine inquire_about_input_file(filename,io)
!=========================================================================================
! routine inquires about the existence of an input file
! and terminates the run cleanly if it doesn't exist
! this routine MUST be called from outside of any if(myid.eq.0) statements
! routine works best when filename passed in character*100 (which is the convention)
!-----------------------------------------------------------------------------------------
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  character*100 filename
  integer io

! local declarations

  logical exist
!-----------------------------------------------------------------------------------------
! inquire about existence of file for myid=0

  if(myid.eq.0) then

    inquire(file=trim(filename),exist=exist)

    if(.not.exist) then   !does not exist

      write(io,*) 'the following input file does not exist:'
      write(io,*) trim(filename)

      term_status=1

    endif

  endif

! check termination status and terminate if file doesn't exist

  call check_term_status(io,0)
!----------------------------------------------------------------------------------------
  return
  end subroutine inquire_about_input_file


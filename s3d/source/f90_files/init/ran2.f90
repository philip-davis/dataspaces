!=========================================================================================
! These routines are different from the ones in turbulence_m.f90
! This really uses the seed provided in the input file 
! - Ramanan (05/12/03)
!=========================================================================================
  real function ran2()
!=========================================================================================
! contains platform-independent calls to random number generators
! initialization of seed should be done before calls to this routine
!-----------------------------------------------------------------------------------------
  implicit none
  real(kind=4) rannum

! local declarations

! calculate random number
  call random_number(rannum)
  ran2=rannum
!-----------------------------------------------------------------------------------------
  return
  end function ran2

!-----------------------------------------------------------------------------------------
  subroutine initialize_rnd_seed(seed)
  implicit none 
  integer, intent(in):: seed

! local declarations
  integer sdsz, k
  integer, allocatable, dimension(:) :: mysd

  call random_seed(SIZE=sdsz)
  allocate(mysd(1:sdsz))

  call random_seed   
  call random_seed(GET=mysd)
  mysd(1) = seed
  do k=2,sdsz
   mysd(k) = 0 
  end do
  call random_seed(PUT=mysd)

  return
  end subroutine initialize_rnd_seed
!-----------------------------------------------------------------------------------------


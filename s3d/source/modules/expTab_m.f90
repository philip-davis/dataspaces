#include "globalDefines.h"
!=========================================================================================
!                               'expTab_m'
!                       James Sutherland, May 2003
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! this module provides tools to tabulate the exponential function
!
! USAGE:  This module may be used in one of 2 ways:
!  1. Use the module where appropriate and calculate
!
!=========================================================================================
module expTab_m
  !---------------------------------------------------------------------------------------
  ! y = exp(x)
  !   ~ aa*x + bb
  !
  ! store "aa" and "bb" for a range of x.
  !---------------------------------------------------------------------------------------
  implicit none
  private

  logical :: initExpTable = .false.
  integer :: n_expTab

!-- set table range
  real(8), parameter :: x_hibound = 200.0, x_lobound=-1000.0

!-- set table spacing (based on desired accuracy for interpolated results)
  real(8), parameter :: dx = 1.0e-3
  real(8), parameter :: invDx = 1.0/dx

  type COEF_TYPE
     real :: aa, bb
  end type COEF_TYPE

  type(COEF_TYPE), allocatable, dimension(:) :: expCoef

  public :: expTab        ! internal access function
  public :: setExpCoefs   ! setup routine

!=========================================================================================

contains

!=========================================================================================

  subroutine setExpCoefs
    implicit none
    integer :: i
    real :: x1, x2, f1, f2

    n_expTab = ceiling( (x_hibound - x_lobound)/dx ) + 1

    allocate( expCoef(n_expTab) )

    x1 = x_lobound
    do i=1,n_expTab
       x2 = x1+dx
       f1 = exp(x1)
       f2 = exp(x2)
       expCoef(i)%aa = (f2-f1)*invDx
       expCoef(i)%bb = f1 - (f2-f1)*invDx*x1
       x1 = x1 + dx
    enddo
    initExpTable = .true.

!!$    print*,'Using ',n_expTab,' points in EXP table.'
!!$    print*,'table spacing is: ',dx
!!$    print*,'Table bounds on x: ',x_lobound,x_hibound

    return
  end subroutine setExpCoefs

!=========================================================================================

  real function expTab( z )
    implicit none
    real(8), intent(in) :: z
    integer :: i
    if(.not. initExpTable)  call setExpCoefs

!-- deal with points outside tabulated range:
    if(z > x_hibound .or. z < x_lobound ) then
!       write(*,*)'z is out of range!',z,x_lobound,x_hibound
       expTab = exp(z)
       return
    endif

!-- initialize the table if necessary
    if(.not. initExpTable)  call setExpCoefs

!-- perform the table look-up
    i = int((z-x_lobound)*invDx)

!-- Do the linear interpolation
    expTab = expCoef(i)%aa * z + expCoef(i)%bb

    return
  end function expTab

!=========================================================================================

end module expTab_m


!=========================================================================================
real function mexp( z )
!
! given z, this function returns the exponential, exp(z)
!
  use expTab_m
  implicit none
  real(8), intent(in) :: z

  mexp = expTab(z)

  return
end function mexp
!=========================================================================================


!!$program tester
!!$  implicit none
!!$  real(8), parameter :: x_lo=-126.5, x_hi=30.141
!!$  integer, parameter :: n=100
!!$
!!$  real(8), dimension(n) :: y,yexact,relerr
!!$  real :: dx, x
!!$
!!$  real(8), external :: mexp
!!$
!!$  integer :: i
!!$
!!$  dx = (x_hi-x_lo)/(n + 1)
!!$  x = x_lo
!!$  do i=1,n
!!$     y(i) = mexp(x)
!!$     yexact(i) = exp(x)
!!$     x = x + dx
!!$  enddo
!!$
!!$  relerr = (y-yexact)/yexact
!!$
!!$  x = maxval(abs(relerr)) 
!!$  write(*,*)'Maximum relative error: ',x
!!$
!!$  do i=1,n
!!$     if(abs(relerr(i))==x) then
!!$        write(*,*)'-------------------------'
!!$        write(*,*)'max rel. err: ',relerr(i)
!!$        write(*,*)'y_exact: ',yexact(i)
!!$        write(*,*)'y_tbl: ',y(i)
!!$        write(*,*)'-------------------------'
!!$     end if
!!$  end do
!!$
!!$end program tester

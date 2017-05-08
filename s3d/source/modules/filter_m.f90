#include "globalDefines.h"
!  Bug fix by Ramanan. 
!     The array subscripts in the Z-direction had a problem
!     This led to the solution blowing up. Bug is fixed now.
!  26-JUL-2005 Evatt Hawkes - vectorising code for X1E
!   Instead of the explicit loops, the f90 style array syntax is used
!   Both are equivalent. 
!   The array syntax solves vectorization problem on cray.
!
!  11-MAR-2005  Evatt Hawkes - this version with infiniband work-arounds
!               problems dealing with strided data-types
!
! Bug fix by Ramanan Sankaran (03/09/05)
! Filter operation now takes into account periodic boundary condition
!
! Bug fix by Ramanan Sankaran (01/31/05)
! Waits were moved from the tail end to just before the filter
! is applied in each direction. 
!Changes by Ramanan Sankaran (01/24/05)
!This revision tries to maximize the communication efficiency.  All the
!    sends and receives were being done using the non-blocking Isends
!    and Irecvs. To push the limits further, the waits for these to
!    complete is postponed to until they are actually needed.  The
!    wait for the sends to complete is postponed to just before
!    exiting the routine.  Until we exit the routine, there is no risk
!    of the data being overwritten.  So we dont have to wait idle
!    until the end of the routine.

!----------------------------------------------------------------------
! Revisions:
!
!   1) Update for nonuniform mesh?  how to do this????
!
!   29-NOV-2004  Evatt Hawkes
!           Possible bug in MPI - waits were called when send and receives were not
!           in cases when lnbr < 0 - not sure whether this was really buggy, 
!           but safety first
! 
!   06-05-03  James Sutherland fixed a parallel bug that gave MPI problems.
!             It was related to nonblocking vs. blocking sends & receives.
!             Also reordered some loops to make them faster.
!
!   11-12-02  Chris Kennedy fixed a bug in one coefficient (16th order filter) and
!             removed a small section of stagnant code

!=========================================================================================
  module filter_m
!=========================================================================================
! module for explicit filter variables

  implicit none
!-----------------------------------------------------------------------------------------
! integers

  integer i_time_fil      !frequency by which to filter
  integer nxfr            !something to do with compatibility check
  integer nxfr1           !something to do with compatibility check

! reals arrays

  real disb(10,20)        !filter arrays
  real disi(11)           !filter arrays

! ghost cell arrays

  real, allocatable :: neg_f_x(:,:,:), pos_f_x(:,:,:)
  real, allocatable :: neg_f_y(:,:,:), pos_f_y(:,:,:)
  real, allocatable :: neg_f_z(:,:,:), pos_f_z(:,:,:)

! Evatt Hawkes infiniband workarounds
  real, allocatable :: neg_f_xs(:,:,:), pos_f_xs(:,:,:)
  real, allocatable :: neg_f_ys(:,:,:), pos_f_ys(:,:,:)
!-----------------------------------------------------------------------------------------
  contains
!=========================================================================================
  subroutine allocate_filter_arrays(flag)
!=========================================================================================
! allocate filter arrays
!-----------------------------------------------------------------------------------------
  use param_m, only : nx, ny, nz, iforder

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed

  integer flag
!-----------------------------------------------------------------------------------------
! filter arrays

  if(flag.eq.1) then

      allocate(neg_f_x(1+iforder/2,ny,nz))
      allocate(pos_f_x(1+iforder/2,ny,nz))

      allocate(neg_f_y(nx,1+iforder/2,nz))
      allocate(pos_f_y(nx,1+iforder/2,nz))

      allocate(neg_f_z(nx,ny,1+iforder/2))
      allocate(pos_f_z(nx,ny,1+iforder/2))

!     Evatt Hawkes inifiniband workarounds

      allocate(neg_f_xs(1+iforder/2,ny,nz))
      allocate(pos_f_xs(1+iforder/2,ny,nz))

      allocate(neg_f_ys(nx,1+iforder/2,nz))
      allocate(pos_f_ys(nx,1+iforder/2,nz))

  elseif(flag.eq.-1) then

      deallocate(neg_f_x)
      deallocate(pos_f_x)

      deallocate(neg_f_y)
      deallocate(pos_f_y)

      deallocate(neg_f_z)
      deallocate(pos_f_z)

!     Evatt Hawkes inifiniband workarounds

      deallocate(neg_f_xs)
      deallocate(pos_f_xs)

      deallocate(neg_f_ys)
      deallocate(pos_f_ys)

  endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine allocate_filter_arrays
!=========================================================================================
  subroutine initialize_filter(io)
!=========================================================================================
! routine initializes filter
!-----------------------------------------------------------------------------------------
  use topology_m, only : myid
  use param_m, only : nx, nxm, periodic_x, vary_in_x, iforder

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing filter module...'
    write(io,*)
  endif
!-----------------------------------------------------------------------------------------
! allocate arrays

  call allocate_filter_arrays(1)
!-----------------------------------------------------------------------------------------
! initialize filter

  if(iforder.eq.0) then
    nxfr = nx
    nxfr1= nx-1
  endif

  if((iforder.ne.0).and.(periodic_x.ne.1)) then
    if((iforder.le.-4).and.(myid.eq.0)) then
      write(io,8050) -iforder
    endif
    nxfr = nxm / 8
    nxfr1= nx - nxfr + 1
  endif

  if(iforder.ge.2) then
    if(myid.eq.0) then
      write(io,8060) iforder
    endif
    nxfr=nx
    nxfr1=nx-1
    call set_filter_coefs(io)
  endif

  if((vary_in_x.eq.1)) then
    if ( nxfr .lt. 5 .or. nxfr1 .lt. 5 .or. nx .lt. 11 ) then
      if (myid.eq.0) then
        write(io,9060) nxfr,nxfr1,nx
      endif
      call terminate_run(io,0)  !must be called by all processors
    endif
  endif
!----------------------------------------------------------------------------------------
! write i_time_fil

  if(myid.eq.0) then
    if(i_time_fil.eq.1) then
      write(io,*) 'filter will be applied every time step'
    else
      write(io,1) 'filter will be applied every ',i_time_fil, ' time steps'
    endif
  endif
1 format(a30,i5,a11)
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!-----------------------------------------------------------------------------------------
! format statements

  8050 format(' using implicit filter of order',i3)
  8060 format(' using explicit filter of order',i3)
  9060 format(/' error: bad grid parameter. nxfr, nxfr1, nx ',3i5)
!-----------------------------------------------------------------------------------------
  return
  end subroutine initialize_filter
!=========================================================================================
  subroutine set_filter_coefs(io)
!=========================================================================================
! defines the dissipation filter coefficients
!
! filters basically work like this:
! feed in the U-vector at the end of every step and do this to it:
!
! U_filt = (I - alpha_D*D)*U_unfilt
!
! where alpha_D is just for scaling and D is the dissipation matrix.
! D is based on finite-difference derivatives like d^{2n}/dx^{2n}.
! A 10th-order filter uses a D based on d^{10}/dx^{10} stencils.
!
! alpha    - Scaling constant for Dissipation matrix coefficients
! disb     - Dissipation matrix coefficients for the boundary grid points
! disi     - Dissipation matrix coefficients for the interior grid points
! iforder  - Filter order
! iordd2   - Filter order divided by 2
! iordd2p1 - Filter order divided by 2 + 1
!-----------------------------------------------------------------------------------------
  use param_m, only : iforder

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  real alpha

  integer i
  integer ii
  integer iordd2
  integer iordd2p1
  integer j
  integer jj
!-----------------------------------------------------------------------------------------
!  HIGH-ORDER SHAPIRO?(Lars ERIKSSON?) FILTERS
!
!  NOTE: Pascal's Triangle/Binomial expansion
!            1
!             1     1
!  disi(2nd)       1     2     1
!           1     3     3     1
!  disi(4th)     1     4     6     4     1
!         1     5    10    10     5     1
!  disi(6th)   1     6    15    20    15     6     1
!       1     7    21    35    35     21    7     1
!  disi(8th) 1     8    28    56    70    56    28     8     1
!
!
!        EXAMPLE
!    -1   2  -1   0   0   0   0 (2nd)
!     2  -5   4  -1   0   0   0 (2nd) Note symmetry about the diagonal
!    -1   4  -6   4  -1   0   0 (4th)  and sum of coeff's =0.
!     0  -1   4  -6   4  -1   0
!     0   0  -1   4  -6   4  -1
!     0   0   0  -1   4  -5   2
!     0   0   0   0  -1   2  -1
!
! Dissipation matrix will be symmetric. Symmetric matricies have real
! eigenvalues. If the scheme is stable then all eigenvalues occur
! in quadrants 2 & 3 of the complex plane (LHP). Hence (Ui.Dij.Uj)=
! U1*U1*lambda1 +U2*U2*lambda2 +U3*U3*lambda3 + .... < 0 since all
! lambda's are less than zero and Ui*Ui is always > 0.
!-----------------------------------------------------------------------------------------
! check for odd filter choices

  if(mod(iforder,2).ne.0) then
    write(io,9020)
    call terminate_run(io,0)  !must be called by all processors
  endif
!-----------------------------------------------------------------------------------------
! form coefficient alpha

  iordd2 = iforder / 2
  iordd2p1 = iordd2 + 1
  alpha = ( - 1.0)**iordd2 / real ( 2**iforder )

! form dissipation matrix

  do i = 1, 10
    disi(i) = 0.0
    do j = 1, 20
      disb(i,j) = 0.0
    enddo
  enddo

  disi(11) = 0.0
!-----------------------------------------------------------------------------------------
! 2nd order inner with 1st order boundaries

  if ( iforder .eq. 2 ) then

    disi(2) = -1.0
    disi(1) = +2.0

    disb(1,1) = +1.0
    disb(1,2) = -1.0

! 4th order inner with 2nd order boundaries

  elseif ( iforder .eq. 4 ) then

    disi(3) = -1.0
    disi(2) = +4.0
    disi(1) = -6.0

    disb(1,1) = -1.0
    disb(1,2) = +2.0
    disb(1,3) = -1.0

    disb(2,1) = +2.0
    disb(2,2) = -5.0
    disb(2,3) = +4.0
    disb(2,4) = -1.0

! 6th order inner with 3rd order boundaries

  elseif ( iforder .eq. 6 ) then

    disi(4) = -1.0
    disi(3) = +6.0
    disi(2) = -15.0
    disi(1) = +20.0

    disb(1,1) =  1.0
    disb(1,2) = -3.0
    disb(1,3) =  3.0
    disb(1,4) = -1.0

    disb(2,1) = -3.0
    disb(2,2) = +10.0
    disb(2,3) = -12.0
    disb(2,4) = +6.0
    disb(2,5) = -1.0

    disb(3,1) = +3.0
    disb(3,2) = -12.0
    disb(3,3) = +19.0
    disb(3,4) = -15.0
    disb(3,5) = +6.0
    disb(3,6) = -1.0

! 8th order inner with 4th order boundaries

  elseif ( iforder .eq. 8 ) then

    disi(5) = -1.0
    disi(4) = +8.0
    disi(3) = -28.0
    disi(2) = +56.0
    disi(1) = -70.0

    disb(1,1) = -1.0
    disb(1,2) = +4.0
    disb(1,3) = -6.0
    disb(1,4) = +4.0
    disb(1,5) = -1.0

    disb(2,1) = +4.0
    disb(2,2) = -17.0
    disb(2,3) = +28.0
    disb(2,4) = -22.0
    disb(2,5) = +8.0
    disb(2,6) = -1.0

    disb(3,1) = -6.0
    disb(3,2) = +28.0
    disb(3,3) = -53.0
    disb(3,4) = +52.0
    disb(3,5) = -28.0
    disb(3,6) = +8.0
    disb(3,7) = -1.0

    disb(4,1) = +4.0
    disb(4,2) = -22.0
    disb(4,3) = +52.0
    disb(4,4) = -69.0
    disb(4,5) = +56.0
    disb(4,6) = -28.0
    disb(4,7) = +8.0
    disb(4,8) = -1.0

! 10th order inner with 5th order boundaries

  elseif ( iforder .eq. 10 ) then

    disi(6) = -1.0
    disi(5) = +10.0
    disi(4) = -45.0
    disi(3) = +120.0
    disi(2) = -210.0
    disi(1) = +252.0

    disb(1,1) = +1.0
    disb(1,2) = -5.0
    disb(1,3) = +10.0
    disb(1,4) = -10.0
    disb(1,5) = +5.0
    disb(1,6) = -1.0

    disb(2,1) = -5.0
    disb(2,2) = +26.0
    disb(2,3) = -55.0
    disb(2,4) = +60.0
    disb(2,5) = -35.0
    disb(2,6) = +10.0
    disb(2,7) = -1.0

    disb(3,1) = +10.0
    disb(3,2) = -55.0
    disb(3,3) = +126.0
    disb(3,4) = -155.0
    disb(3,5) = +110.0
    disb(3,6) = -45.0
    disb(3,7) = +10.0
    disb(3,8) = -1.0

    disb(4,1) = -10.0
    disb(4,2) = +60.0
    disb(4,3) = -155.0
    disb(4,4) = +226.0
    disb(4,5) = -205.0
    disb(4,6) = +120.0
    disb(4,7) = -45.0
    disb(4,8) = +10.0
    disb(4,9) = -1.0

    disb(5,1) = +5.0
    disb(5,2) = -35.0
    disb(5,3) = +110.0
    disb(5,4) = -205.0
    disb(5,5) = +251.0
    disb(5,6) = -210.0
    disb(5,7) = +120.0
    disb(5,8) = -45.0
    disb(5,9) = +10.0
    disb(5,10) = -1.0

! 12th order inner with 6th order boundaries (may be bug here)

  elseif ( iforder .eq. 12 ) then

    disi(7) = -1.0
    disi(6) = +12.0
    disi(5) = -66.0
    disi(4) = +220.0
    disi(3) = -495.0
    disi(2) = +792.0
    disi(1) = -924.0

    disb(1,1) = -1.0
    disb(1,2) = +6.0
    disb(1,3) = -15.0
    disb(1,4) = +20.0
    disb(1,5) = -15.0
    disb(1,6) = +6.0
    disb(1,7) = -1.0

    disb(2,1) = +6.0
    disb(2,2) = -37.0
    disb(2,3) = +96.0
    disb(2,4) = -135.0
    disb(2,5) = +110.0
    disb(2,6) = -51.0
    disb(2,7) = +12.0
    disb(2,8) = -1.0

    disb(3,1) = -15.0
    disb(3,2) = +96.0
    disb(3,3) = -262.0
    disb(3,4) = +396.0
    disb(3,5) = -360.0
    disb(3,6) = +200.0
    disb(3,7) = -66.0
    disb(3,8) = +12.0
    disb(3,9) = -1.0

    disb(4,1) = +20.0
    disb(4,2) = -135.0
    disb(4,3) = +396.0
    disb(4,4) = -662.0
    disb(4,5) = +696.0
    disb(4,6) = -480.0
    disb(4,7) = +220.0
    disb(4,8) = -66.0
    disb(4,9) = +12.0
    disb(4,10) = -1.0

    disb(5,1) = -15.0
    disb(5,2) = +110.0
    disb(5,3) = -360.0
    disb(5,4) = +696.0
    disb(5,5) = -887.0
    disb(5,6) = +786.0
    disb(5,7) = -495.0
    disb(5,8) = +220.0
    disb(5,9) = -66.0
    disb(5,10) = +12.0
    disb(5,11) = -1.0

    disb(6,1) = +6.0
    disb(6,2) = -51.0
    disb(6,3) = +200.0
    disb(6,4) = -480.0
    disb(6,5) = +786.0
    disb(6,6) = -923.0
    disb(6,7) = +792.0
    disb(6,8) = -495.0
    disb(6,9) = +220.0
    disb(6,10) = -66.0
    disb(6,11) = +12.0
    disb(6,12) = -1.0

! 14th order inner with 7th order boundaries

  elseif ( iforder .eq. 14 ) then

    disi(1) =  + 3432.0
    disi(2) =  - 3003.0
    disi(3) =  + 2002.0
    disi(4) =  - 1001.0
    disi(5) =  + 364.0
    disi(6) =  - 91.0
    disi(7) =  + 14.0
    disi(8) =  - 1.0

    disb(1,1)  = +1.0
    disb(1,2)  = -7.0
    disb(1,3)  = +21.0
    disb(1,4)  = -35.0
    disb(1,5)  = +35.0
    disb(1,6)  = -21.0
    disb(1,7)  = +7.0
    disb(1,8)  = -1.0

    disb(2,1)  = -7.0
    disb(2,2)  = +50.0
    disb(2,3)  = -154.0
    disb(2,4)  = +266.0
    disb(2,5)  = -280.0
    disb(2,6)  = +182.0
    disb(2,7)  = -70.0
    disb(2,8)  = +14.0
    disb(2,9)  = -1.0

    disb(3,1)  = +21.0
    disb(3,2)  = -154.0
    disb(3,3)  = +491.0
    disb(3,4)  = -889.0
    disb(3,5)  = +1001.0
    disb(3,6)  = -721.0
    disb(3,7)  = +329.0
    disb(3,8)  = -91.0
    disb(3,9)  = +14.0
    disb(3,10) = -1.0

    disb(4,1)  = -35.0
    disb(4,2)  = +266.0
    disb(4,3)  = -889.0
    disb(4,4)  = +1716.0
    disb(4,5)  = -2114.0
    disb(4,6)  = +1736.0
    disb(4,7)  = -966.0
    disb(4,8)  = +364.0
    disb(4,9)  = -91.0
    disb(4,10) = +14.0
    disb(4,11) = -1.0

    disb(5,1)  = +35.0
    disb(5,2)  = -280.0
    disb(5,3)  = +1001.0
    disb(5,4)  = -2114.0
    disb(5,5)  = +2941.0
    disb(5,6)  = -2849.0
    disb(5,7)  = +1981.0
    disb(5,8)  = -1001.0
    disb(5,9)  = +364.0
    disb(5,10) = -91.0
    disb(5,11) = +14.0
    disb(5,12) = -1.0

    disb(6,1)  = -21.0
    disb(6,2)  = +182.0
    disb(6,3)  = -721.0
    disb(6,4)  = +1736.0
    disb(6,5)  = -2849.0
    disb(6,6)  = +3382.0
    disb(6,7)  = -2996.0
    disb(6,8)  = +2002.0
    disb(6,9)  = -1001.0
    disb(6,10) = +364.0
    disb(6,11) = -91.0
    disb(6,12) = +14.0
    disb(6,13) = -1.0

    disb(7,1)  = +7.0
    disb(7,2)  = -70.0
    disb(7,3)  = +329.0
    disb(7,4)  = -966.0
    disb(7,5)  = +1981.0
    disb(7,6)  = -2996.0
    disb(7,7)  = +3431.0
    disb(7,8)  = -3003.0
    disb(7,9)  = +2002.0
    disb(7,10) = -1001.0
    disb(7,11) = +364.0
    disb(7,12) = -91.0
    disb(7,13) = +14.0
    disb(7,14) = -1.0

! 16th order inner with 8th order boundaries

  elseif ( iforder .eq. 16 ) then

    disi(1) =  - 12870.0
    disi(2) =  + 11440.0
    disi(3) =  - 8008.0
    disi(4) =  + 4368.0
    disi(5) =  - 1820.0
    disi(6) =  + 560.0
    disi(7) =  - 120.0
    disi(8) =  + 16.0
    disi(9) =  - 1.0

    disb(1,1)  = -1.0
    disb(1,2)  = +8.0
    disb(1,3)  = -28.0
    disb(1,4)  = +56.0
    disb(1,5)  = -70.0
    disb(1,6)  = +56.0
    disb(1,7)  = -28.0
    disb(1,8)  = +8.0
    disb(1,9)  = -1.0

    disb(2,1)  = +8.0
    disb(2,2)  = -65.0
    disb(2,3)  = +232.0
    disb(2,4)  = -476.0
    disb(2,5)  = +616.0
    disb(2,6)  = -518.0
    disb(2,7)  = +280.0
    disb(2,8)  = -92.0
    disb(2,9)  = +16.0
    disb(2,10) = -1.0

    disb(3,1)  = -28.0
    disb(3,2)  = +232.0
    disb(3,3)  = -849.0
    disb(3,4)  = +1800.0
    disb(3,5)  = -2436.0
    disb(3,6)  = +2184.0
    disb(3,7)  = -1302.0
    disb(3,8)  = +504.0
    disb(3,9)  = -120.0
    disb(3,10) = +16.0
    disb(3,11) = -1.0

    disb(4,1)  = +56.0
    disb(4,2)  = -476.0
    disb(4,3)  = +1800.0
    disb(4,4)  = -3985.0
    disb(4,5)  = +5720.0
    disb(4,6)  = -5572.0
    disb(4,7)  = +3752.0
    disb(4,8)  = -1750.0
    disb(4,9)  = +560.0
    disb(4,10) = -120.0
    disb(4,11) = +16.0
    disb(4,12) = -1.0

    disb(5,1)  = -70.0
    disb(5,2)  = +616.0
    disb(5,3)  = -2436.0
    disb(5,4)  = +5720.0
    disb(5,5)  = -8885.0
    disb(5,6)  = +9640.0
    disb(5,7)  = -7532.0
    disb(5,8)  = +4312.0
    disb(5,9)  = -1820.0
    disb(5,10) = +560.0
    disb(5,11) = -120.0
    disb(5,12) = +16.0
    disb(5,13) = -1.0

    disb(6,1)  = +56.0
    disb(6,2)  = -518.0
    disb(6,3)  = +2184.0
    disb(6,4)  = -5572.0
    disb(6,5)  = +9640.0
    disb(6,6)  = -12021.0
    disb(6,7)  = +11208.0
    disb(6,8)  = -7980.0
    disb(6,9)  = +4368.0
    disb(6,10) = -1820.0
    disb(6,11) = +560.0
    disb(6,12) = -120.0
    disb(6,13) = +16.0
    disb(6,14) = -1.0

    disb(7,1)  = -28.0
    disb(7,2)  = +280.0
    disb(7,3)  = -1302.0
    disb(7,4)  = +3752.0
    disb(7,5)  = -7532.0
    disb(7,6)  = +11208.0
    disb(7,7)  = -12805.0
    disb(7,8)  = +11432.0
    disb(7,9)  = -8008.0
    disb(7,10) = +4368.0
    disb(7,11) = -1820.0
    disb(7,12) = +560.0
    disb(7,13) = -120.0
    disb(7,14) = +16.0
    disb(7,15) = -1.0

    disb(8,1)  = +8.0
    disb(8,2)  = -92.0
    disb(8,3)  = +504.0
    disb(8,4)  = -1750.0
    disb(8,5)  = +4312.0
    disb(8,6)  = -7980.0
    disb(8,7)  = +11432.0
    disb(8,8)  = -12869.0
    disb(8,9)  = +11440.0
    disb(8,10) = -8008.0
    disb(8,11) = +4368.0
    disb(8,12) = -1820.0
    disb(8,13) = +560.0
    disb(8,14) = -120.0
    disb(8,15) = +16.0
    disb(8,16) = -1.0

! 18th order inner with 9th order boundaries

  elseif ( iforder .eq. 18 ) then

    disi(1)  =  + 48620.0
    disi(2)  =  - 43758.0
    disi(3)  =  + 31824.0
    disi(4)  =  - 18564.0
    disi(5)  =  + 8568.0
    disi(6)  =  - 3060.0
    disi(7)  =  + 816.0
    disi(8)  =  - 153.0
    disi(9)  =  + 18.0
    disi(10) =  - 1.0

    disb(1,1)  = +1.0
    disb(1,2)  = -9.0
    disb(1,3)  = +36.0
    disb(1,4)  = -84.0
    disb(1,5)  = +126.0
    disb(1,6)  = -126.0
    disb(1,7)  = +84.0
    disb(1,8)  = -36.0
    disb(1,9)  = +9.0
    disb(1,10) = -1.0
!
    disb(2,1)  = -9.0
    disb(2,2)  = +82.0
    disb(2,3)  = -333.0
    disb(2,4)  = +792.0
    disb(2,5)  = -1218.0
    disb(2,6)  = +1260.0
    disb(2,7)  = -882.0
    disb(2,8)  = +408.0
    disb(2,9)  = -117.0
    disb(2,10) = +18.0
    disb(2,11) = -1.0
!
    disb(3,1)  = +36.0
    disb(3,2)  = -333.0
    disb(3,3)  = +1378.0
    disb(3,4)  = -3357.0
    disb(3,5)  = +5328.0
    disb(3,6)  = -5754.0
    disb(3,7)  = +4284.0
    disb(3,8)  = -2178.0
    disb(3,9)  = +732.0
    disb(3,10) = -153.0
    disb(3,11) = +18.0
    disb(3,12) = -1.0

    disb(4,1)  = -84.0
    disb(4,2)  = +792.0
    disb(4,3)  = -3357.0
    disb(4,4)  = +8434.0
    disb(4,5)  = -13941.0
    disb(4,6)  = +15912.0
    disb(4,7)  = -12810.0
    disb(4,8)  = +7308.0
    disb(4,9)  = -2934.0
    disb(4,10) = +816.0
    disb(4,11) = -153.0
    disb(4,12) = +18.0
    disb(4,13) = -1.0

    disb(5,1)  = +126.0
    disb(5,2)  = -1218.0
    disb(5,3)  = +5328.0
    disb(5,4)  = -13941.0
    disb(5,5)  = +24310.0
    disb(5,6)  = -29817.0
    disb(5,7)  = +26496.0
    disb(5,8)  = -17346.0
    disb(5,9)  = +8442.0
    disb(5,10) = -3060.0
    disb(5,11) = +816.0
    disb(5,12) = -153.0
    disb(5,13) = +18.0
    disb(5,14) = -1.0

    disb(6,1)  = -126.0
    disb(6,2)  = +1260.0
    disb(6,3)  = -5754.0
    disb(6,4)  = +15912.0
    disb(6,5)  = -29817.0
    disb(6,6)  = +40186.0
    disb(6,7)  = -40401.0
    disb(6,8)  = +31032.0
    disb(6,9)  = -18480.0
    disb(6,10) = +8568.0
    disb(6,11) = -3060.0
    disb(6,12) = +816.0
    disb(6,13) = -153.0
    disb(6,14) = +18.0
    disb(6,15) = -1.0

    disb(7,1)  = +84.0
    disb(7,2)  = -882.0
    disb(7,3)  = +4284.0
    disb(7,4)  = -12810.0
    disb(7,5)  = +26496.0
    disb(7,6)  = -40401.0
    disb(7,7)  = +47242.0
    disb(7,8)  = -43425.0
    disb(7,9)  = +31788.0
    disb(7,10) = -18564.0
    disb(7,11) = +8568.0
    disb(7,12) = -3060.0
    disb(7,13) = +816.0
    disb(7,14) = -153.0
    disb(7,15) = +18.0
    disb(7,16) = -1.0

    disb(8,1)  = -36.0
    disb(8,2)  = +408.0
    disb(8,3)  = -2178.0
    disb(8,4)  = +7308.0
    disb(8,5)  = -17346.0
    disb(8,6)  = +31032.0
    disb(8,7)  = -43425.0
    disb(8,8)  = +48538.0
    disb(8,9)  = -43749.0
    disb(8,10) = +31824.0
    disb(8,11) = -18564.0
    disb(8,12) = +8568.0
    disb(8,13) = -3060.0
    disb(8,14) = +816.0
    disb(8,15) = -153.0
    disb(8,16) = +18.0
    disb(8,17) = -1.0

    disb(9,1)  = +9.0
    disb(9,2)  = -117.0
    disb(9,3)  = +732.0
    disb(9,4)  = -2934.0
    disb(9,5)  = +8442.0
    disb(9,6)  = -18480.0
    disb(9,7)  = +31788.0
    disb(9,8)  = -43749.0
    disb(9,9)  = +48619.0
    disb(9,10) = -43758.0
    disb(9,11) = +31824.0
    disb(9,12) = -18564.0
    disb(9,13) = +8568.0
    disb(9,14) = -3060.0
    disb(9,15) = +816.0
    disb(9,16) = -153.0
    disb(9,17) = +18.0
    disb(9,18) = -1.0

! 20th order inner with 10th order boundaries

  elseif ( iforder .eq. 20 ) then

    disi(1)  =  - 184756.0
    disi(2)  =  + 167960.0
    disi(3)  =  - 125970.0
    disi(4)  =  + 77520.0
    disi(5)  =  - 38760.0
    disi(6)  =  + 15504.0
    disi(7)  =  - 4845.0
    disi(8)  =  + 1140.0
    disi(9)  =  - 190.0
    disi(10) =  + 20.0
    disi(11) =  - 1.0

    disb(1,1)  = -1.0
    disb(1,2)  = +10.0
    disb(1,3)  = -45.0
    disb(1,4)  = +120.0
    disb(1,5)  = -210.0
    disb(1,6)  = +252.0
    disb(1,7)  = -210.0
    disb(1,8)  = +120.0
    disb(1,9)  = -45.0
    disb(1,10) = +10.0
    disb(1,11) = -1.0

    disb(2,1)  = +10.0
    disb(2,2)  = -101.0
    disb(2,3)  = +460.0
    disb(2,4)  = -1245.0
    disb(2,5)  = +2220.0
    disb(2,6)  = -2730.0
    disb(2,7)  = +2352.0
    disb(2,8)  = -1410.0
    disb(2,9)  = +570.0
    disb(2,10) = -145.0
    disb(2,11) = +20.0
    disb(2,12) = -1.0

    disb(3,1)  = -45.0
    disb(3,2)  = +460.0
    disb(3,3)  = -2126.0
    disb(3,4)  = +5860.0
    disb(3,5)  = -10695.0
    disb(3,6)  = +13560.0
    disb(3,7)  = -12180.0
    disb(3,8)  = +7752.0
    disb(3,9)  = -3435.0
    disb(3,10) = +1020.0
    disb(3,11) = -190.0
    disb(3,12) = +20.0
    disb(3,13) = -1.0

    disb(4,1)  = +120.0
    disb(4,2)  = -1245.0
    disb(4,3)  = +5860.0
    disb(4,4)  = -16526.0
    disb(4,5)  = +31060.0
    disb(4,6)  = -40935.0
    disb(4,7)  = +38760.0
    disb(4,8)  = -26580.0
    disb(4,9)  = +13152.0
    disb(4,10) = -4635.0
    disb(4,11) = +1140.0
    disb(4,12) = -190.0
    disb(4,13) = +20.0
    disb(4,14) = -1.0

    disb(5,1)  = -210.0
    disb(5,2)  = +2220.0
    disb(5,3)  = -10695.0
    disb(5,4)  = +31060.0
    disb(5,5)  = -60626.0
    disb(5,6)  = +83980.0
    disb(5,7)  = -85035.0
    disb(5,8)  = +63960.0
    disb(5,9)  = -36030.0
    disb(5,10) = +15252.0
    disb(5,11) = -4845.0
    disb(5,12) = +1140.0
    disb(5,13) = -190.0
    disb(5,14) = +20.0
    disb(5,15) = -1.0

    disb(6,1)  = +252.0
    disb(6,2)  = -2730.0
    disb(6,3)  = +13560.0
    disb(6,4)  = -40935.0
    disb(6,5)  = +83980.0
    disb(6,6)  = -124130.0
    disb(6,7)  = +136900.0
    disb(6,8)  = -115275.0
    disb(6,9)  = +75300.0
    disb(6,10) = -38550.0
    disb(6,11) = +15504.0
    disb(6,12) = -4845.0
    disb(6,13) = +1140.0
    disb(6,14) = -190.0
    disb(6,15) = +20.0
    disb(6,16) = -1.

    disb(7,1)  = -210.0
    disb(7,2)  = +2352.0
    disb(7,3)  = -12180.0
    disb(7,4)  = +38760.0
    disb(7,5)  = -85035.0
    disb(7,6)  = +136900.0
    disb(7,7)  = -168230.0
    disb(7,8)  = +162100.0
    disb(7,9)  = -124725.0
    disb(7,10) = +77400.0
    disb(7,11) = -38760.0
    disb(7,12) = +15504.0
    disb(7,13) = -4845.0
    disb(7,14) = +1140.0
    disb(7,15) = -190.0
    disb(7,16) = +20.0
    disb(7,17) = -1.0

    disb(8,1)  = +120.0
    disb(8,2)  = -1410.0
    disb(8,3)  = +7752.0
    disb(8,4)  = -26580.0
    disb(8,5)  = +63960.0
    disb(8,6)  = -115275.0
    disb(8,7)  = +162100.0
    disb(8,8)  = -182630.0
    disb(8,9)  = +167500.0
    disb(8,10) = -125925.0
    disb(8,11) = +77520.0
    disb(8,12) = -38760.0
    disb(8,13) = +15504.0
    disb(8,14) = -4845.0
    disb(8,15) = +1140.0
    disb(8,16) = -190.0
    disb(8,17) = +20.0
    disb(8,18) = -1.0

    disb(9,1)  = -45.0
    disb(9,2)  = +570.0
    disb(9,3)  = -3435.0
    disb(9,4)  = +13152.0
    disb(9,5)  = -36030.0
    disb(9,6)  = +75300.0
    disb(9,7)  = -124725.0
    disb(9,8)  = +167500.0
    disb(9,9)  = -184655.0
    disb(9,10) = +167950.0
    disb(9,11) = -125970.0
    disb(9,12) = +77520.0
    disb(9,13) = -38760.0
    disb(9,14) = +15504.0
    disb(9,15) = -4845.0
    disb(9,16) = +1140.0
    disb(9,17) = -190.0
    disb(9,18) = +20.0
    disb(9,19) = -1.0

    disb(10,1)  = +10.0
    disb(10,2)  = -145.0
    disb(10,3)  = +1020.0
    disb(10,4)  = -4635.0
    disb(10,5)  = +15252.0
    disb(10,6)  = -38550.0
    disb(10,7)  = +77400.0
    disb(10,8)  = -125925.0
    disb(10,9)  = +167950.0
    disb(10,10) = -184755.0
    disb(10,11) = +167960.0
    disb(10,11) = -125970.0
    disb(10,13) = +77520.0
    disb(10,14) = -38760.0
    disb(10,15) = +15504.0
    disb(10,16) = -4845.0
    disb(10,17) = +1140.0
    disb(10,18) = -190.0
    disb(10,19) = +20.0
    disb(10,20) = -1.0

  endif
!-----------------------------------------------------------------------------------------
! normalize boundary and interior elements
  do i = 1, iordd2
    do j = 1, iforder
      disb(i,j)= alpha * disb(i,j)
    enddo
  enddo

  do i = 1, iordd2p1
    disi(i) = alpha * disi(i)
  enddo
!-----------------------------------------------------------------------------------------
! format statements

  9020 format(' error in _set_filter_coefs_: iforder must be even')
!-----------------------------------------------------------------------------------------
  return
  end subroutine set_filter_coefs
!=========================================================================================
  subroutine filter(fn,io)
!=========================================================================================
! filters the U-vector using an explicit finite difference method.
!
! Filters basically work like this: Feed in the U-vector (U_unfilt)
! at the end of every step and do this to it -
!
! U_filt = (I - alpha_D*D)*U_unfilt
!
! where alpha_D is just for scaling and D is the dissipation matrix.
! D is based on finite-difference derivatives like d^{2n}/dx^{2n}.
! A 10th-order filter uses a D based on d^{10}/dx^{10} stencils.
! These filters are of order (iforder) for the interior and (iforder/2)
! at the boundary. Remember - in the interior, the stencils are symmetric
! but not for the boundary stencils. Details are in NASA TP-3484. Don't
! use filters higher than 12th-order until the bug is found - probably
! lack of sufficient precision. It's something subtle because the coeffs.
! are good.
!
! alpha    - Scaling constant for Dissipation matrix coefficients
! disb     - Dissipation matrix coefficients for the boundary grid points
! disi     - Dissipation matrix coefficients for the interior grid points
! dfn      - Filtered U-vector
! fn       - Unfilter U-vector
! iforder  - Filter order
! ka       - Interior filter stencil width left or right of center, i.e.
!            ....|.... is an 8th-order filter, is 9 wide, and ka=4
! kap1     - ka + 1
! kb       - Width of filter boundary stencil
! k1       - Left boundary filter stencil element
! k1a      - Right boundary filter stencil element
! k2       - Left boundary filter stencil number
! k2a      - Right boundary filter stencil number
!-----------------------------------------------------------------------------------------
  use param_m
  use topology_m
  use work_m, only : dfn => work1_1  ! alias dfn to work array

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io

  real  fn(nx,ny,nz)

! local declarations

  integer :: i, j, k
  integer :: k1, k1a, k2, k2a, ka, kap1, kb, kmag
  integer :: m, nnx, nny, nnz, nm
  integer, dimension(12) :: req=MPI_REQUEST_NULL
  integer, dimension(MPI_Status_size,12) :: stat
  integer, dimension(6) :: lnbr(6)

  
!-----------------------------------------------------------------------------------------
! initialize some variables

  nnx = nx
  nny = ny
  nnz = nz

  ka = iforder / 2
  kb = iforder
  kap1 = ka + 1

  lnbr = neighbor

!-----------------------------------------------------------------------------------------
! filter in x-direction

  if( nnx .eq. 1 ) go to 500

  if( vary_in_x .eq. 1 ) then

    dfn = 0.0

    if( nnx .lt. (iforder+1) ) then
      write(io,3700)
      return
    endif

!   zero neg_f_x and pos_f_x

    neg_f_x=0.0
    pos_f_x=0.0

!   get neighboring cells data

    if(periodic_x.eq.1) then
      if(xid.eq.0) then
         lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
      endif
      if(xid.eq.xpes-1)then
         lnbr(2) = zid*xpes*ypes+ yid*xpes+0
      endif
    endif


    if(lnbr(2).ge.0) then
      nm = nx - iforder/2

!     Evatt Hawkes inifiniband workarounds
!      call MPI_ISend(fn(nm,1,1),1,fxrows_type,lnbr(2),1,gcomm,req(1),ierr)
      neg_f_xs(:,:,:) = fn(nm:nx,:,:)
      call MPI_ISend(neg_f_xs(1,1,1),(ny*nz*(1+(iforder/2))),MPI_REAL8,&
                     lnbr(2),1,gcomm,req(1),ierr)

      call MPI_IRecv(pos_f_x,(ny*nz*(1+(iforder/2))),MPI_REAL8,  &
                     lnbr(2),2,gcomm,req(7),ierr)
    endif

    if(lnbr(1).ge.0) then
!     Evatt Hawkes inifiniband workarounds
!      call MPI_ISend(fn(1,1,1),1,fxrows_type,lnbr(1),2,gcomm,req(2),ierr)
      pos_f_xs(:,:,:) = fn(1:1+(iforder/2),:,:)
      call MPI_ISend(pos_f_xs(1,1,1),(ny*nz*(1+(iforder/2))),MPI_REAL8,&
                     lnbr(1),2,gcomm,req(2),ierr)
      call MPI_IRecv(neg_f_x,(ny*nz*(1+(iforder/2))),MPI_REAL8,  &
                     lnbr(1),1,gcomm,req(8),ierr)
    endif

!   interior filter in the x-direction


!   Evatt 26-JUL-2005 vectorizing
!   Instead of the explicit loops, the f90 style array syntax is used
!   Both are equivalent. 
!   The array syntax solves vectorization problem on cray.
    do i = kap1, nnx - ka
      do k = - ka, ka
        dfn(i,:,:) = dfn(i,:,:) + disi(1+abs(k)) * fn(i+k,:,:)
      enddo
    enddo


!   BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
    if( lnbr(1) >= 0 ) then
      call MPI_Wait(req(8),stat(:,8),ierr)
    endif

    if(lnbr(1).ge.0) then
      do i = 1,kap1-1
        do k = - ka, ka
          if((i+k).lt.1) then
            dfn(i,:,:) = dfn(i,:,:) + disi(1+abs(k)) * neg_f_x(kap1-abs(i+k),:,:)
          else
            dfn(i,:,:) = dfn(i,:,:) + disi(1+abs(k)) * fn(i+k,:,:)
          endif
        enddo
      enddo
    endif

    if( lnbr(2) >= 0 ) then
      call MPI_Wait(req(7),stat(:,7),ierr)
    endif

    if(lnbr(2).ge.0) then
      do i = nnx - ka+1, nnx
        do k = - ka, ka
          if((i+k).gt.nnx) then
            dfn(i,:,:) = dfn(i,:,:) + disi(1+abs(k)) * pos_f_x(i+k-nnx,:,:)
          else
            dfn(i,:,:) = dfn(i,:,:) + disi(1+abs(k)) * fn(i+k,:,:)
          endif
        enddo
      enddo
    endif

! boundary points

    if(lnbr(1).lt.0) then
      do k2 = 1, kb
        do k1 = 1, ka
          dfn(k1,:,:) = dfn(k1,:,:) + disb(k1,k2)*fn(k2,:,:)
        enddo
      enddo
    endif


    if(lnbr(2).lt.0) then
      do k2 = 1, kb
        k2a = nnx + 1 - k2
        do k1 = 1, ka
          k1a = nnx + 1 - k1
          dfn(k1a,:,:) = dfn(k1a,:,:) + disb(k1,k2)*fn(k2a,:,:)
        enddo
      enddo
    endif

    if( lnbr(2) >= 0 ) then
      call MPI_Wait(req(1),stat(:,1),ierr)
    endif
    if( lnbr(1) >= 0 ) then
      call MPI_Wait(req(2),stat(:,2),ierr)
    endif
!   apply filter

    do m = 1, nnz
      do j = 1, nny
        do i = 1, nnx
          fn(i,j,m) = fn(i,j,m) + dfn(i,j,m)
        enddo
      enddo
    enddo

  endif
!-----------------------------------------------------------------------------------------
! filter in y-direction

  500 continue

  if( nny .eq. 1 ) go to 510

  if( vary_in_y .eq. 1 ) then

    dfn = 0.0
    if( nny .lt. (iforder+1) ) then
      write(io,3705)
      return
    endif

!   zero neg_f_y and pos_f_y

    neg_f_y=0.0
    pos_f_y=0.0

!   get neighboring cells data
    if(periodic_y.eq.1) then
      if(yid.eq.0) then
        lnbr(3) = zid*xpes*ypes+(ypes-1)*xpes+xid
      endif
      if(yid.eq.ypes-1)then
        lnbr(4) = zid*xpes*ypes+(0     )*xpes+xid
      endif
    endif

    if(lnbr(4).ge.0) then
      nm = ny - iforder/2
!     Evatt Hawkes inifiniband workarounds
!      call MPI_ISend(fn(1,nm,1),1,fyrows_type,lnbr(4),1,gcomm,req(3),ierr)
      neg_f_ys(:,:,:)=fn(:,nm:ny,:)
      call MPI_ISend(neg_f_ys(1,1,1),(nx*nz*(1+(iforder/2))),MPI_REAL8,  &
                     lnbr(4),1,gcomm,req(3),ierr)

      call MPI_IRecv(pos_f_y,(nx*nz*(1+(iforder/2))),MPI_REAL8,  &
                    lnbr(4),2,gcomm,req(9),ierr)
    endif

    if(lnbr(3).ge.0) then
!     Evatt Hawkes inifiniband workarounds
!      call MPI_ISend(fn(1,1,1),1,fyrows_type,lnbr(3),2,gcomm,req(4),ierr)
      pos_f_ys(:,:,:)=fn(:,1:1+(iforder/2),:)
      call MPI_ISend(pos_f_ys(1,1,1),(nx*nz*(1+(iforder/2))),MPI_REAL8,  &
                    lnbr(3),2,gcomm,req(4),ierr)
      call MPI_IRecv(neg_f_y,(nx*nz*(1+(iforder/2))),MPI_REAL8,  &
                    lnbr(3),1,gcomm,req(10),ierr)
    endif

!   interior filter in the y-direction

    do j = kap1, nny - ka
      do k = - ka, ka
        dfn(:,j,:) = dfn(:,j,:) + disi(1+abs(k))*fn(:,j+k,:)
      enddo
    enddo

!   BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
    if( lnbr(3) >= 0 ) then
      call MPI_Wait(req(10),stat(:,10),ierr)
    endif

    if(lnbr(3).ge.0) then
      do j = 1,kap1-1
        do k = - ka, ka
          if((j+k).lt.1) then
            dfn(:,j,:) = dfn(:,j,:) + disi(1+abs(k)) * neg_f_y(:,kap1-abs(j+k),:)
          else
            dfn(:,j,:) = dfn(:,j,:) + disi(1+abs(k)) * fn(:,j+k,:)
          endif
        enddo
      enddo
    endif

    if( lnbr(4) >= 0 ) then
      call MPI_Wait(req(9),stat(:,9),ierr)
    endif

    if(lnbr(4).ge.0) then
      do j = nny - ka+1, nny
        do k = - ka, ka
          if((j+k).gt.nny) then
            dfn(:,j,:) = dfn(:,j,:) + disi(1+abs(k)) * pos_f_y(:,j+k-nny,:)
          else
            dfn(:,j,:) = dfn(:,j,:) + disi(1+abs(k)) * fn(:,j+k,:)
          endif
        enddo
      enddo
    endif

!   boundary points

    if(lnbr(3).lt.0) then
      do k2 = 1, kb
        do k1 = 1, ka
          dfn(:,k1 ,:) = dfn(:,k1 ,:) + disb(k1,k2)*fn(:,k2 ,:)
        enddo
      enddo
    endif

    if(lnbr(4).lt.0) then
      do k2 = 1, kb
        k2a = nny + 1 - k2
        do k1 = 1, ka
          k1a = nny + 1 - k1
          dfn(:,k1a,:) = dfn(:,k1a,:) + disb(k1,k2)*fn(:,k2a,:)
        enddo
      enddo
    endif

    if( lnbr(4) >= 0 ) then
      call MPI_Wait(req(3),stat(:,3),ierr)
    endif
    if( lnbr(3) >= 0 ) then
      call MPI_Wait(req(4),stat(:,4),ierr)
    endif
!   apply filter

    fn = fn + dfn

  endif
!-----------------------------------------------------------------------------------------
! filter in z-direction

  510 continue

  if( nnz .eq. 1 ) return

  if( vary_in_z .eq. 1 ) then

    dfn = 0.0
    if( nnz .lt. (iforder+1) ) then
      write(io,3710)
      return
    endif

!   zero neg_f_z and pos_f_z

    neg_f_z=0.0
    pos_f_z=0.0

!   get neighboring cells data
    if(periodic_z.eq.1) then
      if(zid.eq.0) then
        lnbr(5) = (zpes-1)*xpes*ypes+yid*xpes+xid
      endif
      if(zid.eq.zpes-1)then
        lnbr(6) = 0*xpes*ypes   +yid*xpes+xid
      endif
    endif

    if(lnbr(6).ge.0) then
      nm = nz - iforder/2
!     Evatt Hawkes inifiniband workarounds
!      call MPI_ISend(fn(1,1,nm),1,fzrows_type,lnbr(6),1,gcomm,req(5),ierr)
      call MPI_ISend(fn(1,1,nm),(nx*ny*(1+(iforder/2))),MPI_REAL8,  &
                     lnbr(6),1,gcomm,req(5),ierr)
      call MPI_IRecv(pos_f_z,(nx*ny*(1+(iforder/2))),MPI_REAL8,  &
                    lnbr(6),2,gcomm,req(11),ierr)
    endif

    if(lnbr(5).ge.0) then
!     Evatt Hawkes inifiniband workarounds
!      call MPI_ISend(fn(1,1,1),1,fzrows_type,lnbr(5),2,gcomm,req(6),ierr)
      call MPI_ISend(fn(1,1,1),(nx*ny*(1+(iforder/2))),MPI_REAL8,  &
                     lnbr(5),2,gcomm,req(6),ierr)
      call MPI_IRecv(neg_f_z,(nx*ny*(1+(iforder/2))),MPI_REAL8,  &
                    lnbr(5),1,gcomm,req(12),ierr)
    endif

!   interior filter in the z direction

!   Evatt 26-JUL-2005 vectorizing
    do m = kap1, nnz - ka
      do k = - ka, ka
        dfn(:,:,m) = dfn(:,:,m) + disi(1+abs(k))*fn(:,:,m+k)
      enddo
    enddo

!   BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
    if( lnbr(5) >= 0 ) then
      call MPI_Wait(req(12),stat(:,12),ierr)
    endif

    if(lnbr(5).ge.0) then
      do k = - ka, ka
        kmag = 1 + abs ( k )
        do m = 1,kap1-1
          if((m+k).lt.1) then
            dfn(:,:,m) = dfn(:,:,m) + disi(kmag) * neg_f_z(:,:,kap1-abs(m+k))
          else
            dfn(:,:,m) = dfn(:,:,m) + disi(kmag) * fn(:,:,m+k)
          endif
        enddo
      enddo
    endif

    if( lnbr(6) >= 0 ) then
      call MPI_Wait(req(11),stat(:,11),ierr)
    endif

    if(lnbr(6).ge.0) then
      do k = - ka, ka
        kmag = 1 + abs ( k )
        do m = nnz - ka+1, nnz
          if((m+k).gt.nnz) then
            dfn(:,:,m) = dfn(:,:,m) + disi(kmag) * pos_f_z(:,:,m+k-nnz)
          else
            dfn(:,:,m) = dfn(:,:,m) + disi(kmag) * fn(:,:,m+k)
          endif
        enddo
      enddo
    endif

!   boundary points

    if(lnbr(5).lt.0) then
      do k1 = 1, ka
        do k2 = 1, kb
          dfn(:,:,k1 ) = dfn(:,:,k1 ) + disb(k1,k2)*fn(:,:,k2 )
        enddo
      enddo
    endif

    if(lnbr(6).lt.0) then
      do k1 = 1, ka
        k1a = nnz + 1 - k1
        do k2 = 1, kb
          k2a = nnz + 1 - k2
          dfn(:,:,k1a) = dfn(:,:,k1a) + disb(k1,k2)*fn(:,:,k2a)
        enddo
      enddo
    endif

    if( lnbr(6) >= 0 ) then
      call MPI_Wait(req(5),stat(:,5),ierr)
    endif
    if( lnbr(5) >= 0 ) then
      call MPI_Wait(req(6),stat(:,6),ierr)
    endif
!   apply filter

    do m = 1, nnz
      do j = 1, nny
        do i = 1, nnx
          fn(i,j,m) = fn(i,j,m) + dfn(i,j,m)
        enddo
       enddo
    enddo

  endif

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
! format statments

  3700 format('x-domain has too few grid points for exp. filtering')
  3705 format('y-domain has too few grid points for exp. filtering')
  3710 format('z-domain has too few grid points for exp. filtering')
!-----------------------------------------------------------------------------------------
  return
  end subroutine filter
!-----------------------------------------------------------------------------------------
  end module filter_m

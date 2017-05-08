#include "globalDefines.h"
! Module to implement filters as described by R. J. Purser (J. of Clim. and
! Apld. Meteorology, 1987)

! Changes coded by R. Grout, 2008

! This is based on filter_m; in fact, much of the code is identical
! The motivation here though is to implement highly dissipative filters
! in order to, for example, prevent a boundary layer from becoming turbulent

! The coefficients are of course different...

! The filters are only, strictly speaking, valid on a uniform grid

!=========================================================================================
  module damping_filter_m
!=========================================================================================
! module for explicit filter variables

  implicit none
!-----------------------------------------------------------------------------------------
! integers

  integer i_time_damp_fil      !frequency by which to filter
  integer, private :: nxfr            !something to do with compatibility check
  integer, private :: nxfr1           !something to do with compatibility check

  integer, private :: damp_fil_R, damp_fil_S, damp_fil_T, iforder

  integer, private :: invert_x_prof, invert_y_prof, invert_z_prof 

! Reals
  real, private :: filter_blend_fac   ! Factor to control steepness 
  real, private :: x_prof_a, x_prof_b ! Locations of x profile transition
  real, private :: y_prof_a, y_prof_b ! Locations of y profile transition
  real, private :: z_prof_a, z_prof_b ! Locations of z profile transition

! reals arrays

  real, private :: disb(10,20)        !filter arrays
  real, private :: disi(11)           !filter arrays

  real, private, allocatable :: damping_mag_x(:) ! Filter weights
  real, private, allocatable :: damping_mag_y(:)
  real, private, allocatable :: damping_mag_z(:)

! ghost cell arrays

  real, private, allocatable :: neg_f_x(:,:,:), pos_f_x(:,:,:)
  real, private, allocatable :: neg_f_y(:,:,:), pos_f_y(:,:,:)
  real, private, allocatable :: neg_f_z(:,:,:), pos_f_z(:,:,:)

! Evatt Hawkes infiniband workarounds
  real, private, allocatable :: neg_f_xs(:,:,:), pos_f_xs(:,:,:)
  real, private, allocatable :: neg_f_ys(:,:,:), pos_f_ys(:,:,:)
!-----------------------------------------------------------------------------------------
  contains
!=========================================================================================
  subroutine initialize_damp_filter(io, flag)
!=========================================================================================
! if flag = 1: read damp_filter.in and allocate arrays
! if flag = -1: deallocate arrays
!-----------------------------------------------------------------------------------------
  use param_m, only : nx, ny, nz, nxm, &
                      periodic_x, periodic_y, periodic_z, &
                      vary_in_x, vary_in_y, vary_in_z
  use topology_m
  use grid_m
  use reference_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed

  integer flag
  integer io

! Locals
  character*100 :: filename
  logical :: exist
  integer i, j, k
!-----------------------------------------------------------------------------------------
! filter arrays

  if(flag.eq.1) then

      ! Read input file

      if (myid == 0) then
        filename = '../input/damp_filter.in'          ! set the file name
        inquire(file=trim(filename),exist=exist)      ! see if the file exists
      endif

      call MPI_Bcast(exist, 1, MPI_LOGICAL, 0, gcomm, ierr)

      if(.not.exist) then   !does not exist
          if(myid == 0) then
              write(io,*) 'the following input file does not exist:'
              write(io,*) trim(filename)
          endif
          call terminate_run(io,0)
      endif

      if(myid == 0) then
          open(unit=20, file=trim(filename), status='old', form='formatted')
          read(20,*) damp_fil_R        !R (See table 1 in Purser)
          read(20,*) damp_fil_S        !S (See table 1 in Purser)
          read(20,*) i_time_damp_fil  ! Frequency by which to filter
          read(20,*) filter_blend_fac  ! Scale factor for tanh transition
          read(20,*) x_prof_a          ! Position of first tanh transition
          read(20,*) x_prof_b          ! Position of second tanh transition
          read(20,*) invert_x_prof     ! 0= filter on outside, 1 = filter inside
          read(20,*) y_prof_a          ! Position of first tanh transition
          read(20,*) y_prof_b          ! Position of second tanh transition
          read(20,*) invert_y_prof     ! 0= filter on outside, 1 = filter inside
          read(20,*) z_prof_a          ! Position of first tanh transition
          read(20,*) z_prof_b          ! Position of second tanh transition
          read(20,*) invert_z_prof     ! 0= filter on outside, 1 = filter inside
          close(20)
      endif

      ! broadcast input file parameters

      call MPI_Bcast(damp_fil_R,       1, MPI_INTEGER,    0, gcomm, ierr)
      call MPI_Bcast(damp_fil_S,       1, MPI_INTEGER,    0, gcomm, ierr)
      call MPI_Bcast(i_time_damp_fil,  1, MPI_REAL8,    0, gcomm, ierr)
      call MPI_Bcast(filter_blend_fac,  1, MPI_REAL8,    0, gcomm, ierr)
      call MPI_Bcast(x_prof_a,  1, MPI_REAL8,    0, gcomm, ierr)
      call MPI_Bcast(x_prof_b,  1, MPI_REAL8,    0, gcomm, ierr)
      call MPI_Bcast(y_prof_a,  1, MPI_REAL8,    0, gcomm, ierr)
      call MPI_Bcast(y_prof_b,  1, MPI_REAL8,    0, gcomm, ierr)
      call MPI_Bcast(z_prof_a,  1, MPI_REAL8,    0, gcomm, ierr)
      call MPI_Bcast(z_prof_b,  1, MPI_REAL8,    0, gcomm, ierr)
      call MPI_Bcast(invert_x_prof,  1, MPI_INTEGER,    0, gcomm, ierr)
      call MPI_Bcast(invert_y_prof,  1, MPI_INTEGER,    0, gcomm, ierr)
      call MPI_Bcast(invert_z_prof,  1, MPI_INTEGER,    0, gcomm, ierr)

      ! Compute filter order
      damp_fil_T = damp_fil_R + damp_fil_S + 1
      iforder = damp_fil_T * 2

      ! Non-dimensionalize profile
      x_prof_a = x_prof_a / l_ref / 100.0
      x_prof_b = x_prof_b / l_ref / 100.0

      y_prof_a = y_prof_a / l_ref / 100.0
      y_prof_b = y_prof_b / l_ref / 100.0

      z_prof_a = z_prof_a / l_ref / 100.0
      z_prof_b = z_prof_b / l_ref / 100.0
      ! write to screen
      if(myid==0) then
          write(io,'(1a,1i4)') 'Damping filter R = ', damp_fil_R
          write(io,'(1a,1i4)') 'Damping filter S = ', damp_fil_S
          write(io,'(1a,1i4)') 'Damping filter T = ', damp_fil_T
          write(io,'(1a,1i4)') 'Damping filter iforder = ', iforder

          write(io,*) 'x profile transition = ', x_prof_a, x_prof_b
      endif

!------------------------------------------------------------------------------
      ! Allocate arrays

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

!     Damping weight arrays for partial field filtering

       allocate(damping_mag_x(nx)) ! Filter weights
       allocate(damping_mag_y(ny))
       allocate(damping_mag_z(nz))
!-----------------------------------------------------------------------------------------
! initialize filter

!----------------------------------------------------------------------------------------
! write header

    if(myid.eq.0) then
        write(io,*) 'initializing damping filter module...'
    endif

    if(iforder.ge.2) then
        if(myid.eq.0) then
            write(io,8060) iforder
        endif

        if((vary_in_x.eq.1)) then
            if ( nx .lt. iforder + 1) then
                write(io,9060) iforder+1, nx
            call terminate_run(io,0)  !must be called by all processors
            endif
        endif

        if((vary_in_y.eq.1)) then
            if ( ny .lt. iforder + 1) then
                write(io,9060) iforder+1, ny
                call terminate_run(io,0)  !must be called by all processors
            endif
        endif
        if((vary_in_z.eq.1)) then
            if ( nz .lt. iforder + 1) then
                write(io,9060) iforder+1, nz
                call terminate_run(io,0)  !must be called by all processors
            endif
        endif
    endif

    call set_damp_filter_coefs(io)
    
!  Set up spatial variation of filter weight

if( invert_x_prof .ge. 0 ) then
   do i=1,nx
     damping_mag_x(i) = -0.5 * ( tanh( (x(i) - x_prof_a)*filter_blend_fac ) + 1.0 ) + 1.0 &
                        +0.5 * ( tanh( (x(i) - x_prof_b)*filter_blend_fac ) + 1.0 )  

   enddo
   if( invert_x_prof == 1 ) damping_mag_x = 1.0 - damping_mag_x

 else
   damping_mag_x = 1.0
 endif

 if( invert_y_prof .ge. 0) then
   do j=1,ny
     damping_mag_y(j) = -0.5 * ( tanh( (y(j) - y_prof_a)*filter_blend_fac ) + 1.0 ) + 1.0 &
                        +0.5 * ( tanh( (y(j) - y_prof_b)*filter_blend_fac ) + 1.0 )  

   enddo
   if( invert_y_prof == 1 ) damping_mag_y = 1.0 - damping_mag_y

 else
   damping_mag_y = 1.0
 endif

 if( invert_z_prof .ge. 0 ) then


   do k=1,nz
     damping_mag_z(k) = -0.5 * ( tanh( (z(k) - z_prof_a)*filter_blend_fac ) + 1.0 ) + 1.0 &
                        +0.5 * ( tanh( (z(k) - z_prof_b)*filter_blend_fac ) + 1.0 )  

   enddo
   if( invert_z_prof == 1 ) damping_mag_z = 1.0 - damping_mag_z
 else
   damping_mag_z = 1.0

 endif
!
!      if( myid == 0) then
!        do i=1, nx
!          write(*,*) x(i), damping_mag_x(i)
!        enddo
!        write(*,*)
!        do j=1, ny
!          write(*,*) y(j), damping_mag_y(j)
!        enddo
!        do k=1, nz
!          write(*,*) z(k), damping_mag_z(k)
!        enddo
!      endif
!
!      call terminate_run(io,0)
!----------------------------------------------------------------------------------------
! write i_time_fil

  if(myid.eq.0) then
    if(i_time_damp_fil.eq.1) then
      write(io,*) 'damping filter will be applied every time step'
    else
      write(io,1) 'damping filter will be applied every ',i_time_damp_fil, ' time steps'
    endif
  endif
1 format(a30,i5,a11)
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
!-----------------------------------------------------------------------------------------
    call write_header(io,'-')
  endif
!-----------------------------------------------------------------------------------------
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
 
 ! Filter weights
      deallocate(damping_mag_x) 
      deallocate(damping_mag_y)
      deallocate(damping_mag_z)

  endif
! format statements

  8060 format(' using explicit filter of order',i3)
  9060 format(/' error: bad grid parameter. Need nx>=',i5,'; nx=',3i5)
!-----------------------------------------------------------------------------------------
  return
  end subroutine initialize_damp_filter
!=========================================================================================
 subroutine set_damp_filter_coefs(io)
!=========================================================================================
! defines the filter coefficients
!
! feed in the U-vector at the end of every step and do this to it:
!
! U_filt = (alpha_D*D)*U_unfilt
!
! where alpha_D is just for scaling ( and is q = 4^(-T) ) and 
! D is the dissipation matrix.
! D is based on a stencil of width 2T+1; T = R+S+1
!
! disb     - Dissipation matrix coefficients for the boundary grid points
!            (kept this from Shapiro filters, where S=0 )
! disi     - Dissipation matrix coefficients for the interior grid points
! iforder  - Filter order
! iordd2   - Filter order divided by 2
! iordd2p1 - Filter order divided by 2 + 1
!-----------------------------------------------------------------------------------------

  use topology_m, only : myid 

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
! check for odd filter choices

  if(mod(iforder,2).ne.0) then
    write(io,9020)
    call terminate_run(io,0)  !must be called by all processors
  endif
!-----------------------------------------------------------------------------------------


! form coefficient alpha

  !alpha = ( - 1.0)**iordd2 / real ( 2**iforder )
  alpha = - 1.0 / (4.0 ** damp_fil_T) 

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

    disb(1,1) = +1.0
    disb(1,2) = -1.0

    ! R = 0, S=0 is only possibility
    disi(2) = -1.0
    disi(1) = +2.0

! 4th order inner with 2nd order boundaries

  elseif ( iforder .eq. 4 ) then

    disb(1,1) = -1.0
    disb(1,2) = +2.0
    disb(1,3) = -1.0

    disb(2,1) = +2.0
    disb(2,2) = -5.0
    disb(2,3) = +4.0
    disb(2,4) = -1.0

    if( damp_fil_S .eq. 0  .and. damp_fil_R .eq. 1 ) then
        disi(3) = +1.0
        disi(2) = -4.0
        disi(1) = +6.0
    elseif( damp_fil_S .eq. 1 .and. damp_fil_R .eq. 0) then
        disi(3) = -1.0
        disi(2) = -4.0
        disi(1) = 10.0
    else
        write(io,*) 'That combination of R, S is not implemented '
        call terminate_run(io, 0)
    endif


! 6th order inner with 3rd order boundaries

  elseif ( iforder .eq. 6 ) then

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

    if( damp_fil_S .eq. 0  .and. damp_fil_R .eq. 2 ) then
        disi(4) = -1.0
        disi(3) = +6.0
        disi(2) = -15.0
        disi(1) = +20.0
    elseif( damp_fil_S .eq. 1 .and. damp_fil_R .eq. 1) then
        disi(4) = +2.0
        disi(3) = +0.0
        disi(2) = -18.0
        disi(1) = +32.0
    elseif( damp_fil_S .eq. 2 .and. damp_fil_R .eq. 0) then
        disi(4) = -1.0
        disi(3) = -6.0
        disi(2) = -15.0
        disi(1) = +44.0
    else
        write(io,*) 'That combination of R, S is not implemented '
        call terminate_run(io, 0)
    endif
! 8th order inner with 4th order boundaries

  elseif ( iforder .eq. 8 ) then


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

    if( damp_fil_S .eq. 0  .and. damp_fil_R .eq. 3 ) then
        disi(5) = +1.0
        disi(4) = -8.0
        disi(3) = +28.0
        disi(2) = -56.0
        disi(1) = +70.0
    elseif( damp_fil_S .eq. 1 .and. damp_fil_R .eq. 2) then
        disi(5) = -3.0 
        disi(4) = +8.0
        disi(3) = +12.0
        disi(2) = -72.0
        disi(1) = +110.0
    elseif( damp_fil_S .eq. 2 .and. damp_fil_R .eq. 1) then
        disi(5) = +3.0
        disi(4) = +8.0
        disi(3) = -12.0
        disi(2) = -72.0
        disi(1) = +146.0
    elseif( damp_fil_S .eq. 3 .and. damp_fil_R .eq. 0) then
        disi(5) = -1.0
        disi(4) = -8.0
        disi(3) = -28.0
        disi(2) = -56.0
        disi(1) = +186.0
    else
        write(io,*) 'That combination of R, S is not implemented '
        call terminate_run(io, 0)
    endif

! 10th order inner with 5th order boundaries

  elseif ( iforder .eq. 10 ) then

        write(io,*) 'That combination of R, S is not implemented '
        call terminate_run(io, 0)

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

        write(io,*) 'That combination of R, S is not implemented '
        call terminate_run(io, 0)

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

        write(io,*) 'That combination of R, S is not implemented '
        call terminate_run(io, 0)


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

else
        write(io,*) 'That combination of R, S is not implemented '
        call terminate_run(io, 0)



    endif
    if(myid == 0 ) then
        write(io,'(1a)') 'Initialized damping filter coefficients as:'
        do i=1,  damp_fil_T
            write(io, *)i, disi(i)
        enddo
    endif
!-----------------------------------------------------------------------------------------
! normalize boundary and interior elements
  do i = 1, damp_fil_T
    do j = 1, iforder
      disb(i,j)= alpha * disb(i,j)
    enddo
  enddo

  do i = 1, damp_fil_T + 1
    disi(i) = alpha * disi(i)
  enddo
!-----------------------------------------------------------------------------------------
! format statements

  9020 format(' error in _set_filter_coefs_: iforder must be even')
!-----------------------------------------------------------------------------------------
  return
  end subroutine set_damp_filter_coefs
!=========================================================================================
  subroutine damping_filter(fn, io)
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
  use topology_m
  use param_m, only : nx, ny, nz, nxm, &
                      periodic_x, periodic_y, periodic_z, &
                      vary_in_x, vary_in_y, vary_in_z
  use work_m, only : dfn => work1_1
  use work_m, only : damping_mag => work1_2
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

  do k = 1, nz
    do j = 1, ny
      damping_mag(:,j,k) = damping_mag_x * damping_mag_y(j)  *  damping_mag_z(k)
    enddo
  enddo
  damping_mag = 1.0 - damping_mag

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
          fn(i,j,m) = fn(i,j,m) + dfn(i,j,m) &
                                  * damping_mag(i,j,m) 
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

    fn = fn + dfn * damping_mag

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
          fn(i,j,m) = fn(i,j,m) + dfn(i,j,m) &
                                  * damping_mag(i,j,m) 
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
  end subroutine damping_filter
!-----------------------------------------------------------------------------------------
!=========================================================================================
  subroutine damp_velocity(io)
!=========================================================================================

  use param_m, only : nx, ny, nz, nxm, &
                      periodic_x, periodic_y, periodic_z, &
                      vary_in_x, vary_in_y, vary_in_z
  use variables_m, only : u
  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations
  integer :: i
  
!-----------------------------------------------------------------------------------------
! Apply filter to each velocity component
  
  do i=1, 3
          call damping_filter( u(:,:,:,i), io )
  enddo

   end subroutine damp_velocity
end module damping_filter_m

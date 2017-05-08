#include "globalDefines.h"
! REVISIONS:
!
!  18-MAY-2005  Evatt Hawkes - this version with infiniband work-arounds
!               problems dealing with strided data-types
!
!  Apr 08 2005 - Ramanan Sankaran
!    Derivative routine was split in two. One does the communication and 
!    the other does the computation. Trying to decouple the communication in 
!    three directions. Should improve performance and scalability.
!
!  29-NOV-2004  Evatt Hawkes
!           Possible bug in MPI - waits were called when send and receives were not
!           in cases when lnbr < 0 - not sure whether this was really buggy, 
!           but safety first
!
!   6-5-03  James Sutherland fixed a parallel bug related to
!           blocking vs. nonblocking sends & receives.  This bug crashed MPI in some
!           instances, but I don't think that it affected the numerics at all.

!=========================================================================================
  subroutine derivative_y(mx,my,mz,f,df,scale_1,n_sym)
!=========================================================================================
! evaluates the first derivative in y-direction using explicit differencing
! for further details of arguments, see description in header of derivative_y_calc
!-----------------------------------------------------------------------------------------
  implicit none

  integer, intent(in) :: mx, my, mz
  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(   my   ) :: scale_1
  integer, intent(in) :: n_sym

  integer, dimension(4) :: req

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_y_comm(mx,my,mz,f,req)

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_y_calc(mx,my,mz,f,df,scale_1,n_sym,req)

  return
  end subroutine derivative_y

!=========================================================================================
  subroutine derivative_y_twopart(mx,my,mz,f,df,scale_1,n_sym, idx, label)
!=========================================================================================
! evaluates the first derivative in y-direction using explicit differencing
! for further details of arguments, see description in header of derivative_y_calc
!-----------------------------------------------------------------------------------------
  implicit none
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(   my   ) :: scale_1
  integer, intent(in) :: n_sym

  character(*), intent(in) :: label
  integer, intent(inout) :: idx

#ifdef COARRAYCOMMUNICATION
  write(*,*) 'Coarrays not implemented for twopart derivatives'
  write(*,*) 'use derivative_y instead of derivative_y_twopart'
  stop
#endif

  call derivative_y_post(mx,my,mz,f,idx,label)
  call derivative_y_send(mx,my,mz,f,idx,label)

  call derivative_y_calc_buff(mx,my,mz,f,df,scale_1,n_sym,idx)

  return
  end subroutine derivative_y_twopart

!=========================================================================================
  subroutine derivative_y_comm(mx,my,mz,f,req)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_y, vary_in_y, iorder
#ifdef COARRAYCOMMUNICATION
  use derivative_m, only : neg_f => neg_f_y
  use derivative_m, only : pos_f => pos_f_y
#else
  use derivative_m, only : neg_f => neg_f_y, neg_fs => neg_fs_y
  use derivative_m, only : pos_f => pos_f_y, pos_fs => pos_fs_y
#endif

  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  integer, dimension(4), intent(out) :: req

  integer :: nm
  integer, dimension(6) :: lnbr(6)

!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_y .eq. 0 ) then
    return
  endif
!-----------------------------------------------------------------------------------------
! zero ghost cell arrays

  neg_f=0.0
  pos_f=0.0
!-----------------------------------------------------------------------------------------
! get neighboring cells data

  lnbr = neighbor

  if(periodic_y.eq.1) then
    if(yid.eq.0) then
      lnbr(3) = zid*xpes*ypes+(ypes-1)*xpes+xid
    endif
    if(yid.eq.ypes-1)then
      lnbr(4) = zid*xpes*ypes+(0     )*xpes+xid
    endif
  endif

  req(:) = MPI_REQUEST_NULL

#ifdef COARRAYCOMMUNICATION
  if(lnbr(3)>=0) then
     neg_f(:,:,:)[lnbr(3)] = f(:,1:iorder/2,:)
  endif
  if(lnbr(4)>=0) then
     nm = my + 1 - iorder/2
     pos_f(:,:,:)[lnbr(4)] = f(:,nm:my,:)
  endif
#else
  if(lnbr(3)>=0) then
   ! get ghost cells from neighbor on (-y) side
     call MPI_IRecv(neg_f,(mx*mz*iorder/2),MPI_REAL8,lnbr(3),1,gcomm,req(1),ierr)
   ! send ghost cells to neighbor on (-y) side
!     call MPI_ISend(f(1,1,1),1,yrows_type,lnbr(3),2,gcomm,req(2),ierr)
!    Evatt Hawkes workaround for infiniband
     pos_fs(:,:,:) = f(:,1:iorder/2,:)
     call MPI_ISend(pos_fs(1,1,1),(mx*mz*iorder/2),MPI_REAL8,lnbr(3),2,gcomm,req(2),ierr)
  endif
  
  if(lnbr(4)>=0) then
   ! get ghost cells from neighbor on (+y) side
     call MPI_IRecv(pos_f,(mx*mz*iorder/2),MPI_REAL8,lnbr(4),2,gcomm,req(3),ierr)
   ! send ghost cells to neighbor on (+y) side
     nm = my + 1 - iorder/2
!     call MPI_ISend(f(1,nm,1),1,yrows_type,lnbr(4),1,gcomm,req(4),ierr)
!    Evatt Hawkes workaround for infiniband
     neg_fs(:,:,:) = f(:,nm:my,:)
     call MPI_ISend(neg_fs(1,1,1),(mx*mz*iorder/2),MPI_REAL8,lnbr(4),1,gcomm,req(4),ierr)
  endif
#endif
  return
  end subroutine derivative_y_comm

!=========================================================================================
  subroutine derivative_y_post(mx,my,mz,f,idx,label)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_y, vary_in_y, iorder

  use derivative_m

  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  integer, intent(in) :: idx
  character(*), intent(in) :: label

  integer :: nm
  integer, dimension(6) :: lnbr(6)

#ifdef COARRAYCOMMUNICATION
  write(*,*) 'Coarray communication not implemented in split post/comm derivative routines'
  write(*,*) 'Use derivative_xyz_comm instead'
#endif
!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_y .eq. 0 ) then
    return
  endif
!-----------------------------------------------------------------------------------------
! Find available index, or use supplied index
  if(  deriv_y_list(idx)%inuse ) then
      write(*,'(1a, 1i3, 1a, 1i5)') 'Error, index', idx, ' is in use for y-direction', myid
      call print_deriv_list(6)
      stop
  endif

  deriv_y_list(idx)%inuse = .true.
  deriv_y_list(idx)%fldname = trim(label)
!-----------------------------------------------------------------------------------------
! zero recv ghost cell arrays for this position

  neg_f_y_buf(:,:,:,idx) = 0.0
  pos_f_y_buf(:,:,:,idx) = 0.0
!-----------------------------------------------------------------------------------------
! get neighboring cells data

  lnbr = neighbor

  if(periodic_y.eq.1) then
    if(yid.eq.0) then
      lnbr(3) = zid*xpes*ypes+(ypes-1)*xpes+xid
    endif
    if(yid.eq.ypes-1)then
      lnbr(4) = zid*xpes*ypes+(0     )*xpes+xid
    endif
  endif

  deriv_y_list(idx)%neg_nbr = lnbr(3)
  deriv_y_list(idx)%pos_nbr = lnbr(4)

  deriv_y_list(idx)%req = MPI_REQUEST_NULL

  if(lnbr(3)>=0) then
   ! get ghost cells from neighbor on (-y) side
     call MPI_IRecv(neg_f_y_buf(:,:,:,idx),(mx*mz*iorder/2), &
                    MPI_REAL8,deriv_y_list(idx)%neg_nbr,idx, &
                    gcomm,deriv_y_list(idx)%req(1),ierr)
  endif
  
  if(lnbr(4)>=0) then
   ! get ghost cells from neighbor on (+y) side
     call MPI_IRecv(pos_f_y_buf(:,:,:,idx),(mx*mz*iorder/2),&
                    MPI_REAL8,deriv_y_list(idx)%pos_nbr,idx+deriv_list_size,&
                    gcomm,deriv_y_list(idx)%req(3),ierr)
  endif

  return
  end subroutine derivative_y_post

!=========================================================================================
  subroutine derivative_y_send(mx,my,mz,f,idx,label)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_y, vary_in_y, iorder

  use derivative_m


  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  integer, intent(in) :: idx

  integer :: nm
  integer, dimension(6) :: lnbr(6)
  character(*), intent(in) :: label

#ifdef COARRAYCOMMUNICATION
  write(*,*) 'Coarray communication not implemented in split post/comm derivative routines'
  write(*,*) 'Use derivative_xyz_comm instead'
#endif
!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_y .eq. 0 ) then
    return
  endif

#ifdef DERIV_CHECK_LABELS
  ! check for misuse
  if( trim(deriv_y_list(idx)%fldname) .ne.  trim(label) ) then
      write(*,'2a,i5') 'Mismatch name: ', trim(label), idx
      call print_deriv_list(6)
  endif
#endif


  if(deriv_y_list(idx)%neg_nbr>=0) then
   ! send ghost cells to neighbor on (-y) side
     pos_fs_y_buf(:,:,:,idx) = f(:,1:iorder/2,:)
     call MPI_ISend(pos_fs_y_buf(1,1,1,idx),(mx*mz*iorder/2),&
                    MPI_REAL8,deriv_y_list(idx)%neg_nbr,idx+deriv_list_size,&
                    gcomm,deriv_y_list(idx)%req(2),ierr)
  endif
  
  if(deriv_y_list(idx)%pos_nbr>=0) then
   ! send ghost cells to neighbor on (+y) side
     nm = my + 1 - iorder/2
     neg_fs_y_buf(:,:,:,idx) = f(:,nm:my,:)
     call MPI_ISend(neg_fs_y_buf(1,1,1,idx),(mx*mz*iorder/2),&
                    MPI_REAL8,deriv_y_list(idx)%pos_nbr,idx, &
                    gcomm,deriv_y_list(idx)%req(4),ierr)
  endif

  return
  end subroutine derivative_y_send
!=========================================================================================
  subroutine derivative_y_calc(mx,my,mz,f,df,scale_1,n_sym,req)
!=========================================================================================
! evaluates the first derivative in y-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!  ae     - Explicit central difference stencil coefficient at (j +/- 1)
!  be     - Explicit central difference stencil coefficient at (j +/- 2)
!  ce     - Explicit central difference stencil coefficient at (j +/- 3)
!  de     - Explicit central difference stencil coefficient at (j +/- 4)
!  mx     - number of grid points in x-direction
!  my     - number of grid points in y-direction
!  mz     - number of grid points in z-direction
!
!       d()/dy = [ d(eta)/dy ]*[ d()/d(eta) ]
!        [ dy/d(eta) ]^-1*[ d()/d(eta) ]

!  scale_1- Proportional to ( dy/d(eta) )^-1 where eta is fictitious
!           uniform grid and y is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along y-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_y
  use param_m, only : periodic_y, vary_in_y, iorder
  use derivative_m, only : isym_y
  use derivative_m, only : ibound
  use derivative_m, only : neg_f => neg_f_y
  use derivative_m, only : pos_f => pos_f_y

  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(   my   ) :: scale_1
  integer, dimension(4), intent(in) :: req

  integer, intent(in) :: n_sym

  integer :: i, j, k
  integer, dimension(6) :: lnbr(6)
  integer, dimension(MPI_Status_size,4) :: stat

  real :: ds,ae,be,ce,de

!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_y .eq. 0 ) then
     df = 0.0
    return
  endif
!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_y.ne.1) then
    ds = real(my*ypes-1)
  else
    ds = real(my*ypes-1) * scale_1(1)
  endif

  lnbr = neighbor

  if(periodic_y.eq.1) then
    if(yid.eq.0) then
      lnbr(3) = zid*xpes*ypes+(ypes-1)*xpes+xid
    endif
    if(yid.eq.ypes-1)then
      lnbr(4) = zid*xpes*ypes+(0     )*xpes+xid
    endif
  endif

!-----------------------------------------------------------------------------------------
!  6th order explicit: (3,3,4-6-4,3,3)
!
      if ( iorder .eq. 6 ) then

         ae =  3./ 4. *ds
         be = -3./20. *ds
         ce =  1./60. *ds
!
!  Internal nodes:
!
         do k = 1, mz
            do j = 4, my-3
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j+1,k)-f(i,j-1,k) )                          &
                            + be *( f(i,j+2,k)-f(i,j-2,k) )                          &
                            + ce *( f(i,j+3,k)-f(i,j-3,k) )
               end do
            end do
         end do

         if(unif_grid_y.ne.1) then
           do j = 4, my-3
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( lnbr(3) >= 0 )then
            call MPI_Wait(req(1),stat(:,1),ierr)
         endif 
#endif
!
!  Internal nodes: edges
!
         if (neighbor(3).ge.0) then
            do k = 1, mz
               do i = 1, mx
                  df(i,1,k) = ae *( f(i,2,k)-neg_f(i,3,k) )                          &
                          + be *( f(i,3,k)-neg_f(i,2,k) )                            &
                          + ce *( f(i,4,k)-neg_f(i,1,k) )
                  df(i,2,k) = ae *( f(i,3,k)-    f(i,1,k) )                          &
                          + be *( f(i,4,k)-neg_f(i,3,k) )                            &
                          + ce *( f(i,5,k)-neg_f(i,2,k) )
                  df(i,3,k) = ae *( f(i,4,k)-    f(i,2,k) )                          &
                            + be *( f(i,5,k)-    f(i,1,k) )                          &
                            + ce *( f(i,6,k)-neg_f(i,3,k) )
               end do
            end do
         endif

#ifndef COARRAYCOMMUNICATION
         if( lnbr(4) >= 0 ) then
            call MPI_Wait(req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(4).ge.0) then
            do k = 1, mz
               do i = 1, mx
                  df(i,my  ,k) = ae *( pos_f(i,1,k)   -f(i,my-1,k) )                 &
                               + be *( pos_f(i,2,k)   -f(i,my-2,k) )                 &
                               + ce *( pos_f(i,3,k)   -f(i,my-3,k) )
                  df(i,my-1,k) = ae *(     f(i,my,k)  -f(i,my-2,k) )                 &
                               + be *( pos_f(i,1,k)   -f(i,my-3,k) )                 &
                               + ce *( pos_f(i,2,k)   -f(i,my-4,k) )
                  df(i,my-2,k) = ae *(     f(i,my-1,k)-f(i,my-3,k) )                 &
                               + be *(     f(i,my,k)  -f(i,my-4,k) )                 &
                               + ce *( pos_f(i,1,k)   -f(i,my-5,k) )
               end do
            end do
         endif

!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_y .eq. 0 ) .and. ( periodic_y .eq. 0 )) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,   1,k) = (-11.*f(i,1,k) + 18.*f(i,2,k)                 &
                                        - 9.*f(i,3,k) +  2.*f(i,4,k))                &
                                        / 6. *ds

                        df(i,   2,k) = (- 2.*f(i,1,k) -  3.*f(i,2,k)                 &
                                        + 6.*f(i,3,k) -  1.*f(i,4,k))                &
                                        / 6. *ds

                        df(i,   3,k) = (+ 1.*f(i,1,k) -  8.*f(i,2,k)                 &
                                        + 8.*f(i,4,k) -  1.*f(i,5,k))                &
                                        /12.*ds
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,my-2,k) = (- 1.*f(i,my  ,k)+ 8.*f(i,my-1,k)             &
                                        - 8.*f(i,my-3,k)+ 1.*f(i,my-4,k)             &
                                       )/12.*ds

                        df(i,my-1,k) = (+ 2.*f(i,my  ,k)+ 3.*f(i,my-1,k)             &
                                        - 6.*f(i,my-2,k)+ 1.*f(i,my-3,k)             &
                                       )/ 6. *ds

                        df(i,my  ,k) = (+11.*f(i,my  ,k)-18.*f(i,my-1,k)             &
                                        + 9.*f(i,my-2,k)- 2.*f(i,my-3,k)             &
                                       )/ 6. *ds
                     end do
                  end do
               end if
            elseif (( isym_y .eq. 0 ) .and. ( periodic_y .eq. 1 )) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,   1,k) = ae *( f(i   ,2,k)- neg_f(i,3,k) )             &
                                     + be *( f(i   ,3,k)- neg_f(i,2,k) )             &
                                     + ce *( f(i   ,4,k)- neg_f(i,1,k) )

                        df(i,   2,k) = ae *( f(i   ,3,k)-     f(i,1,k) )             &
                                     + be *( f(i   ,4,k)- neg_f(i,3,k) )             &
                                     + ce *( f(i   ,5,k)- neg_f(i,2,k) )

                        df(i,   3,k) = ae *( f(i   ,4,k)-     f(i,2,k) )             &
                                     + be *( f(i   ,5,k)-     f(i,1,k) )             &
                                     + ce *( f(i   ,6,k)- neg_f(i,3,k) )
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,my-2,k) = ae*(    f(i,my-1,k)-f(i,my-3,k) )             &
                                     + be*(    f(i,my  ,k)-f(i,my-4,k) )             &
                                     + ce*(pos_f(i,   1,k)-f(i,my-5,k) )

                        df(i,my-1,k) = ae*(    f(i,my  ,k)-f(i,my-2,k) )             &
                                     + be*(pos_f(i,   1,k)-f(i,my-3,k) )             &
                                     + ce*(pos_f(i,   2,k)-f(i,my-4,k) )

                        df(i,my  ,k) = ae*(pos_f(i,   1,k)-f(i,my-1,k) )             &
                                     + be*(pos_f(i,   2,k)-f(i,my-2,k) )             &
                                     + ce*(pos_f(i,   3,k)-f(i,my-3,k) )

                     end do
                  end do
               end if

            elseif ( isym_y .eq. 1 ) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,   1,k) = ae*( f(i,2,k)- n_sym*f(i,2,k) )               &
                                     + be*( f(i,3,k)- n_sym*f(i,3,k) )               &
                                     + ce*( f(i,4,k)- n_sym*f(i,4,k) )

                        df(i,   2,k) = ae*( f(i,3,k)-       f(i,1,k) )               &
                                     + be*( f(i,4,k)- n_sym*f(i,2,k) )               &
                                     + ce*( f(i,5,k)- n_sym*f(i,3,k) )

                        df(i,   3,k) = ae*( f(i,4,k)-       f(i,2,k) )               &
                                     + be*( f(i,5,k)-       f(i,1,k) )               &
                                     + ce*( f(i,6,k)- n_sym*f(i,2,k) )
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,my-2,k) = (- 1.*f(i,my  ,k)+ 8.*f(i,my-1,k)             &
                                        - 8.*f(i,my-3,k)+ 1.*f(i,my-4,k)             &
                                       )/12.*ds

                        df(i,my-1,k) = (+ 2.*f(i,my  ,k)+ 3.*f(i,my-1,k)             &
                                        - 6.*f(i,my-2,k)+ 1.*f(i,my-3,k)             &
                                       )/ 6. *ds

                        df(i,my  ,k) = ( 11.*f(i,my  ,k)-18.*f(i,my-1,k)             &
                                        + 9.*f(i,my-2,k)- 2.*f(i,my-3,k)             &
                                       )/ 6. *ds

                     end do
                  end do
               end if
            end if

         else                   ! Unsupported IBOUND
            if ( myid.eq.0 ) then
               write(6,2) ibound
 2             format('IBOUND = ', i4,' is not supported')
            end if
            call terminate_run(6,0)
         end if

         if(unif_grid_y.ne.1) then
           do j = 1, 3
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
           do j = my-2, my
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
         endif
!
!  8th order: (3,3,4,6-8-6,4,3,3)
                                !
      else if ( iorder .eq. 8 ) then

         ae =  4./  5. *ds
         be = -1./  5. *ds
         ce =  4./105. *ds
         de = -1./280. *ds
!
!  Internal nodes:
!

         do k = 1, mz
            do j = 5, my-4
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j+1,k)-f(i,j-1,k) )                          &
                            + be *( f(i,j+2,k)-f(i,j-2,k) )                          &
                            + ce *( f(i,j+3,k)-f(i,j-3,k) )                          &
                            + de *( f(i,j+4,k)-f(i,j-4,k) )
               end do
            end do
         end do

         if(unif_grid_y.ne.1) then
           do j = 5, my-4
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( lnbr(3) >= 0 )then
            call MPI_Wait(req(1),stat(:,1),ierr)
         endif 
#endif
!
!  Internal nodes: edges
!
         if (neighbor(3).ge.0) then
            do k = 1, mz
               do i = 1, mx
                  df(i,1,k) = ae *( f(i,2,k)-neg_f(i,4,k) )                          &
                            + be *( f(i,3,k)-neg_f(i,3,k) )                          &
                            + ce *( f(i,4,k)-neg_f(i,2,k) )                          &
                            + de *( f(i,5,k)-neg_f(i,1,k) )
                  df(i,2,k) = ae *( f(i,3,k)-    f(i,1,k) )                          &
                            + be *( f(i,4,k)-neg_f(i,4,k) )                          &
                            + ce *( f(i,5,k)-neg_f(i,3,k) )                          &
                            + de *( f(i,6,k)-neg_f(i,2,k) )
                  df(i,3,k) = ae *( f(i,4,k)-    f(i,2,k) )                          &
                            + be *( f(i,5,k)-    f(i,1,k) )                          &
                            + ce *( f(i,6,k)-neg_f(i,4,k) )                          &
                            + de *( f(i,7,k)-neg_f(i,3,k) )
                  df(i,4,k) = ae *( f(i,5,k)-    f(i,3,k) )                          &
                            + be *( f(i,6,k)-    f(i,2,k) )                          &
                            + ce *( f(i,7,k)-    f(i,1,k) )                          &
                            + de *( f(i,8,k)-neg_f(i,4,k) )
               end do
            end do
         end if

#ifndef COARRAYCOMMUNICATION
         if( lnbr(4) >= 0 ) then
            call MPI_Wait(req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(4).ge.0) then
            do k = 1, mz
               do i = 1, mx
                  df(i,my  ,k) = ae *( pos_f(i,1,k)   -f(i,my-1,k) )                 &
                               + be *( pos_f(i,2,k)   -f(i,my-2,k) )                 &
                               + ce *( pos_f(i,3,k)   -f(i,my-3,k) )                 &
                               + de *( pos_f(i,4,k)   -f(i,my-4,k) )
                  df(i,my-1,k) = ae *(     f(i,my,k)  -f(i,my-2,k) )                 &
                               + be *( pos_f(i,1,k)   -f(i,my-3,k) )                 &
                               + ce *( pos_f(i,2,k)   -f(i,my-4,k) )                 &
                               + de *( pos_f(i,3,k)   -f(i,my-5,k) )
                  df(i,my-2,k) = ae *(     f(i,my-1,k)-f(i,my-3,k) )                 &
                               + be *(     f(i,my,k)  -f(i,my-4,k) )                 &
                               + ce *( pos_f(i,1,k)   -f(i,my-5,k) )                 &
                               + de *( pos_f(i,2,k)   -f(i,my-6,k) )
                  df(i,my-3,k) = ae *(     f(i,my-2,k)-f(i,my-4,k) )                 &
                               + be *(     f(i,my-1,k)-f(i,my-5,k) )                 &
                               + ce *(     f(i,my,k)  -f(i,my-6,k) )                 &
                               + de *( pos_f(i,1,k)   -f(i,my-7,k) )
               end do
            end do
         end if
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_y .eq. 0 ) .and. ( periodic_y .eq. 0 )) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,   1,k) = (-11.*f(i,   1,k)+18.*f(i,   2,k)             &
                                        - 9.*f(i,   3,k)+ 2.*f(i,   4,k)             &
                                       )/ 6. *ds

                        df(i,   2,k) = (- 2.*f(i,   1,k)- 3.*f(i,   2,k)             &
                                        + 6.*f(i,   3,k)- 1.*f(i,   4,k)             &
                                       )/ 6. *ds

                        df(i,   3,k) = (+ 1.*f(i,   1,k)- 8.*f(i,   2,k)             &
                                        + 8.*f(i,   4,k)-  1.*f(i,  5,k)             &
                                       )/12.*ds

                        df(i,   4,k) = (- 1.*f(i,   1,k)+  9.*f(i,  2,k)             &
                                        -45.*f(i,   3,k)+ 45.*f(i,  5,k)             &
                                        - 9.*f(i,   6,k)+  1.*f(i,  7,k)             &
                                       )/60.*ds
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,my-3,k) =(+ 1.*f(i,my  ,k)-  9.*f(i,my-1,k)             &
                                       +45.*f(i,my-2,k)- 45.*f(i,my-4,k)             &
                                       + 9.*f(i,my-5,k)-  1.*f(i,my-6,k)             &
                                      )/60.*ds

                        df(i,my-2,k) =(- 1.*f(i,my  ,k)+  8.*f(i,my-1,k)             &
                                       - 8.*f(i,my-3,k)+  1.*f(i,my-4,k)             &
                                      )/12.*ds

                        df(i,my-1,k) =(+ 2.*f(i,my  ,k)+  3.*f(i,my-1,k)             &
                                       - 6.*f(i,my-2,k)+  1.*f(i,my-3,k)             &
                                      )/ 6. *ds

                        df(i,my  ,k) =(+11.*f(i,my  ,k)- 18.*f(i,my-1,k)             &
                                       + 9.*f(i,my-2,k)-  2.*f(i,my-3,k)             &
                                      )/ 6. *ds
                     end do
                  end do
               end if

            elseif (( isym_y .eq. 0 ) .and. ( periodic_y .eq. 1 )) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,   1,k) = ae *( f(i   ,2,k) - neg_f(i,4,k))             &
                                     + be *( f(i   ,3,k) - neg_f(i,3,k))             &
                                     + ce *( f(i   ,4,k) - neg_f(i,2,k))             &
                                     + de *( f(i   ,5,k) - neg_f(i,1,k))

                        df(i,   2,k) = ae *( f(i   ,3,k) -     f(i,1,k))             &
                                     + be *( f(i   ,4,k) - neg_f(i,4,k))             &
                                     + ce *( f(i   ,5,k) - neg_f(i,3,k))             &
                                     + de *( f(i   ,6,k) - neg_f(i,2,k))

                        df(i,   3,k) = ae *( f(i   ,4,k) -     f(i,2,k))             &
                                     + be *( f(i   ,5,k) -     f(i,1,k))             &
                                     + ce *( f(i   ,6,k) - neg_f(i,4,k))             &
                                     + de *( f(i   ,7,k) - neg_f(i,3,k))

                        df(i,   4,k) = ae *( f(i   ,5,k) -     f(i,3,k))             &
                                     + be *( f(i   ,6,k) -     f(i,2,k))             &
                                     + ce *( f(i   ,7,k) -     f(i,1,k))             &
                                     + de *( f(i   ,8,k) - neg_f(i,4,k))
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,my-3,k) = ae*(     f(i,my-2,k)-f(i,my-4,k))             &
                                     + be*(     f(i,my-1,k)-f(i,my-5,k))             &
                                     + ce*(     f(i,my  ,k)-f(i,my-6,k))             &
                                     + de*( pos_f(i,   1,k)-f(i,my-7,k))

                        df(i,my-2,k) = ae*(     f(i,my-1,k)-f(i,my-3,k))             &
                                     + be*(     f(i,my  ,k)-f(i,my-4,k))             &
                                     + ce*( pos_f(i,   1,k)-f(i,my-5,k))             &
                                     + de*( pos_f(i,   2,k)-f(i,my-6,k))

                        df(i,my-1,k) = ae*(     f(i,my  ,k)-f(i,my-2,k))             &
                                     + be*( pos_f(i,   1,k)-f(i,my-3,k))             &
                                     + ce*( pos_f(i,   2,k)-f(i,my-4,k))             &
                                     + de*( pos_f(i,   3,k)-f(i,my-5,k))

                        df(i,my  ,k) = ae*( pos_f(i,   1,k)-f(i,my-1,k))             &
                                     + be*( pos_f(i,   2,k)-f(i,my-2,k))             &
                                     + ce*( pos_f(i,   3,k)-f(i,my-3,k))             &
                                     + de*( pos_f(i,   4,k)-f(i,my-4,k))
                     end do
                  end do
               end if

            elseif ( isym_y .eq. 1 ) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,  1,k) =  ae*( f(i,2,k) - n_sym*f(i,2,k) )              &
                                    +  be*( f(i,3,k) - n_sym*f(i,3,k) )              &
                                    +  ce*( f(i,4,k) - n_sym*f(i,4,k) )              &
                                    +  de*( f(i,5,k) - n_sym*f(i,5,k) )

                        df(i,  2,k) =  ae*( f(i,3,k) -       f(i,1,k) )              &
                                    +  be*( f(i,4,k) - n_sym*f(i,2,k) )              &
                                    +  ce*( f(i,5,k) - n_sym*f(i,3,k) )              &
                                    +  de*( f(i,6,k) - n_sym*f(i,4,k) )

                        df(i,  3,k) =  ae*( f(i,4,k) -       f(i,2,k) )              &
                                    +  be*( f(i,5,k) -       f(i,1,k) )              &
                                    +  ce*( f(i,6,k) - n_sym*f(i,2,k) )              &
                                    +  de*( f(i,7,k) - n_sym*f(i,3,k) )

                        df(i,  4,k) =  ae*( f(i,5,k) -       f(i,3,k) )              &
                                    +  be*( f(i,6,k) -       f(i,2,k) )              &
                                    +  ce*( f(i,7,k) -       f(i,1,k) )              &
                                    +  de*( f(i,8,k) - n_sym*f(i,2,k) )

                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,my-3,k) = (+ 1.*f(i,my  ,k)- 9.*f(i,my-1,k)             &
                                        +45.*f(i,my-2,k)-45.*f(i,my-4,k)             &
                                        + 9.*f(i,my-5,k)- 1.*f(i,my-6,k)             &
                                       )/60.*ds

                        df(i,my-2,k) = (- 1.*f(i,my  ,k)+ 8.*f(i,my-1,k)             &
                                        - 8.*f(i,my-3,k)+ 1.*f(i,my-4,k)             &
                                       )/12.*ds

                        df(i,my-1,k) = (+ 2.*f(i,my  ,k)+ 3.*f(i,my-1,k)             &
                                        - 6.*f(i,my-2,k)+ 1.*f(i,my-3,k)             &
                                       )/ 6.*ds

                        df(i,my  ,k) = (+11.*f(i,my  ,k)-18.*f(i,my-1,k)             &
                                        + 9.*f(i,my-2,k)- 2.*f(i,my-3,k)             &
                                       )/ 6. *ds
                     end do
                  end do
               end if
            end if
         else                   ! Unsupported IBOUND
            if ( myid.eq.0 ) then
               write(6,2) ibound
            end if
            call terminate_run(6,0)
         end if
         if(unif_grid_y.ne.1) then
           do j = 1, 4
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
           do j = my-3, my
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
         endif
      else            ! Unsupported IORDER
         if ( myid.eq.0 ) then
            write(6,1) iorder
 1          format('IORDER = ', i4,' is not supported')
         end if
         call terminate_run(6,0)
      end if


!-----------------------------------------------------------------------------------------
#ifndef COARRAYCOMMUNICATION
  if( lnbr(3) >= 0 )then
     call MPI_Wait(req(2),stat(:,2),ierr)
  endif 
  if( lnbr(4) >= 0 ) then
     call MPI_Wait(req(4),stat(:,4),ierr)
  endif 
#endif

  return
  end subroutine derivative_y_calc

!=========================================================================================
  subroutine derivative_y_calc_buff(mx,my,mz,f,df,scale_1,n_sym, idx, label)
!=========================================================================================
! evaluates the first derivative in y-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!  ae     - Explicit central difference stencil coefficient at (j +/- 1)
!  be     - Explicit central difference stencil coefficient at (j +/- 2)
!  ce     - Explicit central difference stencil coefficient at (j +/- 3)
!  de     - Explicit central difference stencil coefficient at (j +/- 4)
!  mx     - number of grid points in x-direction
!  my     - number of grid points in y-direction
!  mz     - number of grid points in z-direction
!
!       d()/dy = [ d(eta)/dy ]*[ d()/d(eta) ]
!        [ dy/d(eta) ]^-1*[ d()/d(eta) ]

!  scale_1- Proportional to ( dy/d(eta) )^-1 where eta is fictitious
!           uniform grid and y is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along y-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_y
  use param_m, only : periodic_y, vary_in_y, iorder
  use derivative_m, only : isym_y
  use derivative_m, only : ibound

  use derivative_m
  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(   my   ) :: scale_1

  integer, intent(in) :: n_sym
  integer, intent(in) :: idx

  integer :: i, j, k
  integer, dimension(6) :: lnbr(6)
  integer, dimension(MPI_Status_size,4) :: stat

  real :: ds,ae,be,ce,de

  character(*), intent(in) :: label
!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_y .eq. 0 ) then
     df = 0.0
    return
  endif

  ! check for misuse
#ifdef DERIV_CHECK_LABELS
  if( trim(deriv_y_list(idx)%fldname) .ne. trim(label) ) then
      write(*,'(2a,i5)') 'Mismatch name in calc: ', trim(label), idx
      call print_deriv_list(6)
  endif
#endif
!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_y.ne.1) then
    ds = real(my*ypes-1)
  else
    ds = real(my*ypes-1) * scale_1(1)
  endif

  lnbr = neighbor

  if(periodic_y.eq.1) then
    if(yid.eq.0) then
      lnbr(3) = zid*xpes*ypes+(ypes-1)*xpes+xid
    endif
    if(yid.eq.ypes-1)then
      lnbr(4) = zid*xpes*ypes+(0     )*xpes+xid
    endif
  endif

!-----------------------------------------------------------------------------------------
!  6th order explicit: (3,3,4-6-4,3,3)
!
      if ( iorder .eq. 6 ) then

         ae =  3./ 4. *ds
         be = -3./20. *ds
         ce =  1./60. *ds
!
!  Internal nodes:
!
         do k = 1, mz
            do j = 4, my-3
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j+1,k)-f(i,j-1,k) )                          &
                            + be *( f(i,j+2,k)-f(i,j-2,k) )                          &
                            + ce *( f(i,j+3,k)-f(i,j-3,k) )
               end do
            end do
         end do

         if(unif_grid_y.ne.1) then
           do j = 4, my-3
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( deriv_y_list(idx)%neg_nbr >= 0 )then
            call MPI_Wait(deriv_y_list(idx)%req(1),stat(:,1),ierr)
         endif 
#endif
!
!  Internal nodes: edges
!
         if (neighbor(3).ge.0) then
            do k = 1, mz
               do i = 1, mx
                  df(i,1,k) = ae *( f(i,2,k)-neg_f_y_buf(i,3,k,idx) )                          &
                          + be *( f(i,3,k)-neg_f_y_buf(i,2,k,idx) )                            &
                          + ce *( f(i,4,k)-neg_f_y_buf(i,1,k,idx) )
                  df(i,2,k) = ae *( f(i,3,k)-    f(i,1,k) )                          &
                          + be *( f(i,4,k)-neg_f_y_buf(i,3,k,idx) )                            &
                          + ce *( f(i,5,k)-neg_f_y_buf(i,2,k,idx) )
                  df(i,3,k) = ae *( f(i,4,k)-    f(i,2,k) )                          &
                            + be *( f(i,5,k)-    f(i,1,k) )                          &
                            + ce *( f(i,6,k)-neg_f_y_buf(i,3,k,idx) )
               end do
            end do
         endif

#ifndef COARRAYCOMMUNICATION
         if( deriv_y_list(idx)%pos_nbr >= 0 )then
            call MPI_Wait(deriv_y_list(idx)%req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(4).ge.0) then
            do k = 1, mz
               do i = 1, mx
                  df(i,my  ,k) = ae *( pos_f_y_buf(i,1,k,idx)   -f(i,my-1,k) )                 &
                               + be *( pos_f_y_buf(i,2,k,idx)   -f(i,my-2,k) )                 &
                               + ce *( pos_f_y_buf(i,3,k,idx)   -f(i,my-3,k) )
                  df(i,my-1,k) = ae *(     f(i,my,k)  -f(i,my-2,k) )                 &
                               + be *( pos_f_y_buf(i,1,k,idx)   -f(i,my-3,k) )                 &
                               + ce *( pos_f_y_buf(i,2,k,idx)   -f(i,my-4,k) )
                  df(i,my-2,k) = ae *(     f(i,my-1,k)-f(i,my-3,k) )                 &
                               + be *(     f(i,my,k)  -f(i,my-4,k) )                 &
                               + ce *( pos_f_y_buf(i,1,k,idx)   -f(i,my-5,k) )
               end do
            end do
         endif

!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_y .eq. 0 ) .and. ( periodic_y .eq. 0 )) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,   1,k) = (-11.*f(i,1,k) + 18.*f(i,2,k)                 &
                                        - 9.*f(i,3,k) +  2.*f(i,4,k))                &
                                        / 6. *ds

                        df(i,   2,k) = (- 2.*f(i,1,k) -  3.*f(i,2,k)                 &
                                        + 6.*f(i,3,k) -  1.*f(i,4,k))                &
                                        / 6. *ds

                        df(i,   3,k) = (+ 1.*f(i,1,k) -  8.*f(i,2,k)                 &
                                        + 8.*f(i,4,k) -  1.*f(i,5,k))                &
                                        /12.*ds
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,my-2,k) = (- 1.*f(i,my  ,k)+ 8.*f(i,my-1,k)             &
                                        - 8.*f(i,my-3,k)+ 1.*f(i,my-4,k)             &
                                       )/12.*ds

                        df(i,my-1,k) = (+ 2.*f(i,my  ,k)+ 3.*f(i,my-1,k)             &
                                        - 6.*f(i,my-2,k)+ 1.*f(i,my-3,k)             &
                                       )/ 6. *ds

                        df(i,my  ,k) = (+11.*f(i,my  ,k)-18.*f(i,my-1,k)             &
                                        + 9.*f(i,my-2,k)- 2.*f(i,my-3,k)             &
                                       )/ 6. *ds
                     end do
                  end do
               end if
            elseif (( isym_y .eq. 0 ) .and. ( periodic_y .eq. 1 )) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,   1,k) = ae *( f(i   ,2,k)- neg_f_y_buf(i,3,k,idx) )             &
                                     + be *( f(i   ,3,k)- neg_f_y_buf(i,2,k,idx) )             &
                                     + ce *( f(i   ,4,k)- neg_f_y_buf(i,1,k,idx) )

                        df(i,   2,k) = ae *( f(i   ,3,k)-     f(i,1,k) )             &
                                     + be *( f(i   ,4,k)- neg_f_y_buf(i,3,k,idx) )             &
                                     + ce *( f(i   ,5,k)- neg_f_y_buf(i,2,k,idx) )

                        df(i,   3,k) = ae *( f(i   ,4,k)-     f(i,2,k) )             &
                                     + be *( f(i   ,5,k)-     f(i,1,k) )             &
                                     + ce *( f(i   ,6,k)- neg_f_y_buf(i,3,k,idx) )
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,my-2,k) = ae*(    f(i,my-1,k)-f(i,my-3,k) )             &
                                     + be*(    f(i,my  ,k)-f(i,my-4,k) )             &
                                     + ce*(pos_f_y_buf(i,   1,k,idx)-f(i,my-5,k) )

                        df(i,my-1,k) = ae*(    f(i,my  ,k)-f(i,my-2,k) )             &
                                     + be*(pos_f_y_buf(i,   1,k,idx)-f(i,my-3,k) )             &
                                     + ce*(pos_f_y_buf(i,   2,k,idx)-f(i,my-4,k) )

                        df(i,my  ,k) = ae*(pos_f_y_buf(i,   1,k,idx)-f(i,my-1,k) )             &
                                     + be*(pos_f_y_buf(i,   2,k,idx)-f(i,my-2,k) )             &
                                     + ce*(pos_f_y_buf(i,   3,k,idx)-f(i,my-3,k) )

                     end do
                  end do
               end if

            elseif ( isym_y .eq. 1 ) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,   1,k) = ae*( f(i,2,k)- n_sym*f(i,2,k) )               &
                                     + be*( f(i,3,k)- n_sym*f(i,3,k) )               &
                                     + ce*( f(i,4,k)- n_sym*f(i,4,k) )

                        df(i,   2,k) = ae*( f(i,3,k)-       f(i,1,k) )               &
                                     + be*( f(i,4,k)- n_sym*f(i,2,k) )               &
                                     + ce*( f(i,5,k)- n_sym*f(i,3,k) )

                        df(i,   3,k) = ae*( f(i,4,k)-       f(i,2,k) )               &
                                     + be*( f(i,5,k)-       f(i,1,k) )               &
                                     + ce*( f(i,6,k)- n_sym*f(i,2,k) )
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,my-2,k) = (- 1.*f(i,my  ,k)+ 8.*f(i,my-1,k)             &
                                        - 8.*f(i,my-3,k)+ 1.*f(i,my-4,k)             &
                                       )/12.*ds

                        df(i,my-1,k) = (+ 2.*f(i,my  ,k)+ 3.*f(i,my-1,k)             &
                                        - 6.*f(i,my-2,k)+ 1.*f(i,my-3,k)             &
                                       )/ 6. *ds

                        df(i,my  ,k) = ( 11.*f(i,my  ,k)-18.*f(i,my-1,k)             &
                                        + 9.*f(i,my-2,k)- 2.*f(i,my-3,k)             &
                                       )/ 6. *ds

                     end do
                  end do
               end if
            end if

         else                   ! Unsupported IBOUND
            if ( myid.eq.0 ) then
               write(6,2) ibound
 2             format('IBOUND = ', i4,' is not supported')
            end if
            call terminate_run(6,0)
         end if

         if(unif_grid_y.ne.1) then
           do j = 1, 3
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
           do j = my-2, my
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
         endif
!
!  8th order: (3,3,4,6-8-6,4,3,3)
                                !
      else if ( iorder .eq. 8 ) then

         ae =  4./  5. *ds
         be = -1./  5. *ds
         ce =  4./105. *ds
         de = -1./280. *ds
!
!  Internal nodes:
!

         do k = 1, mz
            do j = 5, my-4
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j+1,k)-f(i,j-1,k) )                          &
                            + be *( f(i,j+2,k)-f(i,j-2,k) )                          &
                            + ce *( f(i,j+3,k)-f(i,j-3,k) )                          &
                            + de *( f(i,j+4,k)-f(i,j-4,k) )
               end do
            end do
         end do

         if(unif_grid_y.ne.1) then
           do j = 5, my-4
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( deriv_y_list(idx)%neg_nbr >= 0 )then
            call MPI_Wait(deriv_y_list(idx)%req(1),stat(:,1),ierr)
         endif 
#endif
!
!  Internal nodes: edges
!
         if (neighbor(3).ge.0) then
            do k = 1, mz
               do i = 1, mx
                  df(i,1,k) = ae *( f(i,2,k)-neg_f_y_buf(i,4,k,idx) )                          &
                            + be *( f(i,3,k)-neg_f_y_buf(i,3,k,idx) )                          &
                            + ce *( f(i,4,k)-neg_f_y_buf(i,2,k,idx) )                          &
                            + de *( f(i,5,k)-neg_f_y_buf(i,1,k,idx) )
                  df(i,2,k) = ae *( f(i,3,k)-    f(i,1,k) )                          &
                            + be *( f(i,4,k)-neg_f_y_buf(i,4,k,idx) )                          &
                            + ce *( f(i,5,k)-neg_f_y_buf(i,3,k,idx) )                          &
                            + de *( f(i,6,k)-neg_f_y_buf(i,2,k,idx) )
                  df(i,3,k) = ae *( f(i,4,k)-    f(i,2,k) )                          &
                            + be *( f(i,5,k)-    f(i,1,k) )                          &
                            + ce *( f(i,6,k)-neg_f_y_buf(i,4,k,idx) )                          &
                            + de *( f(i,7,k)-neg_f_y_buf(i,3,k,idx) )
                  df(i,4,k) = ae *( f(i,5,k)-    f(i,3,k) )                          &
                            + be *( f(i,6,k)-    f(i,2,k) )                          &
                            + ce *( f(i,7,k)-    f(i,1,k) )                          &
                            + de *( f(i,8,k)-neg_f_y_buf(i,4,k,idx) )
               end do
            end do
         end if

#ifndef COARRAYCOMMUNICATION
         if( deriv_y_list(idx)%pos_nbr >= 0 )then
            call MPI_Wait(deriv_y_list(idx)%req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(4).ge.0) then
            do k = 1, mz
               do i = 1, mx
                  df(i,my  ,k) = ae *( pos_f_y_buf(i,1,k,idx)   -f(i,my-1,k) )                 &
                               + be *( pos_f_y_buf(i,2,k,idx)   -f(i,my-2,k) )                 &
                               + ce *( pos_f_y_buf(i,3,k,idx)   -f(i,my-3,k) )                 &
                               + de *( pos_f_y_buf(i,4,k,idx)   -f(i,my-4,k) )
                  df(i,my-1,k) = ae *(     f(i,my,k)  -f(i,my-2,k) )                 &
                               + be *( pos_f_y_buf(i,1,k,idx)   -f(i,my-3,k) )                 &
                               + ce *( pos_f_y_buf(i,2,k,idx)   -f(i,my-4,k) )                 &
                               + de *( pos_f_y_buf(i,3,k,idx)   -f(i,my-5,k) )
                  df(i,my-2,k) = ae *(     f(i,my-1,k)-f(i,my-3,k) )                 &
                               + be *(     f(i,my,k)  -f(i,my-4,k) )                 &
                               + ce *( pos_f_y_buf(i,1,k,idx)   -f(i,my-5,k) )                 &
                               + de *( pos_f_y_buf(i,2,k,idx)   -f(i,my-6,k) )
                  df(i,my-3,k) = ae *(     f(i,my-2,k)-f(i,my-4,k) )                 &
                               + be *(     f(i,my-1,k)-f(i,my-5,k) )                 &
                               + ce *(     f(i,my,k)  -f(i,my-6,k) )                 &
                               + de *( pos_f_y_buf(i,1,k,idx)   -f(i,my-7,k) )
               end do
            end do
         end if
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_y .eq. 0 ) .and. ( periodic_y .eq. 0 )) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx

                        df(i,   1,k) = (-11.*f(i,   1,k)+18.*f(i,   2,k)             &
                                        - 9.*f(i,   3,k)+ 2.*f(i,   4,k)             &
                                       )/ 6. *ds

                        df(i,   2,k) = (- 2.*f(i,   1,k)- 3.*f(i,   2,k)             &
                                        + 6.*f(i,   3,k)- 1.*f(i,   4,k)             &
                                       )/ 6. *ds

                        df(i,   3,k) = (+ 1.*f(i,   1,k)- 8.*f(i,   2,k)             &
                                        + 8.*f(i,   4,k)-  1.*f(i,  5,k)             &
                                       )/12.*ds

                        df(i,   4,k) = (- 1.*f(i,   1,k)+  9.*f(i,  2,k)             &
                                        -45.*f(i,   3,k)+ 45.*f(i,  5,k)             &
                                        - 9.*f(i,   6,k)+  1.*f(i,  7,k)             &
                                       )/60.*ds
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,my-3,k) =(+ 1.*f(i,my  ,k)-  9.*f(i,my-1,k)             &
                                       +45.*f(i,my-2,k)- 45.*f(i,my-4,k)             &
                                       + 9.*f(i,my-5,k)-  1.*f(i,my-6,k)             &
                                      )/60.*ds

                        df(i,my-2,k) =(- 1.*f(i,my  ,k)+  8.*f(i,my-1,k)             &
                                       - 8.*f(i,my-3,k)+  1.*f(i,my-4,k)             &
                                      )/12.*ds

                        df(i,my-1,k) =(+ 2.*f(i,my  ,k)+  3.*f(i,my-1,k)             &
                                       - 6.*f(i,my-2,k)+  1.*f(i,my-3,k)             &
                                      )/ 6. *ds

                        df(i,my  ,k) =(+11.*f(i,my  ,k)- 18.*f(i,my-1,k)             &
                                       + 9.*f(i,my-2,k)-  2.*f(i,my-3,k)             &
                                      )/ 6. *ds
                     end do
                  end do
               end if

            elseif (( isym_y .eq. 0 ) .and. ( periodic_y .eq. 1 )) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,   1,k) = ae *( f(i   ,2,k) - neg_f_y_buf(i,4,k,idx))             &
                                     + be *( f(i   ,3,k) - neg_f_y_buf(i,3,k,idx))             &
                                     + ce *( f(i   ,4,k) - neg_f_y_buf(i,2,k,idx))             &
                                     + de *( f(i   ,5,k) - neg_f_y_buf(i,1,k,idx))

                        df(i,   2,k) = ae *( f(i   ,3,k) -     f(i,1,k))             &
                                     + be *( f(i   ,4,k) - neg_f_y_buf(i,4,k,idx))             &
                                     + ce *( f(i   ,5,k) - neg_f_y_buf(i,3,k,idx))             &
                                     + de *( f(i   ,6,k) - neg_f_y_buf(i,2,k,idx))

                        df(i,   3,k) = ae *( f(i   ,4,k) -     f(i,2,k))             &
                                     + be *( f(i   ,5,k) -     f(i,1,k))             &
                                     + ce *( f(i   ,6,k) - neg_f_y_buf(i,4,k,idx))             &
                                     + de *( f(i   ,7,k) - neg_f_y_buf(i,3,k,idx))

                        df(i,   4,k) = ae *( f(i   ,5,k) -     f(i,3,k))             &
                                     + be *( f(i   ,6,k) -     f(i,2,k))             &
                                     + ce *( f(i   ,7,k) -     f(i,1,k))             &
                                     + de *( f(i   ,8,k) - neg_f_y_buf(i,4,k,idx))
                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,my-3,k) = ae*(     f(i,my-2,k)-f(i,my-4,k))             &
                                     + be*(     f(i,my-1,k)-f(i,my-5,k))             &
                                     + ce*(     f(i,my  ,k)-f(i,my-6,k))             &
                                     + de*( pos_f_y_buf(i,   1,k,idx)-f(i,my-7,k))

                        df(i,my-2,k) = ae*(     f(i,my-1,k)-f(i,my-3,k))             &
                                     + be*(     f(i,my  ,k)-f(i,my-4,k))             &
                                     + ce*( pos_f_y_buf(i,   1,k,idx)-f(i,my-5,k))             &
                                     + de*( pos_f_y_buf(i,   2,k,idx)-f(i,my-6,k))

                        df(i,my-1,k) = ae*(     f(i,my  ,k)-f(i,my-2,k))             &
                                     + be*( pos_f_y_buf(i,   1,k,idx)-f(i,my-3,k))             &
                                     + ce*( pos_f_y_buf(i,   2,k,idx)-f(i,my-4,k))             &
                                     + de*( pos_f_y_buf(i,   3,k,idx)-f(i,my-5,k))

                        df(i,my  ,k) = ae*( pos_f_y_buf(i,   1,k,idx)-f(i,my-1,k))             &
                                     + be*( pos_f_y_buf(i,   2,k,idx)-f(i,my-2,k))             &
                                     + ce*( pos_f_y_buf(i,   3,k,idx)-f(i,my-3,k))             &
                                     + de*( pos_f_y_buf(i,   4,k,idx)-f(i,my-4,k))
                     end do
                  end do
               end if

            elseif ( isym_y .eq. 1 ) then

               if (neighbor(3).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,  1,k) =  ae*( f(i,2,k) - n_sym*f(i,2,k) )              &
                                    +  be*( f(i,3,k) - n_sym*f(i,3,k) )              &
                                    +  ce*( f(i,4,k) - n_sym*f(i,4,k) )              &
                                    +  de*( f(i,5,k) - n_sym*f(i,5,k) )

                        df(i,  2,k) =  ae*( f(i,3,k) -       f(i,1,k) )              &
                                    +  be*( f(i,4,k) - n_sym*f(i,2,k) )              &
                                    +  ce*( f(i,5,k) - n_sym*f(i,3,k) )              &
                                    +  de*( f(i,6,k) - n_sym*f(i,4,k) )

                        df(i,  3,k) =  ae*( f(i,4,k) -       f(i,2,k) )              &
                                    +  be*( f(i,5,k) -       f(i,1,k) )              &
                                    +  ce*( f(i,6,k) - n_sym*f(i,2,k) )              &
                                    +  de*( f(i,7,k) - n_sym*f(i,3,k) )

                        df(i,  4,k) =  ae*( f(i,5,k) -       f(i,3,k) )              &
                                    +  be*( f(i,6,k) -       f(i,2,k) )              &
                                    +  ce*( f(i,7,k) -       f(i,1,k) )              &
                                    +  de*( f(i,8,k) - n_sym*f(i,2,k) )

                     end do
                  end do
               end if
               if (neighbor(4).lt.0) then
                  do k = 1, mz
                     do i = 1, mx
                        df(i,my-3,k) = (+ 1.*f(i,my  ,k)- 9.*f(i,my-1,k)             &
                                        +45.*f(i,my-2,k)-45.*f(i,my-4,k)             &
                                        + 9.*f(i,my-5,k)- 1.*f(i,my-6,k)             &
                                       )/60.*ds

                        df(i,my-2,k) = (- 1.*f(i,my  ,k)+ 8.*f(i,my-1,k)             &
                                        - 8.*f(i,my-3,k)+ 1.*f(i,my-4,k)             &
                                       )/12.*ds

                        df(i,my-1,k) = (+ 2.*f(i,my  ,k)+ 3.*f(i,my-1,k)             &
                                        - 6.*f(i,my-2,k)+ 1.*f(i,my-3,k)             &
                                       )/ 6.*ds

                        df(i,my  ,k) = (+11.*f(i,my  ,k)-18.*f(i,my-1,k)             &
                                        + 9.*f(i,my-2,k)- 2.*f(i,my-3,k)             &
                                       )/ 6. *ds
                     end do
                  end do
               end if
            end if
         else                   ! Unsupported IBOUND
            if ( myid.eq.0 ) then
               write(6,2) ibound
            end if
            call terminate_run(6,0)
         end if
         if(unif_grid_y.ne.1) then
           do j = 1, 4
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
           do j = my-3, my
             df(:,j,:) = scale_1(j)*df(:,j,:)
           end do
         endif
      else            ! Unsupported IORDER
         if ( myid.eq.0 ) then
            write(6,1) iorder
 1          format('IORDER = ', i4,' is not supported')
         end if
         call terminate_run(6,0)
      end if


!-----------------------------------------------------------------------------------------
#ifndef COARRAYCOMMUNICATION
  if( deriv_y_list(idx)%neg_nbr >= 0 )then
     call MPI_Wait(deriv_y_list(idx)%req(2),stat(:,2),ierr)
  endif 
  if( deriv_y_list(idx)%pos_nbr >= 0 ) then
     call MPI_Wait(deriv_y_list(idx)%req(4),stat(:,4),ierr)
  endif 

  deriv_y_list(idx)%inuse = .false.
#endif

  return
  end subroutine derivative_y_calc_buff

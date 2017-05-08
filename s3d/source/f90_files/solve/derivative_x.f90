#include "globalDefines.h"
! REVISIONS:
!
!  18-MAY-2005  Evatt Hawkes - this version with infiniband work-arounds
!               problems dealing with strided data-types
! 
!  May 05 2005  Mark Fahey added Coarray communication 
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
  subroutine derivative_x(mx,my,mz,f,df,scale_1,n_sym)
!=========================================================================================
! evaluates the first derivative in x-direction using explicit differencing
! for further details of arguments, see description in header of derivative_x_calc
!-----------------------------------------------------------------------------------------
  implicit none
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(mx      ) :: scale_1
  integer, intent(in) :: n_sym

  integer, dimension(4) :: req

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_x_comm(mx,my,mz,f,req)

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_x_calc(mx,my,mz,f,df,scale_1,n_sym,req)

  return
  end subroutine derivative_x

!=========================================================================================
  subroutine derivative_x_twopart(mx,my,mz,f,df,scale_1,n_sym,idx,label)
!=========================================================================================
! evaluates the first derivative in x-direction using explicit differencing
! for further details of arguments, see description in header of derivative_x_calc
!-----------------------------------------------------------------------------------------
  implicit none
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(mx      ) :: scale_1
  integer, intent(in) :: n_sym

  character(*), intent(in) :: label
  integer, intent(inout) :: idx

#ifdef COARRAYCOMMUNICATION
  write(*,*) 'Coarrays not implemented for twopart derivatives'
  write(*,*) 'use derivative_x instead of derivative_x_twopart'
  stop
#endif

  call derivative_x_post(mx,my,mz,f,idx,label)
  call derivative_x_send(mx,my,mz,f,idx,label)

  call derivative_x_calc_buff(mx,my,mz,f,df,scale_1,n_sym,idx)

  return
  end subroutine derivative_x_twopart

!=========================================================================================
  subroutine derivative_x_comm(mx,my,mz,f,req)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_x, vary_in_x, iorder
#ifdef COARRAYCOMMUNICATION
  use derivative_m, only : neg_f => neg_f_x
  use derivative_m, only : pos_f => pos_f_x
#else
  use derivative_m, only : neg_f => neg_f_x, neg_fs => neg_fs_x
  use derivative_m, only : pos_f => pos_f_x, pos_fs => pos_fs_x
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

  if( vary_in_x .eq. 0 ) then
     return
  endif
!-----------------------------------------------------------------------------------------
! zero ghost cell arrays

  neg_f=0.0
  pos_f=0.0
!-----------------------------------------------------------------------------------------
! get neighboring cells data

  lnbr = neighbor

  if(periodic_x.eq.1) then
    if(xid.eq.0) then
       lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
    endif
    if(xid.eq.xpes-1)then
       lnbr(2) = zid*xpes*ypes+ yid*xpes+0
    endif
  endif

  req(:) = MPI_REQUEST_NULL

#ifdef COARRAYCOMMUNICATION
  if(lnbr(1)>=0) then
     neg_f(:,:,:)[lnbr(1)] = f(1:iorder/2,:,:)
  endif
  if(lnbr(2)>=0) then
     nm = mx + 1 - iorder/2
     pos_f(:,:,:)[lnbr(2)] = f(nm:mx,:,:)
  endif
#else
  if(lnbr(1)>=0) then
   ! get ghost cells from neighbor on (-x) side
     call MPI_IRecv(neg_f,(my*mz*iorder/2),MPI_REAL8,lnbr(1),1,gcomm,req(1),ierr)
   ! send ghost cells to neighbor on (-x) side
!     call MPI_ISend(f(1,1,1),1,xrows_type,lnbr(1),2,gcomm,req(2),ierr)
!    Evatt Hawkes workaround for infiniband
     pos_fs(:,:,:) = f(1:iorder/2,:,:)
     call MPI_ISend(pos_fs(1,1,1),(my*mz*iorder/2),MPI_REAL8,lnbr(1),2,gcomm,req(2),ierr)
  endif
  
  if(lnbr(2)>=0) then
   ! get ghost cells from neighbor on (+x) side
     call MPI_IRecv(pos_f,(my*mz*iorder/2),MPI_REAL8,lnbr(2),2,gcomm,req(3),ierr)
   ! send ghost cells to neighbor on (+x) side
     nm = mx + 1 - iorder/2
!     call MPI_ISend(f(nm,1,1),1,xrows_type,lnbr(2),1,gcomm,req(4),ierr)
!    Evatt Hawkes workaround for infiniband
     neg_fs(:,:,:) = f(nm:mx,:,:)
     call MPI_ISend(neg_fs(1,1,1),(my*mz*iorder/2),MPI_REAL8,lnbr(2),1,gcomm,req(4),ierr)
  endif
#endif

  return
  end subroutine derivative_x_comm
!=========================================================================================
  subroutine derivative_x_post(mx,my,mz,f,idx,label)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_x, vary_in_x, iorder

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

  if( vary_in_x .eq. 0 ) then
     return
  endif
!-----------------------------------------------------------------------------------------
! Find available index, or use supplied index
  if(  deriv_x_list(idx)%inuse ) then
      write(*,'(1a, 1i3, 1a, 1i5)') 'Error, index', idx, ' is in use for x-direction', myid
      call print_deriv_list(6)
      stop
  endif

  deriv_x_list(idx)%inuse = .true.
  deriv_x_list(idx)%fldname = trim(label)
!-----------------------------------------------------------------------------------------
! zero recv ghost cell arrays for this position

  neg_f_x_buf(:,:,:,idx) = 0.0
  pos_f_x_buf(:,:,:,idx) = 0.0
!-----------------------------------------------------------------------------------------
! get neighboring cells data

  lnbr = neighbor

  if(periodic_x.eq.1) then
    if(xid.eq.0) then
       lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
    endif
    if(xid.eq.xpes-1)then
       lnbr(2) = zid*xpes*ypes+ yid*xpes+0
    endif
  endif

  deriv_x_list(idx)%neg_nbr = lnbr(1)
  deriv_x_list(idx)%pos_nbr = lnbr(2)

  deriv_x_list(idx)%req = MPI_REQUEST_NULL

  if(lnbr(1)>=0) then
   ! get ghost cells from neighbor on (-x) side
     call MPI_IRecv(neg_f_x_buf(:,:,:,idx),(my*mz*iorder/2),& 
                    MPI_REAL8,deriv_x_list(idx)%neg_nbr,idx,&
                    gcomm,deriv_x_list(idx)%req(1),ierr)
  endif
  
  if(lnbr(2)>=0) then
   ! get ghost cells from neighbor on (+x) side
     call MPI_IRecv(pos_f_x_buf(:,:,:,idx),(my*mz*iorder/2),&
                    MPI_REAL8,deriv_x_list(idx)%pos_nbr,idx+deriv_list_size,&
                    gcomm,deriv_x_list(idx)%req(3),ierr)
  endif

  return
  end subroutine derivative_x_post

!=========================================================================================
  subroutine derivative_x_send(mx,my,mz,f,idx,label)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_x, vary_in_x, iorder

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

  if( vary_in_x .eq. 0 ) then
     return
  endif

#ifdef DERIV_CHECK_LABELS
  ! check for misuse
  if( trim(deriv_x_list(idx)%fldname) .ne.  trim(label) ) then
      write(*,'2a,i5') 'Mismatch name: ', trim(label), idx
      call print_deriv_list(6)
  endif
#endif


  if(deriv_x_list(idx)%neg_nbr>=0) then
   ! send ghost cells to neighbor on (-x) side
     pos_fs_x_buf(:,:,:,idx) = f(1:iorder/2,:,:)
     call MPI_ISend(pos_fs_x_buf(1,1,1,idx),(my*mz*iorder/2),&
                    MPI_REAL8,deriv_x_list(idx)%neg_nbr,idx+deriv_list_size,&
                    gcomm,deriv_x_list(idx)%req(2),ierr)
  endif
  
  if(deriv_x_list(idx)%pos_nbr>=0) then
   ! send ghost cells to neighbor on (+x) side
     nm = mx + 1 - iorder/2
     neg_fs_x_buf(:,:,:,idx) = f(nm:mx,:,:)
     call MPI_ISend(neg_fs_x_buf(1,1,1,idx),(my*mz*iorder/2), &
                    MPI_REAL8,deriv_x_list(idx)%pos_nbr,idx, &
                    gcomm,deriv_x_list(idx)%req(4),ierr)
  endif

  return
  end subroutine derivative_x_send
!=========================================================================================
  subroutine derivative_x_calc(mx,my,mz,f,df,scale_1,n_sym,req)
!=========================================================================================
! evaluates the first derivative in x-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!  ae     - Explicit central difference stencil coefficient at (i +/- 1)
!  be     - Explicit central difference stencil coefficient at (i +/- 2)
!  ce     - Explicit central difference stencil coefficient at (i +/- 3)
!  de     - Explicit central difference stencil coefficient at (i +/- 4)
!  mx     - number of grid points in x-direction
!  my     - number of grid points in y-direction
!  mz     - number of grid points in z-direction
!
!       d()/dx = [ d(eta)/dx ]*[ d()/d(eta) ]
!        [ dx/d(eta) ]^-1*[ d()/d(eta) ]
!
!  scale_1- Proportional to ( dx/d(eta) )^-1 where eta is fictitious
!           uniform grid and x is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along x-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_x
  use param_m, only : periodic_x, vary_in_x, iorder
  use derivative_m, only : isym_x
  use derivative_m, only : ibound
  use derivative_m, only : neg_f => neg_f_x
  use derivative_m, only : pos_f => pos_f_x

  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(mx      ) :: scale_1
  integer, dimension(4), intent(in) :: req

  integer, intent(in) :: n_sym

  integer :: i, j, k
  integer, dimension(6) :: lnbr(6)
  integer, dimension(MPI_Status_size,4) :: stat

  real :: ds,ae,be,ce,de

!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_x .eq. 0 ) then
     df = 0.0
     return
  endif
!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_x.ne.1) then
    ds = real(mx*xpes-1)
  else
    ds = real(mx*xpes-1) * scale_1(1)
  endif

  lnbr = neighbor

  if(periodic_x.eq.1) then
    if(xid.eq.0) then
       lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
    endif
    if(xid.eq.xpes-1)then
       lnbr(2) = zid*xpes*ypes+ yid*xpes+0
    endif
  endif

!-----------------------------------------------------------------------------------------
!  6th order explicit: (3,3,4-6E-4,3,3)
!
      if ( iorder .eq. 6 ) then

         ae =  3./ 4. *ds
         be = -3./20. *ds
         ce =  1./60. *ds
!
!  Internal nodes:
!
         do k = 1, mz
            do j = 1, my
               do i = 4, mx-3
                  df(i,j,k) = ae *( f(i+1,j,k)-f(i-1,j,k) )                          &
                            + be *( f(i+2,j,k)-f(i-2,j,k) )                          &
                            + ce *( f(i+3,j,k)-f(i-3,j,k) )
               end do
            end do
         end do

         if(unif_grid_x.ne.1) then
           do i = 4, mx-3
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
         endif
#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( lnbr(1) >= 0 )then
            call MPI_Wait(req(1),stat(:,1),ierr)
         endif 
#endif
!
!  Internal nodes: edges
!
         if (neighbor(1).ge.0) then
            do k = 1, mz
               do j = 1, my
                  df(1,j,k) = ae *( f(2,j,k)-neg_f(3,j,k) )                          &
                            + be *( f(3,j,k)-neg_f(2,j,k) )                          &
                            + ce *( f(4,j,k)-neg_f(1,j,k) )
                  df(2,j,k) = ae *( f(3,j,k)-    f(1,j,k) )                          &
                            + be *( f(4,j,k)-neg_f(3,j,k) )                          &
                            + ce *( f(5,j,k)-neg_f(2,j,k) )
                  df(3,j,k) = ae *( f(4,j,k)-    f(2,j,k) )                          &
                            + be *( f(5,j,k)-    f(1,j,k) )                          &
                            + ce *( f(6,j,k)-neg_f(3,j,k) )
               end do
            end do
         endif

#ifndef COARRAYCOMMUNICATION
         if( lnbr(2) >= 0 ) then
            call MPI_Wait(req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(2).ge.0) then
            do k = 1, mz
               do j = 1, my
                  df(mx,j,k)   = ae *( pos_f(1,j,k)   -f(mx-1,j,k) )                 &
                               + be *( pos_f(2,j,k)   -f(mx-2,j,k) )                 &
                               + ce *( pos_f(3,j,k)   -f(mx-3,j,k) )
                  df(mx-1,j,k) = ae *(     f(mx,j,k)  -f(mx-2,j,k) )                 &
                               + be *( pos_f(1,j,k)   -f(mx-3,j,k) )                 &
                               + ce *( pos_f(2,j,k)   -f(mx-4,j,k) )
                  df(mx-2,j,k) = ae *(     f(mx-1,j,k)-f(mx-3,j,k) )                 &
                               + be *(     f(mx,j,k)  -f(mx-4,j,k) )                 &
                               + ce *( pos_f(1,j,k)   -f(mx-5,j,k) )
               end do
            end do
         endif

!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_x .eq. 0 ) .and. ( periodic_x .eq. 0 )) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(1,j,k) = (-11.*f(1,j,k) + 18.*f(2,j,k)                    &
                                     - 9.*f(3,j,k) +  2.*f(4,j,k))/6.                &
                                     * ds

                        df(2,j,k) = (- 2.*f(1,j,k) -  3.*f(2,j,k)                    &
                                     + 6.*f(3,j,k) -  1.*f(4,j,k))/6.                &
                                     * ds

                        df(3,j,k) = (+ 1.*f(1,j,k) -  8.*f(2,j,k)                    &
                                     + 8.*f(4,j,k) -  1.*f(5,j,k))/12.               &
                                     * ds
                     enddo
                  enddo
               endif
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-2,j,k) = (- 1.*f(mx  ,j,k)+ 8.*f(mx-1,j,k)             &
                                        - 8.*f(mx-3,j,k)+ 1.*f(mx-4,j,k)             &
                                       )/12.*ds

                        df(mx-1,j,k) = (+ 2.*f(mx  ,j,k)+ 3.*f(mx-1,j,k)             &
                                        - 6.*f(mx-2,j,k)+ 1.*f(mx-3,j,k)             &
                                       )/6. *ds

                        df(mx  ,j,k) = (+11.*f(mx  ,j,k)-18.*f(mx-1,j,k)             &
                                        + 9.*f(mx-2,j,k)- 2.*f(mx-3,j,k)             &
                                        )/6. *ds

                     end do
                  end do
               endif

            elseif (( isym_x .eq. 0 ) .and. ( periodic_x .eq. 1 )) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(   1,j,k) = ae *( f(2,j,k) - neg_f(3,j,k) )               &
                                     + be *( f(3,j,k) - neg_f(2,j,k) )               &
                                     + ce *( f(4,j,k) - neg_f(1,j,k) )

                        df(   2,j,k) = ae *( f(3,j,k) -     f(1,j,k) )               &
                                     + be *( f(4,j,k) - neg_f(3,j,k) )               &
                                     + ce *( f(5,j,k) - neg_f(2,j,k) )

                        df(   3,j,k) = ae *( f(4,j,k) -     f(2,j,k) )               &
                                     + be *( f(5,j,k) -     f(1,j,k) )               &
                                     + ce *( f(6,j,k) - neg_f(3,j,k) )
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-2,j,k) =ae *(     f(mx-1,j,k)-f(mx-3,j,k))             &
                                     +be *(     f(mx  ,j,k)-f(mx-4,j,k))             &
                                     +ce *( pos_f(1   ,j,k)-f(mx-5,j,k))

                        df(mx-1,j,k) =ae *(     f(mx  ,j,k)-f(mx-2,j,k))             &
                                     +be *( pos_f(1   ,j,k)-f(mx-3,j,k))             &
                                     +ce *( pos_f(2   ,j,k)-f(mx-4,j,k))

                        df(mx  ,j,k) =ae *( pos_f(1   ,j,k)-f(mx-1,j,k))             &
                                     +be *( pos_f(2   ,j,k)-f(mx-2,j,k))             &
                                     +ce *( pos_f(3   ,j,k)-f(mx-3,j,k))

                     end do
                  end do
               end if

            elseif ( isym_x .eq. 1 ) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(1,j,k) = ae*( f(2   ,j,k)-n_sym*f(2   ,j,k) )             &
                                  + be*( f(3   ,j,k)-n_sym*f(3   ,j,k) )             &
                                  + ce*( f(4   ,j,k)-n_sym*f(4   ,j,k) )

                        df(2,j,k) = ae*( f(3   ,j,k)-      f(1   ,j,k) )             &
                                  + be*( f(4   ,j,k)-n_sym*f(2   ,j,k) )             &
                                  + ce*( f(5   ,j,k)-n_sym*f(3   ,j,k) )

                        df(3,j,k) = ae*( f(4   ,j,k)-      f(2   ,j,k) )             &
                                  + be*( f(5   ,j,k)-      f(1   ,j,k) )             &
                                  + ce*( f(6   ,j,k)-n_sym*f(2   ,j,k) )
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx  ,j,k)=(+11.*f(mx  ,j,k)-18.*f(mx-1,j,k)               &
                                      + 9.*f(mx-2,j,k)- 2.*f(mx-3,j,k))              &
                                      /6. * ds

                        df(mx-1,j,k)=(+ 2.*f(mx  ,j,k)+ 3.*f(mx-1,j,k)               &
                                      - 6.*f(mx-2,j,k)+ 1.*f(mx-3,j,k))              &
                                      /6. * ds

                        df(mx-2,j,k)=(- 1.*f(mx  ,j,k)+ 8.*f(mx-1,j,k)               &
                                      - 8.*f(mx-3,j,k)+ 1.*f(mx-4,j,k))              &
                                      /12.* ds
                     end do
                  end do
               end if

            end if
         else                   ! Unsupported IBOUND
            if ( myid.eq.0 ) then
               write(6,2) ibound
2              format('IBOUND = ', i4,' is not supported')
            end if
            call terminate_run(6,0)
         end if         ! if ( ibound ... )

         if(unif_grid_x.ne.1) then
           do i = 1, 3
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
           do i = mx-2, mx
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
         endif
!
!  8th order: (3,3,4,6-8E-6,4,3,3)
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
            do j = 1, my
               do i = 5, mx-4
                  df(i,j,k) = ae *( f(i+1,j,k)-f(i-1,j,k) )                          &
                            + be *( f(i+2,j,k)-f(i-2,j,k) )                          &
                            + ce *( f(i+3,j,k)-f(i-3,j,k) )                          &
                            + de *( f(i+4,j,k)-f(i-4,j,k) )
               end do
            end do
         end do

         if(unif_grid_x.ne.1) then
           do i = 5, mx-4
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( lnbr(1) >= 0 )then
            call MPI_Wait(req(1),stat(:,1),ierr)
         endif 
#endif

!
!  Internal nodes: edges
!
         if (neighbor(1).ge.0) then
            do k = 1, mz
               do j = 1, my
                  df(1,j,k) = ae *( f(2,j,k)-neg_f(4,j,k) )                          &
                            + be *( f(3,j,k)-neg_f(3,j,k) )                          &
                            + ce *( f(4,j,k)-neg_f(2,j,k) )                          &
                            + de *( f(5,j,k)-neg_f(1,j,k) )
                  df(2,j,k) = ae *( f(3,j,k)-    f(1,j,k) )                          &
                            + be *( f(4,j,k)-neg_f(4,j,k) )                          &
                            + ce *( f(5,j,k)-neg_f(3,j,k) )                          &
                            + de *( f(6,j,k)-neg_f(2,j,k) )
                  df(3,j,k) = ae *( f(4,j,k)-    f(2,j,k) )                          &
                            + be *( f(5,j,k)-    f(1,j,k) )                          &
                            + ce *( f(6,j,k)-neg_f(4,j,k) )                          &
                            + de *( f(7,j,k)-neg_f(3,j,k) )
                  df(4,j,k) = ae *( f(5,j,k)-    f(3,j,k) )                          &
                            + be *( f(6,j,k)-    f(2,j,k) )                          &
                            + ce *( f(7,j,k)-    f(1,j,k) )                          &
                            + de *( f(8,j,k)-neg_f(4,j,k) )
               end do
            end do
         end if

#ifndef COARRAYCOMMUNICATION
         if( lnbr(2) >= 0 ) then
            call MPI_Wait(req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(2).ge.0) then
            do k = 1, mz
               do j = 1, my
                  df(mx,  j,k) = ae *( pos_f(1,j,k)   -f(mx-1,j,k) )                 &
                               + be *( pos_f(2,j,k)   -f(mx-2,j,k) )                 &
                               + ce *( pos_f(3,j,k)   -f(mx-3,j,k) )                 &
                               + de *( pos_f(4,j,k)   -f(mx-4,j,k) )
                  df(mx-1,j,k) = ae *(     f(mx,j,k)  -f(mx-2,j,k) )                 &
                               + be *( pos_f(1,j,k)   -f(mx-3,j,k) )                 &
                               + ce *( pos_f(2,j,k)   -f(mx-4,j,k) )                 &
                               + de *( pos_f(3,j,k)   -f(mx-5,j,k) )
                  df(mx-2,j,k) = ae *(     f(mx-1,j,k)-f(mx-3,j,k) )                 &
                               + be *(     f(mx,j,k)  -f(mx-4,j,k) )                 &
                               + ce *( pos_f(1,j,k)   -f(mx-5,j,k) )                 &
                               + de *( pos_f(2,j,k)   -f(mx-6,j,k) )
                  df(mx-3,j,k) = ae *(     f(mx-2,j,k)-f(mx-4,j,k) )                 &
                               + be *(     f(mx-1,j,k)-f(mx-5,j,k) )                 &
                               + ce *(     f(mx,j,k)  -f(mx-6,j,k) )                 &
                               + de *( pos_f(1,j,k)   -f(mx-7,j,k) )
               end do
            end do
         end if
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_x .eq. 0 ) .and. ( periodic_x .eq. 0 )) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(1   ,j,k)=(-11.*f(1   ,j,k) + 18.*f(2   ,j,k)             &
                                      - 9.*f(3   ,j,k) +  2.*f(4   ,j,k)             &
                                     )/6. *ds

                        df(2   ,j,k)=(- 2.*f(1   ,j,k) -  3.*f(2   ,j,k)             &
                                      + 6.*f(3   ,j,k) -  1.*f(4   ,j,k)             &
                                     )/6. *ds

                        df(3   ,j,k)=(+ 1.*f(1   ,j,k) -  8.*f(2   ,j,k)             &
                                      + 8.*f(4   ,j,k) -  1.*f(5   ,j,k)             &
                                     )/12.*ds

                        df(4   ,j,k)=(- 1.*f(1   ,j,k) +  9.*f(2   ,j,k)             &
                                      -45.*f(3   ,j,k) + 45.*f(5   ,j,k)             &
                                      - 9.*f(6   ,j,k) +  1.*f(7   ,j,k)             &
                                     )/60.*ds
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-3,j,k)=(+ 1.*f(mx  ,j,k) -  9.*f(mx-1,j,k)             &
                                      +45.*f(mx-2,j,k) - 45.*f(mx-4,j,k)             &
                                      + 9.*f(mx-5,j,k) -  1.*f(mx-6,j,k)             &
                                     )/60.*ds

                        df(mx-2,j,k)=(- 1.*f(mx  ,j,k) +  8.*f(mx-1,j,k)             &
                                      - 8.*f(mx-3,j,k) +  1.*f(mx-4,j,k)             &
                                     )/12.*ds

                        df(mx-1,j,k)=(+ 2.*f(mx  ,j,k) +  3.*f(mx-1,j,k)             &
                                      - 6.*f(mx-2,j,k) +  1.*f(mx-3,j,k)             &
                                     )/6. *ds

                        df(mx  ,j,k)=(+11.*f(mx  ,j,k) - 18.*f(mx-1,j,k)             &
                                      + 9.*f(mx-2,j,k) -  2.*f(mx-3,j,k)             &
                                     )/6. *ds
                     end do
                  end do
               end if

            elseif (( isym_x .eq. 0 ) .and. ( periodic_x .eq. 1 )) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(   1,j,k) = ae *( f(2   ,j,k) - neg_f(4,j,k))             &
                                     + be *( f(3   ,j,k) - neg_f(3,j,k))             &
                                     + ce *( f(4   ,j,k) - neg_f(2,j,k))             &
                                     + de *( f(5   ,j,k) - neg_f(1,j,k))

                        df(   2,j,k) = ae *( f(3   ,j,k) -     f(1,j,k))             &
                                     + be *( f(4   ,j,k) - neg_f(4,j,k))             &
                                     + ce *( f(5   ,j,k) - neg_f(3,j,k))             &
                                     + de *( f(6   ,j,k) - neg_f(2,j,k))

                        df(   3,j,k) = ae *( f(4   ,j,k) -     f(2,j,k))             &
                                     + be *( f(5   ,j,k) -     f(1,j,k))             &
                                     + ce *( f(6   ,j,k) - neg_f(4,j,k))             &
                                     + de *( f(7   ,j,k) - neg_f(3,j,k))

                        df(   4,j,k) = ae *( f(5   ,j,k) -     f(3,j,k))             &
                                     + be *( f(6   ,j,k) -     f(2,j,k))             &
                                     + ce *( f(7   ,j,k) -     f(1,j,k))             &
                                     + de *( f(8   ,j,k) - neg_f(4,j,k))
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-3,j,k) = ae *(    f(mx-2,j,k)-f(mx-4,j,k))             &
                                     + be *(    f(mx-1,j,k)-f(mx-5,j,k))             &
                                     + ce *(    f(mx  ,j,k)-f(mx-6,j,k))             &
                                     + de *(pos_f(1   ,j,k)-f(mx-7,j,k))

                        df(mx-2,j,k) = ae *(    f(mx-1,j,k)-f(mx-3,j,k))             &
                                     + be *(    f(mx  ,j,k)-f(mx-4,j,k))             &
                                     + ce *(pos_f(1   ,j,k)-f(mx-5,j,k))             &
                                     + de *(pos_f(2   ,j,k)-f(mx-6,j,k))

                        df(mx-1,j,k) = ae *(    f(mx  ,j,k)-f(mx-2,j,k))             &
                                     + be *(pos_f(1   ,j,k)-f(mx-3,j,k))             &
                                     + ce *(pos_f(2   ,j,k)-f(mx-4,j,k))             &
                                     + de *(pos_f(3   ,j,k)-f(mx-5,j,k))

                        df(mx  ,j,k) = ae *(pos_f(1   ,j,k)-f(mx-1,j,k))             &
                                     + be *(pos_f(2   ,j,k)-f(mx-2,j,k))             &
                                     + ce *(pos_f(3   ,j,k)-f(mx-3,j,k))             &
                                     + de *(pos_f(4   ,j,k)-f(mx-4,j,k))
                     end do
                  end do
               end if

            elseif ( isym_x .eq. 1 ) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(1,j,k) = ae*( f(2,j,k) - n_sym*f(2   ,j,k) )              &
                                  + be*( f(3,j,k) - n_sym*f(3   ,j,k) )              &
                                  + ce*( f(4,j,k) - n_sym*f(4   ,j,k) )              &
                                  + de*( f(5,j,k) - n_sym*f(5   ,j,k) )

                        df(2,j,k) = ae*( f(3,j,k) -       f(1   ,j,k) )              &
                                  + be*( f(4,j,k) - n_sym*f(2   ,j,k) )              &
                                  + ce*( f(5,j,k) - n_sym*f(3   ,j,k) )              &
                                  + de*( f(6,j,k) - n_sym*f(4   ,j,k) )

                        df(3,j,k) = ae*( f(4,j,k) -       f(2   ,j,k) )              &
                                  + be*( f(5,j,k) -       f(1   ,j,k) )              &
                                  + ce*( f(6,j,k) - n_sym*f(2   ,j,k) )              &
                                  + de*( f(7,j,k) - n_sym*f(3   ,j,k) )

                        df(4,j,k) = ae*( f(5,j,k) -       f(3   ,j,k) )              &
                                  + be*( f(6,j,k) -       f(2   ,j,k) )              &
                                  + ce*( f(7,j,k) -       f(1   ,j,k) )              &
                                  + de*( f(8,j,k) - n_sym*f(2   ,j,k) )
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-3,j,k)=(+ 1.*f(mx  ,j,k) -  9.*f(mx-1,j,k)             &
                                      +45.*f(mx-2,j,k) - 45.*f(mx-4,j,k)             &
                                      + 9.*f(mx-5,j,k) -  1.*f(mx-6,j,k)             &
                                      )/60.*ds

                        df(mx-2,j,k)=(- 1.*f(mx  ,j,k) +  8.*f(mx-1,j,k)             &
                                      - 8.*f(mx-3,j,k) +  1.*f(mx-4,j,k)             &
                                      )/12.*ds

                        df(mx-1,j,k)=(+ 2.*f(mx  ,j,k) +  3.*f(mx-1,j,k)             &
                                      - 6.*f(mx-2,j,k) +  1.*f(mx-3,j,k)             &
                                      )/6. *ds

                        df(mx  ,j,k)=(+11.*f(mx  ,j,k) - 18.*f(mx-1,j,k)             &
                                      + 9.*f(mx-2,j,k) -  2.*f(mx-3,j,k)             &
                                      )/6. *ds
                     end do
                  end do
               end if
            end if
         else
            if ( myid.eq.0 ) then
               write(6,2) ibound
            end if
            call terminate_run(6,0)
         end if

         if(unif_grid_x.ne.1) then
           do i = 1, 4
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
           do i = mx-3, mx
             df(i,:,:) = scale_1(i)*df(i,:,:)
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
  if( lnbr(1) >= 0 )then
     call MPI_Wait(req(2),stat(:,2),ierr)
  endif 
  if( lnbr(2) >= 0 ) then
     call MPI_Wait(req(4),stat(:,4),ierr)
  endif 
#endif

  return
  end subroutine derivative_x_calc

!=========================================================================================
  subroutine derivative_x_calc_buff(mx,my,mz,f,df,scale_1,n_sym, idx, label)
!=========================================================================================
! evaluates the first derivative in x-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!  ae     - Explicit central difference stencil coefficient at (i +/- 1)
!  be     - Explicit central difference stencil coefficient at (i +/- 2)
!  ce     - Explicit central difference stencil coefficient at (i +/- 3)
!  de     - Explicit central difference stencil coefficient at (i +/- 4)
!  mx     - number of grid points in x-direction
!  my     - number of grid points in y-direction
!  mz     - number of grid points in z-direction
!
!       d()/dx = [ d(eta)/dx ]*[ d()/d(eta) ]
!        [ dx/d(eta) ]^-1*[ d()/d(eta) ]
!
!  scale_1- Proportional to ( dx/d(eta) )^-1 where eta is fictitious
!           uniform grid and x is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along x-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_x
  use param_m, only : periodic_x, vary_in_x, iorder
  use derivative_m, only : isym_x
  use derivative_m, only : ibound

  use derivative_m
  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(mx      ) :: scale_1

  integer, intent(in) :: n_sym
  integer, intent(in) :: idx

  integer :: i, j, k
  integer, dimension(6) :: lnbr(6)
  integer, dimension(MPI_Status_size,4) :: stat

  real :: ds,ae,be,ce,de

  character(*), intent(in) :: label
!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_x .eq. 0 ) then
     df = 0.0
     return
  endif

  ! check for misuse
#ifdef DERIV_CHECK_LABELS
  if( trim(deriv_x_list(idx)%fldname) .ne. trim(label) ) then
      write(*,'2a,i5') 'Mismatch name in calc: ', trim(label), idx
      call print_deriv_list(6)
  endif
#endif
!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_x.ne.1) then
    ds = real(mx*xpes-1)
  else
    ds = real(mx*xpes-1) * scale_1(1)
  endif

  lnbr = neighbor

  if(periodic_x.eq.1) then
    if(xid.eq.0) then
       lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
    endif
    if(xid.eq.xpes-1)then
       lnbr(2) = zid*xpes*ypes+ yid*xpes+0
    endif
  endif

!-----------------------------------------------------------------------------------------
!  6th order explicit: (3,3,4-6E-4,3,3)
!
      if ( iorder .eq. 6 ) then

         ae =  3./ 4. *ds
         be = -3./20. *ds
         ce =  1./60. *ds
!
!  Internal nodes:
!
         do k = 1, mz
            do j = 1, my
               do i = 4, mx-3
                  df(i,j,k) = ae *( f(i+1,j,k)-f(i-1,j,k) )                          &
                            + be *( f(i+2,j,k)-f(i-2,j,k) )                          &
                            + ce *( f(i+3,j,k)-f(i-3,j,k) )
               end do
            end do
         end do

         if(unif_grid_x.ne.1) then
           do i = 4, mx-3
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
         endif
#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( deriv_x_list(idx)%neg_nbr >= 0 )then
            call MPI_Wait(deriv_x_list(idx)%req(1),stat(:,1),ierr)
         endif 
#endif
!
!  Internal nodes: edges
!
         if (neighbor(1).ge.0) then
            do k = 1, mz
               do j = 1, my
                  df(1,j,k) = ae *( f(2,j,k)-neg_f_x_buf(3,j,k, idx) )                          &
                            + be *( f(3,j,k)-neg_f_x_buf(2,j,k, idx) )                          &
                            + ce *( f(4,j,k)-neg_f_x_buf(1,j,k, idx) )
                  df(2,j,k) = ae *( f(3,j,k)-    f(1,j,k) )                          &
                            + be *( f(4,j,k)-neg_f_x_buf(3,j,k, idx) )                          &
                            + ce *( f(5,j,k)-neg_f_x_buf(2,j,k, idx) )
                  df(3,j,k) = ae *( f(4,j,k)-    f(2,j,k) )                          &
                            + be *( f(5,j,k)-    f(1,j,k) )                          &
                            + ce *( f(6,j,k)-neg_f_x_buf(3,j,k, idx) )
               end do
            end do
         endif

#ifndef COARRAYCOMMUNICATION
         if( deriv_x_list(idx)%pos_nbr >= 0 )then
            call MPI_Wait(deriv_x_list(idx)%req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(2).ge.0) then
            do k = 1, mz
               do j = 1, my
                  df(mx,j,k)   = ae *( pos_f_x_buf(1,j,k,idx)   -f(mx-1,j,k) )                 &
                               + be *( pos_f_x_buf(2,j,k,idx)   -f(mx-2,j,k) )                 &
                               + ce *( pos_f_x_buf(3,j,k,idx)   -f(mx-3,j,k) )
                  df(mx-1,j,k) = ae *(     f(mx,j,k)  -f(mx-2,j,k) )                 &
                               + be *( pos_f_x_buf(1,j,k,idx)   -f(mx-3,j,k) )                 &
                               + ce *( pos_f_x_buf(2,j,k,idx)   -f(mx-4,j,k) )
                  df(mx-2,j,k) = ae *(     f(mx-1,j,k)-f(mx-3,j,k) )                 &
                               + be *(     f(mx,j,k)  -f(mx-4,j,k) )                 &
                               + ce *( pos_f_x_buf(1,j,k,idx)   -f(mx-5,j,k) )
               end do
            end do
         endif

!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_x .eq. 0 ) .and. ( periodic_x .eq. 0 )) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(1,j,k) = (-11.*f(1,j,k) + 18.*f(2,j,k)                    &
                                     - 9.*f(3,j,k) +  2.*f(4,j,k))/6.                &
                                     * ds

                        df(2,j,k) = (- 2.*f(1,j,k) -  3.*f(2,j,k)                    &
                                     + 6.*f(3,j,k) -  1.*f(4,j,k))/6.                &
                                     * ds

                        df(3,j,k) = (+ 1.*f(1,j,k) -  8.*f(2,j,k)                    &
                                     + 8.*f(4,j,k) -  1.*f(5,j,k))/12.               &
                                     * ds
                     enddo
                  enddo
               endif
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-2,j,k) = (- 1.*f(mx  ,j,k)+ 8.*f(mx-1,j,k)             &
                                        - 8.*f(mx-3,j,k)+ 1.*f(mx-4,j,k)             &
                                       )/12.*ds

                        df(mx-1,j,k) = (+ 2.*f(mx  ,j,k)+ 3.*f(mx-1,j,k)             &
                                        - 6.*f(mx-2,j,k)+ 1.*f(mx-3,j,k)             &
                                       )/6. *ds

                        df(mx  ,j,k) = (+11.*f(mx  ,j,k)-18.*f(mx-1,j,k)             &
                                        + 9.*f(mx-2,j,k)- 2.*f(mx-3,j,k)             &
                                        )/6. *ds

                     end do
                  end do
               endif

            elseif (( isym_x .eq. 0 ) .and. ( periodic_x .eq. 1 )) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(   1,j,k) = ae *( f(2,j,k) - neg_f_x_buf(3,j,k,idx) )               &
                                     + be *( f(3,j,k) - neg_f_x_buf(2,j,k,idx) )               &
                                     + ce *( f(4,j,k) - neg_f_x_buf(1,j,k,idx) )

                        df(   2,j,k) = ae *( f(3,j,k) -     f(1,j,k) )               &
                                     + be *( f(4,j,k) - neg_f_x_buf(3,j,k,idx) )               &
                                     + ce *( f(5,j,k) - neg_f_x_buf(2,j,k,idx) )

                        df(   3,j,k) = ae *( f(4,j,k) -     f(2,j,k) )               &
                                     + be *( f(5,j,k) -     f(1,j,k) )               &
                                     + ce *( f(6,j,k) - neg_f_x_buf(3,j,k,idx) )
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-2,j,k) =ae *(     f(mx-1,j,k)-f(mx-3,j,k))             &
                                     +be *(     f(mx  ,j,k)-f(mx-4,j,k))             &
                                     +ce *( pos_f_x_buf(1   ,j,k,idx)-f(mx-5,j,k))

                        df(mx-1,j,k) =ae *(     f(mx  ,j,k)-f(mx-2,j,k))             &
                                     +be *( pos_f_x_buf(1   ,j,k,idx)-f(mx-3,j,k))             &
                                     +ce *( pos_f_x_buf(2   ,j,k,idx)-f(mx-4,j,k))

                        df(mx  ,j,k) =ae *( pos_f_x_buf(1   ,j,k,idx)-f(mx-1,j,k))             &
                                     +be *( pos_f_x_buf(2   ,j,k,idx)-f(mx-2,j,k))             &
                                     +ce *( pos_f_x_buf(3   ,j,k,idx)-f(mx-3,j,k))

                     end do
                  end do
               end if

            elseif ( isym_x .eq. 1 ) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(1,j,k) = ae*( f(2   ,j,k)-n_sym*f(2   ,j,k) )             &
                                  + be*( f(3   ,j,k)-n_sym*f(3   ,j,k) )             &
                                  + ce*( f(4   ,j,k)-n_sym*f(4   ,j,k) )

                        df(2,j,k) = ae*( f(3   ,j,k)-      f(1   ,j,k) )             &
                                  + be*( f(4   ,j,k)-n_sym*f(2   ,j,k) )             &
                                  + ce*( f(5   ,j,k)-n_sym*f(3   ,j,k) )

                        df(3,j,k) = ae*( f(4   ,j,k)-      f(2   ,j,k) )             &
                                  + be*( f(5   ,j,k)-      f(1   ,j,k) )             &
                                  + ce*( f(6   ,j,k)-n_sym*f(2   ,j,k) )
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx  ,j,k)=(+11.*f(mx  ,j,k)-18.*f(mx-1,j,k)               &
                                      + 9.*f(mx-2,j,k)- 2.*f(mx-3,j,k))              &
                                      /6. * ds

                        df(mx-1,j,k)=(+ 2.*f(mx  ,j,k)+ 3.*f(mx-1,j,k)               &
                                      - 6.*f(mx-2,j,k)+ 1.*f(mx-3,j,k))              &
                                      /6. * ds

                        df(mx-2,j,k)=(- 1.*f(mx  ,j,k)+ 8.*f(mx-1,j,k)               &
                                      - 8.*f(mx-3,j,k)+ 1.*f(mx-4,j,k))              &
                                      /12.* ds
                     end do
                  end do
               end if

            end if
         else                   ! Unsupported IBOUND
            if ( myid.eq.0 ) then
               write(6,2) ibound
2              format('IBOUND = ', i4,' is not supported')
            end if
            call terminate_run(6,0)
         end if         ! if ( ibound ... )

         if(unif_grid_x.ne.1) then
           do i = 1, 3
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
           do i = mx-2, mx
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
         endif
!
!  8th order: (3,3,4,6-8E-6,4,3,3)
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
            do j = 1, my
               do i = 5, mx-4
                  df(i,j,k) = ae *( f(i+1,j,k)-f(i-1,j,k) )                          &
                            + be *( f(i+2,j,k)-f(i-2,j,k) )                          &
                            + ce *( f(i+3,j,k)-f(i-3,j,k) )                          &
                            + de *( f(i+4,j,k)-f(i-4,j,k) )
               end do
            end do
         end do

         if(unif_grid_x.ne.1) then
           do i = 5, mx-4
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( deriv_x_list(idx)%neg_nbr >= 0 )then
            call MPI_Wait(deriv_x_list(idx)%req(1),stat(:,1),ierr)
         endif 
#endif

!
!  Internal nodes: edges
!
         if (neighbor(1).ge.0) then
            do k = 1, mz
               do j = 1, my
                  df(1,j,k) = ae *( f(2,j,k)-neg_f_x_buf(4,j,k,idx) )                          &
                            + be *( f(3,j,k)-neg_f_x_buf(3,j,k,idx) )                          &
                            + ce *( f(4,j,k)-neg_f_x_buf(2,j,k,idx) )                          &
                            + de *( f(5,j,k)-neg_f_x_buf(1,j,k,idx) )
                  df(2,j,k) = ae *( f(3,j,k)-    f(1,j,k) )                          &
                            + be *( f(4,j,k)-neg_f_x_buf(4,j,k,idx) )                          &
                            + ce *( f(5,j,k)-neg_f_x_buf(3,j,k,idx) )                          &
                            + de *( f(6,j,k)-neg_f_x_buf(2,j,k,idx) )
                  df(3,j,k) = ae *( f(4,j,k)-    f(2,j,k) )                          &
                            + be *( f(5,j,k)-    f(1,j,k) )                          &
                            + ce *( f(6,j,k)-neg_f_x_buf(4,j,k,idx) )                          &
                            + de *( f(7,j,k)-neg_f_x_buf(3,j,k,idx) )
                  df(4,j,k) = ae *( f(5,j,k)-    f(3,j,k) )                          &
                            + be *( f(6,j,k)-    f(2,j,k) )                          &
                            + ce *( f(7,j,k)-    f(1,j,k) )                          &
                            + de *( f(8,j,k)-neg_f_x_buf(4,j,k,idx) )
               end do
            end do
         end if

#ifndef COARRAYCOMMUNICATION
         if( deriv_x_list(idx)%pos_nbr >= 0 )then
            call MPI_Wait(deriv_x_list(idx)%req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(2).ge.0) then
            do k = 1, mz
               do j = 1, my
                  df(mx,  j,k) = ae *( pos_f_x_buf(1,j,k,idx)   -f(mx-1,j,k) )                 &
                               + be *( pos_f_x_buf(2,j,k,idx)   -f(mx-2,j,k) )                 &
                               + ce *( pos_f_x_buf(3,j,k,idx)   -f(mx-3,j,k) )                 &
                               + de *( pos_f_x_buf(4,j,k,idx)   -f(mx-4,j,k) )
                  df(mx-1,j,k) = ae *(     f(mx,j,k)  -f(mx-2,j,k) )                 &
                               + be *( pos_f_x_buf(1,j,k,idx)   -f(mx-3,j,k) )                 &
                               + ce *( pos_f_x_buf(2,j,k,idx)   -f(mx-4,j,k) )                 &
                               + de *( pos_f_x_buf(3,j,k,idx)   -f(mx-5,j,k) )
                  df(mx-2,j,k) = ae *(     f(mx-1,j,k)-f(mx-3,j,k) )                 &
                               + be *(     f(mx,j,k)  -f(mx-4,j,k) )                 &
                               + ce *( pos_f_x_buf(1,j,k,idx)   -f(mx-5,j,k) )                 &
                               + de *( pos_f_x_buf(2,j,k,idx)   -f(mx-6,j,k) )
                  df(mx-3,j,k) = ae *(     f(mx-2,j,k)-f(mx-4,j,k) )                 &
                               + be *(     f(mx-1,j,k)-f(mx-5,j,k) )                 &
                               + ce *(     f(mx,j,k)  -f(mx-6,j,k) )                 &
                               + de *( pos_f_x_buf(1,j,k,idx)   -f(mx-7,j,k) )
               end do
            end do
         end if
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_x .eq. 0 ) .and. ( periodic_x .eq. 0 )) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(1   ,j,k)=(-11.*f(1   ,j,k) + 18.*f(2   ,j,k)             &
                                      - 9.*f(3   ,j,k) +  2.*f(4   ,j,k)             &
                                     )/6. *ds

                        df(2   ,j,k)=(- 2.*f(1   ,j,k) -  3.*f(2   ,j,k)             &
                                      + 6.*f(3   ,j,k) -  1.*f(4   ,j,k)             &
                                     )/6. *ds

                        df(3   ,j,k)=(+ 1.*f(1   ,j,k) -  8.*f(2   ,j,k)             &
                                      + 8.*f(4   ,j,k) -  1.*f(5   ,j,k)             &
                                     )/12.*ds

                        df(4   ,j,k)=(- 1.*f(1   ,j,k) +  9.*f(2   ,j,k)             &
                                      -45.*f(3   ,j,k) + 45.*f(5   ,j,k)             &
                                      - 9.*f(6   ,j,k) +  1.*f(7   ,j,k)             &
                                     )/60.*ds
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-3,j,k)=(+ 1.*f(mx  ,j,k) -  9.*f(mx-1,j,k)             &
                                      +45.*f(mx-2,j,k) - 45.*f(mx-4,j,k)             &
                                      + 9.*f(mx-5,j,k) -  1.*f(mx-6,j,k)             &
                                     )/60.*ds

                        df(mx-2,j,k)=(- 1.*f(mx  ,j,k) +  8.*f(mx-1,j,k)             &
                                      - 8.*f(mx-3,j,k) +  1.*f(mx-4,j,k)             &
                                     )/12.*ds

                        df(mx-1,j,k)=(+ 2.*f(mx  ,j,k) +  3.*f(mx-1,j,k)             &
                                      - 6.*f(mx-2,j,k) +  1.*f(mx-3,j,k)             &
                                     )/6. *ds

                        df(mx  ,j,k)=(+11.*f(mx  ,j,k) - 18.*f(mx-1,j,k)             &
                                      + 9.*f(mx-2,j,k) -  2.*f(mx-3,j,k)             &
                                     )/6. *ds
                     end do
                  end do
               end if

            elseif (( isym_x .eq. 0 ) .and. ( periodic_x .eq. 1 )) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(   1,j,k) = ae *( f(2   ,j,k) - neg_f_x_buf(4,j,k,idx))             &
                                     + be *( f(3   ,j,k) - neg_f_x_buf(3,j,k,idx))             &
                                     + ce *( f(4   ,j,k) - neg_f_x_buf(2,j,k,idx))             &
                                     + de *( f(5   ,j,k) - neg_f_x_buf(1,j,k,idx))

                        df(   2,j,k) = ae *( f(3   ,j,k) -     f(1,j,k))             &
                                     + be *( f(4   ,j,k) - neg_f_x_buf(4,j,k,idx))             &
                                     + ce *( f(5   ,j,k) - neg_f_x_buf(3,j,k,idx))             &
                                     + de *( f(6   ,j,k) - neg_f_x_buf(2,j,k,idx))

                        df(   3,j,k) = ae *( f(4   ,j,k) -     f(2,j,k))             &
                                     + be *( f(5   ,j,k) -     f(1,j,k))             &
                                     + ce *( f(6   ,j,k) - neg_f_x_buf(4,j,k,idx))             &
                                     + de *( f(7   ,j,k) - neg_f_x_buf(3,j,k,idx))

                        df(   4,j,k) = ae *( f(5   ,j,k) -     f(3,j,k))             &
                                     + be *( f(6   ,j,k) -     f(2,j,k))             &
                                     + ce *( f(7   ,j,k) -     f(1,j,k))             &
                                     + de *( f(8   ,j,k) - neg_f_x_buf(4,j,k,idx))
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-3,j,k) = ae *(    f(mx-2,j,k)-f(mx-4,j,k))             &
                                     + be *(    f(mx-1,j,k)-f(mx-5,j,k))             &
                                     + ce *(    f(mx  ,j,k)-f(mx-6,j,k))             &
                                     + de *(pos_f_x_buf(1   ,j,k,idx)-f(mx-7,j,k))

                        df(mx-2,j,k) = ae *(    f(mx-1,j,k)-f(mx-3,j,k))             &
                                     + be *(    f(mx  ,j,k)-f(mx-4,j,k))             &
                                     + ce *(pos_f_x_buf(1   ,j,k,idx)-f(mx-5,j,k))             &
                                     + de *(pos_f_x_buf(2   ,j,k,idx)-f(mx-6,j,k))

                        df(mx-1,j,k) = ae *(    f(mx  ,j,k)-f(mx-2,j,k))             &
                                     + be *(pos_f_x_buf(1   ,j,k,idx)-f(mx-3,j,k))             &
                                     + ce *(pos_f_x_buf(2   ,j,k,idx)-f(mx-4,j,k))             &
                                     + de *(pos_f_x_buf(3   ,j,k,idx)-f(mx-5,j,k))

                        df(mx  ,j,k) = ae *(pos_f_x_buf(1   ,j,k,idx)-f(mx-1,j,k))             &
                                     + be *(pos_f_x_buf(2   ,j,k,idx)-f(mx-2,j,k))             &
                                     + ce *(pos_f_x_buf(3   ,j,k,idx)-f(mx-3,j,k))             &
                                     + de *(pos_f_x_buf(4   ,j,k,idx)-f(mx-4,j,k))
                     end do
                  end do
               end if

            elseif ( isym_x .eq. 1 ) then

               if (neighbor(1).lt.0) then
                  do k = 1, mz
                     do j = 1, my

                        df(1,j,k) = ae*( f(2,j,k) - n_sym*f(2   ,j,k) )              &
                                  + be*( f(3,j,k) - n_sym*f(3   ,j,k) )              &
                                  + ce*( f(4,j,k) - n_sym*f(4   ,j,k) )              &
                                  + de*( f(5,j,k) - n_sym*f(5   ,j,k) )

                        df(2,j,k) = ae*( f(3,j,k) -       f(1   ,j,k) )              &
                                  + be*( f(4,j,k) - n_sym*f(2   ,j,k) )              &
                                  + ce*( f(5,j,k) - n_sym*f(3   ,j,k) )              &
                                  + de*( f(6,j,k) - n_sym*f(4   ,j,k) )

                        df(3,j,k) = ae*( f(4,j,k) -       f(2   ,j,k) )              &
                                  + be*( f(5,j,k) -       f(1   ,j,k) )              &
                                  + ce*( f(6,j,k) - n_sym*f(2   ,j,k) )              &
                                  + de*( f(7,j,k) - n_sym*f(3   ,j,k) )

                        df(4,j,k) = ae*( f(5,j,k) -       f(3   ,j,k) )              &
                                  + be*( f(6,j,k) -       f(2   ,j,k) )              &
                                  + ce*( f(7,j,k) -       f(1   ,j,k) )              &
                                  + de*( f(8,j,k) - n_sym*f(2   ,j,k) )
                     end do
                  end do
               end if
               if (neighbor(2).lt.0) then
                  do k = 1, mz
                     do j = 1, my
                        df(mx-3,j,k)=(+ 1.*f(mx  ,j,k) -  9.*f(mx-1,j,k)             &
                                      +45.*f(mx-2,j,k) - 45.*f(mx-4,j,k)             &
                                      + 9.*f(mx-5,j,k) -  1.*f(mx-6,j,k)             &
                                      )/60.*ds

                        df(mx-2,j,k)=(- 1.*f(mx  ,j,k) +  8.*f(mx-1,j,k)             &
                                      - 8.*f(mx-3,j,k) +  1.*f(mx-4,j,k)             &
                                      )/12.*ds

                        df(mx-1,j,k)=(+ 2.*f(mx  ,j,k) +  3.*f(mx-1,j,k)             &
                                      - 6.*f(mx-2,j,k) +  1.*f(mx-3,j,k)             &
                                      )/6. *ds

                        df(mx  ,j,k)=(+11.*f(mx  ,j,k) - 18.*f(mx-1,j,k)             &
                                      + 9.*f(mx-2,j,k) -  2.*f(mx-3,j,k)             &
                                      )/6. *ds
                     end do
                  end do
               end if
            end if
         else
            if ( myid.eq.0 ) then
               write(6,2) ibound
            end if
            call terminate_run(6,0)
         end if

         if(unif_grid_x.ne.1) then
           do i = 1, 4
             df(i,:,:) = scale_1(i)*df(i,:,:)
           end do
           do i = mx-3, mx
             df(i,:,:) = scale_1(i)*df(i,:,:)
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
  if( deriv_x_list(idx)%neg_nbr >= 0 )then
     call MPI_Wait(deriv_x_list(idx)%req(2),stat(:,2),ierr)
  endif 
  if( deriv_x_list(idx)%pos_nbr >= 0 ) then
     call MPI_Wait(deriv_x_list(idx)%req(4),stat(:,4),ierr)
  endif 

  deriv_x_list(idx)%inuse = .false.
#endif

  return
  end subroutine derivative_x_calc_buff

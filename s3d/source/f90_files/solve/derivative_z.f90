#include "globalDefines.h"
! REVISIONS
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
  subroutine derivative_z(mx,my,mz,f,df,scale_1,n_sym)
!=========================================================================================
! evaluates the first derivative in z-direction using explicit differencing
! for further details of arguments, see description in header of derivative_z_calc
!-----------------------------------------------------------------------------------------
  implicit none

  integer, intent(in) :: mx, my, mz
  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(      mz) :: scale_1
  integer, intent(in) :: n_sym

  integer, dimension(4) :: req

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_z_comm(mx,my,mz,f,req)

#ifdef COARRAYCOMMUNICATION
  call sync_all()
#endif

  call derivative_z_calc(mx,my,mz,f,df,scale_1,n_sym,req)

  return
  end subroutine derivative_z

!=========================================================================================
  subroutine derivative_z_twopart(mx,my,mz,f,df,scale_1,n_sym,idx,label)
!=========================================================================================
! evaluates the first derivative in z-direction using explicit differencing
! for further details of arguments, see description in header of derivative_z_calc
!-----------------------------------------------------------------------------------------
  implicit none

  integer, intent(in) :: mx, my, mz
  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(      mz) :: scale_1
  integer, intent(in) :: n_sym

  character(*), intent(in) :: label
  integer, intent(inout) :: idx

#ifdef COARRAYCOMMUNICATION
  write(*,*) 'Coarrays not implemented for twopart derivatives'
  write(*,*) 'use derivative_z instead of derivative_z_twopart'
  stop
#endif

  call derivative_z_post(mx,my,mz,f,idx,label)
  call derivative_z_send(mx,my,mz,f,idx,label)

  call derivative_z_calc_buff(mx,my,mz,f,df,scale_1,n_sym,idx)

  return
  end subroutine derivative_z_twopart
!=========================================================================================
  subroutine derivative_z_comm(mx,my,mz,f,req)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_z, vary_in_z, iorder
  use derivative_m, only : neg_f => neg_f_z
  use derivative_m, only : pos_f => pos_f_z

  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  integer, dimension(4), intent(out) :: req

  integer :: nm
  integer, dimension(6) :: lnbr(6)

!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_z .eq. 0 ) then
     return
  endif
!-----------------------------------------------------------------------------------------
! zero ghost cell arrays

  neg_f=0.0
  pos_f=0.0
!-----------------------------------------------------------------------------------------
! get neighboring cells data

  lnbr = neighbor

  if(periodic_z.eq.1) then
    if(zid.eq.0) then
      lnbr(5) = (zpes-1)*xpes*ypes+yid*xpes+xid
    endif
    if(zid.eq.zpes-1)then
      lnbr(6) = 0*xpes*ypes   +yid*xpes+xid
    endif
  endif

  req = MPI_REQUEST_NULL

#ifdef COARRAYCOMMUNICATION
  if(lnbr(5)>=0) then
     neg_f(:,:,:)[lnbr(5)] = f(:,:,1:iorder/2)
  endif
  if(lnbr(6)>=0) then
     nm = mz + 1 - iorder/2
     pos_f(:,:,:)[lnbr(6)] = f(:,:,nm:mz)
  endif
#else
  if(lnbr(5)>=0) then
   ! get ghost cells from neighbor on (-y) side
     call MPI_IRecv(neg_f,(mx*my*iorder/2),MPI_REAL8,lnbr(5),1,gcomm,req(1),ierr)
   ! send ghost cells to neighbor on (-y) side
!     call MPI_ISend(f(1,1,1),1,zrows_type,lnbr(5),2,gcomm,req(2),ierr)
!    Evatt Hawkes workaround for infiniband
     call MPI_ISend(f(1,1,1),(mx*my*iorder/2),MPI_REAL8,lnbr(5),2,gcomm,req(2),ierr)
  endif
  
  if(lnbr(6)>=0) then
   ! get ghost cells from neighbor on (+y) side
     call MPI_IRecv(pos_f,(mx*my*iorder/2),MPI_REAL8,lnbr(6),2,gcomm,req(3),ierr)
   ! send ghost cells to neighbor on (+y) side
     nm = mz + 1 - iorder/2
!     call MPI_ISend(f(1,1,nm),1,zrows_type,lnbr(6),1,gcomm,req(4),ierr)
!    Evatt Hawkes workaround for infiniband
     call MPI_ISend(f(1,1,nm),(mx*my*iorder/2),MPI_REAL8,lnbr(6),1,gcomm,req(4),ierr)
  endif
#endif

  return
  end subroutine derivative_z_comm

!=========================================================================================
  subroutine derivative_z_post(mx,my,mz,f,idx,label)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_z, vary_in_z, iorder

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

  if( vary_in_z .eq. 0 ) then
     return
  endif
!-----------------------------------------------------------------------------------------
! Find available index, or use supplied index
  if(  deriv_z_list(idx)%inuse ) then
      write(*,'(1a, 1i3, 1a, 1i5)') 'Error, index', idx, ' is in use for z-direction', myid
      call print_deriv_list(6)
      stop
  endif

  deriv_z_list(idx)%inuse = .true.
  deriv_z_list(idx)%fldname = trim(label)
!-----------------------------------------------------------------------------------------
! zero recv ghost cell arrays for this position

  neg_f_z_buf(:,:,:,idx) = 0.0
  pos_f_z_buf(:,:,:,idx) = 0.0
!-----------------------------------------------------------------------------------------
! get neighboring cells data

  lnbr = neighbor

  if(periodic_z.eq.1) then
    if(zid.eq.0) then
      lnbr(5) = (zpes-1)*xpes*ypes+yid*xpes+xid
    endif
    if(zid.eq.zpes-1)then
      lnbr(6) = 0*xpes*ypes   +yid*xpes+xid
    endif
  endif

  deriv_z_list(idx)%neg_nbr = lnbr(5)
  deriv_z_list(idx)%pos_nbr = lnbr(6)

  deriv_z_list(idx)%req = MPI_REQUEST_NULL

  if(lnbr(5)>=0) then
   ! get ghost cells from neighbor on (-y) side
     call MPI_IRecv(neg_f_z_buf(:,:,:,idx),(mx*my*iorder/2),&
                   MPI_REAL8,deriv_z_list(idx)%neg_nbr,idx,&
                   gcomm,deriv_z_list(idx)%req(1),ierr)
  endif
  
  if(lnbr(6)>=0) then
   ! get ghost cells from neighbor on (+y) side
     call MPI_IRecv(pos_f_z_buf(:,:,:,idx),(mx*my*iorder/2),&
                    MPI_REAL8,deriv_z_list(idx)%pos_nbr,deriv_list_size + idx,&
                    gcomm,deriv_z_list(idx)%req(3),ierr)
  endif

  return
  end subroutine derivative_z_post
!=========================================================================================
  subroutine derivative_z_send(mx,my,mz,f,idx,label)
!=========================================================================================
  use topology_m
  use param_m, only : periodic_z, vary_in_z, iorder

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

  if( vary_in_z .eq. 0 ) then
     return
  endif

#ifdef DERIV_CHECK_LABELS
  ! check for misuse
  if( trim(deriv_z_list(idx)%fldname) .ne.  trim(label) ) then
      write(*,'(2a,i5)') 'Mismatch name: ', trim(label), idx
      call print_deriv_list(6)
  endif
#endif

  if(deriv_z_list(idx)%neg_nbr>=0) then
   ! get ghost cells from neighbor on (-z) side
     !pos_fs_z_buf(:,:,:,idx) = f(:,:,1:iorder/2) ! this adds an extra memcpy
     !                                            ! that we didn't have before
     !call MPI_ISend(pos_fs_z_buf(1,1,1,idx),(mx*my*iorder/2),&
     !               MPI_REAL8,deriv_z_list(idx)%neg_nbr,2, &
     !               gcomm,deriv_z_list(idx)%req(2),ierr)

      ! there's no reason to do copy the send data to the buffer here...
      ! although computing a checksum would be safer
     call MPI_ISend(f(1,1,1),(mx*my*iorder/2),&
                    MPI_REAL8,deriv_z_list(idx)%neg_nbr,deriv_list_size + idx, &
                    gcomm,deriv_z_list(idx)%req(2),ierr)
  endif
  
  if(deriv_z_list(idx)%pos_nbr>=0) then
   ! send ghost cells to neighbor on (+z) side
     nm = mz + 1 - iorder/2
     call MPI_ISend(f(1,1,nm),(mx*my*iorder/2), &
                    MPI_REAL8,deriv_z_list(idx)%pos_nbr,idx, &
                    gcomm,deriv_z_list(idx)%req(4),ierr)
  endif

  return
  end subroutine derivative_z_send
!=========================================================================================
  subroutine derivative_z_calc(mx,my,mz,f,df,scale_1,n_sym,req)
!=========================================================================================
! evaluates the first derivative in z-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!  ae     - Explicit central difference stencil coefficient at (k +/- 1)
!  be     - Explicit central difference stencil coefficient at (k +/- 2)
!  ce     - Explicit central difference stencil coefficient at (k +/- 3)
!  de     - Explicit central difference stencil coefficient at (k +/- 4)
!  mx     - number of grid points in x-direction
!  my     - number of grid points in y-direction
!  mz     - number of grid points in z-direction
!
!       d()/dz = [ d(eta)/dz ]*[ d()/d(eta) ]
!        [ dz/d(eta) ]^-1*[ d()/d(eta) ]
!
!  scale_1- Proportional to ( dz/d(eta) )^-1 where eta is fictitious
!           uniform grid and z is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along z-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_z
  use param_m, only : periodic_z, vary_in_z, iorder
  use derivative_m, only : isym_z
  use derivative_m, only : ibound
  use derivative_m, only : neg_f => neg_f_z
  use derivative_m, only : pos_f => pos_f_z

  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(      mz) :: scale_1
  integer, dimension(4), intent(in) :: req

  integer, intent(in) :: n_sym

  integer :: i, j, k
  integer, dimension(6) :: lnbr(6)
  integer, dimension(MPI_Status_size,4) :: stat

  real :: ds,ae,be,ce,de

!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_z .eq. 0 ) then
     df = 0.0
     return
  endif
!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_z.ne.1) then
    ds = real(mz*zpes-1)
  else
    ds = real(mz*zpes-1) * scale_1(1)
  endif

  lnbr = neighbor

  if(periodic_z.eq.1) then
    if(zid.eq.0) then
      lnbr(5) = (zpes-1)*xpes*ypes+yid*xpes+xid
    endif
    if(zid.eq.zpes-1)then
      lnbr(6) = 0*xpes*ypes   +yid*xpes+xid
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

         do k = 4, mz-3
            do j = 1, my
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j,k+1)-f(i,j,k-1) )                          &
                            + be *( f(i,j,k+2)-f(i,j,k-2) )                          &
                            + ce *( f(i,j,k+3)-f(i,j,k-3) )
               end do
            end do
         end do

         if(unif_grid_z.ne.1) then
           do k = 4, mz-3
            df(:,:,k) = scale_1(k)*df(:,:,k)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( lnbr(5) >= 0 )then
            call MPI_Wait(req(1),stat(:,1),ierr)
         endif 
#endif
!
!  Internal nodes: edges
!
         if (neighbor(5).ge.0) then
            do j = 1, my
               do i = 1, mx
                  df(i,j,1) = ae *( f(i,j,2)-neg_f(i,j,3) )                          &
                            + be *( f(i,j,3)-neg_f(i,j,2) )                          &
                            + ce *( f(i,j,4)-neg_f(i,j,1) )
                  df(i,j,2) = ae *( f(i,j,3)-    f(i,j,1) )                          &
                            + be *( f(i,j,4)-neg_f(i,j,3) )                          &
                            + ce *( f(i,j,5)-neg_f(i,j,2) )
                  df(i,j,3) = ae *( f(i,j,4)-    f(i,j,2) )                          &
                            + be *( f(i,j,5)-    f(i,j,1) )                          &
                            + ce *( f(i,j,6)-neg_f(i,j,3) )
               end do
            end do
         endif

#ifndef COARRAYCOMMUNICATION
         if( lnbr(6) >= 0 ) then
            call MPI_Wait(req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(6).ge.0) then
            do j = 1, my
               do i = 1, mx
                  df(i,j,mz)   = ae *( pos_f(i,j,1   )   -f(i,j,mz-1) )              &
                               + be *( pos_f(i,j,2   )   -f(i,j,mz-2) )              &
                               + ce *( pos_f(i,j,3   )   -f(i,j,mz-3) )
                  df(i,j,mz-1) = ae *(     f(i,j,mz  )   -f(i,j,mz-2) )              &
                               + be *( pos_f(i,j,1   )   -f(i,j,mz-3) )              &
                               + ce *( pos_f(i,j,2   )   -f(i,j,mz-4) )
                  df(i,j,mz-2) = ae *(     f(i,j,mz-1)   -f(i,j,mz-3) )              &
                               + be *(     f(i,j,mz  )   -f(i,j,mz-4) )              &
                               + ce *( pos_f(i,j,1   )   -f(i,j,mz-5) )
               end do
            end do
         endif

!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_z .eq. 0 ) .and. ( periodic_z .eq. 0 )) then

               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,   1) = (-11.*f(i,j,   1)+18.*f(i,j,   2)             &
                                        - 9.*f(i,j,   3)+ 2.*f(i,j,   4)             &
                                       )/ 6. *ds

                        df(i,j,   2) = (- 2.*f(i,j,   1)- 3.*f(i,j,   2)             &
                                        + 6.*f(i,j,   3)- 1.*f(i,j,   4)             &
                                       )/ 6. *ds

                        df(i,j,   3) = (+ 1.*f(i,j,   1)- 8.*f(i,j,   2)             &
                                        + 8.*f(i,j,   4)- 1.*f(i,j,   5)             &
                                       )/12.*ds

                        df(i,j,   4) = (- 1.*f(i,j,   1)+ 9.*f(i,j,   2)             &
                                        -45.*f(i,j,   3)+45.*f(i,j,   5)             &
                                        - 9.*f(i,j,   6)+ 1.*f(i,j,   7)             &
                                       )/60.*ds
                     end do
                  end do
               end if
               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,mz-3) = (+ 1.*f(i,j,mz  )- 9.*f(i,j,mz-1)             &
                                        +45.*f(i,j,mz-2)-45.*f(i,j,mz-4)             &
                                        + 9.*f(i,j,mz-5)- 1.*f(i,j,mz-6)             &
                                       )/60.*ds

                        df(i,j,mz-2) = (- 1.*f(i,j,mz  )+ 8.*f(i,j,mz-1)             &
                                        - 8.*f(i,j,mz-3)+ 1.*f(i,j,mz-4)             &
                                       )/12.*ds

                        df(i,j,mz-1) = (+ 2.*f(i,j,mz  )+ 3.*f(i,j,mz-1)             &
                                        - 6.*f(i,j,mz-2)+ 1.*f(i,j,mz-3)             &
                                       )/6. *ds

                        df(i,j,mz  ) = (+11.*f(i,j,mz  )-18.*f(i,j,mz-1)             &
                                        + 9.*f(i,j,mz-2)- 2.*f(i,j,mz-3)             &
                                       )/ 6. *ds
                     end do
                  end do
               end if
            elseif (( isym_z .eq. 0 ) .and. ( periodic_z .eq. 1 )) then
               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,   1) = ae *( f(i,j   ,2) - neg_f(i,j,3))             &
                                     + be *( f(i,j   ,3) - neg_f(i,j,2))             &
                                     + ce *( f(i,j   ,4) - neg_f(i,j,1))

                        df(i,j,   2) = ae *( f(i,j   ,3) -     f(i,j,1))             &
                                     + be *( f(i,j   ,4) - neg_f(i,j,3))             &
                                     + ce *( f(i,j   ,5) - neg_f(i,j,2))

                        df(i,j,   3) = ae *( f(i,j   ,4) -     f(i,j,2))             &
                                     + be *( f(i,j   ,5) -     f(i,j,1))             &
                                     + ce *( f(i,j   ,6) - neg_f(i,j,3))
                     end do
                  end do
               end if

               if (neighbor(6).lt.0)then

                  do j = 1, my
                     do i = 1, mx
                        df(i,j,mz-2) = ae *(    f(i,j,mz-1)-f(i,j,mz-3))             &
                                     + be *(    f(i,j,mz  )-f(i,j,mz-4))             &
                                     + ce *(pos_f(i,j,   1)-f(i,j,mz-5))

                        df(i,j,mz-1) = ae *(    f(i,j,mz  )-f(i,j,mz-2))             &
                                     + be *(pos_f(i,j,   1)-f(i,j,mz-3))             &
                                     + ce *(pos_f(i,j,   2)-f(i,j,mz-4))

                        df(i,j,mz  ) = ae *(pos_f(i,j,   1)-f(i,j,mz-1))             &
                                     + be *(pos_f(i,j,   2)-f(i,j,mz-2))             &
                                     + ce *(pos_f(i,j,   3)-f(i,j,mz-3))

                     end do
                  end do
               end if

            elseif ( isym_z .eq. 1 ) then

               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,   1) =  ae*( f(i,j,2) - n_sym*f(i,j,2) )             &
                                      + be*( f(i,j,3) - n_sym*f(i,j,3) )             &
                                      + ce*( f(i,j,4) - n_sym*f(i,j,4) )

                        df(i,j,   2) =  ae*( f(i,j,3) -       f(i,j,1) )             &
                                      + be*( f(i,j,4) - n_sym*f(i,j,2) )             &
                                      + ce*( f(i,j,5) - n_sym*f(i,j,3) )

                        df(i,j,   3) =  ae*( f(i,j,4) -       f(i,j,2) )             &
                                      + be*( f(i,j,5) -       f(i,j,1) )             &
                                      + ce*( f(i,j,6) - n_sym*f(i,j,2) )
                     end do
                  end do
               end if

               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,mz-2) = (- 1.*f(i,j,mz  )+ 8.*f(i,j,mz-1)             &
                                        - 8.*f(i,j,mz-3)+ 1.*f(i,j,mz-4)             &
                                       )/12.*ds

                        df(i,j,mz-1) = (+ 2.*f(i,j,mz  )+ 3.*f(i,j,mz-1)             &
                                        - 6.*f(i,j,mz-2)+ 1.*f(i,j,mz-3)             &
                                       )/6. *ds

                        df(i,j,mz  ) = (+11.*f(i,j,mz  )-18.*f(i,j,mz-1)             &
                                        + 9.*f(i,j,mz-2)- 2.*f(i,j,mz-3)             &
                                       )/6. *ds

                     end do
                  end do
               end if

            end if
         else
            if ( myid.eq.0 ) then
               write(6,2) ibound
 2             format('IBOUND = ', i4,' is not supported')
            end if
            call terminate_run(6,0)

         end if

         if(unif_grid_z.ne.1) then
           do k = 1, 3
            df(:,:,k) = scale_1(k)*df(:,:,k)
           end do
           do k = mz-2,mz
            df(:,:,k) = scale_1(k)*df(:,:,k)
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
         do k = 5, mz-4
            do j = 1, my
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j,k+1)-f(i,j,k-1) )                          &
                       + be *( f(i,j,k+2)-f(i,j,k-2) )                               &
                       + ce *( f(i,j,k+3)-f(i,j,k-3) )                               &
                       + de *( f(i,j,k+4)-f(i,j,k-4) )
               end do
            end do
         end do

         if(unif_grid_z.ne.1) then
           do k = 5,mz-4
            df(:,:,k) = scale_1(k)*df(:,:,k)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( lnbr(5) >= 0 )then
            call MPI_Wait(req(1),stat(:,1),ierr)
         endif 
#endif

!
!  Internal nodes: edges
!
         if (neighbor(5).ge.0) then
            do j = 1, my
               do i = 1, mx
                  df(i,j,1) = ae *( f(i,j,2)-neg_f(i,j,4) )                          &
                            + be *( f(i,j,3)-neg_f(i,j,3) )                          &
                            + ce *( f(i,j,4)-neg_f(i,j,2) )                          &
                            + de *( f(i,j,5)-neg_f(i,j,1) )
                  df(i,j,2) = ae *( f(i,j,3)-    f(i,j,1) )                          &
                            + be *( f(i,j,4)-neg_f(i,j,4) )                          &
                            + ce *( f(i,j,5)-neg_f(i,j,3) )                          &
                            + de *( f(i,j,6)-neg_f(i,j,2) )
                  df(i,j,3) = ae *( f(i,j,4)-    f(i,j,2) )                          &
                            + be *( f(i,j,5)-    f(i,j,1) )                          &
                            + ce *( f(i,j,6)-neg_f(i,j,4) )                          &
                            + de *( f(i,j,7)-neg_f(i,j,3) )
                  df(i,j,4) = ae *( f(i,j,5)-    f(i,j,3) )                          &
                            + be *( f(i,j,6)-    f(i,j,2) )                          &
                            + ce *( f(i,j,7)-    f(i,j,1) )                          &
                            + de *( f(i,j,8)-neg_f(i,j,4) )
               end do
            end do
         end if

#ifndef COARRAYCOMMUNICATION
         if( lnbr(6) >= 0 ) then
            call MPI_Wait(req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(6).ge.0) then
            do j = 1, my
               do i = 1, mx
                  df(i,j,mz  ) = ae *( pos_f(i,j,1   )   -f(i,j,mz-1) )              &
                               + be *( pos_f(i,j,2   )   -f(i,j,mz-2) )              &
                               + ce *( pos_f(i,j,3   )   -f(i,j,mz-3) )              &
                               + de *( pos_f(i,j,4   )   -f(i,j,mz-4) )
                  df(i,j,mz-1) = ae *(     f(i,j,mz  )   -f(i,j,mz-2) )              &
                               + be *( pos_f(i,j,1   )   -f(i,j,mz-3) )              &
                               + ce *( pos_f(i,j,2   )   -f(i,j,mz-4) )              &
                               + de *( pos_f(i,j,3   )   -f(i,j,mz-5) )
                  df(i,j,mz-2) = ae *(     f(i,j,mz-1)   -f(i,j,mz-3) )              &
                               + be *(     f(i,j,mz  )   -f(i,j,mz-4) )              &
                               + ce *( pos_f(i,j,1   )   -f(i,j,mz-5) )              &
                               + de *( pos_f(i,j,2   )   -f(i,j,mz-6) )
                  df(i,j,mz-3) = ae *(     f(i,j,mz-2)   -f(i,j,mz-4) )              &
                               + be *(     f(i,j,mz-1)   -f(i,j,mz-5) )              &
                               + ce *(     f(i,j,mz  )   -f(i,j,mz-6) )              &
                               + de *( pos_f(i,j,1   )   -f(i,j,mz-7) )
               end do
            end do
         end if
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_z .eq. 0 ) .and. ( periodic_z .eq. 0 )) then
               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,   1) = (-11.*f(i,j,   1)+18.*f(i,j,   2)             &
                                        - 9.*f(i,j,   3)+ 2.*f(i,j,   4)             &
                                       )/6. *ds

                        df(i,j,   2) = (- 2.*f(i,j,   1)- 3.*f(i,j,   2)             &
                                        + 6.*f(i,j,   3)- 1.*f(i,j,   4)             &
                                       )/6. *ds

                        df(i,j,   3) = (+ 1.*f(i,j,   1)- 8.*f(i,j,   2)             &
                                        + 8.*f(i,j,   4)- 1.*f(i,j,   5)             &
                                       )/12.*ds

                        df(i,j,   4) = (- 1.*f(i,j,   1)+ 9.*f(i,j,   2)             &
                                        -45.*f(i,j,   3)+45.*f(i,j,   5)             &
                                        - 9.*f(i,j,   6)+ 1.*f(i,j,   7)             &
                                       )/60.*ds
                     end do
                  end do
               end if
               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,mz-3) = (+ 1.*f(i,j,mz  )- 9.*f(i,j,mz-1)             &
                                        +45.*f(i,j,mz-2)-45.*f(i,j,mz-4)             &
                                        + 9.*f(i,j,mz-5)- 1.*f(i,j,mz-6)             &
                                       )/60.*ds

                        df(i,j,mz-2) = (- 1.*f(i,j,mz  )+ 8.*f(i,j,mz-1)             &
                                        - 8.*f(i,j,mz-3)+ 1.*f(i,j,mz-4)             &
                                       )/12.*ds

                        df(i,j,mz-1) = (+ 2.*f(i,j,mz  )+ 3.*f(i,j,mz-1)             &
                                        - 6.*f(i,j,mz-2)+ 1.*f(i,j,mz-3)             &
                                       )/6. *ds

                        df(i,j,mz  ) = (+11.*f(i,j,mz  )-18.*f(i,j,mz-1)             &
                                        + 9.*f(i,j,mz-2)- 2.*f(i,j,mz-3)             &
                                       )/6. *ds
                     end do
                  end do
               end if

            elseif (( isym_z .eq. 0 ) .and. ( periodic_z .eq. 1 )) then
               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,   1) = ae *( f(i,j   ,2) - neg_f(i,j,4))             &
                                     + be *( f(i,j   ,3) - neg_f(i,j,3))             &
                                     + ce *( f(i,j   ,4) - neg_f(i,j,2))             &
                                     + de *( f(i,j   ,5) - neg_f(i,j,1))

                        df(i,j,   2) = ae *( f(i,j   ,3) -     f(i,j,1))             &
                                     + be *( f(i,j   ,4) - neg_f(i,j,4))             &
                                     + ce *( f(i,j   ,5) - neg_f(i,j,3))             &
                                     + de *( f(i,j   ,6) - neg_f(i,j,2))

                        df(i,j,   3) = ae *( f(i,j   ,4) -     f(i,j,2))             &
                                     + be *( f(i,j   ,5) -     f(i,j,1))             &
                                     + ce *( f(i,j   ,6) - neg_f(i,j,4))             &
                                     + de *( f(i,j   ,7) - neg_f(i,j,3))

                        df(i,j,   4) = ae *( f(i,j   ,5) -     f(i,j,3))             &
                                     + be *( f(i,j   ,6) -     f(i,j,2))             &
                                     + ce *( f(i,j   ,7) -     f(i,j,1))             &
                                     + de *( f(i,j   ,8) - neg_f(i,j,4))

                     end do
                  end do
               end if
               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,mz-3) = ae *(    f(i,j,mz-2)-f(i,j,mz-4))             &
                                     + be *(    f(i,j,mz-1)-f(i,j,mz-5))             &
                                     + ce *(    f(i,j,mz  )-f(i,j,mz-6))             &
                                     + de *(pos_f(i,j,   1)-f(i,j,mz-7))

                        df(i,j,mz-2) = ae *(    f(i,j,mz-1)-f(i,j,mz-3))             &
                                     + be *(    f(i,j,mz  )-f(i,j,mz-4))             &
                                     + ce *(pos_f(i,j,   1)-f(i,j,mz-5))             &
                                     + de *(pos_f(i,j,   2)-f(i,j,mz-6))

                        df(i,j,mz-1) = ae *(    f(i,j,mz  )-f(i,j,mz-2))             &
                                     + be *(pos_f(i,j,   1)-f(i,j,mz-3))             &
                                     + ce *(pos_f(i,j,   2)-f(i,j,mz-4))             &
                                     + de *(pos_f(i,j,   3)-f(i,j,mz-5))

                        df(i,j,mz  ) = ae *(pos_f(i,j,   1)-f(i,j,mz-1))             &
                                     + be *(pos_f(i,j,   2)-f(i,j,mz-2))             &
                                     + ce *(pos_f(i,j,   3)-f(i,j,mz-3))             &
                                     + de *(pos_f(i,j,   4)-f(i,j,mz-4))

                     end do
                  end do
               end if

            elseif ( isym_z .eq. 1 ) then
               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,   1) = ae*( f(i,j,2) - n_sym*f(i,j,2) )              &
                                     + be*( f(i,j,3) - n_sym*f(i,j,3) )              &
                                     + ce*( f(i,j,4) - n_sym*f(i,j,4) )              &
                                     + de*( f(i,j,5) - n_sym*f(i,j,5) )

                        df(i,j,   2) = ae*( f(i,j,3) -       f(i,j,1) )              &
                                     + be*( f(i,j,4) - n_sym*f(i,j,2) )              &
                                     + ce*( f(i,j,5) - n_sym*f(i,j,3) )              &
                                     + de*( f(i,j,6) - n_sym*f(i,j,4) )

                        df(i,j,   3) = ae*( f(i,j,4) -       f(i,j,2) )              &
                                     + be*( f(i,j,5) -       f(i,j,1) )              &
                                     + ce*( f(i,j,6) - n_sym*f(i,j,2) )              &
                                     + de*( f(i,j,7) - n_sym*f(i,j,3) )

                        df(i,j,   4) = ae*( f(i,j,5) -       f(i,j,3) )              &
                                     + be*( f(i,j,6) -       f(i,j,2) )              &
                                     + ce*( f(i,j,7) -       f(i,j,1) )              &
                                     + de*( f(i,j,8) - n_sym*f(i,j,2) )
                     end do
                  end do
               end if
               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,mz-3) = (  1.*f(i,j,mz  )- 9.*f(i,j,mz-1)             &
                                        +45.*f(i,j,mz-2)-45.*f(i,j,mz-4)             &
                                        + 9.*f(i,j,mz-5)- 1.*f(i,j,mz-6)             &
                                       )/60.*ds

                        df(i,j,mz-2) = (- 1.*f(i,j,mz  )+ 8.*f(i,j,mz-1)             &
                                        - 8.*f(i,j,mz-3)+ 1.*f(i,j,mz-4)             &
                                       )/12.*ds

                        df(i,j,mz-1) = (  2.*f(i,j,mz  )+ 3.*f(i,j,mz-1)             &
                                        - 6.*f(i,j,mz-2)+ 1.*f(i,j,mz-3)             &
                                       )/6. *ds

                        df(i,j,mz  ) = ( 11.*f(i,j,mz  )-18.*f(i,j,mz-1)             &
                                        + 9.*f(i,j,mz-2)- 2.*f(i,j,mz-3)             &
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
         if(unif_grid_z.ne.1) then
           do k = 1,4
            df(:,:,k) = scale_1(k)*df(:,:,k)
           end do
           do k = mz-3,mz
            df(:,:,k) = scale_1(k)*df(:,:,k)
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
  if( lnbr(5) >= 0 )then
     call MPI_Wait(req(2),stat(:,2),ierr)
  endif 
  if( lnbr(6) >= 0 ) then
     call MPI_Wait(req(4),stat(:,4),ierr)
  endif 
#endif

  return
  end subroutine derivative_z_calc
!=========================================================================================
  subroutine derivative_z_calc_buff(mx,my,mz,f,df,scale_1,n_sym, idx, label)
!=========================================================================================
! evaluates the first derivative in z-direction using explicit differencing
!
!  ds     - scaled grid spacing
!  f      - Function to be differentiated
!  df     - Differentiated Function
!  ae     - Explicit central difference stencil coefficient at (k +/- 1)
!  be     - Explicit central difference stencil coefficient at (k +/- 2)
!  ce     - Explicit central difference stencil coefficient at (k +/- 3)
!  de     - Explicit central difference stencil coefficient at (k +/- 4)
!  mx     - number of grid points in x-direction
!  my     - number of grid points in y-direction
!  mz     - number of grid points in z-direction
!
!       d()/dz = [ d(eta)/dz ]*[ d()/d(eta) ]
!        [ dz/d(eta) ]^-1*[ d()/d(eta) ]
!
!  scale_1- Proportional to ( dz/d(eta) )^-1 where eta is fictitious
!           uniform grid and z is nonuniform grid. When the grid is
!           uniform then scale_1 is constant along z-axis.
!  n_sym  - This could be buggy. Symmetry B.C.'s are applied at the
!           left end of the domain only. Scalars are symmetrical about
!           symmetry plane as are tangential comonents of vectors. Normal
!           components of vectors are antisymmetric. Symm. BC is a
!           physical BC not a computational BC!!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_z
  use param_m, only : periodic_z, vary_in_z, iorder
  use derivative_m, only : isym_z
  use derivative_m, only : ibound

  use derivative_m
  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in),  dimension(mx,my,mz) :: f
  real, intent(out), dimension(mx,my,mz) :: df
  real, intent(in),  dimension(      mz) :: scale_1

  integer, intent(in) :: n_sym
  integer, intent(in) :: idx

  integer :: i, j, k
  integer, dimension(6) :: lnbr(6)
  integer, dimension(MPI_Status_size,4) :: stat

  real :: ds,ae,be,ce,de

  character(*), intent(in) :: label
!-----------------------------------------------------------------------------------------
! return zero if direction is not active

  if( vary_in_z .eq. 0 ) then
     df = 0.0
     return
  endif

  ! check for misuse
#ifdef DERIV_CHECK_LABELS
  if( trim(deriv_z_list(idx)%fldname) .ne. trim(label) ) then
      write(*,'(2a,i5)') 'Mismatch name in calc: ', trim(label), idx
      call print_deriv_list(6)
  endif
#endif
!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_z.ne.1) then
    ds = real(mz*zpes-1)
  else
    ds = real(mz*zpes-1) * scale_1(1)
  endif

  lnbr = neighbor

  if(periodic_z.eq.1) then
    if(zid.eq.0) then
      lnbr(5) = (zpes-1)*xpes*ypes+yid*xpes+xid
    endif
    if(zid.eq.zpes-1)then
      lnbr(6) = 0*xpes*ypes   +yid*xpes+xid
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

         do k = 4, mz-3
            do j = 1, my
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j,k+1)-f(i,j,k-1) )                          &
                            + be *( f(i,j,k+2)-f(i,j,k-2) )                          &
                            + ce *( f(i,j,k+3)-f(i,j,k-3) )
               end do
            end do
         end do

         if(unif_grid_z.ne.1) then
           do k = 4, mz-3
            df(:,:,k) = scale_1(k)*df(:,:,k)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( deriv_z_list(idx)%neg_nbr >= 0 )then
            call MPI_Wait(deriv_z_list(idx)%req(1),stat(:,1),ierr)
         endif 
#endif
!
!  Internal nodes: edges
!
         if (neighbor(5).ge.0) then
            do j = 1, my
               do i = 1, mx
                  df(i,j,1) = ae *( f(i,j,2)-neg_f_z_buf(i,j,3,idx) )                          &
                            + be *( f(i,j,3)-neg_f_z_buf(i,j,2,idx) )                          &
                            + ce *( f(i,j,4)-neg_f_z_buf(i,j,1,idx) )
                  df(i,j,2) = ae *( f(i,j,3)-    f(i,j,1) )                          &
                            + be *( f(i,j,4)-neg_f_z_buf(i,j,3,idx) )                          &
                            + ce *( f(i,j,5)-neg_f_z_buf(i,j,2,idx) )
                  df(i,j,3) = ae *( f(i,j,4)-    f(i,j,2) )                          &
                            + be *( f(i,j,5)-    f(i,j,1) )                          &
                            + ce *( f(i,j,6)-neg_f_z_buf(i,j,3,idx) )
               end do
            end do
         endif

#ifndef COARRAYCOMMUNICATION
         if( deriv_z_list(idx)%pos_nbr >= 0 )then
            call MPI_Wait(deriv_z_list(idx)%req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(6).ge.0) then
            do j = 1, my
               do i = 1, mx
                  df(i,j,mz)   = ae *( pos_f_z_buf(i,j,1   ,idx)   -f(i,j,mz-1) )              &
                               + be *( pos_f_z_buf(i,j,2   ,idx)   -f(i,j,mz-2) )              &
                               + ce *( pos_f_z_buf(i,j,3   ,idx)   -f(i,j,mz-3) )
                  df(i,j,mz-1) = ae *(     f(i,j,mz  )   -f(i,j,mz-2) )              &
                               + be *( pos_f_z_buf(i,j,1   ,idx)   -f(i,j,mz-3) )              &
                               + ce *( pos_f_z_buf(i,j,2   ,idx)   -f(i,j,mz-4) )
                  df(i,j,mz-2) = ae *(     f(i,j,mz-1)   -f(i,j,mz-3) )              &
                               + be *(     f(i,j,mz  )   -f(i,j,mz-4) )              &
                               + ce *( pos_f_z_buf(i,j,1   ,idx)   -f(i,j,mz-5) )
               end do
            end do
         endif

!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_z .eq. 0 ) .and. ( periodic_z .eq. 0 )) then

               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,   1) = (-11.*f(i,j,   1)+18.*f(i,j,   2)             &
                                        - 9.*f(i,j,   3)+ 2.*f(i,j,   4)             &
                                       )/ 6. *ds

                        df(i,j,   2) = (- 2.*f(i,j,   1)- 3.*f(i,j,   2)             &
                                        + 6.*f(i,j,   3)- 1.*f(i,j,   4)             &
                                       )/ 6. *ds

                        df(i,j,   3) = (+ 1.*f(i,j,   1)- 8.*f(i,j,   2)             &
                                        + 8.*f(i,j,   4)- 1.*f(i,j,   5)             &
                                       )/12.*ds

                        df(i,j,   4) = (- 1.*f(i,j,   1)+ 9.*f(i,j,   2)             &
                                        -45.*f(i,j,   3)+45.*f(i,j,   5)             &
                                        - 9.*f(i,j,   6)+ 1.*f(i,j,   7)             &
                                       )/60.*ds
                     end do
                  end do
               end if
               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,mz-3) = (+ 1.*f(i,j,mz  )- 9.*f(i,j,mz-1)             &
                                        +45.*f(i,j,mz-2)-45.*f(i,j,mz-4)             &
                                        + 9.*f(i,j,mz-5)- 1.*f(i,j,mz-6)             &
                                       )/60.*ds

                        df(i,j,mz-2) = (- 1.*f(i,j,mz  )+ 8.*f(i,j,mz-1)             &
                                        - 8.*f(i,j,mz-3)+ 1.*f(i,j,mz-4)             &
                                       )/12.*ds

                        df(i,j,mz-1) = (+ 2.*f(i,j,mz  )+ 3.*f(i,j,mz-1)             &
                                        - 6.*f(i,j,mz-2)+ 1.*f(i,j,mz-3)             &
                                       )/6. *ds

                        df(i,j,mz  ) = (+11.*f(i,j,mz  )-18.*f(i,j,mz-1)             &
                                        + 9.*f(i,j,mz-2)- 2.*f(i,j,mz-3)             &
                                       )/ 6. *ds
                     end do
                  end do
               end if
            elseif (( isym_z .eq. 0 ) .and. ( periodic_z .eq. 1 )) then
               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,   1) = ae *( f(i,j   ,2) - neg_f_z_buf(i,j,3,idx))             &
                                     + be *( f(i,j   ,3) - neg_f_z_buf(i,j,2,idx))             &
                                     + ce *( f(i,j   ,4) - neg_f_z_buf(i,j,1,idx))

                        df(i,j,   2) = ae *( f(i,j   ,3) -     f(i,j,1))             &
                                     + be *( f(i,j   ,4) - neg_f_z_buf(i,j,3,idx))             &
                                     + ce *( f(i,j   ,5) - neg_f_z_buf(i,j,2,idx))

                        df(i,j,   3) = ae *( f(i,j   ,4) -     f(i,j,2))             &
                                     + be *( f(i,j   ,5) -     f(i,j,1))             &
                                     + ce *( f(i,j   ,6) - neg_f_z_buf(i,j,3,idx))
                     end do
                  end do
               end if

               if (neighbor(6).lt.0)then

                  do j = 1, my
                     do i = 1, mx
                        df(i,j,mz-2) = ae *(    f(i,j,mz-1)-f(i,j,mz-3))             &
                                     + be *(    f(i,j,mz  )-f(i,j,mz-4))             &
                                     + ce *(pos_f_z_buf(i,j,   1,idx)-f(i,j,mz-5))

                        df(i,j,mz-1) = ae *(    f(i,j,mz  )-f(i,j,mz-2))             &
                                     + be *(pos_f_z_buf(i,j,   1,idx)-f(i,j,mz-3))             &
                                     + ce *(pos_f_z_buf(i,j,   2,idx)-f(i,j,mz-4))

                        df(i,j,mz  ) = ae *(pos_f_z_buf(i,j,   1,idx)-f(i,j,mz-1))             &
                                     + be *(pos_f_z_buf(i,j,   2,idx)-f(i,j,mz-2))             &
                                     + ce *(pos_f_z_buf(i,j,   3,idx)-f(i,j,mz-3))

                     end do
                  end do
               end if

            elseif ( isym_z .eq. 1 ) then

               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,   1) =  ae*( f(i,j,2) - n_sym*f(i,j,2) )             &
                                      + be*( f(i,j,3) - n_sym*f(i,j,3) )             &
                                      + ce*( f(i,j,4) - n_sym*f(i,j,4) )

                        df(i,j,   2) =  ae*( f(i,j,3) -       f(i,j,1) )             &
                                      + be*( f(i,j,4) - n_sym*f(i,j,2) )             &
                                      + ce*( f(i,j,5) - n_sym*f(i,j,3) )

                        df(i,j,   3) =  ae*( f(i,j,4) -       f(i,j,2) )             &
                                      + be*( f(i,j,5) -       f(i,j,1) )             &
                                      + ce*( f(i,j,6) - n_sym*f(i,j,2) )
                     end do
                  end do
               end if

               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,mz-2) = (- 1.*f(i,j,mz  )+ 8.*f(i,j,mz-1)             &
                                        - 8.*f(i,j,mz-3)+ 1.*f(i,j,mz-4)             &
                                       )/12.*ds

                        df(i,j,mz-1) = (+ 2.*f(i,j,mz  )+ 3.*f(i,j,mz-1)             &
                                        - 6.*f(i,j,mz-2)+ 1.*f(i,j,mz-3)             &
                                       )/6. *ds

                        df(i,j,mz  ) = (+11.*f(i,j,mz  )-18.*f(i,j,mz-1)             &
                                        + 9.*f(i,j,mz-2)- 2.*f(i,j,mz-3)             &
                                       )/6. *ds

                     end do
                  end do
               end if

            end if
         else
            if ( myid.eq.0 ) then
               write(6,2) ibound
 2             format('IBOUND = ', i4,' is not supported')
            end if
            call terminate_run(6,0)

         end if

         if(unif_grid_z.ne.1) then
           do k = 1, 3
            df(:,:,k) = scale_1(k)*df(:,:,k)
           end do
           do k = mz-2,mz
            df(:,:,k) = scale_1(k)*df(:,:,k)
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
         do k = 5, mz-4
            do j = 1, my
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j,k+1)-f(i,j,k-1) )                          &
                       + be *( f(i,j,k+2)-f(i,j,k-2) )                               &
                       + ce *( f(i,j,k+3)-f(i,j,k-3) )                               &
                       + de *( f(i,j,k+4)-f(i,j,k-4) )
               end do
            end do
         end do

         if(unif_grid_z.ne.1) then
           do k = 5,mz-4
            df(:,:,k) = scale_1(k)*df(:,:,k)
           end do
         endif

#ifndef COARRAYCOMMUNICATION
       ! be sure that we have finished communicating the "ghost" points
       ! BUG FIX Evatt Hawkes 29-NOV-2004 - only call waits when we did a send or receive
         if( deriv_z_list(idx)%neg_nbr >= 0 )then
            call MPI_Wait(deriv_z_list(idx)%req(1),stat(:,1),ierr)
         endif 
#endif

!
!  Internal nodes: edges
!
         if (neighbor(5).ge.0) then
            do j = 1, my
               do i = 1, mx
                  df(i,j,1) = ae *( f(i,j,2)-neg_f_z_buf(i,j,4,idx) )                          &
                            + be *( f(i,j,3)-neg_f_z_buf(i,j,3,idx) )                          &
                            + ce *( f(i,j,4)-neg_f_z_buf(i,j,2,idx) )                          &
                            + de *( f(i,j,5)-neg_f_z_buf(i,j,1,idx) )
                  df(i,j,2) = ae *( f(i,j,3)-    f(i,j,1) )                          &
                            + be *( f(i,j,4)-neg_f_z_buf(i,j,4,idx) )                          &
                            + ce *( f(i,j,5)-neg_f_z_buf(i,j,3,idx) )                          &
                            + de *( f(i,j,6)-neg_f_z_buf(i,j,2,idx) )
                  df(i,j,3) = ae *( f(i,j,4)-    f(i,j,2) )                          &
                            + be *( f(i,j,5)-    f(i,j,1) )                          &
                            + ce *( f(i,j,6)-neg_f_z_buf(i,j,4,idx) )                          &
                            + de *( f(i,j,7)-neg_f_z_buf(i,j,3,idx) )
                  df(i,j,4) = ae *( f(i,j,5)-    f(i,j,3) )                          &
                            + be *( f(i,j,6)-    f(i,j,2) )                          &
                            + ce *( f(i,j,7)-    f(i,j,1) )                          &
                            + de *( f(i,j,8)-neg_f_z_buf(i,j,4,idx) )
               end do
            end do
         end if

#ifndef COARRAYCOMMUNICATION
         if( deriv_z_list(idx)%pos_nbr >= 0 )then
            call MPI_Wait(deriv_z_list(idx)%req(3),stat(:,3),ierr)
         endif 
#endif

         if (neighbor(6).ge.0) then
            do j = 1, my
               do i = 1, mx
                  df(i,j,mz  ) = ae *( pos_f_z_buf(i,j,1,idx)   -f(i,j,mz-1) )              &
                               + be *( pos_f_z_buf(i,j,2,idx)   -f(i,j,mz-2) )              &
                               + ce *( pos_f_z_buf(i,j,3,idx)   -f(i,j,mz-3) )              &
                               + de *( pos_f_z_buf(i,j,4,idx)   -f(i,j,mz-4) )
                  df(i,j,mz-1) = ae *(     f(i,j,mz  )   -f(i,j,mz-2) )              &
                               + be *( pos_f_z_buf(i,j,1,idx)   -f(i,j,mz-3) )              &
                               + ce *( pos_f_z_buf(i,j,2,idx)   -f(i,j,mz-4) )              &
                               + de *( pos_f_z_buf(i,j,3,idx)   -f(i,j,mz-5) )
                  df(i,j,mz-2) = ae *(     f(i,j,mz-1)   -f(i,j,mz-3) )              &
                               + be *(     f(i,j,mz  )   -f(i,j,mz-4) )              &
                               + ce *( pos_f_z_buf(i,j,1,idx)   -f(i,j,mz-5) )              &
                               + de *( pos_f_z_buf(i,j,2,idx)   -f(i,j,mz-6) )
                  df(i,j,mz-3) = ae *(     f(i,j,mz-2)   -f(i,j,mz-4) )              &
                               + be *(     f(i,j,mz-1)   -f(i,j,mz-5) )              &
                               + ce *(     f(i,j,mz  )   -f(i,j,mz-6) )              &
                               + de *( pos_f_z_buf(i,j,1,idx)   -f(i,j,mz-7) )
               end do
            end do
         end if
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_z .eq. 0 ) .and. ( periodic_z .eq. 0 )) then
               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,   1) = (-11.*f(i,j,   1)+18.*f(i,j,   2)             &
                                        - 9.*f(i,j,   3)+ 2.*f(i,j,   4)             &
                                       )/6. *ds

                        df(i,j,   2) = (- 2.*f(i,j,   1)- 3.*f(i,j,   2)             &
                                        + 6.*f(i,j,   3)- 1.*f(i,j,   4)             &
                                       )/6. *ds

                        df(i,j,   3) = (+ 1.*f(i,j,   1)- 8.*f(i,j,   2)             &
                                        + 8.*f(i,j,   4)- 1.*f(i,j,   5)             &
                                       )/12.*ds

                        df(i,j,   4) = (- 1.*f(i,j,   1)+ 9.*f(i,j,   2)             &
                                        -45.*f(i,j,   3)+45.*f(i,j,   5)             &
                                        - 9.*f(i,j,   6)+ 1.*f(i,j,   7)             &
                                       )/60.*ds
                     end do
                  end do
               end if
               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,mz-3) = (+ 1.*f(i,j,mz  )- 9.*f(i,j,mz-1)             &
                                        +45.*f(i,j,mz-2)-45.*f(i,j,mz-4)             &
                                        + 9.*f(i,j,mz-5)- 1.*f(i,j,mz-6)             &
                                       )/60.*ds

                        df(i,j,mz-2) = (- 1.*f(i,j,mz  )+ 8.*f(i,j,mz-1)             &
                                        - 8.*f(i,j,mz-3)+ 1.*f(i,j,mz-4)             &
                                       )/12.*ds

                        df(i,j,mz-1) = (+ 2.*f(i,j,mz  )+ 3.*f(i,j,mz-1)             &
                                        - 6.*f(i,j,mz-2)+ 1.*f(i,j,mz-3)             &
                                       )/6. *ds

                        df(i,j,mz  ) = (+11.*f(i,j,mz  )-18.*f(i,j,mz-1)             &
                                        + 9.*f(i,j,mz-2)- 2.*f(i,j,mz-3)             &
                                       )/6. *ds
                     end do
                  end do
               end if

            elseif (( isym_z .eq. 0 ) .and. ( periodic_z .eq. 1 )) then
               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,   1) = ae *( f(i,j   ,2) - neg_f_z_buf(i,j,4,idx))             &
                                     + be *( f(i,j   ,3) - neg_f_z_buf(i,j,3,idx))             &
                                     + ce *( f(i,j   ,4) - neg_f_z_buf(i,j,2,idx))             &
                                     + de *( f(i,j   ,5) - neg_f_z_buf(i,j,1,idx))

                        df(i,j,   2) = ae *( f(i,j   ,3) -     f(i,j,1))             &
                                     + be *( f(i,j   ,4) - neg_f_z_buf(i,j,4,idx))             &
                                     + ce *( f(i,j   ,5) - neg_f_z_buf(i,j,3,idx))             &
                                     + de *( f(i,j   ,6) - neg_f_z_buf(i,j,2,idx))

                        df(i,j,   3) = ae *( f(i,j   ,4) -     f(i,j,2))             &
                                     + be *( f(i,j   ,5) -     f(i,j,1))             &
                                     + ce *( f(i,j   ,6) - neg_f_z_buf(i,j,4,idx))             &
                                     + de *( f(i,j   ,7) - neg_f_z_buf(i,j,3,idx))

                        df(i,j,   4) = ae *( f(i,j   ,5) -     f(i,j,3))             &
                                     + be *( f(i,j   ,6) -     f(i,j,2))             &
                                     + ce *( f(i,j   ,7) -     f(i,j,1))             &
                                     + de *( f(i,j   ,8) - neg_f_z_buf(i,j,4,idx))

                     end do
                  end do
               end if
               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,mz-3) = ae *(    f(i,j,mz-2)-f(i,j,mz-4))             &
                                     + be *(    f(i,j,mz-1)-f(i,j,mz-5))             &
                                     + ce *(    f(i,j,mz  )-f(i,j,mz-6))             &
                                     + de *(pos_f_z_buf(i,j,   1,idx)-f(i,j,mz-7))

                        df(i,j,mz-2) = ae *(    f(i,j,mz-1)-f(i,j,mz-3))             &
                                     + be *(    f(i,j,mz  )-f(i,j,mz-4))             &
                                     + ce *(pos_f_z_buf(i,j,   1,idx)-f(i,j,mz-5))             &
                                     + de *(pos_f_z_buf(i,j,   2,idx)-f(i,j,mz-6))

                        df(i,j,mz-1) = ae *(    f(i,j,mz  )-f(i,j,mz-2))             &
                                     + be *(pos_f_z_buf(i,j,   1,idx)-f(i,j,mz-3))             &
                                     + ce *(pos_f_z_buf(i,j,   2,idx)-f(i,j,mz-4))             &
                                     + de *(pos_f_z_buf(i,j,   3,idx)-f(i,j,mz-5))

                        df(i,j,mz  ) = ae *(pos_f_z_buf(i,j,   1,idx)-f(i,j,mz-1))             &
                                     + be *(pos_f_z_buf(i,j,   2,idx)-f(i,j,mz-2))             &
                                     + ce *(pos_f_z_buf(i,j,   3,idx)-f(i,j,mz-3))             &
                                     + de *(pos_f_z_buf(i,j,   4,idx)-f(i,j,mz-4))

                     end do
                  end do
               end if

            elseif ( isym_z .eq. 1 ) then
               if (neighbor(5).lt.0)then
                  do j = 1, my
                     do i = 1, mx
                        df(i,j,   1) = ae*( f(i,j,2) - n_sym*f(i,j,2) )              &
                                     + be*( f(i,j,3) - n_sym*f(i,j,3) )              &
                                     + ce*( f(i,j,4) - n_sym*f(i,j,4) )              &
                                     + de*( f(i,j,5) - n_sym*f(i,j,5) )

                        df(i,j,   2) = ae*( f(i,j,3) -       f(i,j,1) )              &
                                     + be*( f(i,j,4) - n_sym*f(i,j,2) )              &
                                     + ce*( f(i,j,5) - n_sym*f(i,j,3) )              &
                                     + de*( f(i,j,6) - n_sym*f(i,j,4) )

                        df(i,j,   3) = ae*( f(i,j,4) -       f(i,j,2) )              &
                                     + be*( f(i,j,5) -       f(i,j,1) )              &
                                     + ce*( f(i,j,6) - n_sym*f(i,j,2) )              &
                                     + de*( f(i,j,7) - n_sym*f(i,j,3) )

                        df(i,j,   4) = ae*( f(i,j,5) -       f(i,j,3) )              &
                                     + be*( f(i,j,6) -       f(i,j,2) )              &
                                     + ce*( f(i,j,7) -       f(i,j,1) )              &
                                     + de*( f(i,j,8) - n_sym*f(i,j,2) )
                     end do
                  end do
               end if
               if (neighbor(6).lt.0)then
                  do j = 1, my
                     do i = 1, mx

                        df(i,j,mz-3) = (  1.*f(i,j,mz  )- 9.*f(i,j,mz-1)             &
                                        +45.*f(i,j,mz-2)-45.*f(i,j,mz-4)             &
                                        + 9.*f(i,j,mz-5)- 1.*f(i,j,mz-6)             &
                                       )/60.*ds

                        df(i,j,mz-2) = (- 1.*f(i,j,mz  )+ 8.*f(i,j,mz-1)             &
                                        - 8.*f(i,j,mz-3)+ 1.*f(i,j,mz-4)             &
                                       )/12.*ds

                        df(i,j,mz-1) = (  2.*f(i,j,mz  )+ 3.*f(i,j,mz-1)             &
                                        - 6.*f(i,j,mz-2)+ 1.*f(i,j,mz-3)             &
                                       )/6. *ds

                        df(i,j,mz  ) = ( 11.*f(i,j,mz  )-18.*f(i,j,mz-1)             &
                                        + 9.*f(i,j,mz-2)- 2.*f(i,j,mz-3)             &
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
         if(unif_grid_z.ne.1) then
           do k = 1,4
            df(:,:,k) = scale_1(k)*df(:,:,k)
           end do
           do k = mz-3,mz
            df(:,:,k) = scale_1(k)*df(:,:,k)
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
  if( deriv_z_list(idx)%neg_nbr >= 0 )then
     call MPI_Wait(deriv_z_list(idx)%req(2),stat(:,2),ierr)
  endif 
  if( deriv_z_list(idx)%pos_nbr >= 0 ) then
     call MPI_Wait(deriv_z_list(idx)%req(4),stat(:,4),ierr)
  endif 

  deriv_z_list(idx)%inuse = .false.
#endif

  return
  end subroutine derivative_z_calc_buff

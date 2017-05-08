#include "globalDefines.h"
!$Id: deriv_inplane_2.f90,v 1.1.1.1.12.1 2006/08/22 20:47:14 rsankar Exp $
!=========================================================================================
  subroutine deriv_inplane_2(f,df2,dir,n1,n2,n_sym)
!=========================================================================================
! evaluates the first derivative in the second dimension direction 
! using explicit differencing
!
! Author: Evatt Hawkes NOV 2004
!
!
!  f      - Function to be differentiated
!  df2    - Differentiated Function in second varying dimension of f
!
!  dir    - integer indicating which plane it is
!           = 1 for yz plane (x = constant) 
!           = 2 for xz plane (y = constant) 
!           = 3 for xy plane (z = constant) 
!
!  ds     - scaled grid spacing
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
!
!  Notes:   This routine based on original derivative_x routine.
!           Some args that were passed there are taken directly from the modules here.
!           That was to avoid coding six different routines here instead of two.
!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : unif_grid_x, unif_grid_y, unif_grid_z, &
                     scale_1x, scale_1y, scale_1z
  use param_m, only : periodic_x, periodic_y, periodic_z, &
                      vary_in_x, vary_in_y, vary_in_z, &
                      iorder, &
                      nx,ny,nz, &
                      nx_g,ny_g,nz_g
  use derivative_m, only : isym_x, isym_y, isym_z
  use derivative_m, only : ibound
  use derivative_m, only : neg_f_x_xy, neg_f_y_xy, pos_f_x_xy, pos_f_y_xy
  use derivative_m, only : neg_f_x_xz, neg_f_z_xz, pos_f_x_xz, pos_f_z_xz
  use derivative_m, only : neg_f_y_yz, neg_f_z_yz, pos_f_y_yz, pos_f_z_yz
  use derivative_m, only : neg_fs_x_xy, neg_fs_y_xy, pos_fs_x_xy, pos_fs_y_xy
  use derivative_m, only : neg_fs_x_xz, neg_fs_z_xz, pos_fs_x_xz, pos_fs_z_xz
  use derivative_m, only : neg_fs_y_yz, neg_fs_z_yz, pos_fs_y_yz, pos_fs_z_yz

  implicit none
!-----------------------------------------------------------------------------------------
! passed args

  integer, intent(in) :: n1,n2
  real, intent(in),  dimension(n1,n2) :: f
  real, intent(out), dimension(n1,n2) :: df2
  integer, intent(in) :: dir
  integer, intent(in) :: n_sym

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: nm, i, j, k

  integer, dimension(2) :: lnbr, nbr
  integer, dimension(4) :: req=MPI_REQUEST_NULL
  integer, dimension(MPI_Status_size,4) :: stat
  integer :: plane_2rows_type
  integer :: plane_comm, plane_id

  real :: ds,ae,be,ce,de

  integer :: m1, m2, m1_g, m2_g  !note m1=n1, m2=m2 - not checked here

  integer :: unif_grid_dir, periodic_dir, isym_dir

! pointers set for each case
  real,  dimension(:)  , pointer :: scale_1
  real,  dimension(:,:), pointer :: neg_f, neg_fs
  real,  dimension(:,:), pointer :: pos_f, pos_fs

!-----------------------------------------------------------------------------------------
  select case(dir)

  case(1)
    m1 = ny
    m2 = nz
    m1_g = ny_g
    m2_g = nz_g

    scale_1 => scale_1z

    pos_f => pos_f_z_yz
    neg_f => neg_f_z_yz

    pos_fs => pos_fs_z_yz
    neg_fs => neg_fs_z_yz

    lnbr(1) = neighbor(5)
    lnbr(2) = neighbor(6)
    nbr(:)=lnbr(:)
    if(periodic_z.eq.1) then
      if(zid.eq.0) then
        lnbr(1) = (zpes-1)*xpes*ypes+yid*xpes+xid
      endif
      if(zid.eq.zpes-1)then
        lnbr(2) = 0*xpes*ypes   +yid*xpes+xid
      endif
    endif

    plane_id =yz_id
    plane_comm = yz_comm
    plane_2rows_type = yz_zrows_type

    if( vary_in_z .eq. 0 ) then
       df2 = 0.0
       return
    endif

    unif_grid_dir = unif_grid_z
    periodic_dir = periodic_z
    isym_dir = isym_z

  case(2)

    m1 = nx
    m2 = nz
    m1_g = nx_g
    m2_g = nz_g

    scale_1 => scale_1z

    pos_f => pos_f_z_xz
    neg_f => neg_f_z_xz

    pos_fs => pos_fs_z_xz
    neg_fs => neg_fs_z_xz

    lnbr(1) = neighbor(5)
    lnbr(2) = neighbor(6)
    nbr(:)=lnbr(:)
    if(periodic_z.eq.1) then
      if(zid.eq.0) then
        lnbr(1) = (zpes-1)*xpes*ypes+yid*xpes+xid
      endif
      if(zid.eq.zpes-1)then
        lnbr(2) = 0*xpes*ypes   +yid*xpes+xid
      endif
    endif

    plane_id =xz_id
    plane_comm = xz_comm
    plane_2rows_type = xz_zrows_type

    if( vary_in_z .eq. 0 ) then
       df2 = 0.0
       return
    endif

    unif_grid_dir = unif_grid_z
    periodic_dir = periodic_z
    isym_dir = isym_z

  case(3)

    m1 = nx
    m2 = ny
    m1_g = nx_g
    m2_g = ny_g

    scale_1 => scale_1y

    pos_f => pos_f_y_xy
    neg_f => neg_f_y_xy

    pos_fs => pos_fs_y_xy
    neg_fs => neg_fs_y_xy

    lnbr(1) = neighbor(3)
    lnbr(2) = neighbor(4)
    nbr(:)=lnbr(:)
    if(periodic_y.eq.1) then
      if(yid.eq.0) then
        lnbr(1) = zid*xpes*ypes+(ypes-1)*xpes+xid
      endif
      if(yid.eq.ypes-1)then
        lnbr(2) = zid*xpes*ypes+(0     )*xpes+xid
      endif
    endif

    plane_id =xy_id
    plane_comm = xy_comm
    plane_2rows_type = xy_yrows_type

    if( vary_in_y .eq. 0 ) then
       df2 = 0.0
       return
    endif

    unif_grid_dir = unif_grid_y
    periodic_dir = periodic_y
    isym_dir = isym_y

  end select
!-----------------------------------------------------------------------------------------
! zero ghost cell arrays

  neg_f=0.0
  pos_f=0.0
  neg_fs=0.0
  pos_fs=0.0
!-----------------------------------------------------------------------------------------
! calculate ds for grid compression

  if(unif_grid_dir.ne.1) then
    ds = real(m2_g-1)
  else
    ds = real(m2_g-1) * scale_1(1)
  endif
!-----------------------------------------------------------------------------------------
! get neighboring cells data

  req = MPI_REQUEST_NULL
  if(lnbr(1)>=0) then
   ! get ghost cells from neighbor on (-y) side
     call MPI_IRecv(neg_f,(m1*iorder/2),MPI_REAL8,lnbr(1),1,gcomm,req(2),ierr)
   ! send ghost cells to neighbor on (-y) side
     pos_fs(:,:) = f(:,1:iorder/2)
     call MPI_ISend(pos_fs(1,1),(m1*iorder/2),MPI_REAL8,lnbr(1),2,gcomm,req(3),ierr)
   !  call MPI_ISend(f(1,1),1,plane_2rows_type,lnbr(1),2,gcomm,req(3),ierr)
  endif
  
  if(lnbr(2)>=0) then
   ! get ghost cells from neighbor on (+y) side
     call MPI_IRecv(pos_f,(m1*iorder/2),MPI_REAL8,lnbr(2),2,gcomm,req(4),ierr)
   ! send ghost cells to neighbor on (+y) side
     nm = m2 + 1 - iorder/2
     neg_fs(:,:) = f(:,nm:m2)
     call MPI_ISend(neg_fs(1,1),(m1*iorder/2),MPI_REAL8,lnbr(2),1,gcomm,req(1),ierr)
   !  call MPI_ISend(f(1,nm),1,plane_2rows_type,lnbr(2),1,gcomm,req(1),ierr)
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

            do j = 4, m2-3
               do i = 1, m1
                  df2(i,j) = ae *( f(i,j+1)-f(i,j-1) )                          &
                          + be *( f(i,j+2)-f(i,j-2) )                          &
                          + ce *( f(i,j+3)-f(i,j-3) )
               end do
            end do


       ! be sure that we have finished communicating the "ghost" points
         if( lnbr(1) >= 0 .or. lnbr(2) >= 0 ) then
            call MPI_Waitall(4,req,stat,ierr)
         endif
!
!  Internal nodes: edges
!
         if (nbr(1).ge.0) then
               do i = 1, m1
                  df2(i,1) = ae *( f(i,2)-neg_f(i,3) )                          &
                          + be *( f(i,3)-neg_f(i,2) )                            &
                          + ce *( f(i,4)-neg_f(i,1) )
                  df2(i,2) = ae *( f(i,3)-    f(i,1) )                          &
                          + be *( f(i,4)-neg_f(i,3) )                            &
                          + ce *( f(i,5)-neg_f(i,2) )
                  df2(i,3) = ae *( f(i,4)-    f(i,2) )                          &
                            + be *( f(i,5)-    f(i,1) )                          &
                            + ce *( f(i,6)-neg_f(i,3) )
               end do
         endif

         if (nbr(2).ge.0) then
               do i = 1, m1
                  df2(i,m2  ) = ae *( pos_f(i,1)   -f(i,m2-1) )                 &
                               + be *( pos_f(i,2)   -f(i,m2-2) )                 &
                               + ce *( pos_f(i,3)   -f(i,m2-3) )
                  df2(i,m2-1) = ae *(     f(i,m2)  -f(i,m2-2) )                 &
                               + be *( pos_f(i,1)   -f(i,m2-3) )                 &
                               + ce *( pos_f(i,2)   -f(i,m2-4) )
                  df2(i,m2-2) = ae *(     f(i,m2-1)-f(i,m2-3) )                 &
                               + be *(     f(i,m2)  -f(i,m2-4) )                 &
                               + ce *( pos_f(i,1)   -f(i,m2-5) )
               end do
         endif

!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_dir .eq. 0 ) .and. ( periodic_dir .eq. 0 )) then

               if (nbr(1).lt.0) then
                     do i = 1, m1
                        df2(i,   1) = (-11.*f(i,1) + 18.*f(i,2)                 &
                                        - 9.*f(i,3) +  2.*f(i,4))                &
                                        / 6. *ds

                        df2(i,   2) = (- 2.*f(i,1) -  3.*f(i,2)                 &
                                        + 6.*f(i,3) -  1.*f(i,4))                &
                                        / 6. *ds

                        df2(i,   3) = (+ 1.*f(i,1) -  8.*f(i,2)                 &
                                        + 8.*f(i,4) -  1.*f(i,5))                &
                                        /12.*ds
                     end do
               end if
               if (nbr(2).lt.0) then
                     do i = 1, m1
                        df2(i,m2-2) = (- 1.*f(i,m2  )+ 8.*f(i,m2-1)             &
                                        - 8.*f(i,m2-3)+ 1.*f(i,m2-4)             &
                                       )/12.*ds

                        df2(i,m2-1) = (+ 2.*f(i,m2  )+ 3.*f(i,m2-1)             &
                                        - 6.*f(i,m2-2)+ 1.*f(i,m2-3)             &
                                       )/ 6. *ds

                        df2(i,m2  ) = (+11.*f(i,m2  )-18.*f(i,m2-1)             &
                                        + 9.*f(i,m2-2)- 2.*f(i,m2-3)             &
                                       )/ 6. *ds
                     end do
               end if
            elseif (( isym_dir .eq. 0 ) .and. ( periodic_dir .eq. 1 )) then

               if (nbr(1).lt.0) then
                     do i = 1, m1

                        df2(i,   1) = ae *( f(i   ,2)- neg_f(i,3) )             &
                                     + be *( f(i   ,3)- neg_f(i,2) )             &
                                     + ce *( f(i   ,4)- neg_f(i,1) )

                        df2(i,   2) = ae *( f(i   ,3)-     f(i,1) )             &
                                     + be *( f(i   ,4)- neg_f(i,3) )             &
                                     + ce *( f(i   ,5)- neg_f(i,2) )

                        df2(i,   3) = ae *( f(i   ,4)-     f(i,2) )             &
                                     + be *( f(i   ,5)-     f(i,1) )             &
                                     + ce *( f(i   ,6)- neg_f(i,3) )
                     end do
               end if
               if (nbr(2).lt.0) then
                     do i = 1, m1

                        df2(i,m2-2) = ae*(    f(i,m2-1)-f(i,m2-3) )             &
                                     + be*(    f(i,m2  )-f(i,m2-4) )             &
                                     + ce*(pos_f(i,   1)-f(i,m2-5) )

                        df2(i,m2-1) = ae*(    f(i,m2  )-f(i,m2-2) )             &
                                     + be*(pos_f(i,   1)-f(i,m2-3) )             &
                                     + ce*(pos_f(i,   2)-f(i,m2-4) )

                        df2(i,m2  ) = ae*(pos_f(i,   1)-f(i,m2-1) )             &
                                     + be*(pos_f(i,   2)-f(i,m2-2) )             &
                                     + ce*(pos_f(i,   3)-f(i,m2-3) )

                     end do
               end if

            elseif ( isym_y .eq. 1 ) then

               if (nbr(1).lt.0) then
                     do i = 1, m1

                        df2(i,   1) = ae*( f(i,2)- n_sym*f(i,2) )               &
                                     + be*( f(i,3)- n_sym*f(i,3) )               &
                                     + ce*( f(i,4)- n_sym*f(i,4) )

                        df2(i,   2) = ae*( f(i,3)-       f(i,1) )               &
                                     + be*( f(i,4)- n_sym*f(i,2) )               &
                                     + ce*( f(i,5)- n_sym*f(i,3) )

                        df2(i,   3) = ae*( f(i,4)-       f(i,2) )               &
                                     + be*( f(i,5)-       f(i,1) )               &
                                     + ce*( f(i,6)- n_sym*f(i,2) )
                     end do
               end if
               if (nbr(2).lt.0) then
                     do i = 1, m1

                        df2(i,m2-2) = (- 1.*f(i,m2  )+ 8.*f(i,m2-1)             &
                                        - 8.*f(i,m2-3)+ 1.*f(i,m2-4)             &
                                       )/12.*ds

                        df2(i,m2-1) = (+ 2.*f(i,m2  )+ 3.*f(i,m2-1)             &
                                        - 6.*f(i,m2-2)+ 1.*f(i,m2-3)             &
                                       )/ 6. *ds

                        df2(i,m2  ) = ( 11.*f(i,m2  )-18.*f(i,m2-1)             &
                                        + 9.*f(i,m2-2)- 2.*f(i,m2-3)             &
                                       )/ 6. *ds

                     end do
               end if
            end if

         else                   ! Unsupported IBOUND
            if ( plane_id.eq.0 ) then
               write(6,2) ibound
 2             format('IBOUND = ', i4,' is not supported')
            end if
            call terminate_run(6,0)
         end if
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


            do j = 5, m2-4
               do i = 1, m1
                  df2(i,j) = ae *( f(i,j+1)-f(i,j-1) )                          &
                            + be *( f(i,j+2)-f(i,j-2) )                          &
                            + ce *( f(i,j+3)-f(i,j-3) )                          &
                            + de *( f(i,j+4)-f(i,j-4) )
               end do
            end do

       ! be sure that we have finished communicating the "ghost" points
         if( lnbr(1) >= 0 .or. lnbr(2) >= 0 ) then
            call MPI_Waitall(4,req,stat,ierr)
         endif
!
!  Internal nodes: edges
!
         if (nbr(1).ge.0) then
               do i = 1, m1
                  df2(i,1) = ae *( f(i,2)-neg_f(i,4) )                          &
                            + be *( f(i,3)-neg_f(i,3) )                          &
                            + ce *( f(i,4)-neg_f(i,2) )                          &
                            + de *( f(i,5)-neg_f(i,1) )
                  df2(i,2) = ae *( f(i,3)-    f(i,1) )                          &
                            + be *( f(i,4)-neg_f(i,4) )                          &
                            + ce *( f(i,5)-neg_f(i,3) )                          &
                            + de *( f(i,6)-neg_f(i,2) )
                  df2(i,3) = ae *( f(i,4)-    f(i,2) )                          &
                            + be *( f(i,5)-    f(i,1) )                          &
                            + ce *( f(i,6)-neg_f(i,4) )                          &
                            + de *( f(i,7)-neg_f(i,3) )
                  df2(i,4) = ae *( f(i,5)-    f(i,3) )                          &
                            + be *( f(i,6)-    f(i,2) )                          &
                            + ce *( f(i,7)-    f(i,1) )                          &
                            + de *( f(i,8)-neg_f(i,4) )
               end do
         end if

         if (nbr(2).ge.0) then
               do i = 1, m1
                  df2(i,m2  ) = ae *( pos_f(i,1)   -f(i,m2-1) )                 &
                               + be *( pos_f(i,2)   -f(i,m2-2) )                 &
                               + ce *( pos_f(i,3)   -f(i,m2-3) )                 &
                               + de *( pos_f(i,4)   -f(i,m2-4) )
                  df2(i,m2-1) = ae *(     f(i,m2)  -f(i,m2-2) )                 &
                               + be *( pos_f(i,1)   -f(i,m2-3) )                 &
                               + ce *( pos_f(i,2)   -f(i,m2-4) )                 &
                               + de *( pos_f(i,3)   -f(i,m2-5) )
                  df2(i,m2-2) = ae *(     f(i,m2-1)-f(i,m2-3) )                 &
                               + be *(     f(i,m2)  -f(i,m2-4) )                 &
                               + ce *( pos_f(i,1)   -f(i,m2-5) )                 &
                               + de *( pos_f(i,2)   -f(i,m2-6) )
                  df2(i,m2-3) = ae *(     f(i,m2-2)-f(i,m2-4) )                 &
                               + be *(     f(i,m2-1)-f(i,m2-5) )                 &
                               + ce *(     f(i,m2)  -f(i,m2-6) )                 &
                               + de *( pos_f(i,1)   -f(i,m2-7) )
               end do
         end if
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_dir .eq. 0 ) .and. ( periodic_dir .eq. 0 )) then

               if (nbr(1).lt.0) then
                     do i = 1, m1

                        df2(i,   1) = (-11.*f(i,   1)+18.*f(i,   2)             &
                                        - 9.*f(i,   3)+ 2.*f(i,   4)             &
                                       )/ 6. *ds

                        df2(i,   2) = (- 2.*f(i,   1)- 3.*f(i,   2)             &
                                        + 6.*f(i,   3)- 1.*f(i,   4)             &
                                       )/ 6. *ds

                        df2(i,   3) = (+ 1.*f(i,   1)- 8.*f(i,   2)             &
                                        + 8.*f(i,   4)-  1.*f(i,  5)             &
                                       )/12.*ds

                        df2(i,   4) = (- 1.*f(i,   1)+  9.*f(i,  2)             &
                                        -45.*f(i,   3)+ 45.*f(i,  5)             &
                                        - 9.*f(i,   6)+  1.*f(i,  7)             &
                                       )/60.*ds
                     end do
               end if
               if (nbr(2).lt.0) then
                     do i = 1, m1
                        df2(i,m2-3) =(+ 1.*f(i,m2  )-  9.*f(i,m2-1)             &
                                       +45.*f(i,m2-2)- 45.*f(i,m2-4)             &
                                       + 9.*f(i,m2-5)-  1.*f(i,m2-6)             &
                                      )/60.*ds

                        df2(i,m2-2) =(- 1.*f(i,m2  )+  8.*f(i,m2-1)             &
                                       - 8.*f(i,m2-3)+  1.*f(i,m2-4)             &
                                      )/12.*ds

                        df2(i,m2-1) =(+ 2.*f(i,m2  )+  3.*f(i,m2-1)             &
                                       - 6.*f(i,m2-2)+  1.*f(i,m2-3)             &
                                      )/ 6. *ds

                        df2(i,m2  ) =(+11.*f(i,m2  )- 18.*f(i,m2-1)             &
                                       + 9.*f(i,m2-2)-  2.*f(i,m2-3)             &
                                      )/ 6. *ds
                     end do
               end if

            elseif (( isym_dir .eq. 0 ) .and. ( periodic_dir .eq. 1 )) then

               if (nbr(1).lt.0) then
                     do i = 1, m1
                        df2(i,   1) = ae *( f(i   ,2) - neg_f(i,4))             &
                                     + be *( f(i   ,3) - neg_f(i,3))             &
                                     + ce *( f(i   ,4) - neg_f(i,2))             &
                                     + de *( f(i   ,5) - neg_f(i,1))

                        df2(i,   2) = ae *( f(i   ,3) -     f(i,1))             &
                                     + be *( f(i   ,4) - neg_f(i,4))             &
                                     + ce *( f(i   ,5) - neg_f(i,3))             &
                                     + de *( f(i   ,6) - neg_f(i,2))

                        df2(i,   3) = ae *( f(i   ,4) -     f(i,2))             &
                                     + be *( f(i   ,5) -     f(i,1))             &
                                     + ce *( f(i   ,6) - neg_f(i,4))             &
                                     + de *( f(i   ,7) - neg_f(i,3))

                        df2(i,   4) = ae *( f(i   ,5) -     f(i,3))             &
                                     + be *( f(i   ,6) -     f(i,2))             &
                                     + ce *( f(i   ,7) -     f(i,1))             &
                                     + de *( f(i   ,8) - neg_f(i,4))
                     end do
               end if
               if (nbr(2).lt.0) then
                     do i = 1, m1
                        df2(i,m2-3) = ae*(     f(i,m2-2)-f(i,m2-4))             &
                                     + be*(     f(i,m2-1)-f(i,m2-5))             &
                                     + ce*(     f(i,m2  )-f(i,m2-6))             &
                                     + de*( pos_f(i,   1)-f(i,m2-7))

                        df2(i,m2-2) = ae*(     f(i,m2-1)-f(i,m2-3))             &
                                     + be*(     f(i,m2  )-f(i,m2-4))             &
                                     + ce*( pos_f(i,   1)-f(i,m2-5))             &
                                     + de*( pos_f(i,   2)-f(i,m2-6))

                        df2(i,m2-1) = ae*(     f(i,m2  )-f(i,m2-2))             &
                                     + be*( pos_f(i,   1)-f(i,m2-3))             &
                                     + ce*( pos_f(i,   2)-f(i,m2-4))             &
                                     + de*( pos_f(i,   3)-f(i,m2-5))

                        df2(i,m2  ) = ae*( pos_f(i,   1)-f(i,m2-1))             &
                                     + be*( pos_f(i,   2)-f(i,m2-2))             &
                                     + ce*( pos_f(i,   3)-f(i,m2-3))             &
                                     + de*( pos_f(i,   4)-f(i,m2-4))
                     end do
               end if

            elseif ( isym_dir .eq. 1 ) then

               if (nbr(1).lt.0) then
                     do i = 1, m1
                        df2(i,  1) =  ae*( f(i,2) - n_sym*f(i,2) )              &
                                    +  be*( f(i,3) - n_sym*f(i,3) )              &
                                    +  ce*( f(i,4) - n_sym*f(i,4) )              &
                                    +  de*( f(i,5) - n_sym*f(i,5) )

                        df2(i,  2) =  ae*( f(i,3) -       f(i,1) )              &
                                    +  be*( f(i,4) - n_sym*f(i,2) )              &
                                    +  ce*( f(i,5) - n_sym*f(i,3) )              &
                                    +  de*( f(i,6) - n_sym*f(i,4) )

                        df2(i,  3) =  ae*( f(i,4) -       f(i,2) )              &
                                    +  be*( f(i,5) -       f(i,1) )              &
                                    +  ce*( f(i,6) - n_sym*f(i,2) )              &
                                    +  de*( f(i,7) - n_sym*f(i,3) )

                        df2(i,  4) =  ae*( f(i,5) -       f(i,3) )              &
                                    +  be*( f(i,6) -       f(i,2) )              &
                                    +  ce*( f(i,7) -       f(i,1) )              &
                                    +  de*( f(i,8) - n_sym*f(i,2) )

                     end do
               end if
               if (nbr(2).lt.0) then
                     do i = 1, m1
                        df2(i,m2-3) = (+ 1.*f(i,m2  )- 9.*f(i,m2-1)             &
                                        +45.*f(i,m2-2)-45.*f(i,m2-4)             &
                                        + 9.*f(i,m2-5)- 1.*f(i,m2-6)             &
                                       )/60.*ds

                        df2(i,m2-2) = (- 1.*f(i,m2  )+ 8.*f(i,m2-1)             &
                                        - 8.*f(i,m2-3)+ 1.*f(i,m2-4)             &
                                       )/12.*ds

                        df2(i,m2-1) = (+ 2.*f(i,m2  )+ 3.*f(i,m2-1)             &
                                        - 6.*f(i,m2-2)+ 1.*f(i,m2-3)             &
                                       )/ 6.*ds

                        df2(i,m2  ) = (+11.*f(i,m2  )-18.*f(i,m2-1)             &
                                        + 9.*f(i,m2-2)- 2.*f(i,m2-3)             &
                                       )/ 6. *ds
                     end do
               end if
            end if
         else                   ! Unsupported IBOUND
            if ( plane_id.eq.0 ) then
               write(6,2) ibound
            end if
            call terminate_run(6,0)
         end if
      else            ! Unsupported IORDER
         if ( plane_id.eq.0 ) then
            write(6,1) iorder
 1          format('IORDER = ', i4,' is not supported')
         end if
         call terminate_run(6,0)
      end if


      if(unif_grid_dir.ne.1) then
            do j = 1, m2
               do i = 1, m1
                  df2(i,j) = scale_1(j)*df2(i,j)
               enddo
            enddo
      endif
!-----------------------------------------------------------------------------------------
  return
  end

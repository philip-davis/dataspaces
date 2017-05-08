#include "globalDefines.h"
!$Id: deriv_inplane_1.f90,v 1.1.1.1.12.1 2006/08/22 20:47:14 rsankar Exp $
!=========================================================================================
  subroutine deriv_inplane_1(f,df1,dir,n1,n2,n_sym)
!=========================================================================================
! evaluates the first derivative in the first dimension direction 
! using explicit differencing
!
! Author: Evatt Hawkes NOV 2004
!
!
!  f      - Function to be differentiated
!  df1    - Differentiated Function in first varying dimension of f
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
  real, intent(out), dimension(n1,n2) :: df1
  integer, intent(in) :: dir
  integer, intent(in) :: n_sym

!-----------------------------------------------------------------------------------------
! local declarations

  integer :: nm, i, j, k

  integer, dimension(2) :: lnbr, nbr
  integer, dimension(4) :: req=MPI_REQUEST_NULL
  integer, dimension(MPI_Status_size,4) :: stat
  integer :: plane_1rows_type
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

    scale_1 => scale_1y

    pos_f => pos_f_y_yz
    neg_f => neg_f_y_yz

    pos_fs => pos_fs_y_yz
    neg_fs => neg_fs_y_yz

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

    plane_id = yz_id
    plane_comm = yz_comm
    plane_1rows_type = yz_yrows_type

    if( vary_in_y .eq. 0 ) then
       df1 = 0.0
       return
    endif

    unif_grid_dir = unif_grid_y
    periodic_dir = periodic_y
    isym_dir = isym_y

  case(2)

    m1 = nx
    m2 = nz
    m1_g = nx_g
    m2_g = nz_g

    scale_1 => scale_1x

    pos_f => pos_f_x_xz
    neg_f => neg_f_x_xz

    pos_fs => pos_fs_x_xz
    neg_fs => neg_fs_x_xz

    lnbr(1) = neighbor(1)
    lnbr(2) = neighbor(2)
    nbr(:)=lnbr(:)
    if(periodic_x.eq.1) then
      if(xid.eq.0) then
        lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
      endif
      if(xid.eq.xpes-1)then
        lnbr(2) = zid*xpes*ypes+ yid*xpes+0
      endif
    endif

    plane_id = xz_id
    plane_comm = xz_comm
    plane_1rows_type = xz_xrows_type

    if( vary_in_x .eq. 0 ) then
       df1 = 0.0
       return
    endif

    unif_grid_dir = unif_grid_x
    periodic_dir = periodic_x
    isym_dir = isym_x

  case(3)

    m1 = nx
    m2 = ny
    m1_g = nx_g
    m2_g = ny_g

    scale_1 => scale_1x

    pos_f => pos_f_x_xy
    neg_f => neg_f_x_xy

    pos_fs => pos_fs_x_xy
    neg_fs => neg_fs_x_xy

    lnbr(1) = neighbor(1)
    lnbr(2) = neighbor(2)
    nbr(:)=lnbr(:)
    if(periodic_x.eq.1) then
      if(xid.eq.0) then
        lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
      endif
      if(xid.eq.xpes-1)then
        lnbr(2) = zid*xpes*ypes+ yid*xpes+0
      endif
    endif

    plane_id = xy_id
    plane_comm = xy_comm
    plane_1rows_type = xy_xrows_type

    if( vary_in_x .eq. 0 ) then
       df1 = 0.0
       return
    endif

    unif_grid_dir = unif_grid_x
    periodic_dir = periodic_x
    isym_dir = isym_x

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
    ds = real(m1_g-1)
  else
    ds = real(m1_g-1) * scale_1(1)
  endif
!-----------------------------------------------------------------------------------------
! get neighboring cells data

  if(lnbr(1)>=0) then
   ! get ghost cells from neighbor on (-) side
     call MPI_IRecv(neg_f,(m2*iorder/2),MPI_REAL8,lnbr(1),1,gcomm,req(1),ierr)
   ! send ghost cells to neighbor on (-) side
     pos_fs(:,:) = f(1:iorder/2,:)
     call MPI_ISend(pos_fs(1,1),(m2*iorder/2),MPI_REAL8,lnbr(1),2,gcomm,req(2),ierr)
   !  call MPI_ISend(f(1,1),1,plane_1rows_type,lnbr(1),2,gcomm,req(2),ierr)
  endif
  
  if(lnbr(2)>=0) then
   ! get ghost cells from neighbor on (+) side
     call MPI_IRecv(pos_f,(m2*iorder/2),MPI_REAL8,lnbr(2),2,gcomm,req(3),ierr)
   ! send ghost cells to neighbor on (+) side
     nm = m1 + 1 - iorder/2
     neg_fs(:,:) = f(nm:m1,:)
     call MPI_ISend(neg_fs(1,1),(m2*iorder/2),MPI_REAL8,lnbr(2),1,gcomm,req(4),ierr)
   !  call MPI_ISend(f(nm,1),1,plane_1rows_type,lnbr(2),1,gcomm,req(4),ierr)
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

            do j = 1, m2
               do i = 4, m1-3
                  df1(i,j) = ae *( f(i+1,j)-f(i-1,j) )                          &
                          + be *( f(i+2,j)-f(i-2,j) )                          &
                          + ce *( f(i+3,j)-f(i-3,j) )
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
               do j = 1, m2
                  df1(1,j) = ae *( f(2,j)-neg_f(3,j) )                          &
                          + be *( f(3,j)-neg_f(2,j) )                          &
                          + ce *( f(4,j)-neg_f(1,j) )
                  df1(2,j) = ae *( f(3,j)-    f(1,j) )                          &
                          + be *( f(4,j)-neg_f(3,j) )                          &
                          + ce *( f(5,j)-neg_f(2,j) )
                  df1(3,j) = ae *( f(4,j)-    f(2,j) )                          &
                          + be *( f(5,j)-    f(1,j) )                          &
                          + ce *( f(6,j)-neg_f(3,j) )
               end do
         endif

         if (nbr(2).ge.0) then
               do j = 1, m2
                  df1(m1,j)   = ae *( pos_f(1,j)   -f(m1-1,j) )                 &
                             + be *( pos_f(2,j)   -f(m1-2,j) )                 &
                             + ce *( pos_f(3,j)   -f(m1-3,j) )
                  df1(m1-1,j) = ae *(     f(m1,j)  -f(m1-2,j) )                 &
                             + be *( pos_f(1,j)   -f(m1-3,j) )                 &
                             + ce *( pos_f(2,j)   -f(m1-4,j) )
                  df1(m1-2,j) = ae *(     f(m1-1,j)-f(m1-3,j) )                 &
                             + be *(     f(m1,j)  -f(m1-4,j) )                 &
                             + ce *( pos_f(1,j)   -f(m1-5,j) )
               end do
         endif
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_dir .eq. 0 ) .and. ( periodic_dir .eq. 0 )) then

               if (nbr(1).lt.0) then
                     do j = 1, m2

                        df1(1,j) = (-11.*f(1,j) + 18.*f(2,j)                    &
                                   - 9.*f(3,j) +  2.*f(4,j))/6.                &
                                * ds

                        df1(2,j) = (- 2.*f(1,j) -  3.*f(2,j)                    &
                                   + 6.*f(3,j) -  1.*f(4,j))/6.                &
                                * ds

                        df1(3,j) = (+ 1.*f(1,j) -  8.*f(2,j)                    &
                                   + 8.*f(4,j) -  1.*f(5,j))/12.               &
                                * ds
                     enddo
               endif
               if (nbr(2).lt.0) then
                     do j = 1, m2
                        df1(m1-2,j) = (- 1.*f(m1  ,j)+ 8.*f(m1-1,j)             &
                                        - 8.*f(m1-3,j)+ 1.*f(m1-4,j)             &
                                       )/12.*ds

                        df1(m1-1,j) = (+ 2.*f(m1  ,j)+ 3.*f(m1-1,j)             &
                                        - 6.*f(m1-2,j)+ 1.*f(m1-3,j)             &
                                       )/6. *ds

                        df1(m1  ,j) = (+11.*f(m1  ,j)-18.*f(m1-1,j)             &
                                        + 9.*f(m1-2,j)- 2.*f(m1-3,j)             &
                                        )/6. *ds

                     end do
               endif

            elseif (( isym_dir .eq. 0 ) .and. ( periodic_dir .eq. 1 )) then

               if (nbr(1).lt.0) then
                     do j = 1, m2

                        df1(   1,j) = ae *( f(2,j) - neg_f(3,j) )               &
                                   + be *( f(3,j) - neg_f(2,j) )               &
                                   + ce *( f(4,j) - neg_f(1,j) )

                        df1(   2,j) = ae *( f(3,j) -     f(1,j) )               &
                                   + be *( f(4,j) - neg_f(3,j) )               &
                                   + ce *( f(5,j) - neg_f(2,j) )

                        df1(   3,j) = ae *( f(4,j) -     f(2,j) )               &
                                   + be *( f(5,j) -     f(1,j) )               &
                                   + ce *( f(6,j) - neg_f(3,j) )
                     end do
               end if
               if (nbr(2).lt.0) then
                     do j = 1, m2
                        df1(m1-2,j) =ae *(     f(m1-1,j)-f(m1-3,j))             &
                                   +be *(     f(m1  ,j)-f(m1-4,j))             &
                                   +ce *( pos_f(1   ,j)-f(m1-5,j))

                        df1(m1-1,j) =ae *(     f(m1  ,j)-f(m1-2,j))             &
                                   +be *( pos_f(1   ,j)-f(m1-3,j))             &
                                   +ce *( pos_f(2   ,j)-f(m1-4,j))

                        df1(m1  ,j) =ae *( pos_f(1   ,j)-f(m1-1,j))             &
                                   +be *( pos_f(2   ,j)-f(m1-2,j))             &
                                   +ce *( pos_f(3   ,j)-f(m1-3,j))

                     end do
               end if

            elseif ( isym_dir .eq. 1 ) then

               if (nbr(1).lt.0) then
                     do j = 1, m2

                        df1(1,j) = ae*( f(2   ,j)-n_sym*f(2   ,j) )             &
                                + be*( f(3   ,j)-n_sym*f(3   ,j) )             &
                                + ce*( f(4   ,j)-n_sym*f(4   ,j) )

                        df1(2,j) = ae*( f(3   ,j)-      f(1   ,j) )             &
                                + be*( f(4   ,j)-n_sym*f(2   ,j) )             &
                                + ce*( f(5   ,j)-n_sym*f(3   ,j) )

                        df1(3,j) = ae*( f(4   ,j)-      f(2   ,j) )             &
                                + be*( f(5   ,j)-      f(1   ,j) )             &
                                + ce*( f(6   ,j)-n_sym*f(2   ,j) )
                     end do
               end if
               if (nbr(2).lt.0) then
                     do j = 1, m2
                        df1(m1  ,j)=(+11.*f(m1  ,j)-18.*f(m1-1,j)               &
                                      + 9.*f(m1-2,j)- 2.*f(m1-3,j))              &
                                      /6. * ds

                        df1(m1-1,j)=(+ 2.*f(m1  ,j)+ 3.*f(m1-1,j)               &
                                      - 6.*f(m1-2,j)+ 1.*f(m1-3,j))              &
                                      /6. * ds

                        df1(m1-2,j)=(- 1.*f(m1  ,j)+ 8.*f(m1-1,j)               &
                                      - 8.*f(m1-3,j)+ 1.*f(m1-4,j))              &
                                      /12.* ds
                     end do
               end if

            end if
         else                   ! Unsupported IBOUND
            if ( plane_id.eq.0 ) then
               write(6,2) ibound
2              format('IBOUND = ', i4,' is not supported')
            end if
            call terminate_run(6,0)
         end if         ! if ( ibound ... )
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
            do j = 1, m2
               do i = 5, m1-4
                  df1(i,j) = ae *( f(i+1,j)-f(i-1,j) )                          &
                          + be *( f(i+2,j)-f(i-2,j) )                          &
                          + ce *( f(i+3,j)-f(i-3,j) )                          &
                          + de *( f(i+4,j)-f(i-4,j) )
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
               do j = 1, m2
                  df1(1,j) = ae *( f(2,j)-neg_f(4,j) )                          &
                            + be *( f(3,j)-neg_f(3,j) )                          &
                            + ce *( f(4,j)-neg_f(2,j) )                          &
                            + de *( f(5,j)-neg_f(1,j) )
                  df1(2,j) = ae *( f(3,j)-    f(1,j) )                          &
                            + be *( f(4,j)-neg_f(4,j) )                          &
                            + ce *( f(5,j)-neg_f(3,j) )                          &
                            + de *( f(6,j)-neg_f(2,j) )
                  df1(3,j) = ae *( f(4,j)-    f(2,j) )                          &
                            + be *( f(5,j)-    f(1,j) )                          &
                            + ce *( f(6,j)-neg_f(4,j) )                          &
                            + de *( f(7,j)-neg_f(3,j) )
                  df1(4,j) = ae *( f(5,j)-    f(3,j) )                          &
                            + be *( f(6,j)-    f(2,j) )                          &
                            + ce *( f(7,j)-    f(1,j) )                          &
                            + de *( f(8,j)-neg_f(4,j) )
               end do
         end if

         if (nbr(2).ge.0) then
               do j = 1, m2
                  df1(m1,  j) = ae *( pos_f(1,j)   -f(m1-1,j) )                 &
                               + be *( pos_f(2,j)   -f(m1-2,j) )                 &
                               + ce *( pos_f(3,j)   -f(m1-3,j) )                 &
                               + de *( pos_f(4,j)   -f(m1-4,j) )
                  df1(m1-1,j) = ae *(     f(m1,j)  -f(m1-2,j) )                 &
                               + be *( pos_f(1,j)   -f(m1-3,j) )                 &
                               + ce *( pos_f(2,j)   -f(m1-4,j) )                 &
                               + de *( pos_f(3,j)   -f(m1-5,j) )
                  df1(m1-2,j) = ae *(     f(m1-1,j)-f(m1-3,j) )                 &
                               + be *(     f(m1,j)  -f(m1-4,j) )                 &
                               + ce *( pos_f(1,j)   -f(m1-5,j) )                 &
                               + de *( pos_f(2,j)   -f(m1-6,j) )
                  df1(m1-3,j) = ae *(     f(m1-2,j)-f(m1-4,j) )                 &
                               + be *(     f(m1-1,j)-f(m1-5,j) )                 &
                               + ce *(     f(m1,j)  -f(m1-6,j) )                 &
                               + de *( pos_f(1,j)   -f(m1-7,j) )
               end do
         end if
!
!  Boundary nodes:
!
         if ( ibound .eq. 3 ) then

            if (( isym_dir .eq. 0 ) .and. ( periodic_dir .eq. 0 )) then

               if (nbr(1).lt.0) then
                     do j = 1, m2
                        df1(1   ,j)=(-11.*f(1   ,j) + 18.*f(2   ,j)             &
                                      - 9.*f(3   ,j) +  2.*f(4   ,j)             &
                                     )/6. *ds

                        df1(2   ,j)=(- 2.*f(1   ,j) -  3.*f(2   ,j)             &
                                      + 6.*f(3   ,j) -  1.*f(4   ,j)             &
                                     )/6. *ds

                        df1(3   ,j)=(+ 1.*f(1   ,j) -  8.*f(2   ,j)             &
                                      + 8.*f(4   ,j) -  1.*f(5   ,j)             &
                                     )/12.*ds

                        df1(4   ,j)=(- 1.*f(1   ,j) +  9.*f(2   ,j)             &
                                      -45.*f(3   ,j) + 45.*f(5   ,j)             &
                                      - 9.*f(6   ,j) +  1.*f(7   ,j)             &
                                     )/60.*ds
                     end do
               end if
               if (nbr(2).lt.0) then
                     do j = 1, m2
                        df1(m1-3,j)=(+ 1.*f(m1  ,j) -  9.*f(m1-1,j)             &
                                      +45.*f(m1-2,j) - 45.*f(m1-4,j)             &
                                      + 9.*f(m1-5,j) -  1.*f(m1-6,j)             &
                                     )/60.*ds

                        df1(m1-2,j)=(- 1.*f(m1  ,j) +  8.*f(m1-1,j)             &
                                      - 8.*f(m1-3,j) +  1.*f(m1-4,j)             &
                                     )/12.*ds

                        df1(m1-1,j)=(+ 2.*f(m1  ,j) +  3.*f(m1-1,j)             &
                                      - 6.*f(m1-2,j) +  1.*f(m1-3,j)             &
                                     )/6. *ds

                        df1(m1  ,j)=(+11.*f(m1  ,j) - 18.*f(m1-1,j)             &
                                      + 9.*f(m1-2,j) -  2.*f(m1-3,j)             &
                                     )/6. *ds
                     end do
               end if

            elseif (( isym_dir .eq. 0 ) .and. ( periodic_dir .eq. 1 )) then

               if (nbr(1).lt.0) then
                     do j = 1, m2

                        df1(   1,j) = ae *( f(2   ,j) - neg_f(4,j))             &
                                     + be *( f(3   ,j) - neg_f(3,j))             &
                                     + ce *( f(4   ,j) - neg_f(2,j))             &
                                     + de *( f(5   ,j) - neg_f(1,j))

                        df1(   2,j) = ae *( f(3   ,j) -     f(1,j))             &
                                     + be *( f(4   ,j) - neg_f(4,j))             &
                                     + ce *( f(5   ,j) - neg_f(3,j))             &
                                     + de *( f(6   ,j) - neg_f(2,j))

                        df1(   3,j) = ae *( f(4   ,j) -     f(2,j))             &
                                     + be *( f(5   ,j) -     f(1,j))             &
                                     + ce *( f(6   ,j) - neg_f(4,j))             &
                                     + de *( f(7   ,j) - neg_f(3,j))

                        df1(   4,j) = ae *( f(5   ,j) -     f(3,j))             &
                                     + be *( f(6   ,j) -     f(2,j))             &
                                     + ce *( f(7   ,j) -     f(1,j))             &
                                     + de *( f(8   ,j) - neg_f(4,j))
                     end do
               end if
               if (nbr(2).lt.0) then
                     do j = 1, m2
                        df1(m1-3,j) = ae *(    f(m1-2,j)-f(m1-4,j))             &
                                     + be *(    f(m1-1,j)-f(m1-5,j))             &
                                     + ce *(    f(m1  ,j)-f(m1-6,j))             &
                                     + de *(pos_f(1   ,j)-f(m1-7,j))

                        df1(m1-2,j) = ae *(    f(m1-1,j)-f(m1-3,j))             &
                                     + be *(    f(m1  ,j)-f(m1-4,j))             &
                                     + ce *(pos_f(1   ,j)-f(m1-5,j))             &
                                     + de *(pos_f(2   ,j)-f(m1-6,j))

                        df1(m1-1,j) = ae *(    f(m1  ,j)-f(m1-2,j))             &
                                     + be *(pos_f(1   ,j)-f(m1-3,j))             &
                                     + ce *(pos_f(2   ,j)-f(m1-4,j))             &
                                     + de *(pos_f(3   ,j)-f(m1-5,j))

                        df1(m1  ,j) = ae *(pos_f(1   ,j)-f(m1-1,j))             &
                                     + be *(pos_f(2   ,j)-f(m1-2,j))             &
                                     + ce *(pos_f(3   ,j)-f(m1-3,j))             &
                                     + de *(pos_f(4   ,j)-f(m1-4,j))
                     end do
               end if

            elseif ( isym_dir .eq. 1 ) then

               if (nbr(1).lt.0) then
                     do j = 1, m2

                        df1(1,j) = ae*( f(2,j) - n_sym*f(2   ,j) )              &
                                  + be*( f(3,j) - n_sym*f(3   ,j) )              &
                                  + ce*( f(4,j) - n_sym*f(4   ,j) )              &
                                  + de*( f(5,j) - n_sym*f(5   ,j) )

                        df1(2,j) = ae*( f(3,j) -       f(1   ,j) )              &
                                  + be*( f(4,j) - n_sym*f(2   ,j) )              &
                                  + ce*( f(5,j) - n_sym*f(3   ,j) )              &
                                  + de*( f(6,j) - n_sym*f(4   ,j) )

                        df1(3,j) = ae*( f(4,j) -       f(2   ,j) )              &
                                  + be*( f(5,j) -       f(1   ,j) )              &
                                  + ce*( f(6,j) - n_sym*f(2   ,j) )              &
                                  + de*( f(7,j) - n_sym*f(3   ,j) )

                        df1(4,j) = ae*( f(5,j) -       f(3   ,j) )              &
                                  + be*( f(6,j) -       f(2   ,j) )              &
                                  + ce*( f(7,j) -       f(1   ,j) )              &
                                  + de*( f(8,j) - n_sym*f(2   ,j) )
                     end do
               end if
               if (nbr(2).lt.0) then
                     do j = 1, m2
                        df1(m1-3,j)=(+ 1.*f(m1  ,j) -  9.*f(m1-1,j)             &
                                      +45.*f(m1-2,j) - 45.*f(m1-4,j)             &
                                      + 9.*f(m1-5,j) -  1.*f(m1-6,j)             &
                                      )/60.*ds

                        df1(m1-2,j)=(- 1.*f(m1  ,j) +  8.*f(m1-1,j)             &
                                      - 8.*f(m1-3,j) +  1.*f(m1-4,j)             &
                                      )/12.*ds

                        df1(m1-1,j)=(+ 2.*f(m1  ,j) +  3.*f(m1-1,j)             &
                                      - 6.*f(m1-2,j) +  1.*f(m1-3,j)             &
                                      )/6. *ds

                        df1(m1  ,j)=(+11.*f(m1  ,j) - 18.*f(m1-1,j)             &
                                      + 9.*f(m1-2,j) -  2.*f(m1-3,j)             &
                                      )/6. *ds
                     end do
               end if
            end if
         else
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
                  df1(i,j) = scale_1(i)*df1(i,j)
               enddo
            enddo
      endif
!-----------------------------------------------------------------------------------------
  return
  end

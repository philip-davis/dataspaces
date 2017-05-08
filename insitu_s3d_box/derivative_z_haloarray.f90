!=========================================================================================
  subroutine derivative_z_calc_boxlib(mx,my,mz,f,df )
!=========================================================================================
! evaluates the first derivative in z-direction using explicit differencing
!
!  f      - Function to be differentiated
!  df     - Differentiated Function

!  ae     - Explicit central difference stencil coefficient at (k +/- 1)
!  be     - Explicit central difference stencil coefficient at (k +/- 2)
!  ce     - Explicit central difference stencil coefficient at (k +/- 3)
!  de     - Explicit central difference stencil coefficient at (k +/- 4)

!  mx     - number of grid points in x-direction
!  my     - number of grid points in y-direction
!  mz     - number of grid points in z-direction
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : zmin, zmax

  use param_m, only : iorder
  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(out), dimension(mx,my,mz) :: df

  real, intent(in) :: f(1-iorder/2:mx+iorder/2, &
                        1-iorder/2:my+iorder/2, &
                        1-iorder/2:mz+iorder/2) 

  integer :: i, j, k
  integer, dimension(6) :: lnbr(6)

  real :: ds,ae,be,ce,de

!-----------------------------------------------------------------------------------------
! calculate ds for grid compression
  ds = real(mz*zpes-1) / (zmax-zmin)


  lnbr = neighbor

!-----------------------------------------------------------------------------------------
!  8th order: (3,3,4,6-8E-6,4,3,3)
!

         ae =  4./  5. *ds
         be = -1./  5. *ds
         ce =  4./105. *ds
         de = -1./280. *ds
!
!  Internal nodes:
!
         do k = 1, mz
            do j = 1, my
               do i = 1, mx
                  df(i,j,k) = ae *( f(i,j,k+1)-f(i,j,k-1) )                          &
                       + be *( f(i,j,k+2)-f(i,j,k-2) )                               &
                       + ce *( f(i,j,k+3)-f(i,j,k-3) )                               &
                       + de *( f(i,j,k+4)-f(i,j,k-4) )
               end do
            end do
         end do

!
!  Boundary nodes:
!
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


  return
  end subroutine derivative_z_calc_boxlib

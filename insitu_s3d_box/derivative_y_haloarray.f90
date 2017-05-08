!=========================================================================================
  subroutine derivative_y_calc_boxlib(mx,my,mz,f,df )
!=========================================================================================
! evaluates the first derivative in y-direction using explicit differencing
!
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
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : ymin, ymax
  use param_m, only : periodic_y, vary_in_y, iorder

  implicit none
!-----------------------------------------------------------------------------------------
  integer, intent(in) :: mx, my, mz

  real, intent(in) :: f(1-iorder/2:mx+iorder/2, &
                        1-iorder/2:my+iorder/2, &
                        1-iorder/2:mz+iorder/2) 
  real, intent(out), dimension(mx,my,mz) :: df

  integer :: i, j, k
  integer, dimension(6) :: lnbr(6)
  real :: ds,ae,be,ce,de

!-----------------------------------------------------------------------------------------
  ds = real(my*ypes-1) / (ymax-ymin)


  lnbr = neighbor

!-----------------------------------------------------------------------------------------
!  8th order: (3,3,4,6-8-6,4,3,3)
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
                  df(i,j,k) = ae *( f(i,j+1,k)-f(i,j-1,k) )                          &
                            + be *( f(i,j+2,k)-f(i,j-2,k) )                          &
                            + ce *( f(i,j+3,k)-f(i,j-3,k) )                          &
                            + de *( f(i,j+4,k)-f(i,j-4,k) )
               end do
            end do
         end do



 !   Boundary nodes:
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


  return
  end subroutine derivative_y_calc_boxlib

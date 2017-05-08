!=========================================================================================
  subroutine derivative_x_calc_boxlib(mx,my,mz,f,df )
!=========================================================================================
! evaluates the first derivative in x-direction using explicit differencing
!
!  f      - Function to be differentiated - including halo data
!  df     - Differentiated Function

!  ae     - Explicit central difference stencil coefficient at (i +/- 1)
!  be     - Explicit central difference stencil coefficient at (i +/- 2)
!  ce     - Explicit central difference stencil coefficient at (i +/- 3)
!  de     - Explicit central difference stencil coefficient at (i +/- 4)

!  mx     - number of grid points in x-direction
!  my     - number of grid points in y-direction
!  mz     - number of grid points in z-direction
!
!-----------------------------------------------------------------------------------------
  use topology_m
  use grid_m, only : xmin, xmax
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

  ds = real(mx*xpes-1) / (xmax-xmin)

  lnbr = neighbor

!-----------------------------------------------------------------------------------------
!
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
                  df(i,j,k) = ae *( f(i+1,j,k)-f(i-1,j,k) )                          &
                            + be *( f(i+2,j,k)-f(i-2,j,k) )                          &
                            + ce *( f(i+3,j,k)-f(i-3,j,k) )                          &
                            + de *( f(i+4,j,k)-f(i-4,j,k) )
               end do
            end do
         end do



!  Boundary nodes:
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


!-----------------------------------------------------------------------------------------

  return
  end subroutine derivative_x_calc_boxlib

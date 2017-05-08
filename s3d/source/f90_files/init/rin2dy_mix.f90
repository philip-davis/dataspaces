!=========================================================================================
      subroutine rin2dy_mix(xi_k, mixfr )
!
!! RIN2DY initializes 2-D random scalar field.
!
      use param_m
      use topology_m

      implicit none
!
      complex fcmplx(nh,ny_g)
      real mixfr(nx,ny)
      complex xi_k(nh,nw)
      real urnd(nx,ny)

! Local variables:

      integer i, j, jj, l, ll, ione, nyt
      real pi
      real kx, kx0

! Declare temporary out array for Crays


      complex, allocatable, dimension(:) :: tmpout
      real, allocatable, dimension(:) :: table, work
!#if T3E
!      allocate (table(2*ny_g))
!      allocate (work(4*ny_g))
!#else
!#if SGI
!      allocate (table(2*(ny_g+15)))
!#else
!#if SP2 || CPQ || PC
      allocate (table(2*ny_g))
      allocate (work(2*ny_g))
!#endif
!#endif
!#endif

!
      pi = 4.0 * atan ( 1.0 )

      do j=1,ny,1
        do i=1,nx,1
          urnd(i,j)=0.0
        enddo
      enddo

      do j=1,ny_g,1
        do i=1,nh,1
          fcmplx(i,j)=0.0
        enddo
      enddo

      do i = 1, nh
        do j = 1, nw
          if ( j .le. nh ) then
            fcmplx(i,j) = xi_k(i,j)
          else
            fcmplx(i,ny_g+j-nw) = xi_k(i,j)
          end if
        end do
      end do

      ione = 1

#if T3E || SP2 || CPQ || PC
      allocate(tmpout(ny_g))
#endif

      call fft_cinit(ny_g,table)

      do i=1,nh
         call fft_complex( ione, ny_g, table, work,                                  &
                           fcmplx(i,:), tmpout)
      enddo

      deallocate(table)
#if T3E || SP2 || CPQ || PC
      deallocate(tmpout)
      deallocate(work)
#endif

      nyt = ny

      do l = 1, nx
        do j = 1, nyt

           jj = j + yid*ny
           ll = l + xid*nx

           kx0 = real ( nh - 1 ) * 2.0 * pi                                          &
               * real(ll-1)/real(nx_g)
           urnd(l,j)= real(fcmplx(1,jj)) + real(fcmplx(nh,jj))*cos(kx0)              &
                   - aimag(fcmplx(nh,jj)) * sin(kx0)

           do i = 2, nh-1

              kx = real ( i - 1 ) * 2.0 * pi                                         &
                 * real(ll-1)/real(nx_g)

              urnd(l,j)= urnd(l,j) + 2.0 * (real(fcmplx(i,jj))*cos(kx)               &
                                   - aimag(fcmplx(i,jj))*sin(kx))

           end do
        end do

      end do

      call MPI_Barrier(gcomm,ierr)

      do j=1,ny,1
        do i=1,nx,1
          mixfr(i,j)=urnd(i,j)
        enddo
      enddo

 3310 format(' nh, ny=',2i8)
 3400 format(1x,a,'[',i3,',',i3,']= (',1p,e11.3,',',e11.3,')')

      return
      end

#include "globalDefines.h"
subroutine rndini3d_temp( seed, ke, xim, xir, xig, io, mixfr)
!
! Set the 3-D mixture fraction field based on a specified energy spectrum.
!
! INPUT:
!   seed - integer scalar, seed for random number generator
!   ke   - real array(4),  first two entries are length scales (dimensionless)
!                          containing the dominant length scale for the fluctuations
!                          and the range of variations from this length scale
!   xim  - mean mixture fraction
!   xir  - rms of mixture fraction
!   xig - Amplitude of gaussian function superimposed
!   io   - I/O file unit
!
! OUTPUT:
!   mixfr - real, 2D array, mixture fraction
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     Revisions and comments:
!
!     1. The routine computes the initial mixture fraction field 
!        based on a user-prescribed spectrum (e.g. top hat, or from ODT).
!
!     2. The spectrum is specified inside a range Eps < k < Kc,
!        where Eps = is a very small number set to near zero (Ke / 1000, here).
!        Kc  = cut-off scale.
!        Outside this range the spectrum E(k) is set to zero.
!
!     3. The descrete wave numbers are based on all possible descrete periods along
!        a given direction ranging from 0 to (N-1)*2*pi/L.
!
!     4. Since the range L is not specified exactly to yield
!        a multiple of 2*pi, a truncation of the ratio L/(2*pi)
!        is carried out to avoid out-of-range computations.
!----------------------------------------------------------------------------------------

  use param_m
  use topology_m
  use espectrum_m
  use reference_m, only : a_ref
  use grid_m, only : xmin, xmax, ymin, ymax, zmin, zmax, x, y, z, scale_1x, scale_1y
!----------------------------------------------------------------------------------------  
  implicit none
!----------------------------------------------------------------------------------------  

  real, intent(inout), dimension(nx,ny,nz) :: mixfr

  integer seed
!----------------------------------------------------------------------------------------
!     Local variables

  real ke(4)
  real xim
  real xir
  real xig
  real kd
  real amp
  real c
  real e_spec, eps
  real kc, kx, ky
  real mag_k
  real ps 
  complex i_cmplx 
  integer i, j, k
  real xirs, xirs_sum
  real C_spect
  real :: upr = 0.5   !gbansal/ up is hard-wired here, change here if up changes !!
  real, parameter :: pi = 3.14159265358979
  integer, parameter :: ngauss = 4
  real :: sigmag ! Sigma for the gaussian function superimposed on mean
  real :: xc, yc ! center of the domain, where the gaussian peak sits
  real :: xos, yos
  real :: meang  ! Mean correction for gaussian function


!     External functions

  real  ran2
  external ran2

!  external ran_2
  
  integer io
!------------------------------- Executable Statements ----------------------------------
!-- set number of waves (nw) and nh
!-- initialization of turbulence_m module happens much later after this routine is called

  if (numdim == 3) then
! Evatt's change vv
     !nw=int(min(nx_g,ny_g,nz_g)/2)-1
     nw=int(min(nx_g,ny_g,nz_g))
  elseif(numdim == 2) then
     if( vary_in_x==1 .and. vary_in_y==1 ) then
        nw = int(min(nx_g,ny_g)/2)-1
     elseif (vary_in_x==1 .and. vary_in_z==1) then
        nw = int(min(nx_g,nz_g)/2)-1
     elseif (vary_in_y==1 .and. vary_in_z==1) then
        nw = int(min(ny_g,nz_g)/2)-1
     endif
  endif
  nh = 1 + nw / 2
!----------------------------------------------------------------------------------------
  ke(1) = 2. * pi / ke(1)
! Passot - pouquet spectrum constant in 3D
!  C_spect = 16.0 * sqrt(2.0/pi) * upr**2 / ke(1)
  C_spect = 16.0 * sqrt(2.0/pi) * xir**2 / ke(1)
!  if(myid .eq. 0) write(*,*) 'Mark 1'
!----------------------------------------------------------------------------------------
  call temp_3d(xmin,xmax,ymin,ymax,zmin,zmax,x,seed,mixfr,io,C_spect,ke)
!----------------------------------------------------------------------------------------
!  if(myid .eq. 0) write(*,*) 'Mark 2'
!----------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------
!   step 2: To add a gaussian function to the mean temperature 
!           compute the properties of that function
!  vv This needs to be MODIFIED for 3D - gbansal
!----------------------------------------------------------------------------------------
!  sigmag = 0.5*(xmax - xmin) / ngauss
!  xc = 0.5* ( xmax + xmin)
!  yc = 0.5* ( ymax + ymin)
!  meang = pi * xig / (2.0*ngauss*ngauss)
!  xirg = xig * (0.5*pi/ngauss) * sqrt( 1.0/pi - 1.0/(ngauss*ngauss) )

!  xirgs = 0.0
!  do i=1, nx
!    do j=1, ny
!       xos = ((x(i)-xc)/sigmag)**2
!       yos = ((y(j)-yc)/sigmag)**2
!       xirgs = xirgs + mixfr(i,j)*(xig * exp(-0.5*(xos+yos)) -meang)
!    end do
!  end do
!  call MPI_Barrier(gcomm, ierr)
!  call MPI_Allreduce(xirgs,xirgs_sum,1,MPI_REAL8,MPI_SUM,gcomm,ierr)

!----------------------------------------------------------------------------------------
!     step 3. compute the non-normalized mixture fraction rms and 
!     adjust the values of the mixture fraction including the mean

  xirs = 0.
  do i = 1, nx
     do j = 1, ny
       do k = 1, nz
        xirs = xirs + mixfr(i,j,k)*mixfr(i,j,k)
       end do
     end do
  enddo

  call MPI_Barrier(gcomm, ierr)

  call MPI_Allreduce(xirs,xirs_sum,1,MPI_REAL8,MPI_SUM,gcomm,ierr)


  call MPI_Barrier(gcomm, ierr)

!     xirs = sqrt(xirs/real(nx*ny))
  xirs = sqrt(xirs_sum / real(nx_g * ny_g * nz_g))
  xirs = xir / xirs

!--------------------- will eventually modify this portion ---------------------
!-------- a better way to impose the limits on xi should be implemented --------
  do i = 1, nx
     do j = 1, ny
       do k = 1, nz
        mixfr(i,j,k) = xim + xirs*mixfr(i,j,k)
!        mixfr(i,j,k) = min(0.9999999999,mixfr(i,j,k))
!        mixfr(i,j,k) = max(0.0000000001,mixfr(i,j,k))
!         mixfr(i,j) = 0.5 * (1+tanh((mixfr(i,j)-0.5)))
        end do
     end do
  enddo


  return
  end subroutine rndini3d_temp

!----------------------------------------------------------------------
! Scalar generation for 3D
subroutine temp_3d(xmin, xmax, ymin,ymax,zmin,zmax,xx,iseed,mixfr,io,C_spect,ke)
use param_m, only: nx, ny, nz, nx_g, ny_g, nz_g
use topology_m
implicit none
!----------------------------------------
real, intent(in) :: xmin, xmax, ymin, ymax, zmin, zmax, C_spect
real, intent(in) :: xx(nx) !dummy, not used.
integer, intent(in) :: iseed
real, intent(out) :: mixfr(nx, ny, nz)
integer, intent(in) :: io

!----------------------------------------
real, intent(in) :: ke(4)
real pi
complex :: xi_k(nx, ny, nz)
real ps1, ps2, psr
real delta, kc
real tke_l, tke
integer i, j, k, m
integer ir, jr, kr

real kx, ky, kz, k2, mag_k
real e_spec, amp
complex ak, bk
complex i_cmplx
integer, allocatable :: new_seed(:)
integer :: seed_size
real, allocatable, dimension(:) :: table, work
complex, allocatable, dimension(:) :: tmpout, q, qg
real rat_ke

!----------------------------------------
call random_seed(size=seed_size)
allocate(new_seed(seed_size))
new_seed(:) = iseed
call random_seed(put=new_seed)
!JET has a starting problem with random numbers
!do i = 1, 1e6
!  call random_number(psr)
!end do
!----------------------------------------

pi = 4.0*atan(1.0)
i_cmplx = (0.0, 1.0)

!----------------------------------------
delta = max( (xmax-xmin)/real(nx_g-1), &
             (ymax-ymin)/real(ny_g-1), &
             (zmax-zmin)/real(nz_g-1) )
kc = pi/delta
amp = sqrt((2.0*pi)**3/( (xmax-xmin)*(ymax-ymin)*(zmax-zmin) ) )
!----------------------------------------
xi_k(:,:,:) = 0.0
!if (myid .eq. 0) write(*,*) 'Mark b'
!----------------------------------------
loopk: do k = 1, nz_g
  loopj: do j = 1, ny_g
    !----------------------------------------
    !Do only half the i's due to conjugate symmetry
    loopi: do i = 1, 1+nx_g/2
      call random_number(psr); psr = 2.0*pi*(psr-0.5)
      call random_number(ps1); ps1 = 2.0*pi*(ps1-0.5)
      call random_number(ps2); ps2 = 2.0*pi*(ps2-0.5)
      !----------------------------------------
      ! Dont fill the origin. It will add a mean velocity.
      if (i.eq.1 .and. j.eq.1 .and. k.eq.1) cycle loopi
      !----------------------------------------
      ! Dont fill the pi-wavenumber.
      ! When the number of grid points is even, there is a point at `pi'
      ! Since -pi = pi, conjugate symmetry here needs logic. 
      ! I have not coded that logic here.
      ! This wave number corresponds to kc. I leave it at zero.
      ! Ok to directly cycle loopj and loopk here.
      if( 2*int(nx_g/2) == nx_g .and. i .eq. 1+nx_g/2) cycle loopi
      if( 2*int(ny_g/2) == ny_g .and. j .eq. 1+ny_g/2) cycle loopj
      if( 2*int(nz_g/2) == nz_g .and. k .eq. 1+nz_g/2) cycle loopk
      !----------------------------------------
      ! find out if this i, j, k belongs to this processor.
      ! If not dont do anything.
      if ( i.le.xid*nx .or. i.gt.(xid+1)*nx .or. &
           j.le.yid*ny .or. j.gt.(yid+1)*ny .or. &
           k.le.zid*nz .or. k.gt.(zid+1)*nz     ) cycle loopi
      !----------------------------------------
      ir = i - xid*nx
      jr = j - yid*ny
      kr = k - zid*nz
      !----------------------------------------
      kx = real(i-1)*2.0*pi/(xmax-xmin)
      if(j <= 1+ny_g/2) then
        ky = real(j-1)*2.0*pi/(ymax-ymin)
      else
        ky = -real(ny_g+1-j)*2.0*pi/(ymax-ymin)
      end if
      if(k <= 1+nz_g/2) then
        kz = real(k-1)*2.0*pi/(zmax-zmin)
      else
        kz = -real(nz_g+1-k)*2.0*pi/(zmax-zmin)
      end if
      k2 = sqrt(kx**2+ky**2)
      mag_k = sqrt(kx**2+ky**2+kz**2)
      !----------------------------------------
      ! Dont fill wave numbers larger than the cutoff
      if (mag_k .gt. kc) cycle loopi
      !----------------------------------------
      rat_ke = mag_k/ke(1)  ! ke is a module variable
      e_spec = C_spect*(rat_ke)**4 * exp (-2.0 * rat_ke**2)
      !ak = amp*sqrt(e_spec/(2.0*pi*mag_k**2))*exp(i_cmplx*ps1)*cos(psr)
      !bk = amp*sqrt(e_spec/(2.0*pi*mag_k**2))*exp(i_cmplx*ps2)*sin(psr)
      !----------------------------------------
      xi_k(ir,jr,kr) = amp*sqrt(e_spec/(4.0*pi*mag_k**2))*exp(i_cmplx*ps1)
! check the following assignment vv - gbansal
      !if(k2 .lt. ke(1)/1e4) then
      !  xi_k(ir,jr,kr) = (ak+bk)/sqrt(2.0)
      !else
      !  xi_k(ir,jr,kr) = (ak*mag_k*ky+bk*kx*kz)/(mag_k*k2)
      !endif
    end do loopi
  end do loopj
end do loopk

!if (myid .eq. 0) write(*,*) 'Mark c'
!----------------------------------------
! i = 1 plane needs special handling. 
if(xid .eq. 0) then
  call conjugate_yzplane(xi_k(1,:,:))
end if

!if (myid .eq. 0) write(*,*) 'Mark d'
!----------------------------------------
! Now do the FFTS
!----------------------------------------
! Allocate arrays
#if defined(ARCH_T3E)
if (myid .eq. 0) write(*,*) 'T3E'
  allocate (table(2*max(nx_g,ny_g,nz_g)))
  allocate (work (4*max(nx_g,ny_g,nz_g)))
#elif defined(ARCH_SGI)
if (myid .eq. 0) write(*,*) 'SGI'
  allocate (table(2*(max(nx_g,ny_g,nz_g)+15)))
#elif defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
if (myid .eq. 0) write(*,*) 'SP2 || OPTERON || PC'
  allocate (table(2*max(nx_g,ny_g,nz_g)))
  allocate (work (2*max(nx_g,ny_g,nz_g)))
#elif defined(ARCH_X1)
if (myid .eq. 0) write(*,*) 'X1'
  allocate (table(100+8*max(nx_g,ny_g,nz_g)))
  allocate (work(8*max(nx_g,ny_g,nz_g)))
#endif

#if defined(ARCH_T3E) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
if (myid .eq. 0) write(*,*) 'any arch'
  allocate(tmpout(max(nx_g,ny_g,nz_g)))
#endif
if(myid .eq. 0) then
 if(.not. allocated(table)) write(*,*) 'Not allocated Table'
 if(.not. allocated(tmpout)) write(*,*) 'Not allocated tmpout'
end if
allocate(q(max(nx,ny,nz)))
allocate(qg(max(nx_g,ny_g,nz_g)))

!if (myid .eq. 0) write(*,*) 'Mark e'
!----------------------------------------
! First in the y-direction
call fft_cinit(ny_g, table)
!if (myid .eq. 0) write(*,*) 'Mark ea'
do k = 1, nz_g
  do i = 1, 1+nx_g/2
    !----------------------------------------
    ! find out if this i, j, k belongs to this processor.
    ! If not dont do anything.
    if ( i.le.xid*nx .or. i.gt.(xid+1)*nx .or.  &
         k.le.zid*nz .or. k.gt.(zid+1)*nz     ) cycle
    ir = i - xid*nx
    kr = k - zid*nz
    q(1:ny) = xi_k(ir,1:ny,kr)
    call mpi_gather(q, 2*ny, MPI_REAL8, qg, 2*ny, MPI_REAL8, 0, ycomm, ierr)
    if(yid .eq. 0) call fft_complex(1, ny_g, table, work, qg, tmpout)
    call mpi_scatter(qg, 2*ny, MPI_REAL8, q, 2*ny, MPI_REAL8, 0, ycomm, ierr)
    xi_k(ir,1:ny,kr) = q(1:ny)
  end do
end do
!if (myid .eq. 0) write(*,*) 'Mark e1'

!----------------------------------------
! Next in the z-direction
call fft_cinit(nz_g, table)
do j = 1, ny_g
  do i = 1, 1+nx_g/2
    !----------------------------------------
    ! find out if this i, j, k belongs to this processor.
    ! If not dont do anything.
    if ( i.le.xid*nx .or. i.gt.(xid+1)*nx .or.  &
         j.le.yid*ny .or. j.gt.(yid+1)*ny     ) cycle
    ir = i - xid*nx
    jr = j - yid*ny
      q(1:nz) = xi_k(ir,jr,1:nz)
      call mpi_gather(q, 2*nz, MPI_REAL8, qg, 2*nz, MPI_REAL8, 0, zcomm, ierr)
      if(zid .eq. 0) call fft_complex(1, nz_g, table, work, qg, tmpout)
      call mpi_scatter(qg, 2*nz, MPI_REAL8, q, 2*nz, MPI_REAL8, 0, zcomm, ierr)
      xi_k(ir,jr,1:nz) = q(1:nz)
  end do
end do
!if (myid .eq. 0) write(*,*) 'Mark e2'

!----------------------------------------
! Last is the x-direction. Conjugate symmetry is done here.
call fft_cinit(nx_g, table)
do k = 1, nz_g
  do j = 1, ny_g
    !----------------------------------------
    ! find out if this i, j, k belongs to this processor.
    ! If not dont do anything.
    if ( j.le.yid*ny .or. j.gt.(yid+1)*ny .or.  &
         k.le.zid*nz .or. k.gt.(zid+1)*nz     ) cycle
    jr = j - yid*ny
    kr = k - zid*nz
      q(1:nx) = xi_k(1:nx,jr,kr)
      call mpi_gather(q, 2*nx, MPI_REAL8, qg, 2*nx, MPI_REAL8, 0, xcomm, ierr)
      if(xid .eq. 0) then
        do i = 2+nx_g/2, nx_g
          qg(i) = conjg(qg(nx_g+2-i))
        end do
        call fft_complex(1, nx_g, table, work, qg, tmpout)
      end if
      call mpi_scatter(qg, 2*nx, MPI_REAL8, q, 2*nx, MPI_REAL8, 0, xcomm, ierr)
      xi_k(1:nx,jr,kr) = q(1:nx)
  end do
end do

!if (myid .eq. 0) write(*,*) 'Mark e3'
!At this point, uk should be purely real.
mixfr = real(xi_k)

!if (myid .eq. 0) write(*,*) 'Mark f'
return
contains
!----------------------------------------
subroutine conjugate_yzplane(f)
implicit none
complex, intent(inout) :: f(ny, nz)

complex :: fg3(ny, nz, yz_pes), fg(ny_g, nz_g)
integer j, k

!Gather the whole array to yz_id=0. 
!Doing it without derived data type.
call mpi_gather(f, 2*ny*nz, MPI_REAL8, fg3, 2*ny*nz,MPI_REAL8,0,yz_comm,ierr)

if(yz_id .eq. 0) then
  do k = 0, zpes-1
    do j = 0, ypes-1
      fg(j*ny+1:(j+1)*ny, k*nz+1:(k+1)*nz) = fg3(1:ny,1:nz,k*ypes+j+1)
    end do
  end do
  !Apply conjugate symmetry
  do k = 2+nz_g/2, nz_g
    fg(1, k) = conjg(fg(1, nz_g+2-k))
    do j = 2, ny_g
      fg(j,k) = conjg(fg(ny_g+2-j,nz_g+2-k))
    end do
  end do
  !Treat the k=1 line
  do j = 2+ny_g/2, ny_g
    fg(j,1) = conjg(fg(ny_g+2-j,1))
  end do
  do k = 0, zpes-1
    do j = 0, ypes-1
      fg3(1:ny,1:nz,k*ypes+j+1) = fg(j*ny+1:(j+1)*ny, k*nz+1:(k+1)*nz)
    end do
  end do
end if

call mpi_scatter(fg3, 2*ny*nz, MPI_REAL8, f, 2*ny*nz,MPI_REAL8,0,yz_comm,ierr)

return
end subroutine conjugate_yzplane

end subroutine temp_3d
!----------------------------------------------------------------------






subroutine rndini2d_temp(seed, ke, xim, xir, xig, io, mixfr)
!
! Set the 2-D mixture fraction field based on a specified energy spectrum.
!
! INPUT:
!   seed - integer scalar, seed for random number generator
!   ke   - real array(4),  first two entries are length scales (dimensionless)
!                          containing the dominant length scale for the fluctuations
!                          and the range of variations from this length scale
!   xim  - mean mixture fraction
!   xir  - rms of mixture fraction
!   xig  - Amplitude of gaussian function superimposed
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
!  use ranseed_m
  use espectrum_m
  use reference_m, only : a_ref
  use grid_m, only : xmin, xmax, ymin, ymax, x, y, z, scale_1x, scale_1y

!----------------------------------------------------------------------------------------  
  implicit none
!----------------------------------------------------------------------------------------  

  real, intent(inout), dimension(nx,ny) :: mixfr
  complex, allocatable, dimension(:,:) :: xi_k

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
  integer i, j
  real xirs, xirs_sum, xirg, xirgs, xirgs_sum, detmt, cfct
  real :: upr = 0.5 !gbansal/up is hard-wired here, change here if up changes !!
  real:: pi 
  integer, parameter :: ngauss = 4
  real :: sigmag ! Sigma for the gaussian function superimposed on mean
  real :: xc, yc ! center of the domain, where the gaussian peak sits
  real :: xos, yos
  real :: meang  ! Mean correction for gaussian function

!     External functions

  real  ran2
  external ran2
  
  integer io

  pi = 4.0 * datan(1.0)
!------------------------------- Executable Statements ----------------------------------
!-- set number of waves (nw) and nh
!-- initialization of turbulence_m module happens much later after this routine is called

  if (numdim == 3) then
     nw=int(min(nx_g,ny_g,nz_g)/2)-1
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
!     step 1. generate mixture fraction field from specified spectra

  allocate( xi_k(nh,nw) )

  

! check dominant scale versus domain size

  if(myid.eq.0) then
     write(io,*)'mean - xim =',xim
     write(io,*)'RMS  - xir =',xir
     write(io,*)'most energetic length scale =',ke(1)
     if (ke(1).gt.(xmax-xmin)) then
        write(io,*)'domain too small for scales ...'
        write(io,*)'domain: ',xmax-xmin
        write(io,*)'dominant scale: ',ke(1)
     endif
  endif

! check nw

  if ( nw .gt. nym_g/2 ) then
     if ( myid.eq.0 ) then
        write(io,9350)
     end if
     stop
  end if
!----------------------------------------------------------------------------------------
! initialize field with homogeneous turbulence on top of laminar solution

  i_cmplx = ( 0.0, 1.0 )

!     Xmax-Xmin > -2 Ymin

!  kc = real (nh-1) * 2.0 * pi /max((xmax - xmin),(ymax-ymin))
   kc = real (nh-1) * 2.0 * pi /max((xmax - xmin),(ymax-ymin))

!     Truncation to 8 digits only
!     Kc = 1.E-8*int(Kc * 1.e08)

  amp = 2.0 * pi / sqrt ( (xmax-xmin)*(ymax-ymin) )

! initialize spectrum e_spec

!--------------------- will eventually modify this portion ---------------------
!     ke = real(nw-1) * 2. * pi / (xmax-xmin)
!     kd = kc


  ke(1) = 2. * pi / ke(1)
  ke(2) = 2. * pi / ke(2)
!  kd    = kc
   kd   = ke(2)
  
  if(myid.eq.0) then
     write(io,*) 'ke(1)=',ke(1)
     write(io,*) 'ke(2)=',ke(2)
     write(io,*) 'kd   =',kd
  endif

!     ke = 2.0 * pi / ke
!     kd = 2.0 * pi / kd
!------------------------ end of portion to be modified  ------------------------

!     Specify the cut-offs for the evaluation of spectrum
!     Eps means a `small number'

  eps = ke(1) / 10000.0
!gbansal/ for passot-pouquet spectrum:
  upr = upr/a_ref
  c = (32.0/3.0)*sqrt(2.0/pi)*upr*upr/ke(1)
  call initialize_rnd_seed(seed)
  do i = 1, nh
     do j = 1, nw
        ps = 2.0 * pi * ( ran2 () - 0.5 )
        kx = real ( i - 1 ) * 2. * pi / ( xmax - xmin )
        ky = real ( j - 1 ) * 2. * pi / ( ymax - ymin )
        if ( j .gt. nh ) then
           ky = - real(nw+1-j)*2. * pi / ( ymax - ymin )
        end if
        mag_k = sqrt ( kx**2 + ky**2 )
        xi_k(i,j) = 0.
        if ( eps .lt. mag_k .and. mag_k .lt. kc ) then
!           if (eps .lt. abs(kx) .and. eps .lt. abs(ky)) then
!             call function to evaluate mixture fraction fluctuation spectrum
              call e_spectrum( mag_k, e_spec, ke, kd )
              e_spec = c*e_spec
              xi_k(i,j) =  amp * sqrt( e_spec/(2.0*pi*mag_k) ) * exp ( i_cmplx * ps )
!            endif
        end if
     end do
  end do
  
  do j = nh+1, nw
     xi_k(1,j) = conjg ( xi_k(1,nw+2-j) )
  end do

  call rin2dy_mix ( xi_k, mixfr )

!----------------------------------------------------------------------------------------
!   step 2: To add a gaussian function to the mean temperature 
!           compute the properties of that function
!----------------------------------------------------------------------------------------
  sigmag = 0.5*(xmax - xmin) / ngauss
  xc = 0.5* ( xmax + xmin)
  yc = 0.5* ( ymax + ymin)
  meang = pi * xig / (2.0*ngauss*ngauss)
  xirg = xig * (0.5*pi/ngauss) * sqrt( 1.0/pi - 1.0/(ngauss*ngauss) )

  xirgs = 0.0
  do i=1, nx
    do j=1, ny
       xos = ((x(i)-xc)/sigmag)**2
       yos = ((y(j)-yc)/sigmag)**2
       xirgs = xirgs + mixfr(i,j)*(xig * exp(-0.5*(xos+yos)) -meang)
    end do
  end do
  call MPI_Barrier(gcomm, ierr)
  call MPI_Allreduce(xirgs,xirgs_sum,1,MPI_REAL8,MPI_SUM,gcomm,ierr)
  
!----------------------------------------------------------------------------------------
!     step 3. compute the non-normalized mixture fraction rms and 
!     adjust the values of the mixture fraction including the mean
!----------------------------------------------------------------------------------------

  xirs = 0.
  do i = 1, nx
     do j = 1, ny
        xirs = xirs + mixfr(i,j)*mixfr(i,j)
     end do
  enddo
  call MPI_Barrier(gcomm, ierr)
  call MPI_Allreduce(xirs,xirs_sum,1,MPI_REAL8,MPI_SUM,gcomm,ierr)

  detmt = xirgs_sum*xirgs_sum + nx_g*ny_g*xirs_sum*(xir*xir - xirg*xirg)
  if ( detmt .lt. 0.0) then
    cfct = 0.0
    if (myid == 0) write(io,*) 'Warning!! required variance cannot be obtained'
    if (myid == 0) write(io,*) 'Try reducing the amplitude of the gaussian function'
  else 
    cfct = (sqrt(detmt) - xirgs_sum)/xirs_sum
  end if

!--------------------- will eventually modify this portion ---------------------
!-------- a better way to impose the limits on xi should be implemented --------
!  xirs = 0.0
!  xig = 0.0
  do i = 1, nx
     do j = 1, ny
         xos = ((x(i)-xc)/sigmag)**2
         yos = ((y(j)-yc)/sigmag)**2
         mixfr(i,j) = xim + xig * exp(-0.5*(xos+yos)) -meang + cfct*mixfr(i,j)
     end do
  enddo

!----------------------------------------------------------------------------------------
!     formats

9350 format(/' error: wrong specification of nw')
!----------------------------------------------------------------------------------------

  return

end subroutine rndini2d_temp
!--------------------------------------------------------------------------------

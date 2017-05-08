#include "globalDefines.h"
! ==============================================================================
! Implementation of S.K's algorithm to determine dissipation element 
! thickness on slices of constant k
! Author: Ray Grout (2008)

! Based loosely on Chunsang's implementation
! Uses s3d_hdf_interface_m to output results in hdf5
! Writes integration lines in tecplot format
! ==============================================================================

subroutine chi_elements_process(io)

  use topology_m
  use param_m, only : nx, ny, nz, npx, npy,npz, n_spec
  use reference_m
  use grid_m
  use mixfrac_m
  use variables_m, only:  temp, yspecies
  use s3d_hdf_interface_m

  implicit none

  ! Variables passed in
  integer io

  ! For fixed size arrays - this is poor, as there's no bounds checking inside measure_scale
  integer, parameter :: nedgepointsmax = 50000

  ! Input to measure_scale
  real, dimension(nx, ny,nz,3) :: gradz
  real, dimension(nx) :: xgrid
  real, dimension(ny) :: ygrid
  real, dimension(nz) :: zgrid

  ! Output from measure_scale
  integer, dimension(nx, ny,nz)   :: segmented
  real, dimension(3, 2, nedgepointsmax)   :: element_edgepoints ! x/y/z, end, point xyz
  integer nedgepoints 
  integer, dimension(nedgepointsmax)   :: element_status 

  ! To collect results from edgepoints found on each process on one process for output
  integer  gnedgepoints(npx*npy*npz), displs(npx*npy*npz)
  real, dimension(3, 2, nedgepointsmax*npx*npy*npz)   :: global_element_edgepoints ! x/y/z, end, point xyz
  integer, dimension(nedgepointsmax*npx*npy*npz)   :: global_element_status

  ! Counters
  integer i, j, k
  integer ierror

  ! For half-width pdf calculation
  integer ::n, n_total, iloc
  real :: halflength
  integer, parameter :: nbins = 120
  real, parameter :: bin_start = 0.0, bin_del = 16e-4!0.0125
  real, dimension(0:nbins) :: bin_val
  real, dimension(nbins) :: layerLength_pdf


  ! For appending 'segmentation' to existing hdf5 file
  CHARACTER(LEN=20) :: groupname  ! Group name
  CHARACTER(LEN=20) :: fieldname  ! Field name
  CHARACTER(LEN=250) :: fielddesc  ! Very long field name
  CHARACTER(LEN=20) :: fieldunits  ! Field units
  CHARACTER(LEN=50) :: filename   ! File name
  logical :: overwrite



  ! ============================================================================================================================


  ! Get mixture fracion
  call specToMixfr(yspecies)
  write(*,*) 'Computed mixture fracion; min/max = ',minval(mixFrac), maxval(mixFrac)

  ! Differentiate mix frac field
  call derivative_x(nx,ny,nz,mixFrac,gradz(:,:,:,1),scale_1x,1)
  call derivative_y(nx,ny,nz,mixFrac,gradz(:,:,:,2),scale_1y,1)
  call derivative_z(nx,ny,nz,mixFrac,gradz(:,:,:,3),scale_1z,1)

  ! Send grid to measure_scale in mm
  xgrid = x*l_ref*1.0e3
  ygrid = y*l_ref*1.0e3
  zgrid = z*l_ref*1.0e3

  ! Number of points at end of integration paths between local max and nn% of local max
  nedgepoints = 0

  call measure_scale(io,nx,ny, nz, xgrid,ygrid, zgrid, gradz(:,:,:,:), segmented(:,:,:), element_edgepoints, nedgepoints, 0, element_status)


  call mpi_barrier(gcomm, ierr)
  if(myid ==0) write(*,*) ' measured field'

  ! Gather integration points on proc id 0
  CALL  MPI_GATHER(nedgepoints, 1, MPI_INTEGER, gnedgepoints, 1, MPI_INTEGER, 0, gcomm, ierror)

  if(myid==0) then
     displs = 0
     do i=1,npx*npy*npz-1
        displs(i+1) = sum(gnedgepoints(1:i))
     enddo
  endif

  CALL  MPI_GATHERV(element_edgepoints, nedgepoints*6, MPI_DOUBLE_PRECISION, global_element_edgepoints, gnedgepoints*6, DISPLS*6, MPI_DOUBLE_PRECISION, 0, gcomm, ierror)
  CALL  MPI_GATHERV(element_status, nedgepoints, MPI_INTEGER, global_element_status, gnedgepoints, DISPLS, MPI_INTEGER, 0, gcomm, ierror)

  ! FOR HDF5 OUTPUT ON SAME GRID AS INPUT ONLY
  filename = write_hdf5_param%filename 
  groupname = "/derived"
  fieldname = "Segmented"
  fielddesc = "Segmentation"
  fieldunits = ""
  overwrite = .false.
  CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, segmented*1.0d0, 1.0d0, ierror,overwrite)

  filename = write_hdf5_param%filename 
  groupname = "/derived"
  fieldname = "Seg_diss"
  fielddesc = "Dissipation for segmentation"
  fieldunits = ""
  overwrite = .false.
  CALL write_hdf_field( 93, filename, groupname, fieldname, fielddesc, fieldunits, gradz(:,:,:,2)*gradz(:,:,:,2)+gradz(:,:,:,3)*gradz(:,:,:,3), 1.0d0, ierror,overwrite)


  ! Output tecplot data to illustrate search lines and halfwidth pdf
  if(myid==0) then
     write(io,*) 'Writing ',gnedgepoints,'edge points from driver'

     open(unit=22,file='single_element_lines.tec',status='unknown',form='formatted')

     do i=1,sum(gnedgepoints)
        if(global_element_status(i) == 3 ) then
           write(22,*) 'GEOMETRY, T=LINE3D, C=RED, CS=GRID'
        else
           write(22,*) 'GEOMETRY, T=LINE3D, C=GREEN, CS=GRID'
        endif
        write(22,*) '1'
        write(22,*) '2'
        write(22,99) global_element_edgepoints(1,2,i), global_element_edgepoints(2,2,i) , global_element_edgepoints(3,2,i) 
        write(22,99) global_element_edgepoints(1,1,i), global_element_edgepoints(2,1,i), global_element_edgepoints(3,1,i) 
     enddo
     close(22)

     ! Compute half-width pdf
     do n = 0, nbins
        bin_val(n) = bin_start + bin_del*real(n)
     enddo

     n_total = 0
     layerLength_pdf = 0

     open(unit=23,file='short_lines.tec',status='unknown',form='formatted')
     do i = 1,sum(gnedgepoints)
        if( global_element_status(i) .ne. 3) then
           halflength = sqrt( sum(  ( global_element_edgepoints(:,2,i) - global_element_edgepoints(:,1,i) )**2 ) )
           n_total = n_total + 1
           iloc = int(halflength/bin_del) + 1
           if(iloc == 1 ) then
              write(23,*) 'GEOMETRY, T=LINE3D, C=GREEN, CS=GRID'
              write(23,*) '1'
              write(23,*) '2'
              write(23,99) global_element_edgepoints(1,2,i), global_element_edgepoints(2,2,i) , global_element_edgepoints(3,2,i) 
              write(23,99) global_element_edgepoints(1,1,i), global_element_edgepoints(2,1,i), global_element_edgepoints(3,1,i) 
           endif
           if (iloc .gt. nbins ) iloc = nbins ! Contaminates last bin!!!
           if (iloc .eq. 1 ) write(*,'1a,3e15.5') 'added point to last bin at',global_element_edgepoints(:,1,i)
           !write(*,*) 'halflength=',halflength, iloc, bin_del
           layerLength_pdf(iloc) = layerLength_pdf(iloc) + 1
        endif
     enddo
     close(23)
     ! normalize
     layerLength_pdf = layerLength_pdf / real(n_total)

     !  Writing tecplot file
     filename='domain_halfwidthgnup_pdf.tec'
     open(unit=23,file=trim(filename),status='unknown',form='formatted')

     do n = 1,nbins
        write(23,99) 0.5*(bin_val(n-1)+bin_val(n)), layerLength_pdf(n)
     enddo


  endif

1 format(' zone t="stationary", i=',i5,', j=',i5,', k=',i5,', f=block')
2 format('GEOMETRY, T=LINE, CS=GRID')
11 format(' GEOMETRY X=',1pe12.5,', Y=',1pe12.5,', Z=',1pe12.5,', T=CIRCLE, C=CYAN, FC=CYAN, CS=GRID')
99 format(10(1pe12.5,1x))
100 format(a9,1x,20(1pe12.5,1x))
999 format(10(i5,1x))

end subroutine chi_elements_process


subroutine measure_scale(io, nx, ny, nz, x, y, z, grad_mixFrac, segmented, edgepoints, nedgepoints, tecout, pointstatus)
  !  use collectpoints_m
  use grid_m
  use reference_m
  use topology_m
  use param_m
  implicit none

  ! Variables passed in / out
  integer, intent(in)         :: io
  integer nedgepoints
  integer           :: nx, ny, nz, mypx, mypy, mypz, offset(4)
  real, dimension(nx) :: x
  real, dimension(ny) :: y
  real, dimension(nz) :: z
  real, dimension(nx,ny,nz,3)    :: grad_mixFrac
  integer tecout

  integer, dimension(nx,ny,nz)   :: segmented
  real, dimension(3, 2, *)   :: edgepoints ! x/y/z, end, point xyz
  integer, dimension(*) :: pointstatus

  ! Control behaviour
  logical, parameter ::  search_in_3d = .true.

  ! Counters and such
  integer                     :: i,j,k
  integer ierror

  ! Scratch fields for calculation
  real, dimension(nx,ny,nz)      ::  chi
  integer, dimension(nx,ny,nz)   :: peakflag  
  real, dimension(nx,ny,nz,3)    :: grad_temp, grad_chi

  ! Local variables
  logical :: peakflag_l
  integer :: di, dj, radius, peaksearchradius, consearchlength, maxhalfwidth
  real :: cutoff, thresh, thermoDissRateloc
  real :: cos_temp_2D, sin_temp_2D, thermoDissRate_up, thermoDissRate_down
  integer nextrema, nends

  ! For newton search  
  integer newtcount, newterr, direction
  real, allocatable, dimension(:, :) :: s0, ds, locs
  real, allocatable, dimension(:) :: s, smax, ftarg, fval, err,err0, dfds, step, vals
  real :: globalerr
  logical, allocatable, dimension(:) :: abandonsearch
  real, parameter :: deltas = 0.0005!15.0e-2 ! Step size used to approx function derivative
  real xmaxdim, ymaxdim, zmaxdim, xmindim, ymindim, zmindim
  integer, dimension(:), allocatable :: get_pointstatus
  ! Not currently used
  real, dimension(nx,ny,nz)         :: thermoDissRate_2D, thermoDissRate_3D
  real, dimension(3,5000) :: locsdiag
  real, dimension(5000) :: valsdiag
  integer, dimension(5000) :: intscr
  ! ================================================================================
  grad_temp = grad_mixFrac ! Use mixture fraction gradient only

  do k= 1, nz
     do j = 1, ny 
        do i = 1, nx 
           if( search_in_3d ) then
              chi(i,j,k)               = (grad_mixFrac(i,j,k,1)**2+grad_mixFrac(i,j,k,2)**2+grad_mixFrac(i,j,k,3)**2)
           else
              chi(i,j,k)               = (grad_mixFrac(i,j,k,1)**2+grad_mixFrac(i,j,k,2)**2) ! 2d option
           endif
           thermoDissRate_3D(i,j,k) = (grad_temp(i,j,k,1)**2 + grad_temp(i,j,k,2)**2 + grad_temp(i,j,k,3)**2)
           thermoDissRate_2D(i,j,k) = (grad_temp(i,j,k,1)**2 + grad_temp(i,j,k,2)**2)
        enddo
     enddo
  end do

  !  find peaks - planewise
  peaksearchradius = 1
  consearchlength = 15
  maxhalfwidth = 50

  peakflag = 0


  !  need to define appropriate threshold value for dissipation rate
  !  set this as a percentage of domain maximum
  CALL MPI_Allreduce(maxval(chi), thresh, 1, MPI_DOUBLE_PRECISION, MPI_MAX, gcomm, ierr) 
  write(*,*) 'maxval chi=',thresh
  !thresh = maxval(chi)
  thresh = thresh *0.5!*0.2! 0.4d0 !* 0.45d0

  do k=1,nz
     do j = 2, ny-1
        do i = 2, nx-1
           thermoDissRateloc = chi(i,j,k)

           if (thermoDissRateloc > thresh) then
              cos_temp_2D = grad_temp(i,j,k,1)/sqrt(thermoDissRateloc)
              sin_temp_2D = grad_temp(i,j,k,2)/sqrt(thermoDissRateloc) 
              peakflag_l = .true.
              radius = 1        


              do while ((peakflag_l) .and. (radius <= peaksearchradius))

                 di = nint(cos_temp_2D)
                 dj = nint(sin_temp_2D)

                 if (abs(cos_temp_2D).ge.abs(sin_temp_2D)) then
                    thermoDissRate_up   = chi(i+di,j,k)
                    thermoDissRate_down = chi(i-di,j,k)
                 else
                    thermoDissRate_up   = chi(i,j+dj,k)
                    thermoDissRate_down = chi(i,j-dj,k)
                 endif

                 if ((thermoDissRateloc .le. thermoDissRate_up) .or. &
                      (thermoDissRateloc .le. thermoDissRate_down)) then
                    peakflag_l = .false.
                 endif
                 radius = radius + 1

              enddo !do-while

              if (peakflag_l) then 
                 peakflag(i,j,k) = 1
              else
                 peakflag(i,j,k) = 0
              endif
           endif

        enddo
     enddo

  enddo

  ! Store peakflag
  segmented = peakflag

  ! find width of the dissipation layer
  ! defind threshold value
  cutoff = 0.20

  nextrema = sum(peakflag)

  if( nextrema == 0 ) then
     ! allocate dummy varialbe
     allocate(s0(1, 3))
     allocate(ds(1, 3))
     allocate(s(1))
     allocate(smax(1))
     allocate(abandonsearch(1))
     allocate(get_pointstatus(1))
     allocate(ftarg(1))
     allocate(fval(1))
     allocate(vals(1))
     allocate(err(1))
     allocate(err0(1))
     allocate(dfds(1))
     allocate(step(1))
     allocate(locs(3,1))

  else
     ! Do normal allocation
     allocate(s0(2*nextrema, 3))
     allocate(ds(2*nextrema, 3))
     allocate(s(2*nextrema))
     allocate(smax(2*nextrema))
     allocate(abandonsearch(2*nextrema))
     allocate(get_pointstatus(4*nextrema))
     allocate(ftarg(2*nextrema))
     allocate(fval(2*nextrema))
     allocate(vals(4*nextrema))
     allocate(err(2*nextrema))
     allocate(err0(2*nextrema))
     allocate(dfds(2*nextrema))
     allocate(step(2*nextrema))
     allocate(locs(3,4*nextrema))
  endif
  nends = 0

  ! Differentiate chi field for search direction
  call derivative_x(nx,ny,nz,chi,grad_chi(:,:,:,1),scale_1x,1)
  call derivative_y(nx,ny,nz,chi,grad_chi(:,:,:,2),scale_1y,1)
  call derivative_z(nx,ny,nz,chi,grad_chi(:,:,:,3),scale_1z,1)



  do k=1, nz
     do j = 1, ny
        do i = 1, nx
           do direction = -1,1,2

              if (peakflag(i,j,k) == 1) then
                 nends = nends + 1
                 ftarg(nends) = cutoff * chi(i,j,k) 

                 ! Look along s  where func = cutoff*thermoDissRate_peak
                 ! s from grad_chi
                 !               ds(nends,1) = grad_chi(i,j,k,1)
                 !               ds(nends,2) = grad_chi(i,j,k,2)
                 !               ds(nends,3) = grad_chi(i,j,k,3)

                 ! s from grad_temp
                 ds(nends,1) = grad_temp(i,j,k,1)
                 ds(nends,2) = grad_temp(i,j,k,2)
                 if( search_in_3d) then
                    ds(nends,3) = grad_temp(i,j,k,3)
                 else
                    ds(nends,3) = 0 ! for 2d option
                 endif


                 ds(nends,:) = ds(nends,:) / sqrt( ds(nends,1)**2 + ds(nends,2)**2 + ds(nends,3)**2) * direction

                 ! Starting position for this point
                 s0(nends, 1) = x(i)
                 s0(nends, 2) = y(j)
                 s0(nends, 3) = z(k)
              endif
           enddo
        enddo
     enddo
  enddo

  ! Domain extents in mm
  xmindim = xmin * l_ref * 1e3
  xmaxdim = xmax * l_ref * 1e3
  ymindim = ymin * l_ref * 1e3
  ymaxdim = ymax * l_ref * 1e3
  zmindim = zmin * l_ref * 1e3
  zmaxdim = zmax * l_ref * 1e3


  newtcount = 0
  s = 0
  err = 1.0e6
  globalerr = 1.0e6
  abandonsearch = .false.

  ! Set initial s to nonzero value to get off of local chi max (where dfds is very small)
  s = 0.01

  do while (globalerr > 0.01)
     err0 = err

     ! Evaluate function at 2 points along each search vector to approximate df/ds
     do i=1,2*nextrema
        LOCS(:,i) = s0(i,:) + s(i)*ds(i,:)
        LOCS(:,i+2*nextrema) = s0(i,:) + (s(i)+deltas)*ds(i,:)
     enddo

     ! Quit search if either point has run past edge of domain
     where(locs(1,1:2*nextrema) < xmindim )
        abandonsearch = .true.
     end where
     where(locs(1,1:2*nextrema) > xmaxdim )
        abandonsearch = .true.
     end where
     where(locs(2,1:2*nextrema) < ymindim )
        abandonsearch = .true.
     end where
     where(locs(2,1:2*nextrema) > ymaxdim )
        abandonsearch = .true.
     end where
     where(locs(3,1:2*nextrema) < zmindim )
        abandonsearch = .true.
     end where
     where(locs(3,1:2*nextrema) > zmaxdim )
        abandonsearch = .true.
     end where

     where(locs(1,2*nextrema+1:4*nextrema) < xmindim )
        abandonsearch = .true.
     end where
     where(locs(1,2*nextrema+1:4*nextrema) > xmaxdim )
        abandonsearch = .true.
     end where
     where(locs(2,2*nextrema+1:4*nextrema) < ymindim )
        abandonsearch = .true.
     end where
     where(locs(2,2*nextrema+1:4*nextrema) > ymaxdim )
        abandonsearch = .true.
     end where

     where(locs(3,2*nextrema+1:4*nextrema) < zmindim )
        abandonsearch = .true.
     end where

     where(locs(3,2*nextrema+1:4*nextrema) > zmaxdim )
        abandonsearch = .true.
     end where

     do i=1,2*nextrema
        if( abandonsearch(i) ) then
           LOCS(:,i) = s0(i,:) 
           LOCS(:,i+2*nextrema) = s0(i,:) 
        endif
     enddo


     ! Get function at points; this is a collective operation - will do data exchange as needed
     ! Each proc ends up with array vals corresponding to the points originally in LOCS
     CALL GETPOINTS( LOCS(:,1:4*nextrema), VALS(1:4*nextrema), 4*nextrema,chi(:,:,: ), get_pointstatus)


     ! Approximate df/ds and error 
     dfds = ( VALS(2*nextrema+1:4*nextrema) - VALS(1:2*nextrema) ) / deltas
     err = ( ftarg - VALS(1:2*nextrema) )

     ! Where search has been abandoned don't move solution
     where(abandonsearch)
        step = 0
     elsewhere
        step = err / dfds
     end where

     ! Mega under relax
     step  = step * 0.1

     ! Limit max step size
     where ( step > 0.03 ) 
        step = 0.03
     end where

     where ( step < -s/2.0d0 ) 
        step = - s/4.0d0
     end where

     ! Don't allow going negative - move s back towards s0, set step to 0
     where ( s + step < 0 )
        s = s * 0.5d0
        step = 0.0d0
     end where


     ! Stop if the function is increasing along s give up.
     ! This criteria should probably be tighter, e.g., if the function
     ! value at this point is less than the target we know the 
     ! target is bracketed by the current s and s0, so we could switch 
     ! to the bisection  method to find it
     where( dfds .gt. 0.0d0 )
        step = 0.0d0
        abandonsearch = .true.
     end where


     s = s + step

     err = abs(err)

     ! Take points where sesarch has been abandoned out of error calculation
     where(abandonsearch)
        err = 0.0d0
     end where

     ! Find global max error for exit condition
     CALL MPI_Allreduce(maxval(err/ftarg), globalerr, 1, MPI_DOUBLE_PRECISION, MPI_MAX, gcomm, ierr) 

     if(myid ==0 .and. mod(newtcount, 50)==0 )  write(*,*) 'Iteration, max relative error ', newtcount, globalerr

     newtcount = newtcount +1
     if( newtcount .gt. 9000) exit
  end do

  ! Log if search was abandoned for this point
  do i = 1,2*nextrema
     if(abandonsearch(i)) pointstatus(i) = 3
  enddo


  ! Store location of start/end of search path
  do i =1, nends
     nedgepoints = nedgepoints +1
     edgepoints(1, 1, nedgepoints) =   s0(i, 1)
     edgepoints(2, 1, nedgepoints) =   s0(i, 2)
     edgepoints(3, 1, nedgepoints) =   s0(i, 3)
     edgepoints(1, 2, nedgepoints) =   s0(i, 1) + s(i)*ds(i,1) 
     edgepoints(2, 2, nedgepoints) =   s0(i, 2) + s(i)*ds(i,2) 
     edgepoints(3, 2, nedgepoints) =   s0(i, 3) + s(i)*ds(i,3) 
  enddo


  deallocate(s0)
  deallocate(get_pointstatus)
  deallocate(ds)
  deallocate(s)
  deallocate(abandonsearch)
  deallocate(ftarg)
  deallocate(fval)
  deallocate(err)
  deallocate(err0)
  deallocate(dfds)
  deallocate(step)
  deallocate(locs)
  deallocate(vals)

1 format(' zone t="stationary", i=',i5,', j=',i5,', f=block')
2 format('GEOMETRY, T=LINE, CS=GRID')
11 format(' GEOMETRY X=',1pe12.5,', Y=',1pe12.5,', T=CIRCLE, C=CYAN, FC=CYAN, CS=GRID')
12 format(' GEOMETRY X=',1pe12.5,', Y=',1pe12.5,', T=CIRCLE, C=RED, FC=CYAN, CS=GRID')
99 format(10(1pe12.5,1x))
100 format(a9,1x,20(1pe12.5,1x))
999 format(10(i5,1x))

  return

end subroutine measure_scale

SUBROUTINE GETPOINTS( LOCS, VALS, npts,field, pointstatus)
  ! RG APRIL 2008
  ! Call collectively; after call array vals will contain value of field interpolated (tri-linear) onto points in locs
  ! pointstatus indicates if the point was obtained locally or from remote data
  ! uses some bits from collectpoints_m, but I think this is a better way of doing it than the approach originally
  ! in collectpoints - at least when most of the points can be obtained locally
  USE param_m, only: nx,ny,nz, npx, npy, npz, nx_g, ny_g, nz_g
  !  use collectpoints_m
  use topology_m
  use grid_m
  use reference_m, only:l_ref

  implicit none

  ! Input variables
  INTEGER NPTS
  REAL, DIMENSION(3, NPTS) :: LOCS
  REAL, DIMENSION(NPTS) :: VALS
  integer, dimension(NPTS) :: pointstatus
  INTEGER PTSROOT
  REAL, DIMENSION(nx,ny,nz) :: FIELD 

  ! External Routines
  EXTERNAL FUNC
  REAL FUNC
  EXTERNAL INTERP_PT
  REAL INTERP_PT


  ! Counters, misc bits
  integer ipoint, i, j, k, id, isend1, isend2
  integer ierror, istatus
  integer mpistatus(mpi_status_size)
  integer nptsnoteval

  ! For interpolation
  INTEGER, DIMENSION(3,8) :: NEARS
  REAL, DIMENSION(8) :: NEARVAL
  ! 

  ! For global grid
  real, dimension(nx_g) :: xgrid
  real, dimension(ny_g) :: ygrid
  real, dimension(nz_g) :: zgrid
  INTEGER mypx, mypy, mypz, offset(3)


  ! For retreiving points from other processors
  INTEGER, dimension(:,:), allocatable :: REQUESTLIST, SENDLIST
  REAL, dimension(:,:), allocatable :: SENDVALS
  integer, dimension(npts*8) :: recvlist
  real, dimension(npts*8) :: recvvals
  INTEGER, dimension(npx*npy*npz) :: nreq_array, nsend_array
  INTEGER :: nrecv, nptsmax
  integer :: nreq, index(3), recvposn
  INTEGER CHECK

  ! Setup global grid
  call MPI_AllGather(x,nx,MPI_DOUBLE_PRECISION,xgrid,nx,MPI_DOUBLE_PRECISION,xcomm,ierror)
  call MPI_AllGather(y,ny,MPI_DOUBLE_PRECISION,ygrid,ny,MPI_DOUBLE_PRECISION,ycomm,ierror)
  call MPI_AllGather(z,nz,MPI_DOUBLE_PRECISION,zgrid,nz,MPI_DOUBLE_PRECISION,zcomm,ierror)

  xgrid = xgrid*l_ref*1e3 
  ygrid = ygrid*l_ref*1e3
  zgrid = zgrid*l_ref*1e3



  if( minval(locs(1,1:npts)) .lt. xgrid(1) ) then
     write(*,*) 'outside on xgrid(1)'
  endif

  if( maxval(locs(1,1:npts)) .gt. xgrid(nx_g) ) then
     write(*,*) 'outside on xgrid(nx_g)'
  endif

  if( minval(locs(2,1:npts)) .lt. ygrid(1) ) then
     write(*,*) 'outside on ygrid(1)'
     write(*,*) ygrid(1), minval(locs(2,1:npts))
     stop

  endif
  if( maxval(locs(2,1:npts)) .gt. ygrid(ny_g) ) then
     write(*,*) 'outside on ygrid(ny_g)'
  endif


  if( minval(locs(3,1:npts)) .lt. zgrid(1) ) then
     write(*,*) 'outside on zgrid(1)'
  endif
  if( maxval(locs(3,1:npts)) .gt. zgrid(nz_g) ) then
     write(*,*) 'outside on zgrid(nz_g)'
  endif


  ! Processor grid - must match topology_m
  mypz = myid/(npx*npy)
  mypx = mod(myid-(mypz*npx*npy), npx)
  mypy = (myid-(mypz*npx*npy))/npx

  offset(1) = mypx * nx 
  offset(2) = mypy * ny 
  offset(3) = mypz * nz 

  ! Exchange arrays - need to be as big as biggest
  CALL MPI_Allreduce(npts, nptsmax, 1, MPI_INTEGER, MPI_MAX, gcomm, ierr) 

  allocate(requestlist(nptsmax*8,npx*npy*npz))

  allocate(sendlist(nptsmax*8,npx*npy*npz))

  allocate(sendvals(nptsmax*8, npx*npy*npz))

  ! Get local points first
  do ipoint = 1,npts
     pointstatus(ipoint) = 1 ! Haven't tried to calculate point yet
  enddo

  nreq = 0
  DO ipoint = 1, NPTS
     CALL NEARPTS( NEARS, LOCS(:,ipoint),xgrid,ygrid,zgrid )

     do i = 1,8

        CALL GLOBAL2LOCAL( NEARS(:,i), offset, 1) ! Convert global grid index to local
        if( nears(1,i) .gt. 0 .and. nears(2,i) .gt. 0 .and. nears(3,i) .gt. 0&
             .and. nears(1,i) .le. nx .and. nears(2,i) .le. ny .and. nears(3,i) .le.nz) then
           ! Neighbor point is here
           CONTINUE
        else
           ! Neighbor point is not here - add it to request list and flag this point for not being computed
           CALL GLOBAL2LOCAL( NEARS(:,i), offset, -1) ! Convert back to global grid index 
           CALL CONVSINGLEINDEX( NEARS(:,i), 1)
           call addreq( requestlist(:,myid+1), nears(1,i), nreq, npts*8)
           !             if(nreq .gt. 8*npts) then
           !                write(*,*)myid,'requesting',nreq,8*npts
           !                stop
           !             endif
           pointstatus(ipoint) = 2
        endif
     enddo
  ENDDO

  nptsnoteval = 0
  do i=1,npts
     if(pointstatus(i)==1) then
        ! evaluate from local data
        if( locs(2,i)/l_ref/1e3 .gt. y(ny) )then
           write(*,*) myid,'point',i,'pointstatus',pointstatus(i), 'broken. why?'
           write(*,*) locs(2,i)/l_ref/1.0e3, y(ny)
           stop
        endif
        ! func does trilinear interpolation using local grid
        vals(i) = func(locs(1,i)/l_ref/1e3, locs(2,i)/l_ref/1e3, locs(3,i)/l_ref/1e3, x, y, z, field, nx, ny, nz)
        pointstatus(i) = 0
     else
        vals(i) = -99
        nptsnoteval = nptsnoteval + 1
     endif
  enddo

  ! To do the rest of the points, each process needs to get values corresponding to the points in requestlist
  ! Steps:
  ! 1. Every proc broadcast the number & list of points it would like
  ! 2. Every proc checks and assembles a list of points it can donate
  ! 3. Every proc sends the points it has to the appropriate destination

  ! Step 1
  nreq_array(myid+1) = nreq
  do id =0, npx*npy*npz-1
     call MPI_Bcast( nreq_array(id+1), 1, MPI_INTEGER, id, gcomm, ierror)
  enddo

  !    write(*,*) myid,'requesting',nreq, nreq_array(myid+1)
  !    write(*,*) myid,'has nreq_array',nreq_array
  do id =0, npx*npy*npz-1
     call MPI_Bcast( requestlist(:,id+1), nreq_array(id+1), MPI_INTEGER, id, gcomm, ierror)
  enddo

  !    if(myid==1) then
  !       do id =0, npx*npy*npz-1
  !          write(*,*) nreq_array(id+1),' requested from',id
  !       enddo
  !       write(*,*)' id 0 requested:',requestlist(1:nreq_array(1),1)
  !    endif
  !    call mpi_barrier(gcomm,ierror)

  ! Step 2 - check each each other process has requested

  nsend_array = 0
  do id =0, npx*npy*npz-1
     do i=1,nreq_array(id+1)
        index(1) = requestlist(i,id+1)
        CALL CONVSINGLEINDEX( index, -1)
        call global2local(index, offset, 1)
        !          if(myid==1) then
        !             write(*,*) ' 1 considering',index,'from ',id
        !          endif
        if( index(1) .gt. 0 .and. index(2) .gt. 0 .and. index(3) .gt. 0 .and. index(1) .le. nx .and. index(2) .le. ny .and. index(3) .le.nz) then
           ! That point is on this process
           nsend_array(id+1) = nsend_array(id+1) + 1
           sendlist(nsend_array(id+1),id+1) = requestlist(i,id+1)
           SENDVALS(nsend_array(id+1),id+1) = field( index(1), index(2), index(3) )
        endif
     enddo
  enddo

  ! Step 3 - need to do transfer of points - number first, then indicies, then values

  recvposn = 1
  do isend1 = 0,npx*npy*npz-1
     !    isend1 = 0
     if(myid==isend1) then
        nrecv = 4
        do id=0,npx*npy*npz-1
           if(myid .ne. id) then
              call MPI_Send(nsend_array(id+1),1,MPI_INTEGER,id,2,gcomm,ierr)
              if(nsend_array(id+1).gt.0) then
                 call MPI_Send(sendlist(:,id+1),nsend_array(id+1),MPI_INTEGER,id,3,gcomm,ierr)
                 call MPI_Send(sendvals(:,id+1),nsend_array(id+1),MPI_DOUBLE_PRECISION,id,4,gcomm,ierr)
              endif
           endif
        enddo
     else
        call MPI_Recv(nrecv,1,MPI_INTEGER,isend1,2,gcomm,mpistatus,ierr)
        if(nrecv.gt.0) then
           call MPI_Recv(recvlist(recvposn),nrecv,MPI_INTEGER,isend1,3,gcomm,mpistatus,ierr)
           call MPI_Recv(recvvals(recvposn),nrecv,MPI_DOUBLE_PRECISION,isend1,4,gcomm,mpistatus,ierr)
           recvposn = recvposn + nrecv
        endif
     endif
  enddo

  ! Now have all the points we didn't before, and can fill in missing points in the interpolation

  ! First sort table
  if(nreq .ne. recvposn-1) then
     write(*,*) 'ack, error in sizing', nreq, recvposn, recvposn-nrecv
     stop
  endif
  CALL SORT_2DTAB( recvlist, recvvals, nreq)

  do ipoint=1,npts
     if(pointstatus(ipoint)==2) then
        ! evaluate from local and /or gathered data
        CALL NEARPTS( NEARS, LOCS(:,ipoint),xgrid,ygrid,zgrid )
        do i = 1,8
           CALL GLOBAL2LOCAL( NEARS(:,i), offset, 1) ! Convert global grid index to local
           if( nears(1,i) .gt. 0 .and. nears(2,i) .gt. 0 .and. nears(3,i) .gt. 0 .and. nears(1,i) .le. nx .and. nears(2,i) .le. ny .and. nears(3,i) .le.nz) then
              ! Neighbor point is here
              NEARVAL(i) = field(nears(1,i), nears(2,i), nears(3,i) )
              CALL GLOBAL2LOCAL( NEARS(:,i), offset, -11) ! Convert back to global grid index
           else
              ! Neighbor point is not here - get it from the request list
              CALL GLOBAL2LOCAL( NEARS(:,i), offset, -1) ! Convert back to global grid index 
              CALL CONVSINGLEINDEX( NEARS(:,i), 1)
              CALL S_2DTAB_LOOKUP( recvlist, recvvals, NEARS(1, i) , nreq, CHECK, NEARVAL(i))             
              CALL CONVSINGLEINDEX( NEARS(:,i), -1)
           endif
        enddo
        if( abs( locs(1,ipoint) - xgrid(nears(1,1) ) ) .gt. 0.15001) then
           WRITE(*,*) 'LOC: ',LOCS(:,ipoint), 'after nearpts BORDERED BY:'
           DO I = 1,8
              WRITE(*,*)NEARS(:,I)
              WRITE(*,*)I,xgrid(NEARS(1,I)), ygrid(nears(2,I)), zgrid(nears(3,I))
           ENDDO
           stop
        endif

        vals(ipoint) = INTERP_PT( locs(:,ipoint), NEARS, NEARVAL ,xgrid, ygrid, zgrid)


     endif
  enddo

  deallocate(requestlist)
  deallocate(sendlist)
  deallocate(sendvals)

END SUBROUTINE GETPOINTS


function func(x1,y1, z1, x, y,z, f, nx, ny, nz)
  ! Evaluate, by interpolation, f(i,j) at x(i,j) =x1 and y(i,j) = y1
  !  use collectpoints_m
  implicit none
  integer :: nx, ny, nz, i
  real, dimension(nx) :: x
  real, dimension(ny) :: y
  real, dimension(nz) :: z
  real, dimension(nx,ny,nz) :: f
  real func, x1, y1, z1, delta1, delta2
  integer nears(3,8) !nears(1,:) => j, nears(2,:) => k
  real nearval(8)
  INTEGER, DIMENSION(3) :: INCX = (/1,0,0/)
  INTEGER, DIMENSION(3) :: INCY = (/0,1,0/)
  INTEGER, DIMENSION(3) :: INCZ = (/0,0,1/)
  external S_TAB_LOOKUP
  integer  S_TAB_LOOKUP
  external interp_pt
  real interp_pt
  ! Get index of nearby points. y/z must be sorted
  NEARS(1,1) = S_TAB_LOOKUP( x, x1, nx)
  NEARS(2,1) = S_TAB_LOOKUP( y, y1, ny)
  NEARS(3,1) = S_TAB_LOOKUP( z, z1, nz)

  NEARS(:,2) = NEARS(:,1) + INCX
  NEARS(:,3) = NEARS(:,1) + INCY
  NEARS(:,4) = NEARS(:,1) + INCX + INCY

  NEARS(:,5) = NEARS(:,1) + INCZ
  NEARS(:,6) = NEARS(:,2) + INCZ
  NEARS(:,7) = NEARS(:,3) + INCZ
  NEARS(:,8) = NEARS(:,4) + INCZ


  !  3 ----4
  !  |
  !  1 --- 2  y->

  ! Check to make sure neighbours aren't outside of available grid
  do i=1,8
     if( nears(1,i) .le.0 .or. nears(2,i) .le.0.or.nears(3,i).le.0 ) then
        func = -1
        return
     endif
     nearval(i) = f( nears(1,i), nears(2,i),nears(3,i) )
  enddo

  DELTA1 = x1 - X( NEARS(1,1) )
  DELTA2 = X( NEARS(1,2) ) - X( NEARS(1,1) )

  NEARVAL(1) = NEARVAL(1) + DELTA1 / DELTA2 * ( NEARVAL(2) - NEARVAL(1) )
  NEARVAL(3) = NEARVAL(3) + DELTA1 / DELTA2 * ( NEARVAL(4) - NEARVAL(3) )
  NEARVAL(5) = NEARVAL(5) + DELTA1 / DELTA2 * ( NEARVAL(6) - NEARVAL(5) )
  NEARVAL(7) = NEARVAL(7) + DELTA1 / DELTA2 * ( NEARVAL(8) - NEARVAL(7) )

  ! Interpolate in y-dir next
  DELTA1 = y1 - Y( NEARS(2,1) )
  DELTA2 = Y( NEARS(2,3) ) - Y( NEARS(2,1) )

  NEARVAL(1) = NEARVAL(1) + DELTA1 / DELTA2 * ( NEARVAL(3) - NEARVAL(1) )
  NEARVAL(5) = NEARVAL(5) + DELTA1 / DELTA2 * ( NEARVAL(7) - NEARVAL(5) )

  ! Finally interpolate in z-dir
  DELTA1 = z1 - Z( NEARS(3,1) )
  DELTA2 = Z( NEARS(3,7) ) - Z( NEARS(3,3) )

  func = NEARVAL(1) + DELTA1/DELTA2 * ( NEARVAL(5) - NEARVAL(1) )


  return

end function func


!  !===============================================================================================
!  ! TABLE LOOKUP - TAB NEEDS TO BE SORTED
!  !===============================================================================================
FUNCTION S_TAB_LOOKUP( TAB, VAL, TABSIZE)
  ! TABLE LOOKUP; GIVEN A LIST TAB SORTED IN ASCENDING ORDER
  ! WILL RETURN THE LARGEST i FOR WHICH TAB(i) < VAL

  IMPLICIT NONE
  INTEGER S_TAB_LOOKUP
  INTEGER TABSIZE
  REAL, DIMENSION(TABSIZE) :: TAB
  REAL :: VAL
  INTEGER I,J,K
  integer scratch(3)
  IF( VAL < TAB(1) .OR. VAL > TAB(TABSIZE) ) THEN
     WRITE(*,*) 'Value outside of grid!'
     WRITE(*,*) 'Trying to lookup', VAL
     WRITE(*,*) 'Extents:',TAB(1), TAB(TABSIZE)
     WRITE(*,*) 'Grid size:', TABSIZE
     S_TAB_LOOKUP = -1
     RETURN
  ENDIF
  I = 1
  J = TABSIZE

  DO
     K = (I+J) /2
     IF( VAL < TAB(K) ) THEN
        J = K
     ELSE
        I = K
     ENDIF
     IF( I+1 .GE. J ) EXIT
  ENDDO

  S_TAB_LOOKUP = I
  RETURN
END FUNCTION S_TAB_LOOKUP
!===============================================================================================
!
!===============================================================================================
! 2D TABLE SORTING - REORDERS BOTH KEY & VAL SO THAT PAIRS OF ENTRIES THAT STARTED SAME POSITION
!                    FINISH IN SAME POSTION, BUT KEY IS IN ASCENDING ORDER. 
!                    N.B.: INSERTION SORT IS INEFFICIENT FOR LARGE ARRAY SIZES
!===============================================================================================
SUBROUTINE SORT_2DTAB( KEY, VAL, N)
  INTEGER N
  INTEGER KEY(N)
  REAL VAL(N)

  ! SORT KEY AND VAL ACCORDING TO KEY

  ! INSERTION SORT?
  DO I = 1, N
     KEY_T = KEY(I)
     VAL_T = VAL(I)

     J = I
     DO WHILE (( J .GT. 1) .AND. KEY(J-1) .GT. KEY_T )
        VAL(J) = VAL(J-1)
        KEY(J) = KEY(J-1)
        J=J-1
        VAL(J) = VAL_T
        KEY(J) = KEY_T
     ENDDO
  ENDDO

END SUBROUTINE SORT_2DTAB
!===============================================================================================



!===============================================================================================
! 2D TABLE LOOKUP - KEY / VAL MUST BE SORTED IN SAME ORDER OF ASCENDING KEY
!===============================================================================================
SUBROUTINE S_2DTAB_LOOKUP( KEYS, VALS, SEARCHKEY, N, FOUNDKEY, FOUNDVAL)
  INTEGER N
  INTEGER KEYS(N)
  REAL VALS(N)
  INTEGER SEARCHKEY, FOUNDKEY
  REAL FOUNDVAL
  INTEGER I,J
  ! 2D TABLE LOOKUP; FIND SERACHKEY IN ARRAY KEYS, RETURN CORRESPONDING
  ! POSITION FROM VALS. RETURN KEYS AT POSITION USED AS FOUNDKEY

  ! ASSUMES KEYS, VALS SORTED IN ORDER OF ASCENDING KEYS
  IF( SEARCHKEY < KEYS(1) .OR. SERACHKEY > KEYS(N) ) THEN
     !WRITE(*,*) 'Looking for point outside list'
     FOUNDKEY = -1
     RETURN
  ENDIF

  I = 1
  J = N

  DO
     K = (I+J) /2
     IF( SEARCHKEY < KEYS(K) ) THEN
        J = K
     ELSE
        I = K
     ENDIF
     IF( I+1 .GE. J ) EXIT
  ENDDO
  IF( KEYS(N) .EQ. SEARCHKEY ) I = N
  FOUNDKEY = KEYS(I)
  FOUNDVAL = VALS(I)

  RETURN

END SUBROUTINE S_2DTAB_LOOKUP
!===============================================================================================



!===============================================================================================
! INTERPOLATION FROM NEARBY POINTS IN X/Y/Z DIRECTIONS
!===============================================================================================
FUNCTION INTERP_PT( LOC, NEARS, NEARVAL, xgrid, ygrid, zgrid )
  USE param_m, ONLY: nx, ny, nz, nx_g, ny_g, nz_g
  INTEGER NEARS(3,8)
  REAL NEARVAL(8), TEMPSTORE(8)
  REAL, DIMENSION(3) :: LOC
  REAL INTERP_PT
  REAL DELTA

  ! For global grid
  real, dimension(nx_g) :: xgrid
  real, dimension(ny_g) :: ygrid
  real, dimension(nz_g) :: zgrid

  ! There is almost certainly a better way to do this...
  if(minval(nearval) .le. 0.0d0 )then
     write(*,'1a,8e15.5') 'nearval =',nearval
     stop
  endif

  ! Interpolate in x-dir first
  DELTA1 = LOC(1) - XGRID( NEARS(1,1) )
  DELTA2 = XGRID( NEARS(1,2) ) - XGRID( NEARS(1,1) )
  TEMPSTORE = NEARVAL

  NEARVAL(1) = NEARVAL(1) + DELTA1 / DELTA2 * ( NEARVAL(2) - NEARVAL(1) )
  NEARVAL(3) = NEARVAL(3) + DELTA1 / DELTA2 * ( NEARVAL(4) - NEARVAL(3) )
  NEARVAL(5) = NEARVAL(5) + DELTA1 / DELTA2 * ( NEARVAL(6) - NEARVAL(5) )
  NEARVAL(7) = NEARVAL(7) + DELTA1 / DELTA2 * ( NEARVAL(8) - NEARVAL(7) )


  if(minval(nearval) .lt. 0.0d0 )then
     write(*,'1a,8e15.5') 'after first step nearval =',nearval
     write(*,'1a,8e15.5') 'befor first step nearval =',TEMPSTORE
     write(*,'1a,2e15.5') 'delta1/2 =',delta1,delta2
     write(*,'1a,3(" "e7.2),1a,3(" "e7.2)')'what is going on? loc=',loc, ' XGRID(NEARS(1,1))=',XGRID(NEARS(1,1))

     stop
  endif

  ! Interpolate in y-dir next
  DELTA1 = LOC(2) - YGRID( NEARS(2,1) )
  DELTA2 = YGRID( NEARS(2,3) ) - YGRID( NEARS(2,1) )

  TEMPSTORE(1) = NEARVAL(1) + DELTA1 / DELTA2 * ( NEARVAL(3) - NEARVAL(1) )
  TEMPSTORE(5) = NEARVAL(5) + DELTA1 / DELTA2 * ( NEARVAL(7) - NEARVAL(5) )
  NEARVAL = TEMPSTORE

  ! Finally interpolate in z-dir
  DELTA1 = LOC(3) - ZGRID( NEARS(3,3) )
  DELTA2 = ZGRID( NEARS(3,7) ) - ZGRID( NEARS(3,3) )

  INTERP_PT = NEARVAL(1) + DELTA1/DELTA2 * ( NEARVAL(5) - NEARVAL(1) )

  RETURN
END FUNCTION INTERP_PT

SUBROUTINE GLOBAL2LOCAL( INDEX, OFFSET, DIR )
  ! CONVERT BETWEEN GLOBAL INDEX LOCAL I, J, K INDEX
  ! DIR > 0 => GLOBAL INDEX > LOCAL I,J,K
  ! DIR < 0 => LOCAL I, J, K > GLOBAL INDEX
  USE param_m, ONLY: nx_g, ny_g, nz_g
  IMPLICIT NONE
  INTEGER INDEX(3), DIR, offset(3)


  IF( DIR > 0 ) THEN
     !       CALL CONVSINGLEINDEX( INDEX, -1 ) ! THIS GIVES US GLOBAL I,J,K
     INDEX = INDEX - OFFSET
  ELSE
     INDEX = INDEX + OFFSET
     !       CALL CONVSINGLEINDEX (INDEX, 1)
  ENDIF
END SUBROUTINE GLOBAL2LOCAL

!===============================================================================================
! ADD GIVEN POINT TO LIST AND INCREMENT NUMBER OF POINTS IN LIST N ONLY IF LIST DOES NOT
! ALREADY CONTAIN POINT
!===============================================================================================
SUBROUTINE ADDREQ( LIST, POINT , N, NMAX)
  ! ADD ENTRY POINT TO LIST, INCRMENT NUMBER OF POINTS IN LIST
  IMPLICIT NONE
  INTEGER N, NMAX ! NUMBER OF POINTS CURRENTLY IN LIST AND DIMENSION OF LIST
  INTEGER, DIMENSION(NMAX) :: LIST
  INTEGER POINT

  ! IDEA IS TO ADD POINT AND INCREMENT N ONLY IF POINT ISN'T ALREADY IN LIST
  ! PROBABLY A BETTER WAY TO DO THIS, MIGHT EVEN BE CHEAPER TO KEEP THE LIST
  ! SORTED AND USE DECENT SEARCH... HOPEFULLY THE COMPILER WILL BE ABLE TO 
  ! VECTORIZE THIS THOUGH

  IF (MINVAL( abs( LIST(1:N)- POINT ) ) .eq. 0 ) THEN
     CONTINUE
     !       WRITE(*,*) 'LIST: ',LIST(1:N)
     !       WRITE(*,*) POINT, 'NOT ADDED'
     !       WRITE(*,*)'MOD LIST, POINT=:', MOD( LIST(1:N), POINT)
  ELSE
     !       WRITE(*,*) 'LIST: ',LIST(1:N)
     !       WRITE(*,*) POINT, 'ADDED'
     if( (N+1) < NMAX ) then
        LIST(N+1) = POINT
        N = N+1
     else
        write(*,*) 'Need bigger data exchange array'
        stop
     endif
  ENDIF

END SUBROUTINE ADDREQ
!===============================================================================================


!===============================================================================================
! CONVERT BETWEEN GLOBAL I/J/K INDEXING AND SINGLE INDEX =======================================
!===============================================================================================
SUBROUTINE CONVSINGLEINDEX( INDEX, DIR )
  ! CONVERT BETWEEN SINGLE GLOBAL INDEX AND I,J,K GLOBAL INDEX
  ! DIR > 0 => I,J,K GLOBAL TO SINGLE GLOBAL
  ! DIR < 0 => SINGLE GLOBAL > I,J,K GLOBAL
  USE param_m, ONLY: nx_g, ny_g, nz_g
  IMPLICIT NONE
  INTEGER INDEX(3), DIR


  IF( DIR > 0 ) THEN
     INDEX(1) = INDEX(1) + (INDEX(2) - 1) * NX_G + ( INDEX(3) - 1) * NX_G * NY_G
  ELSE
     INDEX(3) = (INDEX(1)-1) / ( NX_G * NY_G) +1
     INDEX(2) = MOD( (INDEX(1)-1), (NX_G*NY_G) ) / NX_G +1
     INDEX(1) = INDEX(1) - (INDEX(3)-1) * NX_G*NY_G - (INDEX(2)-1) * NX_G
  ENDIF
END SUBROUTINE CONVSINGLEINDEX
!===============================================================================================

SUBROUTINE CONVTOSINGLEINDEX( INDEX, DIR )
  ! CONVERT BETWEEN SINGLE GLOBAL INDEX AND I,J,K GLOBAL INDEX
  ! DIR > 0 => I,J,K GLOBAL TO SINGLE GLOBAL
  ! DIR < 0 => SINGLE GLOBAL > I,J,K GLOBAL
  USE param_m, ONLY: nx_g, ny_g, nz_g
  IMPLICIT NONE
  INTEGER INDEX(3), DIR


  INDEX(1) = INDEX(1) + (INDEX(2) - 1) * NX_G + ( INDEX(3) - 1) * NX_G * NY_G
END SUBROUTINE CONVTOSINGLEINDEX

!===============================================================================================
! RETURN GLOBAL I,J,K INDEX OF 8 POINTS SURROUNDING LOC ========================================
!===============================================================================================
SUBROUTINE NEARPTS (NEARS, LOC, xgrid, ygrid, zgrid)
  USE param_m, ONLY: nx, ny, nz, nx_g, ny_g, nz_g
  USE grid_m, ONLY: x, y, z
  USE topology_m

  IMPLICIT NONE
  EXTERNAL S_TAB_LOOKUP
  INTEGER S_TAB_LOOKUP

  REAL, DIMENSION(3) :: LOC
  INTEGER, DIMENSION(3,8) :: NEARS
  ! For global grid
  real, dimension(nx_g) :: xgrid
  real, dimension(ny_g) :: ygrid
  real, dimension(nz_g) :: zgrid

  INTEGER, DIMENSION(3) :: BASE ! I,J,K GLOBAL INDEX OF     
  INTEGER i
  INTEGER, DIMENSION(3) :: INCX = (/1,0,0/)
  INTEGER, DIMENSION(3) :: INCY = (/0,1,0/)
  INTEGER, DIMENSION(3) :: INCZ = (/0,0,1/)

  NEARS(1,1) = S_TAB_LOOKUP( xgrid, LOC(1), NX_G)
  IF(NEARS(1,1) == -1) THEN
     WRITE(*,*) 'NEARPTS ERROR IN LOOKING UP X-VALUE',LOC(1)
     STOP
  ENDIF
  NEARS(2,1) = S_TAB_LOOKUP( ygrid, LOC(2), NY_G)
  IF(NEARS(2,1) == -1) THEN
     WRITE(*,*) 'NEARPTS ERROR IN LOOKING UP Y-VALUE',LOC(2)
     STOP
  ENDIF

  NEARS(3,1) = S_TAB_LOOKUP( zgrid, LOC(3), NZ_G)
  IF(NEARS(3,1) == -1) THEN
     WRITE(*,*) 'NEARPTS ERROR IN LOOKING UP Z-VALUE',LOC(3)    
     STOP
  ENDIF
  NEARS(:,2) = NEARS(:,1) + INCX
  NEARS(:,3) = NEARS(:,1) + INCY
  NEARS(:,4) = NEARS(:,1) + INCX + INCY

  NEARS(:,5) = NEARS(:,1) + INCZ
  NEARS(:,6) = NEARS(:,2) + INCZ
  NEARS(:,7) = NEARS(:,3) + INCZ
  NEARS(:,8) = NEARS(:,4) + INCZ

  if( abs( loc(1) - xgrid(nears(1,1) ) ) .gt. 0.15001) then
     WRITE(*,*) 'LOC: ',LOC, 'BORDERED BY:'
     DO I = 1,8
        WRITE(*,*)NEARS(:,I)
        WRITE(*,*)I,xgrid(NEARS(1,I)), ygrid(nears(2,I)), zgrid(nears(3,I))
     ENDDO
     stop
  endif
END SUBROUTINE NEARPTS
!===============================================================================================




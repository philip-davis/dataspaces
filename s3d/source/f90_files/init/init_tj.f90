#include "globalDefines.h"

subroutine initialize_tj(io, yspecies,temp,pressure,u)

  use topology_m
!  use param_m,     only : n_spec, nx, ny, nz, i_soot, nx_g, ny_g
  use param_m,     only : n_spec, nx, ny, nz, nx_g, ny_g
  use reference_m, only : pres_atm, p_ref, t_ref, l_ref, a_ref
  use chemkin_m,   only : species_name
  use grid_m,      only : xmax, xmin, ymax, ymin, x, y, dely
!  use variables_m, only : ysoot, ndens
  use opthinrad_m
!  use lesfilt_m
!  use filter_smoother_m

 implicit none

!------------------------------------------------------------------------------
! variables passed in

  real, intent(inout), dimension(nx,ny,nz)        :: temp, pressure
  real, intent(inout), dimension(nx,ny,nz,n_spec) :: yspecies
  real, intent(inout), dimension(nx,ny,nz,3)      :: u

  integer, intent(in) :: io

!------------------------------------------------------------------------------
! local declarations

  character*100 :: filename, ctemp
  integer       :: i,j,k,L, i1, i2, nf, n_delta
  real          :: d1, d2
  integer       :: seed
  real, allocatable, dimension(:)     :: temp_flmlt, mixfr_flmlt
  real, allocatable, dimension(:,:)   :: yspec_flmlt
  real          :: ycent1, ycent2, fracYr, tanh_width, f_filter
  real, dimension(nx,ny,nz) :: mixfr, smooth_mixfr
  real          :: ftanh_width, fYcent_off, utanh_width, uYcent_off, delta_U, min_U  
  integer       :: fa_flag

!------------------------------------------------------------------------------
! executable statements

  if(myid==0) write(io,*) '**********Initializing 2D faStripe ************'

  pressure = 1.0 * pres_atm / p_ref
  temp     = 550 / t_ref
  u        = 0.0
  yspecies = 0.0
!  if(i_soot==1)  ysoot = 0.0
!  if(i_soot==1)  ndens = 0.0

!-------------------------------------------------------------------------------
! radiation model on

!  i_opthinrad = 1
!  call initialize_opthinrad(io)

!------------------------------- Read input file, set params -------------------------

  filename = '../input/tj.in'
  call inquire_about_input_file(filename,io)

  if (myid==0) then
     open( unit=20,file=filename,status='old',form='formatted' )
     
     read(20,*) fa_flag              ! 1 for fuel stripe, 0 for air
     read(20,*) ftanh_width          ! cm
     read(20,*) fYcent_off           ! cm tanh centers: offset from domain center for mixf
     read(20,*) utanh_width          ! cm
     read(20,*) uYcent_off           ! cm tanh centers: offset from domain center for vel
     read(20,*) delta_U              ! velocity difference (m/s)
     read(20,*) min_U                ! min velocity: max is min + delta_U (m/s)

     close(20)

     ftanh_width = ftanh_width * 0.01 / l_ref
     fYcent_off  = fYcent_off  * 0.01 / l_ref 
     uYcent_off  = uYcent_off  * 0.01 / l_ref
     utanh_width = utanh_width * 0.01 / l_ref
     delta_U     = delta_U            / a_ref
     min_U       = min_U              / a_ref

     if(utanh_width .lt. 0.0) utanh_width = 10.0 * dely

  endif

  call MPI_Bcast( fa_flag,       1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( ftanh_width,   1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( fYcent_off,    1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( uYcent_off,    1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( delta_U,       1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( min_U,         1, MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( utanh_width,   1, MPI_REAL8, 0, gcomm, ierr )

!--------------------------------- Set mixture fraction field ------------------------------
  
  ycent1 = (ymax-ymin)/2.0 - fYcent_off
  ycent2 = (ymax-ymin)/2.0 + fYcent_off
  do i=1, ny
!    f_filter = 0.5*(1.0+tanh((y(i)-ycent1)*2.0/ftanh_width)) * &
!               0.5*(1.0+tanh((ycent2-y(i))*2.0/ftanh_width))
    f_filter = 0.5*(1.0+tanh( ( (y(i)-ycent1)+5.0*(y(i)-ycent1)**3.0 ) *2.0/ftanh_width)  ) * &
               0.5*(1.0+tanh( ( (ycent2-y(i))+5.0*(ycent2-y(i))**3.0 ) *2.0/ftanh_width)  )
    mixfr(:,i,:) = mixfr(:,i,:) * f_filter
    if(fa_flag==1) then
      mixfr(:,i,:) = f_filter
    else
      mixfr(:,i,:) = 1.0 - f_filter
    endif
  enddo

!--------------------------------- Set mean velocity field ---------------------------------

  ! tanh profile centered on middle of domain with +- umax.
  ! tanh_width is the characteristic width of the profile ( delta y / dydx_max )
  
  u = 0.0
  ycent1 = (ymax-ymin)/2.0 - uYcent_off
  ycent2 = (ymax-ymin)/2.0 + uYcent_off
  do j=1,ny
!    u(:,j,:,1) = 0.5*(1.0+tanh((y(j)-ycent1)*2.0/utanh_width)) * &
!               0.5*(1.0+tanh((ycent2-y(j))*2.0/utanh_width))
    u(:,j,:,1) = 0.5*(1.0+tanh(  ( (y(j)-ycent1)+0.0*(y(j)-ycent1)**3.0 )  *2.0/utanh_width)) * &
                 0.5*(1.0+tanh(  ( (ycent2-y(j))+0.0*(ycent2-y(j))**3.0 )  *2.0/utanh_width))
    u(:,j,:,1) = delta_U * u(:,j,:,1) + min_U 
  enddo

!--------------------------------- Set chemical state field --------------------------------
  

  if(myid==0) then

    filename = '../input/flmlt_prof.in'
    open( unit=20, file=filename, status='old', form='formatted' )
    write(io,*) 'reading file ../input/flmlt_prof.in'
    read(20,*) ctemp, d1, nf
    read(20,*)
    read(20,*)
    allocate(mixfr_flmlt(nf), temp_flmlt(nf))
    allocate(yspec_flmlt(nf,n_spec))
    
    do i=1, nf
        read(20,*) mixfr_flmlt(i), temp_flmlt(i), (yspec_flmlt(i,j),j=1,n_spec), (d1, j=1,2)
    enddo
    close(20)
  endif

  call MPI_Bcast( nf, 1, MPI_INTEGER, 0, gcomm, ierr )

  if(.not. allocated(temp_flmlt))  allocate(temp_flmlt(nf))
  if(.not. allocated(mixfr_flmlt)) allocate(mixfr_flmlt(nf))
  if(.not. allocated(yspec_flmlt)) allocate(yspec_flmlt(nf,n_spec))

  call MPI_Bcast( temp_flmlt,  nf,        MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( mixfr_flmlt, nf,        MPI_REAL8, 0, gcomm, ierr )
  call MPI_Bcast( yspec_flmlt, nf*n_spec, MPI_REAL8, 0, gcomm, ierr )

  temp_flmlt = temp_flmlt / t_ref

  if(myid==0) then
     write(io,*)
     write(io,*) 'Assuming uniform mixf spacing in the flamelet table'
     write(io,*) 'Assuming species in same order in DNS and flamelet table'
     write(io,*)
  endif

  d1 = mixfr_flmlt(2) - mixfr_flmlt(1) 
 
  do i=1,nx
     do j=1,ny
     do k=1,nz
  
        i1 = int(mixfr(i,j,k)/d1) + 1        ! index of lower f of flmlt grid
        i2 = i1 + 1                        ! index of higher f of flmlt grid
        if(i1==nf) i2 = i1
        
        if(i2 > nf) then
          continue
        endif

        temp(i,j,k) = temp_flmlt(i1) + (temp_flmlt(i2)-temp_flmlt(i1))/d1 &
             * (mixfr(i,j,k) - mixfr_flmlt(i1))
        
        do L=1,n_spec
           yspecies(i,j,k,L) = yspec_flmlt(i1,L) + (yspec_flmlt(i2,L)-yspec_flmlt(i1,L))/d1 &
                * (mixfr(i,j,k) - mixfr_flmlt(i1))
        enddo
        
     enddo
     enddo
  enddo

     if(myid==0) then 
       write(*,*) '************ Done init faStripe ***********'
     endif

end subroutine initialize_tj

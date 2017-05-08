#include "globalDefines.h"
!$Id:$
!----------------------------------------------------------------------
! ************** WRITTEN BY RAMANAN SANKARAN *********************
!
! Ramanan Sankaran, NCCS/ORNL (sankaranr@ornl.gov)
! Adds tracer particles and tracks their history
!
! 11Dec08: RG: Change SSN assignement logic
!              Now, global counter startssn; at each insertion
!                                  2. Allgather ninsert to pinsertcount(npes)
!                                  3. Compute first ssn to assign: 
!                              if(myid > 0) mystartssn = startssn + sum( pinsertcount(1:myid) )
!                              else : mystartssn = startssn
!                                  4. Assign new ssn i = mystartssn + i ( check ssn imax <= startssn + sum( 1:myid) ) 
!                                  5. Update startssn = startssn + sum( pinsertcount(:) )
! Version for Lagrangian particles. Particles are convected by local 
! fluid velocity, there is no particle diffusion or propagation.
! Look for the tag !LAG, indicating the changes made 28thApr08, ESR.
!
! 18sept08. SSN written as integer*8 (64 bit) since the SSN 
! had overflowed 32 bits in the production case.
! 
! Options for tracer initialisation and advancement to be based on 
! job title:
!
! Case/run_title	Selection				Code
!
! Lifted: 		init_case: 	Mass weighted. 		[1]
!     "lifted"		feed_case:	Mass weighted.		[1]
!			tran_case: 	Convective.		[1]
! Bunsen: 		init_case:	Flame iso-surface.	[2]
!     "bunsen"		feed_case:	Flame iso-surface.	[2]
!			tran_case:	Flame tracking.		[2]
! Stratified:		init_case:	Flame iso-surface.	[2]
!     "strat"		feed_case:	Flame iso-surface.	[2]
!			tran_case:    Flame tracking.(Part-prmx)[2]
! Temporal Jet:		init_case:	Mass weighted.		[1]
!     "tj"		feed_case:	No feed.		[0]
!			tran_case:	PDF*			[4]
! Jet-in-cross-flow:	init_case:	Mass weighted.		[1]
!     "jicf"		feed_case:	Mass weighted(multi-inlets).[3]
!			tran-case:	Convective.		[1]
!
! *PDF = Convective + mass diffusion + Weiner diffusion
!----------------------------------------------------------------------
module tracer_m
  use topology_m, only: npes
  implicit none
  private

  public initialize_tracer, &  !!!!! initial_tracer_distribution, &
       insert_tracers, &
       write_tracer_tecplot, advance_tracer, &
       write_tracer_savefile, read_tracer_savefile, &
       trace_save_fctr, deallocate_tracer

  integer, public :: tracer_ctrl
  integer, parameter :: ARRAY_SIZE = 100000 !Max no of particles
  integer seed, imass, trcsngl, io_tracer
  real numdens
  real xmincut, xmaxcut, ymincut, ymaxcut
  real trace_save_fctr
  character mode*1
  character save_mf_chi*1
  integer :: init_case, feed_case, tran_case

  !----------------------------------------
  integer :: fill !fill level of the arrays

  ! RG 11Dec08 for new ssn assignment algorithm
  integer(kind=8) startssn, mystartssn, mymaxssn
  integer ssn_cntr_thisinsertion
  integer, allocatable,  dimension( : ) :: pinsertcount
  ! RG 11Dec08 for new ssn assignment algorithm

  integer(kind=8), dimension(ARRAY_SIZE) :: ssn !unique identifier
  real, dimension(ARRAY_SIZE) :: age !since creation
  real, dimension(ARRAY_SIZE) :: dob !time at birth
  integer, dimension(ARRAY_SIZE) :: state !Current state of the tracer

  integer seed_size                    ! size of seed array for random_number call
  integer, allocatable :: new_seed(:)  ! seed array for intrinsic random_number call

  !----------------------------------------
  !An enum list of possible states of the tracer particle. 
  integer, parameter :: & 
       UNOCCUPIED       = -1, &
       HEALTHY          =  0, &
       EXIT_LEFT_X_BDRY =  1, &
       EXIT_RGHT_X_BDRY =  2, &
       EXIT_LEFT_Y_BDRY =  3, &
       EXIT_RGHT_Y_BDRY =  4, &
       EXIT_LEFT_Z_BDRY =  5, &
       EXIT_RGHT_Z_BDRY =  6, &
       WENT_TOO_FAR     =  7, & !Went farther than the nearest neighbor
       WENT_LEFT_X      =  8, &
       WENT_RGHT_X      =  9, &
       WENT_LEFT_Y      = 10, &
       WENT_RGHT_Y      = 11, &
       WENT_LEFT_Z      = 12, &
       WENT_RGHT_Z      = 13, &
       DEATH_HOLE       = 14, & !Kill particles that defy logic
       NBR_NOSPACE      = 15, & !Neighbor has no space to receive 
       ZERO_GRADIENT    = 16, & !Scalar gradient was zero
       DRIFTED          = 17, & !Drifted away from the intended surface
       TRUNCATION_ERROR = 18    !The tracer's location was lost due to truncation error

  !----------------------------------------
  !Number of steps the solver takes for one tracer RK step
  !This has to be a multiple of two - RK method needs a half-step
  !Solver time step has to stay constant within this many number of steps
  integer, public ::  num_tracer_half_step, i_tracer_time
  !----------------------------------------
  integer, public :: itracertec
  logical, public :: tracer_restart
  integer, public :: reseed, read_single

  real, dimension(ARRAY_SIZE, 3) :: & 
       loc, & !holds the particle grid coordinates 
       loc_prev, & !holds the previous locations (yn) for RK evaluation
       dloc !for accumulating h/6(k1 + 2k2 + 2k3 + k4) for RK4 method 

  integer :: ssn_cntr
  integer(kind=8) :: ssn_cntr_dbl, myid_dbl ! 64 bit integers to avoid overflow.

  !----------------------------------------
contains

  !----------------------------------------------------------------------
  subroutine initialize_tracer(io)
    use topology_m
    use reference_m, only: l_ref,rho_ref
    use runtime_m, only : run_title
    implicit none
    integer, intent(in) :: io
    character filename*100
    integer, parameter :: io_inp = 92
    integer idummy, ix
    real rdummy

    io_tracer=io

    if(myid .eq. 0) then
       call write_header(io, '-')
       write(io, *) 'reading tracer input file'
    end if
    filename = '../input/tracer.in'
    call inquire_about_input_file(filename, io)

    if(myid .eq. 0) then
       open(unit=io_inp, file=trim(filename), status='old')
       read(io_inp, *) !header
       read(io_inp, *) num_tracer_half_step
       read(io_inp, *) itracertec
       idummy = 2*num_tracer_half_step*int(itracertec/2/num_tracer_half_step)
       if(idummy .ne. itracertec) then
          itracertec = idummy+2*num_tracer_half_step
          write(io, *) 'itracertec does not correspond to a full tracer step'
          write(io, *) 'Resetting it to itracertec = ', itracertec
       end if
       read(io_inp, *) idummy
       read(io_inp, *) trace_save_fctr
       read(io_inp, *) trcsngl
       read(io_inp, *) seed
       read(io_inp, *) imass
       write(io,*)'imass = ',imass
       read(io_inp, *) numdens
       read(io_inp, *) xmincut
       read(io_inp, *) xmaxcut
       read(io_inp, *) ymincut
       read(io_inp, *) ymaxcut
       read(io_inp, *) mode
       read(io_inp, *) save_mf_chi
       read(io_inp, *) reseed
       read(io_inp, *) read_single

       if( read_single .eq. 1 ) then
          write(io, *) ' ************** EXPECTING 4 BYTE TRACER SSN IN READ *******'
       end if

       numdens=numdens*(l_ref)**3
       if(imass.eq.1)numdens=numdens*rho_ref
       xmincut=xmincut/l_ref
       xmaxcut=xmaxcut/l_ref
       ymincut=ymincut/l_ref
       ymaxcut=ymaxcut/l_ref

       if(idummy .eq. 0) then
          tracer_restart = .false. 
       else
          tracer_restart = .true. 
       end if
       close(io_inp)
    end if
    call mpi_bcast(num_tracer_half_step, 1, MPI_INTEGER, 0, gcomm, ierr)
    call mpi_bcast(itracertec, 1, MPI_INTEGER, 0, gcomm, ierr)
    call mpi_bcast(tracer_restart, 1, MPI_LOGICAL, 0, gcomm, ierr)
    call mpi_bcast(trace_save_fctr, 1, MPI_REAL8, 0, gcomm, ierr)
    call mpi_bcast(trace_save_fctr, 1, MPI_INTEGER, 0, gcomm, ierr)
    call mpi_bcast(trcsngl, 1, MPI_INTEGER, 0, gcomm, ierr)
    call mpi_bcast(seed, 1, MPI_INTEGER, 0, gcomm, ierr)
    call mpi_bcast(imass, 1, MPI_INTEGER, 0, gcomm, ierr)
    call mpi_bcast(numdens, 1, MPI_REAL8, 0, gcomm, ierr)
    call mpi_bcast(xmincut, 1, MPI_REAL8, 0, gcomm, ierr)
    call mpi_bcast(xmaxcut, 1, MPI_REAL8, 0, gcomm, ierr)
    call mpi_bcast(ymincut, 1, MPI_REAL8, 0, gcomm, ierr)
    call mpi_bcast(ymaxcut, 1, MPI_REAL8, 0, gcomm, ierr)
    call mpi_bcast(mode, 1, MPI_CHARACTER, 0, gcomm, ierr)
    call mpi_bcast(save_mf_chi, 1, MPI_CHARACTER, 0, gcomm, ierr)
    call mpi_bcast(reseed, 1, MPI_INTEGER, 0, gcomm, ierr)
    call mpi_bcast(read_single, 1, MPI_INTEGER, 0, gcomm, ierr)


    ! initialize seed for intrinsic random number call
    call random_seed(size=seed_size)
    allocate(new_seed(seed_size)); new_seed(:)=seed+myid
    call random_seed(put=new_seed)

    do ix=1,1e6 ! This is needed to flush the initial junk. 
       call random_number(rdummy)
    enddo

    i_tracer_time = 0
    fill = 0
    state(:) = UNOCCUPIED
    ssn_cntr = 0 

    select case (trim(run_title))
    case ('lifted')
       init_case=1
       feed_case=1
       tran_case=1
    case ('bunsen')
       init_case=2
       feed_case=2
       tran_case=2
    case ('strat')
       init_case=2
       feed_case=2
       tran_case=2  !flame tracking for partially premixed flow
    case ('tj')
       init_case=1
       feed_case=0
       tran_case=4  !PDF = conv+mass diff+Weiner
    case ('jicf')
       init_case=1
       feed_case=3
       tran_case=1
    case default
       init_case=1
       feed_case=1
       tran_case=1
    end select

    allocate( pinsertcount(npes) )
    return

  end subroutine initialize_tracer

  !----------------------------------------------------------------------
  subroutine deallocate_tracer
    implicit none

    deallocate(new_seed)

    return
  end subroutine deallocate_tracer

  !----------------------------------------------------------------------
!!!  subroutine initial_tracer_distribution(io)
!!!  implicit none
!!!  integer, intent(in) :: io
!!!  return !RSA DEBG
!!!  call insert_tracers(io)
!!!  end subroutine initial_tracer_distribution
!!!  
  !----------------------------------------------------------------------
  subroutine insert_tracers(io, num, mask)

    !driver routine to initialize and feed tracers.

    use topology_m   

    integer, intent(in) :: io
    integer, intent(in), optional :: num !number of tracers to insert
    !if mask is true, tracers are inserted on a slice only.
    !logical, intent(in), optional :: mask
    logical :: mask
    integer fillsum

    call mpi_allreduce(fill, fillsum, 1, MPI_INTEGER, MPI_SUM, gcomm, ierr)
    if(fillsum.eq.0.or.reseed.eq.1)then   ! 'if fill=0 then we will impose the initial distribution'
       if( myid == 0 ) then
          write(io, *) 'Reseeding tracer field.... '
       end if


       select case (init_case)   ! tracer initialisation

       case (1)  ! Mass or volume weighted.

          call init_tracer_uniform(io, num, mask)

       case (2)  ! Iso-surface

          mask=.false.
          call init_tracer_surface(io, num, mask)

       end select

       if( reseed.eq.1 ) then
          reseed = 0
       end if

    else

       select case (feed_case)   ! tracer feed

       case (0)  ! No feed

          !   no call needed

       case (1)  ! Mass or volume flow rate weighted.

          call init_tracer_uniform(io, num, mask)

       case (2)  ! Flame iso-surface feed.

          mask=.true.
          call init_tracer_surface(io, num, mask)

       case (3)  ! Mass of volume flow rate weighted. (JICF).

          write(*,*) 'tracer feed not implemented yet for JICF.'

       end select

    endif

    return
  end subroutine insert_tracers
  !----------------------------------------------------------------------
  !ROUTINE TO PUT TRACERS ON AN ISOSURFACE
  subroutine init_tracer_surface(io, num, mask)
    use isosurf_m, only: extract_isosurface
    use triangulate_m, only: vert_loc, vert_dirn, vert_ratio, vert_count
    use triangulate_m, only: X_EDGE, Y_EDGE, Z_EDGE
    use topology_m
    use runtime_m, only: time
    use param_m, only: nx, ny, nz
    implicit none
    integer, intent(in) :: io
    integer, intent(in), optional :: num !number of tracers to insert
    !if mask is true, tracers are inserted on a slice only. 
    logical, intent(in), optional :: mask 

    integer count1, count2, count2_g, skip
    integer n, vert_ptr
    real r

    if(myid .eq. 0) call write_header(io, '-')
    call extract_isosurface(io)

    if(present(mask) .and. mask) then
       if(xid .eq. 0) then
          count1 = count(vert_loc(1, :) .eq. 2) !On the i=2 plane at xid = 0
       else
          count1 = 0
       end if
    else
       count1 = vert_count !All the vertices are eligible
    end if

    if(present(num) .and. count1 > num .and. num >0 ) then
       count2 = num
       skip = ceiling(real(count1)/real(num))
    else
       count2 = count1
       skip = 1
    end if

    if(fill+count2>ARRAY_SIZE) call compact_arrays
    if(fill+count2>ARRAY_SIZE) then
       print *, 'No space to insert ', count2, ' tracers.'
       print *, 'fill = ', fill, ' myid = ', myid
       count1 = 0
    end if

!    print *, 'Estimated insertion of', count2, 'tracers...'
    call reset_mystartssn( count2, gcomm, myid, npes )

    n = 0
    count2 = 0
    do vert_ptr = 1, vert_count
       if(present(mask) .and. mask .and. & 
            (count1 .eq. 0 .or. vert_loc(1,vert_ptr) .ne. 2 )) cycle
       n = n+1
       if(n .ne. skip) cycle
       n = 0  !n .eq. skip
       count2 = count2+1
       fill = fill + 1

       ! RG 11Dec08 for new ssn assignment
       !   ssn_cntr = ssn_cntr + 1
       !   ssn(fill) = ssn_cntr*npes + myid
       ssn_cntr = ssn_cntr + 1
       ssn(fill) = mystartssn + ssn_cntr_thisinsertion
       ssn_cntr_thisinsertion = ssn_cntr_thisinsertion  + 1

       ! Error checking:
       if( ssn(fill) > mymaxssn ) then
          write(io,*) myid, 'assigned invalid ssn: ', ssn(fill), '; max=', mymaxssn
       endif
       ! RG 11Dec08 for new ssn assignment

       age(fill) = 0.0
       dob(fill) = time
       loc(fill, 1) = real(xid*nx + vert_loc(1, vert_ptr))
       loc(fill, 2) = real(yid*ny + vert_loc(2, vert_ptr))
       loc(fill, 3) = real(zid*nz + vert_loc(3, vert_ptr))
       r = 1.0 - vert_ratio(vert_ptr)
       select case (vert_dirn(vert_ptr))
       case(X_EDGE)
          loc(fill, 1) = loc(fill, 1) + r
       case(Y_EDGE)
          loc(fill, 2) = loc(fill, 2) + r
       case(Z_EDGE)
          loc(fill, 3) = loc(fill, 3) + r
       end select
       state(fill) = HEALTHY
       loc_prev(fill, :) = loc(fill, :)
    end do
    call mpi_reduce(count2, count2_g, 1, MPI_INTEGER, MPI_SUM, 0, gcomm, ierr)
    if(myid .eq. 0) write(io, *) 'Inserted ', count2_g,' tracers.'

    call relocate_particles
    if(myid .eq. 0) call write_header(io, '-')

    return
  end subroutine init_tracer_surface

  !----------------------------------------------------------------------
  !ROUTINE TO INITIALIZE and INSERT TRACERS WITH UNIFORM NUMBER PER UNIT
  !VOLUME or MASS.
  subroutine init_tracer_uniform(io, num, mask)

    use topology_m
    use runtime_m, only: time
    use param_m, only: nx, ny, nz
    use reference_m, only:l_ref,a_ref,time_ref,rho_ref
    use grid_m, only: x,y,z, xg,yg,zg   !esr added
    use grid_m, only: xmin,xmax,ymin,ymax !for debug check only, you can remove these.
    use variables_m, only: u, q !esr added
    use runtime_m, only: tstep !esr added
    implicit none
    integer, intent(in) :: io
    integer, intent(in), optional :: num !number of tracers to insert
    !if mask is true, tracers are inserted on a slice only. 
    logical, intent(in), optional :: mask 

    integer count1, count2, count2_g, skip
    integer avail_ssns
    integer n, vert_ptr
    real r

    real lxproc,lyproc,lzproc,volproc

    integer vert_count
    integer ix, iy, iz
    real, dimension(ny,nz) :: Prob
    real, dimension(nx,ny,nz) :: Prob_xyz
    real delt_x, delt_y, delt_z, delt_t
    real random
    integer fillsum

    if(myid .eq. 0) call write_header(io, '-')

    call mpi_allreduce(fill, fillsum, 1, MPI_INTEGER, MPI_SUM, gcomm, ierr)
    if(fillsum.eq.0.or.reseed.eq.1)then   ! 'if fill=0 then we will impose the initial distribution'

       !  1) FIND PROBABILITY THAT DNS NODE IX,IY,IZ CONTAINS A PARTICLE 
       !     Prob_xyz(ix,iy,iz)=Dx*Dy*Dz*(rho)*numdens
       delt_x = (x(nx)-x(1))/real(nx-1)
       delt_y = (y(ny)-y(1))/real(ny-1) !assume uniform grid in each processor 
       delt_z = (z(nz)-z(1))/real(nz-1)
       !delt_t = 2.0 * real(num_tracer_half_step) * tstep !assume time step is constant
       do iz=1,nz
          do iy=1,ny
             do ix=1,nx
                if(imass.ne.1)then
                   ! MAKE IT PROPORTIONAL TO NODE VOLUME.
                   Prob_xyz(ix,iy,iz) = delt_x * delt_y * delt_z * numdens  !(here numdens is number per m^3)
                else
                   ! MAKE IT PROPORTIONAL TO NODE MASS
                   Prob_xyz(ix,iy,iz) = delt_x * delt_y * delt_z * q(ix,iy,iz,4,1) * numdens !(here numdens is number per kg )
                endif
                if(x(ix).lt.xmincut.or.x(ix).gt.xmaxcut) Prob_xyz(ix,iy,iz)=0.0
                if(y(iy).lt.ymincut.or.y(iy).gt.ymaxcut) Prob_xyz(ix,iy,iz)=0.0
             enddo
          enddo
       enddo

       !  2) ARRAY compaction based on estimated number of tracers added and a 
       !  20% safety factor

       count2=max(int(sum(Prob_xyz(:,:,:))*1.20),1)
       count1=count2

       if(fill+count2>ARRAY_SIZE) call compact_arrays
       if(fill+count2>ARRAY_SIZE) then
          print *, 'No space to insert about', count2, ' tracers.'
          print *, 'fill = ', fill, ' myid = ', myid
          count1 = 0
       end if


       call reset_mystartssn( count2, gcomm, myid, npes )

       !  3) IF RAN(iy,iz) .LE. Prob(iy,iz) INSERT PARTICLE AT (iy,iz)

       n = 0
       count2 = 0
       do iz=1,nz
          do iy=1,ny
             do ix=1,nx

                if(count1.eq.0)cycle  ! this says there is insufficient array space
                call random_number(random)

                if(Prob_xyz(ix,iy,iz).LT.random)cycle
                count2 = count2+1
                fill = fill + 1
                ssn_cntr = ssn_cntr + 1
                ssn(fill) = mystartssn + ssn_cntr_thisinsertion
                ssn_cntr_thisinsertion = ssn_cntr_thisinsertion  + 1 ! RG 11Dec08 for new ssn
                ! Where is this ssn used???


                age(fill) = 0.0
                dob(fill) = time

                loc(fill, 1) = real(xid*nx+ix)+0.001    ! don't put it right on the nodes - because I'm superstitious
                loc(fill, 2) = real(yid*ny+iy)+0.001
                loc(fill, 3) = real(zid*nz+iz)+0.001

                state(fill) = HEALTHY
                loc_prev(fill, :) = loc(fill, :)

             enddo
          enddo
       enddo

       call mpi_reduce(count2, count2_g, 1, MPI_INTEGER, MPI_SUM, 0, gcomm, ierr)
       if(myid .eq. 0) write(io, *) 'Initially Inserted ', count2_g,' tracers.'

    else

       count1=0
       count2=0

       if(xid.eq.0)then

          !FOR INSERTING LAGRANGIAN TRACERS AT THE IX=2 PLANE WITH UNIFORM VOLUME DENSITY

          !  1) FIND PROBABILITY THAT DNS NODE IY,IZ LAUNCHES A PARTICLE 
          !     Prob(iy,iz)=Dt_insert*Dy*Dz*U(iy,iz)*numdens
          delt_y = (y(ny)-y(1))/real(ny-1) !assume uniform grid in each processor 
          delt_z = (z(nz)-z(1))/real(nz-1)
          delt_t = 2.0 * real(num_tracer_half_step) * tstep !assume time step is constant
          do iz=1,nz
             do iy=1,ny
                ! MAKE IT PROPORTIONAL TO VOLUME FLOW RATE.
                if(imass.ne.1)then
                   Prob(iy,iz) = delt_y * delt_z * delt_t * max(u(2,iy,iz,1),0.0) * numdens  !(here numdens is number per m^3)
                else
                   ! MAKE IT PROPORTIONAL TO MASS FLUX
                   Prob(iy,iz) = delt_y * delt_z * delt_t * max(q(2,iy,iz,1,1),0.0) * numdens !(here numdens is number per kg )
                endif
                if(y(iy).lt.ymincut.or.y(iy).gt.ymaxcut) Prob(iy,iz)=0.0
             enddo
          enddo

          !if(yid.eq.20.and.zid.eq.1)write(io,*)'delt_y,delt_z,delt_t,numdens,tstep,prob(1,1),u(2,1,1,1)', &
          ! delt_y*l_ref,delt_z*l_ref,delt_t*time_ref,numdens,tstep*time_ref,prob(1,1),u(2,1,1,1)*a_ref,q(2,1,1,1,1)*rho_ref*a_ref

          !  2) ARRAY compaction based on estimated number of tracers added and a 
          !  20% safety factor

          count2=max(int(sum(Prob(:,:))*1.20),1)
          count1=count2

          if(fill+count2>ARRAY_SIZE) call compact_arrays
          if(fill+count2>ARRAY_SIZE) then
             print *, 'No space to insert about', count2, ' tracers.'
             print *, 'fill = ', fill, ' myid = ', myid
             count1 = 0
          end if

       else
          count2 = 0
       endif

       call reset_mystartssn( count2, gcomm, myid, npes )
       avail_ssns = count2

       if(xid .eq. 0 ) then

          !  3) IF RAN(iy,iz) .LE. Prob(iy,iz) INSERT PARTICLE AT (iy,iz)


          n = 0
          count2 = 0
          do iz=1,nz
             do iy=1,ny

                if(count1.eq.0)cycle  ! this says there is insufficient array space
                if( avail_ssns .lt. 1 ) cycle ! this says we're out of allocated ssns for this insertion
                call random_number(random)
                if(Prob(iy,iz).LT.random)cycle
                count2 = count2+1
                fill = fill + 1

                ! RG 11Dec08 for new ssn assignment
                !   ssn_cntr = ssn_cntr + 1
                !   ssn(fill) = ssn_cntr*npes + myid
                avail_ssns = avail_ssns -1
                ssn_cntr = ssn_cntr + 1
                ssn(fill) = mystartssn + ssn_cntr_thisinsertion
                ssn_cntr_thisinsertion = ssn_cntr_thisinsertion  + 1

                ! Error checking:
                if( ssn(fill) > mymaxssn ) then
                   write(io,*) myid, '/', npes, 'assigned invalid ssn: ',&
                   ssn(fill), '; max=', mymaxssn, 'current:', &
                   ssn_cntr_thisinsertion,  'mystart:', mystartssn,&
                   'pinsertcount:', pinsertcount

                endif

                age(fill) = 0.0
                dob(fill) = time

                loc(fill, 1) = real( 2.001 )    ! don't put it right on the nodes - because I'm superstitious
                loc(fill, 2) = real(yid*ny+iy)+0.001
                loc(fill, 3) = real(zid*nz+iz)+0.001

                state(fill) = HEALTHY
                loc_prev(fill, :) = loc(fill, :)

             enddo
          enddo

       endif !xid

       call mpi_reduce(count2, count2_g, 1, MPI_INTEGER, MPI_SUM, 0, gcomm, ierr)
       if(myid .eq. 0) write(io, *) 'Inserted ', count2_g,' tracers.'


    endif !fillsum

    call relocate_particles
    if(myid .eq. 0) call write_header(io, '-')

    return
  end subroutine init_tracer_uniform

  !----------------------------------------------------------------------
  ! Uses standard RK4 for time integration
  !----------------------------------------------------------------------
  subroutine advance_tracer(stage)
    use runtime_m, only:tstep, i_time
    use topology_m, only: myid
    implicit none
    integer, intent(in) :: stage
    real, dimension(ARRAY_SIZE, 3) :: k
    real :: h, hb2, hb6, hb3

    integer cnt, cnt_g, ec

    if(myid .eq. 0) print *, i_time, 'advancing tracer stage ', stage
    h = tstep*real(2*num_tracer_half_step)
    hb2 = 0.5*h
    hb6 = h/6.0
    hb3 = h/3.0

    select case (stage)
    case (1)
       !call debug_single_tracer(stage) !RSA
       call compute_grid_velocity(k,stage)
       dloc(1:fill,:) = hb6*k(1:fill,:)
       loc(1:fill,:) = loc_prev(1:fill,:) + hb2*k(1:fill,:)
       call relocate_particles
    case (2)
       !call debug_single_tracer(stage) !RSA
       call compute_grid_velocity(k,stage)
       dloc(1:fill,:) = dloc(1:fill,:) + hb3*k(1:fill,:)
       loc(1:fill,:) = loc_prev(1:fill,:) + hb2*k(1:fill,:)
       call relocate_particles
    case (3)
       !call debug_single_tracer(stage) !RSA
       call compute_grid_velocity(k,stage)
       dloc(1:fill,:) = dloc(1:fill,:) + hb3*k(1:fill,:)
       loc(1:fill,:) = loc_prev(1:fill,:) + h*k(1:fill,:)
       call relocate_particles
    case (4)
       !call debug_single_tracer(stage) !RSA
       call compute_grid_velocity(k,stage)
       loc(1:fill,:) = loc_prev(1:fill,:) + dloc(1:fill,:) + hb6*k(1:fill,:)
       call relocate_particles
       loc_prev(1:fill,:) = loc(1:fill,:)
       age(1:fill) = age(1:fill) + h
       !----------------------------------------
    end select
    return
  end subroutine advance_tracer

  !----------------------------------------------------------------------
  subroutine print_state
    use topology_m
    implicit none
    integer n, count_l, count_g
    if(myid .eq. 0) call write_header(6, '-')
    call mpi_reduce(fill, count_g, 1, MPI_INTEGER, MPI_SUM, 0, gcomm, ierr)
    if(myid .eq. 0)  print *, 'Tracer stats: Fill level = ', count_g
    do n = -1, 20
       count_l = count(state(1:fill).eq.n)
       call mpi_reduce(count_l, count_g, 1, MPI_INTEGER, MPI_SUM, 0, gcomm, ierr)
       if(myid.eq.0.and.count_g>0) print *, 'state = ', n, ' count_g = ', count_g
    end do
    if(myid .eq. 0) call write_header(6, '-')
    return
  end subroutine print_state

  !----------------------------------------------------------------------
  subroutine compute_grid_velocity(vel,stage)
    use param_m, only: nx_g, ny_g, nz_g
    use grid_m
    implicit none
    real, dimension(ARRAY_SIZE, 3), intent(out) :: vel
    integer, intent(in) :: stage

    call compute_physical_velocity(vel,stage)
    call scale_onedirn(vel(:,1), 1, unif_grid_x, nx_g, scale_1xg)
    call scale_onedirn(vel(:,2), 2, unif_grid_y, ny_g, scale_1yg)
    call scale_onedirn(vel(:,3), 3, unif_grid_z, nz_g, scale_1zg)
    return
  end subroutine compute_grid_velocity

  !----------------------------------------------------------------------
  subroutine compute_physical_velocity(vel,stage)
    use param_m, only: nx, ny, nz
    use variables_m, only: u
    use runtime_m, only: run_title, tstep
    use premix_drvd_var_m, only: calc_sdn_uw, calc_sdn_mass, calc_sdn_rand
    use topology_m, only: myid
    implicit none
    real, dimension(ARRAY_SIZE, 3), intent(out) :: vel
    integer, intent(in) :: stage

    real :: vel_grid(nx, ny, nz, 3)
    real :: vel_rand(nx, ny, nz, 3)
    logical :: sd_flag(nx, ny, nz) 

    real h, hb6, hb3

    vel = 0.0
    if(tran_case.eq.2)then !flame tracking
       call check_for_drift
    endif
    !----------------------------------------
    ! Compute propagation:
    if(tran_case.eq.2)then   !surface tracking, Sd*n

       if(trim(run_title).eq.'bunsen')then
          call calc_sdn_uw(vel_grid, sd_flag)
       else   !(trim(run_title).eq.'strat')then
          if(myid.eq.0)write(*,*)'stratified flame tracking not yet implemented'
       endif

       call flag_tracer(sd_flag, ZERO_GRADIENT)

    elseif(tran_case.eq.4)then  !PDF - mass diffusion part
       ! uj = 1/rho.d(rhoD)/dx
       call calc_sdn_mass(vel_grid)

       ! Add the Weiner term during stage 4 only.
       ! dx = sqrt(2D*dt)*dW
       ! u  = dx/dt_step4

       if (stage.eq.4)then
          h = tstep*real(2*num_tracer_half_step)
          hb6 = h/6.0
          call calc_sdn_rand(vel_rand)
          vel_grid = vel_grid + vel_rand*sqrt(h)/hb6
       endif
    endif
    !----------------------------------------
    if(tran_case.eq.1)then
       vel_grid = u
    else
       ! u + Sd*n
       vel_grid = vel_grid+u
    endif

    call interpolate(vel_grid(:,:,:,1), vel(:,1))
    call interpolate(vel_grid(:,:,:,2), vel(:,2))
    call interpolate(vel_grid(:,:,:,3), vel(:,3))

    return
  end subroutine compute_physical_velocity

  !----------------------------------------------------------------------
  subroutine check_for_drift
    use premix_drvd_var_m, only: calculate_progvar
    use param_m, only: nx, ny, nz
    use reference_m, only: l_ref!RSA DEBG
    use topology_m, only: myid !RSA DEBG
    implicit none
    real :: scalar(nx, ny, nz), sc_t(fill)
    integer i
    real xloc(3) !RSA DEBG

    call calculate_progvar(scalar,io_tracer)
    call interpolate(scalar, sc_t)
    do i = 1, fill
       if(state(i).ne.HEALTHY) cycle
       if(sc_t(i) .lt. 0.64 .or. sc_t(i) .gt. 0.66) state(i) = DRIFTED
       if(sc_t(i) .lt. 0.64 .or. sc_t(i) .gt. 0.66) then
          call grid_coords(loc(i, :), xloc)
          write (*,'(1(A), 1(1i5), 1(i12), 4(1pe12.5, 1x))') 'rsa drf ', &
               myid, ssn(i), xloc(:)*l_ref*1e3, sc_t(i)  !dbl length of myid increased from 3 to 5 characters
          !dbl length of ssn in creased form 7 to 12 characters. esr, 18sept08.
       end if
    end do
    return
  end subroutine check_for_drift

  !----------------------------------------------------------------------
  subroutine relocate_particles
    use topology_m, only: xid, yid, zid, xcomm, ycomm, zcomm, xpes, ypes, zpes
    use param_m, only: periodic_x, periodic_y, periodic_z
    use param_m, only: nx, ny, nz, nx_g, ny_g, nz_g
    implicit none
    call relocate_onedirn(1, periodic_x, nx, nx_g, xcomm, xid, xpes)
    call relocate_onedirn(2, periodic_y, ny, ny_g, ycomm, yid, ypes)
    call relocate_onedirn(3, periodic_z, nz, nz_g, zcomm, zid, zpes)
    return
  end subroutine relocate_particles

  !----------------------------------------------------------------------
  subroutine relocate_onedirn(dirn, prdc, nx, nx_g, xcomm, xid, xpes)
    use topology_m, only: MPI_INTEGER, MPI_LOGICAL, MPI_REAL8, MPI_INTEGER8, MPI_Status_size
    use topology_m, only: myid, ierr
    implicit none
    integer, intent(in) :: dirn, prdc, nx, nx_g, xcomm, xid, xpes

    integer i 
    integer lnbr, rnbr
    integer mymin, mymax, lnbrmin, lnbrmax, rnbrmin, rnbrmax
    integer this_loc
    integer :: EXIT_LEFT, EXIT_RGHT, WENT_LEFT, WENT_RGHT
    integer :: cnt_wn_lf, cnt_wn_rg, cnt_cm_lf, cnt_cm_rg
    integer :: req(4), sttus(MPI_Status_size, 4)
    logical nospace, lf_nospace, rg_nospace

    lnbr = xid - 1
    rnbr = xid + 1

    if(prdc .eq. 1) then
       if(xid .eq. 0) lnbr = xpes-1
       if(xid .eq. xpes-1) rnbr = 0
    end if

    mymin = xid*nx + 1
    mymax = (xid+1)*nx

    lnbrmin = lnbr*nx + 1
    lnbrmax = (lnbr+1)*nx

    rnbrmin = rnbr*nx + 1
    rnbrmax = (rnbr+1)*nx

    select case (dirn)
    case (1)
       EXIT_LEFT = EXIT_LEFT_X_BDRY
       EXIT_RGHT = EXIT_RGHT_X_BDRY
       WENT_LEFT = WENT_LEFT_X
       WENT_RGHT = WENT_RGHT_X
    case (2)
       EXIT_LEFT = EXIT_LEFT_Y_BDRY
       EXIT_RGHT = EXIT_RGHT_Y_BDRY
       WENT_LEFT = WENT_LEFT_Y
       WENT_RGHT = WENT_RGHT_Y
    case (3)
       EXIT_LEFT = EXIT_LEFT_Z_BDRY
       EXIT_RGHT = EXIT_RGHT_Z_BDRY
       WENT_LEFT = WENT_LEFT_Z
       WENT_RGHT = WENT_RGHT_Z
    end select

    mainloop: do i = 1, fill
       if(state(i) .ne. HEALTHY) cycle mainloop
       this_loc = int(loc(i, dirn))
       !----------------------------------------
       ! I did not use an endless do loop.
       ! We limit the movement to no farther than 1 process domain
       ifblkprdc: if(prdc.eq.1) then
          if(this_loc<1) then
             loc(i, dirn) = loc(i, dirn) + real(nx_g)
             loc_prev(i, dirn) = loc_prev(i, dirn) + real(nx_g)
             this_loc = int(loc(i, dirn))
          end if
          if(this_loc>nx_g) then
             loc(i, dirn) = loc(i, dirn) - real(nx_g)
             loc_prev(i, dirn) = loc_prev(i, dirn) - real(nx_g)
             this_loc = int(loc(i, dirn))
          end if
       ! A tracer does not really leave a periodic boundary. 
       ! But if truncation error makes it hard to bring it back, then kill it
          if (loc(i, dirn)<1.0) then
             state(i) = TRUNCATION_ERROR
             cycle mainloop
          else if (loc(i, dirn)>=real(nx_g+1)) then
             state(i) = TRUNCATION_ERROR
             cycle mainloop
          end if
       end if ifblkprdc
       !----------------------------------------
       ! Check if it is within the problem domain or left through a boundary.
       if(prdc .ne. 1) then !non-periodic boundary
          if (loc(i, dirn)<1.0) then
             state(i) = EXIT_LEFT
             cycle mainloop
          else if (loc(i, dirn)>=real(nx_g)) then
             state(i) = EXIT_RGHT
             cycle mainloop
          end if
       end if
       !----------------------------------------
       ! Check if it is still within this process domain
       if(this_loc.ge.mymin .and. this_loc.le.mymax) cycle mainloop
       !----------------------------------------
       ! It left the current process domain
       ! Does it belong to the left neighbor?
       if(this_loc.ge.lnbrmin .and. this_loc.le.lnbrmax) then
          state(i) = WENT_LEFT
          cycle mainloop
       end if
       !----------------------------------------
       ! Or else might belong to the right neighbor
       if(this_loc.ge.rnbrmin .and. this_loc.le.rnbrmax) then
          state(i) = WENT_RGHT
          cycle mainloop
       end if
       !----------------------------------------
       ! It does not belong at the neighbors too. 
       state(i) = WENT_TOO_FAR
    end do mainloop

    !----------------------------------------
    cnt_wn_lf = count(state(1:fill).eq.WENT_LEFT)
    cnt_wn_rg = count(state(1:fill).eq.WENT_RGHT)

    if(lnbr>=0) then
       call mpi_Isend(cnt_wn_lf, 1, MPI_INTEGER, lnbr, 31, xcomm, req(1), ierr)
       call mpi_Irecv(cnt_cm_lf, 1, MPI_INTEGER, lnbr, 32, xcomm, req(2), ierr)
    else
       cnt_cm_lf = 0
    end if

    if(rnbr<xpes) then
       call mpi_Isend(cnt_wn_rg, 1, MPI_INTEGER, rnbr, 32, xcomm, req(3), ierr)
       call mpi_Irecv(cnt_cm_rg, 1, MPI_INTEGER, rnbr, 31, xcomm, req(4), ierr)
    else
       cnt_cm_rg = 0
    end if

    if(lnbr>=0) call mpi_waitall(2, req(1:2), sttus(:, 1:2), ierr)
    if(rnbr<xpes) call mpi_waitall(2, req(3:4), sttus(:, 3:4), ierr)

    !----------------------------------------
    ! Check to see if space is available to receive
    nospace = .false.
    if(fill+cnt_cm_lf+cnt_cm_rg>ARRAY_SIZE) call compact_arrays
    if(fill+cnt_cm_lf+cnt_cm_rg>ARRAY_SIZE) nospace=.true. 

    !----------------------------------------
    ! Communicate space availability
    if(lnbr>=0) then
       call mpi_Isend(nospace, 1, MPI_LOGICAL, lnbr, 51, xcomm, req(1), ierr)
       call mpi_Irecv(lf_nospace, 1, MPI_LOGICAL, lnbr, 52, xcomm, req(2), ierr)
    end if

    if(rnbr<xpes) then
       call mpi_Isend(nospace, 1, MPI_INTEGER, rnbr, 52, xcomm, req(3), ierr)
       call mpi_Irecv(rg_nospace, 1, MPI_INTEGER, rnbr, 51, xcomm, req(4), ierr)
    end if

    if(lnbr>=0) call mpi_waitall(2, req(1:2), sttus(:,1:2), ierr)
    if(rnbr<xpes) call mpi_waitall(2, req(3:4), sttus(:,3:4), ierr)

    !----------------------------------------
    ! Deal with lack of space
    if(nospace) then
       cnt_cm_lf = 0
       cnt_cm_rg = 0
    end if

    if(lnbr>=1 .and. lf_nospace) then
       print *, 'left nospace', myid, lnbr
       cnt_wn_lf = 0
       do i = 1, fill
          if(state(i).eq.WENT_LEFT) state(i) = NBR_NOSPACE
       end do
    end if

    if(rnbr<xpes .and. rg_nospace) then
       print *, 'right nospace', myid, rnbr
       cnt_wn_rg = 0
       do i = 1, fill
          if(state(i).eq.WENT_RGHT) state(i) = NBR_NOSPACE
       end do
    end if

    !----------------------------------------
    ! Migrate particle data
    call exchange_ryl(age)
    call exchange_ryl(dob)
    call exchange_int_dbl(ssn)
    do i = 1, 3
       call exchange_ryl(loc(:,i))
       call exchange_ryl(loc_prev(:,i))
       call exchange_ryl(dloc(:,i))
    end do
    !----------------------------------------
    do i = 1, fill
       if(state(i).eq.WENT_LEFT .or. state(i).eq.WENT_RGHT) state(i) = UNOCCUPIED
    end do
    state(fill+1:fill+cnt_cm_lf+cnt_cm_rg) = HEALTHY
    fill = fill + cnt_cm_lf + cnt_cm_rg

    return

    !----------------------------------------
  contains
    !----------------------------------------
    subroutine exchange_ryl(array)
      implicit none
      real, intent(inout), dimension(ARRAY_SIZE) :: array
      real :: snd_left(cnt_wn_lf), snd_rght(cnt_wn_rg)
      real :: rcv_left(cnt_cm_lf), rcv_rght(cnt_cm_rg)

      !----------------------------------------
      if(cnt_wn_lf>0) then
         snd_left(:) = pack(array(1:fill), state.eq.WENT_LEFT)
         call mpi_Isend &
              (snd_left, cnt_wn_lf, MPI_REAL8, lnbr, 41, xcomm, req(1), ierr)
      end if
      if(cnt_cm_lf>0) call mpi_Irecv &
           (rcv_left, cnt_cm_lf, MPI_REAL8, lnbr, 42, xcomm, req(2), ierr)
      !----------------------------------------
      if(cnt_wn_rg>0) then
         snd_rght(:) = pack(array(1:fill), state.eq.WENT_RGHT)
         call mpi_Isend &
              (snd_rght, cnt_wn_rg, MPI_REAL8, rnbr, 42, xcomm, req(3), ierr)
      end if
      if(cnt_cm_rg>0) call mpi_Irecv &
           (rcv_rght, cnt_cm_rg, MPI_REAL8, rnbr, 41, xcomm, req(4), ierr)
      !----------------------------------------
      if(cnt_cm_lf>0) then
         call mpi_wait(req(2), sttus(:,2), ierr)
         array(fill+1:fill+cnt_cm_lf) = rcv_left(1:cnt_cm_lf)
      end if
      if(cnt_cm_rg>0) then
         call mpi_wait(req(4), sttus(:,4), ierr)
         array(fill+cnt_cm_lf+1:fill+cnt_cm_lf+cnt_cm_rg) = rcv_rght(1:cnt_cm_rg)
      end if
      !----------------------------------------
      if(cnt_wn_lf>0) call mpi_wait(req(1), sttus(:,1), ierr)
      if(cnt_wn_rg>0) call mpi_wait(req(3), sttus(:,3), ierr)
      !----------------------------------------
      return
    end subroutine exchange_ryl

    !----------------------------------------
    !----------------------------------------
    subroutine exchange_int(array)
      implicit none
      integer, intent(inout), dimension(ARRAY_SIZE) :: array
      integer :: snd_left(cnt_wn_lf), snd_rght(cnt_wn_rg)
      integer :: rcv_left(cnt_cm_lf), rcv_rght(cnt_cm_rg)

      !----------------------------------------
      if(cnt_wn_lf>0) then
         snd_left(:) = pack(array(1:fill), state(1:fill).eq.WENT_LEFT)
         call mpi_Isend &
              (snd_left, cnt_wn_lf, MPI_INTEGER, lnbr, 41, xcomm, req(1), ierr)
      end if
      if(cnt_cm_lf>0) call mpi_Irecv &
           (rcv_left, cnt_cm_lf, MPI_INTEGER, lnbr, 42, xcomm, req(2), ierr)
      !----------------------------------------
      if(cnt_wn_rg>0) then
         snd_rght(:) = pack(array(1:fill), state(1:fill).eq.WENT_RGHT)
         call mpi_Isend &
              (snd_rght, cnt_wn_rg, MPI_INTEGER, rnbr, 42, xcomm, req(3), ierr)
      end if
      if(cnt_cm_rg>0) call mpi_Irecv &
           (rcv_rght, cnt_cm_rg, MPI_INTEGER, rnbr, 41, xcomm, req(4), ierr)
      !----------------------------------------
      if(cnt_cm_lf>0) then
         call mpi_wait(req(2), sttus(:,2), ierr)
         array(fill+1:fill+cnt_cm_lf) = rcv_left(1:cnt_cm_lf)
      end if
      if(cnt_cm_rg>0) then
         call mpi_wait(req(4), sttus(:,4), ierr)
         array(fill+cnt_cm_lf+1:fill+cnt_cm_lf+cnt_cm_rg) = rcv_rght(1:cnt_cm_rg)
      end if
      !----------------------------------------
      if(cnt_wn_lf>0) call mpi_wait(req(1), sttus(:,1), ierr)
      if(cnt_wn_rg>0) call mpi_wait(req(3), sttus(:,3), ierr)
      !----------------------------------------
      return
    end subroutine exchange_int

    !----------------------------------------
    subroutine exchange_int_dbl(array)
      !routine added 18sep08 by esr to deal with 64 bit ssn.
      implicit none
      integer(kind=8), intent(inout), dimension(ARRAY_SIZE) :: array
      integer(kind=8) :: snd_left(cnt_wn_lf), snd_rght(cnt_wn_rg)
      integer(kind=8) :: rcv_left(cnt_cm_lf), rcv_rght(cnt_cm_rg)

      !----------------------------------------
      if(cnt_wn_lf>0) then
         snd_left(:) = pack(array(1:fill), state(1:fill).eq.WENT_LEFT)
         call mpi_Isend &
              (snd_left, cnt_wn_lf, MPI_INTEGER8, lnbr, 41, xcomm, req(1), ierr)
      end if
      if(cnt_cm_lf>0) call mpi_Irecv &
           (rcv_left, cnt_cm_lf, MPI_INTEGER8, lnbr, 42, xcomm, req(2), ierr)
      !----------------------------------------
      if(cnt_wn_rg>0) then
         snd_rght(:) = pack(array(1:fill), state(1:fill).eq.WENT_RGHT)
         call mpi_Isend &
              (snd_rght, cnt_wn_rg, MPI_INTEGER8, rnbr, 42, xcomm, req(3), ierr)
      end if
      if(cnt_cm_rg>0) call mpi_Irecv &
           (rcv_rght, cnt_cm_rg, MPI_INTEGER8, rnbr, 41, xcomm, req(4), ierr)
      !----------------------------------------
      if(cnt_cm_lf>0) then
         call mpi_wait(req(2), sttus(:,2), ierr)
         array(fill+1:fill+cnt_cm_lf) = rcv_left(1:cnt_cm_lf)
      end if
      if(cnt_cm_rg>0) then
         call mpi_wait(req(4), sttus(:,4), ierr)
         array(fill+cnt_cm_lf+1:fill+cnt_cm_lf+cnt_cm_rg) = rcv_rght(1:cnt_cm_rg)
      end if
      !----------------------------------------
      if(cnt_wn_lf>0) call mpi_wait(req(1), sttus(:,1), ierr)
      if(cnt_wn_rg>0) call mpi_wait(req(3), sttus(:,3), ierr)
      !----------------------------------------
      return
    end subroutine exchange_int_dbl

  end subroutine relocate_onedirn

  !----------------------------------------------------------------------
  ! Could have called my other interpolate module here
  ! Decided to code it again to keep it simple
  subroutine interpolate(field, field_tracer)
    use topology_m, only: xid, yid, zid, gcomm, ierr
    use param_m, only: nx, ny, nz
    use ghost_nice_m
    implicit none
    real, intent(in) :: field(nx, ny, nz)
    real, intent(out) :: field_tracer(1:fill)

    real :: fieldg(nx+1, ny+1, nz+1)
    integer i
    real ci0j0k0, ci1j0k0, ci0j1k0, ci1j1k0, ci0j0k1, ci1j0k1, ci0j1k1, ci1j1k1
    real r1, r2, r3
    integer x, y, z

    fieldg = 0.0
    fieldg(1:nx, 1:ny, 1:nz) = field(1:nx, 1:ny, 1:nz)
    call mpi_barrier(gcomm, ierr)
    call ghostzone_real(fieldg, nx, ny, nz, 0, 1, 0, 1, 0, 1)

    do i = 1, fill
       if(state(i).ne.HEALTHY) cycle
       !----------------------------------------
       x = int(loc(i, 1))
       y = int(loc(i, 2))
       z = int(loc(i, 3))
       r1 = loc(i, 1) - real(x)
       r2 = loc(i, 2) - real(y)
       r3 = loc(i, 3) - real(z)
       x = x-xid*nx
       y = y-yid*ny
       z = z-zid*nz
       ! To be consistent with how i usually define 'r'
       r1 = 1.0 - r1
       r2 = 1.0 - r2
       r3 = 1.0 - r3
       !----------------------------------------
       ci0j0k0 = (    r1)*(    r2)*(    r3) 
       ci1j0k0 = (1.0-r1)*(    r2)*(    r3) 
       ci0j1k0 = (    r1)*(1.0-r2)*(    r3) 
       ci1j1k0 = (1.0-r1)*(1.0-r2)*(    r3) 
       ci0j0k1 = (    r1)*(    r2)*(1.0-r3) 
       ci1j0k1 = (1.0-r1)*(    r2)*(1.0-r3) 
       ci0j1k1 = (    r1)*(1.0-r2)*(1.0-r3) 
       ci1j1k1 = (1.0-r1)*(1.0-r2)*(1.0-r3) 
       !----------------------------------------
       field_tracer(i) = &
            ci0j0k0*fieldg(x  , y  , z  ) + &
            ci1j0k0*fieldg(x+1, y  , z  ) + &
            ci0j1k0*fieldg(x  , y+1, z  ) + &
            ci1j1k0*fieldg(x+1, y+1, z  ) + &
            ci0j0k1*fieldg(x  , y  , z+1) + &
            ci1j0k1*fieldg(x+1, y  , z+1) + &
            ci0j1k1*fieldg(x  , y+1, z+1) + &
            ci1j1k1*fieldg(x+1, y+1, z+1) 
       !----------------------------------------
    end do

    return
  end subroutine interpolate

  !----------------------------------------------------------------------
  ! Based on flags on the grid, flag tracers
  subroutine flag_tracer(flag_field, flag_value)
    use topology_m, only: xid, yid, zid
    use param_m, only: nx, ny, nz
    use ghost_nice_m
    implicit none
    logical, intent(in) :: flag_field(nx, ny, nz)
    integer, intent(in) :: flag_value

    logical :: fieldg(nx+1, ny+1, nz+1)
    logical :: flagt
    integer i
    integer x, y, z

    fieldg = .false. 
    fieldg(1:nx, 1:ny, 1:nz) = flag_field(1:nx, 1:ny, 1:nz)
    call ghostzone_logical(fieldg, nx, ny, nz, 0, 1, 0, 1, 0, 1)

    do i = 1, fill
       if(state(i).ne.HEALTHY) cycle
       !----------------------------------------
       x = int(loc(i, 1))
       y = int(loc(i, 2))
       z = int(loc(i, 3))
       x = x-xid*nx
       y = y-yid*ny
       z = z-zid*nz
       !----------------------------------------
       flagt = &
            fieldg(x  , y  , z  ) .or. &
            fieldg(x+1, y  , z  ) .or. &
            fieldg(x  , y+1, z  ) .or. &
            fieldg(x+1, y+1, z  ) .or. &
            fieldg(x  , y  , z+1) .or. &
            fieldg(x+1, y  , z+1) .or. &
            fieldg(x  , y+1, z+1) .or. &
            fieldg(x+1, y+1, z+1) 
       !----------------------------------------
       if(flagt) state(i) = flag_value
    end do

    return
  end subroutine flag_tracer


  !----------------------------------------
  subroutine scale_onedirn(vel, dirn, unif_grid, ng, scale_1g)
    implicit none
    real, intent(inout) :: vel(ARRAY_SIZE)
    integer, intent(in) :: dirn, unif_grid, ng
    real, intent(in) :: scale_1g(ng)

    real ds, r, scl
    integer i, x

    if(unif_grid .ne. 1) then
       ds = real(ng-1)
    else
       ds = real(ng-1)*scale_1g(1)
    end if
    vel(1:fill) = vel(1:fill)*ds

    if(unif_grid.eq.1) return

    do i = 1, fill
       if(state(i).ne.HEALTHY) cycle
       x = int(loc(i, dirn))
       r = loc(i, dirn) - real(x)
       scl = (1.0-r)*scale_1g(x) + r*scale_1g(x+1)
       ! x+1? why not? Or would you go non-uniform in a periodic direction?
       vel(i) = vel(i)*scl
    end do
    return
  end subroutine scale_onedirn

  !----------------------------------------------------------------------
  subroutine grid_coords(loc, xloc)
    use grid_m, only: xg, yg, zg
    use param_m
    implicit none
    real, intent(in) :: loc(3)
    real, intent(out) :: xloc(3)

    xloc(1) = grid_one_dirn(loc(1), xg, periodic_x, nx_g)
    xloc(2) = grid_one_dirn(loc(2), yg, periodic_y, ny_g)
    xloc(3) = grid_one_dirn(loc(3), zg, periodic_z, nz_g)

    return

  contains
    !----------------------------------------
    function grid_one_dirn(loc, xg, prdc, nxg) result(x)
      implicit none
      real, intent(in) :: loc, xg(nxg)
      integer, intent(in) :: prdc, nxg
      integer n
      real x, r

      n = int(loc)
      r = loc - real(n)
      if(n.eq.nxg .and. prdc .eq. 1) then
         x = (1.0-r)*xg(n) + r*(2.0*xg(n)-xg(n-1))
      else
         x = (1.0-r)*xg(n) + r*xg(n+1)
      end if

      return
    end function grid_one_dirn

  end subroutine grid_coords

  !----------------------------------------------------------------------
  subroutine compact_arrays(allowed_void)
    implicit none
    integer, optional, intent(in) :: allowed_void !Permissible level of void 
    integer i, cnt_void 

    call trim_empty_tail
    if(fill.le.1) return
    !Check if a compaction is required
    cnt_void = count(state(1:fill).eq.UNOCCUPIED)
    if(cnt_void.eq.0) return
    if(present(allowed_void)) then
       if(cnt_void .le. allowed_void) return 
    end if
    ! Now compact
    i = 0
    fill_loop: do 
       i = i+1
       if(state(i).ne.UNOCCUPIED) cycle fill_loop
       if(i.ge.fill) exit fill_loop
       !Move the tail data to this location
       call copy_tail_data(i)
       state(fill) = UNOCCUPIED
       fill = fill - 1
       call trim_empty_tail
    end do fill_loop

    return
  contains
    !----------------------------------------
    subroutine trim_empty_tail
      implicit none
      do
         if(fill<1) return
         if(state(fill).ne.UNOCCUPIED) exit
         fill = fill-1
      end do
    end subroutine trim_empty_tail
    !----------------------------------------
    subroutine copy_tail_data(i)
      implicit none
      integer, intent(in) :: i
      ssn(i) = ssn(fill)
      age(i) = age(fill)
      dob(i) = dob(fill)
      state(i) = state(fill)
      loc(i,:) = loc(fill,:)
      loc_prev(i,:) = loc_prev(fill,:)
      dloc(i,:) = dloc(fill,:)
    end subroutine copy_tail_data

  end subroutine compact_arrays

  !----------------------------------------------------------------------
  subroutine disable_unhealthy
    implicit none
    integer i
    do i = 1, fill
       if(state(i).ne.HEALTHY .and. state(i).ne.UNOCCUPIED) state(i) = UNOCCUPIED
    end do
    call print_state
  end subroutine disable_unhealthy

  !----------------------------------------------------------------------
  subroutine readwrite_tracer_savefile(io, io_savefile, input)
    !ESR 30Apr08:the routine has been modified to allow an optional
    !  'mode' parmeter
    !Mode ='n': None, only write/read the basic ID and position data
    !     ='f': also write the Field data (U_i,Y_j,T)
    !     ='g': also write the Gradient components of (U_i,Y_j,T)
    !     ='a': also write All field And gradients
    use topology_m
    !RS: In the following choose a good direction for aggregation. 
    !RS: Also pick the filename extension in write_tracer_savefile accordingly
    use topology_m, only: comm=>ycomm, id=>yid, pes=>ypes 
    use reference_m
    use runtime_m, only: time
    use variables_m, only: u,yspecies,temp,q
    use mixFrac_m
    use param_m, only: nx,ny,nz,n_spec
    implicit none
    integer io, io_savefile
    character input*1, dir_ext*1, spec_name*2
    integer, dimension(pes) :: fill_g, displ
    integer(kind=8), dimension(pes) :: ssn_g
    integer(kind=4), dimension(pes) :: ssn_g_sngl
    integer :: fillsum, n
    integer(kind=4), allocatable, dimension(:) :: iarrayg
    integer(kind=8), allocatable, dimension(:) :: iarrayg_dbl
    real, allocatable, dimension(:) :: rarrayg
    real(kind=4), allocatable, dimension(:) :: dummy1
    real :: rdummy
    real, dimension(nx,ny,nz) :: chi
    character :: dirn_tag(3) = (/'x', 'y', 'z'/)
    character :: readwrite_mode
    real, allocatable :: seed_g(:)

    real, dimension(nx,ny,nz) :: scalar

    if(input .ne. 'r' .and. input .ne. 'w') then
       if(myid .eq. 0) & 
            write(io, *) 'Invalid setting for input in readwrite_tracer_savefile'
       call terminate_run(io, 0)
    end if

    if(input .eq. 'w') then
        ! compact arrays before writing...
        call compact_arrays
       ! WRITING ----------------------------------------
       call mpi_allreduce(fill, fillsum, 1, MPI_INTEGER, MPI_SUM, comm, ierr)
       call mpi_gather(fill, 1, MPI_INTEGER, fill_g, 1, MPI_INTEGER, 0, comm, ierr)
       !dbl  call mpi_gather(ssn_cntr, 1, MPI_INTEGER, ssn_g, 1, MPI_INTEGER, 0, comm, ierr)
       ssn_cntr_dbl=ssn_cntr     !dbl
       call mpi_gather(ssn_cntr_dbl, 1, MPI_INTEGER8, ssn_g, 1, MPI_INTEGER8, 0, comm, ierr) !dbl
       call random_seed(get=new_seed)
       if(id .eq. 0) allocate(seed_g(pes*seed_size))
       call mpi_gather(new_seed, seed_size, MPI_INTEGER, seed_g, seed_size, MPI_INTEGER, 0, comm, ierr)
       if(id .eq. 0) then
          write(io_savefile) time
          write(io_savefile) fillsum
          write(io_savefile) fill_g
          write(io_savefile) ssn_g
          write(io_savefile) seed_g
       end if
       if(id .eq. 0) deallocate (seed_g)
       if(fillsum .eq. 0) goto 501
       if(id .eq. 0) then
          allocate(iarrayg(fillsum))
          allocate(iarrayg_dbl(fillsum))
          allocate(rarrayg(fillsum))
          allocate(dummy1(fillsum))
          displ(1) = 0
          do n = 2, pes
             displ(n)=displ(n-1)+fill_g(n-1)
          end do
       end if
       readwrite_mode = 'd' !Restart data to be written in real*8
       call write_int_array_dbl(ssn, 'SSN', 1.0, io_savefile)
       call write_ryl_array(age, 'age', time_ref, io_savefile)
       call write_ryl_array(dob, 'dob', time_ref, io_savefile)
       call write_int_array(state, 'state', 1.0, io_savefile)
       do n = 1, 3
          write(dir_ext,'(i1.1)') n
          call write_ryl_array(loc(:,n), 'loc'//dir_ext, 1.0, io_savefile)
          !call write_ryl_array(loc_prev(:,n), 'loc_prev'//dir_ext, 1.0, io_savefile)
          call write_ryl_array(dloc(:,n), 'dloc'//dir_ext, 1.0, io_savefile)
       end do

501    continue  

       if(trcsngl .eq. 1) readwrite_mode = 's' !Remaining data was chosen to be written in real*4

       if(mode.eq.'f'.or.mode.eq.'a')then
          !Write more scalar fields. 
          do n=1,3
             write(dir_ext,'(i1.1)') n
             call interpolate_write_ryl_array(u(:,:,:,n), 'U'//dir_ext, a_ref, io_savefile) !Velocity
          enddo
          call interpolate_write_ryl_array(q(:,:,:,4,1), 'rho', rho_ref, io_savefile) !density
          call interpolate_write_ryl_array(temp(:,:,:), 'T', t_ref, io_savefile) !temperature
          do n=1,n_spec
             write(spec_name,'(i2.2)') n
             call interpolate_write_ryl_array(yspecies(:,:,:,n), 'Y'//spec_name, 1.0, io_savefile) !Mass fraction
          enddo
          ! you could miss out N2 since we know they all sum to 1.0

          ! Save mixture fraction
          if( save_mf_chi.eq.'m' .or. save_mf_chi.eq.'b' ) then
             call specToMixfr( yspecies )
             call interpolate_write_ryl_array(MixFrac, 'mixfrac', 1.0, io_savefile) ! mixture fraction
          end if
          
       endif

       if(mode.eq.'g'.or.mode.eq.'a')then
          !Write gradients
          do n=1,3
             write(dir_ext,'(i1.1)') n
             call grad_write_ryl_array(u(:,:,:,n), 'U'//dir_ext, a_ref, io_savefile) !Velocity
          enddo
          call grad_write_ryl_array(q(:,:,:,4,1), 'rho', rho_ref, io_savefile) !Density
          call grad_write_ryl_array(temp(:,:,:), 'T', t_ref, io_savefile) !Temperature
          do n=1,n_spec
             write(spec_name,'(i2.2)') n
             call grad_write_ryl_array(yspecies(:,:,:,n), 'Y'//spec_name, 1.0, io_savefile) !Mass fraction
          enddo

          if( save_mf_chi.eq.'b' ) then
             call grad_write_ryl_array(MixFrac, 'mixfrac', 1.0, io_savefile) ! MF grad component
             call computeScalarDissipationRate(io, chi)
             call interpolate_write_ryl_array(chi, 'chi', 1.0, io_savefile) ! scalar dissipation rate
          end if
       endif


          

       if(fillsum .eq. 0) return

       if(id .eq. 0) then
          deallocate(iarrayg)
          deallocate(iarrayg_dbl)
          deallocate(rarrayg)
          deallocate(dummy1)
       end if

       ! END WRITING ----------------------------------------
    else if (input .eq. 'r') then
       ! READING ----------------------------------------
       if(id .eq. 0) allocate(seed_g(pes*seed_size))
       if(id .eq. 0) then
          read(io_savefile) rdummy
          read(io_savefile) fillsum
          read(io_savefile) fill_g

          if( read_single .eq. 1) then
             read(io_savefile) ssn_g_sngl       ! Read in single data conv
             ssn_g=ssn_g_sngl                   ! to dbl
          else
             read(io_savefile) ssn_g
          endif

          read(io_savefile) seed_g
       end if

       call mpi_bcast(fillsum, 1, MPI_INTEGER, 0, comm, ierr)
       call mpi_scatter(fill_g, 1, MPI_INTEGER, fill, 1, MPI_INTEGER, 0, comm, ierr)
       call mpi_scatter(ssn_g, 1, MPI_INTEGER8, ssn_cntr_dbl, 1, MPI_INTEGER8, 0, comm, ierr)  !dbl
       ssn_cntr=ssn_cntr_dbl
       call mpi_scatter(seed_g, seed_size, MPI_INTEGER, new_seed, seed_size, MPI_INTEGER, 0, comm, ierr)
       call random_seed(put=new_seed)
       if(id .eq. 0) deallocate (seed_g)
       if(fillsum .eq. 0) return
       if(id .eq. 0) then
          allocate(iarrayg(fillsum))
          allocate(iarrayg_dbl(fillsum))
          allocate(rarrayg(fillsum))
          allocate(dummy1(fillsum))
          displ(1) = 0
          do n = 2, pes
             displ(n)=displ(n-1)+fill_g(n-1)
          end do
       end if
       readwrite_mode = 'd' !Restart data to be read in real*8
       call read_int_array_dbl(ssn, 'SSN', 1.0)
       !dbl  call read_int_array(ssn_sngl, 'SSN', 1.0)   ! for now read the single data and do the conversion
       !dbl  ssn=ssn_sngl      !dbl

       call read_ryl_array(age, 'age', time_ref)
       call read_ryl_array(dob, 'dob', time_ref)

       !dbl  do a conversion of the corrupted ssn based on its sign and particle dob.

       call read_int_array(state, 'state', 1.0)
       do n = 1, 3
          write(dir_ext,'(i1.1)') n
          call read_ryl_array(loc(:,n), 'loc'//dir_ext, 1.0)
          !call read_ryl_array(loc_prev(:,n), 'loc_prev'//dir_ext, 1.0)
          loc_prev(1:fill,:) = loc(1:fill,:)
          call read_ryl_array(dloc(:,n), 'dloc'//dir_ext, 1.0)
       end do
       if(id .eq. 0) then
          deallocate(iarrayg)
          deallocate(iarrayg_dbl)
          deallocate(rarrayg)
          deallocate(dummy1)
       end if
       ! END READING ----------------------------------------
    end if

    return

  contains
    !----------------------------------------
    !All processors need to call this routine coz it calls interpolate
    !But only those with nonzero fillsum will call the write_ryl_array
    subroutine interpolate_write_ryl_array(field, varname, ref, io_file)
      implicit none
      real, intent(in), dimension(nx, ny, nz) :: field
      integer, intent(in) :: io_file
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      real, dimension(ARRAY_SIZE) :: sc_t
      call interpolate(field, sc_t)
      if(fillsum.ne.0) call write_ryl_array(sc_t(:), varname, ref, io_file)
      return
    end subroutine interpolate_write_ryl_array

    !----------------------------------------
    !All processors need to call this routine coz it calls gradient
    subroutine grad_write_ryl_array(field, varname, ref, io_file)
      implicit none
      real, intent(in), dimension(nx, ny, nz) :: field
      integer, intent(in) :: io_file
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      real, dimension(nx, ny, nz, 3) :: grad
      integer i
      call computescalargradient(field, grad)
      do i = 1, 3
         call interpolate_write_ryl_array(grad(:,:,:,i), 'd'//varname//'d'//dirn_tag(i), ref/l_ref, io_file)
      end do
      return
    end subroutine grad_write_ryl_array

    !----------------------------------------
    subroutine write_int_array(array, varname, ref, io_file)
      implicit none
      integer, intent(in), dimension(1:fill) :: array
      integer, intent(in) :: io_file
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      character :: varfile*100
      varfile = varname
      call mpi_gatherv(array, fill, MPI_INTEGER, &
           iarrayg, fill_g, displ, MPI_INTEGER, 0, comm, ierr)
      if(id .eq. 0) then
         write(io_file) varfile
         write(io_file) ref
         write(io_file) iarrayg
      end if
      return
    end subroutine write_int_array
    !----------------------------------------
    subroutine write_int_array_dbl(array, varname, ref, io_file)
      implicit none
      integer(kind=8), intent(in), dimension(1:fill) :: array
      integer, intent(in) :: io_file
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      character :: varfile*100
      varfile = varname
      !dbl  call mpi_gatherv(array, fill, MPI_INTEGER, &
      !dbl          iarrayg, fill_g, displ, MPI_INTEGER, 0, comm, ierr)
      call mpi_gatherv(array, fill, MPI_INTEGER8, &
           iarrayg_dbl, fill_g, displ, MPI_INTEGER8, 0, comm, ierr)
      if(id .eq. 0) then
         write(io_file) varfile
         write(io_file) ref
         write(io_file) iarrayg_dbl
      end if
      return
    end subroutine write_int_array_dbl
    !----------------------------------------
    subroutine write_ryl_array(array, varname, ref, io_file)
      implicit none
      real, intent(in), dimension(1:fill) :: array
      character(*), intent(in) :: varname
      integer, intent(in) :: io_file
      real, intent(in) :: ref
      character :: varfile*100
      varfile = varname
      call mpi_gatherv(array, fill, MPI_REAL8, &
           rarrayg, fill_g, displ, MPI_REAL8, 0, comm, ierr)
      if(id .eq. 0) then
         write(io_file) varfile
         write(io_file) ref
         if(readwrite_mode.eq.'s')then
            dummy1 = rarrayg; write(io_file) dummy1
         else
            write(io_file) rarrayg
         endif
      end if
      return
    end subroutine write_ryl_array
    !----------------------------------------
    subroutine read_int_array(array, varname, ref)
      implicit none
      integer, intent(out), dimension(1:fill) :: array
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      character :: varfile*100
      real :: reffile
      if(id .eq. 0) then 
         read(io_savefile) varfile
         read(io_savefile) reffile
         if(trim(varfile).ne.trim(varname) .or. reffile.ne.ref) then
            print *, 'variable name and reference value do not match'
            return
         end if
         read(io_savefile) iarrayg
      end if
      call mpi_scatterv(iarrayg, fill_g, displ, MPI_INTEGER, &
           array, fill, MPI_INTEGER, 0, comm, ierr)
      return
    end subroutine read_int_array
    !----------------------------------------
    subroutine read_int_array_dbl(array, varname, ref)
      implicit none
      integer(kind=8), intent(out), dimension(1:fill) :: array
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      character :: varfile*100
      real :: reffile
      if(id .eq. 0) then 
         read(io_savefile) varfile
         read(io_savefile) reffile
         if(trim(varfile).ne.trim(varname) .or. reffile.ne.ref) then
            print *, 'variable name and reference value do not match'
            return
         end if
         read(io_savefile) iarrayg_dbl
      end if
      call mpi_scatterv(iarrayg_dbl, fill_g, displ, MPI_INTEGER8, &
           array, fill, MPI_INTEGER8, 0, comm, ierr)
      return
    end subroutine read_int_array_dbl
    !----------------------------------------
    subroutine read_ryl_array(array, varname, ref)
      implicit none
      real, intent(out), dimension(1:fill) :: array
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      character :: varfile*100
      real :: reffile
      if(id .eq. 0) then 
         read(io_savefile) varfile
         read(io_savefile) reffile
         if(trim(varfile).ne.trim(varname) .or. reffile.ne.ref) then
            print *, 'variable name and reference value do not match'
            return
         end if
         if(readwrite_mode.eq.'s')then
            read(io_savefile) dummy1
            rarrayg=dummy1
         else
            read(io_savefile) rarrayg
         endif
      end if
      call mpi_scatterv(rarrayg, fill_g, displ, MPI_REAL8, &
           array, fill, MPI_REAL8, 0, comm, ierr)
      return
    end subroutine read_ryl_array
    !----------------------------------------
  end subroutine readwrite_tracer_savefile

  !----------------------------------------------------------------------
  subroutine write_tracer_savefile(io)
    use topology_m, only: myid, gcomm, ierr, xz_id, yid
    use runtime_m, only: time, i_time
    use reference_m, only: time_ref
    implicit none
    integer, intent(in) :: io
    character id_ext*5, time_ext*10, filename*200, dirname*200, dirname2*200, dirname3*200  !5sf

    call mpi_barrier(gcomm, ierr)
    call compact_arrays(fill/10)
    call print_state
    !5sf write(time_ext, '(1pe9.3)') time*time_ref
    write(time_ext, '(1pe10.4)')time*time_ref
    write(id_ext, '(I5.5)') xz_id !RS: This has to match the direction chosen in readwrite_
    dirname = '../data/'//'tracer-'//trim(time_ext)//'/'
    !RSA dirname2 = '../data/'//'scalar-'//trim(time_ext)//'/'
    !RSA dirname3 = '../data/'//'grad-'//trim(time_ext)//'/'

    if(myid .eq. 0) &
#ifdef SYSTEMCALLWONTWORK
    call makedirectory(trim(dirname)//char(0))
    !RSA  if(mode.eq.'f'.or.mode.eq.'a') call makedirectory(trim(dirname2)//char(0))
    !RSA  if(mode.eq.'g'.or.mode.eq.'a') call makedirectory(trim(dirname3)//char(0))
#else
    call execute_command( 'mkdir '//trim(dirname))
    !RSA  if(mode.eq.'f'.or.mode.eq.'a') call execute_command( 'mkdir '//trim(dirname2))
    !RSA  if(mode.eq.'g'.or.mode.eq.'a') call execute_command( 'mkdir '//trim(dirname3))
#endif

    call mpi_barrier(gcomm, ierr)

    if(myid .eq. 0) write(io,100) 'writing tracer savefiles for: i_time = ',&
         i_time, ', time = ',time*time_ref,' (sec)'
100 format(1x, a, i7, a, 1pe10.4, a)   !5sf

    filename = trim(dirname)//'tracer.'//trim(id_ext)
    if(yid.eq.0) &
         open(unit=620, file=trim(filename), status='unknown', form='unformatted')
    ! Commented by Ramanan. Will use a single binary file for all output. 
    !if(mode.eq.'f'.or.mode.eq.'a')then
    ! filename = trim(dirname2)//'scalar.'//trim(id_ext)
    ! if(yid.eq.0) &
    !   open(unit=621, file=trim(filename), status='unknown', form='unformatted')
    !endif
    !if(mode.eq.'g'.or.mode.eq.'a')then
    ! filename = trim(dirname3)//'grad.'//trim(id_ext)
    ! if(yid.eq.0) &
    !   open(unit=622, file=trim(filename), status='unknown', form='unformatted')
    !endif

    call readwrite_tracer_savefile(io, 620, 'w')


    if(yid.eq.0) close(620)
    !if(mode.eq.'f'.or.mode.eq.'a')then
    !  if(yid.eq.0) close(621)
    !endif
    !if(mode.eq.'g'.or.mode.eq.'a')then
    !  if(yid.eq.0) close(622)
    !endif


    call mpi_barrier(gcomm, ierr)
    call disable_unhealthy !Tracers in bad state have been written to the disk
    if(myid .eq. 0) call write_header(io, '-')

    return
  end subroutine write_tracer_savefile

  !----------------------------------------------------------------------
  subroutine read_tracer_savefile(io, dirname)
    use topology_m
    use reference_m, only: time_ref
    implicit none
    integer, intent(in) :: io
    character(*), intent(in) :: dirname
    character id_ext*5, filename*200
    logical exist, exist_g
    integer n

    call mpi_barrier(gcomm, ierr)
    write(id_ext, '(I5.5)') xz_id

    if(myid .eq. 0) write(io,*) 'Reading tracer savefiles from ', dirname

    filename = trim(dirname)//'/tracer.'//trim(id_ext)

    if(yid .eq. 0) then 
       inquire(file=trim(filename), exist=exist)
       if(.not. exist) write(io, *) filename, 'does not exist'
       call mpi_reduce(exist, exist_g, 1, MPI_LOGICAL, MPI_LAND, 0, xz_comm, ierr)
    end if
    call mpi_barrier(gcomm, ierr)
    call mpi_bcast(exist_g, 1, MPI_LOGICAL, 0, gcomm, ierr)
    if(.not. exist_g) call terminate_run(io, 0)

    if(yid.eq.0) &
         open(unit=620, file=trim(filename), status='old', form='unformatted')
    call readwrite_tracer_savefile(io, 620, 'r')
    ! To read in from tracer tecplot file - ascii format?
    !do  n  = 0, npes-1
    !  call mpi_barrier(gcomm, ierr)
    !  if(myid .ne. n) cycle
    !  call read_tracer_savefile_tec(io, 620)
    !end do
    if(yid.eq.0) close(620)

    call mpi_barrier(gcomm, ierr)
    call disable_unhealthy !Tracers in bad state were read, but no need to carry it around
    if(myid .eq. 0) call write_header(io, '-')

    return
  end subroutine read_tracer_savefile

  !----------------------------------------------------------------------
#ifdef _CRAYFTN
  ! Cray fortran has trouble with this routine; deactivate for now...
  subroutine write_tracer_tecplot()
  implicit none
  return
  end subroutine write_tracer_tecplot

#else
  subroutine write_tracer_tecplot(io)
    use runtime_m, only: time
    use reference_m
    use topology_m, only: myid, npes, gcomm, ierr, xz_id
    use variables_m, only: temp,yspecies
    use param_m, only: n_spec,nx,ny,nz
    use grid_m, only: scale_1x, scale_1y, scale_1z

    implicit none
    integer, intent(in) :: io
    integer, parameter :: iopt = 823
    !5sf character time_ext*9
    character time_ext*10, myid_ext*5,more_ext*2

    integer n
    real xloc(3)

    real vel(ARRAY_SIZE, 3)
    real yspec(ARRAY_SIZE, n_spec)
    real temppart(ARRAY_SIZE)

    real gradwork(ARRAY_SIZE)
    real Ngradmag(ARRAY_SIZE)
    real Tgradmag(ARRAY_SIZE)

    real scalar(nx,ny,nz)

    integer pz !RSA

    !find temperature gradients

    !x
    call derivative_x( nx,ny,nz, temp, scalar, scale_1x, 1 )
    call interpolate(scalar, gradwork)
    Tgradmag=gradwork*gradwork
    !y  
    call derivative_y( nx,ny,nz, temp, scalar, scale_1y, 1 )
    call interpolate(scalar, gradwork)
    Tgradmag=Tgradmag+gradwork*gradwork
    !z
    call derivative_z( nx,ny,nz, temp, scalar, scale_1z, 1 )
    call interpolate(scalar, gradwork)
    Tgradmag=Tgradmag+gradwork*gradwork

    Tgradmag=(Tgradmag**0.5)*t_ref/l_ref

    !write species mass fraction gradients

    !x
    call derivative_x( nx,ny,nz, yspecies(:,:,:,n_spec), scalar, scale_1x, 1 )
    call interpolate(scalar, gradwork)
    Ngradmag=gradwork*gradwork 
    !y  
    call derivative_y( nx,ny,nz, yspecies(:,:,:,n), scalar, scale_1y, 1 )
    call interpolate(scalar, gradwork)
    Ngradmag=gradwork*gradwork
    !z
    call derivative_z( nx,ny,nz, yspecies(:,:,:,n), scalar, scale_1z, 1 )
    call interpolate(scalar, gradwork)
    Ngradmag=gradwork*gradwork

    Ngradmag=(Ngradmag**0.5)/l_ref


    call print_state
    call compute_physical_velocity(vel,1)  !since stage=1 it does not include the stochastic component

    do n=1,n_spec
       call interpolate(yspecies(:,:,:,n), yspec(:,n))
    enddo
    call interpolate(temp(:,:,:), temppart(:))


    if(myid .eq. 0) write(io, *) 'Writing tracer tecplot files'

    write(myid_ext, '(i5.5)') xz_id
    !5sf write(time_ext,'(1pe9.3)') time*time_ref
    write(time_ext,'(1pe10.4)') time*time_ref
    write(more_ext,'(1i2.2)')n_spec+1

    if(myid .eq. 0) then
       open(unit=iopt, file = '../post/tecplot/tracer-'//'-'//time_ext//'.tec')
       write(iopt, *) 'variables = x[mm] y[mm] z[mm] ssn age dob sx sy sz y1 y2 y3 y4 y5 &
            y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 T[K] gradT[K] gradN2'
       close(iopt)
    end if



    LOOP_PZ: do pz = 0, npes -1 
       call mpi_barrier(gcomm, ierr)
       if(pz .ne. myid) cycle
       if(count(state(1:fill).eq.HEALTHY) .eq. 0) cycle
       open(unit=iopt, file = '../post/tecplot/tracer-'//'-'//time_ext//'.tec', access='append')
       write(iopt, *) 'zone t= "tracer-'//myid_ext//'", datapacking=point'
       do n = 1, fill
          if(state(n) .ne. HEALTHY) cycle
          call grid_coords(loc(n, :), xloc)
          write(iopt,'(3(1pe12.5,1x),1(1i12.12),5(1pe12.5,1x)'//trim(more_ext)//'(1pe12.5,1x))') & 
               xloc(:)*l_ref*1e3, ssn(n), age(n), dob(n), &
               vel(n, 1)*a_ref, vel(n, 2)*a_ref, vel(n, 3)*a_ref, &
               yspec(n,:),temppart(n)*t_ref,Tgradmag,Ngradmag
       end do
       close(iopt)
    end do LOOP_PZ

    return
  end subroutine write_tracer_tecplot
#endif

  !----------------------------------------------------------------------
  subroutine debug_single_tracer(stage)
    use topology_m, only: myid
    use reference_m, only: a_ref, l_ref, time_ref
    use premix_drvd_var_m, only: calculate_progvar
    use param_m, only: nx, ny, nz
    use runtime_m, only: tstep
    implicit none
    integer n
    integer, intent(in) :: stage
    real xloc(3)
    real vel(ARRAY_SIZE, 3), sc_t(ARRAY_SIZE), vel2(ARRAY_SIZE, 3)
    real scalar(nx, ny, nz)

    call compute_physical_velocity(vel,stage) 
    call compute_grid_velocity(vel2,stage)
    call calculate_progvar(scalar,io_tracer)
    call interpolate(scalar, sc_t)

    do n = 1, fill
       if(state(n) .ne. HEALTHY) cycle
       if(ssn(n) .eq. 3612) then
          call grid_coords(loc(n, :), xloc)
          write (*,'(1(A), 3(1i5), 13(1pe15.8, 1x))') 'rsa chk j', &
               myid, stage, n, loc(n, :), xloc(:)*l_ref*1e3, &
               vel(n, :)*a_ref, vel2(n, :)/time_ref, sc_t(n)
       end if
    end do
  end subroutine debug_single_tracer

  !----------------------------------------------------------------------
  !----------------------------------------------------------------------
#define CRAYFTN
#ifdef CRAYFTN
  subroutine read_tracer_savefile_tec(io, io_savefile)
  implicit none
  integer io, io_savefile
  return
  end subroutine 

#else
  subroutine read_tracer_savefile_tec(io, io_savefile)
    use topology_m
    use reference_m
    use runtime_m, only: time
    use param_m, only: nx,ny,nz,n_spec
    implicit none
    integer io, io_savefile
    character dir_ext*1
    integer, parameter :: pes=40
    integer, dimension(pes) :: fill_g, displ 
    integer(kind=8), dimension(pes) :: ssn_g
    integer(kind=4), dimension(pes) :: ssn_g_sngl
    integer :: fillsum, n
    integer(kind=4), allocatable, dimension(:) :: iarrayg
    integer(kind=8), allocatable, dimension(:) :: iarrayg_dbl
    real, allocatable, dimension(:) :: rarrayg, dummy2
    real, allocatable, dimension(:,:) :: xloc
    real(kind=4), allocatable, dimension(:) :: dummy1
    real :: rdummy
    character :: dirn_tag(3) = (/'x', 'y', 'z'/)
    character :: readwrite_mode
    real, allocatable :: seed_g(:)
    character time_ext*10, myid_ext*5
    integer, parameter :: itec=748, skip=80
    real, dimension(nx,ny,nz) :: scalar

    write(myid_ext, '(i5.5)') xz_id
    write(time_ext,'(1pe10.4)') time*time_ref

    allocate(seed_g(pes*seed_size))
    read(io_savefile) rdummy
    read(io_savefile) fillsum
    read(io_savefile) fill_g
    fill = fillsum
    read(io_savefile) ssn_g
    read(io_savefile) seed_g
    deallocate (seed_g)

    if(xz_id .eq. 0) then
       open(unit=itec, file = '../post/tecplot/tracer-'//time_ext//'.tec')
       write(itec, *) 'variables = x[mm] y[mm] z[mm] ssn age dob state u1 u2 u3 T y1 y2 y3 y4 y5 &
            y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22'
       close(itec)
    end if

    if(fillsum .eq. 0) return
    open(unit=itec, file = '../post/tecplot/tracer-'//time_ext//'.tec', access='append')
    write(itec, *) 'zone t= "tracer-'//myid_ext//'", i=',(fillsum-1)/skip+1,'datapacking=block'
    write(*, *) 'writing for zone', myid_ext
    allocate(iarrayg(fillsum))
    allocate(iarrayg_dbl(fillsum))
    allocate(rarrayg(fillsum))
    allocate(xloc(3, fillsum))
    allocate(dummy1(fillsum))
    allocate(dummy2(fillsum))

    readwrite_mode = 'd' !Restart data to be read in real*8
    call read_int_array_dbl(ssn, 'SSN', 1.0)
    call read_ryl_array(age, 'age', time_ref)
    call read_ryl_array(dob, 'dob', time_ref)
    call read_int_array(state, 'state', 1.0)
    do n = 1, 3
       write(dir_ext,'(i1.1)') n
       call read_ryl_array(loc(:,n), 'loc'//dir_ext, 1.0)
       !call read_ryl_array(loc_prev(:,n), 'loc_prev'//dir_ext, 1.0)
       loc_prev(1:fill,:) = loc(1:fill,:)
       call read_ryl_array(dloc(:,n), 'dloc'//dir_ext, 1.0)
    end do

    do n = 1, fillsum
       call grid_coords(loc(n, :), xloc(1:3,n))
    end do

    write(itec,'(10(1pe10.3,1x))') xloc(1,1:fill:skip)*l_ref*1e3
    write(itec,'(10(1pe10.3,1x))') xloc(2,1:fill:skip)*l_ref*1e3
    write(itec,'(10(1pe10.3,1x))') xloc(3,1:fill:skip)*l_ref*1e3
    write(itec,'(12(1i12,1x))') ssn(1:fill:skip)
    write(itec,'(10(1pe10.3,1x))') age(1:fill:skip)*time_ref
    write(itec,'(10(1pe10.3,1x))') dob(1:fill:skip)*time_ref
    write(itec,'(20(1i2,1x))') state(1:fill:skip)

    if(trcsngl .eq. 1) readwrite_mode = 's' !Remaining data was chosen to be written in real*4
    if(mode.eq.'f'.or.mode.eq.'a')then
       !Write more scalar fields. 
       do n=1,3
          write(dir_ext,'(i1.1)') n
          call read_ryl_array(dummy2, 'U'//dir_ext, a_ref) !Velocity
          write(itec,'(10(1pe10.3,1x))') dummy2(1:fill:skip)*a_ref
       enddo
       call read_ryl_array(dummy2, 'rho', rho_ref) !density
       call read_ryl_array(dummy2, 'T', t_ref) !temperature
       write(itec,'(10(1pe10.3,1x))') dummy2(1:fill:skip)*t_ref
       do n=1,n_spec
          write(dir_ext,'(i1.1)') n
          call read_ryl_array(dummy2, 'Y'//dir_ext, 1.0) !Mass fraction
          write(itec,'(10(1pe10.3,1x))') dummy2(1:fill:skip)
       enddo
       ! you could miss out N2 since we know they all sum to 1.0
    endif
    close(itec)

    deallocate(iarrayg)
    deallocate(iarrayg_dbl)
    deallocate(rarrayg)
    deallocate(xloc)
    deallocate(dummy1)
    deallocate(dummy2)

    return

  contains
    !----------------------------------------
    subroutine read_int_array(array, varname, ref)
      implicit none
      integer, intent(out), dimension(1:fill) :: array
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      character :: varfile*100
      real :: reffile
      read(io_savefile) varfile
      read(io_savefile) reffile
      if(trim(varfile).ne.trim(varname) .or. reffile.ne.ref) then
         print *, 'variable name and reference value do not match'
         return
      end if
      read(io_savefile) iarrayg
      array = iarrayg
      return
    end subroutine read_int_array
    !----------------------------------------
    subroutine read_int_array_dbl(array, varname, ref)
      implicit none
      integer(kind=8), intent(out), dimension(1:fill) :: array
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      character :: varfile*100
      real :: reffile
      read(io_savefile) varfile
      read(io_savefile) reffile
      if(trim(varfile).ne.trim(varname) .or. reffile.ne.ref) then
         print *, 'variable name and reference value do not match'
         return
      end if
      read(io_savefile) iarrayg_dbl
      array = iarrayg_dbl
      return
    end subroutine read_int_array_dbl
    !----------------------------------------
    subroutine read_ryl_array(array, varname, ref)
      implicit none
      real, intent(out), dimension(1:fill) :: array
      character(*), intent(in) :: varname
      real, intent(in) :: ref
      character :: varfile*100
      real :: reffile
      read(io_savefile) varfile
      read(io_savefile) reffile
      if(trim(varfile).ne.trim(varname) .or. reffile.ne.ref) then
         print *, 'variable name and reference value do not match'
         return
      end if
      if(readwrite_mode.eq.'s')then
         read(io_savefile) dummy1
         rarrayg=dummy1
      else
         read(io_savefile) rarrayg
      endif
      array = rarrayg
      return
    end subroutine read_ryl_array
    !----------------------------------------
  end subroutine read_tracer_savefile_tec
#endif

  subroutine reset_mystartssn( count2, comm, rank, size )

    use topology_m   
    implicit none
    integer count2
    integer comm, rank, size

    ! RG 11Dec08 for new ssn assignment
    double precision t1
    t1 =MPI_Wtime()
    call mpi_allgather(count2, 1, MPI_INTEGER, &
         pinsertcount, 1, MPI_INTEGER, &
         comm, ierr)
    if( rank == 0 ) then
       mystartssn = startssn
    else
       mystartssn = startssn +  sum( pinsertcount( 1:rank ) )
    endif
    ssn_cntr_thisinsertion = 0
    mymaxssn = startssn + sum( pinsertcount( 1:rank+1) ) -1! Error checking only
    startssn = startssn + sum( pinsertcount(1:size ) )
    ! RG 11Dec08 for new ssn assignm

  end subroutine reset_mystartssn

  subroutine sync_startssn( )
    use topology_m
    implicit none

    integer maxstartssn

    call mpi_allreduce( startssn, maxstartssn, 1, MPI_INTEGER8, MPI_MAX, gcomm, ierr )
    startssn = maxstartssn
    if( myid == 0 ) then
       write(*,*) 'Next insertion will start with ssn = ', startssn
    endif

  end subroutine sync_startssn

end module tracer_m

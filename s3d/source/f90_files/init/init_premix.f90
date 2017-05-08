#include "globalDefines.h"
!========================================================================================
  subroutine initialize_premix(io,yspecies,temp,pressure,u)
!========================================================================================
! initializes 2D solution from OPPST solution
! input files must be generated with P2DNS
!----------------------------------------------------------------------------------------
  use topology_m
  use chemkin_m, only : rckwrk, ickwrk
  use param_m, only : nx, ny, nz, npx, npy, npz
  use param_m, only : nsc, n_elem, n_spec
  use reference_m
  use thermchem_m
  use runtime_m, only : i_restart

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io
  real yspecies(nx,ny,nz,nsc+1), temp(nx,ny,nz), pressure(nx,ny,nz), u(nx,ny,nz,3)
!----------------------------------------------------------------------------------------
! local declarations

  integer i, j, k, L, m, n

  integer idum, jdum, kdum
  real temp_in, dens_in, velo_in, pres_in, ydns_in
  character*4 ext
  real u1
!----------------------------------------------------------------------------------------
! return if restarting

  if(i_restart==1) return
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing PREMIX solution...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! set in x-direction
!----------------------------------------------------------------------------------------
  if(nx.gt.1) then

! check

  if((npy.gt.1).or.(npz.gt.1)) then
    if(myid.eq.0) then
      write(io,*) 'initialize_1d_flame_test will not work for npy or npz > 1'
    endif
    call terminate_run(io,0)  !must be called by all processors
  endif
!----------------------------------------------------------------------------------------
! open file

  call MPI_Barrier(gcomm,ierr)

  call set_file_extension_string(ext,xid,4)

  open(unit=69,file='../input/premix2dns_'//ext//'.in',status='unknown')

  if(myid==0) then
    write(io,*) 'reading input 1d laminar flame input files...'
    write(io,*)
  endif

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! set variables

  do i=1,nx,1

!   read node location

    read(69,*) idum

    if(idum.ne.i) then
      write(io,*) 'idum does not equal i in routine initialize_flame'
      write(io,*) 'i=',i
      write(io,*) 'idum=',idum
      write(io,*) 'terminating run'
      term_status=1
    endif

!   read temp, dens, velo, and pres and non-dimenionalize

    read(69,*) temp_in; temp_in=temp_in/t_ref
    read(69,*) dens_in; dens_in=dens_in/rho_ref  !not needed
    read(69,*) velo_in; velo_in=velo_in/a_ref
    read(69,*) pres_in; pres_in=pres_in/p_ref

!   set temperature and pressure

    temp(i,1,1)=temp_in
    pressure(i,1,1)=pres_in

!   set u vector

    u(i,1,1,1) = velo_in   !u-velocity
    u(i,1,1,2) = 0.0       !v-velocity
    u(i,1,1,3) = 0.0       !w-velocity

!   read species

    do L=1,nsc+1,1
      read(69,*) ydns_in
      yspecies(i,1,1,L)=ydns_in
    enddo

  enddo

! copy j and k

  do k=1,nz,1
    do j=1,ny,1
      do i=1,nx,1

        temp(i,j,k)=temp(i,1,1)
        pressure(i,j,k)=pressure(i,1,1)

        u(i,j,k,1)=u(i,1,1,1)
        u(i,j,k,2)=u(i,1,1,2)
        u(i,j,k,3)=u(i,1,1,3)

        do L=1,nsc+1,1
          yspecies(i,j,k,L)=yspecies(i,1,1,L)
        enddo

      enddo
    enddo
  enddo

! shift velocity

!  if(npes.ne.1) stop 'cannot shift velocity for multiprocessor runs'
!  u1=u(1,1,1,1)
!  do i=1,nx,1
!    u(i,1,1,1)=u(i,1,1,1)-u1
!  enddo
!----------------------------------------------------------------------------------------
! end x-direction

  endif
!----------------------------------------------------------------------------------------
! set in y-direction
!----------------------------------------------------------------------------------------
  if(ny.gt.1) then

! check

  if((npx.gt.1).or.(npz.gt.1)) then
    if(myid.eq.0) then
      write(io,*) 'initialize_1d_flame_test will not work for npx or npz > 1'
    endif
    call terminate_run(io,0)  !must be called by all processors
  endif
!----------------------------------------------------------------------------------------
! open file

  call MPI_Barrier(gcomm,ierr)

  call set_file_extension_string(ext,yid,4)

  open(unit=69,file='../input/premix2dns_'//ext//'.in',status='unknown')

  if(myid==0) then
    write(io,*) 'reading input 1d laminar flame input files...'
    write(io,*)
  endif

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! set variables

  do j=1,ny,1

!   read node location

    read(69,*) jdum

    if(jdum.ne.j) then
      write(io,*) 'jdum does not equal j in routine initialize_flame'
      write(io,*) 'j=',j
      write(io,*) 'jdum=',jdum
      write(io,*) 'terminating run'
      term_status=1
    endif

!   read temp, dens, velo, and pres and non-dimenionalize

    read(69,*) temp_in; temp_in=temp_in/t_ref
    read(69,*) dens_in; dens_in=dens_in/rho_ref  !not needed
    read(69,*) velo_in; velo_in=velo_in/a_ref
    read(69,*) pres_in; pres_in=pres_in/p_ref

!   set temperature and pressure

    temp(1,j,1)=temp_in
    pressure(1,j,1)=pres_in

!   set u vector

    u(1,j,1,1) = 0.0       !u-velocity
    u(1,j,1,2) = velo_in   !v-velocity
    u(1,j,1,3) = 0.0       !w-velocity

!   read species

    do L=1,nsc+1,1
      read(69,*) ydns_in
      yspecies(1,j,1,L)=ydns_in
    enddo

  enddo

! copy j and k

  do k=1,nz,1
    do j=1,ny,1
      do i=1,nx,1

        temp(i,j,k)=temp(1,j,1)
        pressure(i,j,k)=pressure(1,j,1)

        u(i,j,k,1)=u(1,j,1,1)
        u(i,j,k,2)=u(1,j,1,2)
        u(i,j,k,3)=u(1,j,1,3)

        do L=1,nsc+1,1
          yspecies(i,j,k,L)=yspecies(1,j,1,L)
        enddo

      enddo
    enddo
  enddo
!----------------------------------------------------------------------------------------
! end y-direction

  endif
!----------------------------------------------------------------------------------------
! set in z-direction
!----------------------------------------------------------------------------------------
  if(nz.gt.1) then

! check

  if((npx.gt.1).or.(npy.gt.1)) then
    if(myid.eq.0) then
      write(io,*) 'initialize_1d_flame_test will not work for npx or npy > 1'
    endif
    call terminate_run(io,0)  !must be called by all processors
  endif
!----------------------------------------------------------------------------------------
! open file

  call MPI_Barrier(gcomm,ierr)

  call set_file_extension_string(ext,zid,4)

  open(unit=69,file='../input/premix2dns_'//ext//'.in',status='unknown')

  if(myid==0) then
    write(io,*) 'reading input 1d laminar flame input files...'
    write(io,*)
  endif

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! set variables

  do k=1,nz,1

!   read node location

    read(69,*) kdum

    if(kdum.ne.k) then
      write(io,*) 'kdum does not equal k in routine initialize_flame'
      write(io,*) 'k=',k
      write(io,*) 'kdum=',kdum
      write(io,*) 'terminating run'
      term_status=1
    endif

!   read temp, dens, velo, and pres and non-dimenionalize

    read(69,*) temp_in; temp_in=temp_in/t_ref
    read(69,*) dens_in; dens_in=dens_in/rho_ref  !not needed
    read(69,*) velo_in; velo_in=velo_in/a_ref
    read(69,*) pres_in; pres_in=pres_in/p_ref

!   set temperature and pressure

    temp(1,1,k)=temp_in
    pressure(1,1,k)=pres_in

!   set u vector

    u(1,1,k,1) = 0.0       !u-velocity
    u(1,1,k,2) = 0.0       !v-velocity
    u(1,1,k,3) = velo_in   !w-velocity

!   read species

    do L=1,nsc+1,1
      read(69,*) ydns_in
      yspecies(1,1,k,L)=ydns_in
    enddo

  enddo

! copy j and k

  do k=1,nz,1
    do j=1,ny,1
      do i=1,nx,1

        temp(i,j,k)=temp(1,1,k)
        pressure(i,j,k)=pressure(1,1,k)

        u(i,j,k,1)=u(1,1,k,1)
        u(i,j,k,2)=u(1,1,k,2)
        u(i,j,k,3)=u(1,1,k,3)

        do L=1,nsc+1,1
          yspecies(i,j,k,L)=yspecies(1,1,k,L)
        enddo

      enddo
    enddo
  enddo
!----------------------------------------------------------------------------------------
! end z-direction

  endif
!----------------------------------------------------------------------------------------
! check term status

  call check_term_status(io,0)
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_premix

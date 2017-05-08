#include "globalDefines.h"
! $Id: write_tecplot_skip.f90,v 1.1.2.5.4.1 2006/05/16 22:52:33 rsankar Exp $
!----------------------------------------------------------------------
  subroutine write_tecplot_skip(io,iskip)
!----------------------------------------------------------------------
! routine writes tecplot file. Every iskip'th point is written.
! Only the necessary communication and storage is done.
! Author - Ramanan Sankaran(12/23/04)
! Modified from Evatt's write_tecplot_animate_file
!----------------------------------------------------------------------
  use topology_m
  use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec
  use reference_m
  use grid_m, only : x, y, z
  use variables_m, only : q, u, temp, pressure, yspecies, volum !volum added by ERH
  use chemkin_m, only : species_name, n_species, element_name, n_elements, reaction_rate
  use mixfrac_m, only : mixFrac, specToMixfr

! only for old thermchem (comment for new thermchem):
!  use thermchem_m, only : rr_r

  use runtime_m, only : run_title, time, i_time, tstep

  use work_m, only : vort_mag => work1_1
  use work_m, only : hr => work1_2

  implicit none
!----------------------------------------------------------------------
! declarations passed in

  integer io       !output unit
  integer iskip    !skip to use in writing files

! local declarations

  integer i,j,k,L
  real p_conv_atm

  real, dimension(nx,ny,nz,3) :: vort
  real, dimension(nx,ny,nz) :: dil, work1, work2
  real, dimension(nx,ny,nz) :: chiSDR

! comment out for old thermchem
  real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion

  real, dimension(nx_g) :: x_g
  real, dimension(ny_g) :: y_g
  real, dimension(nz_g) :: z_g
  
  character*9 time_ext
  character*100 filename,filename_short

  integer nx_w, ny_w, nz_w, mskp

!----------------------------------------------------------------------
! return if zero dimensions

  if((nx.eq.1).and.(ny.eq.1).and.(nz.eq.1)) then
    return
  else
    if(myid.eq.0) then
      write(io,*) 'writing tecplot files for:'
      write(io,'(a10,i7)') ' i_time = ',i_time
      write(io,'(a8,1pe9.3,a6)') ' time = ',time*time_ref,' (sec)'
    endif
  endif

  p_conv_atm=p_ref/pres_atm
!----------------------------------------------------------------------
! calculate dilitation

  call computeDivergence(u,dil)

! calculate vorticity

  call calc_vorticity(vort,vort_mag,u)

! calculate reaction rate 

#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif

! calculate heat release

! new thermchem_m:
  call calc_heat_release(rr_r,hr)

! compute mixture fraction
  call specToMixfr( yspecies )  

! chi
  call computeScalarDissipationRate(io,chiSDR)

!----------------------------------------------------------------------
! gather the x,y,z grid to zero process
  if(yz_id.eq.0) call MPI_Gather(x,nx,MPI_REAL8,x_g,nx,MPI_REAL8,0,xcomm,ierr)
  if(xz_id.eq.0) call MPI_Gather(y,ny,MPI_REAL8,y_g,ny,MPI_REAL8,0,ycomm,ierr)
  if(xy_id.eq.0) call MPI_Gather(z,nz,MPI_REAL8,z_g,nz,MPI_REAL8,0,zcomm,ierr)
!----------------------------------------------------------------------

  if (myid==0) then  !only myid==0 writes
!   set time stamp 
    write(time_ext,'(1pe9.3)') time*time_ref 
!   set filename 
    filename_short=trim(run_title)//'.'//trim(time_ext)//'.tec'
    filename='../post/tecplot/'//trim(filename_short)
    open(unit=78,file=trim(filename),status='unknown')
!----------------------------------------------------------------------
!   write title of file

    write(78,*) 'title = "',trim(run_title)//' '//trim(time_ext),'"'
!----------------------------------------------------------------------
!   write variables header

    write(78,*) 'variables = '

! write coordinates labels

    if((nx_g.gt.1).and.(ny_g.eq.1).and.(nz_g.eq.1)) then
      write(78,*) '"x (mm)"'
    elseif((nx_g.eq.1).and.(ny_g.gt.1).and.(nz_g.eq.1)) then
      write(78,*) '"y (mm)"'
    elseif((nx_g.eq.1).and.(ny_g.eq.1).and.(nz_g.gt.1)) then
      write(78,*) '"z (mm)"'
    elseif((nx_g.gt.1).and.(ny_g.gt.1).and.(nz_g.eq.1)) then
      write(78,*) '"x (mm)"', '"y (mm)"'
    elseif((nx_g.gt.1).and.(ny_g.eq.1).and.(nz_g.gt.1)) then
      write(78,*) '"x (mm)"', '"z (mm)"'
    elseif((nx_g.eq.1).and.(ny_g.gt.1).and.(nz_g.gt.1)) then
      write(78,*) '"y (mm)"', '"z (mm)"'
    elseif((nx_g.gt.1).and.(ny_g.gt.1).and.(nz_g.gt.1)) then
      write(78,*) '"x (mm)"', '"y (mm)"', '"z (mm)"'
    endif

!   write variables labels

    write(78,*) '"u (cm/s)"'
    write(78,*) '"v (cm/s)"'
    write(78,*) '"w (cm/s)"'
!    write(78,*) '"rho (g/cm^3)"'
    write(78,*) '"T (K)"'
    write(78,*) '"P (atm)"'
!    write(78,*) '"dil (1/s)"'
!    write(78,*) '"vort_x (1/s)"'
!    write(78,*) '"vort_y (1/s)"'
!    write(78,*) '"vort_z (1/s)"'
!    write(78,*) '"vort mag (1/s)"'
    write(78,*) '"heat release (J/m^3/s)"'
    write(78,*) '"mixture fraction"'
    write(78,*) '"Chi (1/s)"'
    do L=1,n_species
        write(78,*) '"Y '//trim(species_name(L))//'"'
    enddo
!    do L=1,n_species,1
!        write(78,*) '"RR '//trim(species_name(L))//' (1/s)"'
!    enddo

!   set dimensions of written data
!----------------------------------------------------------------------
!nx_w, ny_w and nz_w will be the mesh size in the final tecplot file.

    mskp = int((nx_g-1)/iskip)
    if(nx_g .eq. 1+mskp*iskip) then
      nx_w=1+mskp
    else
      nx_w=2+mskp
    endif

    mskp = int((ny_g-1)/iskip)
    if(ny_g .eq. 1+mskp*iskip) then
      ny_w=1+mskp
    else
      ny_w=2+mskp
    endif

    mskp = int((nz_g-1)/iskip)
    if(nz_g .eq. 1+mskp*iskip) then
      nz_w=1+mskp
    else
      nz_w=2+mskp
    endif
    !nz_w = 1
    nz_w = 2

! write zone information

  if((nx_g.gt.1).and.(ny_g.eq.1).and.(nz_g.eq.1)) then
    write(78,1) nx_w
  elseif((nx_g.eq.1).and.(ny_g.gt.1).and.(nz_g.eq.1)) then
    write(78,1) ny_w
  elseif((nx_g.eq.1).and.(ny_g.eq.1).and.(nz_g.gt.1)) then
    write(78,1) nz_w
  elseif((nx_g.gt.1).and.(ny_g.gt.1).and.(nz_g.eq.1)) then
    write(78,2) nx_w, ny_w
  elseif((nx_g.gt.1).and.(ny_g.eq.1).and.(nz_g.gt.1)) then
    write(78,2) nx_w, nz_w
  elseif((nx_g.eq.1).and.(ny_g.gt.1).and.(nz_g.gt.1)) then
    write(78,2) ny_w, nz_w
  elseif((nx_g.gt.1).and.(ny_g.gt.1).and.(nz_g.gt.1)) then
    write(78,3) nx_w, ny_w, nz_w
  endif

!----------------------------------------------------------------------
! write coordinate data

  if((nx_g.gt.1).and.(ny_g.eq.1).and.(nz_g.eq.1)) then
    write(78,9) (x_g(1+(i-1)*iskip)*l_ref*1e3, i=1,nx_w-1), x_g(nx_g)*l_ref*1e3
  elseif((nx_g.eq.1).and.(ny_g.gt.1).and.(nz_g.eq.1)) then
    write(78,9) (y_g(1+(j-1)*iskip)*l_ref*1e3, j=1,ny_w-1), y_g(ny_g)*l_ref*1e3
  elseif((nx_g.eq.1).and.(ny_g.eq.1).and.(nz_g.gt.1)) then
    write(78,9) (z_g(1+(k-1)*iskip)*l_ref*1e3, k=1,nz_w-1), z_g(nz_g)*l_ref*1e3
  elseif((nx_g.gt.1).and.(ny_g.gt.1).and.(nz_g.eq.1)) then
    write(78,9) & 
     ( (x_g(1+(i-1)*iskip)*l_ref*1e3, i=1,nx_w-1), x_g(nx_g)*l_ref*1e3, j=1,ny_w ), &
     ( (y_g(1+(j-1)*iskip)*l_ref*1e3, i=1,nx_w),j=1,ny_w-1), & 
       (y_g(ny_g)*l_ref*1e3, i=1, nx_w)
  elseif((nx_g.gt.1).and.(ny_g.eq.1).and.(nz_g.gt.1)) then
    write(78,9) & 
     ( (x_g(1+(i-1)*iskip)*l_ref*1e3, i=1,nx_w-1), x_g(nx_g)*l_ref*1e3, k=1,nz_w ), &
     ( (z_g(1+(k-1)*iskip)*l_ref*1e3, i=1,nx_w),k=1,nz_w-1), & 
       (z_g(nz_g)*l_ref*1e3, i=1, nx_w)
  elseif((nx_g.eq.1).and.(ny_g.gt.1).and.(nz_g.gt.1)) then
    write(78,9) & 
     ( (y_g(1+(j-1)*iskip)*l_ref*1e3, j=1,ny_w-1), y_g(ny_g)*l_ref*1e3, k=1,nz_w ), &
     ( (z_g(1+(k-1)*iskip)*l_ref*1e3, j=1,ny_w),k=1,nz_w-1), & 
       (z_g(nz_g)*l_ref*1e3, j=1, ny_w)
  elseif((nx_g.gt.1).and.(ny_g.gt.1).and.(nz_g.gt.1)) then
    write(78,9) & 
     ( ( (x_g(1+(i-1)*iskip)*l_ref*1e3, i=1,nx_w-1), x_g(nx_g)*l_ref*1e3,  &
                                                    j=1,ny_w), k=1,nz_w ), &
     ( ( (y_g(1+(j-1)*iskip)*l_ref*1e3, i=1,nx_w),j=1,ny_w-1), & 
       (y_g(ny_g)*l_ref*1e3, i=1, nx_w), k=1,nz_w) , &
     ( ( (z_g(1+(k-1)*iskip)*l_ref*1e3, i=1,nx_w), j=1,ny_w),k=1,nz_w-1), & 
     ( (z_g(nz_g)*l_ref*1e3, i=1,nx_w), j=1,ny_w)
  endif
!----------------------------------------------------------------------
  endif ! myid ==0 writing

!----------------------------------------------------------------------
! write variable data - all processor must call as routine does communication
  do L=1,3
!  do L=1,2
    call write_tecplot_single_skip(78,u(:,:,:,L),a_ref,iskip)
  enddo
!  call write_tecplot_single_skip(78,q(:,:,:,4,1),rho_ref,iskip)
  call write_tecplot_single_skip(78,temp,t_ref,iskip)
  call write_tecplot_single_skip(78,pressure,p_conv_atm,iskip)
!  call write_tecplot_single_skip(78,dil,1.0/time_ref,iskip)
!  do L=1,3
!    call write_tecplot_single_skip(78,vort(:,:,:,L),1.0/time_ref,iskip)
!  enddo
!  call write_tecplot_single_skip(78,vort_mag(:,:,:),1.0/time_ref,iskip)
  call write_tecplot_single_skip(78,hr,-hr_ref,iskip)
  call write_tecplot_single_skip(78,mixfrac,1.0,iskip)
  call write_tecplot_single_skip(78,chiSDR,1.0,iskip)
  do L=1,n_species
      call write_tecplot_single_skip(78,yspecies(:,:,:,L),1.0,iskip)
  enddo
!  do L=1,n_species,1
!      call write_tecplot_single_skip(78,rr_r(:,:,:,L),rr_ref,iskip)
!  enddo

!----------------------------------------------------------------------
  if(myid==0)then
!----------------------------------------------------------------------
! close files
  close(78)
!----------------------------------------------------------------------
  endif ! myid ==0 closing
!----------------------------------------------------------------------



!----------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------
! format statements

  1 format(' zone t="stationary", i=',i5,', f=block')
  2 format(' zone t="stationary", i=',i5,', j=',i5,', f=block')
  3 format(' zone t="stationary", i=',i5,', j=',i5,', k=',i5,', f=block')

  9 format(10(1pe12.5,1x))
!----------------------------------------------------------------------
  return
  end subroutine write_tecplot_skip



!----------------------------------------------------------------------
  subroutine write_tecplot_single_skip(io,field,scaling,iskip)
!----------------------------------------------------------------------
! written by Ramanan Sankaran(12/23/04)
! Minimal storage and communication to bring down
! the necessary data to rank-0 processor.
!
! routine writes a single field to the given io unit
! reduces the field to one processor!
! must be called by all processes
! file must already be open or will overwrite
!----------------------------------------------------------------------
  use topology_m
  use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g

  implicit none
!----------------------------------------------------------------------
! declarations passed in

  real, dimension(nx,ny,nz), intent(in) :: field !the field to write
  real, intent(in) :: scaling                    !scaling units for the field
  integer, intent(in) :: io                      !io unit
  integer, intent(in) :: iskip                   !skip to use in writing files

!----------------------------------------------------------------------
! local declarations

  integer i,j,k,L
  integer ia, ja
  integer ix, jy, kz
  real, allocatable, dimension(:,:) :: field_g
  integer field_type
  integer :: req_recv(xpes), stat_recv(MPI_STATUS_SIZE,xpes), req

  integer nx_w, ny_w, nz_w, mskp
  integer ypr, zpr

!----------------------------------------------------------------------
! return if zero dimensions

  if((nx_g.eq.1).and.(ny_g.eq.1).and.(nz_g.eq.1)) then
    return
  endif
!----------------------------------------------------------------------
  if(xid .eq. 0) then
    allocate( field_g(nx_g, ny) );
    field_g = 0.0;
  end if

!----------------------------------------------------------------------
!nx_w, ny_w and nz_w will be the mesh size in the final tecplot file.

  mskp = int((nx_g-1)/iskip)
  if(nx_g .eq. 1+mskp*iskip) then
    nx_w=1+mskp
  else
    nx_w=2+mskp
  endif

  mskp = int((ny_g-1)/iskip)
  if(ny_g .eq. 1+mskp*iskip) then
    ny_w=1+mskp
  else
    ny_w=2+mskp
  endif

  mskp = int((nz_g-1)/iskip)
  if(nz_g .eq. 1+mskp*iskip) then
    nz_w=1+mskp
  else
    nz_w=2+mskp
  endif
  !nz_w = 1
  nz_w = 2
!----------------------------------------------------------------------

  loop_k: do k = 1, nz_w
    !if(k .eq. nz_w) then
    !  kz = nz_g
    !else
    !  kz = 1+(k-1)*iskip
    !end if
    if(k .eq. 1) then
      kz = 1
    else
      kz = int(nz_g/2)
    end if
    !which processes does this kz belong to?
    !(does f95 have a ceil function?)
    ! if(zid == ceil(kz/nz)+1) then...... 
    zpr = kz/nz
    if( kz /= zpr*nz) zpr=zpr+1
    if(zid+1 == zpr) then
      ! reduce kz to the local k
      kz = kz-zid*nz
      ! Gather a single xy-plane's data onto the respective xid=0 processor
      ! All the data is brought down - no skip is performed at this stage
      ! MPI_gather does not work because of C vs F90 reversed index order.
      ! Do `manual' gather
      call MPI_Isend(field(1,1,kz), nx*ny, MPI_REAL8, 0, xid+k*1000, xcomm, req, ierr)
      if (xid ==0) then
        call MPI_Type_Vector(ny, nx, nx_g, MPI_REAL8, field_type, ierr)
        call MPI_Type_commit(field_type, ierr)
        do i=0, xpes-1
          call MPI_Irecv(field_g(i*nx+1,1), 1, field_type, i, i+k*1000, &
                         xcomm, req_recv(i+1), ierr)
        end do
        call MPI_WaitAll(xpes, req_recv, stat_recv, ierr)
        call MPI_Type_free(field_type, ierr)
      end if
      call MPI_Wait(req, status, ierr)
      ! After sending data to the xid=0 process, all else can skip the rest
      if( xid /= 0) cycle loop_k
      !----------------------------------------------------------------------
      ! Reduce the data at this stage by skipping. Do nothing for iskip == 1
      if (iskip /=1) then
        !L keeps track of the second dimension of field_g in which the reduced data is stored
        L = 0
        do j = 1, ny_w
          if(j .eq. ny_w) then
            jy = ny_g
          else
            jy = 1+(j-1)*iskip
          end if
          !Which process does this jy belong to?
          ypr = jy/ny
          if ( jy /= ypr*ny) ypr=ypr+1
          if (yid+1 == ypr) then
            ! reduce jy to the local j
            jy = jy-yid*ny
            ! Now pack the data for this jy
            L = L+1
            do i = 1, nx_w
              if(i .eq. nx_w) then
                ix = nx_g
              else
                ix = 1+(i-1)*iskip
              end if
              ! The ix does not need to be recomputed to local value because 
              ! we have the whole data on the xid=0 processor.
              field_g(i,L) = field_g(ix,jy)
            end do !i = 1, nx_w
          end if !yid+1 == ypr
        end do !j=1, ny_w
      else !iskip = 1
        !Since we didnt skip anything all data is present
        ! L has to be properly set
        L = ny
      end if !iskip
      !----------------------------------------------------------------------
      !All xid=0 processes in the kz plane have now reduced their data
      !They can start sending it to rank 0.
      !While working at the zid=0 level on z-axis, myid=0 is inside this loop
      !So it should receive and print while inside here.
      !However for zid>0, myid=0 does not enter the zid+1==zpr branch
      !So it should do the receives from outside this branch
      if(myid /= 0) then
        call MPI_Send(L, 1, MPI_INTEGER, 0, kz, yz_comm, ierr)
        call MPI_Type_vector(L, nx_w, nx_g, MPI_REAL8, field_type, ierr)
        call MPI_Type_commit(field_type, ierr)
        call MPI_Send(field_g, 1, field_type, 0,kz*1000, yz_comm, ierr)
        call MPI_Type_free(field_type, ierr)
      !----------------------------------------------------------------------
      else
        !----------------------------------------------------------------------
        do j = 0, ypes-1
          if( j /= 0) then
            call MPI_Recv(L, 1, MPI_INTEGER, j, kz, yz_comm, status, ierr)
            call MPI_Type_vector(L, nx_w, nx_g, MPI_REAL8, field_type, ierr)
            call MPI_Type_commit(field_type, ierr)
            call MPI_Recv(field_g, 1, field_type, j,kz*1000, yz_comm, status, ierr)
            call MPI_Type_free(field_type, ierr)
          end if
          write(io,9) ((field_g(ia,ja)*scaling, ia=1,nx_w),ja=1,L)
        end do !j = 0, ypes-1
        !----------------------------------------------------------------------
      end if
    else if(myid == 0) then !zid+1 /= zpr and myid == 0
      !----------------------------------------------------------------------
      ! reduce kz to the local k for message tag purposes
      kz = kz-(zpr-1)*nz
      do j = 0, ypes-1
        !myid = 0 has to receive data and print it
        call MPI_Recv(L, 1, MPI_INTEGER, (zpr-1)*ypes+j, kz, yz_comm, status, ierr)
        call MPI_Type_vector(L, nx_w, nx_g, MPI_REAL8, field_type, ierr)
        call MPI_Type_commit(field_type, ierr)
        call MPI_Recv(field_g, 1, field_type, (zpr-1)*ypes+j,kz*1000, yz_comm, status, ierr)
        call MPI_Type_free(field_type, ierr)
        write(io,9) ((field_g(ia,ja)*scaling, ia=1,nx_w),ja=1,L)
      end do !j = 0, ypes-1
      !----------------------------------------------------------------------
    end if !zid+1 == zpr
  end do loop_k !k = 1, nz_w

! deallocate
  if(xid ==0) deallocate(field_g)

! format statements

  9 format(10(1pe12.5,1x))
!----------------------------------------------------------------------
  return
  end subroutine write_tecplot_single_skip

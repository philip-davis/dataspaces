#include "globalDefines.h"
!$Id:$
!=========================================================================================
  subroutine dump_klm_files(io)
!=========================================================================================
! writes a file data
!-----------------------------------------------------------------------------------------
  use topology_m, only : myid, gcomm, ierr
  use param_m, only : nx,ny,nz, n_spec
  use variables_m, only : temp, pressure, yspecies, u, q, volum
  use reference_m, only : l_ref, a_ref, p_ref, rho_ref, t_ref, &
                          cp_ref, pres_atm, g_ref, t_o, time_ref, &
                          hr_ref, rr_ref
  use chemkin_m, only : species_name,reaction_rate
  use mixfrac_m, only : mixfrac, specToMixfr, massToMoleAll, phiBarlow
  use zclookup_m
  use runtime_m, only : run_title, time, i_time, tstep
!  use filter_m, only : filter, initialize_filter, allocate_filter_arrays
#ifdef MIXAVG
  use transport_m, only : computeCoefficients,getDiffusionCoeff   !MixAvg
#endif
#ifdef LEWIS
  use transport_m, only : computeCoefficients                     !Lewis
#endif
!  use premix_drvd_var_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io
  
! local declarations
!-----------------------------------------------------------------------------------------

  integer :: L

  real l_conv_cgs, a_conv_cgs, rho_conv_cgs, t_conv_cgs, dil_conv_cgs, hr_conv_cgs
  real rr_conv_cgs
  real p_conv_atm

  real, dimension(nx,ny,nz) :: hr
  real, dimension(nx,ny,nz) :: chi
  real, dimension(nx,ny,nz) :: vort_mag
  real, target, dimension(nx,ny,nz,n_spec) :: spec_array
  real, pointer, dimension(:,:,:,:) :: rr_r
  real, pointer, dimension(:,:,:,:) :: D
  real, target, dimension(nx,ny,nz,3) :: vort
  real, pointer, dimension(:,:,:,:) :: grad_Y
  real, dimension(nx,ny,nz) :: Chi_Y, Cprog

  real, dimension(nx,ny,nz) :: ZZdiss, CCdiss, ZCdiss
  real, dimension(nx,ny,nz, n_spec) :: xspecies, diffusion
  real, dimension(nx,ny,nz) :: equiv

  integer, save :: counter = 0
  character*4 count_ext
  character*9 tail
  character*100 head
  character*100 dirname
  character*100 tarcmd, tartgt, tarsrc
!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

! message
  if(myid==0) write(io,*) 'Writing viz field files for Kwan-Liu Ma'

! increment counter
  counter = counter + 1


! set counter stamp for file
!  write(count_ext, '(I4.4)') i_time
  if(counter.le.9)then
    write(count_ext,'(A3,I1)') '000',counter 
  elseif((counter.gt.9).and.(counter.le.99))then
    write(count_ext,'(A2,I2)') '00',counter 
  elseif((counter.gt.99).and.(counter.le.999))then
    write(count_ext,'(A1,I3)') '0',counter 
  elseif((counter.gt.999).and.(counter.le.9999))then
    write(count_ext,'(I4)') counter 
  endif

  tail = '_'//trim(count_ext)//'.dat'

    dirname = '../post/klm/'//trim(run_title)//'_'//trim(count_ext)
    head = trim(dirname)//'/'//trim(run_title)//'_'
    if(myid == 0)then
      tarcmd = 'mkdir '//trim(dirname)
    endif
#if defined(ARCH_SGI) || defined(ARCH_SP2) || defined(ARCH_OPTERON)
!      if(myid==0) call system(tarcmd)
     call makedirectory(trim(dirname)//char(0))
#endif
#if defined(ARCH_T3E) || defined(ARCH_X1)
      if(myid==0)  call ishell( trim(tarcmd) )
#endif

!-----------------------------------------------------------------------------------------
! set reference values in CGS

  l_conv_cgs=l_ref*100.0
  a_conv_cgs=a_ref*100.0
  t_conv_cgs=t_ref
  rho_conv_cgs=rho_ref*(1000.0/100.0**3)
  dil_conv_cgs=(a_ref/l_ref)
  p_conv_atm=p_ref/pres_atm
  rr_conv_cgs=rho_conv_cgs*a_conv_cgs/l_conv_cgs  ! g/cm^3/s
!  rr_conv_cgs=a_conv_cgs/l_conv_cgs              ! 1/s (remember to multilply rr by volum)
  hr_conv_cgs= cp_ref * t_ref * 1.0e4 * rr_conv_cgs ! [erg/cm^3/s] 

!-----------------------------------------------------------------------------------------
  if(myid.eq.0)write(io,*) 'calling write files'
! mixfrac

! calc mixfrac
  call specToMixfr(yspecies)


!! write mixfrac
  call write_klm_field(mixfrac,trim(head)//'mixfrac'//trim(tail),1.0)

!-----------------------------------------------------------------------------------------
! Chi

! calc dissipation rate
!!  call calcDissipRate(mixfrac,q(:,:,:,4,1),temp,chi)  
!call computeCoefficients( pressure, temp, yspecies, q(:,:,:,4,1) )
!call calc_ScalDissRate(chi)
!
!! write Chi
!  call write_klm_field(chi,trim(head)//'chi'//trim(tail),1.0/time_ref)

!-----------------------------------------------------------------------------------------
! temperature

  call write_klm_field(temp,trim(head)//'T'//trim(tail),t_ref)

!-----------------------------------------------------------------------------------------
! Y
!  do L = 1, n_spec
!    if( (trim(species_name(L))=='OH') .or. (trim(species_name(L))=='HO2') )then
!      call write_klm_field(yspecies(:,:,:,L),&
!            trim(head)//'Y_'//trim(species_name(L))//trim(tail),1.0)
!    endif
!  enddo

!-----------------------------------------------------------------------------------------
! heat release

     rr_r => spec_array
!   
!   ! calculate reaction rate 
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
!   
!   ! write rr_O2
!     if(myid.eq.0)write(io,*)'n_spec_prog=',4
!   
!     call write_klm_field(rr_r(:,:,:,4),trim(head)//'rr'//trim(tail),rr_ref)  
!   
!   ! calculate heat release
     call calc_heat_release(rr_r,hr)
!   
!     nullify(rr_r)
!   
!   ! write hr
     call write_klm_field(hr,trim(head)//'hr'//trim(tail),hr_ref)
!   
!-----------------------------------------------------------------------------------------
! vorticity magnitude

! calculate vorticity magnitude
!  call calc_vorticity(vort,vort_mag,u)

! write vorticity magnitude
!  call write_klm_field(vort_mag,trim(head)//'vort'//trim(tail),1.0/time_ref)

!----------------------------------------------------------------------
  ! Progress variable
!  call calculate_progvar(Cprog)
!  call write_klm_field(Cprog,trim(head)//'Cprog'//trim(tail),1.0)

!-----------------------------------------------------------------------------------------
! scalar dissipation fields

!#!$ ! new nondimensional transport
!#!$   call computeCoefficients( pressure, temp, yspecies, q(:,:,:,4,1) )
!#!$ 
!#!$   D => spec_array
!#!$   grad_Y => vort
!#!$ 
!#!$ ! diffusion coefficient 
!#!$   D = getDiffusionCoeff()
!#!$   do L = 1, n_spec 
!#!$     D(:,:,:,L) = D(:,:,:,L)*volum(:,:,:)
!#!$   enddo
!#!$ 
!#!$   do L = 1,n_spec
!#!$     if((trim(species_name(L))=='OH').or.(trim(species_name(L))=='HO2'))then
!#!$ 
!#!$ !   gradients
!#!$     call computeScalarGradient(yspecies(:,:,:,L),grad_Y(:,:,:,:))
!#!$ 
!#!$ !   Chi_Y
!#!$     Chi_Y(:,:,:) = 2.0*D(:,:,:,L)*sum(grad_Y(:,:,:,:)**2,DIM=4)
!#!$ 
!#!$ !   write dissipation field
!#!$     call write_klm_field(yspecies(:,:,:,L),&
!#!$             trim(head)//'Chi_Y_'//trim(species_name(L))//trim(tail),1.0/time_ref)
!#!$ 
!#!$     endif
!#!$   enddo
!#!$ 
!#!$   nullify(D,grad_Y)

!-----------------------------------------------------------------------------------------
! Partially premixed variables 

!         call SpecToProg(yspecies,io)              ! 
!         call calc_zcdiss(ZZdiss,CCdiss,ZCdiss,io) !
!    !   
!    !   ! write Progress variable, Mixture fraction, Scalar dissipation rates.
!    !   
!    !     call write_klm_field(PartProg,trim(head)//'Cprog'//trim(tail),1.0)
!         call write_klm_field(ZetaField,trim(head)//'Zeta'//trim(tail),1.0)
!         call write_klm_field(ZZdiss,trim(head)//'ZZdiss'//trim(tail),1.0)
!         call write_klm_field(CCdiss,trim(head)//'CCdiss'//trim(tail),1.0)
!         call write_klm_field(ZCdiss,trim(head)//'ZCdiss'//trim(tail),1.0)
!   
!   
!-----------------------------------------------------------------------------------------
! Data for Jonathan Frank: Mole fractions of all species, temperature, HRR, Phi

! calculate the equivalence ratio
!call phiBarlow( yspecies, equiv )
!call write_klm_field(equiv,trim(head)//'Phi'//trim(tail),1.0)

! calculate the mole fractions: 
!  call massToMoleAll( yspecies, xspecies )
!  do L = 1, n_spec
!      call write_klm_field(xspecies(:,:,:,L),&
!            trim(head)//'X_'//trim(species_name(L))//trim(tail),1.0)
!  enddo

  do L = 1, n_spec
      call write_klm_field(yspecies(:,:,:,L),&
            trim(head)//'Y_'//trim(species_name(L))//trim(tail),1.0)
          enddo

!
!-----------------------------------------------------------------------------------------
! velocity 

! write u
!  call write_klm_field(u(:,:,:,1),trim(head)//'u'//trim(tail),a_ref)
!  call write_klm_field(u(:,:,:,2),trim(head)//'v'//trim(tail),a_ref)
!  call write_klm_field(u(:,:,:,3),trim(head)//'w'//trim(tail),a_ref)

!-----------------------------------------------------------------------------------------
  return
  end subroutine dump_klm_files


!=========================================================================================
  subroutine write_klm_field(field,filename,scaling)
!=========================================================================================
! 
!-----------------------------------------------------------------------------------------
  use topology_m
  use param_m, only : nx,ny,nz,nx_g,ny_g,nz_g

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real, dimension(nx,ny,nz) :: field
  character(LEN=*) :: filename
  real :: scaling
!----------------------------------------------------------------------------------------
! local declarations

  integer i,j,k,L
!  real, allocatable, dimension(:,:,:) :: field_g
  real*4, allocatable, dimension(:,:,:) :: field_g
  integer field_recv_type ! a local variable defining the type for receiving each field
  integer xy_plane_type ! a local variable defining the type for the x-y planes 
                        ! corresponding to constant k
  integer sizeof        ! the size in bytes of the MPI input data-type 
  integer procid,req
  integer :: reqvec(2), statvec(MPI_STATUS_SIZE,2)

! local 4-byte real copies
  real*4 :: field_l(nx,ny,nz)
!  real :: field_l(nx,ny,nz)

  real*4 :: field_r(nx,ny,1)
!  real :: field_r(nx,ny,1)

  integer, parameter :: rsize = 4
!  integer, parameter :: rsize = 8

  integer mpi_real_type

  integer :: kc, jc

  character*2 ztag

  logical :: append_files = .true.

!----------------------------------------------------------------------------------------

  !call MPI_Barrier(gcomm,ierr)
  !if(myid==0)then
  !  write(7,*) 'local copy'
  !  call flush(7)
  !endif


! make a local copy of the field and scale
  field_l = real(field*scaling,kind(field_l))

! allocate space only only xy_id==0 to save memory
  if(xy_id==0)then
    allocate(field_g(nx_g,ny_g,1)); field_g=0.0
  endif 
!----------------------------------------------------------------------------------------
! define types:

  if(rsize==8)then
    mpi_real_type = MPI_REAL8
  else
    mpi_real_type = MPI_REAL4
  endif


! loop over z processors writing
  do k=0,zpes-1

   
! whether we do all this depends on whether we are splitting or concatenating in z.
  if(((append_files).and.zid==k).or.((.not.append_files).and.k==0))then

! loop over z planes
  do kc = 1, nz

! loop over x y processors
  do j=0,ypes-1
  do i=0,xpes-1

    procid=i+xpes*j

!   do the sends and receives
    if(xy_id == procid) call MPI_Isend(field_l(1,1,kc),nx*ny*1,mpi_real_type,&
                                      0,xy_id,xy_comm,reqvec(1),ierr)

    if(xy_id == 0) call MPI_IRecv(field_r(1,1,1),nx*ny*1,mpi_real_type, &
                                 procid,procid,xy_comm,reqvec(2),ierr)

!   waits
    if(xy_id==procid) call MPI_Wait(reqvec(1),statvec(:,1),ierr)
    if(xy_id==0) call MPI_Wait(reqvec(2),statvec(:,2),ierr)

!   local copy
    if(xy_id==0) field_g(i*nx+1:i*nx+nx,j*ny+1:j*ny+ny,1) = field_r(:,:,1)
   
    call MPI_Barrier(xy_comm,ierr)
  enddo
  enddo

!  call MPI_Barrier(gcomm,ierr)  
!  if(xy_id==0)then  
!    write(7,*) 'done MPI'  
!    call flush(7)  
!  endif  

!----------------------------------------------------------------------------------------
  if(xy_id==0)then !only the edge processes write

    if(append_files)then  
!     each zid writes at a time, appending.
!       write variable data
        call writebin_append(field_g(:,:,:),nx_g*ny_g*1,trim(filename)//char(0))
    else 
!     each z plane writes a separate file
      write(ztag,'(I2.2)') zid
      call writebin(field_g(:,:,:),nx_g*ny_g*nz,trim(filename)//'_'//trim(ztag)//char(0))
!      call writebin_append(field_g(:,:,:),nx_g*ny_g*1,trim(filename)//'_'//trim(ztag)//char(0))
    endif

  endif !xyid==0 writing
!----------------------------------------------------------------------------------------

  enddo !kc grid loop

  endif !((append_files).and.zid==k).or.((.not.append_files).and.k==0)

  if(append_files) call MPI_Barrier(zcomm,ierr)

  enddo !k processor loop

!-----------------------------------------------------------------------------------------
  if(myid == 0) then
    write(*,*) 'end of write_klm_file'
  endif
! deallocate
  if(xy_id==0) deallocate(field_g)
!-----------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------
  end subroutine write_klm_field

#include "globalDefines.h"
! $Id: rans_bunsen.f90,v 1.1.2.8 2006/08/24 19:59:18 rsankar Exp $
!----------------------------------------------------------------------
!
!    ******* WRITTEN BY RAMANAN SANKARAN *******
!
! Module to postprocess the bunsen flame data
! Adapted to take averages conditional on mixture fraction for analysis
! of the stratified Bunsen case - esr 3 June 08.
! Some changes made so it will compile and work with either the bunsen
! or stratified cases.
!----------------------------------------------------------------------
module bunsen_post_m
use topology_m
use variables_m, only: q, temp, pressure, yspecies, volum, u
use stat_util_m
use premix_drvd_var_m
use grid_m, only: x, y, z
use reference_m
use zclookup_m
use clookup_m
use runtime_m, only: run_title, tstep
implicit none

public

integer, private :: window != 200
integer, private :: nbinz  != 20  ! mixture fraction bins, set this to unity if it is a premixed system
logical, private :: i_calc_tz_ave           ! variables to determine which routines to be called
logical, private :: i_calc_turb
logical, private :: i_calc_c_gradc
logical, private :: i_gradc_stats_given_c
logical, private :: i_calc_c_spcs
logical, private :: i_strain_cndtl
logical, private :: i_print_pdfs
logical, private :: i_curv_cndtl
logical, private :: i_chi_cndtl_sd
logical, private :: i_c_cndtl_sd
logical, private :: i_calc_FSD_IO
logical, private :: i_calc_FSD_XZ
logical, private :: i_calc_consumption
logical, private :: i_curv_cndtl_sd
logical, private :: i_calc_cndtl_sd
logical, private :: i_print_xzavgs
logical, private :: i_write_meanfield
logical, private :: i_calc_dissplot
logical, private :: i_calc_dcond_spcs
real, private, parameter :: Cval_param = 0.7
real, private, parameter :: Cdel_param = 0.05
!real, dimension(0:nbinz) :: binz_val
real, allocatable, dimension(:) :: binz_val

contains

!----------------------------------------------------------------------
! Read the input file, allocate memory and set parameters
!----------------------------------------------------------------------
subroutine initialize_rans_analysis(io)
!use runtime_m, only: run_title
integer, intent(in) :: io
character*100 :: filename

  ! -----------------------------------------------------------------------------
  ! Read rans.in input file
  ! -----------------------------------------------------------------------------

  filename='../input/rans.in'
  call inquire_about_input_file(filename,io)

  if(myid.eq.0) then
     open(unit=1,file=trim(filename),status='old')

     read(1,*)
     read(1,*)
     read(1,*)

     read(1,*) nbinz
     read(1,*) window

     read(1,*) i_calc_tz_ave
     read(1,*) i_calc_turb
     read(1,*) i_calc_c_gradc
     read(1,*) i_gradc_stats_given_c
     read(1,*) i_calc_c_spcs
     read(1,*) i_strain_cndtl
     read(1,*) i_print_pdfs
     read(1,*) i_curv_cndtl
     read(1,*) i_chi_cndtl_sd
     read(1,*) i_c_cndtl_sd
     read(1,*) i_calc_FSD_IO
     read(1,*) i_calc_FSD_XZ
     read(1,*) i_calc_consumption
     read(1,*) i_curv_cndtl_sd
     read(1,*) i_calc_cndtl_sd
     read(1,*) i_print_xzavgs
     read(1,*) i_calc_dissplot
     read(1,*) i_calc_dcond_spcs
     read(1,*) i_write_meanfield

     close(1)

     write(io,*)'************************************'
     write(io,*)'Initializing Rans_Bunsen_Strat with:'
     write(io,*) 'nbinz                ',  nbinz
     write(io,*) 'window               ',  window
     write(io,*) 'i_calc_tz_ave        ',  i_calc_tz_ave
     write(io,*) 'i_calc_turb          ',  i_calc_turb
     write(io,*) 'i_calc_c_gradc       ',  i_calc_c_gradc
     write(io,*) 'i_gradc_stats_given_c',  i_gradc_stats_given_c
     write(io,*) 'i_calc_c_spcs        ',  i_calc_c_spcs
     write(io,*) 'i_strain_cndtl       ',  i_strain_cndtl
     write(io,*) 'i_print_pdfs         ',  i_print_pdfs
     write(io,*) 'i_curv_cndtl         ',  i_curv_cndtl
     write(io,*) 'i_chi_cndtl_sd       ',  i_chi_cndtl_sd
     write(io,*) 'i_c_cndtl_sd         ',  i_c_cndtl_sd
     write(io,*) 'i_calc_FSD_IO        ',  i_calc_FSD_IO
     write(io,*) 'i_calc_FSD_XZ        ',  i_calc_FSD_XZ
     write(io,*) 'i_calc_consumption   ',  i_calc_consumption
     write(io,*) 'i_curv_cndtl_sd      ',  i_curv_cndtl_sd
     write(io,*) 'i_calc_cndtl_sd      ',  i_calc_cndtl_sd
     write(io,*) 'i_print_xzavgs       ',  i_print_xzavgs
     write(io,*) 'i_calc_dissplot      ',  i_calc_dissplot
     write(io,*) 'i_calc_dcond_spcs    ',  i_calc_dcond_spcs
     write(io,*) 'i_write_meanfield    ',  i_write_meanfield
     write(io,*)'************************************'


  endif
  ! call check_term_status(io,0) ! This is a bit harsh...

  call MPI_Bcast(nbinz                , 1, MPI_INTEGER, 0, gcomm, ierr)
  call MPI_Bcast(window               , 1, MPI_INTEGER, 0, gcomm, ierr)
  
  call MPI_Bcast(i_calc_tz_ave        , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_turb          , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_c_gradc       , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_gradc_stats_given_c, 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_c_spcs        , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_strain_cndtl       , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_print_pdfs         , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_curv_cndtl         , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_chi_cndtl_sd       , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_c_cndtl_sd         , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_FSD_IO        , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_FSD_XZ        , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_consumption   , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_curv_cndtl_sd      , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_cndtl_sd      , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_print_xzavgs       , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_dissplot      , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_calc_dcond_spcs    , 1, MPI_LOGICAL, 0, gcomm, ierr)
  call MPI_Bcast(i_write_meanfield    , 1, MPI_LOGICAL, 0, gcomm, ierr)

  allocate(binz_val(0:nbinz))
  
!  select case (trim(run_title))
!  case ('strat')
  call SpecToProg(yspecies,io)
!  end select
  
  call set_eta_bins(io)

end subroutine initialize_rans_analysis
!----------------------------------------------------------------------
! Call the analysis routines selected by the user
!----------------------------------------------------------------------
subroutine rans_analysis_driver(io,finish)
integer, intent(in) :: io
logical, intent(in) :: finish

if(.not.finish)then
  if(i_calc_tz_ave            )   call calc_tz_ave(io,.false.)
  if(i_calc_turb              )   call calc_turb(io,.false.)
  if(i_calc_c_gradc           )   call calc_c_gradc(io,.false.)
  if(i_gradc_stats_given_c    )   call gradc_stats_given_c(io, .false.)
  if(i_calc_c_spcs            )   call calc_c_spcs(io, .false.)
  if(i_strain_cndtl           )   call strain_cndtl(io, .false.)
  if(i_print_pdfs             )   call print_pdfs(io)             !??? do we need this here?? 
  if(i_curv_cndtl             )   call curv_cndtl(io, .false.)
  if(i_chi_cndtl_sd           )   call chi_cndtl_sd(io, .false.)
  if(i_c_cndtl_sd             )   call c_cndtl_sd(io, .false.)
  if(i_calc_FSD_IO            )   call calc_FSD_IO(io,.false.)
  if(i_calc_FSD_XZ            )   call calc_FSD_XZ(io,.false.)
  if(i_calc_consumption       )   call calc_consumption(io, .false.)
  if(i_curv_cndtl_sd          )   call curv_cndtl_sd(io, .false.)
  if(i_calc_cndtl_sd          )   call calc_cndtl_sd(io, .false.)
  if(i_print_xzavgs           )   call print_xzavgs(io)           !??? do we need this here??
  if(i_calc_dissplot          )   call calc_dissplot(io, .false.)
  if(i_calc_dcond_spcs        )   call calc_dcond_spcs(io, .false.)
else
  if(i_calc_tz_ave            )   call calc_tz_ave(io,.true.)
  if(i_calc_turb              )   call calc_turb(io,.true.)
  if(i_calc_c_gradc           )   call calc_c_gradc(io,.true.)
  if(i_gradc_stats_given_c    )   call gradc_stats_given_c(io, .true.)
  if(i_calc_c_spcs            )   call calc_c_spcs(io, .true.)
  if(i_curv_cndtl_sd          )   call curv_cndtl_sd(io, .true.)
  if(i_calc_cndtl_sd          )   call calc_cndtl_sd(io, .true.)
  if(i_strain_cndtl           )   call strain_cndtl(io, .true.)
  if(i_curv_cndtl             )   call curv_cndtl(io, .true.)
  if(i_chi_cndtl_sd           )   call chi_cndtl_sd(io, .true.)
  if(i_c_cndtl_sd             )   call c_cndtl_sd(io, .true.)
  if(i_calc_FSD_IO            )   call calc_FSD_IO(io,.true.)
  if(i_calc_FSD_XZ            )   call calc_FSD_XZ(io,.true.)
  if(i_calc_consumption       )   call calc_consumption(io, .true.)
  if(i_calc_dissplot          )   call calc_dissplot(io, .true.)
  if(i_calc_dcond_spcs        )   call calc_dcond_spcs(io, .true.)
  if(i_write_meanfield        )   call write_meanfield(io)
endif

end subroutine rans_analysis_driver
!----------------------------------------------------------------------
! Set the bins for conditional averaging
!----------------------------------------------------------------------
subroutine set_eta_bins(io)
integer, intent(in) :: io
real, parameter :: binz_start = 0.0 
real :: binz_del 
integer n

  binz_del = (1.0 - binz_start)/real(nbinz)
  do n = 0, nbinz
    binz_val(n) = binz_start + binz_del*real(n)
  end do

end subroutine set_eta_bins

!----------------------------------------------------------------------
! Time and Z averages of all quantities, everywhere
! Unconditional. 
!----------------------------------------------------------------------
subroutine calc_tz_ave(io,finish)
!use topology_m, only: myid
use chemkin_m, only: reaction_rate, species_name
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

! add a dimension for mixture fraction conditioning
 
logical, save :: initialized=.false.
real, save, allocatable, dimension(:, :, :) :: hr_num, hr_den
real, save, allocatable, dimension(:, :, :) :: T_num, T_den
real, save, allocatable, dimension(:, :, :) :: Cf_num, Cf_den
real, save, allocatable, dimension(:, :, :, :) :: U_num, U_den
real, save, allocatable, dimension(:, :, :, :) :: Uf_num, Uf_den
real, save, allocatable, dimension(:, :, :, :) :: Uf_num_ub, Uf_den_ub
real, save, allocatable, dimension(:, :, :, :) :: Yf_num, Yf_den, rr_num, rr_den
real, save, allocatable, dimension(:, :, :, :) :: Y2f_num, Y2f_den

real, dimension(nx,ny,nz) :: wt, Cprog, rho
logical, dimension(nx,ny,nz) :: ifcond
logical, dimension(nx,ny,nz) :: ifzcond
real, dimension(nx, ny, nz, n_spec) :: rr_r, diffusion
real, dimension(nx, ny, nz) :: hr
real, dimension(nx, ny) :: grid, ave
integer i, L, n

character*100 filename
character*2 xiid_ext


if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_tz_ave'

  allocate(hr_num(nx, ny, 0:nbinz), hr_den(nx, ny, 0:nbinz))
  allocate(T_num(nx, ny, 0:nbinz), T_den(nx, ny, 0:nbinz))
  allocate(Cf_num(nx, ny, 0:nbinz), Cf_den(nx, ny, 0:nbinz))
  allocate(U_num(nx, ny, 3, 0:nbinz), U_den(nx, ny, 3, 0:nbinz))
  allocate(Uf_num(nx, ny, 3, 0:nbinz), Uf_den(nx, ny, 3, 0:nbinz))
  allocate(Uf_num_ub(nx, ny, 3, 0:nbinz), Uf_den_ub(nx, ny, 3, 0:nbinz))
  allocate(Yf_num(nx, ny, n_spec, 0:nbinz), Yf_den(nx, ny, n_spec, 0:nbinz))
  allocate(Y2f_num(nx, ny, n_spec, 0:nbinz), Y2f_den(nx, ny, n_spec, 0:nbinz))
  allocate(rr_num(nx, ny, n_spec, 0:nbinz), rr_den(nx, ny, n_spec, 0:nbinz))

  hr_num = 0.0; hr_den = 0.0;
  T_num = 0.0; T_den = 0.0;
  Cf_num = 0.0; Cf_den = 0.0;
  U_num = 0.0; U_den = 0.0;
  Uf_num = 0.0; Uf_den = 0.0;
  Uf_num_ub = 0.0; Uf_den_ub = 0.0;
  Yf_num = 0.0; Yf_den = 0.0;
  Y2f_num = 0.0; Y2f_den = 0.0;
  rr_num = 0.0; rr_den = 0.0;

  initialized = .true.
end if

if(.not. finish) then
  wt = 1.0
  ifcond = .true.
  rho = q(:,:,:,4,1)
  call calculate_progvar(Cprog,io)
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
  call calc_heat_release(rr_r, hr)

do n=0,nbinz   !conditioning loop
  
  if(n.eq.0)then   ! n=0 corresponds to unconditional averages 
      ifcond=.true.
  else
! set the ifcond mask here.
  where(zetafield .gt. binz_val(n-1) .and. &
     zetafield .le. binz_val(n) )
     ifcond = .true.
  elsewhere 
     ifcond = .false.
  endwhere
  endif

  ifzcond=ifcond
  
  call calc_z_accum_numden (temp, ifcond, wt, T_num(:,:,n), T_den(:,:,n))
  call calc_z_accum_numden (Cprog, ifcond, wt, Cf_num(:,:,n), Cf_den(:,:,n))
  do i = 1, 3
    call calc_z_accum_numden (u(:,:,:,i), ifcond, wt, U_num(:,:,i,n), U_den(:,:,i,n))
    !Favre averaged velocity
    call calc_z_accum_numden (u(:,:,:,i), ifcond, rho, Uf_num(:,:,i,n), Uf_den(:,:,i,n))
  end do
  call calc_z_accum_numden (hr, ifcond, wt, hr_num(:,:,n), hr_den(:,:,n))
  do i = 1, n_spec
    !Favre averaged species
    call calc_z_accum_numden (yspecies(:,:,:,i), ifcond, rho, Yf_num(:,:,i,n), Yf_den(:,:,i,n))
    !species squared (to get variances)
    call calc_z_accum_numden (yspecies(:,:,:,i)*yspecies(:,:,:,i), ifcond, rho, Y2f_num(:,:,i,n), Y2f_den(:,:,i,n))
    call calc_z_accum_numden (rr_r(:,:,:,i), ifcond, wt, rr_num(:,:,i,n), rr_den(:,:,i,n))
  end do
  where(Cprog.lt.0.05)
    ifcond = .true.
  elsewhere
    ifcond = .false.
  endwhere
  ifcond=ifcond.and.ifzcond
  do i = 1, 3
    !Favre averaged conditional velocity
    call calc_z_accum_numden (u(:,:,:,i), ifcond, rho, Uf_num_ub(:,:,i,n), Uf_den_ub(:,:,i,n))
  end do

enddo !conditioning loop  

else
  !Finishing step. Write the data out.
  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing tZ avg stats'

do n=0,nbinz
  write(xiid_ext,'(I2.2)') n
  if (myid == 0) then
    filename = '../post/tecplot/tZave_'//trim(xiid_ext)//'.tec'
    open(unit=78, file=trim(filename), status='unknown')

    write(78,*) 'title = tZ-ave'
    write(78,*) 'variables = '
    write(78,*) '"x [mm]"', '"y [mm]"'
    write(78,*) '"T_ave [K]" '
    write(78,*) '"Cf_ave" '
    write(78,*) '"hr_ave [J/m^3/s]"'
    write(78,*) '"u_ave [m/s]"'
    write(78,*) '"v_ave [m/s]"'
    write(78,*) '"w_ave [m/s]"'
    write(78,*) '"uf_ave [m/s]"'
    write(78,*) '"vf_ave [m/s]"'
    write(78,*) '"wf_ave [m/s]"'
    write(78,*) '"uf_ave_ub [m/s]"'
    write(78,*) '"vf_ave_ub [m/s]"'
    write(78,*) '"wf_ave_ub [m/s]"'
    do L=1,n_spec
      write(78,*) '"Yf_ave '//trim(species_name(L))//'"'
    enddo
    do L=1,n_spec
      write(78,*) '"Y2f_ave '//trim(species_name(L))//'"'
    enddo
    do L=1,n_spec
      write(78,*) '"RR_ave '//trim(species_name(L))//' [1/s]"'
    enddo
    !write(78, 1)  time*time_ref, nx_g, ny_g
    write(78, 2)  nx_g, ny_g
  end if
  !----------------------------------------
  do i = 1, ny
    grid(:,i) = x(:)
  end do
  call write_XYfld(78,grid,l_ref*1e3)
  do i = 1, nx
    grid(i,:) = y(:)
  end do
  call write_XYfld(78,grid,l_ref*1e3)

  !----------------------------------------
  call print_avg(T_num(:,:,n), T_den(:,:,n), ave, .true., t_ref)
  call print_avg(Cf_num(:,:,n), Cf_den(:,:,n), ave, .true., 1.0)
  call dump_XYfld('Cf_ave'//trim(xiid_ext),ave)
  call print_avg(hr_num(:,:,n), hr_den(:,:,n), ave, .true., -hr_ref)
  !----------------------------------------
  call print_avg(U_num(:,:,1,n), U_den(:,:,1,n), ave, .true., a_ref)
  call print_avg(U_num(:,:,2,n), U_den(:,:,2,n), ave, .false., a_ref)
  call print_avg(U_num(:,:,3,n), U_den(:,:,3,n), ave, .true., a_ref)
  !----------------------------------------
  call print_avg(Uf_num(:,:,1,n), Uf_den(:,:,1,n), ave, .true., a_ref)
  call dump_XYfld('Uf1_ave'//trim(xiid_ext),ave)
  call print_avg(Uf_num(:,:,2,n), Uf_den(:,:,2,n), ave, .false., a_ref)
  call dump_XYfld('Uf2_ave'//trim(xiid_ext),ave)
  call print_avg(Uf_num(:,:,3,n), Uf_den(:,:,3,n), ave, .true., a_ref)
  !----------------------------------------
  call print_avg(Uf_num_ub(:,:,1,n), Uf_den_ub(:,:,1,n), ave, .true., a_ref)
  call dump_XYfld('Uf1_ub_ave'//trim(xiid_ext),ave)
  call print_avg(Uf_num_ub(:,:,2,n), Uf_den_ub(:,:,2,n), ave, .false., a_ref)
  call dump_XYfld('Uf2_ub_ave'//trim(xiid_ext),ave)
  call print_avg(Uf_num_ub(:,:,3,n), Uf_den_ub(:,:,3,n), ave, .true., a_ref)
  !----------------------------------------
  do i = 1, n_spec
    call print_avg(Yf_num(:,:,i,n), Yf_den(:,:,i,n), ave, .true., 1.0)
  end do
  do i = 1, n_spec
    call print_avg(Y2f_num(:,:,i,n), Y2f_den(:,:,i,n), ave, .true., 1.0)
  end do
  do i = 1, n_spec
    call print_avg(rr_num(:,:,i,n), rr_den(:,:,i,n), ave, .true., 1.0/time_ref)
  end do

  if(myid == 0)close(78)
enddo !nbinz loop


  deallocate(hr_num, hr_den)
  deallocate(T_num, T_den)
  deallocate(Cf_num, Cf_den)
  deallocate(U_num, U_den)
  deallocate(Uf_num, Uf_den)
  deallocate(Yf_num, Yf_den)
  deallocate(Y2f_num, Y2f_den)
  deallocate(rr_num, rr_den)
  initialized = .false.
  if(myid == 0) then
!    close(54)
    write(io,*) 'Finished Zavg stats'
  end if
end if

1 format(' zone t= "',1pe9.3,'", i=',i5,', j=',i5,', f=block')
2 format(' zone t= tz_ave" i=',i5,', j=',i5,', f=block')

return

contains
!----------------------------------------------------------------------
subroutine print_avg(num, den, ave, symm, ref)
implicit none
real, dimension(nx, ny), intent(inout) :: num, den
real, dimension(nx, ny), intent(out) :: ave
logical, intent(in) :: symm
real, intent(in) :: ref

!Apply window averaging
call calc_XYfld_Xmovsum(num, window)
call calc_XYfld_Xmovsum(den, window)

!Apply symmetry condition before dividing
call calc_XYfld_Ysym(num, symm)
call calc_XYfld_Ysym(den, .true.)
where (den .lt. stat_small)
  ave = 0.0
elsewhere
  ave = num/den
end where
call write_XYfld(78,ave,ref)

return
end subroutine print_avg

end subroutine calc_tz_ave

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_turb(io, finish)
use topology_m, only: myid
use chemkin_m, only: reaction_rate, species_name
use thermchem_m, only: cpmix
use transport_m, only: getviscosity, computecoefficients
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

real, save, allocatable, dimension(:, :, :, :) :: Uave
real, save, allocatable, dimension(:, :, :) :: Up_num, Up_den
real, save, allocatable, dimension(:, :, :) :: vis_num, vis_den
real, save, allocatable, dimension(:, :, :) :: eps_num, eps_den

real, dimension(nx,ny,nz) :: wt, Cprog, rho, uprime2, vis, eps
real, dimension(nx,ny,nz,3) :: uprime
logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx, ny) :: grid
real, dimension(nx, ny) :: up_mean, vis_mean, eps_mean, l_t, l_k
integer i, L, n

character*100 filename
character*2 xiid_ext

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_turb'
  allocate(Up_num(nx, ny, 0:nbinz), Up_den(nx, ny, 0:nbinz))
  allocate(vis_num(nx, ny, 0:nbinz), vis_den(nx, ny, 0:nbinz))
  allocate(eps_num(nx, ny, 0:nbinz), eps_den(nx, ny, 0:nbinz))

  Up_num = 0.0; Up_den = 0.0;
  vis_num = 0.0; vis_den = 0.0;
  eps_num = 0.0; eps_den = 0.0;

  allocate(Uave(nx, ny, 3, 0:nbinz))

  Uave = 0.0;
  
  do n=0,nbinz
  write(xiid_ext,'(I2.2)') n   
  call load_XYfld('Uf1_ave'//trim(xiid_ext),Uave(:,:,1,n))
  call load_XYfld('Uf2_ave'//trim(xiid_ext),Uave(:,:,2,n))
  enddo
  Uave(:,:,3,:) = 0.0
  initialized = .true.
end if

if(.not. finish) then
  wt = 1.0
  ifcond = .true.
  rho = q(:,:,:,4,1)

do n=0,nbinz
! set the ifcond mask here.
  if(n.eq.0)then
    ifcond = .true.
  else
  where(zetafield .gt. binz_val(n-1) .and. &
    zetafield .le. binz_val(n) )
    ifcond = .true.
  elsewhere
    ifcond = .false.
  endwhere
  endif
                
  do i = 1, nz
    uprime(:,:,i,:) = u(:,:,i,:) - Uave(:,:,:,n)
  end do
  uprime2 = 0.0
  do i = 1, 3
    uprime2 = uprime2+uprime(:,:,:,i)*uprime(:,:,:,i)
  end do
  uprime2 = uprime2/3.0
  call calc_z_accum_numden (uprime2, ifcond, rho, Up_num(:,:,n), Up_den(:,:,n))

  !Viscosity
!  call computeCoefficients(cpmix, temp)
!  call computeCoefficients( pressure,temp,yspecies,rho)
  vis = getviscosity()
  ! convert to kinematic viscosity
  vis = vis*volum
  call calc_z_accum_numden (vis, ifcond, wt, vis_num(:,:,n), vis_den(:,:,n))

  !Dissipation rate
  call calculate_dissipation(u, uprime, volum, eps)
  call calc_z_accum_numden (eps, ifcond, rho, eps_num(:,:,n), eps_den(:,:,n))

enddo  !nbinz loop  
else
  !Finishing step. Write the data out.
  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing tZ turb stats'

do n=0,nbinz
  if (myid == 0) then
    write(xiid_ext,'(I2.2)') n
    filename = '../post/tecplot/tZturb'//trim(xiid_ext)//'.tec'
    open(unit=78, file=trim(filename), status='unknown')

    write(78,*) 'title = tZ-turb'
    write(78,*) 'variables = '
    write(78,*) '"x [mm]"', '"y [mm]"'
    write(78,*) '"Cf_ave"'
    write(78,*) '"uprime [m/s]"'
    write(78,*) '"viscosity [m^2/s]"'
    write(78,*) '"epsilon"'
    write(78,*) '"l_k"'
    write(78,*) '"l_t"'
    !write(78, 1)  time*time_ref, nx_g, ny_g
    write(78, 2)  nx_g, ny_g
  end if

  !----------------------------------------
  do i = 1, ny
    grid(:,i) = x(:)
  end do
  call write_XYfld(78,grid,l_ref*1e3)
  do i = 1, nx
    grid(i,:) = y(:)
  end do
  call write_XYfld(78,grid,l_ref*1e3)


  !----------------------------------------
  call load_XYfld('Cf_ave'//trim(xiid_ext),grid)
  call write_XYfld(78,grid,1.0)
  !----------------------------------------
  !uprime needs a square root and special treatment
  call calc_XYfld_Xmovsum(Up_num(:,:,n), window)
  call calc_XYfld_Xmovsum(Up_den(:,:,n), window)
  call calc_XYfld_Ysym(Up_num(:,:,n), .true.)
  call calc_XYfld_Ysym(Up_den(:,:,n), .true.)
  where (Up_den(:,:,n) .lt. stat_small)
    up_mean = 0.0
  elsewhere
    up_mean = up_num(:,:,n)/up_den(:,:,n)
  end where
  up_mean = sqrt(up_mean)
  call write_XYfld(78,up_mean,a_ref)

  !----------------------------------------
  call print_avg(Vis_num(:,:,n), Vis_den(:,:,n), vis_mean, .true., a_ref*l_ref)
  call print_avg(eps_num(:,:,n), eps_den(:,:,n), eps_mean, .true., a_ref*a_ref/time_ref)

  !Kolmogorov scale
  l_k = (vis_mean**3/max(eps_mean,1e-30))**0.25
  call write_XYfld(78, l_k, l_ref*1e3)
  
  !Turbulence length scale
  l_t = (up_mean**3/max(eps_mean,1e-30))
  call write_XYfld(78, l_t, l_ref*1e3)

  close(78)
enddo   !nbinz loop

  deallocate(Up_num, Up_den)
  deallocate(vis_num, vis_den)
  deallocate(eps_num, eps_den)
  deallocate(Uave)
  initialized = .false.
  if(myid == 0) then
!    close(54) 
    write(io,*) 'Finished Zavg stats'
  end if
end if

1 format(' zone t= "',1pe9.3,'", i=',i5,', j=',i5,', f=block')
2 format(' zone t= tz_ave" i=',i5,', j=',i5,', f=block')

return

contains
!----------------------------------------------------------------------
subroutine print_avg(num, den, ave, symm, ref)
implicit none
real, dimension(nx, ny), intent(inout) :: num, den
real, dimension(nx, ny), intent(out) :: ave
logical, intent(in) :: symm
real, intent(in) :: ref

!Apply window averaging
call calc_XYfld_Xmovsum(num, window)
call calc_XYfld_Xmovsum(den, window)
!Apply symmetry condition before dividing
call calc_XYfld_Ysym(num, symm)
call calc_XYfld_Ysym(den, .true.)
where (den .lt. stat_small)
  ave = 0.0
elsewhere
  ave = num/den
end where
call write_XYfld(78,ave,ref)

return
end subroutine print_avg

end subroutine calc_turb

!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_c_gradc(io, finish)
use topology_m, only: myid, gcomm
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

integer, parameter :: nbinc = 50
real, parameter :: binc_start = 0.0, binc_del = 0.02
real, save, dimension(0:nbinc) :: binc_val
real, save, allocatable, dimension(:, :, :) :: gradC_num, gradC_den

integer, parameter :: nbingradc = 40
real, save, dimension(0:nbingradc, 1:nbinc) :: bingradc_val
real, save, allocatable, dimension(:, :, :, :) :: pdf_gradc

real, dimension(nx, 1:nbinc, 0:nbinz) :: gradC_ave

logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, mag_gradC
real prbl
integer i, n, l, m

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_c_gradc'
  do n = 0, nbinc
    binc_val(n) = binc_start + binc_del*real(n)
  end do

  ! Initialized grad_c bins based on grad_c min/max at first time
  call calculate_progvar(Cprog,io)
  call calc_mag_grad(Cprog, mag_gradC)
  do n = 1, nbinc
!    where(Cprog .gt. binc_val(n-1) .and. &
!          Cprog .le. binc_val(n) )
!      ifcond = .true.
!    elsewhere 
!      ifcond = .false.
!    endwhere
!    call make_bins(mag_gradC, ifcond, gcomm, nbingradc, bingradc_val(:,n))
    prbl = 119.0 - 272.0*0.25*(binc_val(n-1)+binc_val(n)-1.0)**2
    call make_bins_lmts(0.0, prbl, nbingradc, bingradc_val(:,n))
  end do

  allocate(pdf_gradc(nx,0:nbingradc+1,1:nbinc,0:nbinz))
  allocate(gradC_num(nx,1:nbinc,0:nbinz))
  allocate(gradC_den(nx,1:nbinc,0:nbinz))

  pdf_gradc = 0.0
  gradC_num = 0.0; gradC_den = 0.0;
  initialized = .true.
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
  call calc_mag_grad(Cprog, mag_gradC)
  wt = 1.0

  do m = 0, nbinz
  do n = 1, nbinc
    where((Cprog .gt. binc_val(n-1) .and.            &
           Cprog .le. binc_val(n))                   &
     .and.                                           &
         ((zetafield.gt.binz_val(m-1) .and.          &
           zetafield.le.binz_val(m)          ) .or.  &
           (m.eq.0))    )                           ! bin zero is unconditional 
      ifcond = .true.
    elsewhere 
      ifcond = .false.
    endwhere
    
!esr    !----------------------------------------
!esr    !On each processor, find the pdf of grad_c on each yz-plane
!esr    call calc_yz_pdf_addtl(mag_gradC, ifcond, wt, &
!esr           nbingradc, bingradc_val(:,n), pdf_gradc(:,:,n,m) )
!esr    !----------------------------------------
    do i=1,nx
      call calc_yz_hist_addtl(mag_gradC(i,:,:), ifcond(i,:,:), wt, nbingradc, bingradc_val(:,n), pdf_gradc(i,:,n,m))
    enddo
    !----------------------------------------
    ! Calculate conditional mean grad_c
    call calc_yz_accum_numden (mag_gradC, ifcond, wt, gradc_num(:,n,m), gradc_den(:,n,m))
  end do
  end do
else !Finishing step
  !----------------------------------------
  !Apply the moving window average
  do m = 0, nbinz
  do n = 1, nbinc
    call calc_Xfld_Xmovsum(gradc_num(:,n,m), window)
    call calc_Xfld_Xmovsum(gradc_den(:,n,m), window)
    do l = 0, nbingradc+1
      call calc_Xfld_Xmovsum(pdf_gradc(:,l,n,m), window)
    end do
  end do
  end do
  !----------------------------------------
  !Normalize the pdf
  do m = 0, nbinz
  do n = 1, nbinc
    do i = 1, nx
!esr      call calc_norm_from_hist(nbingradc, bingradc_val, pdf_gradc(i,:,n,m), pdf_gradc(i,1:nbingradc,n,m))
!esr      pdf_gradc(i,0,n,m)=0.0
!esr      pdf_gradc(i,nbingradc+1,n,m)=0.0
      call normalize_pdf(nbingradc, pdf_gradc(i,:,n,m))
    end do
  end do
  end do
  !----------------------------------------
  !Find gradc_ave
  where(gradc_den .lt. stat_small)
    gradc_ave = 0.0
  elsewhere
    gradc_ave = gradc_num/gradc_den
  end where
  !----------------------------------------
  !Write the data out.
  !For the pdfs each yz_id==0 writes out a tecplot contour file.
  call write_data(0.25)
  call write_data(0.5)
  call write_data(0.75)
end if

! WARNING, YOU HAVE NOT DEALLOCATED VARIABLES
return

contains
  !----------------------------------------
  subroutine write_data(b)

! bug fix by esr, 3 June 2008. il was found after the call to calc_cumpdf_frompdf, this was giving a seg fault.
  
  implicit none
  real, intent(in) :: b

  integer i
  integer n, l, m
  integer il
  character*4 loc_ext
  character*100 filename
  character*2 xiid_ext

  real, dimension(0:nbingradc+1, 1:nbinc, 0:nbinz) :: cumpdf_gradc

  cumpdf_gradc(:,:,:)=0.0;
  do m=0,nbinz

  i = int(nx_g*b)

  if (i<=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return

  il = i - xid*nx

  do n = 1, nbinc
    call calc_cumnorm_from_hist(nbingradc, pdf_gradc(il,:,n,m), cumpdf_gradc(:,n,m))
!esr    call calc_cumpdf_frompdf(nbingradc, pdf_gradc(il, :, n, m), cumpdf_gradc(:, n, m))
  end do

!  il = i - xid*nx
  write(loc_ext,'(F4.2)') b
  write(xiid_ext,'(I2.2)') m
  filename = '../post/tecplot/c_gradc_pdf.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'title = c_gradc_pdf'
  write(78,*) 'variables = '
  write(78,*) '"C" "gradC[1/mm]"'
  write(78,*) '"f"'
  write(78,*) '"cumf"'
  write(78, 2)  nbinc, nbingradc+2

  do l = 0, nbingradc+1
    write(78,9) (0.5*(binc_val(n-1)+binc_val(n)), n = 1, nbinc)
  end do

  do l = 0, nbingradc+1
    if( l .eq. 0) then
      write(78,9) (bingradc_val(l,n)/l_ref/1e3, n = 1, nbinc)
    else if (l .eq. nbingradc+1) then
      write(78,9) (bingradc_val(l-1,n)/l_ref/1e3, n = 1, nbinc)
    else
      write(78,9) (0.5*(bingradc_val(l-1,n)+bingradc_val(l,n))/l_ref/1e3, &
                   n = 1, nbinc)
    end if
  end do

  do l = 0, nbingradc+1
    do n = 1, nbinc
      write(78,9) pdf_gradc(il,l,n,m)
    end do
  end do

 do l = 0, nbingradc+1
    do n = 1, nbinc
      write(78,9) cumpdf_gradc(l,n,m)
    end do
  end do
  close(78)
  !----------------------------------------
  filename = '../post/tecplot/c_ave.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'title = c_ave'
  write(78,*) 'variables = '
  write(78,*) '"C"'
  write(78,*) '"mag_gradC[1/mm]"'
  write(78, 3)  nbinc

  write(78,9) (0.5*(binc_val(n-1)+binc_val(n)), n = 1, nbinc)
  write(78,9) (gradC_ave(il, n, m)/l_ref/1e3, n = 1, nbinc)
  close(78)

enddo !nbinz loop


2 format(' zone t= c_gradc_pdf" i=',i5,', j=',i5,', f=block')
3 format(' zone t= c_ave" i=',i5,', f=block')
9 format(10(1pe12.5,1x))

  return
  end subroutine write_data


end subroutine calc_c_gradc

!----------------------------------------------------------------------
! A utility to write a tecplot file with the mean velocity field
! mean C field and du/dy, d2u/dy2 etc, needed to mark the mixing layer.
subroutine write_meanfield(io)
use topology_m, only: myid
use grid_m, only: x, y, scale_1y
implicit none
integer, intent(in) :: io

real, dimension(nx, ny, nz) :: Uf1ave, dU, d2U
real, dimension(nx, ny) :: Cfave, grid
integer i, j, k, m

character*100 filename
character*2 xiid_ext


if(myid==0) write(io,*) '!----------------------------------------'
if(myid==0) write(io,*) 'Writing mean field'

do m=0,nbinz
write(xiid_ext,'(I2.2)') m
call load_XYfld('Uf1_ave'//trim(xiid_ext),Uf1ave(:,:,1))
do k = 2, nz
  Uf1ave(:,:,k) = Uf1ave(:,:,1)
end do
call load_XYfld('Cf_ave'//trim(xiid_ext),Cfave)

call derivative_y(nx, ny, nz, Uf1ave, dU, scale_1y, 1)
call derivative_y(nx, ny, nz, dU, d2U, scale_1y, 1)
!Write the data out.
if (myid == 0) then
  filename = '../post/tecplot/tz_ave_d2U'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'title = tZ-d2U'
  write(78,*) 'variables = '
  write(78,*) '"x [mm]" "y[mm]" "C"'
  write(78,*) '"U1 [m/s]"'
  write(78,*) '"dU1 [1/s]"'
  write(78,*) '"d2U1 [1/m/s]"'
  write(78, 2)  nx_g, ny_g
end if

!----------------------------------------
do j = 1, ny_g
  call write_Xfld(78,x,l_ref*1e3)
end do
do i = 1, nx
  grid(i,:) = y(:)
end do
call write_XYfld(78,grid,l_ref*1e3)

call write_XYfld(78,Cfave,1.0)
call write_XYfld(78,Uf1ave(:,:,1),a_ref)
call write_XYfld(78,dU(:,:,1),time_ref)
call write_XYfld(78,d2U(:,:,1),time_ref/l_ref)

! PUT A BARRIER HERE SO THAT ALL PROCS HAVE FINISHED BEFORE I CLOSE
call MPI_Barrier(gcomm, ierr)
if(myid.eq.0)close(78)

enddo !nbinz loop

1 format(' zone t= "',1pe9.3,'", i=',i5,', j=',i5,', f=block')
2 format(' zone t= tz_ave" i=',i5,', j=',i5,', f=block')

return
end subroutine write_meanfield

!----------------------------------------------------------------------
subroutine gradc_stats_given_c(io, finish)
use topology_m, only: myid, gcomm
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

!real, parameter :: Cgiven=0.6, Cdel=0.02

integer, parameter :: nbingradc = 25
real, save, dimension(0:nbingradc) :: bingradc_val
real, save, allocatable, dimension(:,:,:) :: pdf_gradc
real, save, allocatable, dimension(:,:,:) ::  &
         at_num,   at_den, &
         curv_num, curv_den 
real, allocatable, dimension(:,:,:) :: at_ave, curv_ave        

logical, dimension(nx,ny,nz) :: C_cond, gradC_cond, ifcond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, mag_gradC, curv, strain
real, dimension(nx,ny,nz,3) :: normal
real, dimension(nx) :: num, den
real, dimension(0:nbinz) :: pdf_sum
character*100 filename
character*2 xiid_ext
integer n, m, i, l

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing gradc_stats_given_c'
  call make_bins_lmts(0.0, 120.0, nbingradc, bingradc_val)

allocate(pdf_gradc(nx,0:nbingradc+1,0:nbinz))
allocate(at_num(nx,1:nbingradc, 0:nbinz))
allocate(at_den(nx,1:nbingradc, 0:nbinz))
allocate(curv_num(nx,1:nbingradc, 0:nbinz))
allocate(curv_den(nx,1:nbingradc, 0:nbinz))


  pdf_gradc = 0.0
  at_num = 0.0; at_den = 0.0
  curv_num = 0.0; curv_den = 0.0

  initialized = .true.
end if

if(.not. finish) then
  !----------------------------------------
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)

  call computeDivergence(normal, curv)
  curv = abs(curv)
  call calc_strain(normal, strain)
  !----------------------------------------
  wt = 1.0
        
do m=0,nbinz

!use ifcond for the mixture fraction condition, it is used for other purposes later.  
  where((zetafield .gt. binz_val(m-1) .and. &
     zetafield .le. binz_val(m)).or.m.eq.0 )
    ifcond = .true.
  elsewhere
    ifcond = .false.
  endwhere               
  
  where(Cprog .gt. Cval_param - Cdel_param .and. Cprog .le. Cval_param + Cdel_param)
    C_cond = .true.
  elsewhere 
    C_cond = .false.
  endwhere

  C_cond=C_cond.and.ifcond
      
!----------------------------------------
!esr!On each processor, find the pdf of grad_c on each yz-plane
!esr  call calc_yz_pdf_addtl(mag_gradC, C_cond, wt, &
!esr  nbingradc, bingradc_val(:), pdf_gradc(:,:,m) )
    do i=1,nx
      call calc_yz_hist_addtl(mag_gradC(i,:,:), C_cond(i,:,:), wt, nbingradc, bingradc_val(:), pdf_gradc(i,:,m))
    enddo

  do n = 1, nbingradc
    where(mag_gradC .gt. bingradc_val(n-1) .and. &
          mag_gradC .le. bingradc_val(n))
      gradC_cond = .true.
    elsewhere 
      gradC_cond = .false.
    endwhere
    ifcond = C_cond .and. gradC_cond
    !----------------------------------------

! Calculate conditional mean grad_c
  call calc_yz_accum_numden (strain, ifcond, wt, at_num(:,n,m), at_den(:,n,m))
  call calc_yz_accum_numden (curv, ifcond, wt, curv_num(:,n,m), curv_den(:,n,m))

  end do
end do !nbinz loop


else !Finishing step

allocate(at_ave(nx,1:nbingradc, 0:nbinz))
allocate(curv_ave(nx,1:nbingradc, 0:nbinz))

!  if(myid.eq.0)write(io,*) 'at allocated'
      

!----------------------------------------
!Apply the moving window average
  do m = 0, nbinz
  do n = 1, nbingradc
    call calc_Xfld_Xmovsum(at_num(:,n,m), window)
    call calc_Xfld_Xmovsum(at_den(:,n,m), window)
    call calc_Xfld_Xmovsum(curv_num(:,n,m), window)
    call calc_Xfld_Xmovsum(curv_den(:,n,m), window)
  end do
    do l = 0, nbingradc+1
      call calc_Xfld_Xmovsum(pdf_gradc(:,l,m), window)
    end do
  end do

!  if(myid.eq.0)write(io,*) ' after window'
      
  
!----------------------------------------
!Normalize the pdf
  do m = 0, nbinz
    do i = 1, nx
!esr      call calc_norm_from_hist(nbingradc, bingradc_val, pdf_gradc(i,:,m), pdf_gradc(i,1:nbingradc,m))
!esr      pdf_gradc(i,0,m)=0.0
!esr      pdf_gradc(i,nbingradc+1,m)=0.0
      call normalize_pdf(nbingradc, pdf_gradc(i,:,m))
    end do
  end do

!  if(myid.eq.0)write(io,*) ' after norm'
      
!----------------------------------------
!Find gradc_ave
  where(at_den .lt. stat_small)
    at_ave = 0.0
  elsewhere
    at_ave = at_num/at_den
  end where
  where(curv_den .lt. stat_small)
    curv_ave = 0.0
  elsewhere
    curv_ave = curv_num/curv_den
  end where

!    if(myid.eq.0)write(io,*) 'after division'
        
!----------------------------------------
!Write the data out.
!For the pdfs each yz_id==0 writes out a tecplot contour file.
  call write_data(0.25)
  call write_data(0.5)
  call write_data(0.75)
 
end if !Finish

! WARNING YOU HAVE NOT DEALLOCATED VARIABLES

contains
  !----------------------------------------
  subroutine write_data(b)

! bug fix by esr, 3 June 2008. il was found after the call to calc_cumpdf_frompdf, this was giving a seg fault.
  
  implicit none
  real, intent(in) :: b

  integer i
  integer n, l, m
  integer il
  character*4 loc_ext
  character*100 filename
  character*2 xiid_ext

  real, dimension(0:nbingradc+1, 0:nbinz) :: cumpdf_gradc
  real, dimension(1:nbingradc) :: ave

!if(myid.eq.0)write(io,*)'in write data'


  cumpdf_gradc(:,:)=0.0;
  do m=0,nbinz

  i = int(nx_g*b)

  if (i<=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return

  il = i - xid*nx

!write(io,*)'il',il,m
    call calc_cumnorm_from_hist(nbingradc, pdf_gradc(il, :, m), cumpdf_gradc(:, m))
!esr    call calc_cumpdf_frompdf(nbingradc, pdf_gradc(il, :, m), cumpdf_gradc(:, m))
!write(io,*)'after cum'

!  il = i - xid*nx
  write(loc_ext,'(F4.2)') b
  write(xiid_ext,'(I2.2)') m
  filename = '../post/tecplot/gradc_cndtl_stats.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

       write(78,*) 'title = gradc_cndtl_stats'
       write(78,*) 'variables = '
       write(78,*) '"mag_gradC[1/mm]"'
       write(78,*) '"pdf"'
       write(78,*) '"strain[1/s]"'
       write(78,*) '"Curvature[1/mm]"'
       write(78, 3)  nbingradc
   
       write(78,9) (0.5*(bingradC_val(n-1)+bingradC_val(n)), n = 1, nbingradc)
       write(78,9) (pdf_gradc(il,n,m), n = 1, nbingradc)
!       call print_avg(at_num(il,:,m), at_den(il,:,m), 1.0/time_ref)

       where (at_den(il,:,m).gt.stat_small)
         ave = at_num(il,:,m)/at_den(il,:,m)
       elsewhere
         ave = 0.0
       end where
       write(78,9) (ave(n)/time_ref, n=1, nbingradc)

!       call print_avg(curv_num(il,:,m), curv_den(il,:,m), 1.0/l_ref/1e3)
       where (curv_den(il,:,m).gt.stat_small)
         ave = curv_num(il,:,n)/curv_den(il,:,n)
       elsewhere
         ave = 0.0
       end where
       write(78,9) (ave(n)/l_ref/1e3, n=1, nbingradc)

  close(78)

enddo !nbinz loop

3 format(' zone t= gradc_cndtl_stats" i=',i5,', f=block')
9 format(10(1pe12.5,1x))

  return
  end subroutine write_data


!ESR   deallocate(pdf_gradc)
!ESR   deallocate(at_num)
!ESR   deallocate(at_den)
!ESR   deallocate(curv_num)
!ESR   deallocate(curv_den)
!ESR   deallocate(at_ave)
!ESR   deallocate(curv_ave)

end subroutine gradc_stats_given_c

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_c_spcs(io, finish)
use topology_m, only: myid
use chemkin_m, only: reaction_rate, species_name
use mixfrac_m
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

integer, parameter :: nbinc = 50
real, parameter :: binc_start = 0.0, binc_del = 0.02
real, save, dimension(0:nbinc) :: binc_val

real, save, allocatable, dimension(:,:,:,:) :: Y_num, Y_den, RR_num, RR_den
real, save, allocatable, dimension(:,:,:) :: hr_num, hr_den, &
                           T_num, T_den, strn_num, strn_den, phi_num, phi_den,&
                           P_num, P_den

logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, hr, strain, mag_gradC
real, dimension(nx,ny,nz,3) :: normal
real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion

integer n, m, l

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_c_spcs'
  do n = 0, nbinc
    binc_val(n) = binc_start + binc_del*real(n)
  end do

  allocate(Y_num(nx,1:nbinc,n_spec,0:nbinz), Y_den(nx,1:nbinc,n_spec,0:nbinz))
  allocate(RR_num(nx,1:nbinc,n_spec,0:nbinz), RR_den(nx,1:nbinc,n_spec,0:nbinz))
  allocate(hr_num(nx,1:nbinc,0:nbinz), hr_den(nx,1:nbinc,0:nbinz))
  allocate(T_num(nx,1:nbinc,0:nbinz), T_den(nx,1:nbinc,0:nbinz))
  allocate(P_num(nx,1:nbinc,0:nbinz), P_den(nx,1:nbinc,0:nbinz))
  allocate(strn_num(nx,1:nbinc,0:nbinz), strn_den(nx,1:nbinc,0:nbinz))
  allocate(phi_num(nx,1:nbinc,0:nbinz), phi_den(nx,1:nbinc,0:nbinz))
  Y_num = 0.0; Y_den = 0.0;
  RR_num = 0.0; RR_den = 0.0;
  hr_num = 0.0; hr_den = 0.0;
  T_num = 0.0; T_den = 0.0;
  P_num = 0.0; P_den = 0.0;
  strn_num = 0.0; strn_den = 0.0;
  phi_num = 0.0; phi_den = 0.0;
  initialized = .true.
  call allocate_mixFrac_arrays(1)
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
  call calc_heat_release(rr_r, hr)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call calc_strain(normal, strain)
  call specToMixFr(yspecies)
  call mixFracToPhi(mixFrac, phi)
  wt = 1.0
  do l = 0, nbinz
  do n = 1, nbinc
    !----------------------------------------
    where(Cprog .gt. binc_val(n-1) .and.  &
          Cprog .le. binc_val(n)          &
     .and.((Zetafield.gt.binz_val(l-1).and. &
          Zetafield.le.binz_val(l)) .or.(l.eq.0))        &
     )
      ifcond = .true.
    elsewhere 
      ifcond = .false.
    endwhere
    !----------------------------------------
    do m = 1, n_spec
      call calc_yz_accum_numden &
        (yspecies(:,:,:,m), ifcond, wt, Y_num(:,n,m,l), Y_den(:,n,m,l))
    end do
    do m = 1, n_spec
      call calc_yz_accum_numden &
        (rr_r(:,:,:,m), ifcond, wt, RR_num(:,n,m,l), RR_den(:,n,m,l))
    end do
    call calc_yz_accum_numden (hr, ifcond, wt, hr_num(:,n,l), hr_den(:,n,l))
    call calc_yz_accum_numden (temp, ifcond, wt, T_num(:,n,l), T_den(:,n,l))
    call calc_yz_accum_numden (pressure, ifcond, wt, P_num(:,n,l), P_den(:,n,l))
    call calc_yz_accum_numden (strain, ifcond, wt, strn_num(:,n,l), strn_den(:,n,l))
    call calc_yz_accum_numden (phi, ifcond, wt, phi_num(:,n,l), phi_den(:,n,l))
    !----------------------------------------
  end do
  end do
else !Finishing step
  !----------------------------------------
  !Apply the moving window average
  do l = 0, nbinz
  do n = 1, nbinc
    do m = 1, n_spec
      call calc_Xfld_Xmovsum(Y_num(:,n,m,l), window)
      call calc_Xfld_Xmovsum(Y_den(:,n,m,l), window)
      call calc_Xfld_Xmovsum(RR_num(:,n,m,l), window)
      call calc_Xfld_Xmovsum(RR_den(:,n,m,l), window)
    end do
    call calc_Xfld_Xmovsum(hr_num(:,n,l), window)
    call calc_Xfld_Xmovsum(hr_den(:,n,l), window)
    call calc_Xfld_Xmovsum(T_num(:,n,l), window)
    call calc_Xfld_Xmovsum(T_den(:,n,l), window)
    call calc_Xfld_Xmovsum(P_num(:,n,l), window)
    call calc_Xfld_Xmovsum(P_den(:,n,l), window)
    call calc_Xfld_Xmovsum(strn_num(:,n,l), window)
    call calc_Xfld_Xmovsum(strn_den(:,n,l), window)
    call calc_Xfld_Xmovsum(phi_num(:,n,l), window)
    call calc_Xfld_Xmovsum(phi_den(:,n,l), window)
  end do
  end do
  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing calc_c_spcs'

  call write_data(0.25)
  call write_data(0.50)
  call write_data(0.75)

end if !Finish

contains
  !----------------------------------------
  subroutine write_data(b)
  implicit none
  real, intent(in) :: b
  integer i, il

  character loc_ext*4, filename*100
  character xiid_ext*2

  i = int(nx_g*b)
  if(i <=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return
  il = i - xid*nx

  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing calc_c_spcs'

  write(loc_ext,'(F4.2)') b
  do l=0,nbinz
  write(xiid_ext,'(I2.2)')l
  filename = '../post/tecplot/c_spcs.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'title = c_spcs'
  write(78,*) 'variables = '
  write(78,*) '"Cprog"'
  write(78,*) '"T"'
  write(78,*) '"HR"'
  write(78,*) '"strain[s^-1]"'
  write(78,*) '"EquivRatio"'
  do m=1,n_spec
    write(78,*) '"'//trim(species_name(m))//'"'
  enddo
  do m=1,n_spec
    write(78,*) '"RR-'//trim(species_name(m))//'"'
  enddo
  !----------------------------------------
  ! CMC Stuff
  do m=1,n_spec
    write(78,*) '"CMC-RR-'//trim(species_name(m))//'"'
  enddo
  !----------------------------------------------------------------------
  write(78, 3)  b, nbinc
  write(78,9) (0.5*(binc_val(n-1)+binc_val(n)), n = 1, nbinc)
  call print_avg(T_num(il,:,l), T_den(il,:,l), t_ref)
  call print_avg(hr_num(il,:,l), hr_den(il,:,l), -hr_ref)
  call print_avg(strn_num(il,:,l), strn_den(il,:,l), 1.0/time_ref)
  call print_avg(phi_num(il,:,l), phi_den(il,:,l), 1.0)
  do m = 1, n_spec
    call print_avg(Y_num(il,:,m,l), Y_den(il,:,m,l), 1.0)
  end do
  do m = 1, n_spec
    call print_avg(RR_num(il,:,m,l), RR_den(il,:,m,l), rr_ref)
  end do
  !----------------------------------------------------------------------
  ! Print the CMC Reaction rate
  call write_cmc_rates(il,l)

  close(78)
  end do  !nbinz

3 format(' zone t= "c_spcs-',F4.2,'", i=',i5,', f=block')
9 format(10(1pe12.5,1x))

  end subroutine write_data

  !----------------------------------------
  subroutine print_avg(num, den, ref)
  implicit none
  real, intent(in), dimension(1:nbinc) :: num, den
  real, intent(in) :: ref
  real :: ave(1:nbinc)
  integer n
  
  where(den .gt. stat_small) 
     ave = num/den 
  elsewhere
     ave = 0.0
  end where
  write(78,9) (ave(n)*ref, n = 1, nbinc)
  
  9 format(10(1pe12.5,1x))
  return
  end subroutine print_avg

  !----------------------------------------
  subroutine write_cmc_rates(il,l)
  use reference_m
  use chemkin_m
  implicit none
  integer, intent(in) :: il,l
  integer m, n
  real :: rr_cmc(1:n_spec, 1:nbinc)
  real :: yave(1:n_spec), t_ave, p_ave, rr1(1:n_spec)

  rr_cmc = 0.0
  do n = 1, nbinc
    if(t_den(il,n,l).lt.stat_small) cycle
    t_ave = T_num(il,n,l)/T_den(il,n,l)
    p_ave = P_num(il,n,l)/P_den(il,n,l)
    yave(:) = Y_num(il,n,:,l)/Y_den(il,n,:,l)
    call getrates &
      (p_ave*p_ref*10.0, t_ave*t_ref, yave, ickwrk, rckwrk, rr1)
    rr_cmc(:, n) = rr1(:)*molwt(:)*1e6/rr_ref
  end do
  do m = 1, n_spec
    write(78,9) (rr_cmc(m, n)*rr_ref, n = 1, nbinc)
  end do
  return
  9 format(10(1pe12.5,1x))
  end subroutine write_cmc_rates

end subroutine calc_c_spcs


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine print_pdfs(io)
implicit none
integer, intent(in) :: io

integer, parameter :: nbin = 100
real bin(0:nbin), pdf(0:nbin+1)
logical ifcond(nx, ny, nz)
real wt(nx, ny, nz)
integer i

ifcond = .true.
wt = 1.0

call make_bins(nx*ny*nz,temp, ifcond, gcomm, nbin, bin)
call calc_field_hist(temp, ifcond, wt, nbin, bin, pdf)
call calc_norm_from_hist(nbin, bin, pdf, pdf(1:nbin))
pdf(0)=0.0
pdf(nbin+1)=0.0

if(myid .eq. 0) then
  do i = 1, nbin
    print *, 0.5*(bin(i-1)+bin(i))*t_ref, pdf(i)
  end do
end if

return
end subroutine print_pdfs

!----------------------------------------------------------------------
subroutine print_zavgs(io)
!Prints in tecplot format
use chemkin_m, only: reaction_rate, species_name, n_species
use topology_m, only: myid, xcomm, ycomm, MPI_REAL8
use runtime_m, only: run_title, time
use grid_m, only: x, y

integer, intent(in) :: io

real, dimension(nx, ny, nz, n_spec) :: rr_r, diffusion
real, dimension(nx, ny, nz) :: hr

real, dimension(nx, ny) :: mean
real, dimension(nx, ny, nz) :: wt
logical, dimension(nx, ny, nz) :: ifcond

character*100 filename
character*9 time_ext
real, dimension(nx_g) :: x_g
real, dimension(ny_g) :: y_g
integer ierr
integer i, j, L

wt = 1.0
ifcond = .true.
if(myid==0) write(io,*) '!----------------------------'
if(myid==0) write(io,*) 'Writing Zavg stats'

#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
call calc_heat_release(rr_r, hr)

if (myid == 0) then
  write(time_ext,'(1pe9.3)') time*time_ref
  filename = '../post/tecplot/'//trim(run_title)//'_Z.'//trim(time_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'title = "',trim(run_title)//' '//trim(time_ext),' Z-means "'
  write(78,*) 'variables = '
  write(78,*) '"x (cm)"', '"y (cm)"'
  
  write(78,*) '"T_ave (K)" '
  write(78,*) '"hr_ave (J/m^3/s)"'
  write(78,*) '"u_ave (m/s)"'
  do L=1,n_species
    write(78,*) '"Y '//trim(species_name(L))//'"'
  enddo
  do L=1,n_species
    write(78,*) '"RR '//trim(species_name(L))//' (1/s)"'
  enddo
end if

call MPI_Gather(x,nx,MPI_REAL8,x_g,nx,MPI_REAL8,0,xcomm,ierr)
call MPI_Gather(y,ny,MPI_REAL8,y_g,ny,MPI_REAL8,0,ycomm,ierr) 
if (myid == 0) then
  write(78, 1)  time*time_ref, nx_g, ny_g
  write(78, 9) ((x_g(i)*l_ref*100.0, i=1,nx_g), j=1,ny_g)
  write(78, 9) ((y_g(j)*l_ref*100.0, i=1,nx_g), j=1,ny_g)
end if

call print_avg(temp, t_ref)

hr_ref = -cp_ref*t_ref*rho_ref/time_ref
call print_avg(hr, hr_ref)

call print_avg(u(:,:,:,1), a_ref)

do L = 1, n_species
  call print_avg(yspecies(:,:,:,L), 1.0)
end do
do L = 1, n_species
  call print_avg(rr_r(:,:,:,L), rr_ref)
end do

1 format(' zone t= "',1pe9.3,'", i=',i5,', j=',i5,', f=block')
9 format(10(1pe12.5,1x))

if(myid == 0) then
  close(54)
end if

if(myid ==0) write(io,*) 'Finished Zavg stats'

return

contains
!----------------------------------------------------------------------
subroutine print_avg(field, ref)
implicit none
real, dimension(nx, ny, nz), intent(in) :: field
real, intent(in) :: ref

real, dimension(nx, ny) :: mean

!Mean
call calc_z_mean(field, ifcond, wt, mean)
call write_XYfld(78,mean,ref)

return
end subroutine print_avg
!----------------------------------------------------------------------
end subroutine print_zavgs


!----------------------------------------------------------------------
subroutine print_xzavgs(io)
!Prints in tecplot format
use thermchem_m, only: cpmix
use chemkin_m, only: reaction_rate
use topology_m, only: myid, ycomm, MPI_REAL8
use runtime_m, only: run_title, time
use grid_m, only: y
use transport_m, only: getviscosity, computecoefficients

integer, intent(in) :: io

real, dimension(nx, ny, nz) :: wt, work, viscosity
real, dimension(nx, ny, nz, 3) :: lil_u
logical, dimension(nx, ny, nz) :: ifcond

character*100 filename
character*9 time_ext
real, dimension(ny_g) :: y_g
real, dimension(ny) :: vis_mean, eps_mean, up, U_bar, mean1
integer ierr
integer i, j, k

wt = 1.0
ifcond = .true.
if(myid==0) write(io,*) 'Writing XZavg stats'

if (myid == 0) then
  write(time_ext,'(1pe9.3)') time*time_ref
  filename = '../post/tecplot/'//trim(run_title)//'_XZ.'//trim(time_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'variables = '
  write(78,*) '"y (mm)"'
  write(78,*) '"U_ave (m/s)" "U_prime(m/s)"'
  write(78,*) '"vis" "eps" "l_k" "l_t"'
end if

call MPI_Gather(y,ny,MPI_REAL8,y_g,ny,MPI_REAL8,0,ycomm,ierr) 
if (myid == 0) then
  write(78, 1)  time*time_ref, ny_g
  write(78, 9) (y_g(j)*l_ref*1000.0, j=1,ny_g)
end if

! U_1 bar
call calc_xz_sym_mean(u(:,:,:,1), ifcond, wt, .true., U_bar)
call write_Yfld(78, U_bar, a_ref)

!U_prime
do k = 1, nz
  do i = 1, nx
    lil_u(i,:,k,1) = u(i,:,k,1) - U_bar(:)
  end do
end do
lil_u(:,:,:,2) = u(:,:,:,2)
lil_u(:,:,:,3) = u(:,:,:,3)

!RMS of u' fluctuation
work = lil_u(:,:,:,1)*lil_u(:,:,:,1)
work = work + lil_u(:,:,:,2)*lil_u(:,:,:,2)
work = work + lil_u(:,:,:,3)*lil_u(:,:,:,3)
work = work/3.0
call calc_xz_sym_mean(work, ifcond, wt, .true., up)
up = sqrt(up)
call write_Yfld(78, up, a_ref)

!Viscosity
!call computeCoefficients(cpmix, temp)
!call computeCoefficients( pressure,temp,yspecies,1.0/volum)
work = getviscosity()
! convert to kinematic viscosity
work = work*volum
call calc_xz_sym_mean(work, ifcond, wt, .true., vis_mean)
call write_Yfld(78, vis_mean, l_ref*a_ref)

!Dissipation rate
call calculate_dissipation(u, lil_u, volum, work)
call calc_xz_sym_mean(work, ifcond, wt, .true., eps_mean)
call write_Yfld(78, eps_mean, a_ref*a_ref/time_ref)

!Kolmogorov scale
mean1 = (vis_mean**3/max(eps_mean,1e-30))**0.25
call write_Yfld(78, mean1, l_ref*1e3)

!Turbulence length scale
mean1 = (up**3/max(eps_mean,1e-30))
call write_Yfld(78, mean1, l_ref*1e3)


if(myid == 0) then
  close(54)
end if

1 format(' zone t= "',1pe9.3,'", i=',i5,', f=block')
9 format(10(1pe12.5,1x))
return
end subroutine print_xzavgs

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine curv_cndtl(io, finish)
use topology_m, only: myid, gcomm
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

!real, parameter:: Cval = 0.5, Cdel = 0.02  ! NB, Cval_param is used instead

integer, parameter :: nbinc = 40
real, save, dimension(0:nbinc) :: binc_val
real, save, allocatable, dimension(:,:,:) :: pdf_c
real, save, allocatable, dimension(:,:,:) :: gradc_num, gradc_den
real, save, allocatable, dimension(:,:) :: curv_num, curv_den
real, dimension(nx,0:nbinz) :: curv_ave
real, dimension(nx, 0:nbinc+1,0:nbinz) :: cum
real, dimension(nx, 1:nbinc,0:nbinz) :: gradC_ave

logical, dimension(nx,ny,nz) :: C_cond, curv_cond, ifcond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, mag_gradC, curv
real, dimension(nx,ny,nz, 3) :: normal
integer i, n, l

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing curv'

  ! Initialize curv bins based on curv rate (global) min/max at first time
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call computeDivergence(normal, curv)
!  where(Cprog .gt. Cval-Cdel .and. Cprog .le. Cval+Cdel)
  where(Cprog .gt. Cval_param-Cdel_param .and. Cprog .le. Cval_param+Cdel_param)  
    ifcond = .true.
  elsewhere 
    ifcond = .false.
  endwhere
  call make_bins(nx*ny*nz,curv, ifcond, gcomm, nbinc, binc_val)
!  if(myid .eq. 0) write(io,*) 'curv bin range ',binc_val(0),binc_val(nbinc)
  call make_bins_lmts(-600.0, 600.0, nbinc, binc_val)

  allocate(pdf_c(nx, 0:nbinc+1,0:nbinz))
  allocate(gradc_num(nx, nbinc,0:nbinz))
  allocate(gradc_den(nx, nbinc,0:nbinz))
  allocate(curv_num(nx,0:nbinz))
  allocate(curv_den(nx,0:nbinz))

  pdf_c = 0.0;
  gradC_num = 0.0; gradC_den = 0.0;
  curv_num = 0.0; curv_den = 0.0;
  initialized = .true.
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call computeDivergence(normal, curv)

do l=0,nbinz
  
!  where(Cprog .gt. Cval-Cdel .and. Cprog .le. Cval+Cdel           &
  where(Cprog .gt. Cval_param-Cdel_param .and. Cprog .le. Cval_param+Cdel_param           &  
   .and.((zetafield.gt.binz_val(l-1).and.zetafield.le.binz_val(l)).or.(l.eq.0)) &
         )
    C_cond = .true.
  elsewhere 
    C_cond = .false.
  endwhere
  ifcond = C_cond
  wt = 1.0
  !----------------------------------------
!esr  !On each processor, find the pdf of curv
!esr  !for the yz-plane corresponding to i = nx. 
!esr  call calc_yz_pdf_addtl(curv, ifcond, wt, &
!esr         nbinc, binc_val, pdf_c(:,:,l))
    do i=1,nx
      call calc_yz_hist_addtl(curv(i,:,:), ifcond(i,:,:), wt, nbinc, binc_val(:), pdf_c(i,:,l))
    enddo
  !----------------------------------------
  ! Find the mean curv rate
  call calc_yz_accum_numden (curv, ifcond, wt, curv_num(:,l), curv_den(:,l))
  !----------------------------------------
  ! Calculate conditional mean grad_c.
  ! Again calculate only for i=nx.
  do n = 1, nbinc
    where(curv .gt. binc_val(n-1) .and. curv .le. binc_val(n))
      curv_cond = .true.
    elsewhere 
      curv_cond = .false.
    endwhere
    ifcond = C_cond .and. curv_cond
    call calc_yz_accum_numden(mag_gradC,ifcond,wt,gradc_num(:,n,l),gradc_den(:,n,l))
  end do
end do !nbinz
else !Finishing step
do l=0,nbinz
  !----------------------------------------
  ! Apply the moving window average
  do n = 0, nbinc+1
    call calc_Xfld_Xmovsum(pdf_c(:,n,l), window)
  end do 
  do n = 1, nbinc
    call calc_Xfld_Xmovsum(gradc_num(:,n,l), window)
    call calc_Xfld_Xmovsum(gradc_den(:,n,l), window)
  end do
  call calc_Xfld_Xmovsum(curv_num(:,l), window)
  call calc_Xfld_Xmovsum(curv_den(:,l), window)
  !----------------------------------------
  !Normalize the pdf
  do i = 1, nx
!esr      call calc_norm_from_hist(nbinc, binc_val, pdf_c(i,:,l), pdf_c(i,1:nbinc,l))
!esr      pdf_c(i,0,l)=0.0
!esr      pdf_c(i,nbinc+1,l)=0.0
    call normalize_pdf(nbinc, pdf_c(i,:,l))
    call calc_cumnorm_from_hist(nbinc, pdf_c(i,:,l), cum(i,:,l))
!esr    call calc_cumpdf_frompdf(nbinc, pdf_c(i,:,l), cum(i,:,l))
  end do
  !----------------------------------------
  !Calculate averages
  where(gradc_den .lt. stat_small)
    gradc_ave = 0.0
  elsewhere
    gradc_ave = gradc_num/gradc_den
  endwhere
  where(curv_den .lt. stat_small)
    curv_ave = 0.0
  elsewhere
    curv_ave = curv_num/curv_den
  endwhere
end do

  call write_data(0.25)
  call write_data(0.50)
  call write_data(0.75)
end if !Finish

return
contains !CONTAINS CONTAINS CONTAINS
  !----------------------------------------
  subroutine write_data(b)
  implicit none
  real, intent(in) :: b

  integer i
  integer n, l, m
  integer il
  character*4 loc_ext
  character*100 filename
  character*2 xiid_ext

  i = int(nx_g*b)
  if (i<=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return

  il = i - xid*nx
  write(loc_ext,'(F4.2)') b
  do m=0,nbinz
  write(xiid_ext,'(I2.2)')m
  filename = '../post/tecplot/curv_cndtl.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'title = curv_cndtl'
  write(78,*) 'variables = '
  write(78,*) 'curv[1/mm]'
  write(78,*) '"f"'
  write(78,*) '"cumf"'
  write(78,*) '"gradc_ave[1/mm]"'
  write(78,*) '"mean curv [1/mm]"'
  write(78, 3)  xid, nbinc
  write(78,9) (0.5*(binc_val(n-1)+binc_val(n))/l_ref/1e3, n = 1, nbinc)
  write(78,9) (pdf_c(il, n,m), n = 1, nbinc)
  write(78,9) (cum(il, n,m), n = 1, nbinc)
  write(78,9) (gradc_ave(il, n,m)/l_ref/1e3, n = 1, nbinc)
  !Silly to write this n times... but why open another file.
  write(78,9) (curv_ave(il,m)/l_ref/1e3, n = 1, nbinc)

  close(78)  ! NB, this is only done on one processor so no barrier needed here.
  enddo !nbinz

3 format(' zone t= "curv_cndtl-',i2,'" i=',i5,', f=block')
9 format(10(1pe12.5,1x))
  return
  end subroutine write_data
  !----------------------------------------
end subroutine curv_cndtl
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine strain_cndtl(io, finish)
use topology_m, only: myid, gcomm
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

!real, parameter:: Cval = 0.5, Cdel = 0.02

integer, parameter :: nbinc = 40
real, save, dimension(0:nbinc) :: binc_val
real, save, allocatable, dimension(:,:,:) :: pdf_c
real, save, allocatable, dimension(:,:,:) :: gradc_num, gradc_den
real, save, allocatable, dimension(:,:) :: strain_num, strain_den
real, dimension(nx,0:nbinz) :: strain_ave
real, dimension(nx, 0:nbinc+1,0:nbinz) :: cum
real, dimension(nx, 1:nbinc,0:nbinz) :: gradC_ave

logical, dimension(nx,ny,nz) :: C_cond, strain_cond, ifcond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, mag_gradC, strain
real, dimension(nx,ny,nz, 3) :: normal
integer i, n, m

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing strain'

  ! Initialize strain bins based on strain rate (global) min/max at first time
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call calc_strain(normal, strain)
!  call make_bins(strain, ifcond, gcomm, nbinc, binc_val)
!  if(myid .eq. 0) write(io,*) 'Strain bin range ',binc_val(0),binc_val(nbinc)
  call make_bins_lmts(-1.5, 2.5, nbinc, binc_val)

  allocate(pdf_c(nx, 0:nbinc+1,0:nbinz))
  allocate(gradc_num(nx, nbinc,0:nbinz))
  allocate(gradc_den(nx, nbinc,0:nbinz))
  allocate(strain_num(nx,0:nbinz))
  allocate(strain_den(nx,0:nbinz))

  pdf_c = 0.0;
  gradC_num = 0.0; gradC_den = 0.0;
  strain_num = 0.0; strain_den = 0.0;
  initialized = .true.
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call calc_strain(normal, strain)

do m=0,nbinz
  where(Cprog .gt. Cval_param-Cdel_param .and. Cprog .le. Cval_param+Cdel_param  &
      .and. ((zetafield.gt.binz_val(m-1).and.zetafield.le.binz_val(m)).or.(m.eq.0)) &
  )
    C_cond = .true.
  elsewhere 
    C_cond = .false.
  endwhere
  ifcond = C_cond
  wt = 1.0
  !----------------------------------------
!esr  !On each processor, find the pdf of strain
!esr  !for the yz-plane corresponding to i = nx. 
!esr  call calc_yz_pdf_addtl(strain, ifcond, wt, &
!esr         nbinc, binc_val, pdf_c(:,:,m))
    do i=1,nx
      call calc_yz_hist_addtl(strain(i,:,:), ifcond(i,:,:), wt, nbinc, binc_val(:), pdf_c(i,:,m))
    enddo
  !----------------------------------------
  ! Find the mean strain rate
  call calc_yz_accum_numden (strain, ifcond, wt, strain_num(:,m), strain_den(:,m))
  !----------------------------------------
  ! Calculate conditional mean grad_c.
  ! Again calculate only for i=nx.
  do n = 1, nbinc
    where(strain .gt. binc_val(n-1) .and. strain .le. binc_val(n))
      strain_cond = .true.
    elsewhere 
      strain_cond = .false.
    endwhere
    ifcond = C_cond .and. strain_cond
    call calc_yz_accum_numden(mag_gradC,ifcond,wt,gradc_num(:,n,m),gradc_den(:,n,m))
  end do
end do !nbinz
else !Finishing step
do m=0,nbinz
  !----------------------------------------
  ! Apply the moving window average
  do n = 0, nbinc+1
    call calc_Xfld_Xmovsum(pdf_c(:,n,m), window)
  end do 
  do n = 1, nbinc
    call calc_Xfld_Xmovsum(gradc_num(:,n,m), window)
    call calc_Xfld_Xmovsum(gradc_den(:,n,m), window)
  end do
  call calc_Xfld_Xmovsum(strain_num(:,m), window)
  call calc_Xfld_Xmovsum(strain_den(:,m), window)
  !----------------------------------------
  !Normalize the pdf
  do i = 1, nx
!esr      call calc_norm_from_hist(nbinc, binc_val, pdf_c(i,:,m), pdf_c(i,1:nbinc,m))
!esr      pdf_c(i,0,m)=0.0
!esr      pdf_c(i,nbinc+1,m)=0.0
    call normalize_pdf(nbinc, pdf_c(i,:,m))
    call calc_cumnorm_from_hist(nbinc, pdf_c(i,:,m), cum(i,:,m))
!esr    call calc_cumpdf_frompdf(nbinc, pdf_c(i,:,m), cum(i,:,m))
  end do
  !----------------------------------------
  !Calculate averages
  where(gradc_den .lt. stat_small)
    gradc_ave = 0.0
  elsewhere
    gradc_ave = gradc_num/gradc_den
  endwhere
  where(strain_den .lt. stat_small)
    strain_ave = 0.0
  elsewhere
    strain_ave = strain_num/strain_den
  endwhere
end do !nbinz

  call write_data(0.25)
  call write_data(0.50)
  call write_data(0.75)
end if !Finish

return
contains !CONTAINS CONTAINS CONTAINS
  !----------------------------------------
  subroutine write_data(b)
  implicit none
  real, intent(in) :: b

  integer i
  integer n, l
  integer il
  character*4 loc_ext
  character*100 filename
  character*2 xiid_ext

  i = int(nx_g*b)
  if (i<=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return

  il = i - xid*nx
  write(loc_ext,'(F4.2)') b
do m=0,nbinz
  write(xiid_ext,'(I2.2)') m
  filename = '../post/tecplot/strain_cndtl.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'title = strain_cndtl'
  write(78,*) 'variables = '
  write(78,*) 'strain[1/s]'
  write(78,*) '"f"'
  write(78,*) '"cumf"'
  write(78,*) '"gradc_ave[1/mm]"'
  write(78,*) '"mean strain [1/s]"'
  write(78, 3)  xid, nbinc
  write(78,9) (0.5*(binc_val(n-1)+binc_val(n))/time_ref, n = 1, nbinc)
  write(78,9) (pdf_c(il, n, m), n = 1, nbinc)
  write(78,9) (cum(il, n, m), n = 1, nbinc)
  write(78,9) (gradc_ave(il, n, m)/l_ref/1e3, n = 1, nbinc)
  !Silly to write this n times... but why open another file.
  write(78,9) (strain_ave(il, m)/time_ref, n = 1, nbinc)
  close(78)  ! NB, this only gets done on one processor so no barrier is required here
end do !nbinz

3 format(' zone t= "strain_cndtl-',i2,'" i=',i5,', f=block')
9 format(10(1pe12.5,1x))
  return
  end subroutine write_data
  !----------------------------------------
end subroutine strain_cndtl

!----------------------------------------------------------------------
!----------------------------------------------------------------------

!----------------------------------------------------------------------
subroutine chi_cndtl_sd(io, finish)
use topology_m, only: myid, gcomm
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

!real, parameter:: Cval = 0.5, Cdel = 0.05

integer, parameter :: nbinc = 40, nbint = 40
real, save, dimension(0:nbinc) :: binc_val
real, save, dimension(0:nbint) :: bint_val

real, save, dimension(:,:,:), allocatable :: gradc_num, gradc_den
real, save, dimension(:,:,:), allocatable :: tgradc_num, tgradc_den
real, save, dimension(:,:,:), allocatable :: sd_num, sd_den
real, save, dimension(:,:,:), allocatable :: sd_r_num, sd_r_den
real, save, dimension(:,:,:), allocatable :: sd_n_num, sd_n_den
real, save, dimension(:,:,:), allocatable :: sd_c_num, sd_c_den
real, save, dimension(:,:,:), allocatable :: sd_mf_num, sd_mf_den
real, save, dimension(:,:,:), allocatable :: sd_cr_num, sd_cr_den
real, save, dimension(:,:,:), allocatable :: sd_dd_num, sd_dd_den
real, save, dimension(:,:,:), allocatable :: strain_num, strain_den

real, save, dimension(:,:,:), allocatable :: tsd_num, tsd_den
real, save, dimension(:,:,:), allocatable :: tsd_r_num, tsd_r_den
real, save, dimension(:,:,:), allocatable :: tsd_n_num, tsd_n_den
real, save, dimension(:,:,:), allocatable :: tsd_c_num, tsd_c_den
real, save, dimension(:,:,:), allocatable :: tsd_mf_num, tsd_mf_den
real, save, dimension(:,:,:), allocatable :: tsd_cr_num, tsd_cr_den
real, save, dimension(:,:,:), allocatable :: tsd_dd_num, tsd_dd_den
real, save, dimension(:,:,:), allocatable :: tstrain_num, tstrain_den

logical, dimension(nx,ny,nz) :: C_cond, dzdn_cond, dzdt_cond, ifcond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, mag_gradC, mag_dZ, dzdn, dzdt   ! chi is the 
real, dimension(nx,ny,nz) :: sd, sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd
real, dimension(nx,ny,nz) :: an, at
real, dimension(nx,ny,nz, 3) :: normal, normal_dz
real, dimension(nx) :: num, den

real, dimension(1:nbinc,0:nbinz) :: ave
integer n, m, i

character*100 filename
character*2 xid_ext

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing chi_cndtl_sd'

  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  where(Cprog .gt. Cval_param-Cdel_param .and. Cprog .le. Cval_param+Cdel_param)
    ifcond = .true.
  elsewhere 
    ifcond = .false.
  endwhere
  if(myid.eq.0)write(io,*)'making c bins'
  call make_bins_lmts(-80.0, 80.0, nbinc, binc_val)
  if(myid.eq.0)write(io,*)'making t bins'
  call make_bins_lmts(0.0, 80.0, nbint, bint_val)
  if(myid.eq.0)write(io,*)'made t bins'

  allocate(gradc_num(nx,1:nbinc,0:nbinz)); allocate(gradc_den(nx,1:nbinc,0:nbinz))
  allocate(sd_num(nx,1:nbinc,0:nbinz)); allocate(sd_den(nx,1:nbinc,0:nbinz))
  allocate(sd_r_num(nx,1:nbinc,0:nbinz)); allocate(sd_r_den(nx,1:nbinc,0:nbinz))
  allocate(sd_n_num(nx,1:nbinc,0:nbinz)); allocate(sd_n_den(nx,1:nbinc,0:nbinz))
  allocate(sd_c_num(nx,1:nbinc,0:nbinz)); allocate(sd_c_den(nx,1:nbinc,0:nbinz))
  allocate(sd_mf_num(nx,1:nbinc,0:nbinz)); allocate(sd_mf_den(nx,1:nbinc,0:nbinz))
  allocate(sd_cr_num(nx,1:nbinc,0:nbinz)); allocate(sd_cr_den(nx,1:nbinc,0:nbinz))
  allocate(sd_dd_num(nx,1:nbinc,0:nbinz)); allocate(sd_dd_den(nx,1:nbinc,0:nbinz))
  allocate(strain_num(nx,1:nbinc,0:nbinz)); allocate(strain_den(nx,1:nbinc,0:nbinz))

  allocate(tgradc_num(nx,1:nbint,0:nbinz)); allocate(tgradc_den(nx,1:nbint,0:nbinz))
  allocate(tsd_num(nx,1:nbint,0:nbinz)); allocate(tsd_den(nx,1:nbint,0:nbinz))
  allocate(tsd_r_num(nx,1:nbint,0:nbinz)); allocate(tsd_r_den(nx,1:nbint,0:nbinz))
  allocate(tsd_n_num(nx,1:nbint,0:nbinz)); allocate(tsd_n_den(nx,1:nbint,0:nbinz))
  allocate(tsd_c_num(nx,1:nbint,0:nbinz)); allocate(tsd_c_den(nx,1:nbint,0:nbinz))
  allocate(tsd_mf_num(nx,1:nbint,0:nbinz)); allocate(tsd_mf_den(nx,1:nbint,0:nbinz))
  allocate(tsd_cr_num(nx,1:nbint,0:nbinz)); allocate(tsd_cr_den(nx,1:nbint,0:nbinz))
  allocate(tsd_dd_num(nx,1:nbint,0:nbinz)); allocate(tsd_dd_den(nx,1:nbint,0:nbinz))
  allocate(tstrain_num(nx,1:nbint,0:nbinz)); allocate(tstrain_den(nx,1:nbint,0:nbinz))

  gradc_num = 0.0; gradc_den = 0.0;
  tgradc_num = 0.0; tgradc_den = 0.0;
  sd_num = 0.0; sd_den = 0.0;
  sd_r_num = 0.0; sd_r_den = 0.0;
  sd_n_num = 0.0; sd_n_den = 0.0;
  sd_c_num = 0.0; sd_c_den = 0.0;
  sd_mf_num = 0.0; sd_mf_den = 0.0;
  sd_cr_num = 0.0; sd_cr_den = 0.0;
  sd_dd_num = 0.0; sd_dd_den = 0.0;
  strain_num = 0.0; strain_den = 0.0;

  tsd_num = 0.0; tsd_den = 0.0; 
  tsd_r_num = 0.0; tsd_r_den = 0.0;
  tsd_n_num = 0.0; tsd_n_den = 0.0;
  tsd_c_num = 0.0; tsd_c_den = 0.0;
  tsd_mf_num = 0.0; tsd_mf_den = 0.0;
  tsd_cr_num = 0.0; tsd_cr_den = 0.0;
  tsd_dd_num = 0.0; tsd_dd_den = 0.0;
  tstrain_num = 0.0; tstrain_den = 0.0;

  initialized = .true.
  if(myid.eq.0)write(io,*)'leaving initialization of chi'
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)  

! FIND THE GRADIENT OF Z NORMAL TO THE FLAME
! call zetafield
  call calc_mag_grad_and_normal &
     (zetafield, (/ 0.0, 0.0, 1.0/), .false., mag_dz, normal_dz)
! take the dot product of the zetafields and cprog field     

dzdn=0.0
do i=1,3
dzdn=dzdn+normal(:,:,:,i)*normal_dz(:,:,:,i)
enddo
dzdn=dzdn*mag_dz
dzdt=sqrt(mag_dz*mag_dz-dzdn*dzdn)

!write(*,*)myid,'dzdn',min(dzdn),max(dzdn),'dzdt',min(dzdt),max(dzdt)

  call calc_strain(normal,at)
  call calc_normal_strain(normal,an)

  
!  call calc_sd_comp &
!  (385.23/rr_ref, 46.81/rr_ref, 0.4261/rho_ref, .true., sd_r, sd_n, sd_c, sd)
  call calc_sd_comp &
    (0.0, 0.0, 0.4261/rho_ref, .false., &
                 sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd, sd,io)

! reweight the sd contributions here
   sd=sd       *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_r=sd_r   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_n=sd_n   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_c=sd_c   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_mf=sd_mf *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_cr=sd_cr *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_dd=sd_dd *q(:,:,:,4,1)/(0.4251/rho_ref)

do m=0,nbinz 
  where(Cprog .gt. Cval_param-Cdel_param .and. Cprog .le. Cval_param+Cdel_param &
   .and.((zetafield.gt.binz_val(m-1).and.zetafield.le.binz_val(m)).or.(m.eq.0)) &
  )
    C_cond = .true.
  elsewhere 
    C_cond = .false.
  endwhere
  ifcond = C_cond
  wt = 1.0
  !----------------------------------------
  ! Calculate conditional mean grad_c.
  ! Again calculate only for i=nx.
  do n = 1, nbinc
    where(dzdn .gt. binc_val(n-1) .and. dzdn .le. binc_val(n)  &
    .and.dzdt.le.bint_val(min0(nbint/5,3))  &
    )
      dzdn_cond = .true.
    elsewhere 
      dzdn_cond = .false.
    endwhere
! introduce condition to limit the dzdt range also

    
    ifcond = C_cond .and. dzdn_cond
    !----------------------------------------
    call calc_yz_accum_numden (mag_gradC, ifcond, wt, gradc_num(:,n,m), gradc_den(:,n,m))    
    call calc_yz_accum_numden (sd, ifcond, wt, sd_num(:,n,m), sd_den(:,n,m))
    call calc_yz_accum_numden (sd_r, ifcond, wt, sd_r_num(:,n,m), sd_r_den(:,n,m))
    call calc_yz_accum_numden (sd_n, ifcond, wt, sd_n_num(:,n,m), sd_n_den(:,n,m))
    call calc_yz_accum_numden (sd_c, ifcond, wt, sd_c_num(:,n,m), sd_c_den(:,n,m))
    call calc_yz_accum_numden (sd_mf, ifcond, wt, sd_mf_num(:,n,m), sd_mf_den(:,n,m))
    call calc_yz_accum_numden (sd_cr, ifcond, wt, sd_cr_num(:,n,m), sd_cr_den(:,n,m))
    call calc_yz_accum_numden (sd_dd, ifcond, wt, sd_dd_num(:,n,m), sd_dd_den(:,n,m))
    call calc_yz_accum_numden (an, ifcond, wt, strain_num(:,n,m), strain_den(:,n,m))
  end do

  do n = 1, nbinc
    where(dzdt .gt. bint_val(n-1) .and. dzdt .le. bint_val(n) &
    .and.abs(dzdn).le.bint_val(min0(nbint/5,3))  &
    )
    dzdt_cond = .true.
  elsewhere
    dzdt_cond = .false.
  endwhere
  ifcond = C_cond .and. dzdt_cond
  !----------------------------------------
    call calc_yz_accum_numden (mag_gradC, ifcond, wt, tgradc_num(:,n,m), tgradc_den(:,n,m))
    call calc_yz_accum_numden (sd, ifcond, wt, tsd_num(:,n,m), tsd_den(:,n,m))
    call calc_yz_accum_numden (sd_r, ifcond, wt, tsd_r_num(:,n,m), tsd_r_den(:,n,m))
    call calc_yz_accum_numden (sd_n, ifcond, wt, tsd_n_num(:,n,m), tsd_n_den(:,n,m))
    call calc_yz_accum_numden (sd_c, ifcond, wt, tsd_c_num(:,n,m), tsd_c_den(:,n,m))
    call calc_yz_accum_numden (sd_mf, ifcond, wt, tsd_mf_num(:,n,m), tsd_mf_den(:,n,m))
    call calc_yz_accum_numden (sd_cr, ifcond, wt, tsd_cr_num(:,n,m), tsd_cr_den(:,n,m))
    call calc_yz_accum_numden (sd_dd, ifcond, wt, tsd_dd_num(:,n,m), tsd_dd_den(:,n,m))
    call calc_yz_accum_numden (at, ifcond, wt, tstrain_num(:,n,m), tstrain_den(:,n,m))
  end do

end do
else !Finishing step
do m=0,nbinz
 !Apply moving window averages
  do n = 1, nbinc
    call calc_Xfld_Xmovsum(gradc_num(:,n,m), window)
    call calc_Xfld_Xmovsum(gradc_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_r_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_r_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_n_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_n_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_c_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_c_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_mf_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_mf_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_cr_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_cr_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_dd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_dd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(strain_num(:,n,m), window)
    call calc_Xfld_Xmovsum(strain_den(:,n,m), window)
  end do
  do n = 1, nbint
    call calc_Xfld_Xmovsum(tgradc_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tgradc_den(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_r_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_r_den(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_n_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_n_den(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_c_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_c_den(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_mf_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_mf_den(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_cr_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_cr_den(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_dd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tsd_dd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(tstrain_num(:,n,m), window)
    call calc_Xfld_Xmovsum(tstrain_den(:,n,m), window)
  end do
end do
  call write_data(0.25)
  call write_data(0.50)
  call write_data(0.75)
end if
return

contains !CONTAINS CONTAINS CONTAINS
  !----------------------------------------
  subroutine write_data(b)
  implicit none
  real, intent(in) :: b

  integer i
  integer n, l
  integer il
  character*4 loc_ext
  character*100 filename
  character*2 xiid_ext

  i = int(nx_g*b)
  if (i<=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return

  il = i - xid*nx
  write(loc_ext,'(F4.2)') b
do m=0,nbinz
  write(xiid_ext,'(I2.2)')m
  filename = '../post/tecplot/dzdn_cndtl_dsd.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

   write(78,*) 'title = dzdn_cndtl_dsd'
   write(78,*) 'variables = '
   write(78,*) '"dZdn[1/mm]"'
   write(78,*) '"gradc[1/mm]"'
   write(78,*) '"sd[m/s]"'
   write(78,*) '"sdr[m/s]"'
   write(78,*) '"sdn[m/s]"'
   write(78,*) '"sdc[m/s]"'
   write(78,*) '"sdmf[m/s]"'
   write(78,*) '"sdcr[m/s]"'
   write(78,*) '"sddd[m/s]"'
   write(78,*) '"strain[1/s]"'
   write(78, 3)  b, nbinc
   write(78,9) (0.5*(binc_val(n-1)+binc_val(n))/l_ref/1e3, n = 1, nbinc)
   !----------------------------------------
   call print_avg(gradc_num(il,:,m), gradc_den(il,:,m), 1.0/l_ref/1.0e3)
   call print_avg(sd_num(il,:,m), sd_den(il,:,m), a_ref)
   call print_avg(sd_r_num(il,:,m), sd_r_den(il,:,m), a_ref)
   call print_avg(sd_n_num(il,:,m), sd_n_den(il,:,m), a_ref)
   call print_avg(sd_c_num(il,:,m), sd_c_den(il,:,m), a_ref)
   call print_avg(sd_mf_num(il,:,m), sd_mf_den(il,:,m), a_ref)
   call print_avg(sd_cr_num(il,:,m), sd_cr_den(il,:,m), a_ref)
   call print_avg(sd_dd_num(il,:,m), sd_dd_den(il,:,m), a_ref)
   call print_avg(strain_num(il,:,m), strain_den(il,:,m), -1.0/time_ref)
   close(78)

! write the data conditional on the tangential mf gradient

  filename = '../post/tecplot/dzdt_cndtl_dsd.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

   write(78,*) 'title = dzdn_cndtl_dsd'
   write(78,*) 'variables = '
   write(78,*) '"dZdt[1/mm]"'
   write(78,*) '"gradc[1/mm]"'
   write(78,*) '"sd[m/s]"'
   write(78,*) '"sdr[m/s]"'
   write(78,*) '"sdn[m/s]"'
   write(78,*) '"sdc[m/s]"'
   write(78,*) '"sdmf[m/s]"'
   write(78,*) '"sdcr[m/s]"'
   write(78,*) '"sddd[m/s]"'
   write(78,*) '"strain[1/s]"'
   write(78, 4)  b, nbint
   write(78,9) (0.5*(bint_val(n-1)+bint_val(n))/l_ref/1e3, n = 1, nbint)
   !----------------------------------------
   call print_avg(tgradc_num(il,:,m), tgradc_den(il,:,m), 1.0/l_ref/1e3)
   call print_avg(tsd_num(il,:,m), tsd_den(il,:,m), a_ref)
   call print_avg(tsd_r_num(il,:,m), tsd_r_den(il,:,m), a_ref)
   call print_avg(tsd_n_num(il,:,m), tsd_n_den(il,:,m), a_ref)
   call print_avg(tsd_c_num(il,:,m), tsd_c_den(il,:,m), a_ref)
   call print_avg(tsd_mf_num(il,:,m), tsd_mf_den(il,:,m), a_ref)
   call print_avg(tsd_cr_num(il,:,m), tsd_cr_den(il,:,m), a_ref)
   call print_avg(tsd_dd_num(il,:,m), tsd_dd_den(il,:,m), a_ref)
   call print_avg(tstrain_num(il,:,m), tstrain_den(il,:,m), -1.0/time_ref)
   close(78)
 
end do  !nbinz

3 format(' zone t= "dzdn_cndtl-dsd-',F4.2,'" i=',i5,', f=block')
4 format(' zone t= "dzdt_cndtl-dsd-',F4.2,'" i=',i5,', f=block')
9 format(10(1pe12.5,1x))
  return
  end subroutine write_data

  !----------------------------------------
  !----------------------------------------
  !----------------------------------------
  subroutine print_avg(num, den, ref)
  implicit none
  real, intent(in), dimension(nbinc) :: num, den
  real, intent(in) :: ref
  real, dimension(nbinc) :: ave
  
  where(den .lt. stat_small)
    ave = 0.0
  elsewhere
    ave = num/den
  endwhere
  write(78,9) (ave(n)*ref, n = 1, nbinc)
  return
  9 format(10(1pe12.5,1x))
  end subroutine print_avg
!----------------------------------------
end subroutine chi_cndtl_sd

!----------------------------------------------------------------------
subroutine curv_cndtl_sd(io, finish)
use topology_m, only: myid, gcomm
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

!real, parameter:: Cval = 0.5, Cdel = 0.05

integer, parameter :: nbinc = 50
real, save, dimension(0:nbinc) :: binc_val

real, save, dimension(:,:,:), allocatable :: an_num, an_den
real, save, dimension(:,:,:), allocatable :: at_num, at_den
real, save, dimension(:,:,:), allocatable :: dsd_num, dsd_den
real, save, dimension(:,:,:), allocatable :: dsd_r_num, dsd_r_den
real, save, dimension(:,:,:), allocatable :: dsd_n_num, dsd_n_den
real, save, dimension(:,:,:), allocatable :: dsd_c_num, dsd_c_den
real, save, dimension(:,:,:), allocatable :: dsd_mf_num, dsd_mf_den
real, save, dimension(:,:,:), allocatable :: dsd_cr_num, dsd_cr_den
real, save, dimension(:,:,:), allocatable :: dsd_dd_num, dsd_dd_den

real, save, dimension(:,:,:), allocatable :: sd_num, sd_den
real, save, dimension(:,:,:), allocatable :: sd_r_num, sd_r_den
real, save, dimension(:,:,:), allocatable :: sd_n_num, sd_n_den
real, save, dimension(:,:,:), allocatable :: sd_c_num, sd_c_den
real, save, dimension(:,:,:), allocatable :: sd_mf_num, sd_mf_den
real, save, dimension(:,:,:), allocatable :: sd_cr_num, sd_cr_den
real, save, dimension(:,:,:), allocatable :: sd_dd_num, sd_dd_den

logical, dimension(nx,ny,nz) :: C_cond, curv_cond, ifcond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, mag_gradC, curv
real, dimension(nx,ny,nz) :: at, an, sd, sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd
real, dimension(nx,ny,nz) :: dsd, dsd_r, dsd_n, dsd_c, dsd_mf, dsd_cr, dsd_dd
real, dimension(nx,ny,nz, 3) :: normal
real, dimension(nx) :: num, den

real, dimension(1:nbinc,0:nbinz) :: ave
integer n, m

character*100 filename
character*2 xid_ext

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing curv_cndtl_sd'

  ! Initialized curv bins based on curv (global) min/max at first time
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call computeDivergence(normal, curv)
  where(Cprog .gt. Cval_param-Cdel_param .and. Cprog .le. Cval_param+Cdel_param)
    ifcond = .true.
  elsewhere 
    ifcond = .false.
  endwhere
!  call make_bins(curv, ifcond, gcomm, nbinc, binc_val)
  call make_bins_lmts(-330.0, 330.0, nbinc, binc_val)

  allocate(an_num(nx,1:nbinc,0:nbinz)); allocate(an_den(nx,1:nbinc,0:nbinz))
  allocate(at_num(nx,1:nbinc,0:nbinz)); allocate(at_den(nx,1:nbinc,0:nbinz))
  allocate(dsd_num(nx,1:nbinc,0:nbinz)); allocate(dsd_den(nx,1:nbinc,0:nbinz))
  allocate(dsd_r_num(nx,1:nbinc,0:nbinz)); allocate(dsd_r_den(nx,1:nbinc,0:nbinz))
  allocate(dsd_n_num(nx,1:nbinc,0:nbinz)); allocate(dsd_n_den(nx,1:nbinc,0:nbinz))
  allocate(dsd_c_num(nx,1:nbinc,0:nbinz)); allocate(dsd_c_den(nx,1:nbinc,0:nbinz))
  allocate(dsd_mf_num(nx,1:nbinc,0:nbinz)); allocate(dsd_mf_den(nx,1:nbinc,0:nbinz))
  allocate(dsd_cr_num(nx,1:nbinc,0:nbinz)); allocate(dsd_cr_den(nx,1:nbinc,0:nbinz))
  allocate(dsd_dd_num(nx,1:nbinc,0:nbinz)); allocate(dsd_dd_den(nx,1:nbinc,0:nbinz))
  allocate(sd_num(nx,1:nbinc,0:nbinz)); allocate(sd_den(nx,1:nbinc,0:nbinz))
  allocate(sd_r_num(nx,1:nbinc,0:nbinz)); allocate(sd_r_den(nx,1:nbinc,0:nbinz))
  allocate(sd_n_num(nx,1:nbinc,0:nbinz)); allocate(sd_n_den(nx,1:nbinc,0:nbinz))
  allocate(sd_c_num(nx,1:nbinc,0:nbinz)); allocate(sd_c_den(nx,1:nbinc,0:nbinz))
  allocate(sd_mf_num(nx,1:nbinc,0:nbinz)); allocate(sd_mf_den(nx,1:nbinc,0:nbinz))
  allocate(sd_cr_num(nx,1:nbinc,0:nbinz)); allocate(sd_cr_den(nx,1:nbinc,0:nbinz))
  allocate(sd_dd_num(nx,1:nbinc,0:nbinz)); allocate(sd_dd_den(nx,1:nbinc,0:nbinz))

  an_num = 0.0; an_den = 0.0;
  at_num = 0.0; at_den = 0.0;
  dsd_num = 0.0; dsd_den = 0.0;
  dsd_r_num = 0.0; dsd_r_den = 0.0;
  dsd_n_num = 0.0; dsd_n_den = 0.0;
  dsd_c_num = 0.0; dsd_c_den = 0.0;
  dsd_mf_num = 0.0; dsd_mf_den = 0.0;
  dsd_cr_num = 0.0; dsd_cr_den = 0.0;
  dsd_dd_num = 0.0; dsd_dd_den = 0.0;
  sd_num = 0.0; sd_den = 0.0;
  sd_r_num = 0.0; sd_r_den = 0.0;
  sd_n_num = 0.0; sd_n_den = 0.0;
  sd_c_num = 0.0; sd_c_den = 0.0;
  sd_mf_num = 0.0; sd_mf_den = 0.0;
  sd_cr_num = 0.0; sd_cr_den = 0.0;
  sd_dd_num = 0.0; sd_dd_den = 0.0;

  initialized = .true.
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call computeDivergence(normal, curv)
  call calc_strain(normal,at)
  call calc_normal_strain(normal,an)
!  call calc_sd_comp &
!  (385.23/rr_ref, 46.81/rr_ref, 0.4261/rho_ref, .true., sd_r, sd_n, sd_c, sd)
  call calc_sd_comp &
    (0.0, 0.0, 0.4261/rho_ref, .false., &
                 sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd, sd,io)

  call calc_normal_deriv(sd, normal, dsd)
  call calc_normal_deriv(sd_r, normal, dsd_r)
  call calc_normal_deriv(sd_n, normal, dsd_n)
  call calc_normal_deriv(sd_c, normal, dsd_c)
  call calc_normal_deriv(sd_mf, normal, dsd_mf)
  call calc_normal_deriv(sd_cr, normal, dsd_cr)
  call calc_normal_deriv(sd_dd, normal, dsd_dd)

! reweight the sd contributions here
   sd=sd       *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_r=sd_r   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_n=sd_n   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_c=sd_c   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_mf=sd_mf *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_cr=sd_cr *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_dd=sd_dd *q(:,:,:,4,1)/(0.4251/rho_ref)

do m=0,nbinz  
  where(Cprog .gt. Cval_param-Cdel_param .and. Cprog .le. Cval_param+Cdel_param &
   .and.((zetafield.gt.binz_val(m-1).and.zetafield.le.binz_val(m)).or.(m.eq.0)) &
  )
    C_cond = .true.
  elsewhere 
    C_cond = .false.
  endwhere
  ifcond = C_cond
  wt = 1.0
  !----------------------------------------
  ! Calculate conditional mean grad_c.
  ! Again calculate only for i=nx.
  do n = 1, nbinc
    where(curv .gt. binc_val(n-1) .and. curv .le. binc_val(n))
      curv_cond = .true.
    elsewhere 
      curv_cond = .false.
    endwhere
    ifcond = C_cond .and. curv_cond
    !----------------------------------------
    call calc_yz_accum_numden (at, ifcond, wt, at_num(:,n,m), at_den(:,n,m))
    call calc_yz_accum_numden (an, ifcond, wt, an_num(:,n,m), an_den(:,n,m))
    call calc_yz_accum_numden (dsd, ifcond, wt, dsd_num(:,n,m), dsd_den(:,n,m))
    call calc_yz_accum_numden (dsd_r, ifcond, wt, dsd_r_num(:,n,m), dsd_r_den(:,n,m))
    call calc_yz_accum_numden (dsd_n, ifcond, wt, dsd_n_num(:,n,m), dsd_n_den(:,n,m))
    call calc_yz_accum_numden (dsd_c, ifcond, wt, dsd_c_num(:,n,m), dsd_c_den(:,n,m))
    call calc_yz_accum_numden (dsd_mf, ifcond, wt, dsd_mf_num(:,n,m), dsd_mf_den(:,n,m))
    call calc_yz_accum_numden (dsd_cr, ifcond, wt, dsd_cr_num(:,n,m), dsd_cr_den(:,n,m))
    call calc_yz_accum_numden (dsd_dd, ifcond, wt, dsd_dd_num(:,n,m), dsd_dd_den(:,n,m))
    call calc_yz_accum_numden (sd, ifcond, wt, sd_num(:,n,m), sd_den(:,n,m))
    call calc_yz_accum_numden (sd_r, ifcond, wt, sd_r_num(:,n,m), sd_r_den(:,n,m))
    call calc_yz_accum_numden (sd_n, ifcond, wt, sd_n_num(:,n,m), sd_n_den(:,n,m))
    call calc_yz_accum_numden (sd_c, ifcond, wt, sd_c_num(:,n,m), sd_c_den(:,n,m))
    call calc_yz_accum_numden (sd_mf, ifcond, wt, sd_mf_num(:,n,m), sd_mf_den(:,n,m))
    call calc_yz_accum_numden (sd_cr, ifcond, wt, sd_cr_num(:,n,m), sd_cr_den(:,n,m))
    call calc_yz_accum_numden (sd_dd, ifcond, wt, sd_dd_num(:,n,m), sd_dd_den(:,n,m))
  end do
end do
else !Finishing step
do m=0,nbinz
 !Apply moving window averages
  do n = 1, nbinc
    call calc_Xfld_Xmovsum(at_num(:,n,m), window)
    call calc_Xfld_Xmovsum(at_den(:,n,m), window)
    call calc_Xfld_Xmovsum(an_num(:,n,m), window)
    call calc_Xfld_Xmovsum(an_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_r_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_r_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_n_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_n_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_c_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_c_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_mf_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_mf_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_cr_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_cr_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_dd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_dd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_r_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_r_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_n_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_n_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_c_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_c_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_mf_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_mf_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_cr_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_cr_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_dd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_dd_den(:,n,m), window)
  end do
end do  
  call write_data(0.25)
  call write_data(0.50)
  call write_data(0.75)
end if
return

contains !CONTAINS CONTAINS CONTAINS
  !----------------------------------------
  subroutine write_data(b)
  implicit none
  real, intent(in) :: b

  integer i
  integer n, l
  integer il
  character*4 loc_ext
  character*100 filename
  character*2 xiid_ext

  real z1,z2,zst
  real, dimension(0:nbinz) :: binphi_val

  i = int(nx_g*b)
  if (i<=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return

  il = i - xid*nx
  write(loc_ext,'(F4.2)') b
do m=0,nbinz
  write(xiid_ext,'(I2.2)')m
  filename = '../post/tecplot/curv_cndtl_dsd.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

   write(78,*) 'title = curv_cndtl_dsd'
   write(78,*) 'variables = '
   write(78,*) '"curv[1/mm]"'
   write(78,*) '"at[1/s]"'
   write(78,*) '"an[1/s]"'
   write(78,*) '"dsd[1/s]"'
   write(78,*) '"dsdr[1/s]"'
   write(78,*) '"dsdn[1/s]"'
   write(78,*) '"dsdc[1/s]"'
   write(78,*) '"dsdmf[1/s]"'
   write(78,*) '"dsdcr[1/s]"'
   write(78,*) '"dsddd[1/s]"'
   write(78,*) '"sd[1/s]"'
   write(78,*) '"sdr[1/s]"'
   write(78,*) '"sdn[1/s]"'
   write(78,*) '"sdc[1/s]"'
   write(78,*) '"sdmf[1/s]"'
   write(78,*) '"sdcr[1/s]"'
   write(78,*) '"sddd[1/s]"'
   write(78, 3)  b, nbinc
   write(78,9) (0.5*(binc_val(n-1)+binc_val(n))/l_ref/1e3, n = 1, nbinc)
   !----------------------------------------
   call print_avg(at_num(il,:,m), at_den(il,:,m), 1.0/time_ref)
   call print_avg(an_num(il,:,m), an_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_num(il,:,m), dsd_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_r_num(il,:,m), dsd_r_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_n_num(il,:,m), dsd_n_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_c_num(il,:,m), dsd_c_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_mf_num(il,:,m), dsd_mf_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_cr_num(il,:,m), dsd_cr_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_dd_num(il,:,m), dsd_dd_den(il,:,m), 1.0/time_ref)
   call print_avg(sd_num(il,:,m), sd_den(il,:,m), a_ref)
   call print_avg(sd_r_num(il,:,m), sd_r_den(il,:,m), a_ref)
   call print_avg(sd_n_num(il,:,m), sd_n_den(il,:,m), a_ref)
   call print_avg(sd_c_num(il,:,m), sd_c_den(il,:,m), a_ref)
   call print_avg(sd_mf_num(il,:,m), sd_mf_den(il,:,m), a_ref)
   call print_avg(sd_cr_num(il,:,m), sd_cr_den(il,:,m), a_ref)
   call print_avg(sd_dd_num(il,:,m), sd_dd_den(il,:,m), a_ref)
   close(78)
end do  !nbinz

   if (nbinz.gt.0)then ! the following plots things versus z, only do this if nbinz>0)
!do m=1,nbinz

  if(n_spec.eq.15)then
    z1=0.0233
    z2=0.055066
  else!if(n_spec.eq.29)then
    z1=0.0
    z2=0.0784
!  else
!    z1=0.0
!    z2=1.0
  endif
  zst=0.055066

  do n=1,nbinz
    binphi_val(n)=z1+binz_val(n)*(z2-z1)
    binphi_val(n)=binphi_val(n)*(1.0-zst)/zst/(1.0-binphi_val(n))
  enddo

!z1,z2,zst,binphi_val  

  m=nbinc/2
  write(xiid_ext,'(I2.2)')m
  filename = '../post/tecplot/z_curv_cndtl_dsd.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

   write(78,*) 'title = z_curv_cndtl_dsd'
   write(78,*) 'variables = '
   write(78,*) '"mixfrac"'
   write(78,*) '"phi"'
   write(78,*) '"at[1/s]"'
   write(78,*) '"an[1/s]"'
   write(78,*) '"dsd[1/s]"'
   write(78,*) '"dsdr[1/s]"'
   write(78,*) '"dsdn[1/s]"'
   write(78,*) '"dsdc[1/s]"'
   write(78,*) '"dsdmf[1/s]"'
   write(78,*) '"dsdcr[1/s]"'
   write(78,*) '"dsddd[1/s]"'
   write(78,*) '"sd[1/s]"'
   write(78,*) '"sdr[1/s]"'
   write(78,*) '"sdn[1/s]"'
   write(78,*) '"sdc[1/s]"'
   write(78,*) '"sdmf[1/s]"'
   write(78,*) '"sdcr[1/s]"'
   write(78,*) '"sddd[1/s]"'
   write(78, 3)  b, nbinz
   write(78,9) (0.5*(binz_val(n-1)+binz_val(n)), n = 1, nbinz)
   write(78,9) (0.5*(binphi_val(n-1)+binphi_val(n)), n = 1, nbinz)
   !----------------------------------------
   call print_avg_z(at_num(il,m,:), at_den(il,m,:), 1.0/time_ref)
   call print_avg_z(an_num(il,m,:), an_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_num(il,m,:), dsd_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_r_num(il,m,:), dsd_r_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_n_num(il,m,:), dsd_n_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_c_num(il,m,:), dsd_c_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_mf_num(il,m,:), dsd_mf_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_cr_num(il,m,:), dsd_cr_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_dd_num(il,m,:), dsd_dd_den(il,m,:), 1.0/time_ref)
   call print_avg_z(sd_num(il,m,:), sd_den(il,m,:), a_ref)
   call print_avg_z(sd_r_num(il,m,:), sd_r_den(il,m,:), a_ref)
   call print_avg_z(sd_n_num(il,m,:), sd_n_den(il,m,:), a_ref)
   call print_avg_z(sd_c_num(il,m,:), sd_c_den(il,m,:), a_ref)
   call print_avg_z(sd_mf_num(il,m,:), sd_mf_den(il,m,:), a_ref)
   call print_avg_z(sd_cr_num(il,m,:), sd_cr_den(il,m,:), a_ref)
   call print_avg_z(sd_dd_num(il,m,:), sd_dd_den(il,m,:), a_ref)
   close(78)
!end do  !nbinz
   endif !nbinz 

3 format(' zone t= "z_curv_cndtl-dsd-',F4.2,'" i=',i5,', f=block')
9 format(10(1pe12.5,1x))
  return
  end subroutine write_data

  !----------------------------------------
  !----------------------------------------
  !----------------------------------------
  subroutine print_avg(num, den, ref)
  implicit none
  real, intent(in), dimension(nbinc) :: num, den
  real, intent(in) :: ref
  real, dimension(nbinc) :: ave
  
  where(den .lt. stat_small)
    ave = 0.0
  elsewhere
    ave = num/den
  endwhere
  write(78,9) (ave(n)*ref, n = 1, nbinc)
  return
  9 format(10(1pe12.5,1x))
  end subroutine print_avg
  !----------------------------------------
  !----------------------------------------
  !----------------------------------------
  subroutine print_avg_z(num, den, ref)
  implicit none
  real, intent(in), dimension(0:nbinz) :: num, den
  real, intent(in) :: ref
  real, dimension(0:nbinz) :: ave
   
  where(den .lt. stat_small)
    ave = 0.0
  elsewhere
    ave = num/den
  endwhere
  write(78,9) (ave(n)*ref, n = 1, nbinz)  ! note that we don't write the first value, which is the unconditional value.
  return
  9 format(10(1pe12.5,1x))
  end subroutine print_avg_z
!----------------------------------------
end subroutine curv_cndtl_sd

!----------------------------------------------------------------------
subroutine calc_cndtl_sd(io, finish)
use topology_m, only: myid, gcomm
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

real, parameter:: Cval = 0.5, Cdel = 0.02

real, parameter :: binz_start = 0.0, binz_del = 0.05

integer, parameter :: nbinz = 20
real, save, dimension(0:nbinz) :: binz_val

real, save, dimension(1:nbinz) :: sd_num, sd_den
real, save, dimension(1:nbinz) :: sd_r_num, sd_r_den
real, save, dimension(1:nbinz) :: sd_n_num, sd_n_den
real, save, dimension(1:nbinz) :: sd_c_num, sd_c_den
real, save, dimension(1:nbinz) :: sd_mf_num, sd_mf_den
real, save, dimension(1:nbinz) :: sd_cr_num, sd_cr_den
real, save, dimension(1:nbinz) :: sd_dd_num, sd_dd_den
real, save, dimension(1:nbinz) :: rho_den


logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx,ny,nz) :: wt, rho
real, dimension(nx,ny,nz) :: Cprog
real, dimension(nx,ny,nz) :: sd, sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd
real, dimension(nx) :: num, den

real, dimension(1:nbinz) :: ave
real rho_0
integer n

character*100 filename
character*2 xid_ext

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_cndtl_sd'

  do n = 0, nbinz
    binz_val(n) = binz_start + binz_del*real(n)
  enddo
            
!set the mixture fraction variable      
  call calculate_progvar(Cprog,io)
  
  sd_num = 0.0; sd_den = 0.0;
  sd_r_num = 0.0; sd_r_den = 0.0;
  sd_n_num = 0.0; sd_n_den = 0.0;
  sd_c_num = 0.0; sd_c_den = 0.0;
  sd_mf_num = 0.0; sd_mf_den = 0.0;
  sd_cr_num = 0.0; sd_cr_den = 0.0;
  sd_dd_num = 0.0; sd_dd_den = 0.0;  
  rho_den = 0.0;

  initialized = .true.
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
!note that the far field values for reaction, normal diffusion and unburned
! density are those Ramanan used in his phi=0.7, T=800K premixed case.
  rho_0 =  0.4261/rho_ref
  call calc_sd_comp &
    (0.0, 0.0, 0.4261/rho_ref, .false., &
                 sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd, sd,io)
!  call calc_sd_comp &
!  (385.23/rr_ref, 46.81/rr_ref,  rho_0, .false., &
!               sd_r, sd_n, sd_c, sd,io)

!  call calc_sd_comp &
!  (0.0, 0.0,  rho_0, .false., &
!               sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd, sd,io)

  wt = 1.0
  rho = q(:,:,:,4,1)

  !----------------------------------------
  ! Calculate conditional mean grad_c.
  ! Again calculate only for i=nx.
  do n = 1, nbinz
    where(Zetafield .gt. binz_val(n-1) .and. &
          Zetafield .le. binz_val(n) .and. &
          Cprog .gt. Cval-Cdel .and. Cprog .le. Cval+Cdel)
      ifcond = .true.
    elsewhere
      ifcond = .false.
    endwhere
    !----------------------------------------
    call calc_yz_accum_numden (sd, ifcond, rho, num, den)
    sd_num(n) = sd_num(n) + num(nx);
    sd_den(n) = sd_den(n) + den(nx);
    call calc_yz_accum_numden (sd_r, ifcond, rho, num, den)
    sd_r_num(n) = sd_r_num(n) + num(nx);
    sd_r_den(n) = sd_r_den(n) + den(nx);
    call calc_yz_accum_numden (sd_n, ifcond, rho, num, den)
    sd_n_num(n) = sd_n_num(n) + num(nx);
    sd_n_den(n) = sd_n_den(n) + den(nx);
    call calc_yz_accum_numden (sd_c, ifcond, rho, num, den)
    sd_c_num(n) = sd_c_num(n) + num(nx);
    sd_c_den(n) = sd_c_den(n) + den(nx);
    call calc_yz_accum_numden (sd_mf, ifcond, rho, num, den)
    sd_mf_num(n) = sd_mf_num(n) + num(nx);
    sd_mf_den(n) = sd_mf_den(n) + den(nx);
    call calc_yz_accum_numden (sd_cr, ifcond, rho, num, den)
    sd_cr_num(n) = sd_cr_num(n) + num(nx);
    sd_cr_den(n) = sd_cr_den(n) + den(nx);
    call calc_yz_accum_numden (sd_dd, ifcond, rho, num, den)
    sd_dd_num(n) = sd_dd_num(n) + num(nx);
    sd_dd_den(n) = sd_dd_den(n) + den(nx);

    call calc_yz_accum_numden (rho_0*wt, ifcond, wt, num, den)
    rho_den(n) = rho_den(n) + num(nx);
    
  end do
else !Finishing step
 if (yz_id == 0) then
   write(xid_ext,'(I2.2)') xid
   filename = '../post/tecplot/cndtl_sd.'//trim(xid_ext)//'.tec'
   open(unit=78, file=trim(filename), status='unknown')

   write(78,*) 'title = curv_cndtl_dsd'
   write(78,*) 'variables = '
   write(78,*) '"MF"'      
   write(78,*) '"sd[m/s]"'
   write(78,*) '"sdr[m/s]"'
   write(78,*) '"sdn[m/s]"'
   write(78,*) '"sdc[m/s]"'
   write(78,*) '"sdmf[m/s]"'
   write(78,*) '"sdcr[m/s]"'
   write(78,*) '"sddd[m/s]"'
   write(78, 3)  xid, nbinz
   write(78,9) (0.5*(binz_val(n-1)+binz_val(n)), n = 1, nbinz)
   !----------------------------------------

   call print_avg(sd_num, rho_den, a_ref)
   call print_avg(sd_r_num, rho_den, a_ref)
   call print_avg(sd_n_num, rho_den, a_ref)
   call print_avg(sd_c_num, rho_den, a_ref)
   call print_avg(sd_mf_num, rho_den, a_ref)
   call print_avg(sd_cr_num, rho_den, a_ref)
   call print_avg(sd_dd_num, rho_den, a_ref)
 end if
end if !Finish

3 format(' zone t= "cndtl_sd-',i2,'" i=',i5,', f=block')
9 format(10(1pe12.5,1x))

contains
!----------------------------------------
subroutine print_avg(num, den, ref)
implicit none
real, intent(in), dimension(nbinz) :: num, den
real, intent(in) :: ref
real, dimension(nbinz) :: ave

where(den .lt. stat_small)
  ave = 0.0
elsewhere
  ave = num/den
endwhere
write(78,9) (ave(n)*ref, n = 1, nbinz)
return
9 format(10(1pe12.5,1x))
end subroutine print_avg
!----------------------------------------
end subroutine calc_cndtl_sd

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine c_cndtl_sd(io, finish)
use topology_m, only: myid, gcomm

! 19june08, I am modifying this to out put the raw conditional sd components.

implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

integer, parameter :: nbinc = 25
real, save, dimension(0:nbinc) :: binc_val

real, save, dimension(:,:,:), allocatable :: an_num, an_den
real, save, dimension(:,:,:), allocatable :: at_num, at_den
real, save, dimension(:,:,:), allocatable :: dsd_num, dsd_den
real, save, dimension(:,:,:), allocatable :: dsd_r_num, dsd_r_den
real, save, dimension(:,:,:), allocatable :: dsd_n_num, dsd_n_den
real, save, dimension(:,:,:), allocatable :: dsd_c_num, dsd_c_den
real, save, dimension(:,:,:), allocatable :: dsd_mf_num, dsd_mf_den
real, save, dimension(:,:,:), allocatable :: dsd_cr_num, dsd_cr_den
real, save, dimension(:,:,:), allocatable :: dsd_dd_num, dsd_dd_den

real, save, dimension(:,:,:), allocatable :: sd_num, sd_den
real, save, dimension(:,:,:), allocatable :: sd_r_num, sd_r_den
real, save, dimension(:,:,:), allocatable :: sd_n_num, sd_n_den
real, save, dimension(:,:,:), allocatable :: sd_c_num, sd_c_den
real, save, dimension(:,:,:), allocatable :: sd_mf_num, sd_mf_den
real, save, dimension(:,:,:), allocatable :: sd_cr_num, sd_cr_den
real, save, dimension(:,:,:), allocatable :: sd_dd_num, sd_dd_den


logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, mag_gradC
real, dimension(nx,ny,nz) :: at, an, sd, sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd
real, dimension(nx,ny,nz) :: dsd, dsd_r, dsd_n, dsd_c, dsd_mf, dsd_cr, dsd_dd
real, dimension(nx,ny,nz, 3) :: normal
real, dimension(nx) :: num, den

real, dimension(1:nbinc,0:nbinz) :: ave
integer n, m

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing c_cndtl_sd'

  call make_bins_lmts(0.0, 1.0, nbinc, binc_val)

   allocate(an_num(nx,1:nbinc,0:nbinz)); allocate(an_den(nx,1:nbinc,0:nbinz))
   allocate(at_num(nx,1:nbinc,0:nbinz)); allocate(at_den(nx,1:nbinc,0:nbinz))
   allocate(dsd_num(nx,1:nbinc,0:nbinz)); allocate(dsd_den(nx,1:nbinc,0:nbinz))
   allocate(dsd_r_num(nx,1:nbinc,0:nbinz)); allocate(dsd_r_den(nx,1:nbinc,0:nbinz))
   allocate(dsd_n_num(nx,1:nbinc,0:nbinz)); allocate(dsd_n_den(nx,1:nbinc,0:nbinz))
   allocate(dsd_c_num(nx,1:nbinc,0:nbinz)); allocate(dsd_c_den(nx,1:nbinc,0:nbinz))
   allocate(dsd_mf_num(nx,1:nbinc,0:nbinz)); allocate(dsd_mf_den(nx,1:nbinc,0:nbinz))
   allocate(dsd_cr_num(nx,1:nbinc,0:nbinz)); allocate(dsd_cr_den(nx,1:nbinc,0:nbinz))
   allocate(dsd_dd_num(nx,1:nbinc,0:nbinz)); allocate(dsd_dd_den(nx,1:nbinc,0:nbinz))

   allocate(sd_num(nx,1:nbinc,0:nbinz)); allocate(sd_den(nx,1:nbinc,0:nbinz))
   allocate(sd_r_num(nx,1:nbinc,0:nbinz)); allocate(sd_r_den(nx,1:nbinc,0:nbinz))
   allocate(sd_n_num(nx,1:nbinc,0:nbinz)); allocate(sd_n_den(nx,1:nbinc,0:nbinz))
   allocate(sd_c_num(nx,1:nbinc,0:nbinz)); allocate(sd_c_den(nx,1:nbinc,0:nbinz))
   allocate(sd_mf_num(nx,1:nbinc,0:nbinz)); allocate(sd_mf_den(nx,1:nbinc,0:nbinz))
   allocate(sd_cr_num(nx,1:nbinc,0:nbinz)); allocate(sd_cr_den(nx,1:nbinc,0:nbinz))
   allocate(sd_dd_num(nx,1:nbinc,0:nbinz)); allocate(sd_dd_den(nx,1:nbinc,0:nbinz))
                     

  an_num = 0.0; an_den = 0.0;
  at_num = 0.0; at_den = 0.0;
  dsd_num = 0.0; dsd_den = 0.0;
  dsd_r_num = 0.0; dsd_r_den = 0.0;
  dsd_n_num = 0.0; dsd_n_den = 0.0;
  dsd_c_num = 0.0; dsd_c_den = 0.0;
  dsd_mf_num = 0.0;dsd_mf_den= 0.0;
  dsd_cr_num = 0.0;dsd_cr_den= 0.0;
  dsd_dd_num = 0.0;dsd_dd_den= 0.0;

  sd_num = 0.0; sd_den = 0.0;
  sd_r_num = 0.0; sd_r_den = 0.0;
  sd_n_num = 0.0; sd_n_den = 0.0;
  sd_c_num = 0.0; sd_c_den = 0.0;
  sd_mf_num = 0.0;sd_mf_den= 0.0;
  sd_cr_num = 0.0;sd_cr_den= 0.0;
  sd_dd_num = 0.0;sd_dd_den= 0.0;
            

  initialized = .true.
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call calc_strain(normal,at)
  call calc_normal_strain(normal,an)
!  call calc_sd_comp &
!  (0.0, 0.0, 0.4261/rho_ref, .true., sd_r, sd_n, sd_c, sd)
!!  (385.23/rr_ref, 46.81/rr_ref, 0.4261/rho_ref, .true., sd_r, sd_n, sd_c, sd)
  call calc_sd_comp &
    (0.0, 0.0, 0.4261/rho_ref, .false., &
                  sd_r, sd_n, sd_c, sd_mf, sd_cr, sd_dd, sd,io)

  call calc_normal_deriv(sd, normal, dsd)
  call calc_normal_deriv(sd_r, normal, dsd_r)
  call calc_normal_deriv(sd_n, normal, dsd_n)
  call calc_normal_deriv(sd_c, normal, dsd_c)
  call calc_normal_deriv(sd_mf, normal, dsd_mf)
  call calc_normal_deriv(sd_cr, normal, dsd_cr)
  call calc_normal_deriv(sd_dd, normal, dsd_dd)

! reweight the sd contributions here
   sd=sd       *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_r=sd_r   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_n=sd_n   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_c=sd_c   *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_mf=sd_mf *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_cr=sd_cr *q(:,:,:,4,1)/(0.4251/rho_ref)
   sd_dd=sd_dd *q(:,:,:,4,1)/(0.4251/rho_ref)
  
  wt = 1.0
  !----------------------------------------
  ! Calculate conditional mean grad_c.
  ! Again calculate only for i=nx.
  do m = 0, nbinz
  do n = 1, nbinc
    where(Cprog .gt. binc_val(n-1) .and. Cprog .le. binc_val(n) &
    .and.(( zetafield.gt.binz_val(m-1).and.zetafield.le.binz_val(m)).or.(m.eq.0) )  )
      ifcond = .true.
    elsewhere 
      ifcond = .false.
    endwhere

    call calc_yz_accum_numden (at, ifcond, wt, at_num(:,n,m), at_den(:,n,m))
    call calc_yz_accum_numden (an, ifcond, wt, an_num(:,n,m), an_den(:,n,m))
    call calc_yz_accum_numden (dsd, ifcond, wt, dsd_num(:,n,m), dsd_den(:,n,m))
    call calc_yz_accum_numden (dsd_r, ifcond, wt, dsd_r_num(:,n,m), dsd_r_den(:,n,m))
    call calc_yz_accum_numden (dsd_n, ifcond, wt, dsd_n_num(:,n,m), dsd_n_den(:,n,m))
    call calc_yz_accum_numden (dsd_c, ifcond, wt, dsd_c_num(:,n,m), dsd_c_den(:,n,m))
    call calc_yz_accum_numden (dsd_mf, ifcond, wt, dsd_mf_num(:,n,m), dsd_mf_den(:,n,m))
    call calc_yz_accum_numden (dsd_cr, ifcond, wt, dsd_cr_num(:,n,m), dsd_cr_den(:,n,m))
    call calc_yz_accum_numden (dsd_dd, ifcond, wt, dsd_dd_num(:,n,m), dsd_dd_den(:,n,m))

    call calc_yz_accum_numden (sd, ifcond, wt, sd_num(:,n,m), sd_den(:,n,m))
    call calc_yz_accum_numden (sd_r, ifcond, wt, sd_r_num(:,n,m), sd_r_den(:,n,m))
    call calc_yz_accum_numden (sd_n, ifcond, wt, sd_n_num(:,n,m), sd_n_den(:,n,m))
    call calc_yz_accum_numden (sd_c, ifcond, wt, sd_c_num(:,n,m), sd_c_den(:,n,m))
    call calc_yz_accum_numden (sd_mf, ifcond, wt, sd_mf_num(:,n,m), sd_mf_den(:,n,m))
    call calc_yz_accum_numden (sd_cr, ifcond, wt, sd_cr_num(:,n,m), sd_cr_den(:,n,m))
    call calc_yz_accum_numden (sd_dd, ifcond, wt, sd_dd_num(:,n,m), sd_dd_den(:,n,m))
 

  end do
  end do !nbinz
else !Finishing step
  do m = 0, nbinz
  do n = 1, nbinc
    call calc_Xfld_Xmovsum(at_num(:,n,m), window)
    call calc_Xfld_Xmovsum(at_den(:,n,m), window)
    call calc_Xfld_Xmovsum(an_num(:,n,m), window)
    call calc_Xfld_Xmovsum(an_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_r_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_r_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_n_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_n_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_c_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_c_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_mf_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_mf_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_cr_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_cr_den(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_dd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(dsd_dd_den(:,n,m), window)

    call calc_Xfld_Xmovsum(sd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_r_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_r_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_n_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_n_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_c_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_c_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_mf_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_mf_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_cr_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_cr_den(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_dd_num(:,n,m), window)
    call calc_Xfld_Xmovsum(sd_dd_den(:,n,m), window)
 
  enddo
  enddo
  call write_data(0.25)
  call write_data(0.50)
  call write_data(0.75)
endif      
return

contains !CONTAINS CONTAINS CONTAINS
  !----------------------------------------
  subroutine write_data(b)
  implicit none
  real, intent(in) :: b

  integer i
  integer n, l
  integer il
  character*4 loc_ext
  character*100 filename
  character*2 xiid_ext
  real zst,z1,z2
  real, dimension(0:nbinz) :: binphi_val

  i = int(nx_g*b)
  if (i<=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return

  il = i - xid*nx
  write(loc_ext,'(F4.2)') b
do m=0,nbinz
  write(xiid_ext,'(I2.2)')m
  filename = '../post/tecplot/c_cndtl_dsd.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')
   write(78,*) 'title = c_cndtl_dsd'
   write(78,*) 'variables = '
   write(78,*) '"C"'
   write(78,*) '"at[1/s]"'
   write(78,*) '"an[1/s]"'
   write(78,*) '"dsd[1/s]"'
   write(78,*) '"dsdr[1/s]"'
   write(78,*) '"dsdn[1/s]"'
   write(78,*) '"dsdc[1/s]"'
   write(78,*) '"dsdmf[1/s]"'
   write(78,*) '"dsdcr[1/s]"'
   write(78,*) '"dsddd[1/s]"'
   write(78,*) '"sd[m/s]"'
   write(78,*) '"sdr[m/s]"'
   write(78,*) '"sdn[m/s]"'
   write(78,*) '"sdc[m/s]"'
   write(78,*) '"sdmf[m/s]"'
   write(78,*) '"sdcr[m/s]"'
   write(78,*) '"sddd[m/s]"'
   write(78, 3)  b, nbinc
   write(78,9) (0.5*(binc_val(n-1)+binc_val(n)), n = 1, nbinc)
   !----------------------------------------
   call print_avg(at_num(il,:,m), at_den(il,:,m), 1.0/time_ref)
   call print_avg(an_num(il,:,m), an_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_num(il,:,m), dsd_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_r_num(il,:,m), dsd_r_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_n_num(il,:,m), dsd_n_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_c_num(il,:,m), dsd_c_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_mf_num(il,:,m), dsd_mf_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_cr_num(il,:,m), dsd_cr_den(il,:,m), 1.0/time_ref)
   call print_avg(dsd_dd_num(il,:,m), dsd_dd_den(il,:,m), 1.0/time_ref)
   call print_avg(sd_num(il,:,m), sd_den(il,:,m), a_ref)
   call print_avg(sd_r_num(il,:,m), sd_r_den(il,:,m), a_ref)
   call print_avg(sd_n_num(il,:,m), sd_n_den(il,:,m), a_ref)
   call print_avg(sd_c_num(il,:,m), sd_c_den(il,:,m), a_ref)
   call print_avg(sd_mf_num(il,:,m), sd_mf_den(il,:,m), a_ref)
   call print_avg(sd_cr_num(il,:,m), sd_cr_den(il,:,m), a_ref)
   call print_avg(sd_dd_num(il,:,m), sd_dd_den(il,:,m), a_ref)
 
   close(78)
end do  !nbinz

!do m=1,nbinz
if(nbinz.gt.0)then  ! it only makes sense to write stuff versus z if you have nbinz>0

  if(n_spec.eq.15)then
    z1=0.0233
    z2=0.055066
  else!if(n_spec.eq.29)then
    z1=0.0
    z2=0.0784
!  else
!    z1=0.0
!    z2=1.0
  endif
  zst=0.055066

  do n=1,nbinz
    binphi_val(n)=z1+binz_val(n)*(z2-z1)
    binphi_val(n)=binphi_val(n)*(1.0-zst)/zst/(1.0-binphi_val(n))
  enddo
!zst,z1,z2,binphi_val(nbinz)  

  m=nbinc/2
  write(xiid_ext,'(I2.2)')m
  filename = '../post/tecplot/z_cndtl_dsd.'//trim(loc_ext)//'.'//trim(xiid_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')
   write(78,*) 'title = z_cndtl_dsd'
   write(78,*) 'variables = '
   write(78,*) '"Z"'
   write(78,*) '"Phi"'   
   write(78,*) '"at[1/s]"'
   write(78,*) '"an[1/s]"'
   write(78,*) '"dsd[1/s]"'
   write(78,*) '"dsdr[1/s]"'
   write(78,*) '"dsdn[1/s]"'
   write(78,*) '"dsdc[1/s]"'
   write(78,*) '"dsdmf[1/s]"'
   write(78,*) '"dsdcr[1/s]"'
   write(78,*) '"dsddd[1/s]"'
   write(78,*) '"sd[m/s]"'
   write(78,*) '"sdr[m/s]"'
   write(78,*) '"sdn[m/s]"'
   write(78,*) '"sdc[m/s]"'
   write(78,*) '"sdmf[m/s]"'
   write(78,*) '"sdcr[m/s]"'
   write(78,*) '"sddd[m/s]"'
   write(78, 3)  b, nbinz
   write(78,9) (0.5*(binz_val(n-1)+binz_val(n)), n = 1, nbinz)
   write(78,9) (0.5*(binphi_val(n-1)+binphi_val(n)), n = 1, nbinc)   
   !----------------------------------------
   call print_avg_z(at_num(il,m,:), at_den(il,m,:), 1.0/time_ref)
   call print_avg_z(an_num(il,m,:), an_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_num(il,m,:), dsd_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_r_num(il,m,:), dsd_r_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_n_num(il,m,:), dsd_n_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_c_num(il,m,:), dsd_c_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_mf_num(il,m,:), dsd_mf_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_cr_num(il,m,:), dsd_cr_den(il,m,:), 1.0/time_ref)
   call print_avg_z(dsd_dd_num(il,m,:), dsd_dd_den(il,m,:), 1.0/time_ref)
   call print_avg_z(sd_num(il,m,:), sd_den(il,m,:), a_ref)
   call print_avg_z(sd_r_num(il,m,:), sd_r_den(il,m,:), a_ref)
   call print_avg_z(sd_n_num(il,m,:), sd_n_den(il,m,:), a_ref)
   call print_avg_z(sd_c_num(il,m,:), sd_c_den(il,m,:), a_ref)
   call print_avg_z(sd_mf_num(il,m,:), sd_mf_den(il,m,:), a_ref)
   call print_avg_z(sd_cr_num(il,m,:), sd_cr_den(il,m,:), a_ref)
   call print_avg_z(sd_dd_num(il,m,:), sd_dd_den(il,m,:), a_ref)
 
   close(78)

endif   !nbinz>0
!end do  !nbinz

3 format(' zone t= "c_cndtl-dsd-',F4.2,'" i=',i5,', f=block')
9 format(10(1pe12.5,1x))
  return
  end subroutine write_data

  !----------------------------------------
  !----------------------------------------
  !----------------------------------------
  subroutine print_avg(num, den, ref)
  implicit none
  real, intent(in), dimension(nbinc) :: num, den
  real, intent(in) :: ref
  real, dimension(nbinc) :: ave
 
  where(den .lt. stat_small)
    ave = 0.0
  elsewhere
    ave = num/den
  endwhere
  write(78,9) (ave(n)*ref, n = 1, nbinc)
  return
  9 format(10(1pe12.5,1x))
  end subroutine print_avg
!----------------------------------------
!----------------------------------------
  subroutine print_avg_z(num, den, ref)
  implicit none
  real, intent(in), dimension(0:nbinz) :: num, den
  real, intent(in) :: ref
  real, dimension(0:nbinz) :: ave

  where(den .lt. stat_small)
  ave = 0.0
  elsewhere
  ave = num/den
  endwhere
  write(78,9) (ave(n)*ref, n = 1, nbinz)
  return
  9 format(10(1pe12.5,1x))
  end subroutine print_avg_z
!----------------------------------------
end subroutine c_cndtl_sd

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!DIST !----------------------------------------------------------------------
!DIST subroutine dist_cndtl_mean(io, finish)
!DIST use topology_m, only: myid
!DIST use reference_m
!DIST use chemkin_m, only: reaction_rate, species_name
!DIST use isosurf_m
!DIST use distance_m
!DIST implicit none
!DIST integer, intent(in) :: io
!DIST logical, intent(in) :: finish
!DIST 
!DIST logical, save :: initialized=.false.
!DIST 
!DIST integer, parameter :: nbinc = 50
!DIST real, parameter :: maxdist = 0.5e-3 !1mm 
!DIST real, save, dimension(0:nbinc) :: binc_val
!DIST real :: binc_start, binc_del
!DIST 
!DIST real, save, allocatable, dimension(:,:) :: c_num, c_den
!DIST real :: c_ave(nx, nbinc)
!DIST real, save, allocatable, dimension(:,:,:) :: Y_num, Y_den, RR_num, RR_den
!DIST real, save, allocatable, dimension(:,:) :: hr_num, hr_den
!DIST 
!DIST logical, dimension(nx,ny,nz) :: ifcond
!DIST real, dimension(nx,ny,nz) :: wt
!DIST real, dimension(nx,ny,nz) :: Cprog, hr
!DIST real, dimension(nx,ny,nz,n_spec) :: rr_r
!DIST 
!DIST integer n, m
!DIST 
!DIST if(.not.initialized) then
!DIST   if(myid .eq. 0) write(io,*) 'Initializing dist_cndtl_mean'
!DIST   binc_start = -maxdist/l_ref
!DIST   binc_del = 2.0*maxdist/l_ref/real(nbinc)
!DIST   do n = 0, nbinc
!DIST     binc_val(n) = binc_start + binc_del*real(n)
!DIST   end do
!DIST 
!DIST   allocate(c_num(nx,1:nbinc), c_den(nx,1:nbinc))
!DIST   allocate(Y_num(nx,1:nbinc,n_spec), Y_den(nx,1:nbinc,n_spec))
!DIST   allocate(RR_num(nx,1:nbinc,n_spec), RR_den(nx,1:nbinc,n_spec))
!DIST   allocate(hr_num(nx,1:nbinc), hr_den(nx,1:nbinc))
!DIST   c_num = 0.0; c_den = 0.0;
!DIST   Y_num = 0.0; Y_den = 0.0;
!DIST   RR_num = 0.0; RR_den = 0.0;
!DIST   hr_num = 0.0; hr_den = 0.0;
!DIST   initialized = .true.
!DIST end if
!DIST 
!DIST if(.not. finish) then
!DIST   call calculate_progvar(Cprog,io)
!DIST   call extract_isosurface(io)
!DIST   call compute_dist_from_isosurface(maxdist/l_ref)
!DIST   call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
!DIST   call calc_heat_release(rr_r, hr)
!DIST   wt = 1.0
!DIST   do n = 1, nbinc
!DIST     !----------------------------------------
!DIST     where(dist .gt. binc_val(n-1) .and. &
!DIST           dist .le. binc_val(n) )
!DIST       ifcond = .true.
!DIST     elsewhere 
!DIST       ifcond = .false.
!DIST     endwhere
!DIST     !----------------------------------------
!DIST     call calc_yz_accum_numden (Cprog, ifcond, wt, c_num(:,n), c_den(:,n))
!DIST     do m = 1, n_spec
!DIST       call calc_yz_accum_numden &
!DIST         (yspecies(:,:,:,m), ifcond, wt, Y_num(:,n,m), Y_den(:,n,m))
!DIST     end do
!DIST     do m = 1, n_spec
!DIST       call calc_yz_accum_numden &
!DIST         (rr_r(:,:,:,m), ifcond, wt, RR_num(:,n,m), RR_den(:,n,m))
!DIST     end do
!DIST     call calc_yz_accum_numden (hr, ifcond, wt, hr_num(:,n), hr_den(:,n))
!DIST     !----------------------------------------
!DIST   end do
!DIST else !Finishing step
!DIST   !----------------------------------------
!DIST   !Apply the moving window average
!DIST   do n = 1, nbinc
!DIST     call calc_Xfld_Xmovsum(c_num(:,n), window)
!DIST     call calc_Xfld_Xmovsum(c_den(:,n), window)
!DIST     do m = 1, n_spec
!DIST       call calc_Xfld_Xmovsum(Y_num(:,n,m), window)
!DIST       call calc_Xfld_Xmovsum(Y_den(:,n,m), window)
!DIST       call calc_Xfld_Xmovsum(RR_num(:,n,m), window)
!DIST       call calc_Xfld_Xmovsum(RR_den(:,n,m), window)
!DIST     end do
!DIST     call calc_Xfld_Xmovsum(hr_num(:,n), window)
!DIST     call calc_Xfld_Xmovsum(hr_den(:,n), window)
!DIST   end do
!DIST   !----------------------------------------
!DIST   ! Dump the mean to disk. used to calculate next moment
!DIST   where(c_den .gt. stat_small) 
!DIST      c_ave = c_num/c_den 
!DIST   elsewhere
!DIST      c_ave = 0.0
!DIST   end where
!DIST   call dump_XCfld('Cave_dist', nbinc, c_ave)
!DIST   !----------------------------------------
!DIST   if(myid==0) write(io,*) '!----------------------------'
!DIST   if(myid==0) write(io,*) 'Writing dist_cndtl_mean'
!DIST 
!DIST   call write_data(0.25)
!DIST   call write_data(0.50)
!DIST   call write_data(0.75)
!DIST end if !Finish
!DIST 
!DIST contains
!DIST   !----------------------------------------
!DIST   subroutine write_data(b)
!DIST   implicit none
!DIST   real, intent(in) :: b
!DIST   integer i, il
!DIST 
!DIST   character loc_ext*4, filename*100
!DIST 
!DIST   i = int(nx_g*b)
!DIST   if(i <=xid*nx .or. i>(xid+1)*nx) return
!DIST   if(yz_id .ne. 0) return
!DIST   il = i - xid*nx
!DIST 
!DIST   if(myid==0) write(io,*) '!----------------------------'
!DIST   if(myid==0) write(io,*) 'Writing dist_mean'
!DIST 
!DIST   write(loc_ext,'(F4.2)') b
!DIST   filename = '../post/tecplot/dist_mean.'//trim(loc_ext)//'.tec'
!DIST   open(unit=78, file=trim(filename), status='unknown')
!DIST 
!DIST   write(78,*) 'title = dist_mean'
!DIST   write(78,*) 'variables = '
!DIST   write(78,*) '"Distance [mm]"'
!DIST   write(78,*) '"Cmean"'
!DIST   write(78,*) '"HR"'
!DIST   do m=1,n_spec
!DIST     write(78,*) '"'//trim(species_name(m))//'"'
!DIST   enddo
!DIST   do m=1,n_spec
!DIST     write(78,*) '"RR-'//trim(species_name(m))//'"'
!DIST   enddo
!DIST   write(78, 3)  b, nbinc
!DIST   write(78,9) (0.5*(binc_val(n-1)+binc_val(n))*l_ref*1e3, n = 1, nbinc)
!DIST   call print_avg(c_num(il,:), c_den(il,:), 1.0)
!DIST   call print_avg(hr_num(il,:), hr_den(il,:), -hr_ref)
!DIST   do m = 1, n_spec
!DIST     call print_avg(Y_num(il,:,m), Y_den(il,:,m), 1.0)
!DIST   end do
!DIST   do m = 1, n_spec
!DIST     call print_avg(RR_num(il,:,m), RR_den(il,:,m), rr_ref)
!DIST   end do
!DIST 
!DIST 3 format(' zone t= "dist_mean-',F4.2,'", i=',i5,', f=block')
!DIST 9 format(10(1pe12.5,1x))
!DIST 
!DIST   end subroutine write_data
!DIST 
!DIST   !----------------------------------------
!DIST   subroutine print_avg(num, den, ref)
!DIST   implicit none
!DIST   real, intent(in), dimension(1:nbinc) :: num, den
!DIST   real, intent(in) :: ref
!DIST   real :: ave(1:nbinc)
!DIST   integer n
!DIST   
!DIST   where(den .gt. stat_small) 
!DIST      ave = num/den 
!DIST   elsewhere
!DIST      ave = 0.0
!DIST   end where
!DIST   write(78,9) (ave(n)*ref, n = 1, nbinc)
!DIST   
!DIST   9 format(10(1pe12.5,1x))
!DIST   return
!DIST   end subroutine print_avg
!DIST 
!DIST  end subroutine dist_cndtl_mean
!DIST !----------------------------------------------------------------------
!DIST !----------------------------------------------------------------------
!DIST   !----------------------------------------------------------------------
!DIST   subroutine dist_cndtl_prime(io, finish)
!DIST   use topology_m, only: myid
!DIST   use reference_m
!DIST   use isosurf_m
!DIST   use distance_m
!DIST   implicit none
!DIST   integer, intent(in) :: io
!DIST   logical, intent(in) :: finish
!DIST   
!DIST   logical, save :: initialized=.false.
!DIST   
!DIST   integer, parameter :: nbinc = 50
!DIST   real :: maxdist
!DIST   real, save, dimension(0:nbinc) :: binc_val
!DIST   real :: binc_start, binc_del
!DIST   
!DIST   real, save, allocatable, dimension(:,:) :: cp_num, cp_den
!DIST   real, save, allocatable, dimension(:,:) :: C_ave
!DIST   
!DIST   logical, dimension(nx,ny,nz) :: ifcond
!DIST   real, dimension(nx,ny,nz) :: wt
!DIST   real, dimension(nx,ny,nz) :: Cprog,Cprime2
!DIST   real, dimension(nx, nbinc) :: cp_ave
!DIST   integer indx
!DIST   
!DIST   integer n, m
!DIST   integer i, j, k
!DIST   
!DIST   maxdist = 1.0e-3/l_ref ! 1mm
!DIST   
!DIST   if(.not.initialized) then
!DIST     if(myid .eq. 0) write(io,*) 'Initializing dist_cndtl_prime'
!DIST     binc_start = -maxdist
!DIST     binc_del = 2.0*maxdist/real(nbinc)
!DIST     do n = 0, nbinc
!DIST       binc_val(n) = binc_start + binc_del*real(n)
!DIST     end do
!DIST   
!DIST     allocate(cp_num(nx,1:nbinc), cp_den(nx,1:nbinc))
!DIST     cp_num = 0.0; cp_den = 0.0;
!DIST   
!DIST     allocate(C_ave(nx,1:nbinc))
!DIST     call load_XCfld('Cave_dist', nbinc, C_ave)
!DIST     initialized = .true.
!DIST   end if
!DIST   
!DIST   if(.not. finish) then
!DIST     call calculate_progvar(Cprog,io)
!DIST     call extract_isosurface(io)
!DIST     call compute_dist_from_isosurface(maxdist)
!DIST     Cprime2 = 0.0
!DIST     do k = 1, nz
!DIST       do j = 1, ny
!DIST         do i = 1, nx
!DIST           indx = int( (dist(i,j,k)+maxdist)*real(nbinc)/2.0/maxdist) + 1
!DIST           if(indx<1 .or. indx>nbinc) cycle
!DIST           Cprime2(i,j,k) = Cprog(i,j,k) - C_ave(i, indx)
!DIST         end do
!DIST       end do
!DIST     end do
!DIST     Cprime2 = Cprime2*Cprime2
!DIST     wt = 1.0
!DIST     do n = 1, nbinc
!DIST       !----------------------------------------
!DIST       where(dist .gt. binc_val(n-1) .and. &
!DIST             dist .le. binc_val(n) )
!DIST         ifcond = .true.
!DIST       elsewhere 
!DIST         ifcond = .false.
!DIST       endwhere
!DIST       !----------------------------------------
!DIST       call calc_yz_accum_numden (Cprime2, ifcond, wt, cp_num(:,n), cp_den(:,n))
!DIST       !----------------------------------------
!DIST     end do
!DIST   else !Finishing step
!DIST     !----------------------------------------
!DIST     !Apply the moving window average
!DIST     do n = 1, nbinc
!DIST       call calc_Xfld_Xmovsum(cp_num(:,n), window)
!DIST       call calc_Xfld_Xmovsum(cp_den(:,n), window)
!DIST     end do
!DIST     !----------------------------------------
!DIST     if(myid==0) write(io,*) '!----------------------------'
!DIST     if(myid==0) write(io,*) 'Writing dist_cndtl_prime'
!DIST   
!DIST     call write_data(0.25)
!DIST     call write_data(0.50)
!DIST     call write_data(0.75)
!DIST   end if !Finish
!DIST   
!DIST   contains
!DIST     !----------------------------------------
!DIST     subroutine write_data(b)
!DIST     implicit none
!DIST     real, intent(in) :: b
!DIST     integer i, il
!DIST   
!DIST     character loc_ext*4, filename*100
!DIST   
!DIST     i = int(nx_g*b)
!DIST     if(i <=xid*nx .or. i>(xid+1)*nx) return
!DIST     if(yz_id .ne. 0) return
!DIST     il = i - xid*nx
!DIST   
!DIST     if(myid==0) write(io,*) '!----------------------------'
!DIST     if(myid==0) write(io,*) 'Writing dist_prime'
!DIST   
!DIST     write(loc_ext,'(F4.2)') b
!DIST     filename = '../post/tecplot/dist_prime.'//trim(loc_ext)//'.tec'
!DIST     open(unit=78, file=trim(filename), status='unknown')
!DIST   
!DIST     write(78,*) 'title = dist_prime'
!DIST     write(78,*) 'variables = '
!DIST     write(78,*) '"Distance [mm]"'
!DIST     write(78,*) '"Cprime"'
!DIST     write(78, 3)  b, nbinc
!DIST     write(78,9) (0.5*(binc_val(n-1)+binc_val(n))*l_ref*1e3, n = 1, nbinc)
!DIST     where(cp_den .gt. stat_small)
!DIST       cp_ave = cp_num/cp_den
!DIST     elsewhere
!DIST       cp_ave = 0.0
!DIST     end where
!DIST     cp_ave = sqrt(cp_ave)
!DIST     write(78, 9) (cp_ave(il,n), n = 1, nbinc)
!DIST   
!DIST   3 format(' zone t= "dist_prime-',F4.2,'", i=',i5,', f=block')
!DIST   9 format(10(1pe12.5,1x))
!DIST   
!DIST     end subroutine write_data
!DIST   
!DIST   end subroutine dist_cndtl_prime
!DIST   
!DIST   !----------------------------------------------------------------------
!DIST   !----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine dump_XCfld(string, nc, field)
use topology_m, only: xid, yz_id, yz_comm
use param_m, only: nx
implicit none
character(*) string
integer, intent(in) :: nc
real, intent(in) :: field(nx, nc)
character filename*200, myid_ext*5

if(yz_id .ne. 0) return
! Append my xy-id to the filename string
write(myid_ext, '(I5.5)') xid
filename = "../post/"//trim(string)//"."//trim(myid_ext)
open(unit=329, file=trim(filename), status='unknown', form='unformatted')
write(329) field
close(329)
return
end subroutine dump_XCfld

!----------------------------------------------------------------------
subroutine load_XCfld(string, nc, field)
use topology_m, only: xid, yz_id, yz_comm
use param_m, only: nx
implicit none
character(*) string
integer, intent(in) :: nc
real, intent(out) :: field(nx, nc)
character filename*200, myid_ext*5

if (yz_id .eq. 0) then
  ! Append my xy-id to the filename string
  write(myid_ext, '(I5.5)') xid
  filename = "../post/"//trim(string)//"."//trim(myid_ext)
  open(unit=329, file=trim(filename), status='old', form='unformatted')
  read(329) field
  close(329)
end if
call MPI_Bcast(field, nx*nc, MPI_REAL8, 0, yz_comm, ierr)

return
end subroutine load_XCfld

!----------------------------------------------------------------------
subroutine calc_FSD_IO(io,finish)
!use topology_m, only: myid
use chemkin_m, only: reaction_rate, species_name
use isosurf_m, only: extract_isosurface
use param_m, only: periodic_x, periodic_y, periodic_z
use triangulate_m
use grid_m
use ghost_nice_m
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.
real, save, allocatable, dimension(:, :) :: rrcp_num, rrcp_den
real, save, allocatable, dimension(:, :) :: rrsp_num, rrsp_den
real, save, allocatable, dimension(:, :) :: Slam_num, Slam_den
real, save, allocatable, dimension(:, :) :: tri_area
real, save, allocatable, dimension(:, :) :: fsd2_num, fsd2_den
real, save, allocatable, dimension(:, :) :: fsd3_num, fsd3_den
integer, save :: timecount = 0

real, dimension(nx,ny,nz) :: wt, Cprog, gradC, velLam
real, dimension(nx,ny) :: grid, io1, io2, fsd1_num, fsd1_den,fsd1,fsd2,rrcp,rrsp,slam
logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx, ny, nz, n_spec) :: rr_r, diffusion, ysp_prod, ysp_reac
real, allocatable :: vert_area(:)
real :: xx(nx+1), yy(ny+1), zz(nz+1)
integer n
integer i, j
character filename*50

real, dimension(nx) :: fsd1_xint,fsd2_xint,rrsp_xint,rrcp_xint,Slam_xint,I01_xint,I02_xint
integer iy1,iy2
real deltay


if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_FSD_IO'

  allocate(rrsp_num(nx, ny), rrsp_den(nx, ny))
  allocate(rrcp_num(nx, ny), rrcp_den(nx, ny))
  allocate(tri_area(0:nx+1, 0:ny+1))
  allocate(fsd2_num(nx, ny), fsd2_den(nx, ny))
  allocate(fsd3_num(nx, ny), fsd3_den(nx, ny))
  allocate(Slam_num(nx, ny), Slam_den(nx, ny))

  rrsp_num=0.0; rrsp_num=0.0
  rrcp_num=0.0; rrcp_num=0.0
  tri_area=0.0
  fsd2_num=0.0; fsd2_num=0.0
  fsd3_num=0.0; fsd3_num=0.0
  Slam_num=0.0; Slam_num=0.0

  initialized = .true.
end if

if(.not. finish) then
  wt = 1.0
  ifcond = .true.
  call calculate_progvar(Cprog,io)
  call calc_mag_grad(Cprog, gradC)
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
  !Based on species 4.
  call calc_z_accum_numden (rr_r(:,:,:,4), ifcond, wt, rrsp_num, rrsp_den)
  !Based on c.
  cprog=1.0
  call zclookup(cprog,zetafield,ysp_prod,temp,io)
  cprog=0.0
  call zclookup(cprog,zetafield,ysp_reac,temp,io)
  rr_r(:,:,:,4)=rr_r(:,:,:,4)/(ysp_prod(:,:,:,4)-ysp_reac(:,:,:,4))
  call calc_z_accum_numden (rr_r(:,:,:,4), ifcond, wt, rrcp_num, rrcp_den)
  !average rho_u.Sl
  call calc_ZtoRhoSl(zetafield,VelLam,io)
  VelLam = VelLam *gradC
!  VelLam(:,:,:)=gradC*1.8/a_ref*0.4261/rho_ref
  call calc_z_accum_numden (VelLam(:,:,:), ifcond, wt, Slam_num, Slam_den)

  !----------------------------------------
  ! Compute fsd based on triangle area
  call extract_isosurface(io)
  allocate(vert_area(vert_count))
  !----------------------------------------
  !Compute the vertex coordinates
  do i = 1, nx+1
    if(xid*nx+i <= nx_g) xx(i) = xg(xid*nx+i)
  end do
  if(xid.eq.xpes-1 .and. periodic_x.eq.1) xx(nx+1) = 2.0*xx(nx)-xx(nx-1)
  do i = 1, ny+1
    if(yid*ny+i <= ny_g) yy(i) = yg(yid*ny+i)
  end do
  if(yid.eq.ypes-1 .and. periodic_y.eq.1) yy(ny+1) = 2.0*yy(ny)-yy(ny-1)
  do i = 1, nz+1
    if(zid*nz+i <= nz_g) zz(i) = zg(zid*nz+i)
  end do
  if(zid.eq.zpes-1 .and. periodic_z.eq.1) zz(nz+1) = 2.0*zz(nz)-zz(nz-1)
  !----------------------------------------
  call vertex_area_weights(xx, yy, zz, vert_area)
  do n = 1, vert_count
    i = vert_loc(1, n)
    j = vert_loc(2, n)
    tri_area(i,j) = tri_area(i,j)+vert_area(n)
  end do
  !Count one for every time-step data read in.
  timecount = timecount + 1

  !----------------------------------------
  ! Compute fsd based on mean gradc everywhere

  call calc_z_accum_numden (gradC, ifcond, wt, fsd2_num, fsd2_den)

  !----------------------------------------
  ! Compute fsd based on mean GRADC on 0.65 surface
  where(Cprog .gt. 0.63 .and. Cprog .le. 0.67) 
    ifcond = .true.
  elsewhere
    ifcond = .false.
  end where
  call calc_z_accum_numden (gradC, ifcond, wt, fsd3_num, fsd3_den)
else
  !----------------------------------------
  ! Finishing step
  call ghostzone_real(tri_area, nx+1, ny+1, 1, 1, 0, 1, 0, 0, 0)
  if(xid>0 .or. periodic_x .eq. 1) &
    tri_area(1,1:ny) = tri_area(1,1:ny)+tri_area(0,1:ny)
  if(yid>0 .or. periodic_y .eq. 1) &
    tri_area(1:nx,1) = tri_area(1:nx,1)+tri_area(1:nx,0)
  call mpi_allreduce(tri_area, fsd1_num, nx*ny, MPI_REAL8, MPI_SUM, zcomm, ierr)
  fsd1_den = real(timecount)
  !----------------------------------------
  fsd3_den = fsd2_den 
  !----------------------------------------
  ! Divide triangle areas by volume to get FSD
  
  do j = 1, ny
    do i = 1, nx
      if(xid .eq. xpes-1 .and. i .eq. nx) cycle
      if(yid .eq. ypes-1 .and. j .eq. ny) cycle
      fsd1_num(i,j) = fsd1_num(i,j)/(xg(xid*nx+i+1)-xg(xid*nx+i))
      fsd1_num(i,j) = fsd1_num(i,j)/(yg(yid*ny+j+1)-yg(yid*ny+j))
      fsd1_num(i,j) = fsd1_num(i,j)/real(nz_g)/real(z(2)-z(1))
    end do
  end do
  !----------------------------------------
  !Finishing step. Write the data out.
  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing FSD_IO'

  if (myid == 0) then
    filename = '../post/tecplot/fsdio.tec'
    open(unit=78, file=trim(filename), status='unknown')

    write(78,*) 'title = fsdio'
    write(78,*) 'variables = '
    write(78,*) '"x [mm]"', '"y [mm]"'
    write(78,*) '"RR_ave '//trim(species_name(4))//' [1/s]"'
    write(78,*) '"RR_ave prog var [1/s]"'
    write(78,*) '"FSD-Triangle [1/m]"'
    write(78,*) '"FSD-|gC| [1/m]"'
    write(78,*) '"FSD-|gC| cond c=0.65 [1/m]"'
    write(78,*) '"IO-1"'
    write(78,*) '"IO-2"'
    write(78, 2)  nx_g, ny_g
  end if
  !----------------------------------------
  do i = 1, ny
    grid(:,i) = x(:)
  end do
  call write_XYfld(78,grid,l_ref*1e3)
  do i = 1, nx
    grid(i,:) = y(:)
  end do
  call write_XYfld(78,grid,l_ref*1e3)

  !----------------------------------------
  call print_avg(rrsp_num, rrsp_den, .true., rr_ref, rrsp)
  call print_avg(rrcp_num, rrcp_den, .true., rr_ref, rrcp)
  call print_avg(fsd1_num, fsd1_den, .true., 1.0/l_ref, fsd1)
  call print_avg(fsd2_num, fsd2_den, .true., 1.0/l_ref, fsd2)
  call print_avg(fsd3_num, fsd3_den, .true., 1.0/l_ref)
  call print_avg(Slam_num, Slam_den, .true., rho_ref*a_ref,Slam)

  !----------------------------------------
!  ! Divide by rho_0
!  rrcp = rrcp*rho_ref/0.4261
!  ! Divide by SL
!  rrcp = rrcp*a_ref/1.8
!  ! Convert to RR of Progress variable, here O2
!  rrcp = -rrcp/(0.223855-0.066943)

  where(Slam*fsd1<(1e-4/l_ref*0.05/a_ref*0.5/rho_ref) )
    IO1 = 1.0
  elsewhere
    IO1 = rrcp/(fsd1*Slam)
  end where

  where(Slam*fsd2<(1e-4/l_ref*0.05/a_ref*0.5/rho_ref) )
    IO2 = 1.0
  elsewhere
    IO2 = rrcp/(fsd2*Slam)
  end where

!  rrcp=-rrcp*(0.223855-0.066943)*1.8*0.4261/rho_ref/a_ref

  call write_XYfld(78,IO1,1.0)
  call write_XYfld(78,IO2,1.0)
  
  if(myid == 0) then
    close(78)
    write(io,*) 'Finished FSD_IO'
  end if

      deltay=min_grid_y/l_ref

!Apply averaging over z-direction
iy1=1
iy2=ny_g
do i=1,nx
call calc_sum_yrange(rrsp(i,:),rrsp_xint(i),iy1,iy2)
rrsp_xint(i)=0.5*deltay*rrsp_xint(i)
call calc_sum_yrange(rrcp(i,:),rrcp_xint(i),iy1,iy2)
rrcp_xint(i)=0.5*deltay*rrcp_xint(i)
call calc_sum_yrange(fsd1(i,:),fsd1_xint(i),iy1,iy2)
fsd1_xint(i)=0.5*deltay*fsd1_xint(i)
call calc_sum_yrange(fsd2(i,:),fsd2_xint(i),iy1,iy2)
fsd2_xint(i)=0.5*deltay*fsd2_xint(i)
call calc_sum_yrange(Slam(i,:),Slam_xint(i),iy1,iy2)
Slam_xint(i)=0.5*deltay*Slam_xint(i)/fsd2_xint(i)
enddo !nx

I01_xint(:)=rrcp_xint(:)/max(1.0D-10,fsd1_xint(:))/max(1.0D-10,Slam_xint(:))
I02_xint(:)=rrcp_xint(:)/max(1.0D-10,fsd2_xint(:))/max(1.0D-10,Slam_xint(:))

  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing FSD_XZ'

  if (myid == 0) then
    filename = '../post/tecplot/fsdxint.tec'
    open(unit=78, file=trim(filename), status='unknown')

! note that these variables have been integrated across the y-direction 
! which is why there is an extra length in the dimensions
    write(78,*) 'title = crossstreamFSD'
    write(78,*) 'variables = '
    write(78,*) '"x [mm]"'
    write(78,*) '"RR_ave spec [m/s]"'
    write(78,*) '"RR_ave Cprog [m/s]"'
    write(78,*) '"FSD-Triangle [-]"'
    write(78,*) '"FSD-|gC| [-]"'
!    write(78,*) '"Zeta_gradc [-]"'
!    write(78,*) '"Zeta2_gradc []"'
    write(78,*) '"rhoSlam_gradc [kg/m^2/s]"'
    write(78,*) '"IO1 [-]"'
    write(78,*) '"IO2 [-]"'
    write(78, 4)  nx_g
  end if
  !----------------------------------------
  call write_Xfld(78,x(:),l_ref*1e3)
  !----------------------------------------
  call write_Xfld(78,rrsp_xint(:),      rr_ref*l_ref)
  call write_Xfld(78,rrcp_xint(:),      rr_ref*l_ref)
  call write_Xfld(78,fsd1_xint(:),      1.0)
  call write_Xfld(78,fsd2_xint(:),      1.0)
  call write_Xfld(78,Slam_xint(:),      rho_ref*a_ref)
  call write_Xfld(78,I01_xint(:),      1.0)
  call write_Xfld(78,I02_xint(:),      1.0)

  if(myid == 0) then
    close(78)
    write(io,*) 'Finished FSD_XZ'
  end if

  
end if



1 format(' zone t= "',1pe9.3,'", i=',i5,', j=',i5,', f=block')
2 format(' zone t= tz_ave" i=',i5,', j=',i5,', f=block')
3 format(' zone t= "',1pe9.3,'", i=',i5,i' f=block')
4 format(' zone t= ty_ave i=',i5,', f=block')

return

contains
!----------------------------------------------------------------------
subroutine print_avg(num, den, symm, ref, aveout)
implicit none
real, dimension(nx, ny), intent(inout) :: num, den
real, dimension(nx, ny), optional, intent(out) :: aveout
real, dimension(nx, ny) :: ave
logical, intent(in) :: symm
real, intent(in) :: ref

!Apply window averaging
call calc_XYfld_Xmovsum(num, window)
call calc_XYfld_Xmovsum(den, window)

!Apply symmetry condition before dividing
call calc_XYfld_Ysym(num, symm)
call calc_XYfld_Ysym(den, .true.)
where (den .lt. stat_small)
  ave = 0.0
elsewhere
  ave = num/den
end where
call write_XYfld(78,ave,ref)

if(present(aveout)) aveout = ave

return
end subroutine print_avg

end subroutine calc_FSD_IO

!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_FSD_XZ(io,finish)
! this computes FSD statistics integrated over the flame 
! normal direction (assumed to be y).
! it reports these versus x at four z positions: 
!  (1) integrated over all z
!  (2) at z=0.0 Lz
!  (3) at z=0.25Lz
!  (4) at z=0.5 Lz,
! and versus z at three x positions: x = 0.25, 0.5, 0.75 Lx

! it reports averages of:
!  (1) O2 reaction rate
!  (2) c raction rate
!  (3) FSD based on triangles
!  (4) FSD based on gradient
!  (5) Mixture fraction weighted by gradient FSD
!  (6) Mixture fraction ^2 weighted by gradient FSD
!  (7) Sl(Z) weighted by gradient FSD.

! it then computes A_wrinkled and IO in various ways
! including by using <Z> and <Z^2> in delta and beta
! PDF approximations for Sl.

!use topology_m, only: myid
use chemkin_m, only: reaction_rate, species_name
use isosurf_m, only: extract_isosurface
use param_m, only: periodic_x, periodic_y, periodic_z
use triangulate_m
use grid_m
use ghost_nice_m
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized_XZ=.false.
real, save, allocatable, dimension(:, :) :: rrcp_num, rrcp_den
real, save, allocatable, dimension(:, :) :: rro2_num, rro2_den
real, save, allocatable, dimension(:, :) :: tri_area
real, save, allocatable, dimension(:, :) :: fsd2_num, fsd2_den
real, save, allocatable, dimension(:, :) :: fsd3_num, fsd3_den
real, save, allocatable, dimension(:, :) :: zeta_num, zeta_den
real, save, allocatable, dimension(:, :) :: zsqu_num, zsqu_den
real, save, allocatable, dimension(:, :) :: Slam_num, Slam_den
integer, save :: timecount = 0

real, dimension(nx,ny,nz) :: wt, Cprog, gradC,VelLam
real, dimension(nx,nz) :: grid, fsd1_num, fsd1_den, fsd1, fsd2, rrcp, io1, io2
real, dimension(nx) :: rro2_xint,rrcp_xint,fsd1_xint,fsd2_xint,zeta_xint,zsqu_xint,Slam_xint,io1x,io2x
logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx, ny, nz, n_spec) :: rr_r, diffusion,ysp_prod,ysp_reac
real, allocatable :: vert_area(:)
real :: xx(nx+1), yy(ny+1), zz(nz+1)
integer n
integer i, j, k
character filename*50
real deltay

if(.not.initialized_XZ) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_FSD_XZ'

  allocate(rrcp_num(nx, nz), rrcp_den(nx, nz))
  allocate(rro2_num(nx, nz), rro2_den(nx, nz))
  allocate(tri_area(0:nx+1, 0:nz+1))
  allocate(fsd2_num(nx, nz), fsd2_den(nx, nz))
  allocate(fsd3_num(nx, nz), fsd3_den(nx, nz))
  allocate(zeta_num(nx, nz), zeta_den(nx, nz))
  allocate(zsqu_num(nx, nz), zsqu_den(nx, nz))
  allocate(Slam_num(nx, nz), Slam_den(nx, nz))

  rrcp_num=0.0; rrcp_den=0.0
  rro2_num=0.0; rro2_den=0.0
  tri_area=0.0
  fsd2_num=0.0; fsd2_den=0.0
  fsd3_num=0.0; fsd3_den=0.0
  zeta_num=0.0; zeta_den=0.0
  zsqu_num=0.0; zsqu_den=0.0
  Slam_num=0.0; Slam_den=0.0

  initialized_XZ = .true.
end if

if(.not. finish) then

    
  wt = 1.0
  ifcond = .true.
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif

  !Based on Species4.
  call calc_y_accum_numden (rr_r(:,:,:,4), ifcond, wt, rro2_num, rro2_den)
  !Based on c.
  cprog=1.0
  call zclookup(cprog,zetafield,ysp_prod,temp,io)
  cprog=0.0
  call zclookup(cprog,zetafield,ysp_reac,temp,io)
  rr_r(:,:,:,4)=rr_r(:,:,:,4)/(ysp_prod(:,:,:,4)-ysp_reac(:,:,:,4))
  call calc_y_accum_numden (rr_r(:,:,:,4), ifcond, wt, rrcp_num, rrcp_den)

  call calculate_progvar(Cprog,io)
  call calc_mag_grad(Cprog, gradC)

  !----------------------------------------
  ! Compute fsd based on triangle area
  call extract_isosurface(io)
  allocate(vert_area(vert_count))
  !----------------------------------------
  !Compute the vertex coordinates
  do i = 1, nx+1
    if(xid*nx+i <= nx_g) xx(i) = xg(xid*nx+i)
  end do
  if(xid.eq.xpes-1 .and. periodic_x.eq.1) xx(nx+1) = 2.0*xx(nx)-xx(nx-1)
  do i = 1, ny+1
    if(yid*ny+i <= ny_g) yy(i) = yg(yid*ny+i)
  end do
  if(yid.eq.ypes-1 .and. periodic_y.eq.1) yy(ny+1) = 2.0*yy(ny)-yy(ny-1)
  do i = 1, nz+1
    if(zid*nz+i <= nz_g) zz(i) = zg(zid*nz+i)
  end do
  if(zid.eq.zpes-1 .and. periodic_z.eq.1) zz(nz+1) = 2.0*zz(nz)-zz(nz-1)
  !----------------------------------------
  call vertex_area_weights(xx, yy, zz, vert_area)
  do n = 1, vert_count
    i = vert_loc(1, n)
    k = vert_loc(3, n)
    tri_area(i,k) = tri_area(i,k)+vert_area(n)
!if(xid*nx+i.eq.10)    write(io,*)'ixg=10,i,k,vertarea',i,k,vert_area(n)
  end do
!test  do i=1,nx
!testwrite(io,*)'ixg',xid*nx+i,tri_area(i,1),tri_area(i,nz/2)
!test  enddo
  !Count one for every time-step data read in.
  timecount = timecount + 1

  !----------------------------------------
  ! Compute fsd based on mean gradc everywhere

  call calc_y_accum_numden (gradC, ifcond, wt, fsd2_num, fsd2_den)

  !----------------------------------------
  ! Compute mixture fraction and Sd statistics
  call calc_y_accum_numden (gradC*zetafield, ifcond, wt, zeta_num, zeta_den)
  call calc_y_accum_numden (gradC*zetafield*zetafield, ifcond, wt, zsqu_num, zsqu_den)
  VelLam(:,:,:)=1.8/a_ref*0.4261/rho_ref
  call calc_y_accum_numden (gradC*VelLam, ifcond, wt, Slam_num, Slam_den)
  
else
  !----------------------------------------
  ! Finishing step
!  call ghostzone_real(tri_area, nx+1, ny+1, 1, 1, 0, 1, 0, 0, 0)
  call ghostzone_real(tri_area, nx+1, 1, nz+1, 1, 0, 0, 0, 1, 0)
  if(xid>0 .or. periodic_x .eq. 1) &
    tri_area(1,1:nz) = tri_area(1,1:nz)+tri_area(0,1:nz)
  if(zid>0 .or. periodic_z .eq. 1) &
    tri_area(1:nx,1) = tri_area(1:nx,1)+tri_area(1:nx,0)
!  fsd1_num(1:nx, 1:nz) = tri_area(1:nx, 1:nz)
  call mpi_allreduce(tri_area, fsd1_num, nx*nz, MPI_REAL8, MPI_SUM, ycomm, ierr)
  fsd1_den = real(timecount)
  deltay=min_grid_y/l_ref
  !----------------------------------------
  ! Divide triangle areas by volume to get FSD
  do k = 1, nz
    do i = 1, nx
      if(xid .eq. xpes-1 .and. i .eq. nx) cycle
      if(zid .eq. zpes-1 .and. k .eq. nz) cycle
      fsd1_num(i,k) = fsd1_num(i,k)/(xg(xid*nx+i+1)-xg(xid*nx+i))
      fsd1_num(i,k) = fsd1_num(i,k)/deltay
      fsd1_num(i,k) = fsd1_num(i,k)/(zg(zid*nz+k+1)-zg(zid*nz+k)) 
    end do
  end do
  !----------------------------------------
  !Finishing step. Write the data out.
  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing FSD_XZ'

  if (myid == 0) then
    filename = '../post/tecplot/fsdxdir.tec'
    open(unit=78, file=trim(filename), status='unknown')

! note that these variables have been integrated across the y-direction 
! which is why there is an extra length in the dimensions
    write(78,*) 'title = crossstreamFSD'
    write(78,*) 'variables = '
    write(78,*) '"x [mm]"'
    write(78,*) '"RR_ave '//trim(species_name(4))//' [m/s]"'
    write(78,*) '"RR_ave Cprog [m/s]"'
    write(78,*) '"FSD-Triangle [-]"'
    write(78,*) '"FSD-|gC| [-]"'
    write(78,*) '"Zeta_gradc [-]"'
    write(78,*) '"Zeta2_gradc []"'
    write(78,*) '"rhoSlam_gradc [kg/m^2/s]"'
    write(78,*) '"IO1 [-]"'
    write(78,*) '"IO2 [-]"'
    write(78, 2)  nx_g
  end if
  !----------------------------------------
  call write_Xfld(78,x(:),l_ref*1e3)
  !----------------------------------------
  rro2_num = 0.5*rro2_num*deltay/real(timecount) ! assumes uniform y grid in region containing flame.
  rrcp_num = 0.5*rrcp_num*deltay/real(timecount) ! assumes uniform y grid in region containing flame.
  fsd1_num = 0.5*fsd1_num*deltay/real(timecount) ! assumes uniform y grid in region containing flame.
  fsd2_num = 0.5*fsd2_num*deltay/real(timecount) ! assumes uniform y grid in region containing flame.
  zeta_num = 0.5*zeta_num*deltay/real(timecount) ! assumes uniform y grid in region containing flame.
  zsqu_num = 0.5*zsqu_num*deltay/real(timecount) ! assumes uniform y grid in region containing flame.
  Slam_num = 0.5*Slam_num*deltay/real(timecount) ! assumes uniform y grid in region containing flame.

  
  call print_xint(rro2_num, 1,nz_g, rr_ref*l_ref,rro2_xint)
  call print_xint(rrcp_num, 1,nz_g, rr_ref*l_ref,rrcp_xint)
  call print_xint(fsd1_num, 1,nz_g, 1.0,fsd1_xint)  ! multiply by 0.5 to get one side of the flame only
  call print_xint(fsd2_num, 1,nz_g, 1.0,fsd2_xint)  ! multiply by 0.5 to get one side of the flame only  

  call just_xint(zeta_num, 1,nz_g, zeta_xint)
  call just_xint(zsqu_num, 1,nz_g, zsqu_xint)
  call just_xint(Slam_num, 1,nz_g, Slam_xint)
  
  zeta_xint=zeta_xint/max(fsd2_xint,1.0D-10)
  zsqu_xint=zsqu_xint/max(fsd2_xint,1.0D-10)
  zsqu_xint=(abs(zsqu_xint-zeta_xint**2))**0.5
  Slam_xint=Slam_xint/max(fsd2_xint,1.0D-10)

  IO1X=rrcp_xint/max(fsd1_xint,1.0D-10)/max(Slam_xint,1.0D-10)
  IO2X=rrcp_xint/max(fsd2_xint,1.0D-10)/max(Slam_xint,1.0D-10)
  
  call write_Xfld(78,zeta_xint(:),1.0)
  call write_Xfld(78,zsqu_xint(:),1.0)
  call write_Xfld(78,Slam_xint(:),rho_ref*a_ref)
  call write_Xfld(78,IO1X(:),      1.0)
  call write_Xfld(78,IO2X(:),      1.0)

  if(myid == 0) then
    close(78)
    write(io,*) 'Finished FSD_XZ'
  end if
end if

1 format(' zone t= "',1pe9.3,'", i=',i5,i' f=block')
2 format(' zone t= ty_ave" i=',i5,', f=block')

return
contains
!----------------------------------------------------------------------
subroutine print_xint(num, iz1,iz2, ref, intout)
implicit none
real, dimension(nx, nz), intent(inout) :: num
integer, intent(in) :: iz1,iz2
real, dimension(nx), optional, intent(out) :: intout
real, intent(in) :: ref
real, dimension(nx, nz) :: den
real, dimension(nx) :: sumz

!Apply window averaging
call calc_XZfld_Xmovsum(num, window)
den(:,:)=1.0
call calc_XZfld_Xmovsum(den, window)
num=num/den

!Apply averaging over z-direction
do i=1,nx
call calc_sum_zrange(num(i,:),sumz(i),iz1,iz2)
sumz(i)=sumz(i)/real(1+iz2-iz1)
enddo !nx

call write_Xfld(78,sumz,ref)

if(present(intout)) intout = sumz


return
end subroutine print_xint
!--------------------------------------------------------------------
subroutine just_xint(num, iz1,iz2, intout)
implicit none
real, dimension(nx, nz), intent(inout) :: num
integer, intent(in) :: iz1,iz2
real, dimension(nx), optional, intent(out) :: intout
real, dimension(nx, nz) :: den
real, dimension(nx) :: sumz

!Apply window averaging
call calc_XZfld_Xmovsum(num, window)
den(:,:)=1.0
call calc_XZfld_Xmovsum(den, window)
num=num/den

!Apply averaging over z-direction
do i=1,nx
call calc_sum_zrange(num(i,:),sumz(i),iz1,iz2)
sumz(i)=sumz(i)/real(1+iz2-iz1)
enddo !nx

if(present(intout)) intout = sumz
return
end subroutine just_xint

end subroutine calc_FSD_XZ

!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_consumption(io, finish)
use topology_m, only: myid
use chemkin_m, only: reaction_rate, species_name
use mixfrac_m
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

integer, parameter :: nbinc = 50
real, parameter :: binc_start = 0.0, binc_del = 0.02
real, save, dimension(0:nbinc) :: binc_val, binphi_val

real, save, allocatable, dimension(:,:) :: RR_num, RR_den
real, save, allocatable, dimension(:,:,:) :: C_num, C_den, Z_num, Z_den
real, allocatable, dimension(:,:) :: A_num, A_den

logical, dimension(nx,ny,nz) :: ifcond, c_cond
real, dimension(nx,ny,nz) :: wt
real, dimension(nx,ny,nz) :: Cprog, hr, mag_gradC
real, dimension(nx,ny,nz,3) :: normal
real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion
real, dimension(nx,ny,nz,n_spec) :: ysp
real, dimension(nx,ny,nz) :: rr_c, YU, YB, t_dummy
real, dimension(nx,ny,nz) :: rhoU
real rho0,cval_in,zval_in, press_in   
real c_trigger, deltx, deltz
real, dimension(ny) :: delty
real zst,z1,z2

integer n, m, i,j,k,l

if(.not.initialized) then

  if(n_spec.eq.15)then
    z1=0.0233
    z2=0.055066
  else!if(n_spec.eq.29)then
    z1=0.0
    z2=0.0784
!  else
!    z1=0.0
!    z2=1.0
  endif
  zst=0.055066
 
  if(myid .eq. 0) write(io,*) 'Initializing calc_consumption'
  do n = 0, nbinc
    binc_val(n) = binc_start + binc_del*real(n)
    binphi_val(n)=z1+binc_val(n)*(z2-z1)
    binphi_val(n)=binphi_val(n)*(1.0-zst)/zst/(1.0-binphi_val(n))
  end do

  allocate(RR_num(nx,1:nbinc), RR_den(nx,1:nbinc))
  allocate(c_num(nx,ny,nz), c_den(nx,ny,nz))
  allocate(z_num(nx,ny,nz), z_den(nx,ny,nz))
  RR_num = 0.0; RR_den = 0.0;
  c_num  = 0.0; c_den  = 0.0;
  z_num  = 0.0; z_den  = 0.0;
  initialized = .true.
  call allocate_mixFrac_arrays(1)
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
  call calc_heat_release(rr_r, hr)
  call calc_mag_grad_and_normal &
    (Cprog, (/ 0.0, -1.0, 0.0/), .true., mag_gradC, normal)
  call specToMixFr(yspecies)
  call mixFracToPhi(mixFrac, phi)
 
  t_dummy=0.0  !t_dummy is used as the first argument to input the progress variable value = unburned
  call zclookup(t_dummy,zetafield,ysp,t_dummy,io)
  YU=ysp(:,:,:,4)
  t_dummy=1.0  !t_dummy is used as the first argument to input the progress variable value = burned
  call zclookup(t_dummy,zetafield,ysp,t_dummy,io)
  YB=ysp(:,:,:,4)

  do k=1,nz
  do j=1,ny
  do i=1,nx
   cval_in=cprog(i,j,k)
   zval_in=zetafield(i,j,k)
   press_in=pressure(i,j,k)
   call calc_zrho0(rho0,cval_in,zval_in,press_in,io)
   rhoU(i,j,k)=rho0
!declare rhoU,rho0,cval_in,zval_in, press_in
  enddo
  enddo
  enddo
 
  deltx=x(2)-x(1)
  do j=1,ny
    delty(j)=0.5*(y(min(ny,j+1))-y(max(j-1,1)))
  enddo
  deltz=z(2)-z(1)
 
  rr_c=rr_r(:,:,:,4)/(YB-YU+1.0e-5)/rhoU    ! divide by the unburned density/q(:,:,:,1,4)
  do j=1,ny
  rr_c(:,j,:)=rr_c(:,j,:)*deltx*delty(j)*deltz
  enddo

  if(myid.eq.0)write(io,*)'rr_c',rr_c(1,1,1),'rr,YB,YU,rhoU',rr_r(1,1,1,4),YB(1,1,1),YU(1,1,1),rhoU(1,1,1)
  if(myid.eq.0)write(io,*)'cval',cprog(1,1,1),'z',zetafield(1,1,1),'pr',pressure(1,1,1),'rho_ref',rho_ref

  wt = 1.0
  do n = 1, nbinc
    !----------------------------------------
    where(Zetafield .gt. binc_val(n-1) .and.  &
          Zetafield .le. binc_val(n)          &
     )
      ifcond = .true.
    elsewhere 
      ifcond = .false.
    endwhere
    !----------------------------------------
      call calc_yz_accum_numden &
        (rr_c(:,:,:), ifcond, wt, RR_num(:,n), RR_den(:,n))

  enddo

  c_num=c_num+cprog
  c_den=c_den+1.0
  z_num=z_num+zetafield
  z_den=z_den+1.0
 
else !Finishing step
  allocate(A_num(nx,1:nbinc), A_den(nx,1:nbinc))
  !----------------------------------------
  !Apply the moving window average
  do l = 1, nbinc
    call calc_Xfld_Xmovsum(RR_num(:,l), window)
    call calc_Xfld_Xmovsum(RR_den(:,l), window)
  end do

! find the average c field:

  do k=1,nz
  do j=1,ny
   call calc_Xfld_Xmovsum(c_num(:,j,k), window)
   call calc_Xfld_Xmovsum(c_den(:,j,k), window)
   call calc_Xfld_Xmovsum(z_num(:,j,k), window)
   call calc_Xfld_Xmovsum(z_den(:,j,k), window)
   enddo
  enddo

  where(c_den .gt. stat_small)
    c_num = c_num/c_den
  elsewhere
    c_num = 0.0
  end where

  where(z_den .gt. stat_small)
    z_num = z_num/z_den
  elsewhere
    z_num = 0.0
  end where
 
! there will need to be an mpi reduction at some stage.

! take a conditional summation of points, conditional on
! the c_num field passing across 0.5 in the y direction, and
! on the value of z_num.
 
 
  wt = 1.0

    c_trigger = 0.5
    c_cond = .false.
    ifcond = .false.

    do j=2,ny-1
      where((C_num(:,j-1,:).lt.c_trigger .and.  &
             C_num(:,j+1,:).gt.c_trigger )      &
      .or.  (C_num(:,j-1,:).gt.c_trigger .and.  &
             C_num(:,j+1,:).lt.c_trigger ) )
       c_cond(:,j,:) = .true.
      elsewhere
       c_cond(:,j,:) = .false.
      endwhere
    enddo
 
  do n = 1, nbinc
    !----------------------------------------
    where(Z_num .gt. binc_val(n-1) .and.  &
          Z_num .le. binc_val(n)          &
     )
      ifcond = .true.
    elsewhere 
      ifcond = .false.
    endwhere

    ifcond=ifcond.and.c_cond
    !----------------------------------------
! what is the area of every vertex? assume the mean flame brush is
! approximately normal to the y axis, and dx and dz are uniform.

      deltx=x(2)-x(1)
      deltz=z(2)-z(1)
      t_dummy=deltx*deltz
    
      call calc_yz_accum_numden &
        (t_dummy(:,:,:), ifcond, wt, A_num(:,n), A_den(:,n))

  enddo
  
  do l = 1, nbinc
    call calc_Xfld_Xmovsum(A_num(:,l), window)
    call calc_Xfld_Xmovsum(A_den(:,l), window)
  end do                                                 
 
  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing calc_consumption'

  call write_data(0.25)
  call write_data(0.50)
  call write_data(0.75)

end if !Finish

contains
  !----------------------------------------
  subroutine write_data(b)
  implicit none
  real, intent(in) :: b
  integer i, il

  character loc_ext*4, filename*100
  character xiid_ext*2

  i = int(nx_g*b)
  if(i <=xid*nx .or. i>(xid+1)*nx) return
  if(yz_id .ne. 0) return
  il = i - xid*nx

  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing calc_c_spcs'

  write(loc_ext,'(F4.2)') b
  filename = '../post/tecplot/cons_spd.'//trim(loc_ext)//'.tec'
  open(unit=78, file=trim(filename), status='unknown')

  write(78,*) 'title = c_spcs'
  write(78,*) 'variables = '
  write(78,*) '"Mixfr"'
  write(78,*) '"Phi"'
  write(78,*) '"Sc [m/s]"'
  !----------------------------------------------------------------------
  write(78, 3)  b, nbinc
  write(78,9) (0.5*(binc_val(n-1)+binc_val(n)), n = 1, nbinc)
  write(78,9) (0.5*(binphi_val(n-1)+binphi_val(n)), n = 1, nbinc)
  call print_avg(rr_num(il,:), a_num(il,:), a_ref)
  !----------------------------------------------------------------------

  close(78)

3 format(' zone t= "Sc_prog-',F4.2,'", i=',i5,', f=block')
9 format(10(1pe12.5,1x))

  end subroutine write_data

  !----------------------------------------
  subroutine print_avg(num, den, ref)
  implicit none
  real, intent(in), dimension(1:nbinc) :: num, den
  real, intent(in) :: ref
  real :: ave(1:nbinc)
  integer n
  
  where(den .gt. stat_small) 
     ave = num/den 
  elsewhere
     ave = 0.0
  end where
  write(78,9) (ave(n)*ref, n = 1, nbinc)
  
  9 format(10(1pe12.5,1x))
  return
  end subroutine print_avg
  
end subroutine calc_consumption
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_dissplot(io, finish)             
use topology_m, only: myid, gcomm
implicit none
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

real, parameter :: Cval = 0.5, Cdel = 0.05
real, parameter :: Zval = 0.5, Zdel = 0.05

integer, parameter :: nbinc = 20
integer, parameter :: nbinz = 20

integer, parameter :: nbinzz = 40
integer, parameter :: nbincc = 40

real, parameter :: binc_start = 0.0, binc_del = 0.05
real, parameter :: binz_start = 0.0, binz_del = 0.05
real, parameter :: bincc_start = -1.0, bincc_del = 0.2
real, parameter :: binzz_start = -1.0, binzz_del = 0.2
real, save, dimension(0:nbinc) :: binc_val
real, save, dimension(0:nbinz) :: binz_val
real, save, dimension(0:nbincc) :: bincc_val
real, save, dimension(0:nbinzz) :: binzz_val

real, save, dimension(1:nbinc,1:nbinz) :: zzdiss_num, zzdiss_den
real, save, dimension(1:nbinc,1:nbinz) :: zcdiss_num, zcdiss_den
real, save, dimension(1:nbinc,1:nbinz) :: ccdiss_num, ccdiss_den
real, save, dimension(1:nbinc,1:nbinz) :: zcpdf_num , zcpdf_den
real, save, dimension(1:nbinc,1:nbinz) :: coszc_num, coszc_den

real, save, dimension(1:nbinz) :: zpdf_num, zpdf_den
real, save, dimension(1:nbinzz,1:nbinz) :: zzpdf_num, zzpdf_den
real, save, dimension(1:nbincc,1:nbinc) :: ccpdf_num, ccpdf_den

real, save :: zzmean_num, zzmean_den, zzsqu_num, zzsqu_den
real, save :: ccmean_num, ccmean_den, ccsqu_num, ccsqu_den

!real, save, dimension(1:nbinc) :: gradC_num, gradC_den

!esr integer, parameter :: nbingradc = 40   !what does this do?
!esr real, save, dimension(0:nbingradc, 1:nbinc) :: bingradc_val
!esr real, save, dimension(0:nbingradc+1, 1:nbinc) :: pdf_gradc

!esr real, dimension(1:nbinc) :: gradC_ave
!esr real, dimension(0:nbingradc+1, 1:nbinc) :: cumpdf_gradc

real, dimension(0:nbinc+1,1:nbinz) :: cumpdf_ZC

logical, dimension(nx,ny,nz) :: ifZcond
logical, dimension(nx,ny,nz) :: ifCcond
logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx,ny,nz) :: wt,rho

real, dimension(nx,ny,nz) :: ZZdiss, ZCdiss, CCdiss, CosZC
real, dimension(nx,ny,nz) :: logZZdiss, logCCdiss
real, dimension(nx,ny,nz) :: Cprog   !, mag_gradC
real, dimension(nx) :: num, den
real pdf_sum, prbl
character*100 filename
character*2 xid_ext
integer m, n, l

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_dissplot'
  do n = 0, nbinc
    binc_val(n) = binc_start + binc_del*real(n)
  end do
  do n = 0, nbinz
    binz_val(n) = binz_start + binz_del*real(n)
  enddo
  
! can I give zzpdf and ccpdf a log scale?

  do n = 1, nbinzz
    binzz_val(n) = 0.0 + binzz_del*real(n)
  end do
  do n = 1, nbincc
    bincc_val(n) = 0.0 + bincc_del*real(n)
  end do
 
!esr  ! Initialized grad_c bins based on grad_c min/max at first time
  
  call calculate_progvar(Cprog,io)
! the call to progvar also sets zetafield
  call calc_zcdiss(ZZdiss,CCdiss,ZCdiss,io)
  call calc_CosZC(CosZC,io)
  
  
!esr  call calc_mag_grad(Cprog, mag_gradC)

!esr  do n = 1, nbinc
!    where(Cprog .gt. binc_val(n-1) .and. &
!          Cprog .le. binc_val(n) )
!      ifcond = .true.
!    elsewhere 
!      ifcond = .false.
!    endwhere
!    call make_bins(mag_gradC, ifcond, gcomm, nbingradc, bingradc_val(:,n))
!esr    prbl = 119.0 - 272.0*0.25*(binc_val(n-1)+binc_val(n)-1.0)**2
!esr    call make_bins_lmts(0.0, prbl, nbingradc, bingradc_val(:,n))
!esr  end do

!esr  pdf_gradc = 0.0
!esr  gradC_num = 0.0; gradC_den = 0.0;

zzdiss_num = 0.0; zzdiss_den = 0.0
zcdiss_num = 0.0; zcdiss_den = 0.0
ccdiss_num = 0.0; ccdiss_den = 0.0
coszc_num  = 0.0; coszc_den  = 0.0
zcpdf_num = 0.0 ; zcpdf_den  = 0.0

zzpdf_num = 0.0; zzpdf_num = 0.0
ccpdf_num = 0.0; ccpdf_num = 0.0

zzmean_num = 0.0; zzmean_den = 0.0
ccmean_num = 0.0; ccmean_den = 0.0
zzsqu_num  = 0.0; zzsqu_den  = 0.0
ccsqu_num  = 0.0; ccsqu_den  = 0.0

  initialized = .true.
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
  call calc_zcdiss(ZZdiss,CCdiss,ZCdiss,io)
  call calc_CosZC(CosZC,io)
  logZZdiss=log(ZZdiss)
  logCCdiss=log(CCdiss)
    
!esr  call calc_mag_grad(Cprog, mag_gradC)
  rho = q(:,:,:,4,1)  
  wt = 1.0
   
  do n = 1, nbinc
  do m = 1, nbinz
    where(Cprog .gt. binc_val(n-1) .and. &
          Cprog .le. binc_val(n) )
      ifCcond = .true.
    elsewhere 
      ifCcond = .false.
    endwhere
    
    where(Zetafield .gt. binz_val(m-1) .and. &
          Zetafield .le. binz_val(m) )
      ifZcond = .true.
    elsewhere
      ifZcond = .false.
    endwhere
    ifcond = ifCcond.and.ifZcond
!esr    !----------------------------------------
!esr    !On each processor, find the pdf of grad_c
!esr    !for the yz-plane corresponding to i = nx. 
!esr    call calc_pdf_addtl(ny*nz, &
!esr           reshape(mag_gradC(nx,:,:), (/ny*nz/) ), &
!esr           reshape(ifcond(nx,:,:), (/ny*nz/) ), &
!esr           reshape(wt(nx,:,:), (/ny*nz/) ), &
!esr           nbingradc, bingradc_val(:,n), yz_comm, pdf_gradc(:,n) )
!esr    !----------------------------------------
!esr    ! Calculate conditional mean grad_c
!esr    ! Again calculate only for i=nx.
!esr    call calc_yz_accum_numden (mag_gradC, ifcond, wt, num, den)
!esr    gradc_num(n) = gradc_num(n) + num(nx);
!esr    gradc_den(n) = gradc_den(n) + den(nx);

! Calculate double conditional mean ZZ, CC, ZC
     call calc_yz_accum_numden(ZZdiss,ifcond,rho,num,den)
     ZZdiss_num(n,m) = ZZdiss_num(n,m) + num(nx);
     ZZdiss_den(n,m) = ZZdiss_den(n,m) + den(nx);
     call calc_yz_accum_numden(CCdiss,ifcond,rho,num,den)
     CCdiss_num(n,m) = CCdiss_num(n,m) + num(nx);
     CCdiss_den(n,m) = CCdiss_den(n,m) + den(nx);
     call calc_yz_accum_numden(ZCdiss,ifcond,rho,num,den)
     ZCdiss_num(n,m) = ZCdiss_num(n,m) + num(nx);
     ZCdiss_den(n,m) = ZCdiss_den(n,m) + den(nx);
     call calc_yz_accum_numden(rho,ifcond,wt,num,den)
     ZCpdf_num(n,m) = ZCpdf_num(n,m) + num(nx);
     ZCpdf_den(n,m) = ZCpdf_den(n,m) + den(nx);
     call calc_yz_accum_numden(coszc,ifcond,rho,num,den)
     coszc_num(n,m) = coszc_num(n,m) + num(nx);
     coszc_den(n,m) = coszc_den(n,m) + den(nx);
    
  end do
  end do

!  do m = 1, nbinz
    where(Zetafield .gt. 0.02 .and. &
          Zetafield .le. 0.98 )
      ifZcond = .true.
    elsewhere
      ifZcond = .false.
    endwhere
    do n = 1, nbinzz
    where(logZZdiss .gt. binzz_val(n-1) .and. &
          logZZdiss .le. binzz_val(n)         &
!         .and. Cprog .gt. (Cval-Cdel)     .and. &
!          Cprog .le. (Cval+Cdel)                &
          )
      ifCcond = .true.
    elsewhere
      ifCcond = .false.
    endwhere
    ifcond = ifZcond .and. ifCcond
               
      call calc_yz_accum_numden(wt, ifcond,wt,num,den)
      ZZpdf_num(n,m) = ZZpdf_num(n,m) + num(nx)
      ZZpdf_den(n,m) = ZZpdf_den(n,m) + den(nx)
      call calc_yz_accum_numden(logZZdiss,ifcond,wt,num,den)
      zzmean_num = zzmean_num + num(nx)
      zzmean_den = zzmean_den + den(nx)
      call calc_yz_accum_numden(logZZdiss*logZZdiss,ifcond,wt,num,den)
      zzsqu_num = zzsqu_num + num(nx)
      zzsqu_den = zzsqu_den + den(nx)
      
    end do
!  end do

  do m = 1, nbinz
    where(Zetafield .gt. binz_val(m-1) .and. &
          Zetafield .le. binz_val(m) )
      ifZcond = .true.
    elsewhere
      ifZcond = .false.
    endwhere

    call calc_yz_accum_numden(wt, ifZcond,rho,num,den)
    Zpdf_num(m) = Zpdf_num(m) + num(nx)
    Zpdf_den(m) = Zpdf_den(m) + den(nx)
    
  end do

!  do m = 1, nbinc
!    where(Cprog .gt. binc_val(m-1) .and. &
!          Cprog .le. binc_val(m) )
!      ifCcond = .true.
!    elsewhere
!      ifCcond = .false.
!    endwhere

    where(Cprog .gt. 0.02 .and. &
          Cprog .le. 0.98 )
      ifCcond = .true.
    elsewhere
      ifCcond = .false.
    endwhere
    
    do n = 1, nbincc
    where(logCCdiss .gt. bincc_val(n-1) .and. &
          logCCdiss .le. bincc_val(n)     & 
!    .and. Zetafield .gt. (Zval-Zdel)  &
!          Zetafield .le. (Zval+Zdel)  &
           )
      ifZcond = .true.
    elsewhere
      ifZcond = .false.
    endwhere
    ifcond = ifZcond .and. ifCcond

    call calc_yz_accum_numden(wt,ifcond,wt,num,den)
    CCpdf_num(n,:) = CCpdf_num(n,:) + num(nx)
    CCpdf_den(n,:) = CCpdf_den(n,:) + den(nx)
    call calc_yz_accum_numden(logccdiss,ifcond,wt,num,den)
    ccmean_num = ccmean_num + num(nx)
    ccmean_den = ccmean_den + den(nx)
    call calc_yz_accum_numden(logccdiss*logccdiss,ifcond,wt,num,den)
    ccsqu_num = ccmean_num + num(nx)
    ccsqu_den = ccsqu_den + den(nx)
        
    
    end do
!  end do
  
  
else !Finishing step
    
!esr  do n = 1, nbinc

!esr    !Normalize the pdf
!esr    pdf_sum = sum(pdf_gradc(:,n))
!esr    if(pdf_sum .gt. stat_small) pdf_gradc(:,n) = pdf_gradc(:,n)/pdf_sum
!esr    call calc_cumpdf_frompdf(nbingradc, pdf_gradc(:,n), cumpdf_gradc(:,n))

!esr  end do

! ZCpdf will be the Cpdf for each mixture fraction value.
! also find the ZZdiss pdf for each mf value

  do m = 1, nbinz

    pdf_sum = 0.0
    pdf_sum = sum(ZCpdf_num(:,m))
    if(pdf_sum .gt. stat_small) ZCpdf_num(:,m) = ZCpdf_num(:,m)/pdf_sum/binc_del !the division by binc_del is not right
    call calc_cumnorm_from_hist(nbinz, ZCpdf_num(:,m), cumpdf_ZC(:,m)) 

    pdf_sum = 0.0
    pdf_sum = sum(ZZpdf_num(:,m))
    if(pdf_sum .gt. stat_small) ZZpdf_num(:,m) = ZZpdf_num(:,m)/pdf_sum/binzz_del ! this assumes uniform spacings
 
  end do

! find the mixture fraction pdf

  pdf_sum = 0.0
  pdf_sum = sum(Zpdf_num(:))
  if(pdf_sum .gt. stat_small) Zpdf_num(:) = Zpdf_num(:)/pdf_sum/binz_del ! this assumes uniform spacings

! find the CCdiss pdf for each progress variable value

  do m = 1, nbinc

    pdf_sum = 0.0
    pdf_sum = sum(CCpdf_num(:,m))
    if(pdf_sum.gt. stat_small) CCpdf_num(:,m) = CCpdf_num(:,m)/pdf_sum/bincc_del  ! this assumes uniform spacings

  end do
 
!esr  !Find gradc_ave
!esr  where(gradc_den .lt. stat_small)
!esr    gradc_ave = 0.0
!esr  elsewhere
!esr    gradc_ave = gradc_num/gradc_den
!esr  end where

  where(ZZdiss_den .lt. stat_small)
    ZZdiss_num = 0.0
    CCdiss_num = 0.0
    ZCdiss_num = 0.0
    CosZC_num  = 0.0
  elsewhere
    ZZdiss_num = ZZdiss_num/ZZdiss_den
    CCdiss_num = CCdiss_num/CCdiss_den
    ZCdiss_num = ZCdiss_num/ZCdiss_den
    CosZC_num  = CosZC_num /CosZC_den
  end where

  zzmean_num = zzmean_num/zzmean_den
  ccmean_num = ccmean_num/ccmean_den
  zzsqu_num  = zzsqu_num/zzsqu_den
  ccsqu_num  = ccsqu_num/ccsqu_den

!find the unconditional mean and mean square of logZZdiss, logCCdiss

  !Write the data out.
  !For the pdfs each yz_id==0 writes out a tecplot contour file.
  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing zc_data'

  if (yz_id == 0) then
    write(xid_ext,'(I2.2)') xid
    filename = '../post/tecplot/zc_pdf.'//trim(xid_ext)//'.tec'
    open(unit=78, file=trim(filename), status='unknown')

    write(78,*) 'title = z_pdf'
    write(78,*) 'variables = '
    write(78,*) '"Z" "C"'
    write(78,*) '"f"'
    write(78,*) '"cumf"'
    write(78,*) '"ZZ"'
    write(78,*) '"CC"'
    write(78,*) '"ZC"'
    write(78,*) '"Cos"'
    write(78,*) '"Zpdf"'
    write(78, 2)  nbinc, nbinz

    do l = 1, nbinc
      write(78,9) (0.5*(binz_val(n-1)+binz_val(n)), n = 1, nbinz)
    end do

    do l = 1, nbinc
      write(78,9) (0.5*(binc_val(l-1)+binc_val(l)),n = 1, nbinz)
    end do

    do l = 1, nbinc
      do n = 1, nbinz
        write(78,9) zcpdf_num(l,n)
      end do
    end do

    do l = 1, nbinc
      do n = 1, nbinz
        write(78,9) cumpdf_ZC(l,n)
     end do
    end do                                       

    do l = 1, nbinc
      do n = 1, nbinz
        write(78,9) ZZdiss_num(l,n)/time_ref
      end do
    end do

    do l = 1, nbinc
      do n = 1, nbinz
        write(78,9) CCdiss_num(l,n)/time_ref
      end do
    end do

    do l = 1, nbinc                    
      do n = 1, nbinz
        write(78,9) ZCdiss_num(l,n)/time_ref
      end do
    end do

    do l = 1, nbinc
      do n = 1, nbinz
        write(78,9) CosZC_num(l,n)
      end do
    end do
    
    do l = 1, nbinc
      do n = 1, nbinz
        write(78,9) Zpdf_num(n)
      end do
    end do
    
! write the zzdiss pdf data    
    filename = '../post/tecplot/zzdiss_pdf.'//trim(xid_ext)//'.tec'
    open(unit=78, file=trim(filename), status='unknown')

    write(78,*) 'title = zz_pdf'
    write(78,*) 'variables = '
    write(78,*) '"Z" "zzdiss"'
    write(78,*) '"f"'
    write(78, 4)  nbinzz, nbinz

    do l = 1, nbinzz
      write(78,9) (0.5*(binz_val(n-1)+binz_val(n)), n = 1, nbinz)
    end do

    do l = 1, nbinzz
      write(78,9) (0.5*(binzz_val(l-1)+binzz_val(l)),n = 1, nbinz)
    end do

    do l = 1, nbinzz
      do n = 1, nbinz
        write(78,9) ZZpdf_num(l,n)
      end do
    end do

    do l = 1, nbinzz
      do n = 1, nbinz
        write(78,9) zzmean_num/time_ref
      end do
    end do

    do l = 1, nbinzz
      do n = 1, nbinz
        write(78,9) zzsqu_num/time_ref/time_ref
      end do
    end do

! write the ccdiss pdf data    
    filename = '../post/tecplot/ccdiss_pdf.'//trim(xid_ext)//'.tec'
    open(unit=78, file=trim(filename), status='unknown')

    write(78,*) 'title = cc_pdf'
    write(78,*) 'variables = '
    write(78,*) '"C" "zzdiss"'
    write(78,*) '"f"'
    write(78, 4)  nbincc, nbinc

    do l = 1, nbincc
      write(78,9) (0.5*(binc_val(n-1)+binc_val(n)), n = 1, nbinc)
    end do

    do l = 1, nbincc
      write(78,9) (0.5*(bincc_val(l-1)+bincc_val(l)),n = 1, nbinc)
    end do

    do l = 1, nbincc
      do n = 1, nbinc
        write(78,9) CCpdf_num(l,n)
      end do
    end do

    do l = 1, nbinzz
      do n = 1, nbinz
        write(78,9) ccmean_num/time_ref
      end do
    end do

    do l = 1, nbinzz
      do n = 1, nbinz
        write(78,9) ccsqu_num/time_ref/time_ref
      end do
    end do

!esr      write(78,9) (0.5*(binc_val(n-1)+binc_val(n)), n = 1, nbinc)
!esr    end do
!esr
!esr    do l = 0, nbingradc+1
!esr      if( l .eq. 0) then
!esr        write(78,9) (bingradc_val(l,n)/l_ref/1e3, n = 1, nbinc)
!esr      else if (l .eq. nbingradc+1) then
!esr        write(78,9) (bingradc_val(l-1,n)/l_ref/1e3, n = 1, nbinc)
!esr      else
!esr        write(78,9) (0.5*(bingradc_val(l-1,n)+bingradc_val(l,n))/l_ref/1e3, &
!esr                     n = 1, nbinc)
!esr      end if
!esr    end do
!esr
!esr    do l = 0, nbingradc+1
!esr      do n = 1, nbinc
!esr        write(78,9) pdf_gradc(l,n)
!esr      end do
!esr    end do
!esr
!esr    do l = 0, nbingradc+1
!esr      do n = 1, nbinc
!esr        write(78,9) cumpdf_gradc(l,n)
!esr      end do
!esr    end do
    
  end if ! YZ_id == 0
!esr  !----------------------------------------
!esr  if(myid==0) write(io,*) 'Writing surface means conditioned on C'
!esr  if (yz_id == 0) then
!esr    write(xid_ext,'(I2.2)') xid
!esr    filename = '../post/tecplot/c_ave.'//trim(xid_ext)//'.tec'
!esr    open(unit=78, file=trim(filename), status='unknown')
!esr
!esr    write(78,*) 'title = c_ave'
!esr    write(78,*) 'variables = '
!esr    write(78,*) '"C"'
!esr    write(78,*) '"mag_gradC[1/mm]"'
!esr    write(78, 3)  nbinc
!esr
!esr    write(78,9) (0.5*(binc_val(n-1)+binc_val(n)), n = 1, nbinc)
!esr    write(78,9) (gradC_ave(n)/l_ref/1e3, n = 1, nbinc)
!esr  end if ! yz_id ==0

end if !Finish

2 format(' zone t= zc_pdf" i=',i5,', j=',i5,', f=block')
3 format(' zone t= c_ave" i=',i5,', f=block')
4 format(' zone t= zz_pdf" i=',i5,', j=',i5,', f=block')
5 format(' zone t= cc_pdf" i=',i5,', j=',i5,', f=block')
6 format(' zone t= z_pdf" i=',i5,', j=',i5,', f=block')

9 format(10(1pe12.5,1x))
end subroutine calc_dissplot

!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine calc_dcond_spcs(io, finish)             
use topology_m, only: myid, gcomm 
use chemkin_m, only: reaction_rate, species_name
implicit none                                                                                                               
integer, intent(in) :: io
logical, intent(in) :: finish

logical, save :: initialized=.false.

integer, parameter :: nbinc = 20
integer, parameter :: nbinz = 20
real, parameter :: binc_start = 0.0, binc_del = 0.05
real, parameter :: binz_start = 0.0, binz_del = 0.05
real, save, dimension(0:nbinc) :: binc_val
real, save, dimension(0:nbinz) :: binz_val

!real, save, dimension(1:nbinc,1:nbinz) :: zzdiss_num, zzdiss_den

!esr real, save, dimension(1:nbinc, 1:nbinz, n_spec) :: Y_num, Y_den, RR_num, RR_den
!esr real, save, dimension(1:nbinc, 1:nbinz) :: hr_num, hr_den, T_num, T_den

real, save, allocatable, dimension(:,:,:) :: Y_num, Y_den, RR_num, RR_den
real, save, allocatable, dimension(:,:) :: hr_num, hr_den, T_num, T_den

logical, dimension(nx,ny,nz) :: ifZcond
logical, dimension(nx,ny,nz) :: ifCcond
logical, dimension(nx,ny,nz) :: ifcond
real, dimension(nx,ny,nz) :: wt,rho
real, dimension(nx,ny,nz) :: hr
real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion

real, dimension(nx,ny,nz) :: ZZdiss
real, dimension(nx,ny,nz) :: Cprog   !, mag_gradC
real, dimension(nx) :: num, den
!real pdf_sum, prbl
character*100 filename
character*2 xid_ext
integer m, n, l

if(.not.initialized) then
  if(myid .eq. 0) write(io,*) 'Initializing calc_dcond_spcs, only processing temp, no species'
!esr all the lines for species and species_rr are tagged spec
  do n = 0, nbinc
    binc_val(n) = binc_start + binc_del*real(n)
  end do
  do n = 0, nbinz
    binz_val(n) = binz_start + binz_del*real(n)
  enddo

  call calculate_progvar(Cprog,io)
      
!spec  allocate(Y_num(1:nbinc,1:nbinz,n_spec), Y_den(1:nbinc,1:nbinz,n_spec))
!spec  allocate(RR_num(1:nbinc,1:nbinz,n_spec), RR_den(1:nbinc,1:nbinz,n_spec))
  allocate(hr_num(1:nbinc,1:nbinz), hr_den(1:nbinc, 1:nbinz))
  allocate(T_num(1:nbinc,1:nbinz), T_den(1:nbinc,1:nbinz))
!spec  Y_num = 0.0; Y_den = 0.0;
!spec  RR_num = 0.0; RR_den = 0.0;
  hr_num = 0.0; hr_den = 0.0;
  T_num = 0.0; T_den = 0.0;

  initialized = .true.
end if

if(.not. finish) then
  call calculate_progvar(Cprog,io)
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif  
  call calc_heat_release(rr_r, hr)

  rho = q(:,:,:,4,1)  
  wt = 1.0
   
  do n = 1, nbinc
  do m = 1, nbinz
    where(Cprog .gt. binc_val(n-1) .and. &
          Cprog .le. binc_val(n) )
      ifCcond = .true.
    elsewhere 
      ifCcond = .false.
    endwhere
    
    where(Zetafield .gt. binz_val(m-1) .and. &
          Zetafield .le. binz_val(m) )
      ifZcond = .true.
    elsewhere
      ifZcond = .false.
    endwhere
    ifcond = ifCcond.and.ifZcond


! Calculate double conditional mean

!spec     do l=1,n_spec
!spec
!spec     call calc_yz_accum_numden(yspecies,ifcond,rho,num,den)
!spec     Y_num(n,m) = Y_num(n,m) + num(nx);
!spec     Y_den(n,m) = Y_den(n,m) + den(nx);
!spec
!spec     call calc_yz_accum_numden(rr_r(:,:,:,l),ifcond,rho,num,den)
!spec     RR_num(n,m,l) = RR_num(n,m,l) + num(nx);
!spec     RR_den(n,m,l) = RR_den(n,m,l) + den(nx);
!spec
!spec     enddo

     call calc_yz_accum_numden(temp,ifcond,rho,num,den)
     T_num(n,m) = T_num(n,m) + num(nx);
     T_den(n,m) = T_den(n,m) + den(nx);

     call calc_yz_accum_numden(hr,ifcond,rho,num,den)
     hr_num(n,m) = hr_num(n,m) + num(nx);
     hr_den(n,m) = hr_den(n,m) + den(nx);
    
  end do
  end do

else !Finishing step
    
  where(T_den .lt. stat_small)
!spec    Y_num = 0.0
!spec    RR_num = 0.0
  
    T_num = 0.0
    hr_num = 0.0
  elsewhere
!spec    Y_num = Y_num/Y_den
!spec    RR_num = RR_num/RR_den
  
    T_num = T_num/T_den
    hr_num = hr_num/hr_den
  end where

  !Write the data out.
  !For the pdfs each yz_id==0 writes out a tecplot contour file.
  if(myid==0) write(io,*) '!----------------------------'
  if(myid==0) write(io,*) 'Writing dcond_spcs'

  if (yz_id == 0) then
    write(xid_ext,'(I2.2)') xid
    filename = '../post/tecplot/zc_spcs.'//trim(xid_ext)//'.tec'
    open(unit=78, file=trim(filename), status='unknown')

    write(78,*) 'title = dcond_spcs'
    write(78,*) 'variables = '
    write(78,*) '"Z" "C"'
    write(78,*) '"Temp"'
    write(78,*) '"HR"'
!spec    do m=1,n_spec
!spec      write(78,*) '"'//trim(species_name(m))//'"'
!spec    enddo
!spec    do m=1,n_spec
!spec      write(78,*) '"RR-'//trim(species_name(m))//'"'
!spec    enddo

    write(78, 2)  nbinc, nbinz

    do l = 1, nbinc
      write(78,9) (0.5*(binz_val(n-1)+binz_val(n)), n = 1, nbinz)
    end do

    do l = 1, nbinc
      write(78,9) (0.5*(binc_val(l-1)+binc_val(l)),n = 1, nbinz)
    end do

    do l = 1, nbinc
      do n = 1, nbinz
        write(78,9) T_num(l,n)*T_ref
      end do
    end do

    do l = 1, nbinc
      do n = 1, nbinz
        write(78,9) HR_num(l,n)* (-hr_ref)
      end do
    end do

!spec    do m = 1, n_spec
!spec    do l = 1, nbinc
!spec      do n = 1, nbinz
!spec        write(78,9) Y_num(l,n,m)
!spec      end do
!spec    end do
!spec    end do
!spec
!spec    do m = 1, n_spec
!spec    do l = 1, nbinc
!spec      do n = 1, nbinz
!spec        write(78,9) RR_num(l,n,m)
!spec      end do
!spec    end do
!spec    end do
 
  end if ! YZ_id == 0

end if !Finish

2 format(' zone t= dcond" i=',i5,', j=',i5,', f=block')
9 format(10(1pe12.5,1x))
end subroutine calc_dcond_spcs


!----------------------------------------------------------------------
!----------------------------------------------------------------------
end module bunsen_post_m

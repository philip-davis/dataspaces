#include "globalDefines.h"
!========================================================================================
  subroutine write_basic_tecplot_file(io,flag)
!========================================================================================
! routine writes various basic quantities for monitoring during run execution
! for runtime ("on-the-fly") diagnostics
! format is in tecplot blockdata format
! units are CGS except for:
!   - pressure (atm)
!   - heat release (non-dimensional)
!----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz, n_spec

  use reference_m, only : time_ref, l_ref, a_ref, t_ref
  use reference_m, only : rho_ref, p_ref, pres_atm, t_o, g_ref

  use grid_m, only : x, y, z

  use variables_m, only : q, u, temp, pressure, yspecies

  use chemkin_m, only : species_name, n_species
  use chemkin_m, only : reaction_rate  !routine reference

  use runtime_m, only : run_title, time, i_time, tstep

  use work_m, only : vort_mag => work1_1
  use work_m, only : hr => work1_2

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io    !output unit
  integer flag  !deletion flag

! local declarations

  integer i,j,k,L
  real l_conv_cgs, a_conv_cgs, rho_conv_cgs, t_conv_cgs, dil_conv_cgs, hr_conv_cgs
  real p_conv_atm, rr_conv_CGS

  real, dimension(nx,ny,nz,3) :: vort
  real, dimension(nx,ny,nz) :: dil
  real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion
  
  character*5 myid_ext
  character*9 time_ext
  character*100 filename
  character*100 tarcmd, tartgt, tarsrc
!----------------------------------------------------------------------------------------
! return if zero dimensions

  if((nx.eq.1).and.(ny.eq.1).and.(nz.eq.1)) then
    return
  else
    if(myid.eq.0) then
      write(io,100) 'writing tecplot files for: i_time = ',i_time,  &
                    ', time = ',time*time_ref,' (sec)'
    endif
  endif
!----------------------------------------------------------------------------------------
! set reference values in CGS

  l_conv_cgs=l_ref*100.0
  a_conv_cgs=a_ref*100.0
  t_conv_cgs=t_ref
  rho_conv_cgs=rho_ref*(1000.0/100.0**3)
  dil_conv_cgs=(a_ref/l_ref)
  p_conv_atm=p_ref/pres_atm
  hr_conv_cgs=1.0
  rr_conv_CGS = (rho_ref * a_ref) / (l_ref * 1.0e3)
!----------------------------------------------------------------------------------------
! calculate dilitation

  call computeDivergence(u,dil)

! calculate vorticity

  call calc_vorticity(vort,vort_mag,u)

! compute reaction rate
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
  call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif

! calculate heat release

  call calc_heat_release(rr_r,hr)
!----------------------------------------------------------------------------------------
! set processor extensions

  call set_file_extension_string(myid_ext,myid,5)
!----------------------------------------------------------------------------------------
! set filename and open files

  filename=myid_ext//'.plt'
  open(unit=78,file=trim(filename),status='unknown')
!----------------------------------------------------------------------------------------
! write title of file

  write(78,*) 'title = "',trim(filename),'"'
!----------------------------------------------------------------------------------------
! write variables header

  write(78,*) 'variables = '

! write coordinates labels

  if((nx.gt.1).and.(ny.eq.1).and.(nz.eq.1)) then
    write(78,*) '"x (cm)"'
  elseif((nx.eq.1).and.(ny.gt.1).and.(nz.eq.1)) then
    write(78,*) '"y (cm)"'
  elseif((nx.eq.1).and.(ny.eq.1).and.(nz.gt.1)) then
    write(78,*) '"z (cm)"'
  elseif((nx.gt.1).and.(ny.gt.1).and.(nz.eq.1)) then
    write(78,*) '"x (cm)"', '"y (cm)"'
  elseif((nx.gt.1).and.(ny.eq.1).and.(nz.gt.1)) then
    write(78,*) '"x (cm)"', '"z (cm)"'
  elseif((nx.eq.1).and.(ny.gt.1).and.(nz.gt.1)) then
    write(78,*) '"y (cm)"', '"z (cm)"'
  elseif((nx.gt.1).and.(ny.gt.1).and.(nz.gt.1)) then
    write(78,*) '"x (cm)"', '"y (cm)"', '"z (cm)"'
  endif

! write variables labels

  write(78,*) '"u [cm/s]"'
  write(78,*) '"v [cm/s]"'
  write(78,*) '"w [cm/s]"'
  write(78,*) '"rho [g/cm^3]"'
  write(78,*) '"T [K]"'
  write(78,*) '"P [atm]"'
  write(78,*) '"dil [1/s]"'
  write(78,*) '"vort_x [1/s]"'
  write(78,*) '"vort_y [1/s]"'
  write(78,*) '"vort_z [1/s]"'
  write(78,*) '"vort mag [1/s]"'
  write(78,*) '"heat release [non-dim]"'
  do L=1,n_species,1
    write(78,*) '"Y '//trim(species_name(L))//'"'
  enddo
  do L=1,n_species,1
    write(78,*) '"RR '//trim(species_name(L))//' [g/(s-cm^3)]"'
  enddo
!----------------------------------------------------------------------------------------
! write zone information

  if((nx.gt.1).and.(ny.eq.1).and.(nz.eq.1)) then
    write(78,1) nx
  elseif((nx.eq.1).and.(ny.gt.1).and.(nz.eq.1)) then
    write(78,1) ny
  elseif((nx.eq.1).and.(ny.eq.1).and.(nz.gt.1)) then
    write(78,1) nz
  elseif((nx.gt.1).and.(ny.gt.1).and.(nz.eq.1)) then
    write(78,2) nx, ny
  elseif((nx.gt.1).and.(ny.eq.1).and.(nz.gt.1)) then
    write(78,2) nx, nz
  elseif((nx.eq.1).and.(ny.gt.1).and.(nz.gt.1)) then
    write(78,2) ny, nz
  elseif((nx.gt.1).and.(ny.gt.1).and.(nz.gt.1)) then
    write(78,3) nx, ny, nz
  endif
!----------------------------------------------------------------------------------------
! write coordinate data

  if((nx.gt.1).and.(ny.eq.1).and.(nz.eq.1)) then
    write(78,9) (x(i)*l_conv_cgs, i=1,nx,1)
  elseif((nx.eq.1).and.(ny.gt.1).and.(nz.eq.1)) then
    write(78,9) (y(j)*l_conv_cgs, j=1,ny,1)
  elseif((nx.eq.1).and.(ny.eq.1).and.(nz.gt.1)) then
    write(78,9) (z(k)*l_conv_cgs, k=1,nz,1)
  elseif((nx.gt.1).and.(ny.gt.1).and.(nz.eq.1)) then
    write(78,9) ((x(i)*l_conv_cgs, i=1,nx),j=1,ny),  &
                ((y(j)*l_conv_cgs, i=1,nx),j=1,ny)
  elseif((nx.gt.1).and.(ny.eq.1).and.(nz.gt.1)) then
    write(78,9) ((x(i)*l_conv_cgs, i=1,nx),k=1,nz),  &
                ((z(k)*l_conv_cgs, i=1,nx),k=1,nz)
  elseif((nx.eq.1).and.(ny.gt.1).and.(nz.gt.1)) then
    write(78,9) ((y(j)*l_conv_cgs, j=1,ny),k=1,nz),  &
                ((z(k)*l_conv_cgs, j=1,ny),k=1,nz)
  elseif((nx.gt.1).and.(ny.gt.1).and.(nz.gt.1)) then
    write(78,9) (((x(i)*l_conv_cgs, i=1,nx),j=1,ny),k=1,nz),  &
                (((y(j)*l_conv_cgs, i=1,nx),j=1,ny),k=1,nz),  &
                (((z(k)*l_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  endif
!----------------------------------------------------------------------------------------
! write variable data

  write(78,9) (((u(i,j,k,1)*a_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((u(i,j,k,2)*a_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((u(i,j,k,3)*a_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((q(i,j,k,4,1)*rho_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((temp(i,j,k)*t_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((pressure(i,j,k)*p_conv_atm, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((dil(i,j,k)*dil_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((vort(i,j,k,1)*dil_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((vort(i,j,k,2)*dil_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((vort(i,j,k,3)*dil_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((vort_mag(i,j,k)*dil_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  write(78,9) (((hr(i,j,k)*hr_conv_cgs, i=1,nx),j=1,ny),k=1,nz)
  do L=1,n_species,1
    write(78,9) (((yspecies(i,j,k,L), i=1,nx),j=1,ny),k=1,nz)
  enddo
  do L=1,n_species,1
    write(78,9) (((rr_r(i,j,k,L)*rr_conv_CGS, i=1,nx),j=1,ny),k=1,nz)
  enddo
!----------------------------------------------------------------------------------------
! close files

  close(78)
!----------------------------------------------------------------------------------------
! delete any existing tarred tecplot files if flag = 1

  if((i_time.ne.0).and.(flag.eq.1)) then

    if(myid == 0) then

#if PC

        tarcmd = 'del ..\post\tecplot\'//trim(run_title)//'.*.tar'
        call execute_command( trim(tarcmd) )

#else

        tarcmd = 'rm ../post/tecplot/'//trim(run_title)//'.*.tar'

        call execute_command( trim(tarcmd) )

#endif

    endif

  endif
!----------------------------------------------------------------------------------------
! file compression

  call MPI_Barrier( gcomm, ierr )

  write(time_ext,'(1pe9.3)') time*time_ref       !set time stamp for tar files

! set system commands for UNIX platforms

  tartgt = '../post/tecplot/'//trim(run_title)//'.'//time_ext//'.tar'
  tarsrc = '*.plt'

  tarcmd = 'tar -cf '//trim(tartgt)//' '//trim(tarsrc)//';'//'rm -f '//trim(tarsrc)//' &'

  if(myid==0)  call execute_command( trim(tarcmd) )

! set system commands for PC platform (DOS)
! note: you must have a tar package installed and it must 
! reside in a directory contained in the DOS path
! recommended files are tar.exe and cygwin1.dll 

#if PC

    tartgt = '..\post\tecplot\'//trim(run_title)//'.'//time_ext//'.tar' 
    tarsrc = '*.plt'

    if (myid == 0) then
      tarcmd = 'tar -cf '//trim(tartgt)//' '//trim(tarsrc)
      call execute_command( trim(tarcmd) )      ! tar the files
      tarcmd = 'del '//trim(tarsrc)
      call execute_command( trim(tarcmd) )      ! delete the old files
    endif

#endif
!----------------------------------------------------------------------------------------
! format statements

  1 format(' zone t="Flow Solution", i=',i5,', f=block')
  2 format(' zone t="Flow Solution", i=',i5,', j=',i5,', f=block')
  3 format(' zone t="Flow Solution", i=',i5,', j=',i5,', k=',i5,', f=block')
  9 format(10(1pe12.4e3,1x))

  100 format(1x,a,i7,a,1pe9.3,a)
!----------------------------------------------------------------------------------------
  return
  end subroutine write_basic_tecplot_file

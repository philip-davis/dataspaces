#include "globalDefines.h"
! $Id: write_tecplot_yzplane.f90,v 1.1.2.1 2006/05/16 23:21:18 rsankar Exp $
!----------------------------------------------------------------------
!subroutine write_tecplot_xyplane(io, zplane, rr_r, hr, FI, Chi, Da, vort_term, Sd, U_n)
subroutine write_tecplot_xyplane(io, zfrac)
!----------------------------------------------------------------------
! routine writes tecplot file for a selected xy plane
!----------------------------------------------------------------------
use topology_m
use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec
use reference_m
use grid_m, only : x, y, z
use variables_m, only : q, u, temp, pressure, yspecies, volum 
use chemkin_m, only : species_name, n_species, element_name, n_elements, reaction_rate
use runtime_m, only : run_title, time, i_time, tstep
use mixfrac_m, only : mixfrac
!use premix_drvd_var_m, only: calculate_progvar

implicit none
!----------------------------------------------------------------------
! declarations passed in

integer, intent(in) :: io
real, intent(in) :: zfrac !(which of the nz_g planes is needed)

! local declarations

integer zplane
integer i,j,k,L
real p_conv_atm

real, dimension(nx,ny,nz,3) :: vort
real, dimension(nx,ny,nz) :: dil, vort_mag, hr, c

real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion

real, dimension(nx_g) :: x_g
real, dimension(ny_g) :: y_g

character*9 time_ext
character*4 plane_ext
character*100 filename,filename_short
integer kw
  
!----------------------------------------------------------------------
if(zfrac < 0 .or. zfrac > 1) then
  if(myid .eq. 0) & 
    write(io, *) 'Not writing tecplot_xyplane. zplane = ', zplane
  return
end if

zplane = int(zfrac*nz_g)

if(myid.eq.0) then
  write(io,*) 'writing tecplot_xyplane for:'
  write(io,'(a10,i7)') ' i_time = ',i_time
  write(io,'(a10,i7)') ' zplane = ',zplane
  write(io,'(a8,1pe9.3,a6)') ' time = ',time*time_ref,' (sec)'
endif
!----------------------------------------------------------------------
!call computeDivergence(u,dil)
!call calc_vorticity(vort,vort_mag,u)
#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  tstep = 1.0
call reaction_rate(rr_r,temp,pressure,yspecies,&
                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
call calc_heat_release(rr_r,hr)
!call calculate_progvar(c)
p_conv_atm=p_ref/pres_atm
!----------------------------------------------------------------------
if(zplane<zid*nz+1 .or. zplane>(zid+1)*nz) return
kw = zplane - zid*nz
!----------------------------------------------------------------------
! gather the x,y grid to zero process
if(yid.eq.0) call MPI_Gather(x,nx,MPI_REAL8,x_g,nx,MPI_REAL8,0,xcomm,ierr)
if(xid.eq.0) call MPI_Gather(y,ny,MPI_REAL8,y_g,ny,MPI_REAL8,0,ycomm,ierr)

!----------------------------------------------------------------------
if (xy_id==0) then 
  write(time_ext,'(1pe9.3)') time*time_ref
  !write(plane_ext,'(1i4.4)') zplane
  write(plane_ext,'(1f4.2)') zfrac
  filename_short='plane-xy-'//trim(plane_ext)//'.'//trim(time_ext)//'.tec'
  filename='../post/tecplot/'//trim(filename_short)
  open(unit=78,file=trim(filename),status='unknown')
  write(78,*) 'title = "',trim(run_title)//' '//trim(time_ext),'"'
  write(78,*) 'variables = '
  write(78,*) '"x (mm)"', '"y (mm)"'

!  write(78,*) '"C"'
  write(78,*) '"u (m/s)"'
  write(78,*) '"v (m/s)"'
!  write(78,*) '"w (m/s)"'
!  write(78,*) '"rho (g/cm^3)"'
  write(78,*) '"T (K)"'
  write(78,*) '"P (atm)"'
!  write(78,*) '"dil (1/s)"'
!  write(78,*) '"vort_x (1/s)"'
!  write(78,*) '"vort_y (1/s)"'
!  write(78,*) '"vort_z (1/s)"'
!  write(78,*) '"vort mag (1/s)"'
  write(78,*) '"heat release (J/m^3/s)"'
  do L=1,n_species
    select case(species_name(L))
!      case ('CH4', 'O2', 'CO2', 'CO')
      case ('o', 'oh', 'h2o2', 'c5h11-1', 'nc7h16', 'n2')
        write(78,*) '"Y '//trim(species_name(L))//'"'
    end select
  enddo
  do L=1,n_species
    select case(species_name(L))
!      case ('OH', 'CH4', 'CO', 'CH3', 'HO2')
      case ('o')
        write(78,*) '"RR '//trim(species_name(L))//' (1/s)"'
    end select
  enddo
!  do L=1,n_species,1
!  enddo

  ! write zone information
    write(78,2) nx_g, ny_g
  
  ! write coordinate data
    write(78, 9) ((x_g(i)*l_ref*1e3, i = 1, nx_g), j = 1, ny_g)
    write(78, 9) ((y_g(j)*l_ref*1e3, i = 1, nx_g), j = 1, ny_g)
endif 
  
!----------------------------------------------------------------------
! write variable data - all processor must call as routine does communication
!call write_tecplot_single_xy(78,c,1.0)
!do L=1,3
do L=1,2
  call write_tecplot_single_xy(78,u(:,:,:,L),a_ref)
enddo
!  call write_tecplot_single_xy(78,q(:,:,:,4,1),rho_ref)
call write_tecplot_single_xy(78,temp,t_ref)
call write_tecplot_single_xy(78,pressure,p_conv_atm)
!call write_tecplot_single_xy(78,dil,1.0/time_ref)
!do L=1,3
!  call write_tecplot_single_xy(78,vort(:,:,:,L),1.0/time_ref)
!enddo
!call write_tecplot_single_xy(78,vort_mag(:,:,:),1.0/time_ref)
call write_tecplot_single_xy(78,hr,-hr_ref)
  do L=1,n_species
    select case(species_name(L))
!      case ('CH4', 'O2', 'CO2', 'CO')
      case ('o', 'oh', 'h2o2', 'c5h11-1', 'nc7h16', 'n2')
        call write_tecplot_single_xy(78,yspecies(:,:,:,L),1.0)
    end select
  enddo

  do L=1,n_species
    select case(species_name(L))
!      case ('OH', 'CH4', 'CO', 'CH3', 'HO2')
      case ('o')
        call write_tecplot_single_xy(78,rr_r(:,:,:,L),rr_ref)
    end select
  enddo

!----------------------------------------------------------------------
if(xy_id==0) close(78)

!----------------------------------------------------------------------
! format statements

1 format(' zone t="stationary", i=',i5,', f=block')
2 format(' zone t="stationary", i=',i5,', j=',i5,', f=block')
3 format(' zone t="stationary", i=',i5,', j=',i5,', k=',i5,', f=block')

9 format(10(1pe12.5,1x))
!----------------------------------------------------------------------
return

contains

!----------------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine write_tecplot_single_xy(io,field,scaling)
implicit none

real, dimension(nx,ny,nz), intent(in) :: field !the field to write
real, intent(in) :: scaling                    !scaling units for the field
integer, intent(in) :: io                      !io unit

integer i, j, i_tecplot
integer jw, yw
real f(nx)
real field_g(nx_g)

  loopj: do j = 1, ny_g
    !Find which yid this j belongs to
    yw = (j-1)/ny
    if( yid .eq. yw) then
      jw = j-yid*ny
      f(:) = field(:, jw, kw)
      call mpi_gather (f,nx,MPI_REAL8,field_g,nx,MPI_REAL8,0,xcomm,ierr)
      if(xid .ne. 0) cycle loopj
      if(yid .ne. 0) call mpi_send(field_g, nx_g, MPI_REAL8, 0, j, ycomm, ierr)
    end if
    if (xy_id.eq.0 .and. yw.ne.0) &
      call mpi_recv(field_g, nx_g, MPI_REAL8, yw, j, ycomm, status, ierr)
    if(xy_id.eq.0) write(io, 9) (field_g(i)*scaling, i = 1, nx_g)
  end do loopj

9 format(10(1pe12.5,1x))
return
end subroutine write_tecplot_single_xy

end subroutine write_tecplot_xyplane

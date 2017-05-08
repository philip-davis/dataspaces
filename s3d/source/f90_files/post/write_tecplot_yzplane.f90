#include "globalDefines.h"
! $Id: write_tecplot_yzplane.f90,v 1.1.2.2 2007/06/05 16:40:58 rsankar Exp $
!----------------------------------------------------------------------
subroutine write_tecplot_yzplane(io, xfrac)
!----------------------------------------------------------------------
! routine writes tecplot file for a selected yz plane
!----------------------------------------------------------------------
use topology_m
use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec
use reference_m
use grid_m, only : x, y, z
use variables_m, only : q, u, temp, pressure, yspecies, volum 
use chemkin_m, only : species_name, n_species, element_name, n_elements, reaction_rate
use runtime_m, only : run_title, time, i_time, tstep
!use premix_drvd_var_m, only: calculate_progvar

implicit none
!----------------------------------------------------------------------
! declarations passed in

integer, intent(in) :: io
real, intent(in) :: xfrac !(which of the nx_g planes is needed)

! local declarations

integer xplane
integer i,j,k,L
real p_conv_atm

real, dimension(nx,ny,nz,3) :: vort
real, dimension(nx,ny,nz) :: dil, vort_mag, hr, c

real, dimension(nx,ny,nz,n_spec) :: rr_r, diffusion

real, dimension(ny_g) :: y_g
real, dimension(nz_g) :: z_g

character*9 time_ext
character*4 plane_ext
character*100 filename,filename_short
integer iw

!----------------------------------------------------------------------
if(xfrac < 0 .or. xfrac > 1) then
  if(myid .eq. 0) & 
    write(io, *) 'Not writing tecplot_yzplane. xfrac = ', xfrac
  return
end if

xplane = int(xfrac*nx_g)

if(myid.eq.0) then
  write(io,*) 'writing tecplot_yzplane for:'
  write(io,'(a10,i7)') ' i_time = ',i_time
  write(io,'(a10,i7)') ' xplane = ',xplane
  write(io,'(a10,1f4.2)') ' xfrac = ',xfrac
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
if(xplane<xid*nx+1 .or. xplane>(xid+1)*nx) return
iw = xplane - xid*nx
!----------------------------------------------------------------------
! gather the y,z grid to zero process
if(zid.eq.0) call MPI_Gather(y,ny,MPI_REAL8,y_g,ny,MPI_REAL8,0,ycomm,ierr)
if(yid.eq.0) call MPI_Gather(z,nz,MPI_REAL8,z_g,nz,MPI_REAL8,0,zcomm,ierr)

!----------------------------------------------------------------------
if (yz_id==0) then 
  write(time_ext,'(1pe9.3)') time*time_ref 
  !write(plane_ext,'(1i4.4)') xplane
  write(plane_ext,'(1f4.2)') xfrac
  filename_short='plane-yz-'//trim(plane_ext)//'.'//trim(time_ext)//'.tec'
  filename='../post/tecplot/'//trim(filename_short)
  open(unit=78,file=trim(filename),status='unknown')

  write(78,*) 'title = "',trim(run_title)//' '//trim(time_ext),'"'
  write(78,*) 'variables = '
  write(78,*) '"y (mm)"', '"z (mm)"'

!  write(78,*) '"C"'
  write(78,*) '"u (cm/s)"'
  write(78,*) '"v (cm/s)"'
  write(78,*) '"w (cm/s)"'
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
!    select case(species_name(L))  
!      case ('CH4', 'O2', 'CO2', 'CO')
        write(78,*) '"Y '//trim(species_name(L))//'"'
!    end select
  enddo

!  do L=1,n_species
!    select case(species_name(L))  
!      case ('OH', 'CH4', 'CO', 'CH3', 'HO2')
!        write(78,*) '"RR '//trim(species_name(L))//' (1/s)"'
!    end select
!  enddo
!  do L=1,n_species,1
!  enddo

! write zone information
  write(78,2) ny_g, nz_g

! write coordinate data
  write(78, 9) ((y_g(j)*l_ref*1e3, j = 1, ny_g), k = 1, nz_g)
  write(78, 9) ((z_g(k)*l_ref*1e3, j = 1, ny_g), k = 1, nz_g)
endif 

!----------------------------------------------------------------------
! write variable data - all processor must call as routine does communication
!call write_tecplot_single(78,c,1.0)
do L=1,3
!do L=1,2
  call write_tecplot_single(78,u(:,:,:,L),a_ref)
enddo
!  call write_tecplot_single(78,q(:,:,:,4,1),rho_ref)
call write_tecplot_single(78,temp,t_ref)
call write_tecplot_single(78,pressure,p_conv_atm)
!call write_tecplot_single(78,dil,1.0/time_ref)
!do L=1,3
!  call write_tecplot_single(78,vort(:,:,:,L),1.0/time_ref)
!enddo
!call write_tecplot_single(78,vort_mag(:,:,:),1.0/time_ref)
call write_tecplot_single(78,hr,-hr_ref)
  do L=1,n_species
!    select case(species_name(L))  
!      case ('CH4', 'O2', 'CO2', 'CO')
        call write_tecplot_single(78,yspecies(:,:,:,L),1.0)
!    end select
  enddo

!  do L=1,n_species
!    select case(species_name(L))  
!      case ('OH', 'CH4', 'CO', 'CH3', 'HO2')
!        call write_tecplot_single(78,rr_r(:,:,:,L),rr_ref)
!    end select
!  enddo

!----------------------------------------------------------------------
if(yz_id==0) close(78)

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
subroutine write_tecplot_single(io,field,scaling)
implicit none

real, dimension(nx,ny,nz), intent(in) :: field !the field to write
real, intent(in) :: scaling                    !scaling units for the field
integer, intent(in) :: io                      !io unit

integer j, k
integer kw, zw
real f(ny)
real field_g(ny_g)

loopk: do k = 1, nz_g
  !Find which zid this k belongs to
  zw = (k-1)/nz
  if( zid .eq. zw) then
    kw = k-zid*nz
    f(:) = field(iw, :, kw)
    call mpi_gather (f,ny,MPI_REAL8,field_g,ny,MPI_REAL8,0,ycomm,ierr)
    if(yid .ne. 0) cycle loopk
    if(zid .ne. 0) call mpi_send(field_g, ny_g, MPI_REAL8, 0, k, zcomm, ierr)
  end if
  if (yz_id.eq.0 .and. zw.ne.0) &
    call mpi_recv(field_g, ny_g, MPI_REAL8, zw, k, zcomm, status, ierr)
  if(yz_id.eq.0) write(io, 9) (field_g(j)*scaling, j = 1, ny_g)
end do loopk

9 format(10(1pe12.5,1x))
return
end subroutine write_tecplot_single

end subroutine write_tecplot_yzplane

!----------------------------------------------------------------------
subroutine write_netcdf_yzplane(io, xfrac)
!----------------------------------------------------------------------
! Routine writes netcdf file for a selected yz plane
! Code is from Ramanan by way of Norbert Podhorszki with some 
! reformatting and produces output in a format convenient for scripted 
! creation of images that can be displayed in ORNL dashboard monitoring
! system
!----------------------------------------------------------------------

use netcdf
use topology_m
use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec
use reference_m
use grid_m, only : x, y, z
use variables_m, only : q, u, temp, pressure, yspecies, volum 
use chemkin_m, only : species_name, n_species, element_name,& 
     n_elements, reaction_rate
use runtime_m, only : run_title, time, i_time, tstep

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
character *1 velocity_num
character *9 velocity_name
character*100 filename,filename_short
integer iw

integer fileid, timedimid, timevarid, ydimid, zdimid, varid

!----------------------------------------------------------------------

if(xfrac < 0 .or. xfrac > 1) then
  if(myid .eq. 0) & 
    write(io, *) 'Not writing netcdf_yzplane. xfrac = ', xfrac
  return
end if

xplane = int(xfrac*nx_g)

if(myid.eq.0) then
  write(io,*) 'writing netcdf_yzplane for:'
  write(io,'(a10,i7)') ' i_time = ',i_time
  write(io,'(a10,i7)') ' xplane = ',xplane
  write(io,'(a10,1f4.2)') ' xfrac = ',xfrac
  write(io,'(a8,1pe9.3,a6)') ' time = ',time*time_ref,' (sec)'
endif


call computeDivergence(u,dil)
!call calc_vorticity(vort,vort_mag,u)
!#ifdef GETRATES_NEEDS_DIFFUSION
!  diffusion = 0.0
!  tstep = 1.0
!  call reaction_rate(rr_r,temp,pressure,yspecies,&
!                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
!#else
!call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
!#endif
!call calc_heat_release(rr_r,hr)
p_conv_atm=p_ref/pres_atm

!----------------------------------------------------------------------

if(xplane<xid*nx+1 .or. xplane>(xid+1)*nx) return
iw = xplane - xid*nx

! gather the y,z grid to zero process
if(zid.eq.0) call MPI_Gather(y,ny,MPI_REAL8,y_g,ny,MPI_REAL8,0,ycomm,ierr)
if(yid.eq.0) call MPI_Gather(z,nz,MPI_REAL8,z_g,nz,MPI_REAL8,0,zcomm,ierr)

if(yz_id .eq. 0) then 
  write(time_ext, '(1pe9.3)') time*time_ref
  filename = '../post/yzplane-'//trim(time_ext)//'.nc'
  write(io,*) 'creating new file', filename
  call nferr(nf90_create(filename, nf90_clobber, fileid), 'open new')
  call nferr(nf90_put_att(fileid, NF90_GLOBAL, 'title', &
     "Contains yz plane"))

  !----------------------------------------
  call nferr(nf90_def_dim(fileid, 'y', ny_g, ydimid) )
  call nferr(nf90_def_var(fileid, 'y', NF90_FLOAT, ydimid, varid) )
  call nferr(nf90_put_att(fileid, varid, 'units', 'mm') )
  call nferr(nf90_put_att(fileid, varid, 'scale_factor', l_ref*1e3) )
  call nferr(nf90_enddef(fileid))
  call nferr(nf90_put_var(fileid, varid, y_g))
  !----------------------------------------
  call nferr(nf90_redef(fileid))
  call nferr(nf90_def_dim(fileid, 'z', nz_g, zdimid) )
  call nferr(nf90_def_var(fileid, 'z', NF90_FLOAT, zdimid, varid) )
  call nferr(nf90_put_att(fileid, varid, 'units', 'mm') )
  call nferr(nf90_put_att(fileid, varid, 'scale_factor', l_ref*1e3) )
  call nferr(nf90_enddef(fileid))
  call nferr(nf90_put_var(fileid, varid, z_g))
  !----------------------------------------
  call nferr(nf90_sync(fileid))
  !----------------------------------------
end if !myid eq 0

!----------------------------------------------------------------------
! write variable data - all processor must call as routine does communication
!call write_netcdf_single(io,c,1.0)
do L=1,3
!do L=1,2
  write(velocity_num,'I1') L
  velocity_name = 'velocity'//velocity_num
  call write_netcdf_single(io,u(:,:,:,L),a_ref, velocity_name, 'm/s' )
enddo
!  call write_netcdf_single(io,q(:,:,:,4,1),rho_ref)

call write_netcdf_single(io,temp,t_ref, 'Temperature', 'K')
call write_netcdf_single(io,pressure,p_conv_atm, 'pressure', 'atm')
call write_netcdf_single(io,dil,1.0/time_ref, 'dilatation', '1/s')

!do L=1,3
!  call write_netcdf_single(io,vort(:,:,:,L),1.0/time_ref)
!enddo
!call write_netcdf_single(io,vort_mag(:,:,:),1.0/time_ref)
!call write_netcdf_single(io,hr,-hr_ref)

  do L=1,n_species
    select case(species_name(L))  
      case ('O2', 'N2')
        call write_netcdf_single(io,yspecies(:,:,:,L),1.0, & 
                'Y'//trim(species_name(L)), ' ')
    end select
  enddo

!----------------------------------------------------------------------
if(yz_id==0) call nferr(nf90_close(fileid))
!----------------------------------------------------------------------


return

contains

!======================================================================
subroutine write_netcdf_single(io,field,scaling,varname,varunit)
implicit none

real, dimension(nx,ny,nz), intent(in) :: field !the field to write
real, intent(in) :: scaling                    !scaling units for 
                                               ! the field
integer, intent(in) :: io                      !io unit
character(*), intent(in) :: varname, varunit

integer j, k
integer kw, zw
real f(ny)
real field_g(ny_g)
integer varid2

if(yz_id.eq.0) then
   call nferr(nf90_redef(fileid))
   call nferr(nf90_def_var(fileid, trim(varname), NF90_FLOAT, & 
        (/ydimid, zdimid/), varid) )
   call nferr(nf90_put_att(fileid, varid, 'scale_factor', scaling))
   call nferr(nf90_put_att(fileid, varid, 'units', trim(varunit) ))
   call nferr(nf90_def_var(fileid, trim(varname)//'_y', NF90_FLOAT, & 
        (/ydimid/), varid2) )
   call nferr(nf90_put_att(fileid, varid2, 'scale_factor', scaling))
   call nferr(nf90_put_att(fileid, varid2, 'units', trim(varunit) ))
   call nferr(nf90_enddef(fileid))
end if

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

   if(yz_id.eq.0) call nferr(nf90_put_var(fileid, varid, field_g, & 
        start=(/1, k/), count=(/ny_g,1/) ))

   if(yz_id.eq.0 .and. k .eq. nz_g/2) &
        call nferr(nf90_put_var(fileid, varid2, field_g))

end do loopk

return
end subroutine write_netcdf_single

!----------------------------------------------------------------------
subroutine nferr(status, tag)

  integer, intent (in) :: status
  character(len=*), optional, intent(in) :: tag
  
  if(status /= nf90_noerr) then 
    if(present(tag)) then
      print *, trim(tag)//':'//trim(nf90_strerror(status))
    else
      print *, trim(nf90_strerror(status))
    end if
    stop "Stopped"
  end if

end subroutine nferr  


!======================================================================
end subroutine write_netcdf_yzplane
!----------------------------------------------------------------------
subroutine write_netcdf_xyplane(io, zfrac)
!----------------------------------------------------------------------
! Routine writes netcdf file for a selected xy plane
! Code is from Ramanan by way of Norbert Podhorszki with some 
! reformatting and produces output in a format convenient for scripted 
! creation of images that can be displayed in ORNL dashboard monitoring
! system
!----------------------------------------------------------------------

use netcdf
use topology_m
use param_m, only : nx, ny, nz, nx_g, ny_g, nz_g, n_spec
use reference_m
use grid_m, only : x, y, z
use variables_m, only : q, u, temp, pressure, yspecies, volum 
use chemkin_m, only : species_name, n_species, element_name,& 
     n_elements, reaction_rate
use runtime_m, only : run_title, time, i_time, tstep

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

real, dimension(ny_g) :: y_g
real, dimension(nx_g) :: x_g

character*9 time_ext
character*4 plane_ext
character *1 velocity_num
character *9 velocity_name
character*100 filename,filename_short
integer kw

integer fileid, timedimid, timevarid, ydimid, xdimid, varid

!----------------------------------------------------------------------

if(zfrac < 0 .or. zfrac > 1) then
  if(myid .eq. 0) & 
    write(io, *) 'Not writing netcdf_xyplane. zfrac = ', zfrac
  return
end if

zplane = int(zfrac*nz_g)

if(myid.eq.0) then
  write(io,*) 'writing netcdf_xyplane for:'
  write(io,'(a10,i7)') ' i_time = ',i_time
  write(io,'(a10,i7)') ' zplane = ',zplane
  write(io,'(a10,1f4.2)') ' zfrac = ',zfrac
  write(io,'(a8,1pe9.3,a6)') ' time = ',time*time_ref,' (sec)'
endif


call computeDivergence(u,dil)
!call calc_vorticity(vort,vort_mag,u)
!#ifdef GETRATES_NEEDS_DIFFUSION
!  diffusion = 0.0
!  tstep = 1.0
!  call reaction_rate(rr_r,temp,pressure,yspecies,&
!                     diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
!#else
!  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
!#endif
!call calc_heat_release(rr_r,hr)
p_conv_atm=p_ref/pres_atm

!----------------------------------------------------------------------

if(zplane<zid*nz+1 .or. zplane>(zid+1)*nz) return
kw = zplane - zid*nz

! gather the y,x grid to zero process
if(xid.eq.0) call MPI_Gather(y,ny,MPI_REAL8,y_g,ny,MPI_REAL8,0,ycomm,ierr)
if(yid.eq.0) call MPI_Gather(x,nx,MPI_REAL8,x_g,nx,MPI_REAL8,0,xcomm,ierr)

if(xy_id .eq. 0) then 
  write(time_ext, '(1pe9.3)') time*time_ref
  filename = '../post/xyplane-'//trim(time_ext)//'.nc'
  write(io,*) 'creating new file', filename
  call nferr(nf90_create(filename, nf90_clobber, fileid), 'open new')
  call nferr(nf90_put_att(fileid, NF90_GLOBAL, 'title', &
     "Contains xy plane"))

  !----------------------------------------
  call nferr(nf90_def_dim(fileid, 'y', ny_g, ydimid) )
  call nferr(nf90_def_var(fileid, 'y', NF90_FLOAT, ydimid, varid) )
  call nferr(nf90_put_att(fileid, varid, 'units', 'mm') )
  call nferr(nf90_put_att(fileid, varid, 'scale_factor', l_ref*1e3) )
  call nferr(nf90_enddef(fileid))
  call nferr(nf90_put_var(fileid, varid, y_g))
  !----------------------------------------
  call nferr(nf90_redef(fileid))
  call nferr(nf90_def_dim(fileid, 'x', nx_g, xdimid) )
  call nferr(nf90_def_var(fileid, 'x', NF90_FLOAT, xdimid, varid) )
  call nferr(nf90_put_att(fileid, varid, 'units', 'mm') )
  call nferr(nf90_put_att(fileid, varid, 'scale_factor', l_ref*1e3) )
  call nferr(nf90_enddef(fileid))
  call nferr(nf90_put_var(fileid, varid, x_g))
  !----------------------------------------
  call nferr(nf90_sync(fileid))
  !----------------------------------------
end if !myid eq 0

!----------------------------------------------------------------------
! write variable data - all processor must call as routine does communication
!call write_netcdf_single(io,c,1.0)
!do L=1,3
do L=1,3
  write(velocity_num,'I1') L
  velocity_name = 'velocity'//velocity_num
  call write_netcdf_single(io,u(:,:,:,L),a_ref, velocity_name, "m/s")
enddo
!  call write_netcdf_single(io,q(:,:,:,4,1),rho_ref)

call write_netcdf_single(io,temp,t_ref, 'Temperature', 'K')
call write_netcdf_single(io,pressure,p_conv_atm, 'pressure', 'atm')
call write_netcdf_single(io,dil,1.0/time_ref, 'dilatation', '1/s')

!do L=1,3
!  call write_netcdf_single(io,vort(:,:,:,L),1.0/time_ref)
!enddo
!call write_netcdf_single(io,vort_mag(:,:,:),1.0/time_ref)
!call write_netcdf_single(io,hr,-hr_ref)

  do L=1,n_species
    select case(species_name(L))  
      case ('O2', 'N2')
        call write_netcdf_single(io,yspecies(:,:,:,L),1.0, & 
                'Y'//trim(species_name(L)), ' ')
    end select
  enddo

!----------------------------------------------------------------------
if(xy_id==0) call nferr(nf90_close(fileid))
!----------------------------------------------------------------------


return

contains

!======================================================================
subroutine write_netcdf_single(io,field,scaling,varname,varunit)
implicit none

real, dimension(nx,ny,nz), intent(in) :: field !the field to write
real, intent(in) :: scaling                    !scaling units for 
                                               ! the field
integer, intent(in) :: io                      !io unit
character(*), intent(in) :: varname, varunit

integer j, i
integer iw, xw
real f(ny)
real field_g(ny_g)
integer varid2

if(xy_id.eq.0) then
   call nferr(nf90_redef(fileid))
   call nferr(nf90_def_var(fileid, trim(varname), NF90_FLOAT, & 
        (/ydimid, xdimid/), varid) )
   call nferr(nf90_put_att(fileid, varid, 'scale_factor', scaling))
   call nferr(nf90_put_att(fileid, varid, 'units', trim(varunit) ))
   call nferr(nf90_def_var(fileid, trim(varname)//'_y', NF90_FLOAT, & 
        (/ydimid/), varid2) )
   call nferr(nf90_put_att(fileid, varid2, 'scale_factor', scaling))
   call nferr(nf90_put_att(fileid, varid2, 'units', trim(varunit) ))
   call nferr(nf90_enddef(fileid))
end if

loopi: do i = 1, nx_g
   !Find which xid this i belongs to
   xw = (i-1)/nx

   if( xid .eq. xw) then
      iw = i-xid*nx
      f(:) = field(iw, :, kw)
      call mpi_gather (f,ny,MPI_REAL8,field_g,ny,MPI_REAL8,0,ycomm,ierr)
      if(yid .ne. 0) cycle loopi
      if(xid .ne. 0) call mpi_send(field_g, ny_g, MPI_REAL8, 0, i, xcomm, ierr)
   end if

   if (xy_id.eq.0 .and. xw.ne.0) &
        call mpi_recv(field_g, ny_g, MPI_REAL8, xw, i, xcomm, status, ierr)

   if(xy_id.eq.0) call nferr(nf90_put_var(fileid, varid, field_g, & 
        start=(/1, i/), count=(/ny_g,1/) ))

   if(xy_id.eq.0 .and. i .eq. nx_g/2) &
        call nferr(nf90_put_var(fileid, varid2, field_g))

end do loopi

return
end subroutine write_netcdf_single

!----------------------------------------------------------------------
subroutine nferr(status, tag)

  integer, intent (in) :: status
  character(len=*), optional, intent(in) :: tag
  
  if(status /= nf90_noerr) then 
    if(present(tag)) then
      print *, trim(tag)//':'//trim(nf90_strerror(status))
    else
      print *, trim(nf90_strerror(status))
    end if
    stop "Stopped"
  end if

end subroutine nferr  


!======================================================================
end subroutine write_netcdf_xyplane

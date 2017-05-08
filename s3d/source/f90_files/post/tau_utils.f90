#include "globalDefines.h"
! 
!     SUBROUTINE write_field(io, field, nxf, nyf, nzf, FX, FY, FZ, filename)
!     
!       USE HDF5
!       use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
!       use topology_m
!       use reference_m
!     
!       IMPLICIT NONE
!       ! INPUT VARIABLES
!       INTEGER io
!       INTEGER nxf, nyf, nzf
!       REAL FX(nxf, npx*npy*npz), FY(nyf, npx*npy*npz), FZ(nzf, npx*npy*npz)
!       REAL, intent(in):: field(nxf,nyf,nzf)
!       REAL :: buffer(nxf,nyf,nzf)
!       CHARACTER(LEN=8) :: dsetname   ! Dataset name
!       CHARACTER(LEN=20) :: fieldname  ! Field name
!       CHARACTER(LEN=50) :: filename   ! File name
!       
!       LOGICAL :: overwrite
!       INTEGER :: error
!       
!       ! Scratch variables
!     !  integer mypx, mypy, mypz   ! already allocated in topology
!       
!       ! HDF variables
!       INTEGER(HID_T) :: file_id       ! File identifier 
!       INTEGER(HID_T) :: dset_id,x_id,y_id,z_id       ! Dataset identifier 
!       INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
!       INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
!       INTEGER(HSIZE_T), DIMENSION(3) :: dimsf, blockdim, offset  ! Dataset dimensions: global, local, local offset
!       INTEGER(HSIZE_T), DIMENSION(3) :: dimschk
!       INTEGER OS(3)
!       INTEGER(HSIZE_T), DIMENSION(3) :: stride, count
!       INTEGER, DIMENSION(NPX) :: COUNTX
!       INTEGER, DIMENSION(NPY) :: COUNTY
!       INTEGER, DIMENSION(NPZ) :: COUNTZ
!       INTEGER, DIMENSION(npx*npy*npz) :: COUNTXG, COUNTYG, COUNTZG
!       INTEGER pcounter, countpx, countpy, countpz
!       INTEGER :: rank = 3 ! Dataset rank 
!       real, dimension(nxf,nyf,nzf) :: grid_store
!       integer req
!       ! counters
!       INTEGER i,j,k
!       !----------------------------------------------------------------------
!     
!       dimsf(1) = nxf*npx
!       dimsf(2) = nyf*npy
!       dimsf(3) = nzf*npz
!     
!     
!       ! Compute offset on each process from global grid
!         mypz = myid/(npx*npy)
!         mypx = MOD(myid-(mypz*npx*npy), npx)
!         mypy = (myid-(mypz*npx*npy))/npx
!     
!         ! Dimensions of output block on this process
!         blockdim(1) = nxf
!         blockdim(2) = nyf
!         blockdim(3) = nzf
!     
!         
!         call MPI_Allgather( blockdim(1), 1, MPI_INTEGER, countxg, 1, MPI_INTEGER, gcomm, ierr)
!         call MPI_Allgather( blockdim(2), 1, MPI_INTEGER, countyg, 1, MPI_INTEGER, gcomm, ierr)
!         call MPI_Allgather( blockdim(3), 1, MPI_INTEGER, countzg, 1, MPI_INTEGER, gcomm, ierr)
!         
!     
!         offset = 0
!         dimschk = 0
!         ! setup countx/y/z in grid topology coordinates.
!         DO pcounter = 0,npx*npy*npz-1
!            countpz = pcounter/(npx*npy)
!            countpx = MOD(pcounter-(countpz*npx*npy), npx)
!            countpy = (pcounter-(countpz*npx*npy))/npx
!                   
!            if( countpx == mypx ) then
!               if( countpy == mypy ) then
!                  countz(countpz+1) = countzg(pcounter+1)
!               endif
!               if( countpz == mypz ) then
!                  county(countpy+1) = countyg(pcounter+1)
!               endif
!            endif
!            if(countpy == mypy .and. countpz == mypz) then
!               countx(countpx+1) =countxg(pcounter+1)
!            endif
!         ENDDO
!     !    write(*,*) 'myid,countxyz=',myid,countx,county,countz
!         ! Count up grid points on other processes, check total number
!         DO pcounter = 1, npx
!            IF(pcounter <= mypx ) then
!               offset(1) = offset(1) + countx(pcounter)
!            ENDIF
!            dimschk(1) = dimschk(1) + countx(pcounter)
!         ENDDO
!         
!         DO pcounter = 1, npy
!            IF(pcounter <= mypy ) then
!               offset(2) = offset(2) + county(pcounter)
!            ENDIF
!            dimschk(2) = dimschk(2) + county(pcounter)
!         ENDDO
!         
!         DO pcounter = 1, npz
!            IF(pcounter <= mypz ) then
!               offset(3) = offset(3) + countz(pcounter)
!            ENDIF
!            dimschk(3) = dimschk(3) + countz(pcounter)
!         ENDDO
!         
!     !    write(*,*) 'myid,offset=',myid,offset
!         ! Get global file dimensions (use count as scracth space)
!         CALL MPI_Allreduce(dimschk(1), count(1), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
!         CALL MPI_Allreduce(dimschk(2), count(2), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
!         CALL MPI_Allreduce(dimschk(3), count(3), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
!         
!         if(myid==0) then
!            do i = 1,3
!            if (dimsf(i).ne.count(i)) then
!               write(*,*) 'error, count mismatch: dimsf/dimschk=',dimsf, count
!            endif
!            enddo
!         endif
!         
!       ! End compute offset 
!     
!     
!       
!     
!         
!       ! Do output on root only
!     
!         if(myid==0) then
!     !       filename = 'filtered.h5'
!            
!            CALL h5open_f(error) 
!            CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
!            CALL h5screate_simple_f(rank, dimsf, filespace, error)
!            
!            fieldname = 'f1'    
!            CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
!                 dset_id, error)
!            fieldname = 'x'    
!            CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
!                 x_id, error)
!            fieldname = 'y'    
!            CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
!                 y_id, error)
!            fieldname = 'z'    
!            CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
!                 z_id, error)
!     
!     
!     
!            CALL h5screate_simple_f(rank, blockdim, memspace, error)    ! create memory space
!            stride = 1
!            count = 1
!            CALL h5dget_space_f(dset_id, filespace, error)
!     
!            ! First dump what's here
!            CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, blockdim, error, &
!                 stride, count)          
!            
!     !       write(*,*)'offset=',offset
!     !       write(*,*)'blockdim=',blockdim
!     !       write(*,*)'stride=',stride
!     !       write(*,*)'count=',count
!     !       write(*,*)'field(1,1,1)=',field(1,1,1)
!            
!            CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, field, dimsf, error, &
!                 file_space_id = filespace, mem_space_id = memspace)
!     
!            DO k=1,nzf
!               DO j=1,nyf
!                  grid_store(:,j,k) = fx(:,myid+1)*l_ref*1.0e3
!               ENDDO
!            ENDDO
!            CALL h5dwrite_f(x_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
!                 file_space_id = filespace, mem_space_id = memspace)
!     
!            DO k=1,nzf
!               do j=1,nyf
!                  DO i=1,nxf
!                     grid_store(i,j,k) = fy(j,myid+1)*l_ref*1.0e3
!                  enddo
!               ENDDO
!            ENDDO
!            CALL h5dwrite_f(y_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
!                 file_space_id = filespace, mem_space_id = memspace)
!     
!            do k = 1,nzf
!            DO j=1,nyf
!               DO i=1,nxf
!                  grid_store(i,j,k) = fz(k,myid+1)*l_ref*1.0e3
!                  enddo
!               ENDDO
!            ENDDO
!            CALL h5dwrite_f(z_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
!                 file_space_id = filespace, mem_space_id = memspace)
!     
!     !       write(*,*)'bd, nx/y/z f',blockdim, nxf, nyf, nzf
!            DO pcounter=1,npx*npy*npz-1
!               CALL MPI_Recv(os,rank, MPI_INTEGER, pcounter, 15, gcomm, req, ierr)
!               offset=os
!     !          write(*,*) 'root recieved offset from',pcounter,offset
!     
!               CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
!                    , MPI_DOUBLE_PRECISION, pcounter, 16, gcomm, req, ierr)
!     
!               CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, blockdim, error, &
!                    stride, count)          
!               
!               CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
!                    file_space_id = filespace, mem_space_id = memspace)
!     
!               CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
!                    , MPI_DOUBLE_PRECISION, pcounter, 26, gcomm, req, ierr)
!               CALL h5dwrite_f(x_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
!                    file_space_id = filespace, mem_space_id = memspace)
!     !
!              CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
!                    , MPI_DOUBLE_PRECISION, pcounter, 36, gcomm, req, ierr)
!               CALL h5dwrite_f(y_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
!                    file_space_id = filespace, mem_space_id = memspace)
!     !
!               CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
!                    , MPI_DOUBLE_PRECISION, pcounter, 46, gcomm, req, ierr)
!               CALL h5dwrite_f(z_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
!                    file_space_id = filespace, mem_space_id = memspace)
!     !
!            ENDDO          
!     
!            
!            CALL h5dclose_f(dset_id, error)
!            CALL h5sclose_f(filespace, error)
!            CALL h5sclose_f(memspace, error)
!            CALL h5fclose_f(file_id, error)
!            CALL h5close_f(error)
!            
!         ELSE
!     !esr       if(myid==241) write(*,*) 'id241 is sending offset=',offset
!            os = offset
!            CALL MPI_Send( os, rank, MPI_INTEGER, 0, 15, gcomm,req,  ierr)
!     
!            CALL MPI_Send( field, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
!                 , 0, 16, gcomm, req,  ierr)
!     
!            DO k=1,nzf
!               DO j=1,nyf
!                  grid_store(:,j,k) = fx(:,myid+1)*l_ref*1.0e3
!               ENDDO
!            ENDDO
!           CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
!                , 0, 26, gcomm,req,  ierr)
!     
!            DO k=1,nzf
!               do j=1,nyf
!               DO i=1,nxf
!                  grid_store(i,j,k) = fy(j,myid+1)*l_ref*1.0e3
!               ENDDO
!            enddo
!            ENDDO
!           
!            CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
!                 , 0, 36, gcomm,  req, ierr)
!            do k=1,nzf
!            DO j=1,nyf
!               DO i=1,nxf
!                  grid_store(i,j,k) = fz(k,myid+1)*l_ref*1.0e3
!               ENDDO
!            ENDDO
!         enddo
!            CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
!                 , 0, 46, gcomm,  req, ierr)
!     !
!     
!     
!         endif
!     END SUBROUTINE WRITE_FIELD
! 

SUBROUTINE write_basics(io, basics, xpl, zpl, n1, n2 , writemin, writemax)

  use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
  use topology_m
  use reference_m

  IMPLICIT NONE
  integer io,n1,n2
!  real, allocatable, dimension(:,:,:) :: basics, basics_g
  real, dimension(n1,n2,ny) :: basics
  real, dimension(n1,n2,ny*npy) :: basics_g
  real xpl,zpl, writemin, writemax

  integer ic,id,iy

call MPI_Gather(basics,ny*n1*n2,MPI_REAL8,basics_g,ny*n1*n2,MPI_REAL8,0,ycomm,ierr)

if(yid.eq.0 .and. &
   real(xid*nx).le.xpl*real(nx*npx).and.real((xid+1)*nx).gt.xpl*real(nx*npx)  .and.  &
   real(zid*nz).le.zpl*real(nz*npz).and.real((zid+1)*nz).gt.zpl*real(nz*npz))then

do ic=1,2!3
do id=1,2!3
 write(io,9) (basics_g(ic,id,iy), iy=int(ny*npy*writemin),int(ny*npy*writemax))
enddo
enddo

endif

  9 format(10(1pe12.5,1x))
RETURN
END SUBROUTINE WRITE_BASICS

!-------------------------------------------------------------------

SUBROUTINE write_onedim(io, onedim, xpl, zpl , writemin, writemax)

  use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
  use topology_m
  use reference_m
      
  IMPLICIT NONE
  integer io,n1,n2
  real, dimension(ny) :: onedim
  real, dimension(ny*npy) :: onedim_g
  real xpl,zpl, writemin, writemax

  integer ic,id,iy
  
call MPI_Gather(onedim,ny,MPI_REAL8,onedim_g,ny,MPI_REAL8,0,ycomm,ierr)

if(yid.eq.0 .and. &
   real(xid*nx).le.xpl*real(nx*npx).and.real((xid+1)*nx).gt.xpl*real(nx*npx)  .and.  &
   real(zid*nz).le.zpl*real(nz*npz).and.real((zid+1)*nz).gt.zpl*real(nz*npz))then
   write(78,9) (onedim_g(iy), iy=int(ny*npy*writemin),int(ny*npy*writemax))
endif
           
  9 format(10(1pe12.5,1x))
RETURN
END SUBROUTINE WRITE_ONEDIM

!-------------------------------------------------------------------

subroutine write_filtfields(io,uf,yf,wf,pf,cf,ic)
use topology_m, only : myid, gcomm, ierr
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
use reference_m, only : time_ref

implicit none
integer,intent(in) :: io, ic

character*5 :: myid_ext*5, cond_ext*3, filename*100, dirname*100

real uf(nx*ny*nz*3),yf(nx*ny*nx*n_spec),wf(nx*ny*nx*n_spec),pf(nx*ny*nz),cf(nx*ny*nz)

call MPI_Barrier( gcomm, ierr )

write(myid_ext, '(I5.5)') myid
write(cond_ext, '(I3.3)') ic
dirname = '../post/filtfields/'

!----------------------------------------
!filename=trim(dirname)//'field.'//cond_ext//'.'//myid_ext
filename=trim(dirname)//'field.'//myid_ext
open(unit=201,file=trim(filename),status='unknown',form='unformatted')
  call readwrite_filtfile_data(io,201,'w',uf,yf,wf,pf,cf)
close(201)

!----------------------------------------
call MPI_Barrier( gcomm, ierr )
return
end subroutine write_filtfields


!***********************************************
!***********************************************

subroutine read_filtfields(io,uf,yf,wf,pf,cf,ic)
use topology_m, only : myid, gcomm, ierr
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
use reference_m, only : time_ref
  
implicit none
integer,intent(in) :: io, ic

character*5 :: myid_ext*5, cond_ext*3, filename*100, dirname*100

real uf(nx*ny*nz*3),yf(nx*ny*nx*n_spec),wf(nx*ny*nx*n_spec),pf(nx*ny*nz),cf(nx*ny*nz)

call MPI_Barrier( gcomm, ierr )

write(myid_ext, '(I5.5)') myid
write(cond_ext, '(I3.3)') ic
dirname = '../post/filtfields/'

!----------------------------------------
!filename=trim(dirname)//'field.'//cond_ext//'.'//myid_ext
filename=trim(dirname)//'field.'//myid_ext

open(unit=201,file=trim(filename),status='unknown',form='unformatted')
  call readwrite_filtfile_data(io,201,'r',uf,yf,wf,pf,cf)
close(201)

!----------------------------------------
call MPI_Barrier( gcomm, ierr )
return
end subroutine read_filtfields


! reads and writes data for save file in double precision
subroutine readwrite_filtfile_data(io,io_savefile,input,uf,yf,wf,pf,cf)
use topology_m, only : myid
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
!use runtime_m, only : time, tstep, time_save
!use variables_m, only : temp, pressure, yspecies, u
!use bc_m, only : pout

implicit none
integer io, io_savefile
character*1 input

real uf(nx*ny*nz*3),yf(nx*ny*nz*n_spec),wf(nx*ny*nz*n_spec),pf(nx*ny*nz),cf(nx*ny*nz)


if(input .ne. 'r' .and. input .ne. 'w') then
  if(myid.eq.0) then
    write(io,*) 'improper setting for variable input'
    write(io,*) 'in routine readwrite_savefile_data'
  endif
  call terminate_run(io,0)  !must be called by all processors
  return
end if

if(input.eq.'w') then
  write(io_savefile) uf
  write(io_savefile) yf
  write(io_savefile) wf
  write(io_savefile) pf
  write(io_savefile) cf
elseif(input.eq.'r') then
  read(io_savefile) uf
  read(io_savefile) yf
  read(io_savefile) wf
  read(io_savefile) pf
  read(io_savefile) cf
endif

return
end subroutine readwrite_filtfile_data
!***********************************************
! There are two versions of accum planar. The first is useful if 
!    1) you want to report things averaged all along z,
!    2) you want to report an average centred on one z position, and you have only one processor in the z direction.
!
! The other version is useful if you want to report things at 
! multiple z positions or you have several processors in the z direction, but it is much slower
!
subroutine accum_planar(field, cond, wt, windowx, windowy, windowz, total, totSQ,io)
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
use stat_util_m, only: calc_z_mean_numden_window, calc_XYfld_Xmovsum, calc_XYfld_Ymovsum
  
implicit none
real, dimension(nx,ny,nz), intent(in) :: field, wt
logical, dimension(nx,ny,nz), intent(in) :: cond
integer,intent(in) :: io, windowx, windowy, windowz

real, dimension(nx,ny,nz) :: total, totsq

real, dimension(nx,ny) :: fsum, wsum
integer k

!23dec09!  call calc_z_mean_numden(field, cond, wt, fsum, wsum)
!23dec09  call calc_z_mean_numden_window(field, cond, wt, windowz, nz/2, fsum, wsum)
!23dec09  call calc_XYfld_Xmovsum(fsum, windowx)
!23dec09  call calc_XYfld_Ymovsum(fsum, windowy)
!23dec09  total(:,:,1)=total(:,:,1)+fsum(:,:)
!23dec09  totsq(:,:,1)=totsq(:,:,1)+fsum(:,:)*fsum(:,:)
!23dec09  do k=1,nz
!23dec09   total(:,:,k)=total(:,:,1)
!23dec09   totsq(:,:,k)=totsq(:,:,1)
!23dec09  enddo
  do k=1,nz
!  call calc_z_mean_numden(field, cond, wt, fsum, wsum)
  call calc_z_mean_numden_window(field, cond, wt, windowz, k, fsum, wsum)
  call calc_XYfld_Xmovsum(fsum, windowx)
  call calc_XYfld_Ymovsum(fsum, windowy)
  total(:,:,k)=total(:,:,k)+fsum(:,:)
  totsq(:,:,k)=totsq(:,:,k)+fsum(:,:)*fsum(:,:)
  enddo

return
end subroutine accum_planar
!***********************************************
!20nov09!***********************************************
!20nov09subroutine accum_planar(field, cond, wt, windowx, windowy, windowz, total, totSQ,io)
!20nov09use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
!20nov09use stat_util_m, only: calc_z_mean_numden_window, calc_XYfld_Xmovsum, calc_XYfld_Ymovsum, calc_Zfld_Zmovsum
!20nov09  
!20nov09implicit none
!20nov09real, dimension(nx,ny,nz), intent(in) :: field, wt
!20nov09logical, dimension(nx,ny,nz), intent(in) :: cond
!20nov09integer,intent(in) :: io, windowx, windowy, windowz
!20nov09
!20nov09real, dimension(nx,ny,nz) :: total, totsq
!20nov09
!20nov09real, dimension(nx,ny) :: fsum
!20nov09real, dimension(nz) :: gsum
!20nov09integer i,j,k
!20nov09
!20nov09  do k=1,nz   ! first do the windows in X and Y:
!20nov09    fsum(:,:)=field(:,:,k)
!20nov09    call calc_XYfld_Xmovsum(fsum, windowx)
!20nov09    call calc_XYfld_Ymovsum(fsum, windowy)      
!20nov09    total(:,:,k)=fsum(:,:)
!20nov09  enddo
!20nov09  do j=1,ny  
!20nov09  do i=1,nx
!20nov09    gsum(:)=total(i,j,:)
!20nov09    call calc_Zfld_Zmovsum(gsum, windowz)
!20nov09    total(i,j,:)=gsum(:)
!20nov09    totsq(i,j,:)=gsum(:)*gsum(:)   !not sure what totsq is useful unless you do separate summation of field**2 then it can be used for errors.
!20nov09  enddo
!20nov09  enddo
!20nov09  
!20nov09return
!20nov09end subroutine accum_planar
!***********************************************


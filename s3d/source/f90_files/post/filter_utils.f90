#include "globalDefines.h"

SUBROUTINE GENERALIZED_FILTER( io, field, filfield, nxf, nyf, nzf, fx, fy, fz, method, delta, filinflu, xval, zval)
  ! ==================================================================================================
  ! GENERALIZED FILTER ROUTINE - RAY GROUT , APRIL 2008
  ! INPUT:
  !      io          UNIT FOR MESSAGES
  !      field       INPUT FIELD 
  !      filfield    OUTPUT FIELD WITH DIMENSIONS (nxf, nyf, nzf)
  !      fx, fy, fz  OUTPUT GRID ON EACH PROC SHOULD BE STORED IN fx(:,myid+1)
  !      method      1 FOR BOX FILTER, 2 FOR GAUSSIAN
  !      delta       FILTER WIDTH
  !      filinflu    CUTOFF DISTANCE FOR CALCULATION OF CONTRIBUTION
  !                  (USE DELTA/2 FOR OPTIMAL BOX FILTER CALCULATION)
  ! ==================================================================================================

  ! WARNING: this is hardwired to only add contributions in the vicinity of xval,zval - take care


  ! MODULES AND VARIABLES ============================================================================
    use topology_m
    use param_m, only : nx, ny, nz, npx, npy, npz
    use reference_m
    use grid_m, only : x, y, z
  
    IMPLICIT NONE

    ! DECLARATIONS PASSED IN
    INTEGER nxf, nyf, nzf
    REAL, DIMENSION(nx,ny,nz) :: field
    REAL, DIMENSION(nxf,nyf,nzf) :: filfield
    INTEGER, intent(in) :: io, method
    REAL, DIMENSION(nxf,npx*npy*npz) :: FX
    REAL, DIMENSION(nyf,npx*npy*npz) :: FY
    REAL, DIMENSION(nzf,npx*npy*npz) :: FZ
    REAL, intent(in) :: filinflu, delta
    REAL delta1, filinflu1
    REAL delta1x, delta1y, delta1z

    ! SCRATCH / WORKING VARIABLES
    REAL, DIMENSION(nxf,nyf,nzf) :: filfield_norm
    REAL, DIMENSION(nxf,nyf,npx*npy*npz) :: contrib
    REAL, DIMENSION(nxf,nyf,npx*npy*npz) :: contrib_norm_fac

    
    REAL, DIMENSION(nx,npx*npy*npz) :: XG ! To store x/y/z grid from all nodes
    REAL, DIMENSION(ny,npx*npy*npz) :: YG
    REAL, DIMENSION(nz,npx*npy*npz) :: ZG


    INTEGER i,j,k, ifil,jfil,kfil, id ! counters

    REAL G1 

    REAL XVAL(3), ZVAL(3), XINFLU, ZINFLU

    ! These take part in array calculations to let compiler vectorize easily
    real r(nxf), G(nx)
    logical computecontrib(nxf)

    real deltax(nx), deltay(ny), deltaz(nz) ! This is distance from filter grid point for kernel

    real dx(nx), dy(ny), dz(nz), dv(nx,ny,nz)  ! This is for differential

    
    ! SETUP WORKING GRIDS AND DIFFERENTIAL VOLUME =====================================================

    ! Sync ouput grid across processes
    DO id = 0,npx*npy*npz-1
       CALL MPI_Bcast( FX(:,id+1), nxf, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
       CALL MPI_Bcast( FY(:,id+1), nyf, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
       CALL MPI_Bcast( FZ(:,id+1), nzf, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
    ENDDO

    ! Move global input grid to all 
    XG(:,myid+1) = x
    YG(:,myid+1) = y
    ZG(:,myid+1) = z
    DO id = 0,npx*npy*npz-1
       CALL MPI_Bcast( XG(:,id+1), nx, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
       CALL MPI_Bcast( YG(:,id+1), ny, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
       CALL MPI_Bcast( ZG(:,id+1), nz, MPI_DOUBLE_PRECISION, id, gcomm, ierr)
    ENDDO



    ! Compute dv for each node on this proc, referring to neighbours as necessary at edges
    do i=2,nx-1
       dx(i) = 0.5d0*( x(i+1) - x(i-1) )
    enddo
    if( neighbor(1) >= 0) then
       dx(1) = 0.5d0*(x(2) - XG(nx,neighbor(1)+1) )
    else
       dx(1) = x(2) - x(1) 
    endif
    if( neighbor(2) >= 0) then
       dx(nx) = 0.5d0*(XG(1,neighbor(2)+1) - x(nx-1))
    else
       dx(nx) = x(nx) - x(nx-1)
    endif

      

    do i=2,ny-1
       dy(i) = 0.5d0*( y(i+1) - y(i-1) )
    enddo
    if( neighbor(3) >= 0) then
       dy(1) = 0.5d0*(y(2) - YG(ny,neighbor(3)+1))
    else
       dy(1) = y(2) - y(1)          
    endif
    if( neighbor(4) >=0 ) then
       dy(ny) = 0.5*(YG(1,neighbor(4)+1) - y(ny-1) )
    else
       dy(ny) = y(ny) - y(ny-1)   
    endif



    
    do i=2,nz-1
       dz(i) = 0.5d0*( z(i+1) - z(i-1) )
    enddo
    if( neighbor(5) >=0) then
       dz(1) = 0.5*(z(2) - ZG(nz,neighbor(5)+1))
    else
       dz(1) = z(2) - z(1) 
    endif
    if( neighbor(6) >=0) then
       dz(nz) = 0.5*(ZG(1,neighbor(6)+1) - z(nz-1))
    else
       dz(nz) = z(nz) - z(nz-1)
    endif

    

    
    do k=1,nz
       do j=1,ny
          do i=1,nx
             dv(i,j,k) =dx(i) * dy(j) * dz(k)
          enddo
       enddo
    enddo

    ! SET FILTER PARAMETERS ======================================================================
    delta1 = delta/l_ref
    delta1x = delta1 ! * 10.0
    delta1y = delta1
    delta1z = delta1 * 30.0
    filinflu1 = filinflu/l_ref

    if( method == 1 ) then
!       G1 = 1.0/delta1**3 ! Constant for box filter
       G1 = 1.0/delta1x/delta1y/delta1z
    else if(method == 2) then
       G1 = sqrt( 6.0d0 / 3.14149 / delta1**6 ) ! Constant for Gaussian
    else
       write(*,*) 'method must be 1 (box) or 2 (Gaussian)'
       stop
    endif


    ! COMPUTE CONTRIBUTION TO FILTERED GRID POINTS FROM LOCAL POINTS ============================
    DO kfil =1,nzf ! Loop over k-planes in output grid so that working buffer needs to only be of size nxf*nyf
!       if(myid==0) write(*,*)'kfil=',kfil
       contrib_norm_fac = 0
       contrib = 0

       DO id = 0, npx*npy*npz-1 ! Loop over contribution to all other processes bit of output grid
          DO jfil = 1, nyf  ! Loop over output grid j-planes
             
             ! Check to see how close (conservatively) a point here could be to a bit of the output
             ! grid on proc id
             r = ( FX(:,id+1) - x((nx/2)) )**2 &
                  + ( FY(jfil,id+1)- y((ny/2)) )**2 &
                  + ( FZ(kfil,id+1) - z((nz/2)) )**2 

             r = sqrt(r) - sqrt( (x(nx)-x(1))**2 + (y(ny) - y(1))**2 + (z(nz) - z(1))**2 ) 

             WHERE( r < 0 )
                r = 0
             ELSEWHERE
                r = r / 1.732 ! dx = dy = dz with same r = r/sqrt(3)
             END WHERE

             WHERE( r > filinflu1 )
                computecontrib =  .FALSE.
             ELSEWHERE
                computecontrib = .TRUE.
             END WHERE

! check whether the point is within one filter half width plus one stencil half width.
! we assume that the stencil half width is four times the distance between output grid points
! since the output grid is usually equal or coarser than the input grid.
if(xval(1).gt.0.0)then
xinflu=5.0*(x(2)-x(1))+delta1x/2.0
xinflu=xinflu*xinflu
WHERE( (FX(:,id+1)-xval(1))**2.0.gt. xinflu .and. &
       (FX(:,id+1)-xval(2))**2.0.gt. xinflu .and. &
       (FX(:,id+1)-xval(3))**2.0.gt. xinflu ) 
computecontrib = .FALSE.
END WHERE
endif


zinflu=5.0*(z(2)-z(1))+delta1z/2.0
zinflu=zinflu*zinflu
IF(    zval(1).gt.0.0 .and. &
       (FZ(kfil,id+1)-zval(1))**2.0.gt. zinflu .and. &
       (FZ(kfil,id+1)-zval(2))**2.0.gt. zinflu .and. &
       (FZ(kfil,id+1)-zval(3))**2.0.gt. zinflu ) THEN 
computecontrib(:) = .FALSE.
ENDIF

             
             DO ifil = 1,nxf ! Loop over output i-planes
                
                IF( computecontrib(ifil) ) THEN ! Check to see if it's worth computing contrib

                   deltaz = ABS( FZ(kfil,id+1) - z )
                   deltay = ABS( FY(jfil,id+1) - y )
                   deltax = ABS( FX(ifil,id+1) - x )

                   IF( method == 1) THEN 
                      ! For box filter. If things get slow, comment this bit (including the if) when using gaussian
                      ! especially for fine output grids
                      G = 0
                      DO k=1,nz
                         if( deltaz(k) .LE. delta1z/2 ) THEN
                            DO j=1,ny
                               IF(deltay(j) .LE. delta1y/2 ) THEN
                                  WHERE (deltax  .LE. delta1x/2 )
                                     G = 1
                                  ELSEWHERE
                                     G = 0
                                  END WHERE
                                  G = G*dv(:,j,k)*G1
                                  
                                  contrib(ifil,jfil, id+1) = contrib(ifil,jfil, id+1) + SUM(G * field(:,j,k) )
!esr                                  contrib_norm_fac(ifil,jfil, id+1) = contrib_norm_fac(ifil,jfil, id+1) + SUM(G) 
                               ENDIF
                            ENDDO
                         ENDIF
                      ENDDO
                   ELSE IF (method == 2) THEN
                      ! For Gaussian filter. If things get slow, comment this bit (including the if) when using box filter
                      ! especially for fine output grids
                      G = 0
                      DO k=1,nz
                         if( deltaz(k) .le. filinflu1 ) THEN
                            DO j=1,ny
                               if(deltay(j) .le. filinflu1 ) THEN
                                  WHERE (deltax .le. filinflu1 )
                                     G = exp ( -6.0d0 / delta1**2 * (deltaz(k)**2 + deltay(j)**2 + deltax**2 ) )
                                  ELSEWHERE 
                                     G = 0
                                  END WHERE
                                  G = G*dv(:,j,k)*G1
                                  
                                  contrib(ifil,jfil, id+1) = contrib(ifil,jfil, id+1) + SUM(G * field(:,j,k) )
!esr                                  contrib_norm_fac(ifil,jfil, id+1) = contrib_norm_fac(ifil,jfil, id+1) + SUM(G) 
                               ENDIF
                            ENDDO
                         ENDIF
                      ENDDO
                   END IF
                   
                ENDIF ! end if ( computercontrib(ifil) )

             ENDDO ! Loop over ifil
          ENDDO ! Loop over jfil
       ENDDO ! Loop over id

       ! Wait for all contributions to be available before we start reduction
       call mpi_barrier(gcomm,ierr)

       DO id=0,npx*npy*npz-1
          CALL MPI_Reduce( contrib(:,:,id+1), filfield(:,:,kfil),&
               nxf*nyf, MPI_DOUBLE_PRECISION, MPI_SUM, id, gcomm, ierr)

!esr          CALL MPI_Reduce( contrib_norm_fac(:,:,id+1), filfield_norm(:,:,kfil),&
!esr               nxf*nyf, MPI_DOUBLE_PRECISION, MPI_SUM, id, gcomm, ierr)

       ENDDO


    ENDDO
    
    ! Should be synced up here anyway after global reduction but make sure
    call mpi_barrier(gcomm,ierr) 

!    write(*,*)myid, 'max filfield_norm=',MAXVAL(filfield_norm)
!    write(*,*)myid, 'min filfield_norm=',MINVAL(filfield_norm)


!    where(filfield_norm < 1e-32)
!       filfield_norm = 1
!    end where

!esr    filfield = filfield / filfield_norm
    
END SUBROUTINE generalized_filter



    SUBROUTINE write_field(io, field, nxf, nyf, nzf, FX, FY, FZ, filename)
    
      USE HDF5
      use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
      use topology_m
      use reference_m
    
      IMPLICIT NONE
      ! INPUT VARIABLES
      INTEGER io
      INTEGER nxf, nyf, nzf
      REAL FX(nxf, npx*npy*npz), FY(nyf, npx*npy*npz), FZ(nzf, npx*npy*npz)
      REAL, intent(in):: field(nxf,nyf,nzf)
      REAL :: buffer(nxf,nyf,nzf)
      CHARACTER(LEN=8) :: dsetname   ! Dataset name
      CHARACTER(LEN=20) :: fieldname  ! Field name
      CHARACTER(LEN=50) :: filename   ! File name
      
      LOGICAL :: overwrite
      INTEGER :: error
      
      ! Scratch variables
    !  integer mypx, mypy, mypz   ! already allocated in topology
      
      ! HDF variables
      INTEGER(HID_T) :: file_id       ! File identifier 
      INTEGER(HID_T) :: dset_id,x_id,y_id,z_id       ! Dataset identifier 
      INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
      INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
      INTEGER(HSIZE_T), DIMENSION(3) :: dimsf, blockdim, offset  ! Dataset dimensions: global, local, local offset
      INTEGER(HSIZE_T), DIMENSION(3) :: dimschk
      INTEGER OS(3)
      INTEGER(HSIZE_T), DIMENSION(3) :: stride, count
      INTEGER, DIMENSION(NPX) :: COUNTX
      INTEGER, DIMENSION(NPY) :: COUNTY
      INTEGER, DIMENSION(NPZ) :: COUNTZ
      INTEGER, DIMENSION(npx*npy*npz) :: COUNTXG, COUNTYG, COUNTZG
      INTEGER pcounter, countpx, countpy, countpz
      INTEGER :: rank = 3 ! Dataset rank 
      real, dimension(nxf,nyf,nzf) :: grid_store
      integer req
      ! counters
      INTEGER i,j,k
      !----------------------------------------------------------------------
    
      dimsf(1) = nxf*npx
      dimsf(2) = nyf*npy
      dimsf(3) = nzf*npz
    
    
      ! Compute offset on each process from global grid
        mypz = myid/(npx*npy)
        mypx = MOD(myid-(mypz*npx*npy), npx)
        mypy = (myid-(mypz*npx*npy))/npx
    
        ! Dimensions of output block on this process
        blockdim(1) = nxf
        blockdim(2) = nyf
        blockdim(3) = nzf
    
        
        call MPI_Allgather( blockdim(1), 1, MPI_INTEGER, countxg, 1, MPI_INTEGER, gcomm, ierr)
        call MPI_Allgather( blockdim(2), 1, MPI_INTEGER, countyg, 1, MPI_INTEGER, gcomm, ierr)
        call MPI_Allgather( blockdim(3), 1, MPI_INTEGER, countzg, 1, MPI_INTEGER, gcomm, ierr)
        
    
        offset = 0
        dimschk = 0
        ! setup countx/y/z in grid topology coordinates.
        DO pcounter = 0,npx*npy*npz-1
           countpz = pcounter/(npx*npy)
           countpx = MOD(pcounter-(countpz*npx*npy), npx)
           countpy = (pcounter-(countpz*npx*npy))/npx
                  
           if( countpx == mypx ) then
              if( countpy == mypy ) then
                 countz(countpz+1) = countzg(pcounter+1)
              endif
              if( countpz == mypz ) then
                 county(countpy+1) = countyg(pcounter+1)
              endif
           endif
           if(countpy == mypy .and. countpz == mypz) then
              countx(countpx+1) =countxg(pcounter+1)
           endif
        ENDDO
    !    write(*,*) 'myid,countxyz=',myid,countx,county,countz
        ! Count up grid points on other processes, check total number
        DO pcounter = 1, npx
           IF(pcounter <= mypx ) then
              offset(1) = offset(1) + countx(pcounter)
           ENDIF
           dimschk(1) = dimschk(1) + countx(pcounter)
        ENDDO
        
        DO pcounter = 1, npy
           IF(pcounter <= mypy ) then
              offset(2) = offset(2) + county(pcounter)
           ENDIF
           dimschk(2) = dimschk(2) + county(pcounter)
        ENDDO
        
        DO pcounter = 1, npz
           IF(pcounter <= mypz ) then
              offset(3) = offset(3) + countz(pcounter)
           ENDIF
           dimschk(3) = dimschk(3) + countz(pcounter)
        ENDDO
        
    !    write(*,*) 'myid,offset=',myid,offset
        ! Get global file dimensions (use count as scracth space)
        CALL MPI_Allreduce(dimschk(1), count(1), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
        CALL MPI_Allreduce(dimschk(2), count(2), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
        CALL MPI_Allreduce(dimschk(3), count(3), 1, MPI_INTEGER, MPI_MAX, gcomm, error) 
        
        if(myid==0) then
           do i = 1,3
           if (dimsf(i).ne.count(i)) then
              write(*,*) 'error, count mismatch: dimsf/dimschk=',dimsf, count
           endif
           enddo
        endif
        
      ! End compute offset 
    
    
      
    
        
      ! Do output on root only
    
        if(myid==0) then
    !       filename = 'filtered.h5'
           
           CALL h5open_f(error) 
           CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
           CALL h5screate_simple_f(rank, dimsf, filespace, error)
           
           fieldname = 'f1'    
           CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
                dset_id, error)
           fieldname = 'x'    
           CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
                x_id, error)
           fieldname = 'y'    
           CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
                y_id, error)
           fieldname = 'z'    
           CALL h5dcreate_f(file_id, fieldname, H5T_NATIVE_REAL, filespace, &
                z_id, error)
    
    
    
           CALL h5screate_simple_f(rank, blockdim, memspace, error)    ! create memory space
           stride = 1
           count = 1
           CALL h5dget_space_f(dset_id, filespace, error)
    
           ! First dump what's here
           CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, blockdim, error, &
                stride, count)          
           
    !       write(*,*)'offset=',offset
    !       write(*,*)'blockdim=',blockdim
    !       write(*,*)'stride=',stride
    !       write(*,*)'count=',count
    !       write(*,*)'field(1,1,1)=',field(1,1,1)
           
           CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, field, dimsf, error, &
                file_space_id = filespace, mem_space_id = memspace)
    
           DO k=1,nzf
              DO j=1,nyf
                 grid_store(:,j,k) = fx(:,myid+1)*l_ref*1.0e3
              ENDDO
           ENDDO
           CALL h5dwrite_f(x_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
                file_space_id = filespace, mem_space_id = memspace)
    
           DO k=1,nzf
              do j=1,nyf
                 DO i=1,nxf
                    grid_store(i,j,k) = fy(j,myid+1)*l_ref*1.0e3
                 enddo
              ENDDO
           ENDDO
           CALL h5dwrite_f(y_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
                file_space_id = filespace, mem_space_id = memspace)
    
           do k = 1,nzf
           DO j=1,nyf
              DO i=1,nxf
                 grid_store(i,j,k) = fz(k,myid+1)*l_ref*1.0e3
                 enddo
              ENDDO
           ENDDO
           CALL h5dwrite_f(z_id, H5T_NATIVE_DOUBLE, grid_store, dimsf, error, &
                file_space_id = filespace, mem_space_id = memspace)
    
    !       write(*,*)'bd, nx/y/z f',blockdim, nxf, nyf, nzf
           DO pcounter=1,npx*npy*npz-1
              CALL MPI_Recv(os,rank, MPI_INTEGER, pcounter, 15, gcomm, req, ierr)
              offset=os
    !          write(*,*) 'root recieved offset from',pcounter,offset
    
              CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
                   , MPI_DOUBLE_PRECISION, pcounter, 16, gcomm, req, ierr)
    
              CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, offset, blockdim, error, &
                   stride, count)          
              
              CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
                   file_space_id = filespace, mem_space_id = memspace)
    
              CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
                   , MPI_DOUBLE_PRECISION, pcounter, 26, gcomm, req, ierr)
              CALL h5dwrite_f(x_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
                   file_space_id = filespace, mem_space_id = memspace)
    !
             CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
                   , MPI_DOUBLE_PRECISION, pcounter, 36, gcomm, req, ierr)
              CALL h5dwrite_f(y_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
                   file_space_id = filespace, mem_space_id = memspace)
    !
              CALL MPI_Recv(buffer, blockdim(1)*blockdim(2)*blockdim(3) &
                   , MPI_DOUBLE_PRECISION, pcounter, 46, gcomm, req, ierr)
              CALL h5dwrite_f(z_id, H5T_NATIVE_DOUBLE, buffer, dimsf, error, &
                   file_space_id = filespace, mem_space_id = memspace)
    !
           ENDDO          
    
           
           CALL h5dclose_f(dset_id, error)
           CALL h5sclose_f(filespace, error)
           CALL h5sclose_f(memspace, error)
           CALL h5fclose_f(file_id, error)
           CALL h5close_f(error)
           
        ELSE
    !esr       if(myid==241) write(*,*) 'id241 is sending offset=',offset
           os = offset
           CALL MPI_Send( os, rank, MPI_INTEGER, 0, 15, gcomm,req,  ierr)
    
           CALL MPI_Send( field, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
                , 0, 16, gcomm, req,  ierr)
    
           DO k=1,nzf
              DO j=1,nyf
                 grid_store(:,j,k) = fx(:,myid+1)*l_ref*1.0e3
              ENDDO
           ENDDO
          CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
               , 0, 26, gcomm,req,  ierr)
    
           DO k=1,nzf
              do j=1,nyf
              DO i=1,nxf
                 grid_store(i,j,k) = fy(j,myid+1)*l_ref*1.0e3
              ENDDO
           enddo
           ENDDO
          
           CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
                , 0, 36, gcomm,  req, ierr)
           do k=1,nzf
           DO j=1,nyf
              DO i=1,nxf
                 grid_store(i,j,k) = fz(k,myid+1)*l_ref*1.0e3
              ENDDO
           ENDDO
        enddo
           CALL MPI_Send( grid_store, blockdim(1)*blockdim(2)*blockdim(3), MPI_DOUBLE_PRECISION &
                , 0, 46, gcomm,  req, ierr)
    !
    
    
        endif
    END SUBROUTINE WRITE_FIELD


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

subroutine write_filtfields(io,uf,yf,wf)
use topology_m, only : myid, gcomm, ierr
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
use reference_m, only : time_ref

implicit none
integer,intent(in) :: io

character*5 :: myid_ext*5, filename*100, dirname*100

real uf(nx*ny*nz*3),yf(nx*ny*nx*n_spec),wf(nx*ny*nx*n_spec)

call MPI_Barrier( gcomm, ierr )

write(myid_ext, '(I5.5)') myid
dirname = '../post/filtfields/'

!----------------------------------------
filename=trim(dirname)//'field.'//myid_ext
open(unit=201,file=trim(filename),status='unknown',form='unformatted')
  call readwrite_filtfile_data(io,201,'w',uf,yf,wf)
close(201)

!----------------------------------------
call MPI_Barrier( gcomm, ierr )
return
end subroutine write_filtfields


!***********************************************
!***********************************************

subroutine read_filtfields(io,uf,yf,wf)
use topology_m, only : myid, gcomm, ierr
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
use reference_m, only : time_ref
  
implicit none
integer,intent(in) :: io

character*5 :: myid_ext*5, filename*100, dirname*100

real uf(nx*ny*nz*3),yf(nx*ny*nx*n_spec),wf(nx*ny*nx*n_spec)

call MPI_Barrier( gcomm, ierr )

write(myid_ext, '(I5.5)') myid
dirname = '../post/filtfields/'

!----------------------------------------
filename=trim(dirname)//'field.'//myid_ext
open(unit=201,file=trim(filename),status='unknown',form='unformatted')
  call readwrite_filtfile_data(io,201,'r',uf,yf,wf)
close(201)

!----------------------------------------
call MPI_Barrier( gcomm, ierr )
return
end subroutine read_filtfields


! reads and writes data for save file in double precision
subroutine readwrite_filtfile_data(io,io_savefile,input,uf,yf,wf)
use topology_m, only : myid
use param_m, only: nx,ny,nz,n_spec, npx, npy,npz
!use runtime_m, only : time, tstep, time_save
!use variables_m, only : temp, pressure, yspecies, u
!use bc_m, only : pout

implicit none
integer io, io_savefile
character*1 input

real uf(nx*ny*nz*3),yf(nx*ny*nz*n_spec),wf(nx*ny*nz*n_spec)


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
elseif(input.eq.'r') then
  read(io_savefile) uf
  read(io_savefile) yf
  read(io_savefile) wf
endif

return
end subroutine readwrite_filtfile_data
!***********************************************
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

!  call calc_z_mean_numden(field, cond, wt, fsum, wsum)
  call calc_z_mean_numden_window(field, cond, wt, nz/2, windowz, fsum, wsum)
  call calc_XYfld_Xmovsum(fsum, windowx)
  call calc_XYfld_Ymovsum(fsum, windowy)
  total(:,:,1)=total(:,:,1)+fsum(:,:)
  totsq(:,:,1)=totsq(:,:,1)+fsum(:,:)*fsum(:,:)
  do k=1,nz
   total(:,:,k)=total(:,:,1)
   totsq(:,:,k)=totsq(:,:,1)
  enddo

return
end subroutine accum_planar
!***********************************************


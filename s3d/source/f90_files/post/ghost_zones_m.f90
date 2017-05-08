#include "globalDefines.h"
!$Id: ghost_zones_m.f90,v 1.1.2.1 2006/05/16 23:24:33 rsankar Exp $
!=========================================================================================
  module ghost_zones_m
!=========================================================================================
! Author: Evatt Hawkes 2002-2003
! module for creating a ghost zone around a processor domain

  implicit none
!-----------------------------------------------------------------------------------------
! variable declarations

! none as yet.  potentially could make the size of the ghost zone global, and thus the 
! MPI types associated with it.  If the type creation is being called more than once to 
! create the same type it makes sense, but if it is called in more than one way (i.e. if 
! differently sized ghost zones are required) then probably better to leave it like this.

!-----------------------------------------------------------------------------------------
public fill_ghost_zones
  contains




!=========================================================================================
subroutine fill_ghost_zones(field,nx_padlow,nx_padhigh,ny_padlow,ny_padhigh,nz_padlow,&
                            nz_padhigh,field_padded)
!=========================================================================================
! - author: Evatt Hawkes, September 2002
! - routine fills a ghost zone for a field variable using MPI
!
! - routine takes a field of local size (nx,ny,nz)
! - it is padded on the i=1 side by nx_padlow nodes
! - it is padded on the i=nx side by nx_padhigh nodes
! - the padded field is filled with the entries from the neighbouring process
! - similarly for the other directions
!
! - the ghost zones are filled sequentially in the yz, xz, and xy planes, each plane 
!   bigger than the last in order to efficiently transfer the edge and corner nodes
! - limited to ghost zones smaller than or equal to domain size
! - sizing of the result is not checked here... this must be taken care of in the calling
!   routine!
! - handles periodic boundaries, but ghost zones are not filled at other kinds of boundaries.
!
!  NOTE: this routine could be made more general now that it is a module, thus having an
!        explicit interface.  the size of the input array could be left to the calling 
!        routine, removing the dependence on param_m.  however the routine would still be 
!        s3d specific as the bc's and topology are hard coded here.
!-----------------------------------------------------------------------------------------
! modules used
  use param_m, only : nx,ny,nz, periodic_x, periodic_y, periodic_z
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! passed input arguments
  real, dimension(nx,ny,nz), intent(in) :: field   !field requiring a ghost zone
  integer, intent(in) :: nx_padlow,nx_padhigh,ny_padlow,ny_padhigh,nz_padlow,nz_padhigh
                         !the number of cells to pad in on the low and high index sides
                         !in the x, y, and z directions

! result arguments
  real, dimension(1-nx_padlow:nx+nx_padhigh,1-ny_padlow:ny+ny_padhigh, &
                  1-nz_padlow:nz+nz_padhigh), intent(out) :: field_padded   
                                                             !field padded with ghost zone
!-----------------------------------------------------------------------------------------   
! local declarations

  integer yzface_low_type,yzface_high_type  !MPI datatype for x-direction transfers
  integer xzface_low_type,xzface_high_type  !MPI datatype for y-direction transfers
  integer xyface_low_type,xyface_high_type  !MPI datatype for z-direction transfers

  integer lnbr(6)   !local processor topology neighbour variable
  integer req       !for MPI_Wait

! local variables for sizing (minimum and maximum indices for each dimension)
  integer :: nxmin,nxmax, &
             nymin,nymax, &
             nzmin,nzmax

  integer nm         !local start point integer in array passing
  integer :: error_flag = 0  !for bad input parameters for create_MPI_array_segment

!-----------------------------------------------------------------------------------------
! set local variables for sizing
  nxmin=1-nx_padlow
  nxmax=nx+nx_padhigh

  nymin=1-ny_padlow
  nymax=ny+ny_padhigh

  nzmin=1-nz_padlow
  nzmax=nz+nz_padhigh

! define MPI types for data transfer
! i.e. ghost "faces" for padding

! yz_faces:
  if (nx_padlow.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,   &
                                  nxmin,0,1,ny,1,nz,                     &
                                  MPI_REAL8, yzface_low_type, error_flag)
  endif
   
  if (nx_padhigh.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nx+1,nxmax,1,ny,1,nz,                   &
                                  MPI_REAL8, yzface_high_type, error_flag)
  endif 

! xz_faces:
  if (ny_padlow.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nxmin,nxmax,nymin,0,1,nz,               &
                                  MPI_REAL8, xzface_low_type, error_flag)
  endif
   
  if (ny_padhigh.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nxmin,nxmax,ny+1,nymax,1,nz,            &
                                  MPI_REAL8, xzface_high_type, error_flag)

  endif   

! xy_faces:
  if (nz_padlow.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nxmin,nxmax,nymin,nymax,nzmin,0,        &
                                  MPI_REAL8, xyface_low_type, error_flag)
  endif
   
  if (nz_padhigh.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nxmin,nxmax,nymin,nymax,nz+1,nzmax,     &
                                  MPI_REAL8, xyface_high_type, error_flag)
  endif   

!-----------------------------------------------------------------------------------------

! temporary for local topology neighbours
  lnbr=neighbor

! set periodic neighbours in the x-direction if required
  if ((nx_padlow.gt.0).or.(nx_padhigh.gt.0)) then
    if(periodic_x.eq.1) then
      if(xid.eq.0) then
        lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
      endif
      if(xid.eq.xpes-1)then
        lnbr(2) = zid*xpes*ypes+ yid*xpes+0
      endif
    endif   
  endif

! set periodic neighbours in the y-direction if required
  if ((ny_padlow.gt.0).or.(ny_padhigh.gt.0)) then
    if(periodic_y.eq.1) then
      if(yid.eq.0) then
        lnbr(3) = zid*xpes*ypes+(ypes-1)*xpes+xid
      endif
      if(yid.eq.ypes-1)then
        lnbr(4) = zid*xpes*ypes+(0     )*xpes+xid
      endif
    endif   
  endif

! set periodic neighbours in the z-direction if required
  if ((nz_padlow.gt.0).or.(nz_padhigh.gt.0)) then
    if(periodic_z.eq.1) then
      if(zid.eq.0) then
        lnbr(5) = (zpes-1)*xpes*ypes+yid*xpes+xid
      endif
      if(zid.eq.zpes-1)then
        lnbr(6) = 0*xpes*ypes   +yid*xpes+xid
      endif
    endif
  endif

! set the middle of the padded array
  field_padded(1:nx,1:ny,1:nz)=field(:,:,:)

! transfer in x-direction if required
  if (nx_padhigh.gt.0) then

    if(lnbr(1).ge.0) then
      call MPI_ISend(field_padded(1,1,1),1,yzface_high_type,lnbr(1),1,gcomm,req,ierr)
    endif

    if(lnbr(2).ge.0) then
      call MPI_Recv(field_padded(nx+1,1,1),1,yzface_high_type,lnbr(2),1,gcomm,status,ierr)
    endif

    if(lnbr(1).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

  if (nx_padlow.gt.0) then

    if(lnbr(2).ge.0) then
      nm = nx + 1 - nx_padlow
      call MPI_ISend(field_padded(nm,1,1),1,yzface_low_type,lnbr(2),     &
                                                             2,gcomm,req,ierr)
    endif

    if(lnbr(1).ge.0) then
      call MPI_Recv(field_padded(nxmin,1,1),1,yzface_low_type,lnbr(1),   &
                                                             2,gcomm,status,ierr)
    endif

    if(lnbr(2).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

! transfer in y-direction if required
  if (ny_padhigh.gt.0) then

    if(lnbr(3).ge.0) then
      call MPI_ISend(field_padded(nxmin,1,1),1,xzface_high_type,lnbr(3),    &
                                                               3,gcomm,req,ierr)
    endif

    if(lnbr(4).ge.0) then
      call MPI_Recv(field_padded(nxmin,ny+1,1),1,xzface_high_type,lnbr(4),  &
                                                               3,gcomm,status,ierr)
    endif

    if(lnbr(3).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

  if (ny_padlow.gt.0) then

    if(lnbr(4).ge.0) then
      nm = ny + 1 - ny_padlow
      call MPI_ISend(field_padded(nxmin,nm,1),1,xzface_low_type,lnbr(4),   &
                                                                 4,gcomm,req,ierr)
    endif

    if(lnbr(3).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,1),1,xzface_low_type,lnbr(3), &
                                                                 4,gcomm,status,ierr)
    endif

    if(lnbr(4).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

! transfer in z-direction if required
  if (nz_padhigh.gt.0) then

    if(lnbr(5).ge.0) then
      call MPI_ISend(field_padded(nxmin,nymin,1),1,xyface_high_type,lnbr(5),  &
                                                                  5,gcomm,req,ierr)
    endif

    if(lnbr(6).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,nz+1),1,xyface_high_type,lnbr(6), &
                                                                  5,gcomm,status,ierr)
    endif

    if(lnbr(5).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

  if (nz_padlow.gt.0) then

    if(lnbr(6).ge.0) then
      nm = nz + 1 - nz_padlow
      call MPI_ISend(field_padded(nxmin,nymin,nm),1,xyface_low_type,lnbr(6),   &
                                                                   6,gcomm,req,ierr)
    endif

    if(lnbr(5).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,nzmin),1,xyface_low_type,lnbr(5), &
                                                                   6,gcomm,status,ierr)
    endif

    if(lnbr(6).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif


! free MPI types

! yz_faces:
  if (nx_padlow.ge.1) then
    call MPI_Type_free(yzface_low_type,ierr)
  endif
   
  if (nx_padhigh.ge.1) then
    call MPI_Type_free(yzface_high_type,ierr)
  endif 

! xz_faces:
  if (ny_padlow.ge.1) then
    call MPI_Type_free(xzface_low_type,ierr)
  endif
   
  if (ny_padhigh.ge.1) then
    call MPI_Type_free(xzface_high_type,ierr)

  endif   

! xy_faces:
  if (nz_padlow.ge.1) then
    call MPI_Type_free(xyface_low_type,ierr)
  endif
   
  if (nz_padhigh.ge.1) then
    call MPI_Type_free(xyface_high_type,ierr)
  endif     

!-----------------------------------------------------------------------------------------
! all done
  return
  end subroutine fill_ghost_zones
!
!
!
!=========================================================================================
subroutine create_MPI_array_segment(imin,imax,jmin,jmax,kmin,kmax,is,ie,js,je,ks,ke,&
                                    input_type, array_segment_type, error_flag)
!=========================================================================================
! - author: Evatt Hawkes, September 2002
! routine creates an MPI data-type to pass a sub-segment of an array.
! array dimensions are (imin:inmax,jmin:jmax,kmin:kmax)
! segment extends over the range(is:ie,js:je,ks:ke)
!-----------------------------------------------------------------------------------------

!  use topology_m  !temporary for diagnostics

implicit none
  

! passed input arguments:
  integer, intent(in) :: imin,imax,jmin,jmax,kmin,kmax   ! array dimensions
  integer, intent(in) :: is,ie,js,je,ks,ke               ! segment dimensions
  integer, intent(in) :: input_type                      ! the input MPI data-type

! output arguments:
  integer, intent(out) :: array_segment_type ! the derived MPI data-type
  integer, intent(out) :: error_flag         ! =1 if an input error, 0 otherwise

! local variables:
  integer ni,nj         ! the number of elements in i and j directions
  integer xy_plane_type ! a local variable defining the type for the x-y planes 
                        ! corresponding to constant k
  integer sizeof        ! the size in bytes of the MPI input data-type 
  integer ierror        ! MPI error flag
!-----------------------------------------------------------------------------------------

! check the input data for errors:

  if (((imax.lt.imin).or.(jmax.lt.jmin)).or.(kmax.lt.kmin)) then
    error_flag=1
    return
  else if (((ie.lt.is).or.(je.lt.js)).or.(ke.lt.ks)) then
    error_flag=1
    return
  else if (((ie.gt.imax).or.(je.gt.jmax)).or.(ke.gt.kmax)) then
    error_flag=1
    return
  else if (((is.lt.imin).or.(js.lt.jmin)).or.(ks.lt.kmin)) then
    error_flag=1
    return
  else
!    error_flag=0
  endif

! set the number of elements
  ni=imax-imin+1
  nj=jmax-jmin+1
 
! define a data-type for the x-y planes:


  call MPI_Type_vector(je-js+1, ie-is+1, ni, input_type, xy_plane_type, ierror)
  call MPI_Type_commit(xy_plane_type,ierror)


! define a data-type consisting of a vector of these types:

  call MPI_Type_extent(input_type, sizeof, ierror)
  call MPI_Type_hvector(ke-ks+1, 1, ni*nj*sizeof, &
                        xy_plane_type, array_segment_type, ierror)
  call MPI_Type_commit(array_segment_type, ierror)


!-----------------------------------------------------------------------------------------
! all done
  return
  end subroutine create_MPI_array_segment
!-----------------------------------------------------------------------------------------
!
!=========================================================================================
subroutine fill_ghost_zones_vector(field,nx_padlow,nx_padhigh, &
                                         ny_padlow,ny_padhigh, &
                                         nz_padlow,nz_padhigh,field_padded)
!=========================================================================================
! - author: Evatt Hawkes, September 2002
! - routine fills a ghost zone for a vector field variable using MPI
!
! - routine takes a field of local size (nx,ny,nz)
! - it is padded on the i=1 side by nx_padlow nodes
! - it is padded on the i=nx side by nx_padhigh nodes
! - the padded field is filled with the entries from the neighbouring process
! - similarly for the other directions
!
! - the ghost zones are filled sequentially in the yz, xz, and xy planes, each plane 
!   bigger than the last in order to efficiently transfer the edge and corner nodes
! - limited to ghost zones smaller than or equal to domain size
! - sizing of the result is not checked here... this must be taken care of in the calling
!   routine
! - this routine requires an explicit interface!!! (required for sizing of number of 
!   field components)
!  NOTE: this routine could be made more general now that it is a module, thus having an
!        explicit interface.  the size of the input array could be left to the calling 
!        routine, removing the dependence on param_m.  however the routine would still be 
!        s3d specific as the bc's and topology are hard coded here.
!-----------------------------------------------------------------------------------------
! modules used
  use param_m
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! passed input arguments
  real, dimension(:,:,:,:), intent(in) :: field   !field requiring a ghost zone
                                                  !dimensions are nx,ny,nz,ncomp
                                                  !AVOID CALLING WITH WRONG DIMENSIONS

  integer, intent(in) :: nx_padlow,nx_padhigh,ny_padlow,ny_padhigh,nz_padlow,nz_padhigh
                         !the number of cells to pad in on the low and high index sides
                         !in the x, y, and z directions

! result arguments
  real, dimension(1-nx_padlow:nx+nx_padhigh,1-ny_padlow:ny+ny_padhigh, &
                  1-nz_padlow:nz+nz_padhigh,size(field,4)), intent(out) :: field_padded   
                                                             !field padded with ghost zone
!-----------------------------------------------------------------------------------------   
! local declarations

  integer yzface_low_type,yzface_high_type  !MPI datatype for x-direction transfers
  integer xzface_low_type,xzface_high_type  !MPI datatype for y-direction transfers
  integer xyface_low_type,xyface_high_type  !MPI datatype for z-direction transfers

  integer lnbr(6)   !local processor topology neighbour variable
  integer req       !for MPI_Wait

! local variables for sizing (minimum and maximum indices for each dimension)
  integer :: nxmin,nxmax, &
             nymin,nymax, &
             nzmin,nzmax

  integer ncomp ! the number of components of the vector array

  integer nm         !local start point integer in array passing
  integer :: error_flag = 0  !for bad input parameters for create_MPI_array_segment

!-----------------------------------------------------------------------------------------   
! set local variables for sizing
  nxmin=1-nx_padlow
  nxmax=nx+nx_padhigh

  nymin=1-ny_padlow
  nymax=ny+ny_padhigh

  nzmin=1-nz_padlow
  nzmax=nz+nz_padhigh

  ncomp=size(field,4)

! define MPI types for data transfer
! i.e. ghost "faces" for padding

! yz_faces:
  if (nx_padlow.ge.1) then
    call create_MPI_vector_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,ncomp,   &
                                  nxmin,0,1,ny,1,nz,                     &
                                  MPI_REAL8, yzface_low_type, error_flag)
  endif
   
  if (nx_padhigh.ge.1) then
    call create_MPI_vector_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,ncomp,    &
                                  nx+1,nxmax,1,ny,1,nz,                   &
                                  MPI_REAL8, yzface_high_type, error_flag)
  endif 
  
! xz_faces:
  if (ny_padlow.ge.1) then
    call create_MPI_vector_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,ncomp,   &
                                  nxmin,nxmax,nymin,0,1,nz,               &
                                  MPI_REAL8, xzface_low_type, error_flag)
  endif
   
  if (ny_padhigh.ge.1) then
    call create_MPI_vector_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,ncomp,    &
                                  nxmin,nxmax,ny+1,nymax,1,nz,            &
                                  MPI_REAL8, xzface_high_type, error_flag)

  endif   

! xy_faces:
  if (nz_padlow.ge.1) then
    call create_MPI_vector_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,ncomp,    &
                                  nxmin,nxmax,nymin,nymax,nzmin,0,        &
                                  MPI_REAL8, xyface_low_type, error_flag)
  endif
   
  if (nz_padhigh.ge.1) then
    call create_MPI_vector_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,ncomp,    &
                                  nxmin,nxmax,nymin,nymax,nz+1,nzmax,     &
                                  MPI_REAL8, xyface_high_type, error_flag)
  endif   

!-----------------------------------------------------------------------------------------

! temporary for local topology neighbours
  lnbr=neighbor

! set periodic neighbours in the x-direction if required
  if ((nx_padlow.gt.0).or.(nx_padhigh.gt.0)) then
    if(periodic_x.eq.1) then
      if(xid.eq.0) then
        lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
      endif
      if(xid.eq.xpes-1)then
        lnbr(2) = zid*xpes*ypes+ yid*xpes+0
      endif
    endif   
  endif


! set periodic neighbours in the y-direction if required
  if ((ny_padlow.gt.0).or.(ny_padhigh.gt.0)) then
    if(periodic_y.eq.1) then
      if(yid.eq.0) then
        lnbr(3) = zid*xpes*ypes+(ypes-1)*xpes+xid
      endif
      if(yid.eq.ypes-1)then
        lnbr(4) = zid*xpes*ypes+(0     )*xpes+xid
      endif
    endif   
  endif

! set periodic neighbours in the z-direction if required
  if ((nz_padlow.gt.0).or.(nz_padhigh.gt.0)) then
    if(periodic_z.eq.1) then
      if(zid.eq.0) then
        lnbr(5) = (zpes-1)*xpes*ypes+yid*xpes+xid
      endif
      if(zid.eq.zpes-1)then
        lnbr(6) = 0*xpes*ypes   +yid*xpes+xid
      endif
    endif
  endif

! set the middle of the padded array
  field_padded(1:nx,1:ny,1:nz,:)=field(:,:,:,:)

! transfer in x-direction if required
  if (nx_padhigh.gt.0) then

    if(lnbr(1).ge.0) then
      call MPI_ISend(field_padded(1,1,1,1),1,yzface_high_type,lnbr(1), &
                     1,gcomm,req,ierr)
    endif

    if(lnbr(2).ge.0) then
      call MPI_Recv(field_padded(nx+1,1,1,1),1,yzface_high_type,lnbr(2), &
                    1,gcomm,status,ierr)
    endif

    if(lnbr(1).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

  if (nx_padlow.gt.0) then

    if(lnbr(2).ge.0) then
      nm = nx + 1 - nx_padlow
      call MPI_ISend(field_padded(nm,1,1,1),1,yzface_low_type,lnbr(2),     &
                                                             2,gcomm,req,ierr)
    endif

    if(lnbr(1).ge.0) then
      call MPI_Recv(field_padded(nxmin,1,1,1),1,yzface_low_type,lnbr(1),   &
                                                             2,gcomm,status,ierr)
    endif

    if(lnbr(2).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

! transfer in y-direction if required
  if (ny_padhigh.gt.0) then

    if(lnbr(3).ge.0) then
      call MPI_ISend(field_padded(nxmin,1,1,1),1,xzface_high_type,lnbr(3),    &
                                                               3,gcomm,req,ierr)
    endif

    if(lnbr(4).ge.0) then
      call MPI_Recv(field_padded(nxmin,ny+1,1,1),1,xzface_high_type,lnbr(4),  &
                                                               3,gcomm,status,ierr)
    endif

    if(lnbr(3).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif


  if (ny_padlow.gt.0) then

    if(lnbr(4).ge.0) then
      nm = ny + 1 - ny_padlow
      call MPI_ISend(field_padded(nxmin,nm,1,1),1,xzface_low_type,lnbr(4),   &
                                                                 4,gcomm,req,ierr)
    endif

    if(lnbr(3).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,1,1),1,xzface_low_type,lnbr(3), &
                                                                 4,gcomm,status,ierr)
    endif

    if(lnbr(4).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

! transfer in z-direction if required
  if (nz_padhigh.gt.0) then

    if(lnbr(5).ge.0) then
      call MPI_ISend(field_padded(nxmin,nymin,1,1),1,xyface_high_type,lnbr(5),  &
                                                                  5,gcomm,req,ierr)
    endif

    if(lnbr(6).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,nz+1,1),1,xyface_high_type,lnbr(6), &
                                                                  5,gcomm,status,ierr)
    endif

    if(lnbr(5).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

  if (nz_padlow.gt.0) then

    if(lnbr(6).ge.0) then
      nm = nz + 1 - nz_padlow
      call MPI_ISend(field_padded(nxmin,nymin,nm,1),1,xyface_low_type,lnbr(6),   &
                                                                   6,gcomm,req,ierr)
    endif

    if(lnbr(5).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,nzmin,1),1,xyface_low_type,lnbr(5), &
                                                                   6,gcomm,status,ierr)
    endif

    if(lnbr(6).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

! free MPI types

! yz_faces:
  if (nx_padlow.ge.1) then
    call MPI_Type_free(yzface_low_type,ierr)
  endif
   
  if (nx_padhigh.ge.1) then
    call MPI_Type_free(yzface_high_type,ierr)
  endif 

! xz_faces:
  if (ny_padlow.ge.1) then
    call MPI_Type_free(xzface_low_type,ierr)
  endif
   
  if (ny_padhigh.ge.1) then
    call MPI_Type_free(xzface_high_type,ierr)

  endif   

! xy_faces:
  if (nz_padlow.ge.1) then
    call MPI_Type_free(xyface_low_type,ierr)
  endif
   
  if (nz_padhigh.ge.1) then
    call MPI_Type_free(xyface_high_type,ierr)
  endif   

!-----------------------------------------------------------------------------------------
! all done
  return
  end subroutine fill_ghost_zones_vector
!
!

!
!=========================================================================================
subroutine create_MPI_vector_array_segment(imin,imax,jmin,jmax,kmin,kmax,ncomp,       &
                                           is,ie,js,je,ks,ke,                         &
                                        input_type, vector_array_segment_type, error_flag)
!=========================================================================================
! - author: Evatt Hawkes, September 2002
! routine creates an MPI data-type to pass a sub-segment of a vector array.
! array dimensions are (imin:imax,jmin:jmax,kmin:kmax,1:ncomp)
! segment extends over the range(is:ie,js:je,ks:ke)
!-----------------------------------------------------------------------------------------

!  use topology_m  !temporary for diagnostics

implicit none
  

! passed input arguments:
  integer, intent(in) :: imin,imax,jmin,jmax,kmin,kmax   ! array dimensions
  integer, intent(in) :: ncomp                           ! number of vector components
  integer, intent(in) :: is,ie,js,je,ks,ke               ! segment dimensions
  integer, intent(in) :: input_type                      ! the input MPI data-type

! output arguments:
  integer, intent(out) :: vector_array_segment_type ! the derived MPI data-type
  integer, intent(out) :: error_flag                ! =1 if an input error, 0 otherwise

! local variables:
  integer ni,nj,nk      ! the number of elements in i, j and k directions
  integer xy_plane_type ! a local variable defining the type for the x-y planes 
                        ! corresponding to constant k
  integer scalar_array_segment_type ! a local variable defining of the type
                        ! for each component of the vector array
  integer sizeof        ! the size in bytes of the MPI input data-type 
  integer ierror        ! MPI error flag
!-----------------------------------------------------------------------------------------

! check the input data for errors:

  if (((imax.lt.imin).or.(jmax.lt.jmin)).or.(kmax.lt.kmin)) then
    error_flag=1
    return
  else if (((ie.lt.is).or.(je.lt.js)).or.(ke.lt.ks)) then
    error_flag=1
    return
  else if (((ie.gt.imax).or.(je.gt.jmax)).or.(ke.gt.kmax)) then
    error_flag=1
    return
  else if (((is.lt.imin).or.(js.lt.jmin)).or.(ks.lt.kmin)) then
    error_flag=1
    return
  else if (ncomp.lt.1) then
    error_flag=1
    return
  else
!    error flag is unchanged... do nothing!
!    error_flag=0
  endif

! set the number of elements
  ni=imax-imin+1
  nj=jmax-jmin+1
  nk=kmax-kmin+1
 
! define a data-type for the x-y planes:


  call MPI_Type_vector(je-js+1, ie-is+1, ni, input_type, xy_plane_type, ierror)
  call MPI_Type_commit(xy_plane_type,ierror)


! define a data-type consisting of a vector of these types:

  call MPI_Type_extent(input_type, sizeof, ierror)
  call MPI_Type_hvector(ke-ks+1, 1, ni*nj*sizeof, &
                        xy_plane_type, scalar_array_segment_type, ierror)
  call MPI_Type_commit(scalar_array_segment_type, ierror)


! define a data-type consisting of a vector of the scalar array types:

  call MPI_Type_hvector(ncomp, 1, ni*nj*nk*sizeof, &
                        scalar_array_segment_type, vector_array_segment_type, ierror)
  call MPI_Type_commit(vector_array_segment_type, ierror)

!-----------------------------------------------------------------------------------------
! all done
  return
  end subroutine create_MPI_vector_array_segment
!-----------------------------------------------------------------------------------------


!=========================================================================================
subroutine fill_ghost_zones_logical(field,nx_padlow,nx_padhigh,ny_padlow,ny_padhigh,nz_padlow,&
                            nz_padhigh,field_padded)
!=========================================================================================
! - author: Evatt Hawkes, September 2002
! - routine fills a ghost zone for a logical field variable using MPI
!
! - routine takes a field of local size (nx,ny,nz)
! - it is padded on the i=1 side by nx_padlow nodes
! - it is padded on the i=nx side by nx_padhigh nodes
! - the padded field is filled with the entries from the neighbouring process
! - similarly for the other directions
!
! - the ghost zones are filled sequentially in the yz, xz, and xy planes, each plane 
!   bigger than the last in order to efficiently transfer the edge and corner nodes
! - limited to ghost zones smaller than or equal to domain size
! - sizing of the result is not checked here... this must be taken care of in the calling
!   routine!
! - handles periodic boundaries, but ghost zones are not filled at other kinds of boundaries.
!
!  NOTE: this routine could be made more general now that it is a module, thus having an
!        explicit interface.  the size of the input array could be left to the calling 
!        routine, removing the dependence on param_m.  however the routine would still be 
!        s3d specific as the bc's and topology are hard coded here.
!-----------------------------------------------------------------------------------------
! modules used
  use param_m, only : nx,ny,nz, periodic_x, periodic_y, periodic_z
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! passed input arguments
  logical, dimension(nx,ny,nz), intent(in) :: field   !field requiring a ghost zone
  integer, intent(in) :: nx_padlow,nx_padhigh,ny_padlow,ny_padhigh,nz_padlow,nz_padhigh
                         !the number of cells to pad in on the low and high index sides
                         !in the x, y, and z directions

! result arguments
  logical, dimension(1-nx_padlow:nx+nx_padhigh,1-ny_padlow:ny+ny_padhigh, &
                  1-nz_padlow:nz+nz_padhigh), intent(out) :: field_padded   
                                                             !field padded with ghost zone
!-----------------------------------------------------------------------------------------   
! local declarations

  integer yzface_low_type,yzface_high_type  !MPI datatype for x-direction transfers
  integer xzface_low_type,xzface_high_type  !MPI datatype for y-direction transfers
  integer xyface_low_type,xyface_high_type  !MPI datatype for z-direction transfers

  integer lnbr(6)   !local processor topology neighbour variable
  integer req       !for MPI_Wait

! local variables for sizing (minimum and maximum indices for each dimension)
  integer :: nxmin,nxmax, &
             nymin,nymax, &
             nzmin,nzmax

  integer nm         !local start point integer in array passing
  integer :: error_flag = 0  !for bad input parameters for create_MPI_array_segment

!-----------------------------------------------------------------------------------------
! set local variables for sizing
  nxmin=1-nx_padlow
  nxmax=nx+nx_padhigh

  nymin=1-ny_padlow
  nymax=ny+ny_padhigh

  nzmin=1-nz_padlow
  nzmax=nz+nz_padhigh

! define MPI types for data transfer
! i.e. ghost "faces" for padding

! yz_faces:
  if (nx_padlow.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,   &
                                  nxmin,0,1,ny,1,nz,                     &
                                  MPI_LOGICAL, yzface_low_type, error_flag)
  endif
   
  if (nx_padhigh.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nx+1,nxmax,1,ny,1,nz,                   &
                                  MPI_LOGICAL, yzface_high_type, error_flag)
  endif 

! xz_faces:
  if (ny_padlow.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nxmin,nxmax,nymin,0,1,nz,               &
                                  MPI_LOGICAL, xzface_low_type, error_flag)
  endif
   
  if (ny_padhigh.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nxmin,nxmax,ny+1,nymax,1,nz,            &
                                  MPI_LOGICAL, xzface_high_type, error_flag)

  endif   

! xy_faces:
  if (nz_padlow.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nxmin,nxmax,nymin,nymax,nzmin,0,        &
                                  MPI_LOGICAL, xyface_low_type, error_flag)
  endif
   
  if (nz_padhigh.ge.1) then
    call create_MPI_array_segment(nxmin,nxmax,nymin,nymax,nzmin,nzmax,    &
                                  nxmin,nxmax,nymin,nymax,nz+1,nzmax,     &
                                  MPI_LOGICAL, xyface_high_type, error_flag)
  endif   

!-----------------------------------------------------------------------------------------

! temporary for local topology neighbours
  lnbr=neighbor

! set periodic neighbours in the x-direction if required
  if ((nx_padlow.gt.0).or.(nx_padhigh.gt.0)) then
    if(periodic_x.eq.1) then
      if(xid.eq.0) then
        lnbr(1) = zid*xpes*ypes+ yid*xpes+xpes-1
      endif
      if(xid.eq.xpes-1)then
        lnbr(2) = zid*xpes*ypes+ yid*xpes+0
      endif
    endif   
  endif

! set periodic neighbours in the y-direction if required
  if ((ny_padlow.gt.0).or.(ny_padhigh.gt.0)) then
    if(periodic_y.eq.1) then
      if(yid.eq.0) then
        lnbr(3) = zid*xpes*ypes+(ypes-1)*xpes+xid
      endif
      if(yid.eq.ypes-1)then
        lnbr(4) = zid*xpes*ypes+(0     )*xpes+xid
      endif
    endif   
  endif

! set periodic neighbours in the z-direction if required
  if ((nz_padlow.gt.0).or.(nz_padhigh.gt.0)) then
    if(periodic_z.eq.1) then
      if(zid.eq.0) then
        lnbr(5) = (zpes-1)*xpes*ypes+yid*xpes+xid
      endif
      if(zid.eq.zpes-1)then
        lnbr(6) = 0*xpes*ypes   +yid*xpes+xid
      endif
    endif
  endif

! set the middle of the padded array
  field_padded(1:nx,1:ny,1:nz)=field(:,:,:)

! transfer in x-direction if required
  if (nx_padhigh.gt.0) then

    if(lnbr(1).ge.0) then
      call MPI_ISend(field_padded(1,1,1),1,yzface_high_type,lnbr(1),1,gcomm,req,ierr)
    endif

    if(lnbr(2).ge.0) then
      call MPI_Recv(field_padded(nx+1,1,1),1,yzface_high_type,lnbr(2),1,gcomm,status,ierr)
    endif

    if(lnbr(1).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

  if (nx_padlow.gt.0) then

    if(lnbr(2).ge.0) then
      nm = nx + 1 - nx_padlow
      call MPI_ISend(field_padded(nm,1,1),1,yzface_low_type,lnbr(2),     &
                                                             2,gcomm,req,ierr)
    endif

    if(lnbr(1).ge.0) then
      call MPI_Recv(field_padded(nxmin,1,1),1,yzface_low_type,lnbr(1),   &
                                                             2,gcomm,status,ierr)
    endif

    if(lnbr(2).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

! transfer in y-direction if required
  if (ny_padhigh.gt.0) then

    if(lnbr(3).ge.0) then
      call MPI_ISend(field_padded(nxmin,1,1),1,xzface_high_type,lnbr(3),    &
                                                               3,gcomm,req,ierr)
    endif

    if(lnbr(4).ge.0) then
      call MPI_Recv(field_padded(nxmin,ny+1,1),1,xzface_high_type,lnbr(4),  &
                                                               3,gcomm,status,ierr)
    endif

    if(lnbr(3).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

  if (ny_padlow.gt.0) then

    if(lnbr(4).ge.0) then
      nm = ny + 1 - ny_padlow
      call MPI_ISend(field_padded(nxmin,nm,1),1,xzface_low_type,lnbr(4),   &
                                                                 4,gcomm,req,ierr)
    endif

    if(lnbr(3).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,1),1,xzface_low_type,lnbr(3), &
                                                                 4,gcomm,status,ierr)
    endif

    if(lnbr(4).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

! transfer in z-direction if required
  if (nz_padhigh.gt.0) then

    if(lnbr(5).ge.0) then
      call MPI_ISend(field_padded(nxmin,nymin,1),1,xyface_high_type,lnbr(5),  &
                                                                  5,gcomm,req,ierr)
    endif

    if(lnbr(6).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,nz+1),1,xyface_high_type,lnbr(6), &
                                                                  5,gcomm,status,ierr)
    endif

    if(lnbr(5).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

  if (nz_padlow.gt.0) then

    if(lnbr(6).ge.0) then
      nm = nz + 1 - nz_padlow
      call MPI_ISend(field_padded(nxmin,nymin,nm),1,xyface_low_type,lnbr(6),   &
                                                                   6,gcomm,req,ierr)
    endif

    if(lnbr(5).ge.0) then
      call MPI_Recv(field_padded(nxmin,nymin,nzmin),1,xyface_low_type,lnbr(5), &
                                                                   6,gcomm,status,ierr)
    endif

    if(lnbr(6).ge.0) then
      call MPI_Wait(req,status,ierr)
    endif

  endif

! free MPI types

! yz_faces:
  if (nx_padlow.ge.1) then
    call MPI_Type_free(yzface_low_type,ierr)
  endif
   
  if (nx_padhigh.ge.1) then
    call MPI_Type_free(yzface_high_type,ierr)
  endif 

! xz_faces:
  if (ny_padlow.ge.1) then
    call MPI_Type_free(xzface_low_type,ierr)
  endif
   
  if (ny_padhigh.ge.1) then
    call MPI_Type_free(xzface_high_type,ierr)

  endif   

! xy_faces:
  if (nz_padlow.ge.1) then
    call MPI_Type_free(xyface_low_type,ierr)
  endif
   
  if (nz_padhigh.ge.1) then
    call MPI_Type_free(xyface_high_type,ierr)
  endif   

!-----------------------------------------------------------------------------------------
! all done
  return
  end subroutine fill_ghost_zones_logical
!



!-----------------------------------------------------------------------------------------
  end module ghost_zones_m

!=========================================================================================
subroutine filter_feeddata(io)
!=========================================================================================
!-----------------------------------------------------------------------------------------
! filters the bunsen inlet velocity field and dumps to file
! Routine provided by Evatt Hawkes, University of New South Wales.
!
!----------------------------------------------------------------------------------------
! modules used
  use topology_m
  use param_m
  use variables_m
  use grid_m
  use reference_m
  use ghost_nice_m, only : ghostzone_real

  implicit none

!----------------------------------------------------------------------------------------
! input argument declarations

  integer, intent(in)  :: io
  
!-----------------------------------------------------------------------------------------
! local declarations

  integer :: i,j,k
  integer :: il,jl,kl
  integer :: ig,jg,kg
  integer :: L
  
  real :: deltax, deltay, deltaz
  integer :: nlesxg, nlesyg, nleszg
  real, allocatable :: xlesg(:), ylesg(:), zlesg(:)
  integer :: igst, jgst, kgst
  integer :: igend, jgend, kgend
  real :: xming, xmaxg, yming, ymaxg, zming, zmaxg
  integer, allocatable :: idnsst(:), idnsend(:)
  integer, allocatable :: jdnsst(:), jdnsend(:)
  integer, allocatable :: kdnsst(:), kdnsend(:)
  real, allocatable :: gfiltx(:,:), gfilty(:,:), gfiltz(:,:)
  integer :: nlesx, nlesy, nlesz
  integer :: countsx(npx), countsy(npy), countsz(npz)
  integer :: igstp(npx), jgstp(npy), kgstp(npz)
  integer :: igendp(npx), jgendp(npy), kgendp(npz)
  integer :: jid, kid, jkid
  integer :: ndx, ndy, ndz
  integer :: recpoint

  real :: dyg(ny_g)

  real, allocatable :: xpad(:), ypad(:), zpad(:)

  real, allocatable :: uk(:,:,:), ukj(:,:,:), udns(:,:,:), ules(:,:,:,:)
  real, allocatable :: urecv(:,:,:), uplane(:,:,:)
  
  integer :: req(2), statvec(MPI_STATUS_SIZE,2)

  real :: mindx, mindy, mindz

!-----------------------------------------------------------------------------------------
! executable statements
!-----------------------------------------------------------------------------------------

! set up filter size
!Evatt's values for filtering the Bunsen case C for Ed Knudsen:
  deltax=90.0E-6/l_ref
  deltay=36.0E-6/l_ref
  deltaz=56.0E-6/l_ref
!ESR's values for filtering the Lifted C2H4 case for Ed Knudsen:
  deltax=100.0E-6/l_ref
  deltay=37.30E-6/l_ref
  deltaz=62.50E-6/l_ref

! set up les grid
  nlesxg=floor((xmax+delx-xmin)/deltax)
  nlesyg=floor((ymax-ymin)/deltay)
  nleszg=floor((zmax+delz-zmin)/deltaz)

  if(myid.eq.0)write(io,*)'nlesxg,nlesyg,nleszg',nlesxg,nlesyg,nleszg

  mindx=minval(xg(2:nx_g)-xg(1:nx_g-1))
  mindy=minval(yg(2:ny_g)-yg(1:ny_g-1))
  mindz=minval(zg(2:nz_g)-zg(1:nz_g-1))

! set safe dns compute stencil for ghost zone sizing
! assumes uniform grid and same spacing in each direction
  ndx=int(deltax/mindx/2.0)+3
  ndy=int(deltay/mindy/2.0)+3
  ndz=int(deltaz/mindz/2.0)+3

 allocate(xpad(1-ndx:nx+ndx),ypad(1-ndy:ny+ndy),zpad(1-ndz:nz+ndz))

  xpad(1:nx)=xg(xid*nx+1:xid*nx+nx)
  do i=0,1-ndx,-1
    xpad(i)=xpad(i+1)-delx
  enddo
  do i=nx+1,nx+ndx
    xpad(i)=xpad(i-1)+delx
  enddo
  
  dyg(1:ny_g-1)=yg(2:ny_g)-yg(1:ny_g-1)
  dyg(ny_g)=dyg(ny_g-1)
  if((yid.gt.0).and.(yid.lt.(npy-1)))then
    ypad(1-ndy:ny+ndy)=yg(yid*ny+1-ndy:yid*ny+ny+ndy)
  else
    ypad(1:ny)=yg(yid*ny+1:yid*ny+ny)
    do j=0,1-ndy,-1
      ypad(j)=ypad(j+1)-(yg(2)-yg(1))
    enddo
    do j=ny+1,ny+ndy
      ypad(j)=ypad(j-1)+(yg(ny_g)-yg(ny_g-1))
    enddo
  endif
   
  zpad(1:nz)=zg(zid*nz+1:zid*nz+nz)
  do k=0,1-ndz,-1
    zpad(k)=zpad(k+1)-delz
  enddo
  do k=nz+1,nz+ndz
    zpad(k)=zpad(k-1)+delz
  enddo  

  allocate(xlesg(nlesxg),ylesg(nlesyg),zlesg(nleszg));

  do i=1,nlesxg
    xlesg(i)=xg(1) + real(i-1)*deltax
  enddo
  do j=1,nlesyg
    ylesg(j)=yg(1) + real(j-1)*deltay
  enddo
  do k=1,nleszg
    zlesg(k)=zg(1) + real(k-1)*deltaz
  enddo

! find the minimum and maximum points on the dns grid
  iminloop: do i=1,nlesxg
    if(xlesg(i).ge.x(1))then
      igst=i
      exit iminloop
    endif
  enddo iminloop

  jminloop: do j=1,nlesyg
    if(ylesg(j).ge.y(1))then
      jgst=j
      exit jminloop
    endif
  enddo jminloop

  kminloop: do k=1,nleszg
    if(zlesg(k).ge.z(1))then
      kgst=k
      exit kminloop
    endif
  enddo kminloop

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
    write(io,*) 'I got to here'
    call flush(io)
  endif

  if((xid+1).lt.npx)then
    imaxloop: do i=nlesxg,1,-1
      if(xlesg(i).lt.xg(nx*(xid+1)+1))then
        igend=i
        exit imaxloop
      endif
    enddo imaxloop
  else
    igend=nlesxg
  endif

  if((yid+1).lt.npy)then
    jmaxloop: do j=nlesyg,1,-1
      if(ylesg(j).lt.yg(ny*(yid+1)+1))then
        jgend=j
        exit jmaxloop
      endif
    enddo jmaxloop
  else
    jgend=nlesyg
  endif
  
  if((zid+1).lt.npz)then
    kmaxloop: do k=nleszg,1,-1
      if(zlesg(k).lt.zg(nz*(zid+1)+1))then
        kgend=k
        exit kmaxloop
      endif
    enddo kmaxloop
  else
    kgend=nleszg
  endif

  allocate(idnsst(nlesxg),idnsend(nlesxg))
  allocate(jdnsst(nlesyg),jdnsend(nlesyg))
  allocate(kdnsst(nleszg),kdnsend(nleszg))

! find the max and min points of each filter
  do il=igst,igend
    xming=xlesg(il)-deltax/2.0
    do i=1-ndx,nx+ndx
      if((xpad(i)+delx/2.0).ge.xming)then
        idnsst(il)=i
        exit
      endif
    enddo
    xmaxg=xlesg(il)+deltax/2.0
    do i=nx+ndx,1-ndx,-1
      if((xpad(i)-delx/2).le.xmaxg)then
        idnsend(il)=i
        exit
      endif
    enddo
  enddo

  do jl=jgst,jgend
 ! do jl=jgst,5
    yming=ylesg(jl)-deltay/2.0
    do j=1-ndy,ny+ndy
      if(((ypad(j)+ypad(j+1))/2.0).ge.yming)then
        jdnsst(jl)=j
        exit
      endif
    enddo
    ymaxg=ylesg(jl)+deltay/2.0
    do j=ny+ndy,1-ndy,-1
      if(((ypad(j)+ypad(j-1))/2.0).le.ymaxg)then
        jdnsend(jl)=j
        exit
      endif
    enddo
  enddo
  
  do kl=kgst,kgend
    zming=zlesg(kl)-deltaz/2.0
    do k=1-ndz,nz+ndz
      if((zpad(k)+delz/2.0).ge.zming)then
        kdnsst(kl)=k
        exit
      endif
    enddo
    zmaxg=zlesg(kl)+deltaz/2.0
    do k=nz+ndz,1-ndz,-1
      if((zpad(k)-delz/2.0).le.zmaxg)then
        kdnsend(kl)=k
        exit
      endif
    enddo
  enddo  

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
    write(io,*) 'I got to here'
    write(io,*) igst,igend
    call flush(io)
    write(io,*) idnsst(igst),idnsend(igend)
    call flush(io)
    write(io,*) jgst,jgend
    call flush(io)
    write(io,*) jdnsst(jgst),jdnsend(jgend)
    call flush(io)
    write(io,*) kgst,kgend
    call flush(io)
    write(io,*) kdnsst(kgst),kdnsend(kgend)
    call flush(io)
  endif


! set les filter kernel
  allocate(gfiltx(igst:igend,idnsst(igst):idnsend(igend))); gfiltx=0.0
  allocate(gfilty(jgst:jgend,jdnsst(jgst):jdnsend(jgend))); gfilty=0.0
  allocate(gfiltz(kgst:kgend,kdnsst(kgst):kdnsend(kgend))); gfiltz=0.0

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
    write(io,*) 'I got to here x'
    call flush(io)
  endif

  do il=igst,igend
    if((idnsend(il)-1-(idnsst(il)+1)).ge.0)then
      gfiltx(il,idnsst(il)+1:idnsend(il)-1)=delx
    endif
    if((idnsend(il)-idnsst(il)).gt.0)then
      gfiltx(il,idnsst(il))= (xpad(idnsst(il)+1)-delx/2.0) - (xlesg(il)-deltax/2.0)
      gfiltx(il,idnsend(il))=(xlesg(il)+deltax/2.0)-(xpad(idnsend(il)-1)+delx/2.0)
    else
      gfiltz(jl,jdnsend(il))=deltax
    endif
  enddo
  gfiltx=max(gfiltx/deltax,0.0)

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
    write(io,*) 'I got to here y'
    call flush(io)
  endif


  do jl=jgst,jgend
    if((jdnsend(jl)-1-(jdnsst(jl)+1)).ge.0) &
    gfilty(jl,jdnsst(jl)+1:jdnsend(jl)-1)=(ypad((jdnsst(jl)+2):(jdnsend(jl)))-ypad(jdnsst(jl):(jdnsend(jl)-2)))/2.0
    if((jdnsend(jl)-jdnsst(jl)).gt.0)then
      gfilty(jl,jdnsst(jl))= ((ypad(jdnsst(jl)+1)+ypad(jdnsst(jl)))/2.0) - (ylesg(jl)-deltay/2.0)
      gfilty(jl,jdnsend(jl))=(ylesg(jl)+deltay/2.0)-((ypad(jdnsend(jl))+ypad(jdnsend(jl)-1))/2.0)
    else
      gfilty(jl,jdnsend(jl))=deltay
    endif
  enddo
  gfilty=max(gfilty/deltay,0.0)


! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
    write(io,*) 'I got to here'
    call flush(io)
  endif
  
  do kl=kgst,kgend
    if((kdnsend(kl)-1-(kdnsst(kl)+1)).ge.0) gfiltz(kl,kdnsst(kl)+1:kdnsend(kl)-1)=delz
    if((kdnsend(kl)-kdnsst(kl)).gt.0)then
      gfiltz(kl,kdnsst(kl))= (zpad(kdnsst(kl)+1)-delz/2.0) - (zlesg(kl)-deltaz/2.0)
      gfiltz(kl,kdnsend(kl))=(zlesg(kl)+deltaz/2.0)-(zpad(kdnsend(kl)-1)+delz/2.0)
    else
      gfiltz(kl,kdnsend(kl))=deltaz
    endif
  enddo  
  gfiltz=max(gfiltz/deltaz,0.0)

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
    write(io,*) 'I got to here'
    call flush(io)
  endif


!  write(*,*) 'x', igst, igend,idnsst(igst),idnsend(igend)
!  open(666,file='gfiltx.dat',form='unformatted',access='direct',recl=4)
!  do il=igst,igend
!  do i=idnsst(igst),idnsend(igend)
!    recpoint=(idnsend(igend)-idnsst(igst)+1)*(il-igst)+(i-idnsst(igst)+1)
!    write(666,rec=recpoint) real(gfiltx(il,i),kind=4)
!  enddo
!  enddo
!  close(666)

  allocate(udns(1-ndx:nx+ndx,1-ndy:ny+ndy,1-ndz:nz+ndz)); udns=0.0
  allocate(uk(1-ndx:nx+ndx,1-ndy:ny+ndy,kgst:kgend)); uk=0.0
  allocate(ukj(1-ndx:nx+ndx,jgst:jgend,kgst:kgend)); ukj=0.0
  allocate(ules(igst:igend,jgst:jgend,kgst:kgend,3)); ules=0.0

! loop over directions
  do L=1,3

! fill ghost
  udns(1:nx,1:ny,1:nz)=u(:,:,:,L)
  call ghostzone_real(udns,nx,ny,nz,ndx,ndx,ndy,ndy,ndz,ndz)

! filter
  do i=1-ndx,nx+ndx
    do j=1-ndy,ny+ndy
      do kg=kgst,kgend    
       uk(i,j,kg)=sum(gfiltz(kg,kdnsst(kg):kdnsend(kg))*udns(i,j,kdnsst(kg):kdnsend(kg)))
      enddo
    enddo
  enddo

  do kg=kgst,kgend    
    do i=1-ndx,nx+ndx
      do jg=jgst,jgend
        ukj(i,jg,kg)=sum(gfilty(jg,jdnsst(jg):jdnsend(jg))*uk(i,jdnsst(jg):jdnsend(jg),kg))
      enddo
    enddo
  enddo

  do kg=kgst,kgend    
    do jg=jgst,jgend
      do ig=igst,igend
        ules(ig,jg,kg,L)=sum(gfiltx(ig,idnsst(ig):idnsend(ig))*ukj(idnsst(ig):idnsend(ig),jg,kg))
      enddo
    enddo
  enddo

  enddo ! direction

! debug
  call MPI_Barrier(gcomm,ierr)
  if(myid==0)then
    write(io,*) 'I got to here'
    call flush(io)
  endif


  nlesx=igend-igst+1
  nlesy=jgend-jgst+1
  nlesz=kgend-kgst+1
   
  call MPI_AllGather(nlesx,1,MPI_INTEGER,countsx,1,MPI_INTEGER,xcomm,ierr)
  call MPI_AllGather(nlesy,1,MPI_INTEGER,countsy,1,MPI_INTEGER,ycomm,ierr)
  call MPI_AllGather(nlesz,1,MPI_INTEGER,countsz,1,MPI_INTEGER,zcomm,ierr)
 
  call MPI_AllGather(igst,1,MPI_INTEGER,igstp,1,MPI_INTEGER,xcomm,ierr)
  call MPI_AllGather(igend,1,MPI_INTEGER,igendp,1,MPI_INTEGER,xcomm,ierr)

  call MPI_AllGather(jgst,1,MPI_INTEGER,jgstp,1,MPI_INTEGER,ycomm,ierr)
  call MPI_AllGather(jgend,1,MPI_INTEGER,jgendp,1,MPI_INTEGER,ycomm,ierr)

  call MPI_AllGather(kgst,1,MPI_INTEGER,kgstp,1,MPI_INTEGER,zcomm,ierr)
  call MPI_AllGather(kgend,1,MPI_INTEGER,kgendp,1,MPI_INTEGER,zcomm,ierr)

  if(yz_id==0) allocate(uplane(nlesyg,nleszg,3))

! loop over ig, writing each yz plane
  do ig=1,nlesxg
!   only the processors involved 
    if((ig.ge.igst).and.(ig.le.igend))then
      do jid=0,npy-1
      do kid=0,npz-1
        jkid=(kid)*npy+jid
        if(yz_id==0)then
          allocate(urecv(countsy(jid+1),countsz(kid+1),3)); urecv(:,:,:)=0.0
          call MPI_IRecv(urecv,countsy(jid+1)*countsz(kid+1)*3,&
	                 MPI_REAL8,jkid,jkid,yz_comm,req(2),ierr)
        endif
        if(jkid==yz_id)then
          call MPI_ISend(ules(ig,jgst:jgend,kgst:kgend,1:3),nlesy*nlesz*3,&
                         MPI_REAL8,0,jkid,yz_comm,req(1),ierr)
        endif
        if(jkid==yz_id)then
          call MPI_Wait(req(1),statvec(:,1),ierr)
        endif
        if(yz_id==0)then
          call MPI_Wait(req(2),statvec(:,2),ierr)
          uplane(jgstp(jid+1):jgendp(jid+1),kgstp(kid+1):kgendp(kid+1),:)=urecv(:,:,:)
          deallocate(urecv)
        endif
        call MPI_Barrier(yz_comm,ierr)
      enddo
      enddo
      if(yz_id==0)then
        open(666,file='sandiaDNS.inflow',form='unformatted',position='append')
        write(666) real(uplane(:,:,:),kind=4)
        close(666)

!       for checking
        if(ig==1)then
          open(667,file='check.dat',form='unformatted',access='direct',recl=4)
          do j=1,nlesyg
            do k=1,nleszg
              recpoint=(ig-1)*nleszg*nlesyg+(j-1)*nleszg + k
              write(667,rec=recpoint) real(uplane(j,k,1),kind=4)
            enddo
          enddo
          close(667)
        endif
      endif
      
    endif
    call MPI_Barrier(gcomm,ierr)
  enddo

  deallocate(udns,uk,ukj,ules)
  deallocate(gfiltx,gfilty,gfiltz)
  deallocate(idnsst,idnsend,jdnsst,jdnsend,kdnsst,kdnsend)
  deallocate(xlesg,ylesg,zlesg)
  if(yz_id==0) deallocate(uplane)

  return
end subroutine filter_feeddata
!-----------------------------------------------------------------------------------------

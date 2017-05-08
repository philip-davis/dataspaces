#include "globalDefines.h"
!========================================================================================
  subroutine check_resolution(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
!========================================================================================
! analyzes the energy spectrum to check spatial resolution.
! Reading this subroutine is challenging!!
!
! This routine calls the 1D fft one line at a time.  Multiple 1D fft's
! in a single call can improve performance on parallel machines.
! Some other optimization is also possible.  Non-Periodic
! directions are mirrored to simulate periodicity.
!
! If you want to try and deal with nonunifrom grids, try getting
! SIAM J. Sci. Comp, Vol. 14, No. 6, 1368-93 (1993).
! Rokhlin is at ``rokhlin@cs.yale.edu'' - he's got three BIG codes.
!
! If you want to try something better than this domain flipping -
! my guess is that a good discrete wavelet transform is the ticket.
!
!----------------------------------------------------------------------------------------
! Change record
!
! 24-NOV-2004: Evatt Hawkes - adding Cray X1 compatibility
! 
!
!----------------------------------------------------------------------------------------
! Console (see illustration below):
! emon =[0,1]:  Spectral energy cut-off.
! ximax=[0,pi]: Upper frequency for resolved energy.
! eimax=[0,pi]: Maximum spectral energy allowed to exceed eimax.
! itask=[0,1,2,3,4]: 0=silent, 1-3=verbose, 4=warn if e(ximax)>eimax
!
! 1 - integrated energy
!
!        ^
!        |
!      1 +*
!        | ***
!        |    *
!        |     **
!        |       **
!  emon  +---->----***
!        |          | ***
! seint  +----<-----|----****
!        |          |      | **
!        |          |      ^   **
!        |          v      |     **
!      0 +----------+------+-------***************+---> xi
!        0          c      x                      p
!                   u      i                      i
!                   t      m
!                   o      a
!                   f      x
!                   f
!----------------------------------------------------------------------------------------
  use param_m
  use topology_m
  use grid_m, only : unif_grid_x, unif_grid_y, unif_grid_z
  use reference_m, only : time_ref
  use runtime_m, only : i_time, time

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io
  integer, intent(in) :: qnx, qny, qnz, qnvar_tot, qn_reg
  real, dimension(qnx,qny,qnz,qnvar_tot,qn_reg) :: q !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)

! local declarations

  integer i, j, k
  integer ij, jk, ik, ti, tj, tk, tij, tjk, tik, l1, l2
  integer it, itail, ipe
  integer i_g, j_g, k_g
  integer myi, myj, myk, myij, myjk, myik

  integer itask
  integer itec
  integer iwrite
  integer nvar
  integer nn, nn2

  real cutoff
  real e(    nxyz_g)
  real eint( nxyz_g)
  real eimax
  real emon
  real invn2
  real pi
  real seq( nxyz2_g+2), sseq(nxyz_g)
  real seqr(nxyz2_g)
  real wnumb(nxyz_g)
  real wnfact
  real ximax
  real seint

  integer         jcxmax, jsxmax, kcxmax, ksxmax
  integer icymax, isymax,         kcymax, ksymax
  integer iczmax, iszmax, jczmax, jszmax
  integer ncxmax, nsxmax
  integer ncymax, nsymax
  integer nczmax, nszmax

  real cxmax, cymax, czmax
  real sxmax, symax, szmax

  real xmax_g(4), ymax_g(4), zmax_g(4)
  real xmax_l(4), ymax_l(4), zmax_l(4)
  real  jxmax(6),  jymax(6),  jzmax(6)

  logical iflag

  character*7 title(5)

  real, allocatable, dimension(:) :: table, work
!----------------------------------------------------------------------------------------
! return if zero-dimensions

  if((vary_in_x.eq.0).and.(vary_in_y.eq.0).and.(vary_in_z.eq.0)) return
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
    write(io,100) 'checking spatial resolution at: i_time = ',i_time,  &
                   'time = ',time*time_ref,' (sec)'
    write(io,*)
  endif
  100 format(1x,a,i7,a,1pe9.3,a)
!----------------------------------------------------------------------------------------
! sync processors

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! initialize stuff

  title(1) = 'u*rho'
  title(2) = 'v*rho'
  title(3) = 'w*rho'
  title(4) = 'rho'
  title(5) = 'energy*rho'

  emon  = 0.10e-2
  ximax = 2.0
  eimax = emon
  itask = 0
  itec  = 0

  pi    = 4.0 * atan(1.0)
  cxmax = 0.0
  sxmax = 0.0
  cymax = 0.0
  symax = 0.0
  czmax = 0.0
  szmax = 0.0
!----------------------------------------------------------------------------------------
! start of loop over variables

  do nvar = 1, nvar_tot
!----------------------------------------------------------------------------------------
!   set some stuff

    if( nvar .le. 5 ) then
      cxmax = 0.0
      sxmax = 0.0
      cymax = 0.0
      symax = 0.0
      czmax = 0.0
      szmax = 0.0
    endif
!----------------------------------------------------------------------------------------
!   x-direction
!----------------------------------------------------------------------------------------
!   initialize 1-D FFT Real to Complex workspace:

    if( vary_in_x .eq. 1 .and. unif_grid_x .eq. 1 ) then

      if( periodic_x .ne. 1) then
        nn  = 2 * ( nx_g - 1 )
      elseif(  periodic_x .eq. 1 ) then
        nn  = nx_g
      endif

      nn2    = nn / 2
      invn2  = 1.0 / nn2
      wnfact = pi * invn2

      do k = 1, nn2
        wnumb(k) = k * wnfact
      enddo

!     write tecplot file (not sure how this works, inactive for now)
!     this one opens the file (I think)

!      if( myid.eq.0 ) then
!        if( itec .eq. 1 .and. nvar .eq. 1) then
!          iwrite = 1
!          write(io,*) 'write_spectrum not ready yet'
!          call terminate_run(io,0)
!!          call write_spectrum ( eint, iwrite, nn2, wnumb, i_time, time, time_ref)
!        endif
!      endif

!     checks for out of bound wavenumber:

      if( myid.eq.0 ) then
        if( ximax .lt. 0.0 .or. ximax .gt. pi ) then
          write ( io, 9500 ) ximax
        endif
      endif

!     machine-dependent initialization of FFT
!     allocate fft workspace now

#if defined(ARCH_T3E)
        allocate (table(2*nn))
        allocate (work(2*nn))
#endif

#if defined(ARCH_X1)
        allocate (table(100+4*nn))
        allocate (work(4+4*nn))
#endif

#if defined(ARCH_SGI)
        allocate (table(nn+15))
#endif

#if defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
        allocate (table(nn))
        allocate (work (nn))
#endif

!     initialize FFTs

      call fft_rinit(nn,table)

      do jk = 1, (ny*nz), xpes

        iflag = .false.

        k = ((jk-1)/ny)     + 1
        j = mod((jk-1), ny) + 1

        myjk = jk + xid
        k_g  = (myjk-1)/ny  + 1 + (nz*zid)
        j_g  = mod((myjk-1),ny) + 1 + (ny*yid)

!       read x-pencil

        if( (jk-1).le.ny*nz-xpes ) then
          do ipe=0,xpes-1
            myk =    ((jk+ipe-1)/ny)  + 1
            myj = mod((jk+ipe-1), ny) + 1
            sseq((ipe*nx)+1:(ipe+1)*nx)=q(:,myj,myk,nvar,1)
          enddo
          call MPI_Alltoall(sseq,nx,MPI_REAL8,seq,nx,MPI_REAL8,xcomm,ierr)
          iflag = .true.
        else
          itail = mod(ny*nz, xpes)
          do it = 1, itail
            tjk = jk + it -1
            tk  = ((tjk-1) / ny) + 1
            tj  = mod((tjk-1), ny) + 1
            call MPI_Gather(q(1,tj,tk,nvar,1),nx,MPI_REAL8,seq,nx,  &
                            MPI_REAL8,it-1,xcomm,ierr)
            if(xid.eq.it-1)then
              iflag = .true.
            endif
          enddo
        endif

!       read 2nd-half of x-pencil so that it's symmetric in nonperiodic domains

        if(iflag) then

          if( periodic_x .ne. 1 ) then
            do i = 1, nx_g-1
              seq(nx_g+i-1) = seq(nx_g-i)
            enddo
          endif

          call ftckxy( nn, nn2, emon, cutoff, ximax, eimax,  seint, seq,  &
                       seqr, e, eint, wnumb, pi, invn2, nvar, io, itask, table, work )

!         keep track of max. wavenumber and which pencil was it.

          if( cutoff .gt. cxmax ) then
            cxmax  = cutoff
            jcxmax = j_g
            kcxmax = k_g
            ncxmax = nvar
          endif

!         keep track of max. energy integral and which pencil was it.

          if( seint .gt. sxmax ) then
            sxmax  = seint
            jsxmax = j_g
            ksxmax = k_g
            nsxmax = nvar
          endif

        endif

      enddo

      xmax_l(1) = cxmax
      xmax_l(2) = myid
      xmax_l(3) = sxmax
      xmax_l(4) = myid

      jxmax(1)  = jcxmax
      jxmax(2)  = kcxmax
      jxmax(3)  = ncxmax
      jxmax(4)  = jsxmax
      jxmax(5)  = ksxmax
      jxmax(6)  = nsxmax

#if defined(ARCH_SGI) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
        call MPI_Allreduce(xmax_l,xmax_g,2,MPI_2DOUBLE_PRECISION,MPI_MAXLOC,gcomm,ierr)
#else
#if defined(ARCH_T3E)
          call MPI_Allreduce(xmax_l,xmax_g,2,MPI_2REAL,MPI_MAXLOC,gcomm,ierr)
#endif
#endif

      call MPI_Bcast(jxmax(1),3,MPI_INTEGER,nint(xmax_g(2)),gcomm,ierr)
      call MPI_Bcast(jxmax(4),3,MPI_INTEGER,nint(xmax_g(4)),gcomm,ierr)

!     deallocate FFT work arrays previously allocated

      deallocate (table)

#if defined(ARCH_T3E) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
        deallocate (work)
#endif

!     tecplot output

!      if(myid.eq.0) then
!        if( itec .eq. 1 ) then
!          iwrite = 2
!          write(io,*) 'write_spectrum not ready yet'
!          call terminate_run(io,0)
!!          call write_spectrum ( eint, iwrite, nn2, wnumb, i_time, time, time_ref)
!        endif
!      endif
!----------------------------------------------------------------------------------------
!   end of x-direction
 
    endif
!----------------------------------------------------------------------------------------
!   x-direction
!----------------------------------------------------------------------------------------
!   initialize 1-D FFT Real to Complex workspace

    if( vary_in_y .eq. 1 .and. unif_grid_x .eq. 1 ) then

      if( periodic_y .ne. 1 ) then
        nn  = 2 * ( ny_g - 1 )
      elseif(  periodic_y .eq. 1 ) then
        nn  = ny_g
      endif

      nn2 = nn / 2
      invn2 = 1.0 / nn2
      wnfact = pi * invn2

      do k = 1, nn2
        wnumb(k) = k * wnfact
      enddo

!     checks for out of bound wavenumber (originally commented out by PSC)

!      if(myid.eq.0) then
!        if( ximax .lt. 0.0 .or. ximax .gt. pi ) then
!          write ( io, 9500 ) ximax
!        endif
!      endif

!     machine-dependent initialization of FFT:
!     allocate fft workspace now

!     machine-dependent initialization of FFT
!     allocate fft workspace now

#if T3E
        allocate (table(2*nn))
        allocate (work(2*nn))
#endif

#if X1
        allocate (table(100+4*nn))
        allocate (work(4+4*nn))
#endif

#if defined(ARCH_SGI)
        allocate (table(nn+15))
#endif

#if defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
        allocate (table(nn))
        allocate (work (nn))
#endif

!     initialize FFTs

      call fft_rinit(nn,table)

      do ik = 1,nx*nz, ypes

        iflag = .false.

        k = ((ik-1)/nx)     + 1
        i = mod((ik-1), nx) + 1

        myik = ik + yid
        k_g  = (myik-1)/nx  + 1 + (nz*zid)
        i_g  = mod((myik-1),nx) + 1 + (nx*xid)

!       read y-pencil

        if( (ik-1).le.nx*nz-ypes ) then
          do ipe=0,ypes-1
            myk =    ((ik+ipe-1)/nx)  + 1
            myi = mod((ik+ipe-1), nx) + 1
            sseq((ipe*ny)+1:(ipe+1)*ny)=q(myi,:,myk,nvar,1)
          enddo
          call MPI_Alltoall(sseq,ny,MPI_REAL8,seq,ny,MPI_REAL8,ycomm,ierr)
          iflag = .true.
        else
          itail = mod(nx*nz, ypes)
          do it = 1, itail
            tik = ik + it -1
            tk  = ((tik-1) / nx) + 1
            ti  = mod((tik-1), nx) + 1
            call MPI_Gather(q(ti,1,tk,nvar,1),1,yrfft_type,seq,ny,  &
                            MPI_REAL8,it-1,ycomm,ierr)
            if(yid.eq.it-1)then
              iflag = .true.
            endif
          enddo
        endif

!       read 2nd-half of y-pencil so that it's symmetric in nonperiodic domains

        if(iflag) then

          if( periodic_y .ne. 1 ) then
            do j = 1, ny_g-1
              seq(ny_g+j-1) = seq(ny_g-j)
            enddo
          endif

          call ftckxy ( nn, nn2, emon, cutoff, ximax, eimax, seint, seq,  &
                        seqr, e, eint, wnumb, pi, invn2, nvar, io, itask, table, work )

!         keep track of max. wavenumber and which pencil was it.

          if( cutoff .gt. cymax ) then
            cymax  = cutoff
            icymax = i_g
            kcymax = k_g
            ncymax = nvar
          endif

!         keep track of max. energy integral and which pencil was it.

          if( seint .gt. symax ) then
            symax  = seint
            isymax = i_g
            ksymax = k_g
            nsymax = nvar
          endif

        endif

      enddo

      ymax_l(1) = cymax
      ymax_l(2) = myid
      ymax_l(3) = symax
      ymax_l(4) = myid

      jymax(1)  = icymax
      jymax(2)  = kcymax
      jymax(3)  = ncymax
      jymax(4)  = isymax
      jymax(5)  = ksymax
      jymax(6)  = nsymax

#if defined(ARCH_SGI) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
        call MPI_Allreduce(ymax_l,ymax_g,2,MPI_2DOUBLE_PRECISION,MPI_MAXLOC,gcomm,ierr)
#else
#if defined(ARCH_T3E)
          call MPI_Allreduce(ymax_l,ymax_g,2,MPI_2REAL,MPI_MAXLOC,gcomm,ierr)
#endif
#endif

      call MPI_Bcast(jymax(1),3,MPI_INTEGER,nint(ymax_g(2)),gcomm,ierr)
      call MPI_Bcast(jymax(4),3,MPI_INTEGER,nint(ymax_g(4)),gcomm,ierr)

!     deallocate FFT work arrays previously allocated

      deallocate (table)

#if defined(ARCH_T3E) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
        deallocate (work)
#endif

!     tecplot output

!      if(myid.eq.0) then
!        if( itec .eq. 1 ) then
!          iwrite = 2
!          write(io,*) 'write_spectrum not ready yet'
!          call terminate_run(io,0)
!!          call write_spectrum ( eint, iwrite, nn2, wnumb, i_time, time, time_ref)
!        endif
!      endif
!----------------------------------------------------------------------------------------
!   end of y-direction
 
    endif
!----------------------------------------------------------------------------------------
!   z-direction
!----------------------------------------------------------------------------------------
!   initialize 1-D FFT Real to Complex workspace

    if( vary_in_z .eq. 1 .and. unif_grid_x .eq. 1 ) then

      if( periodic_z .ne. 1 ) then
        nn  = 2 * ( nz_g - 1 )
      elseif(  periodic_z .eq. 1 ) then
        nn  = nz_g
      endif

      nn2 = nn / 2
      invn2 = 1.0 / nn2
      wnfact = pi * invn2

      do k = 1, nn2
        wnumb(k) = k * wnfact
      enddo

!     checks for out of bound wavenumber (originally commented out by PSC)

!      if(myid.eq.0) then
!        if( ximax .lt. 0.0 .or. ximax .gt. pi ) then
!          write ( io, 9500 ) ximax
!        endif
!      endif

!     machine-dependent initialization of FFT:
!     allocate fft workspace now

#if defined(ARCH_T3E)
        allocate (table(2*nn))
        allocate (work(2*nn))
#endif

#if defined(ARCH_X1)
        allocate (table(100+4*nn))
        allocate (work(4+4*nn))
#endif

#if defined(ARCH_SGI)
        allocate (table(nn+15))
#endif

#if defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
        allocate (table(nn))
        allocate (work (nn))
#endif

!     initialize FFTs

      call fft_rinit(nn,table)

      do ij = 1, nx*ny, zpes

        iflag = .false.

        j = ((ij-1)/nx)     + 1
        i = mod((ij-1), nx) + 1

        myij = ij + zid
        j_g  = (myij-1)/nx  + 1 + (ny*yid)
        i_g  = mod((myij-1),nx) + 1 + (nx*xid)

!       Read z-pencil

        if( (ij-1).le.nx*ny-zpes ) then
          do ipe=0,zpes-1
            myj =    ((ij+ipe-1)/nx) + 1
            myi = mod((ij+ipe-1),nx) + 1
            sseq((ipe*nz)+1:(ipe+1)*nz)=q(myi,myj,:,nvar,1)
          enddo
          call MPI_Alltoall(sseq,nz,MPI_REAL8,seq,nz,MPI_REAL8,zcomm,ierr)
          iflag = .true.
        else
          itail = mod(nx*ny, zpes)
          do it = 1, itail
            tij = ij + it -1
            tj  = ((tij-1) / nx) + 1
            ti  = mod((tij-1), nx) + 1
            call MPI_Gather(q(ti,tj,1,nvar,1),1,zrfft_type,seq,nz,  &
                            MPI_REAL8,it-1,zcomm,ierr)
            if(zid.eq.it-1)then
              iflag = .true.
            endif
          enddo
        endif

!       read 2nd-half of z-pencil so that it's symmetric in nonperiodic domains

        if(iflag) then

          if( periodic_z .ne. 1 ) then
            do k = 1, nz_g-1
              seq(nz_g+k-1) = seq(nz_g-k)
            enddo
          endif

          call ftckxy ( nn, nn2, emon, cutoff, ximax, eimax, seint, seq,  &
                        seqr, e, eint, wnumb, pi, invn2, nvar, io, itask, table, work )

!         keep track of max. wavenumber and which pencil was it.

          if( cutoff .gt. czmax ) then
            czmax  = cutoff
            iczmax = i_g
            jczmax = j_g
            nczmax = nvar
          endif

!         keep track of max. energy integral and which pencil was it.

          if( seint .gt. szmax ) then
            szmax  = seint
            iszmax = i_g
            jszmax = j_g
            nszmax = nvar
          endif

        endif

      enddo

      zmax_l(1) = czmax
      zmax_l(2) = myid
      zmax_l(3) = szmax
      zmax_l(4) = myid

      jzmax(1)  = iczmax
      jzmax(2)  = jczmax
      jzmax(3)  = nczmax
      jzmax(4)  = iszmax
      jzmax(5)  = jszmax
      jzmax(6)  = nszmax

#if defined(ARCH_SGI) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
        call MPI_Allreduce(zmax_l,zmax_g,2,MPI_2DOUBLE_PRECISION,MPI_MAXLOC,gcomm,ierr)
#else
#if defined(ARCH_T3E)
          call MPI_Allreduce(zmax_l,zmax_g,2,MPI_2REAL,MPI_MAXLOC,gcomm,ierr)
#endif
#endif

      call MPI_Bcast(jzmax(1),3,MPI_INTEGER,nint(zmax_g(2)),gcomm,ierr)
      call MPI_Bcast(jzmax(4),3,MPI_INTEGER,nint(zmax_g(4)),gcomm,ierr)

!     deallocate FFT work arrays previously allocated

      deallocate (table)

#if defined(ARCH_T3E) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
        deallocate (work)
#endif

!     tecplot output:

!      if(myid.eq.0) then
!        if( itec .eq. 1 ) then
!          iwrite = 2
!          write(io,*) 'write_spectrum not ready yet'
!          call terminate_run(io,0)
!!          call write_spectrum ( eint, iwrite, nn2, wnumb, i_time, time, time_ref)
!        endif
!      endif
!----------------------------------------------------------------------------------------
!   end of z-direction
 
    endif
!----------------------------------------------------------------------------------------
!   write data to file io

    if( myid.eq.0 ) then

      if( nvar .eq. 1 ) then
        write ( io, 5400 ) 100.0 * emon, ximax
      endif

!     x-direction

      if( vary_in_x .eq. 1 .and. unif_grid_x .eq. 1 ) then

        if( nvar .le. 5 ) then
          write ( io, 5410 ) 'x', title(nvar), xmax_g(1),                       &
                              nint(jxmax(3)), nint(jxmax(1)), nint(jxmax(2)),   &
                              100.0 * xmax_g(3),                                &
                              nint(jxmax(6)),nint(jxmax(4)), nint(jxmax(5))
        endif

        if( nvar .eq. nvar_tot ) then
          write ( io, 5410 ) 'x', 'species', xmax_g(1),                         &
                              nint(jxmax(3)), nint(jxmax(1)), nint(jxmax(2)),   &
                              100.0 * xmax_g(3),                                &
                              nint(jxmax(6)), nint(jxmax(4)), nint(jxmax(5))
        endif

      endif

!     y-direction

      if( vary_in_y .eq. 1 .and. unif_grid_y .eq. 1 ) then

        if( nvar .le. 5 ) then
          write ( io, 5410 ) 'y', title(nvar), ymax_g(1),                       &
                              nint(jymax(3)), nint(jymax(1)), nint(jymax(2)),   &
                              100.0 * ymax_g(3),                                &
                              nint(jymax(6)), nint(jymax(4)), nint(jymax(5))
        endif

        if( nvar .eq. nvar_tot ) then
          write ( io, 5410 ) 'y', 'species', ymax_g(1),                         &
                              nint(jymax(3)), nint(jymax(1)), nint(jymax(2)),   &
                              100.0 * ymax_g(3),                                &
                              nint(jymax(6)), nint(jymax(4)), nint(jymax(5))
        endif

      endif

!     y-direction

      if( vary_in_z .eq. 1 .and. unif_grid_z .eq. 1 ) then

        if( nvar .le. 5 ) then
          write ( io, 5410 ) 'z', title(nvar), zmax_g(1),                       &
                              nint(jzmax(3)), nint(jzmax(1)), nint(jzmax(2)),   &
                              100.0 * zmax_g(3),                                &
                              nint(jzmax(6)), nint(jzmax(4)), nint(jzmax(5))
        endif

        if( nvar .eq. nvar_tot ) then
          write ( io, 5410 ) 'z', 'species', zmax_g(1),                         &
                              nint(jzmax(3)), nint(jzmax(1)), nint(jzmax(2)),   &
                              100.0 * zmax_g(3),                                &
                              nint(jzmax(6)), nint(jzmax(4)), nint(jzmax(5))
        endif

      endif

    endif
!----------------------------------------------------------------------------------------
! end of loop over variables

  enddo
!----------------------------------------------------------------------------------------
! tecplot (close the file)

!  if( itec .eq. 1 ) then
!    iwrite = 3
!    write(io,*) 'write_spectrum not ready yet'
!    call terminate_run(io,0)
!!    call write_spectrum ( eint, iwrite, nn2, wnumb, i_time, time, time_ref)
!  endif
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------------------------
  return
!----------------------------------------------------------------------------------------
! format statements

  5400 format(' dir. title',2x,0pf7.3,'%-cutoff   eq. pos.',          &
              '    e(xi=',0pf4.2,')    eq. pos.')
  5410 format(2x,a,3x,a,4x,0pf8.5,3x,3i5,3x,1pe10.2,'% ',3i5)
  9500 format(' error in ftckx: ximax outside [0,pi] (',1pe10.3,')')
  9800 format(' error: x-direction ifax(1)=',i4)
  9810 format(' error: y-direction ifax(1)=',i4)
  9820 format(' error: z-direction ifax(1)=',i4)
!----------------------------------------------------------------------------------------
  end subroutine check_resolution
!=========================================================================================
  subroutine ftckxy( n, n2, emon, cutoff, ximax, eimax, seint, seq,  &
                     seqr, e, eint, wnumb, pi, invn2, nvar, io, itask, table, work )
!=========================================================================================
! analyzes a 1-dimensional energy spectrum.
!-----------------------------------------------------------------------------------------
  use topology_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer n
  integer n2
  integer nvar
  integer io
  integer itask

  real emon
  real ximax
  real eimax
  real seint
  real seq(n+2)
  real seqr(n)
  real e(n2)
  real eint(n2)
  real wnumb(n2)
  real invn2
  real table(*)
  real work(*)

! local delcarations

  real cutoff
  real pi

  integer k
  integer kximax
  integer kemon

  real eps
  parameter ( eps = 1.0e-30 )
!-----------------------------------------------------------------------------------------
! machine-dependent Real to Complex transform:

  do k = 1, n
    seqr(k) = seq(k)
  enddo

  call fft_real( -1, n, table, work, seqr, seq )

  do k = 1, n2
    e(k) = invn2 * sqrt ( seq(2*k-1)**2 + seq(2*k)**2 )
  enddo
!-----------------------------------------------------------------------------------------
! analyze energy spectrum

! integrate:

  eint(1) = e(1)
  do k = 2, n2
    eint(k) = eint(k-1) + e(k)
  enddo

! normalize and flip:

  eint(n2) = 1.0 / ( eint(n2) + eps )
  kemon = 1

  do k = 1, n2-1
    eint(k) = 1.0 - eint(k) * eint(n2)
    if( eint(k) .ge. emon ) then
      kemon = k
    endif
  enddo

  eint(n2) = 0.0
  cutoff = wnumb(kemon)

! energy above ximax

  kximax = 1 + nint ( ximax / pi * ( n2 - 1 ) )
  seint = eint(kximax)

  if(myid.eq.0) then

    if( itask .eq. 1 .or. itask .eq. 3 ) then
      write ( io, 7800 ) ximax, nvar, 100.*eint(kximax)
    endif

    if( itask .eq. 2 .or. itask .eq. 3 ) then
      write ( io, 7810 ) 100.*emon, cutoff
    endif

    if( itask .eq. 4 .and. seint .gt. eimax ) then
      write ( io, 8800 )
      write ( io, 7800 ) ximax, nvar, 100.*seint
    endif

  endif
!-----------------------------------------------------------------------------------------
! format statements

  7800 format(' spectral energy above xi=',f4.2,' for variable',i3,':',1pe12.4,'%')
  7810 format(' the',1pe10.2,'% spectral cut-off is xi=',0pf6.3)
  8800 format(' warning from ftckxy: excessive high-frequency energy')
!-----------------------------------------------------------------------------------------
  return
  end subroutine ftckxy
!-----------------------------------------------------------------------------------------
! machine independent FFT interface routines
! currently implemented on:
!   SGI with -r8 compiler option (SGI)
!   Cray-T3E (T3E)
!   Cray-X1 (X1)
!
! user must allocate space for table and work in the calling routines
! as shown in the following examples
!-----------------------------------------------------------------------------------------
! real-to-complex and complex-to-real 1-D FFTS
!
!      real, allocatable, dimension(:) :: table, work
!  #if defined(ARCH_T3E)
!      allocate (table(2*n))
!      allocate (work(2*n))
!  #else
!  #if defined(ARCH_SGI)
!      allocate (table(n+15))
!  #else
!  #if defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
!      allocate (table(n))
!      allocate (work(n))
!  #else
!  #if defined(ARCH_X1)
!      allocate (table(100+4*n))
!      allocate (work(4+4*n))
!  #endif
!  #endif
!  #endif
!  #endif
!-----------------------------------------------------------------------------------------
! Complex-to-complex 1-D FFTs:
!
!      real, allocatable, dimension(:) :: table, work
!  #if defined(ARCH_T3E)
!      allocate (table(2*n))
!      allocate (work(4*n))
!  #else
!  #if defined(ARCH_SGI)
!      allocate (table(2*(n+15)))
!  #else
!  #if defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
!      allocate (table(n))
!      allocate (work(n))
!  #else
!  #if X1
!      allocate (table(100+8*n))
!      allocate (work(8*n))
!  #endif
!  #endif
!  #endif
!  #endif
!=========================================================================================
  subroutine fft_rinit(n,table)
!=========================================================================================
  implicit none

  integer n
  integer dummyi
  real    table(*)
  real dummyr
!-----------------------------------------------------------------------------------------
#if defined(ARCH_T3E)
    call scfft ( 0, n, 1.0, dummyr, dummyr, table, dummyr, 0 )
#else
#if defined(ARCH_SGI)
      call dzfft1dui ( n, table )
#endif
#endif

#if defined(ARCH_X1)
    call zzfft ( 0, n, 1.0, dummyr, dummyr, table, dummyr, 0 )
#endif

#if defined(ARCH_PC)
#endif

  return
  end subroutine fft_rinit
!=========================================================================================
  subroutine fft_cinit( n, table)
!=========================================================================================
  implicit none

  integer n
  real table(*)
  real dummyr
!-----------------------------------------------------------------------------------------
#if defined(ARCH_T3E)
    call ccfft ( 0, n, 1.0, dummyr, dummyr, table, dummyr, 0 )
#else
#if defined(ARCH_SGI)
       call zfft1di ( n, table )
#endif
#endif

#if defined(ARCH_PC)
#endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine fft_cinit
!=========================================================================================
  subroutine fft_real( direction, n, table, work, in, out )
!=========================================================================================
  implicit none
!-----------------------------------------------------------------------------------------
  integer direction, n, m, i
  real    in, out, table(*), work(*), dummyr

  dimension in(*),out(*)
  real in1(n), in2(n)
!-----------------------------------------------------------------------------------------

#if defined(ARCH_X1)
    if( direction.eq.-1 ) then
      call dzfft ( direction, n, 1.0, in, out, table, work, 0 )
    else
      call zdfft ( direction, n, 1.0, in, out, table, work, 0 )
    endif
#endif


#if defined(ARCH_T3E)
    if( direction.eq.-1 ) then
      call scfft ( direction, n, 1.0, in, out, table, work, 0 )
    else
      call csfft ( direction, n, 1.0, in, out, table, work, 0 )
    endif
#else
#if defined(ARCH_SGI)
      if(direction.eq.-1) then
        m = n
        out(1:m) = in(1:m)
        call dzfft1du ( direction, n, out, 1, table )
      else
        m = n+2
        out(1:m) = in(1:m)
        call zdfft1du ( direction, n, out, 1, table )
      endif
#else
#if defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
        in1(1:n) = in(1:n)
        in2(:) = 0.

!       only for real-to-complex (forward ffts)

        call fourt(in1,in2,n,1,1,0,table,work)

        out(1:n+1:2) = in1(1:n/2+1)
        out(2:n+2:2) = in2(1:n/2+1)

#endif
#endif
#endif

#if PC
#endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine fft_real
!=========================================================================================
  subroutine fft_complex( direction, n, table, work, in, out )
!=========================================================================================
  implicit none

  integer direction, n, direc
  complex    in, out
  real   table(*), work(*), dummyr

  dimension in(*),out(*)
  real in1(n), in2(n)
!-----------------------------------------------------------------------------------------
#if defined(ARCH_X1)
    call zzfft ( direction, n, 1.0, in, out, table, work, 0 )
    in(1:n) = out(1:n)
#endif

#if defined(ARCH_T3E)
    call ccfft ( direction, n, 1.0, in, out, table, work, 0 )
    in(1:n) = out(1:n)
#else
#if defined(ARCH_SGI)
      call zfft1d ( direction, n, in, 1, table )
#else
#if defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC)
        in1(1:n) = real(in(1:n))
        in2(1:n) = aimag(in(1:n))

!       only for complex-to-complex (forward ffts with direction = 1)

        direc = 0
        call fourt(in1,in2,n,1,direc,1,table,work)

        in(1:n) = cmplx(in1(1:n),in2(1:n))
!        out(1:2*n:2) = in1(1:n)
!        out(2:2*n:2) = in2(1:n)
#endif
#endif
#endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine fft_complex

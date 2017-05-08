#include "globalDefines.h"
subroutine computeScalarDissipationRate(io, chiSDR)

  use param_m 
  use variables_m
  use reference_m
  use mixFrac_m,   only : mixFrac
  use transport_m
  use thermchem_m

  implicit none

  integer, intent(in)                              :: io
  real, intent(out), dimension(nx,ny,nz)           :: chiSDR

  integer                             :: i,j,k,L, ii,jj
  real, dimension(nx,ny,nz,3)         :: gradF
  real, dimension(nx,ny,nz)           :: DDiff

#ifdef LEWIS
  real, dimension(nx,ny,nz)            :: Cp
#endif

  !--------------------------------------------------------------------------------------

#ifdef MIXAVG
  call computeCoefficients(pressure, temp, yspecies, q(:,:,:,4,1))
#endif

#ifdef LEWIS
  ! RG: I'm not sure that cpmix and temp are set here; if not, need to call
  !     calc_temp from thermochem_m
  call computeCoefficients(cpmix, temp)
#endif


  
  gradF = 0.0
  call computeScalarGradient( mixFrac, gradF )

   DDiff = getThermalConductivity() / cpmix * volum

  gradF  = gradF**2.0
  chiSDR = 2.0*DDiff*(gradF(:,:,:,1) + gradF(:,:,:,2) + gradF(:,:,:,3)) 

end subroutine computeScalarDissipationRate

!========================================================================================

subroutine chi_cond_fstoic(io)

  use param_m
  use topology_m
  use variables_m
  use reference_m
  use mixFrac_m
  use grid_m
  use runtime_m,    only : run_title, time
  use chemkin_m

  implicit none

  integer, intent(in)                   :: io

  character*100                         :: filename
  character*9                           :: time_ext
  integer                               :: i,j,k,L, ixi, ii
  integer,parameter                     :: nfbins = 100
  integer                               :: nphi
  
  real, allocatable, dimension(:)       :: phiM,   phiSTD
  real, allocatable, dimension(:)       :: phiM_g, phiSTD_g
  real, dimension(nx,ny,nz)             :: chiSDR
  real                                  :: df
  real, dimension(nfbins)               :: sumFbin, sumFbin_g

  real :: osuml, osumg
  

!----------------------------------------------------------------------------------------

  if(myid==0) write(io,*) 'writing <chi|fstoic> for time: ', time*time_ref

  nphi = 1         

  allocate(phiM(nfbins),   phiSTD(nfbins))
  allocate(phiM_g(nfbins), phiSTD_g(nfbins))

  call computeScalarDissipationRate(io, chiSDR)

  df = 1.0/nfbins

  !-------------------------- compute the avg in each mix frac bin
  
  phiM      = 0.0
  phiM_g    = 0.0
  sumFbin   = 0.0
  sumFbin_g = 0.0

  osuml = 0.0
  osumg = 0.0

  do i=1,nx
    do j=1,ny
      do k=1,nz

        if(k==1) osuml = osuml + chiSDR(i,j,k)

        ixi = mixfrac(i,j,k) / df           ! f bin; same for all phi at i,j,k; integer div
        ixi = ixi + 1
        if(ixi > nfbins) ixi = nfbins
        sumFbin(ixi) = sumFbin(ixi) + 1.0
  
        phiM(ixi) = phiM(ixi) + chiSDR(i,j,k)

      enddo
    enddo
  enddo

  call MPI_ALLREDUCE(sumFbin, sumFbin_g,   nfbins, MPI_REAL8, MPI_SUM, gcomm, ierr )
  call MPI_ALLREDUCE(phiM,    phiM_g,      nfbins, MPI_REAL8, MPI_SUM, gcomm, ierr )
  
  call MPI_ALLREDUCE(osuml, osumg,   1, MPI_REAL8, MPI_SUM, gcomm, ierr )

  do i=1,nfbins
    if(sumFbin_g(i) .ne. 0.0)  phiM_g(i) = phiM_g(i) / sumFbin_g(i)
  enddo

  osumg = osumg / (nx*ny)/2048
  if(myid == 0) write(*,*) 'osumg = ', osumg
  osuml = maxval(chiSDR)
  call MPI_ALLREDUCE(osuml, osumg,   1, MPI_REAL8, MPI_MAX, gcomm, ierr )
  if(myid == 0) write(*,*) 'max chi = ', osumg
  

  !------------------------ output the results

  if(myid == 0) then
    write(time_ext,'(1pe9.3)') time * time_ref
    filename = '../post/' // 'chiCondFstoic' // '.' // trim(time_ext) // '.dat'
    open(unit=22,file=filename,status='unknown',form='formatted')

    write(22,100) '# mixfrac', 'chi_(1/s)'
  
    do i=1,nfbins
      write(22,101) df*i-df/2.0, phiM_g(i)
    enddo

    close(22)

  endif


100 format(1000(A12,2x))
101 format(1000(1pe12.5,2x))

end subroutine chi_cond_fstoic

!================================================================================

subroutine surfChi(io)

  use param_m
  use topology_m
  use variables_m
  use reference_m
  use mixFrac_m
  use grid_m
  use runtime_m,    only : run_title, time
  use chemkin_m

  implicit none

  integer, intent(in)                   :: io

  character*100                         :: filename
  character*9                           :: time_ext
  integer                               :: i,j,k,L, ixi, ii
  integer                               :: nphi, nphi_g
  
  real                                  :: phiM, phiM_g
  real, dimension(nx,ny,nz)             :: chiSDR
  
  real :: fstoic = 0.25
  

!----------------------------------------------------------------------------------------

  call computeScalarDissipationRate(io, chiSDR)

  do i=1,nx-1
    do j=1,ny-1
      do k=1,nz-1

        if( (mixfrac(i,j,k)-fstoic)*(mixfrac(i+1,j,k)-fstoic) < 0.0 ) then
          phiM = phiM + chiSDR(i,j,k) + (chiSDR(i+1,j,k)-chiSDR(i,j,k))   / &
                                (mixfrac(i+1,j,k)-mixfrac(i,j,k)) * &  
                                (fstoic - mixfrac(i,j,k))
          nphi = nphi+1
        endif
        if( (mixfrac(i,j,k)-fstoic)*(mixfrac(i,j+1,k)-fstoic) < 0.0 ) then
          phiM = phiM + chiSDR(i,j,k) + (chiSDR(i,j+1,k)-chiSDR(i,j,k))   / &
                                (mixfrac(i,j+1,k)-mixfrac(i,j,k)) * &  
                                (fstoic - mixfrac(i,j,k))
          nphi = nphi+1
        endif
        if( (mixfrac(i,j,k)-fstoic)*(mixfrac(i,j,k+1)-fstoic) < 0.0 ) then
          phiM = phiM + chiSDR(i,j,k) + (chiSDR(i,j,k+1)-chiSDR(i,j,k))   / &
                                (mixfrac(i,j,k+1)-mixfrac(i,j,k)) * &  
                                (fstoic - mixfrac(i,j,k))
          nphi = nphi+1
        endif

      enddo
    enddo
  enddo


  call MPI_ALLREDUCE(nphi, nphi_g, 1 , MPI_INTEGER, MPI_SUM, gcomm, ierr )
  call MPI_ALLREDUCE(phiM, phiM_g, 1 , MPI_REAL8,   MPI_SUM, gcomm, ierr )
 
  phiM = phiM / nphi

  !------------------------ output the results

  if(myid == 0) then
    write(time_ext,'(1pe9.3)') time * time_ref
    write(io,*) "time = ", time_ext, " <chi|fstoic> = ", phiM
  endif

end subroutine surfChi










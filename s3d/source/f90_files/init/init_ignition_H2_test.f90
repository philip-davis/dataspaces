!========================================================================================
  subroutine initialize_ignition_H2_test(io,yspecies,temp,pressure,u)
!========================================================================================
!----------------------------------------------------------------------------------------
  use topology_m
  use grid_m 
  use param_m, only : nx, ny, nz, nsc, npx, npy, npz
  use chemkin_m, only : species_name, rckwrk, ickwrk
  use reference_m
  use bc_m, only : pout

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real yspecies(nx,ny,nz,nsc+1), temp(nx,ny,nz), pressure(nx,ny,nz), u(nx,ny,nz,3)

  integer io

! local declarations

  integer i, j, k, L, m, n, i_scal
  integer idum, jdum, kdum
  real dum, temp_in, velo_x, velo_y, pres_in, ydns_in
  real tstep, time, time_save
  character*4 ext
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing counterflow test...'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------

   call MPI_Barrier(gcomm,ierr)

!----------------------------------------------------------------------------------------
! set variables

  yspecies=0.0
! skip fields up to xid nodes

   do k=1,nz,1
     do j=1,ny,1
       do i=1,nx,1

         u(i,j,k,1)=0.0
         u(i,j,k,2)=0.0
         u(i,j,k,3)=0.0
         pressure(i,j,k)=30.0*101325.0/p_ref    !30 atm
!         pressure(i,j,k)=101325.0/p_ref*(1. &
!                      +3.0*exp(-((x(i)-(0.0012/l_ref))/(0.00010/l_ref))**2) &
!                          *exp(-((y(j)-(0.0012/l_ref))/(0.00010/l_ref))**2)
!       Y = Y_boundary*(1+(Y_center/Y_boundary-1)*....)
         do L = 1,nsc+1,1
             if(trim(species_name(L)).eq.'O2') &
             yspecies(i,j,k,L)=0.178*(1+(0.169/0.178-1)*exp(-((x(i)-(xmax/2.))/(0.0001/l_ref))**2))
             if(trim(species_name(L)).eq.'N2') &
             yspecies(i,j,k,L)=0.675*(1+(0.642/0.675-1)*exp(-((x(i)-(xmax/2.))/(0.0001/l_ref))**2))
             if(trim(species_name(L)).eq.'CO2') &
             yspecies(i,j,k,L)=0.075*(1+(0.072/0.075-1)*exp(-((x(i)-(xmax/2.))/(0.0001/l_ref))**2))
             if(trim(species_name(L)).eq.'H2O') &
             yspecies(i,j,k,L)=0.046*(1+(0.044/0.046-1)*exp(-((x(i)-(xmax/2.))/(0.0001/l_ref))**2))
             if(trim(species_name(L)).eq.'CH3OCH3') &
             yspecies(i,j,k,L)=0.026*(1+(0.073/0.026-1)*exp(-((x(i)-(xmax/2.))/(0.0001/l_ref))**2))
         enddo
!         yspecies(i,j,k)=700.0/t_ref*(1.0 &
!                       +3.0/7.0*exp(-((x(i)-(xmax/2.))/(0.0001/l_ref))**2)) !&
!                           *exp(-((y(j)-(ymax/2.))/(0.0002/l_ref))**2)) 
!                           *exp(-((z(k)-(zmax/2.))/(0.0002/l_ref))**2))
          temp(i,j,k) = 850.0/t_ref
       enddo
     enddo
   enddo

!   yspecies(:,:,:,1)=1.0/9.0
!   yspecies(:,:,:,2)=8.0/9.0
!   yspecies(:,:,:,1)=1.0/16.0
!   yspecies(:,:,:,2)=8.0/16.0
!   yspecies(:,:,:,9)=7.0/16.0
  
!  yspecies=0.0
!  do L = 1,nsc+1,1
!     if(trim(species_name(L)).eq.'O2') yspecies(:,:,:,L)=0.2255
!     if(trim(species_name(L)).eq.'N2') yspecies(:,:,:,L)=0.7421
!     if(trim(species_name(L)).eq.'CH3OCH3') yspecies(:,:,:,L)=0.0324
!  enddo


   pout = 101325.0/p_ref

!----------------------------------------------------------------------------------------
! check term status

  call check_term_status(io,0)
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_ignition_H2_test

#include "globalDefines.h"
!----------------------------------------------------------------------
  subroutine initialize_tracer_test &
    ( io, yspecies, temp, pressure, u, half_width, decay_factor )
!----------------------------------------------------------------------
! Based on lifted flame initialization
! Simple test case for tracer module
!----------------------------------------------------------------------
  use param_m
  use topology_m
  use grid_m, only : x, y, z, xmin, xmax, ymin, ymax, zmin, zmax
  use reference_m
  use chemkin_m, only : species_name
  use thermchem_m, only : avmolwt, calc_inv_avg_mol_wt

  use frozenfeed_m, only : vel_bar, feedVel

  implicit none
!----------------------------------------------------------------------
  real, intent(inout), dimension(nx,ny,nz) :: temp, pressure
  real, intent(inout), dimension(nx,ny,nz,n_spec) :: yspecies
  real, intent(inout), dimension(nx,ny,nz,3) :: u

  integer, intent(in) :: io
  real, intent(out) :: half_width, decay_factor

  character*100 :: filename
  integer, parameter :: io_lifted=204

  real u_inlet, press_in, co_flow, t_inlet, t_coflow, thickness, s, sigma
  real sigma2, Lx_heat, Lx_heat2
  real cp_H2, cp_air, Y_C2H4, t_max
  integer i, j, k
  real :: vel_bar_xinlet(ny, nz)


!----------------------------------------------------------------------
! write header

  if(myid == 0) then
    call write_header(io,'-')
    write(io,*) 'initializing tracer test ....'
    write(io,*)
  endif
!----------------------------------------------------------------------
! Read the lifted input file
  filename = '../input/lifted.in'
  call inquire_about_input_file(filename, io)

  if(myid .eq. 0) then
    open(unit=io_lifted, file = trim(filename), status = 'old')

    !header
    read(io_lifted, *)

    !slot half width (input in mm)
    read(io_lifted, *) half_width
      !convert to SI
      half_width = half_width*1e-3
    
    !inflow velocity in m/s
    read(io_lifted, *) u_inlet
    
    !coflow velocity in m/s
    read(io_lifted, *) co_flow
    
    !inflow temperature in K
    read(io_lifted, *) t_inlet
    
    !coflow temperature in K
    read(io_lifted, *) t_coflow
    
    !Inflow profile decays to zero using this factor
    !Fraction of h, half-width. Non-dimensionalization is not needed.
    read(io_lifted, *) decay_factor
    
    !Pressure at inlet 
    read(io_lifted, *) press_in
    
    close(io_lifted)
    
    !nondimensionalize
    half_width = half_width/l_ref
    u_inlet = u_inlet/a_ref
    co_flow = co_flow/a_ref
    t_inlet = t_inlet/t_ref
    t_coflow = t_coflow/t_ref
    press_in  = press_in*pres_atm/p_ref
  
  end if

! broadcast input data
  call MPI_Bcast(half_width,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(u_inlet,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(co_flow,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(t_inlet,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(t_coflow,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(decay_factor,1,MPI_REAL8,0,gcomm,ierr)
  call MPI_Bcast(press_in,1,MPI_REAL8,0,gcomm,ierr)

! set initial temperature and velocities
  temp(:,:,:) = t_coflow
  u(:,:,:,1) = co_flow
  u(:,:,:,2:3) = 0.0
  yspecies(:,:,:,:) = 0.0
  pressure(:,:,:) = press_in

! set C2H4(15th), O2(4th), and N2(22nd) profiles at inlet boundary
  s = decay_factor * half_width
  cp_H2 = 14.31 ! kJ/kg/K
  cp_air = 1.182 ! kJ/kg/K
  sigma = 2.5*s
!  Y_C2H4 = 0.20424
  Y_C2H4 = 0.18
  do j = 1, ny
    yspecies(:,j,1,15) = Y_C2H4*0.5 * &
                    (   tanh((y(j)-0.5*(ymax+ymin) + half_width)/sigma) &
                      - tanh((y(j)-0.5*(ymax+ymin) - half_width)/sigma))
    yspecies(:,j,1,22) = (1.0 - Y_C2H4)*0.5 * &
                    (   tanh((y(j)-0.5*(ymax+ymin) + half_width)/sigma) &
                      - tanh((y(j)-0.5*(ymax+ymin) - half_width)/sigma))
    yspecies(:,j,1,4) = 0.233*(1.0-yspecies(:,j,1,15)-yspecies(:,j,1,22))
    yspecies(:,j,1,22) = (1.0 - yspecies(:,j,1,15) - yspecies(:,j,1,4))
    u(:,j,1,1) = co_flow + (u_inlet-co_flow)*0.5*                            &
               (       tanh((y(j)-0.5*(ymax+ymin) + half_width )/sigma)   &
                     - tanh((y(j)-0.5*(ymax+ymin) - half_width )/sigma) )
  enddo

  Lx_heat = 12.0/1000/l_ref
  Lx_heat2 = 10.0/1000/l_ref

  t_max = 2300./t_ref
  
  do i = 1, nx
    do j = 1, ny
        if( xid == 0 .and. i == 1 ) then
            temp(i,j,1) = t_coflow - (t_coflow-t_inlet)*0.5*                       &
            (       tanh((y(j)-0.5*(ymax+ymin) + half_width)/sigma)     &
            - tanh((y(j)-0.5*(ymax+ymin) - half_width)/sigma) )  ! &
        else
            temp(i,j,1) = t_coflow
        endif
              !   * 0.5*(1.0-tanh((x(i)-Lx_heat2)/sigma))                    & 
              !   + (t_max-t_coflow)*0.5*                                    &
              !  ( tanh((y(j)-0.5*(ymax+ymin) + half_width + 5.0*s)/sigma)     &
              !  - tanh((y(j)-0.5*(ymax+ymin) - half_width - 5.0*s)/sigma) )   &
              !  * 0.5*(1.0+tanh((x(i)-Lx_heat)/sigma))
    enddo
  enddo
  
  if ( nz .gt. 1) then
    do k = 2, nz
      yspecies(:,:,k,15) = yspecies(:,:,1,15)
      yspecies(:,:,k,4) = yspecies(:,:,1,4)
      yspecies(:,:,k,22) = yspecies(:,:,1,22)
      temp(:,:,k) = temp(:,:,1)
      u(:,:,k,1) = u(:,:,1,1)
    enddo
  endif

  if(xid==0) then
     vel_bar_xinlet = u(1,:,:,1)
  endif
    
  call MPI_Bcast(vel_bar_xinlet,ny*nz,MPI_REAL8,0,xcomm,ierr) 
    
! set mean velocity for feeding turbulence
  feedVel = u_inlet 
  allocate(vel_bar(ny,nz))
  vel_bar = vel_bar_xinlet

!----------------------------------------------------------------------
  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------
  return
  end subroutine initialize_tracer_test


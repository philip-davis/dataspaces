#include "globalDefines.h"
!========================================================================================
  subroutine set_variables(io, q, qnx, qny, qnz, qnvar_tot, qn_reg)
!========================================================================================
! set various derived variables based on primitave variable initialization
!----------------------------------------------------------------------------------------
  use topology_m
  use param_m
  use variables_m
  use thermchem_m, only : i_react, Ru, cpmix, avmolwt, mixCp, mixEnth, calc_inv_avg_mol_wt
  use reference_m
  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io

  integer, intent(in) :: qnx, qny, qnz, qnvar_tot, qn_reg
  real, dimension(qnx,qny,qnz,qnvar_tot,qn_reg) :: q !solution vector (rho*u,rho*v,rho*w,rho,rho*e,rho*Y_i)

! local declarations

  integer i,j,k,n,m,L
  real r_gas, enthmix
!----------------------------------------------------------------------------------------
! zero q vector

  q=0.0

! calculate inverse of average molecular weight

  call calc_inv_avg_mol_wt(yspecies,avmolwt)

! set density in q-vector (equation of state)

  q(:,:,:,4,1)=pressure(:,:,:)/(Ru*temp(:,:,:)*avmolwt(:,:,:))
  volum(:,:,:)=1.0/q(:,:,:,4,1)

! calculate mixture specific heat

  do k=1,nz,1
    do j=1,ny,1
      do i=1,nx,1

        cpmix(i,j,k) = mixCp( yspecies(i,j,k,:),temp(i,j,k) )

      enddo
    enddo
  enddo

! set velocity in q-vector

  if(vary_in_x==0) u(:,:,:,1) = 0.0
  if(vary_in_y==0) u(:,:,:,2) = 0.0
  if(vary_in_z==0) u(:,:,:,3) = 0.0
  q(:,:,:,1,1)=u(:,:,:,1)*q(:,:,:,4,1)
  q(:,:,:,2,1)=u(:,:,:,2)*q(:,:,:,4,1)
  q(:,:,:,3,1)=u(:,:,:,3)*q(:,:,:,4,1)

! set energy in q-vector (velocity included)

  do k=1,nz,1
    do j=1,ny,1
      do i=1,nx,1

        r_gas=avmolwt(i,j,k)*Ru

        enthmix = mixEnth( yspecies(i,j,k,:), temp(i,j,k) )
        q(i,j,k,5,1)=(enthmix-r_gas*temp(i,j,k))*q(i,j,k,4,1)

      ! add in the kinetic energy
        if(vary_in_x==1) then
           q(i,j,k,5,1) = q(i,j,k,5,1) + 0.5*q(i,j,k,1,1)*q(i,j,k,1,1)/q(i,j,k,4,1)
        endif
        if(vary_in_y==1) then
           q(i,j,k,5,1) = q(i,j,k,5,1) + 0.5*q(i,j,k,2,1)*q(i,j,k,2,1)/q(i,j,k,4,1)
        endif
        if(vary_in_z==1) then
           q(i,j,k,5,1) = q(i,j,k,5,1) + 0.5*q(i,j,k,3,1)*q(i,j,k,3,1)/q(i,j,k,4,1)
        endif

      enddo
    enddo
  enddo

! set species in q-vector

  do L=1,nsc,1
    q(:,:,:,L+5,1)=q(:,:,:,4,1)*yspecies(:,:,:,L)
  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine set_variables

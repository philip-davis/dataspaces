#include "globalDefines.h"
!BUG FIX Evatt 3 NOV 2009 -ijkeq_loc and ijkeq_id  not initialized
!send and receive of ijkeq_loc were blocking, code hangs
!========================================================================================
  subroutine integrate(io)
!========================================================================================
! integrates solution vector one time step using ARK
!----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz

  use rk_m, only : Un
  use rk_m, only : jac_age
  use rk_m, only : tstep_init
  use rk_m, only : U_err => q_err
  use rk_m, only : tstep_vec
  use rk_m, only : alphad
  use rk_m, only : redo_step
  use rk_m, only : iter_status
  use rk_m, only : jac_method
  use rk_m, only : ijkeq_id, ijkeq_loc

  use runtime_m, only : i_time, tstep

  use reference_m, only : time_ref

  use variables_m, only : U_ => q

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed

  integer io

! local declarations

  integer i, j, k

  character*4 :: ext

!----------------------------------------------------------------------------------------
! TEST JACOBIANS IF DESIRED

  if(jac_method < 0) then
    call test_jacobians(io)
  endif
!----------------------------------------------------------------------------------------
! INITIALIZE VARIABLES

  if(i_time==1) then
    tstep=tstep_init/time_ref   !set timestep for first time
    U_err=0.0                   !must start with all registers at zero
    tstep_vec=0.0               !must start with all registers at zero
    ijkeq_loc(:)=0              !BUG FIX Evatt 3 NOV 2009 - not initialized
    ijkeq_id=0                  !BUG FIX Evatt 3 NOV 2009 - not initialized
  endif
!----------------------------------------------------------------------------------------
! WRITE TIMESTEP FILE

  call write_timestep_file(io)
!----------------------------------------------------------------------------------------
! SAVE U-VECTOR

  Un(:,:,:,:)=U_(:,:,:,:,1)
!----------------------------------------------------------------------------------------
! BEGIN/REDO INTEGRATION

  100 continue            !if redo_step=true then time integration is redone
  redo_step=.false.       !assume this to be true unless otherwise indicated
  iter_status(:,:,:)='unchecked'
!----------------------------------------------------------------------------------------
! UPDATE JACOBIANS

  call update_jacobians
!----------------------------------------------------------------------------------------
! PERFORM IMPLICIT ITERATION

  call implicit_integration

!----------------------------------------------------------------------------------------
! REDO STEP IF NECESSARY

  if(redo_step) then
    call prepare_redo_step(io)
    goto 100
  endif
!----------------------------------------------------------------------------------------
! AGE THE JACOBIAN AND WRITE TO DIAGNOSTIC FILE

  where(jac_age(:,:,:) >= 0)
    jac_age(:,:,:)=jac_age(:,:,:)+1
  endwhere
!----------------------------------------------------------------------------------------
! UPDATE SOLUTION VECTOR AND ERROR VECTOR

  call update_solution_vector
!----------------------------------------------------------------------------------------
! PREPARE NEXT TIMESTEP

  call prepare_timestep
!----------------------------------------------------------------------------------------
! SYNC PROCESSORS

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
  return
  end subroutine integrate
!========================================================================================
  subroutine write_timestep_file(io)
!========================================================================================
! writes timestep file for ark time integration
!----------------------------------------------------------------------------------------
  use rk_m, only : q_err
  use rk_m, only : i_time_cont
  use rk_m, only : q_err_max
  use rk_m, only : cfl_switch
  use rk_m, only : cfl_no
  use rk_m, only : ijkeq_loc, ijkeq_id
  use rk_m, only : tstep_limit

  use topology_m
  use param_m, only : nx, ny, nz, nvar_tot

  use runtime_m, only : time, tstep, run_title
  use reference_m, only : time_ref
  use variables_m, only : pressure, u, volum
  use runtime_m, only : i_time

! for calc_cfl_limit
  use thermchem_m, only : gamma
  use grid_m, only : delx, dely, delz
  use runtime_m, only : i_restart, time_restart

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed

  integer io

! local declarations

  logical exist

  integer mpireq(2)

  real q_err_max_l

  real tstep_cfl

  character*100 filename
!----------------------------------------------------------------------------------------
! write timestep data to output files if we are at the appropriate time step

  if((mod(i_time,i_time_cont) == 0).or.(i_time <= 10)) then

!   determine maximum error from the previous iteration

    q_err_max_l=maxval(q_err(:,:,:,:,1))
    
    call MPI_Allreduce(q_err_max_l,q_err_max,1,MPI_REAL8,MPI_MAX,gcomm,ierr)

!   processor 0 gets information about where the timestep was determined
!   from the processor which owns that timestep (ijkeq_id)

!   BUG FIX Evatt 3 Nov 2009  both send and receive were blocking
!   so code hangs if send and receive happen on same process

    if(myid == ijkeq_id) then
      call MPI_ISend(ijkeq_loc,4,MPI_INTEGER,0,9999,gcomm,mpireq(1),ierr)
    endif

    if(myid == 0) then
      call MPI_IRecv(ijkeq_loc,4,MPI_INTEGER,ijkeq_id,9999,gcomm,mpireq(2),ierr)
    endif

    if(myid==0) call MPI_Wait(mpireq(2),status,ierr)
    if(myid==ijkeq_id) call MPI_Wait(mpireq(1),status,ierr)

!   calculate cfl timestep limit (if desired)

    tstep_cfl=0.0
    if(cfl_switch == 1) then
     call calc_cfl_limit(tstep_cfl,u,volum,gamma,pressure,time_ref,delx,dely,delz,cfl_no)
    endif

!   write diagnostic information to a file
!
!   NOTE that the location ijkeq_loc refers to the grid point and equation
!   which determined the timestep.  This does NOT necessarily coincide with
!   the grid point where the maximum error (q_err_max) occurs.  This is 
!   because the time step is determined using an error history, not just the
!   maximum error for this time step...

    999 continue

    if(myid == 0) then

      filename='../data/'//trim(run_title)//'.ts.dat'
      inquire(file=trim(filename),exist=exist)

      if(exist) then

        open(unit=1,file=trim(filename),status='old',position='append')

        if((i_restart == 1).and.(i_time == 1)) then
          write(1,*)
          write(1,'(a37,1pe9.3,a6)') '# run was restarted here from time = ',  &
                                      time_restart,' (sec)'
        endif

      else

        open(unit=1,file=trim(filename),status='unknown')
        write(1,25)
        write(1,26)

      endif

      write(1,24) i_time, time*time_ref, tstep*time_ref, tstep_cfl, q_err_max,  &
                  ijkeq_loc(4), ijkeq_loc(1), ijkeq_loc(2), ijkeq_loc(3), trim(tstep_limit)

      close(1)

    endif

  endif

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! format statements

  24 format(i9,1x,4(1pe10.3,1x),3x,4(i4,3x),a)
  25 format('#timestep    time     time step  time step   maximum   ',  &
            '-----time step location----     time step')
  26 format('#  number    (sec)      (sec)    cfl (sec)    error    ',  &
            'eq_loc  i_loc  j_loc  k_loc      control')
!----------------------------------------------------------------------------------------
  return
  end subroutine write_timestep_file
!========================================================================================
  subroutine update_jacobians
!========================================================================================
! routine updates the jacobians
!----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz, nvar_tot

  use rk_m, only : jac_fresh, jac_method
  use rk_m, only : jac, jac_age, tstep_LU, iM
  use rk_m, only : rk_ai
  use rk_m, only : IPiv
!  use rk_m, only : jac_age_max

  use runtime_m, only : i_time, tstep

  use variables_m, only : U_ => q, temp

  implicit none
!----------------------------------------------------------------------------------------
! local declarations

  integer i, j, k
!----------------------------------------------------------------------------------------
! UPDATE JACOBIAN AND ITERATION MATRIX IF REQUIRED BY ITERATION CONTROL
! JAC_FRESH=0 AND JAC_AGE(I,J,K) < 0
!
! OR
!
! UPDATE JACOBIAN AND ITERATION MATRIX EVERY TIMESTEP IF ENFORCED
! JAC_FRESH=1
!----------------------------------------------------------------------------------------
  do k=1,nz
    do j=1,ny
      do i=1,nx

!       update jacobian

!        if((jac_age(i,j,k) < 0).or.(jac_fresh==1).or.(jac_age(i,j,k)>=jac_age_max)) then
        if((jac_age(i,j,k) < 0).or.(jac_fresh==1)) then
          jac_age(i,j,k)=0
          tstep_LU(i,j,k)=-1.0  !flag to update iM (always if Jacobian is updated)
          call get_dFdU_prim(Jac(i,j,k,:,:),U_(i,j,k,:,1),temp(i,j,k))
        endif

!       update iteration matrix

        if(tstep_LU(i,j,k) <= 0.0) then
          tstep_LU(i,j,k)=tstep
          call get_M_LU(Jac(i,j,k,:,:),tstep,rk_ai(2,2),iM(i,j,k,:,:),IPiv(i,j,k,:,:))
        endif

      enddo
    enddo
  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine update_jacobians
!========================================================================================
  subroutine implicit_integration
!========================================================================================
! routine performs implicit integration
!----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz, nvar_tot, n_spec

  use rk_m, only : jac_fresh, jac_method
  use rk_m, only : rk_rtol, rk_atol, rk_itol
  use rk_m, only : Un, Fns, Fs
  use rk_m, only : jac, jac_age, iM, IPiv
  use rk_m, only : U_err => q_err
  use rk_m, only : rk_ai, rk_ae, rk_time, nstage
  use rk_m, only : newton_iter_min, newton_iter_max, redo_limit
  use rk_m, only : iter_status, redo_step
  use rk_m, only : alphad
  use rk_m, only : max_iter

  use runtime_m, only : tstep, time, time_accum, i_time

  use variables_m, only : U_ => q, temp, yspecies
  use thermchem_m, only : avmolwt

  implicit none
!----------------------------------------------------------------------------------------
! local declarations

  real Ustar(nvar_tot)        !intermediate solution during modified Newton iteration
  real Xi(nvar_tot)           !explicit portion of solution at current stage
  real displ(nvar_tot)        !displacement of solution
  real resid(nvar_tot)        !residual of solution

  real resid_max_new          !maximum value of residual vector (current)
  real resid_max_old          !maximum value of residual vector (old)
  real displ_max_new          !maximum value of displacement vector (current)
  real displ_max_old          !maximum value of displacement vector (old)
  real dresid, ddispl         !dummy variable for calculating L2 norms

  real alphad_loc             !node-wise convergence rate based on displacement
  real alphar_loc             !node-wise convergence rate based on residual

  integer newton_iter         !index for newton iteration
  integer i_stage, j_stage    !stage counters
  integer i, j, k, n          !generic indices

  logical redo_step_g         !logical for redo of timestep (global)

  real tol                    !overall error tolerance
  real small                  !small negative number used to control species mass fracs
  real eps                    !small positive number used in alpha calculation
!----------------------------------------------------------------------------------------
! set some stuff

  tol=rk_atol/rk_rtol
  small=-10.0**(-precision(small))
  eps=abs(epsilon(eps)*10.0)

  max_iter=0
  alphad=0.0
!----------------------------------------------------------------------------------------
! begin stage loop

  do i_stage=2,nstage

!   update time_accum = t_n + c_i * tstep

    time_accum=time+rk_time(i_stage)*tstep

!   Compute stiff and non-stiff function using the i_stage-1 value of the U-vector.
!     rhsFs: contains stiff portion, i.e. the reaction rate
!     rhsFns: contains non-stiff portion, i.e. convection and diffusion
!   Note: both of these routines calculate the reaction rate since rr_r is a local array.
!     This is not very efficient, but fixing it would require placing rr_r in a module
!     or passing it from this routine into both rhs routines. Also, ARK requires so much
!     computational effort that the difference may not be significant.

!   non-stiff rhs evaluation
    call rhsf(U_(:,:,:,:,1),Fns(1,1,1,1,i_stage-1))

!   stiff rhs evaluation
    call rhsf_stiff(Fs(1,1,1,1,i_stage-1)) !Fs here is needed to form first Xi (first guess)

!   Begin implicit portion of time integration

    do k=1,nz
      do j=1,ny
        do i=1,nx

!         set initial guess

          Ustar(:)=U_(i,j,k,:,1)

!         update jacobian every stage if enforced

          if(jac_fresh==2) then

            call get_dFdU_prim(Jac(i,j,k,:,:),Ustar(:),temp(i,j,k))

            call get_M_LU(Jac(i,j,k,:,:),tstep,rk_ai(2,2),iM(i,j,k,:,:),IPiv(i,j,k,:,:))

          endif

!         compute the explicit portion of the iterative expression

          Xi(:)=0.0
          do j_stage=1, i_stage-1
            Xi(:) = Xi(:) + tstep * (rk_ae(i_stage,j_stage)*Fns(i,j,k,:,j_stage)  &
                                  +  rk_ai(i_stage,j_stage)* Fs(i,j,k,:,j_stage))
          enddo

!         initialize new and old maximum of residual and displacment vectors

          resid_max_new=0.0
          resid_max_old=0.0
          displ_max_new=0.0
          displ_max_old=0.0

!         begin modified Newton iteration current (i,j,k) grid point

          do newton_iter=1,newton_iter_max

!           keep running maximum of iterations

            if(newton_iter > max_iter) max_iter = newton_iter

!           update jacobian every iteration if enforced

            if(jac_fresh==3) then
              call get_dFdU_prim(Jac(i,j,k,:,:),Ustar(:),temp(i,j,k))
              call get_M_LU(Jac(i,j,k,:,:),tstep,rk_ai(2,2),iM(i,j,k,:,:),IPiv(i,j,k,:,:))
            endif

!           calculate stiff function

            call get_Fs_node(Fs(i,j,k,:,i_stage),Ustar,temp(i,j,k))

!           calculate residual vector

            resid(:) = Un(i,j,k,:) - Ustar(:) + Xi(:)  &
                     + tstep * rk_ai(i_stage,i_stage) * Fs(i,j,k,:,i_stage)

!           calcuate displacement vector

            call get_displ(iM(i,j,k,:,:),IPiv(i,j,k,:,:),resid(:),displ(:))

!           update iteration guess vector

            Ustar(:)=Ustar(:)+displ(:)

!           ensure that species mass fractions stay small

            do n=6,nvar_tot
              Ustar(n)=max(Ustar(n),small)
            enddo

!           normalize residual and displacement vectors

            resid(:)=abs(resid(:) / ( abs(Ustar(:)) + tol ))
            displ(:)=abs(displ(:) / ( abs(Ustar(:)) + tol ))

!           compute L2 norm over all equations of displacement and residual vectors

            ddispl=0.0
            dresid=0.0
            do n=6,nvar_tot
              ddispl=ddispl+displ(n)**2
              dresid=dresid+resid(n)**2
            enddo
            displ_max_new=sqrt(ddispl/real(nvar_tot-5))
            resid_max_new=sqrt(dresid/real(nvar_tot-5))

!           compute max norm over all equations of displacement and residual vectors

!            displ_max_new=maxval(displ(:))
!            resid_max_new=maxval(resid(:))

!           calculate nodal convergence rate (filter here!!!)

            if(newton_iter > 1) then
              if(displ_max_new < eps) then  !prevents stall on machine prec
                alphad_loc=eps
              else
                alphad_loc=displ_max_new/displ_max_old
              endif
              if(resid_max_new < eps) then  !prevents stall on machine prec
                alphar_loc=eps
              else
                alphar_loc=resid_max_new/resid_max_old
              endif
            else
              alphad_loc=eps
              alphar_loc=eps
            endif

!           calculate max convergence rate (running max over all stages and iterations)

            alphad(i,j,k) = max(alphad(i,j,k),alphad_loc)

!           pass off new values of maximum displacement and residual values to old

            displ_max_old=displ_max_new
            resid_max_old=resid_max_new

!           perform tests

            if(newton_iter>=newton_iter_min) then

!             convergence test

              if(displ_max_new < rk_itol) then
                iter_status(i,j,k)='iteration converged'
                U_(i,j,k,:,1)=Ustar(:)   !assign converged U
                goto 200                 !jump out of iteration loop
              endif

!             divergence test

!              if(alphad_loc > 1.0) then
!                iter_status(i,j,k)='iteration diverged'
!                redo_step=.true.
!                goto 200                !jump out of iteration loop
!              endif

!             slow convergence test

              if(newton_iter==newton_iter_max) then
                iter_status(i,j,k)='slow convergence'
                redo_step=.true.
                goto 200                !jump out of iteration loop
              endif

            endif

          enddo  !end of newton iteration

          200 continue

        enddo   !end of i loop
      enddo     !end of j loop
    enddo       !end of k loop

!   sync processors (right here could be the biggest wait time -> load balancing)

    call MPI_Barrier(gcomm,ierr)

!   check whether or not timestep must be redone

    call MPI_Allreduce(redo_step,redo_step_g,1,MPI_LOGICAL,MPI_LOR,gcomm,ierr)

    if(redo_step_g) then  !exit stage loop if any of the processors have to redo the timestep
      redo_step=.true.    !all PEs need to redo
      return              !jump out of stage loop and prepare for redo
    else                  !continue on with next stage
!     impose hard boundary conditions
      call impose_hard_bc(U_,yspecies,avmolwt)
    endif

  enddo  !end of stage loop
!----------------------------------------------------------------------------------------
  return
  end subroutine implicit_integration
!========================================================================================
  subroutine prepare_redo_step(io)
!========================================================================================
! Routine prepares for a redo step by simply dividing the previous timestep in half.
!
! Note: This routine is written without the MPI logic for iteration logic.
!   For simulations with iteration control, use routine prepare_redo_step_mpi
!   and modify accordingly.
!
! This routine DOES NOT set ijkeq_loc and ijkeq_id since they would be the same as
!   previous timestep.
!----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz, nvar_tot

  use rk_m, only : Un
  use rk_m, only : jac_age
  use rk_m, only : redo_limit
  use rk_m, only : alphad
  use rk_m, only : iter_status
  use rk_m, only : redo_count
  use rk_m, only : max_iter
  use rk_m, only : tstep_limit
  use rk_m, only : ijkeq_loc
  use rk_m, only : ijkeq_id
!  use rk_m, only : alpha_opt

  use runtime_m, only : i_time, tstep

  use reference_m, only : time_ref

  use variables_m, only : U_ => q

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

!  real tstep_alpha        !timestep based on convergence rate
  real tstep_iter         !timestep predicted by iteration control
  real tstep_l(2)         !local timestep used in minimum evaluations
  real tstep_g(2)         !local timestep used in minimum evaluations
  real fmax               !factors for bracketing timestep in error control

  integer i, j, k         !generic indices
!----------------------------------------------------------------------------------------
! set fmax

!  fmax=2.0
!----------------------------------------------------------------------------------------
! overwrite the current U-vector with the one from the last good completed step

  U_(:,:,:,:,1)=Un(:,:,:,:)

! advance redo count

  redo_count=redo_count+1

! check redo limit and terminate if exceeded

  if(redo_count > redo_limit) then
    if(myid == 0) then
      write(io,*) 'Error: Maximum number of redo steps exceeded!'
      write(io,*) 'redo_limit =',redo_limit
      write(io,*) 'i_time =',i_time
      write(io,*) 'tstep =',tstep*time_ref
    endif
    call terminate_run(io,1)
  endif

! set timesteps

  tstep=tstep/2.0

! set tstep_limit

  tstep_limit='timestep redo'
!----------------------------------------------------------------------------------------
! write indication of redo

  if(myid == 0) then
    write(io,100)  'redoing timestep number ', i_time, ', new timestep = ',tstep*time_ref
  endif
!----------------------------------------------------------------------------------------
! format statements

  100 format(1x,a,i7,a,1pe9.3)
!----------------------------------------------------------------------------------------
  return
  end subroutine prepare_redo_step
!========================================================================================
  subroutine prepare_redo_step_mpi(io)
!========================================================================================
! Routine prepares for a redo step by simply dividing the previous timestep in half.
!
! Note: This routine is written with the MPI logic in place for future iteration logic
!   implementation. For simulations without iteration control, use routine
!   prepare_redo_step since it is more efficient.
!
! This routine sets ijkeq_loc and ijkeq_id appropriately in case such information
!   is needed for MPI runs.
!----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz, nvar_tot

  use rk_m, only : Un
  use rk_m, only : jac_age
  use rk_m, only : redo_limit
  use rk_m, only : alphad
  use rk_m, only : iter_status
  use rk_m, only : redo_count
  use rk_m, only : max_iter
  use rk_m, only : tstep_limit
  use rk_m, only : ijkeq_loc
  use rk_m, only : ijkeq_id
!  use rk_m, only : alpha_opt

  use runtime_m, only : i_time, tstep

  use reference_m, only : time_ref

  use variables_m, only : U_ => q

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

!  real tstep_alpha        !timestep based on convergence rate
  real tstep_iter         !timestep predicted by iteration control
  real tstep_l(2)         !local timestep used in minimum evaluations
  real tstep_g(2)         !local timestep used in minimum evaluations
  real fmax               !factors for bracketing timestep in error control

  integer i, j, k         !generic indices
!----------------------------------------------------------------------------------------
! set fmax

!  fmax=2.0
!----------------------------------------------------------------------------------------
! overwrite the current U-vector with the one from the last good completed step

  U_(:,:,:,:,1)=Un(:,:,:,:)

! advance redo count

  redo_count=redo_count+1

! check redo limit and terminate if exceeded

  if(redo_count > redo_limit) then
    if(myid == 0) then
      write(io,*) 'Error: Maximum number of redo steps exceeded!'
      write(io,*) 'redo_limit =',redo_limit
      write(io,*) 'i_time =',i_time
      write(io,*) 'tstep =',tstep*time_ref
    endif
    call terminate_run(io,1)
  endif

! set temporary timesteps to be large

  tstep_iter=huge(tstep_iter)
  tstep_l(1)=huge(tstep_l(1))

! begin ijk loop for iteration control on redo

  do k=1,nz
    do j=1,ny
      do i=1,nx

!       calculate timestep based on convergence rate
!        tstep_alpha=tstep*alpha_opt/max(alphad(i,j,k),alpha_opt/fmax)

!       iteration logic - divergence

        if(trim(iter_status(i,j,k)) == 'iteration diverged') then
          tstep_iter=tstep/2.0
        endif

!       iteration logic - slow convergence

        if(trim(iter_status(i,j,k)) == 'slow convergence') then
          tstep_iter=tstep/2.0
        endif

!       keep running minimum

        if(tstep_iter < tstep_l(1)) then
          tstep_l(1)=tstep_iter
          ijkeq_loc(1)=i+(xid*nx)
          ijkeq_loc(2)=j+(yid*ny)
          ijkeq_loc(3)=k+(zid*nz)
          ijkeq_loc(4)=0  !equation information cannot be known
        endif

!       end ijk loop

      enddo
    enddo
  enddo
!----------------------------------------------------------------------------------------
! sync processors

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! determine global minimum timestep and processor location

  tstep_l(2)=float(myid)

#if defined(ARCH_SGI) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
    call MPI_Allreduce(tstep_l,tstep_g,1,MPI_2DOUBLE_PRECISION,MPI_MINLOC,gcomm,ierr)
#endif
#if defined(ARCH_T3E)
    call MPI_Allreduce(tstep_l,tstep_g,1,MPI_2REAL,MPI_MINLOC,gcomm,ierr)
#endif

! set tstep where tstep_cont_g(1) contains the minimum time-step

  tstep=tstep_g(1)

! set ijkeq_id where tstep_cont_g(2) contains the processor ID
! which has the gridpoint that determined the minimum time-step

  ijkeq_id=tstep_g(2)

! set tstep_limit

  tstep_limit='timestep redo'
!----------------------------------------------------------------------------------------
! write indication of redo

  if(myid == 0) then
    write(io,100)  'redoing timestep number ', i_time, ', new timestep = ',tstep*time_ref
  endif
!----------------------------------------------------------------------------------------
! format statements

  100 format(1x,a,i7,a,1pe9.3)
!----------------------------------------------------------------------------------------
  return
  end subroutine prepare_redo_step_mpi
!========================================================================================
  subroutine update_solution_vector
!========================================================================================
! routine updates solution vector upon successful iteration convergence
!
! BUG FIX 10-SEP-04 Evatt Hawkes
! - avmolwt was not updated after the sub-stage, leading to first order errors in 
!   the step T and pressure
!----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz, nvar_tot, iforder

  use rk_m, only : rk_rtol, rk_atol
  use rk_m, only : Un
  use rk_m, only : rk_b, rk_err
  use rk_m, only : Fns, Fs
  use rk_m, only : nstage
  use rk_m, only : U_err => q_err
  use rk_m, only : tstep_vec, cont_n_reg

  use runtime_m, only : tstep, i_time

  use variables_m, only : U_ => q, temp, pressure, u, yspecies, volum
  use variables_m, only : get_mass_frac  !routine reference

  use thermchem_m, only : gamma, avmolwt, cpmix, mixMW
  use thermchem_m, only : calc_temp, calc_gamma, calc_press, calc_inv_avg_mol_wt  
                          !routine references

  use filter_m, only : i_time_fil
  use filter_m, only : filter  !routine reference

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed

  integer io

! local declarations

  integer i_stage             !stage counters
  integer n                   !generic index

  real tol                    !overall error tolerance
  real small                  !small negative number used to control species mass fracs
!----------------------------------------------------------------------------------------
! set some stuff

  tol=rk_atol/rk_rtol
  small=-10.0**(-precision(small))
!----------------------------------------------------------------------------------------
! compute Fns at nstage (current Fs is for nstage and does not need to be updated)

  call rhsf(U_(:,:,:,:,1),Fns(1,1,1,1,nstage))

! pass off previous timesteps

  do n=cont_n_reg,2,-1
    tstep_vec(n)=tstep_vec(n-1)
  enddo
  tstep_vec(1)=tstep

! pass off previous errors

  do n=cont_n_reg,2,-1
    U_err(:,:,:,:,n)=U_err(:,:,:,:,n-1)
  enddo

! set initial values of U and U_err

  U_(:,:,:,:,1)=Un(:,:,:,:)
  U_err(:,:,:,:,1)=0.0

! begin update

  do i_stage=1,nstage

!   non-stiff contribution

    U_(:,:,:,:,1)=U_(:,:,:,:,1) + tstep*rk_b(i_stage)*Fns(:,:,:,:,i_stage)
    U_err(:,:,:,:,1)=U_err(:,:,:,:,1) + tstep*rk_err(i_stage)*Fns(:,:,:,:,i_stage)

!   stiff contribution

    U_(:,:,:,:,1)=U_(:,:,:,:,1) + tstep*rk_b(i_stage)*Fs(:,:,:,:,i_stage)
    U_err(:,:,:,:,1)=U_err(:,:,:,:,1) + tstep*rk_err(i_stage)*Fs(:,:,:,:,i_stage)

  enddo
!----------------------------------------------------------------------------------------
! normalize error

  U_err(:,:,:,:,1)=abs(U_err(:,:,:,:,1))/(abs(U_(:,:,:,:,1))+tol)
!----------------------------------------------------------------------------------------
! filter out high-wavenumber waves

  if((iforder > 1).and.(i_time_fil > 0)) then
    if(mod(i_time,i_time_fil) == 0) then
       do n=1,nvar_tot
         call filter(U_(1,1,1,n,1),io)
       enddo
    endif
  endif
!----------------------------------------------------------------------------------------
! ensure that species mass fractions stay small

  do n=6,nvar_tot,1
   U_(:,:,:,n,1)=max(U_(:,:,:,n,1),small)
  enddo
!----------------------------------------------------------------------------------------
! extract primative variables and enforce hard boundary conditions
! for last stage (after filter and species trim)

! impose hard boundary conditions
  call impose_hard_bc(U_,yspecies,avmolwt)

  call get_mass_frac(U_(1,1,1,1,1),volum,yspecies)
  call get_velocity_vec(u,U_(1,1,1,1,1),volum)

! BUG FIX 10-SEP-04 - avmolwt ws not updated after the sub-stage,
! leading to first order errors in the step T and pressure
  call calc_inv_avg_mol_wt( yspecies, avmolwt )         ! set inverse of mixture MW

  call calc_temp(temp,U_(:,:,:,5,1)*volum, u, yspecies )   !set T, Cp_mix
  call calc_gamma( gamma, cpmix, mixMW )                   !set gamma
  call calc_press( pressure, U_(:,:,:,4,1), temp, mixMW )  !set pressure
!----------------------------------------------------------------------------------------
  return
  end subroutine update_solution_vector
!========================================================================================
  subroutine prepare_timestep
!========================================================================================
! routine prepares for next timestep
!----------------------------------------------------------------------------------------
  use topology_m

  use param_m, only : nx, ny, nz, nvar_tot

  use rk_m, only : jac_age
  use rk_m, only : tstep_init
  use rk_m, only : cont_switch
  use rk_m, only : alphad
  use rk_m, only : iter_status
  use rk_m, only : max_iter
  use rk_m, only : tstep_min, tstep_max
  use rk_m, only : cont_factor, cont_n_reg
  use rk_m, only : tstep_limit
  use rk_m, only : ijkeq_loc
  use rk_m, only : ijkeq_id
!  use rk_m, only : alpha_opt

  use runtime_m, only : i_time, tstep

  use reference_m, only : time_ref

  implicit none
!----------------------------------------------------------------------------------------
! local declarations

!  real tstep_alpha        !timestep based on convergence rate
  real tstep_PID          !timestep based error control
  real tstep_iter         !timestep predicted by iteration control
  real tstep_l(2)         !local timestep used in minimum evaluations
  real tstep_g(2)         !global timestep used in minimum evaluations
!  real fmax               !factors for bracketing timestep in error control

  integer eq              !local equation location
  integer i, j, k, n      !generic indices
!----------------------------------------------------------------------------------------
! set fmax

!  fmax=2.0
!----------------------------------------------------------------------------------------
! initialize ijkeq_loc and ijkeq_id

  ijkeq_loc(:)=0
  ijkeq_id=0
!----------------------------------------------------------------------------------------
! no controller

  if(cont_switch==0) then

    tstep=tstep_init/time_ref
    tstep_limit='fixed timestep'
    goto 100  !skip MPI stuff

! goofy controller

  elseif(cont_switch==-1) then

    tstep=tstep*1.1
    tstep_limit='fixed increase of 10%'
    goto 100  !skip MPI stuff

! initial timestep

  elseif(i_time < cont_n_reg) then

    if(tstep < tstep_init/time_ref) then  !keep lower tstep from redo_step
      tstep_limit='maintained from a redo step'
    else
      tstep=tstep_init/time_ref
      tstep_limit='initial timestep'
    endif
    goto 100  !skip MPI stuff

! use controller

  else

!   set temporary timestep to be large

    tstep_l(1)=huge(tstep_l(1))

!   begin ijk loop

    do k=1,nz
      do j=1,ny
        do i=1,nx

!         check iteration status

          if(trim(iter_status(i,j,k)(1:19)) /= 'iteration converged') then
            write(6,*) 'ERROR with iter_status'
            write(6,*) 'i = ',i
            write(6,*) 'j = ',j
            write(6,*) 'k = ',k
            write(6,*) 'iter_status = ',iter_status(i,j,k)
            stop
          endif

!         calculate timestep based on error control

          call controller_node(tstep_PID,eq,i,j,k)

!         calculate timestep based on convergence rate
!          tstep_alpha=tstep*alpha_opt/max(alphad(i,j,k),alpha_opt/fmax)

!         select timestep (add iteration control for converged cases here!)

          tstep_iter=tstep_PID  !just take PID for now

!         keep running minimum

          if(tstep_iter < tstep_l(1)) then
            tstep_l(1)=tstep_iter
            ijkeq_loc(1)=i+(xid*nx)
            ijkeq_loc(2)=j+(yid*ny)
            ijkeq_loc(3)=k+(zid*nz)
            ijkeq_loc(4)=eq
          endif

!         end ijk loops

        enddo
      enddo
    enddo

!   set tstep_limit

    tstep_limit='error control'

  endif
!----------------------------------------------------------------------------------------
! sync processors

  call MPI_Barrier(gcomm,ierr)
!----------------------------------------------------------------------------------------
! determine global minimum timestep and processor location

  tstep_l(2)=float(myid)

#if defined(ARCH_SGI) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
    call MPI_Allreduce(tstep_l,tstep_g,1,MPI_2DOUBLE_PRECISION,MPI_MINLOC,gcomm,ierr)
#endif
#if defined(ARCH_T3E)
    call MPI_Allreduce(tstep_l,tstep_g,1,MPI_2REAL,MPI_MINLOC,gcomm,ierr)
#endif

! set tstep where tstep_cont_g(1) contains the minimum time-step

  tstep=tstep_g(1)

! set ijkeq_id where tstep_cont_g(2) contains the processor ID
! which has the gridpoint that determined the minimum time-step

  ijkeq_id=tstep_g(2)
!----------------------------------------------------------------------------------------
! continue

  100 continue
!----------------------------------------------------------------------------------------
! set minimum timestep

  if(tstep*time_ref < tstep_min) then
    tstep=tstep_min/time_ref
    tstep_limit='minimum timestep'
  endif

! set maximum timestep

  if(tstep*time_ref > tstep_max) then
    tstep=tstep_max/time_ref
    tstep_limit='maximum timestep'
  endif

! set maximum change in timestep

  if(tstep/tstep.gt.cont_factor) then
    tstep=tstep*cont_factor
    tstep_limit='maximum change'
  endif
!----------------------------------------------------------------------------------------
  return
  end subroutine prepare_timestep
!========================================================================================
  subroutine controller_node(tstep_error,eq,i,j,k)
!========================================================================================
! performs error control for ARK time integration on a node basis
! controller designed never to return a number less than or equal to zero
!----------------------------------------------------------------------------------------
  use param_m, only : nx, ny, nz, nvar_tot

  use runtime_m, only : i_time

  use rk_m, only : tstep_init
  use rk_m, only : cont_n_reg, cont_type, rk_p
  use rk_m, only : rk_rtol, cont_safety
  use rk_m, only : tstep_vec
  use rk_m, only : U_err => q_err
  use rk_m, only : k_I, k_P, k_D, k_D2

  use reference_m, only : time_ref

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed

  real :: tstep_error         !timestep predicted by error control (output)
  integer :: eq               !equation location (output)
  integer :: i, j, k          !node location (input)

! local declarations

  real :: expc(6)             !controller exponent (hard-coded)
  real :: tstep_t             !temporary timestep
  real :: sign                !temporary timestep

  real :: w(2)
  real :: alpha_cont
  real :: beta_cont
  real :: gamma_cont
  real :: delta_cont

  integer :: L, n

! BUG FIX 10-SEP-2004 Evatt Hawkes
  real vsmall
!----------------------------------------------------------------------------------------
! zero tstep_error

  tstep_error=0.0
!----------------------------------------------------------------------------------------
! set H321 controller properties (for ARK in zero-D)

  if(trim(cont_type)=='H321') then

    expc(1) =  1.0 / ( 3.0 * rk_p)
    expc(2) = -1.0 / (18.0 * rk_p)
    expc(3) = -5.0 / (18.0 * rk_p)
    expc(4) =  0.0
    expc(5) =  5.0 /   6.0
    expc(6) =  1.0 /   6.0

! set PIDD controller properties (for ARK in multidimensions)

  else

!   calculate ratio of time steps (hard-coded to incorporate up to PID2 controller)

    w(1)=tstep_vec(1)/tstep_vec(2)
    w(2)=tstep_vec(2)/tstep_vec(3)

!   set alpha_cont, beta_cont, gamma_cont, and delta_cont based on time-step ratio
!   (hard-coded to incorporate up to PID2 controller)

    alpha_cont=k_I                            &
              +k_P                            &
              +((2.0*w(1))/(1.0+w(1)))*k_D    &
              +((6.0*(w(1)**2)*w(2))/((1.0+w(1))*(1.0+w(2)+w(1)*w(2))))*k_D2

    beta_cont=k_P                               &
             +2.0*w(1)*k_D                      &
             +((6.0*(w(1)**2)*w(2))/(1.0+w(2)))*k_D2

    gamma_cont=((2.0*(w(1)**2))/(1.0+w(1)))*k_D          &
              +((6.0*(w(1)**3)*w(2))/(1.0+w(1)))*k_D2

    delta_cont=((6.0*(w(1)**3)*(w(2)**3))                &
              /((1.0+w(2))*(1.0+w(2)+w(1)*w(2))))*k_D2

    expc(1) = alpha_cont/rk_p
    expc(2) = beta_cont/rk_p
    expc(3) = gamma_cont/rk_p
    expc(4) = delta_cont/rk_p
    expc(5) = 0.0
    expc(6) = 0.0

  endif
!----------------------------------------------------------------------------------------
! set tstep_error to large number to start

  tstep_error=huge(tstep_error)

! BUG FIX 10-SEP-2004 Evatt Hawkes
! set a minimum error to avoid divide by zero
  vsmall = tiny(vsmall)  

! perform error control

  do L=1,nvar_tot

    tstep_t=tstep_vec(1)*cont_safety

    do n=1,cont_n_reg
      sign=(-1.0)**n
      tstep_t=tstep_t*((U_err(i,j,k,L,n)+vsmall)/rk_rtol)**(sign*expc(n))
    enddo

    if(trim(cont_type)=='H321') then  !for H321 controller
      do n=1,2
        tstep_t=tstep_t*(tstep_vec(n)/tstep_vec(n+1))**(expc(4+n))
      enddo
    endif

    if((tstep_t > 0.0).and.(tstep_t < tstep_error)) then
      tstep_error=tstep_t
      eq=L
    endif

  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine controller_node
!========================================================================================
  subroutine rhsf_stiff(Fs)
!========================================================================================
! This routine addes the reaction rate term to the stiff function evaluation for all
! grid points.
!----------------------------------------------------------------------------------------
  use param_m, only : nx, ny, nz, n_spec, nvar_tot

  use variables_m, only : temp, pressure, yspecies

  use reference_m, only : g_ref, rho_ref, a_ref, l_ref, t_o

  use chemkin_m, only : reaction_rate

  use runtime_m, only : tstep

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real Fs(nx,ny,nz,nvar_tot)  !stiff function evaluation

! local variables

  real, dimension(nx,ny,nz,n_spec) :: rr_r
#ifdef GETRATES_NEEDS_DIFFUSION
  real, dimension(nx,ny,nz,n_spec) :: diffusion
#endif
  integer i, j, k, L
!----------------------------------------------------------------------------------------
! calcuate reaction rate term

#ifdef GETRATES_NEEDS_DIFFUSION
  diffusion = 0.0
  call reaction_rate(rr_r,temp,pressure,yspecies,diffusion,tstep,g_ref,rho_ref,a_ref,l_ref,t_o)
#else
  call reaction_rate(rr_r,temp,pressure,yspecies,g_ref,rho_ref,a_ref,l_ref,t_o)
#endif
!----------------------------------------------------------------------------------------
! add reaction rate term to stiff grid points
  
  do L=1,n_spec-1
    do k=1,nz
      do j=1,ny
        do i=1,nx
          Fs(i,j,k,5+L) = rr_r(i,j,k,L)
        enddo
      enddo
     enddo
  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine rhsf_stiff
!========================================================================================
  subroutine get_Fs_node(Fs_node,U,T_in)
!========================================================================================
! Routine calculates non-dimensional reaction rate (stiff function evaluation) Fs_node
! at a single node from a the solution vector U at the given grid point.
!
! Note that this routine must calculate temperature from the perturbed state using the
! same Newton-Raphson iteration procedure given in routine calc_temp.
!----------------------------------------------------------------------------------------
  use param_m, only : nvar_tot, nsc
  use reference_m, only : t_ref, a_ref, l_ref, p_ref, rho_ref
  use chemkin_m, only : ickwrk, rckwrk, molwt

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real U(nvar_tot)        !perturbed conserved variable array at given grid point (input)
  real T_in               !initial temperature guess for Newton-Raphson iteration (input)
  real Fs_node(nvar_tot)  !stiff function evaluation for grid point (output)

! local declarations

  real V(nvar_tot+1)    !perturbed primitive variable vector at given grid point
  real rr(nsc+1)        !reaction rate
  real tconv            !temperature conversion factor
  real pconv            !pressure conversion factor
  real rateconv         !reaction rate conversion factor
!-----------------------------------------------------------------------------------------
! set reference quantities

  tconv = t_ref
  pconv = p_ref*10.0
  rateconv = (l_ref * 1.0e6) / (rho_ref * a_ref)
!----------------------------------------------------------------------------------------
! get primitive variables

  call get_V_node(V,U,T_in)
!----------------------------------------------------------------------------------------
! get dimensional reaction rate

  call getrates(V(4)*pconv, V(5)*tconv, V(6:nvar_tot+1), ickwrk, rckwrk, rr)
!----------------------------------------------------------------------------------------
! non-dimensionalize reaction rate and fill Fs_node

  Fs_node(1:5) = 0.0
  Fs_node(6:5+nsc) = rr(1:nsc) * rateconv * molwt(1:nsc)
!----------------------------------------------------------------------------------------
  return
  end subroutine get_Fs_node
!========================================================================================
  subroutine get_dFdU_prim(dFdU,U_in,temp_in)
!========================================================================================
! Calculates the reaction rate Jacobian dFdU based on primative variables.
!
! The independent variable array U is given as:
!
!   U = [ rho*u, rho*v, rho*w, rho, rho*e_{0}, rho*Y_{1}, rho*Y_{2},...,rho*Y_{nsc-1} ].
!
! The dependent variable array F are the reaction rates and is given as:
!
!   F = [ 0, 0, ... , 0, rr_{1}, rr_{2}, ..., rr_{nsc} ].
!
! The routine contructs the Jacobian dFdU (referred to as the outer Jacobian) using
! an analytical expression for the product of the inverse of the transformation
! matrix dUdV and the Jacobian dFdV (referred to as the inner jacobian).
!
! The inner Jacobian dFdV may be calculated either using a numerical approach or using
! an analytical routine generated from the ADIFOR package.
!
! Note that the input U and the output Jacobian are both non-dimensional.
!
! For reference, see "Analytical Jacobian for S3D DNS Code using ADIFOR: Compressible 
!   Reacting Flows" by Christoper A. Kennedy.
!----------------------------------------------------------------------------------------
  use param_m, only : nvar_tot, n_spec
  use rk_m, only : jac_method
  use chemkin_m, only : molwt, molwt_c
  use reference_m

  use thermchem_m, only : mixCp, specEnth  !routine references

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real U_in(nvar_tot)               !input dependent conserved variable array
  real dFdU(nvar_tot,nvar_tot)      !numerical jacobian
  real temp_in                      !initial temperature guess for Newton-Raphson iteration

! other delcarations

  real V(nvar_tot+1)                !input dependent primitive variable array
  real dFdV(n_spec+2,n_spec)        !inner Jacobian calculated from primative variables
  real u(3), p, T, rho, Y(n_spec)   !primative variables in human space
  real cp, cv                       !mixture specific heats
  real avmolwt, r_gas               !mixture molecular weight (inverse) and gas constant
  real lowH(n_spec)                 !species enthalpies
  real capH(n_spec)                 !species enthalpies minus pressure term
  real scrH(n_spec-1)               !species enthalpies (minus pressure term) minus H(n_spec)
  real W                            !mixture molecular weight
  real u_mag                        !u^2 + v^2 + w^2
  real e0                           !total energy

  real WiN(n_spec-1)
  real phi
  real zeta
  real b44
  real b54
  real sumup

  integer i, j, k, n, m
!----------------------------------------------------------------------------------------
! zero jacobian

  dFdU(:,:)=0.0
!----------------------------------------------------------------------------------------
! get primitive variables

  call get_V_node(V,U_in,temp_in)

! calculate inner Jacobian using Adifor

  if(abs(jac_method)==1) then

    if(jac_method==-1) then
      write(6,*) '  Calling routine get_dFdV_adi...'
      write(6,*)
    endif

    call get_dFdV_adi(dFdV,V)

! calculate inner Jacobian using numerical approximation

  elseif(abs(jac_method)==2) then

    if(jac_method==-2) then
      write(6,*) '  Calling routine get_dFdV_num...'
      write(6,*)
    endif

    call get_dFdV_num(dFdV,V)

! error checking

  else

    write(6,*) 'Error! Improper setting for jac_method!'
    write(6,*) 'jac_method = ', jac_method
    write(6,*) 'See routine get_dFdU_prim.'
    call terminate_run(6,0)

  endif
!----------------------------------------------------------------------------------------
! compute primitive variables (non-dimensional)
!----------------------------------------------------------------------------------------
  u(1:3)=V(1:3)
  p=V(4)
  T=V(5)
  Y(1:n_spec)=V(6:nvar_tot+1)
  rho=U_in(4)
!----------------------------------------------------------------------------------------
! compute other basic quantities (non-dimensional)
!----------------------------------------------------------------------------------------
! set mixture specific heat at constant pressure

  cp = mixCp( Y, T )

! set species enthalpies

  do i=1,n_spec
    lowH(i)=specEnth( i, T )
  enddo

! set (inverse of) average molecular weight

  avmolwt = sum( Y(:)*molwt_c(:) )

! set average molecular weight

  W=1.0/avmolwt

! set r_gas

  r_gas = avmolwt * univ_gascon / cp_ref  !univ_gascon, cp_ref, and avmolwt are dimensional

! set mixture specific heat at constant volume

  cv=cp-r_gas

! set e0

  e0=U_in(5)/U_in(4)

! set u_mag

  u_mag = u(1)**2 + u(2)**2 + u(3)**2
!----------------------------------------------------------------------------------------
! calculate derived quantities for matrix inversion
! (dimensional ONLY if primitive and other basic quantities are dimensional)
!----------------------------------------------------------------------------------------
! set enthalpies minus pressure term (units of molecular weights cancel)

  capH(:)=lowH(:)-((p*W)/(rho*molwt(:)))

! enthalpies (minus pressure term) minus H(n_spec)

  do i=1,n_spec-1
    scrH(i)=capH(i)-capH(n_spec)
  enddo

! set WiN (units of molecular weights cancel)

  do i=1,n_spec-1
    WiN(i) = W * ( (1.0/molwt(i)) - (1.0/molwt(n_spec)) )
  enddo

! set phi

  phi=0.0
  do i=1,n_spec-1
    phi=phi+Y(i)*WiN(i)
  enddo

! set zeta

  zeta=0.0
  do i=1,n_spec-1
    zeta=zeta+scrH(i)*Y(i)
  enddo

! set b44

  b44=p*(cv*T*(1.0-phi)+zeta+(u_mag-e0))/(rho*cv*T)

! set b54

  b54=(zeta+(u_mag-e0))/(rho*cv)
!----------------------------------------------------------------------------------------
! fill in energy block (column 5 of jacobian)

  do i=1,n_spec-1
    dFdU(5+i,5)=((t*dFdV(2,i))+(p*dFdV(1,i)))/(rho*cv*T)
  enddo
!----------------------------------------------------------------------------------------
! fill in momentum block (columns 1, 2, 3 of jacobian)

  do m=1,3
    do i=1,n_spec-1
      dFdU(5+i,m)=-u(m)*((t*dFdV(2,i))+(p*dFdV(1,i)))/(rho*cv*T)
    enddo
  enddo
!----------------------------------------------------------------------------------------
! fill in density block (column 4 of jacobian)

  do i=1,n_spec-1

    sumup=0.0
    do j=1,n_spec-1
      sumup=sumup+(Y(j)/rho)*dFdV(2+j,i)
    enddo

    dFdU(5+i,4) = b44*dFdV(1,i) + b54*dFdV(2,i) - sumup

  enddo
!----------------------------------------------------------------------------------------
! fill in species block (column 6 through n_spec-1 of jacobian)

  do i=1,n_spec-1
    do j=1,n_spec-1
      dFdU(5+i,5+j) = (1.0/rho)*dFdV(2+j,i)  &
                   - (scrH(j)/(rho*cv*T))*(p*dFdV(1,i)+t*dFdV(2,i))  &
                   + ((p*WiN(j))/rho)*dFdV(1,i)
    enddo
  enddo
!----------------------------------------------------------------------------------------
  return
  end subroutine get_dFdU_prim
!=========================================================================================
  subroutine get_dFdV_adi(dFdV,V)
!=========================================================================================
! Calculates analytical reaction rate Jacobian based on primative variables using
! g_getrates, which is code autogenerated by the ADIFOR package.
!
! The independent variable array V is given as:
!
!     V = [ P, T, Y_{1}, y{2}, ..., y{n_spec} ].
!
! The dependent variable array F are the reaction rates and is given as:
!
!     F = [ rr_{1}, rr_{2}, ..., rr_{n_spec} ].
!
! Note that the input V and the output Jacobian are non-dimensional.
!
! Also note that this Jacobian is a smaller "inner" Jacobian with respect to the Jacobian
! needed by ARK, which is based on the entire solution vector U.
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec, nvar_tot
  use chemkin_m, only : ickwrk, rckwrk, lenr, molwt
  use reference_m, only : t_ref, p_ref, l_ref, rho_ref, a_ref

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real dFdV(n_spec+2,n_spec)    !Jacobian based on primative variables
  real V(nvar_tot+1)            !primative variables

! primative variables in usual notation

  real p, t, y(n_spec)

! ADIFOR variables

  integer g_p_, ldg_p, ldg_t, ldg_y, ldg_rckwrk, ldg_wspl
  real g_p(n_spec+2)
  real g_t(n_spec+2)
  real g_y(n_spec+2,n_spec)
  real g_rckwrk(n_spec+2,lenr)
  real rr(n_spec)

! other declarations

  real tconv, pconv, rateconv   !conversion factors
  integer i, j, n
!-----------------------------------------------------------------------------------------
! set g_getrates input (messy, but only happens here!)

  g_p_=n_spec+2
  ldg_p=n_spec+2
  ldg_t=n_spec+2
  ldg_y=n_spec+2
  ldg_rckwrk=n_spec+2
  ldg_wspl=n_spec+2

  g_p=0.0
  g_p(1)=1.0

  g_t=0.0
  g_t(2)=1.0

  g_y=0.0
  do i=1,n_spec+2
    do j=1,n_spec
      if(i==j+2) g_y(i,j)=1.0
    enddo
  enddo

  g_rckwrk(:,:)=0.0 !this is magic; why zero, I don't know, but it works???
!-----------------------------------------------------------------------------------------
! set reference quantities

  tconv = t_ref
  pconv = p_ref*10.0
  rateconv = (l_ref * 1.0e6) / (rho_ref * a_ref)
!-----------------------------------------------------------------------------------------
! set and dimensionalize primative variables

  p=V(4)*pconv
  t=V(5)*tconv

  y(1:n_spec)=V(6:nvar_tot+1)
!-----------------------------------------------------------------------------------------
! get reaction rate and jacobian from ADIFOR jacobian routine

  rr=0.0
  dFdV=0.0
  call g_getrates(g_p_, p, g_p, ldg_p, t, g_t, ldg_t, y, g_y, ldg_y,  &
                  ickwrk, rckwrk, g_rckwrk, ldg_rckwrk, rr, dFdV, ldg_wspl)
!-----------------------------------------------------------------------------------------
! non-dimensionalize jacobian

  dFdV(1,:)=dFdV(1,:)*(pconv*rateconv*molwt(:))           !pressure rows
  dFdV(2,:)=dFdV(2,:)*(tconv*rateconv*molwt(:))           !temperature rows
  do n=3,n_spec+2
    dFdV(n,:)=dFdV(n,:)*rateconv*molwt(:)                 !species rows
  enddo
!-----------------------------------------------------------------------------------------
! non-dimensionalize independent variables
! (these variables entered the routine non-dimensional and must exit as such!)

  p=p/pconv
  t=t/tconv
!-----------------------------------------------------------------------------------------
  return
  end
!=========================================================================================
  subroutine get_dFdV_num(dFdV,V)
!=========================================================================================
! Calculates numerical reaction rate Jacobian based on primative variables using
! a centered finite-difference formulation.

! The independent variable array V is given as:
!
!     V = [ P, T, Y_{1}, y{2}, ..., y{n_spec} ].
!
! The dependent variable array F are the reaction rates and is given as:
!
!     F = [ rr_{1}, rr_{2}, ..., rr_{n_spec} ].
!
! Note that the input V and the output Jacobian are non-dimensional.
!
! Also note that this Jacobian is a smaller "inner" Jacobian with respect to the Jacobian
! needed by ARK, which is based on the entire solution vector U.
!-----------------------------------------------------------------------------------------
  use rk_m
  use param_m, only : n_spec, nvar_tot
  use chemkin_m, only : ickwrk, rckwrk, leni, lenr, molwt
  use reference_m, only : t_ref, p_ref, l_ref, rho_ref, a_ref

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real dFdV(n_spec+2,n_spec)    !Jacobian based on primative variables
  real V(nvar_tot+1)            !primative variables (non-dimensional)

! other declarations

  real V_o(n_spec+2)      !original primative variables (dimenionsal)
  real V_e(n_spec+2)      !perturbed indepedent variable array
  real F_p(n_spec)        !dependent array for plus perturbation
  real F_m(n_spec)        !dependent array for minus perturbation
  real fdcoef(3)          !finite difference stencil coefficients
  integer s_width         !width of finite difference stencil
  real small              !small number to insure accurate numerical jacobian

  real tconv, pconv, rateconv   !conversion factors
  integer i, j, k, n
!----------------------------------------------------------------------------------------
! zero stuff out

  dFdV=0.0
  V_o=0.0
  V_e=0.0
  F_p=0.0
  F_m=0.0
  fdcoef=0.0
  s_width=0
!----------------------------------------------------------------------------------------
! set up central difference stencil coefficients

  s_width=int(jac_order/2)

  if(s_width == 1) then
    fdcoef(1) =  1.0 /  2.0
  elseif (s_width == 2) then
    fdcoef(1) =  2.0 /  3.0
    fdcoef(2) = -1.0 / 12.0
  elseif(s_width == 3) then
    fdcoef(1) =  3.0 /  4.0
    fdcoef(2) = -3.0 / 20.0
    fdcoef(3) =  1.0 / 60.0
  endif
!-----------------------------------------------------------------------------------------
! set reference quantities

  tconv = t_ref
  pconv = p_ref*10.0
  rateconv = (l_ref * 1.0e6) / (rho_ref * a_ref)
!----------------------------------------------------------------------------------------
! set and dimensionalize original primative variables

  V_o(1)=V(4)*pconv
  V_o(2)=V(5)*tconv

  V_o(3:n_spec+2)=V(6:nvar_tot+1)
!----------------------------------------------------------------------------------------
! check for zero entries in V

  small=10.0**(-precision(small))

  do i=1,n_spec+2
    if(V_o(i) < small) V_o(i)=small
  enddo
!----------------------------------------------------------------------------------------
! compute numerical jacobian dFdV

  do i=1,n_spec
    do j=1,n_spec+2

      do k=1,s_width

!       perturb the jth-element of V vector (plus)

        V_e(:)=V_o(:)

        V_e(j) = (1.0 + real(k)*jac_eps) * V_o(j)

!       calculate the independent variable F_p

        call getrates(V_e(1),V_e(2),V_e(3:n_spec+2),ickwrk,rckwrk,F_p)

!       perturb the jth-element of V vector (minus)

        V_e(:)=V_o(:)

        V_e(j) = (1.0 - real(k)*jac_eps) * V_o(j)

!       calculate the independent variable F_m

        call getrates(V_e(1),V_e(2),V_e(3:n_spec+2),ickwrk,rckwrk,F_m)

!       add the ith-value of independent variable to the numerator of the jacobian

        dFdV(j,i) = dFdV(j,i) + fdcoef(k) * (F_p(i) - F_m(i))

      enddo

!     divide dF by dV to get dFdV

      dFdV(j,i) = dFdV(j,i) / (jac_eps * V_o(j))

    enddo
  enddo
!-----------------------------------------------------------------------------------------
! non-dimensionalize jacobian

  dFdV(1,:)=dFdV(1,:)*(pconv*rateconv*molwt(:))           !pressure rows
  dFdV(2,:)=dFdV(2,:)*(tconv*rateconv*molwt(:))           !temperature rows
  do n=3,n_spec+2
    dFdV(n,:)=dFdV(n,:)*rateconv*molwt(:)                 !species rows
  enddo
!-----------------------------------------------------------------------------------------
  return
  end
!========================================================================================
  subroutine get_displ(M,IPiv,resid,displ)
!========================================================================================
! This Routine solves for the displacement by solving the Md=r where M has previously
! been decomposed using LU decomposition.
!
! M (input)         - iteration matrix stored as LU decomposition
! IPiv (input)      - stored pivot points
! resid (input)     - residual vector
! displ (output)    - displacement vector
!----------------------------------------------------------------------------------------
  use topology_m, only : myid
  use param_m, only : nvar_tot

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real :: M(nvar_tot,nvar_tot)
  integer :: IPiv(nvar_tot,nvar_tot)
  real :: resid(nvar_tot), displ(nvar_tot)

! local variables

  integer i
  integer info
!----------------------------------------------------------------------------------------
! set variables

  info=0

! calculate displacement

  displ=resid

  call sgetrs('N', nvar_tot, 1, M, nvar_tot, IPiv, displ, nvar_tot, info)
!  call get_inv(M,nvar_tot)         !for debugging only
!  displ=matmul(M(:,:),resid(:))    !for debugging only

  if(info.ne.0) then
    write(6,*) 'ERROR from routine sgetrf: info = ', info
    call terminate_run(6,1)
  endif
!----------------------------------------------------------------------------------------
  return
  end subroutine get_displ
!========================================================================================
  subroutine get_M_inv(Jac,tstep,rk_gamma,M_inv)
!========================================================================================
! routine computes the iteration matrix M_inv at a given grid point
!
! Jac (input)       - jacobian matrix at the node
! tstep (input)     - time step
! rk_gamma (input)  - the diagonal butcher coeff
! M_Inv (output)    - inverse of iteration matrix 
!----------------------------------------------------------------------------------------
  use topology_m, only : myid
  use param_m, only : nvar_tot

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real :: Jac(nvar_tot,nvar_tot), M_inv(nvar_tot,nvar_tot)
  real :: tstep, rk_gamma

! local variables

  integer i
!----------------------------------------------------------------------------------------
! calculate M_inv

  M_Inv(:,:) = -(tstep * rk_gamma * Jac(:,:))

  do i=1,nvar_tot
    M_Inv(i,i) = 1.0 + M_Inv(i,i)
  enddo

  call get_inv(M_Inv,nvar_tot)
!----------------------------------------------------------------------------------------
  return
  end subroutine get_m_inv
!========================================================================================
  subroutine get_inv(A,size)
!========================================================================================
! routine computes the inverse of matrix A[size,size]
! routine will only work properly for SQUARE matrices!
! routine calls various LAPACK routines
!----------------------------------------------------------------------------------------
  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer size
  real, dimension(size,size) :: A

! local variables

  real, dimension(size) :: work

  integer, dimension(size,size) :: IPiv
  integer info
!----------------------------------------------------------------------------------------
! set variables

  work=0.0
  IPiv=0
  info=0

! calculate LU decomposition

  call sgetrf(size, size, A, size, IPiv, info)

  if(info.ne.0) then
    write(6,*) 'ERROR from routine sgetrf: info = ', info
    call terminate_run(6,1)
  endif

! calculate inverse

  call sgetri(size, A, size, IPiv, work, size, info)

  if(info.ne.0) then
    write(6,*) 'ERROR from routine sgetrf: info = ', info
    call terminate_run(6,1)
  endif
!----------------------------------------------------------------------------------------
  return
  end subroutine get_inv
!========================================================================================
  subroutine get_M_LU(Jac,tstep,rk_gamma,M,IPiv)
!========================================================================================
! This routine calculate the LU decomposition of the iteration matrix.
!
! Jac (input)       - jacobian matrix at the current node
! tstep (input)     - timestep
! rk_gamma (input)  - the diagonal butcher coefficients
! M (output)        - iteration matrix stored as LU decomposition
! IPiv (output)     - pivot points (must be stored!)
!----------------------------------------------------------------------------------------
  use topology_m, only : myid
  use param_m, only : nvar_tot

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real :: Jac(nvar_tot,nvar_tot), M(nvar_tot,nvar_tot)
  real :: tstep, rk_gamma
  integer :: IPiv(nvar_tot,nvar_tot)

! local variables

  integer i
  integer info
!----------------------------------------------------------------------------------------
! set variables

  IPiv=0
  info=0

! calculate M

  M(:,:) = -(tstep * rk_gamma * Jac(:,:))

  do i=1,nvar_tot
    M(i,i) = 1.0 + M(i,i)
  enddo

! calculate LU decomposition of M

  call sgetrf(nvar_tot, nvar_tot, M, nvar_tot, IPiv, info)

  if(info.ne.0) then
    write(6,*) 'ERROR from routine sgetrf: info = ', info
    call terminate_run(6,1)
  endif
!----------------------------------------------------------------------------------------
  return
  end subroutine get_M_LU
!========================================================================================
  subroutine get_V_node(V,U,temp_in)
!========================================================================================
! Routine calculates the primative variable array V from the conserved variable array U.
! This routine acts on a single node only.
!
! Note that this routine must calculate temperature from the energy using the
! same Newton-Raphson iteration procedure given in routine calc_temp. The initial guess
! for the temperature must be provided.
!
! Note that the inputs U and temp_in and the output V are non-dimensional.
!----------------------------------------------------------------------------------------
  use param_m, only : nvar_tot, nsc
  use reference_m, only : t_ref, univ_gascon, a_ref, cp_ref, l_ref, p_ref, rho_ref
  use topology_m, only : myid
  use chemkin_m, only : molwt, molwt_c, ickwrk, rckwrk, cckwrk
  use thermchem_m, only : Ru
  use thermchem_m, only : mixCp, mixEnth  !routine references

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  real U(nvar_tot)      !conserved variable array at given grid point
  real V(nvar_tot+1)    !primitive variable array at given grid point (includes last species)
  real temp_in          !initial temperature guess for Newton-Raphson iteration

! local declarations

  real avmolwt
  real cpmix
  real entmix
  real inte
  real r_gas
  real volum
  real yspecies(nsc+1)
  real temp

  real abserr
  real deltat

  integer :: icount
  integer, parameter :: icountmax = 100  !max number of iterations
!----------------------------------------------------------------------------------------
! set some values

  temp = temp_in
  abserr = 0.001 / t_ref

  volum = 1.0 / U(4)
  yspecies(1:nsc) = volum * U(6:5+nsc)
  yspecies(nsc+1) = 1.0 - sum(yspecies(1:nsc))
  
  avmolwt = sum( yspecies(:)*molwt_c(:) )
  r_gas = avmolwt * Ru

  V(:)=0.0
!----------------------------------------------------------------------------------------
! subtract chemical enthalpy from internal energy

  inte = volum * ( U(5) - 0.5*volum * ( U(1)*U(1) + U(2)*U(2) + U(3)*U(3) ))
!----------------------------------------------------------------------------------------
! start Newton-Raphson iteration

  icount = 1

  10 continue

! compute mixture heat capacities and enthalpies

  cpmix = mixCp ( yspecies, temp )
  entmix = mixEnth ( yspecies, temp )

! compute change in temperature

  deltat = ( entmix - r_gas * temp - inte ) / (cpmix - r_gas)

! update temperature

  temp = temp - deltat

! check convergence

  if(icount.gt.icountmax) then
    write(6,*) 'routine getFs_nd cannot converge within maximum number of iterations'
    write(6,*) 'for processor with rank =',myid
    stop  !ugly termination but that's the way it is without doing a broadcast
  endif

  if(abs(deltat) .gt. abserr) then
     icount = icount + 1
     go to 10
  endif

! end of Newton-Raphson iteration
!----------------------------------------------------------------------------------------
! set primitive variable array

  V(1)=U(1)/U(4)                      !u-velocity
  V(2)=U(2)/U(4)                      !v-velocity
  V(3)=U(3)/U(4)                      !w-velocity
  V(4)=Ru * U(4) * temp * avmolwt     !pressure
  V(5)=temp                           !temperature
  V(6:nvar_tot+1)=yspecies(1:nsc+1)   !species
!----------------------------------------------------------------------------------------
  return
  end subroutine get_V_node
!=========================================================================================
  subroutine test_jacobians(io)
!=========================================================================================
! tests jacobians
!-----------------------------------------------------------------------------------------
  use topology_m, only : myid
  use rk_m, only : jac_method
  use param_m, only : n_spec, nvar_tot, nx, ny, nz
  use variables_m, only : temp, q, yspecies
  use chemkin_m, only : species_name
  use chemkin_m, only : molwt, molwt_c
  use reference_m

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local delcarations

  real U_in(nvar_tot+1)              !conservative variables array
  real V(nvar_tot+1)                 !primative variables array
  real u(3), p, T, rho, Y(n_spec)    !primative variables in usual notation

  real T_in                          !input temperature

  real dFdV_adi_in(n_spec+2,n_spec)  !inner analytical Jacobian based on primative variables
  real dFdV_num_in(n_spec+2,n_spec)  !inner numerical Jacobian based on primative variables

  real dUdV_ana(nvar_tot,nvar_tot)   !analytical Jacobian conversion matrix
  real dVdU_ana(nvar_tot,nvar_tot)   !analytical inverse of Jacobian conversion matrix
  real dVdU_num(nvar_tot,nvar_tot)   !numerical inverse of Jacobian conversion matrix

  real dUdV(nvar_tot,nvar_tot)       !CAK's analytical Jacobian conversion matrix
  real dVdU(nvar_tot,nvar_tot)       !CAK's analytical inverse of Jacobian conversion matrix

  real dFdV(nvar_tot,nvar_tot)       !general Jacobian based on primative variables

  real dFdU_prim(nvar_tot,nvar_tot)  !Jacobian based on primative variables
  real dFdU_check(nvar_tot,nvar_tot) !checked Jacobian based on primative variables

  integer i, j
!----------------------------------------------------------------------------------------
! error checking on multi-dimensions

  if((nx>1).and.(ny>1).or.(nz>1)) then
    if(myid==0) then
      write(io,*) 'Error! Jacobian testing will not work for multi-dimensions!'
      write(io,*) 'If Jacobian testing is not desired for current problem parameters,'
      write(io,*) '  please check reset of jac_method in ark.in file.'
    endif
    call terminate_run(io,0)
  endif
!----------------------------------------------------------------------------------------
! write header

  call write_header(io,'=')
  write(io,*) 'TESTING JACOBIANS!'
  write(io,*)
!----------------------------------------------------------------------------------------
! write output format warning based on number of species

  if(n_spec > 10) then
    write(io,*) 'WARNING: Number of species is greater than 10!'
    write(io,*) 'Matrix output to screen may look crappy!'
    write(io,*) 'Try writing output to a file!'
    write(io,*) 'Otherwise, modify routine write_matrix to make output nicer!'
    write(io,*)
  endif
!----------------------------------------------------------------------------------------
! zero stuff

  dFdV_adi_in=0.0
  dFdV_num_in=0.0

  dUdV_ana=0.0
  dVdU_ana=0.0
  dVdU_num=0.0

  dUdV=0.0
  dVdU=0.0

  dFdV=0.0

  dFdU_prim=0.0
  dFdU_check=0.0
!----------------------------------------------------------------------------------------
! set U_in and T_in

  U_in(1:nvar_tot)=q(1,1,1,1:nvar_tot,1)
  U_in(nvar_tot+1)=yspecies(1,1,1,n_spec)*q(1,1,1,4,1)
  T_in=temp(1,1,1)
!----------------------------------------------------------------------------------------
! get primitive variables

  call get_V_node(V,U_in,T_in)
!----------------------------------------------------------------------------------------
! set primitive variables (non-dimensional)

  u(1:3)=V(1:3)
  p=V(4)
  T=V(5)
  Y(1:n_spec)=V(6:nvar_tot+1)
  rho=U_in(4)
!-----------------------------------------------------------------------------------------
! write header information

  call write_header(io,'-')
  write(io,*) 'Testing Various Jacobians Based on the Following Initial Conditions:'
  write(io,*)

  write(io,1) 'Temperature       = ',T
  write(io,*)
  write(io,1) 'Pressure          = ',p
  write(io,*)
  write(io,1) 'Density           = ',rho
  write(io,*)
  write(io,1) 'u-Velocity        = ',u(1)
  write(io,1) 'v-Velocity        = ',u(2)
  write(io,1) 'w-Velocity        = ',u(3)
  write(io,*)

  do i=1,n_spec
    write(io,2) 'Y_',species_name(i),'= ', yspecies(1,1,1,i)
  enddo
  write(io,*)

  call write_header(io,'-')

  1 format(1x,a,1pe12.5)
  2 format(1x,a,a,a,1pe12.5)
!----------------------------------------------------------------------------------------
! get dFdV_adi_in

  call get_dFdV_adi(dFdV_adi_in,V)

! write dFdV_adi_in to screen

  write(io,*) 'Analytical dFdV is given by dFdV_adi_in:'
  call write_matrix(dFdV_adi_in,n_spec+2,n_spec,io)
!----------------------------------------------------------------------------------------
! get dFdV_num_in

  call get_dFdV_num(dFdV_num_in,V)

! write dFdV_num_in to screen

  write(io,*) 'Numerical dFdV is given by dFdV_num_in:'
  call write_matrix(dFdV_num_in,n_spec+2,n_spec,io)
!----------------------------------------------------------------------------------------
! compare dFdV_adi_in and dFdV_num_in

  write(io,*) 'Comparison of dFdV_adi_in and dFdV_num_in is as follows:'
  call compare_matrix(dFdV_adi_in,dFdV_num_in,n_spec+2,n_spec,io)
  call write_header(io,'-')
!----------------------------------------------------------------------------------------
! get dUdV_ana

  call get_dUdV_ana(dUdV_ana,U_in,T_in)

! write dUdV_ana to screen

  write(io,*) 'Analytical dUdV is given by dUdV_ana:'
  call write_matrix(dUdV_ana,nvar_tot,nvar_tot,io)
!-----------------------------------------------------------------------------------------
! get dVdU_ana

  call get_dVdU_ana(dVdU_ana,U_in,T_in)

! write dVdU_ana to screen

  write(io,*) 'Analytical dVdU is given by dVdU_ana:'
  call write_matrix(dVdU_ana,nvar_tot,nvar_tot,io)
!----------------------------------------------------------------------------------------
! get dVdU_num

  call get_dVdU_num(dVdU_num,dUdV_ana)

! write dVdU_num to screen

  write(io,*) 'Numerical dVdU is given by dVdU_num:'
  call write_matrix(dVdU_num,nvar_tot,nvar_tot,io)
!----------------------------------------------------------------------------------------
! compare dVdU_ana and dVdU_num

  write(io,*) 'Comparison of dVdU_ana and dVdU_num is as follows:'
  call compare_matrix(dVdU_ana,dVdU_num,nvar_tot,nvar_tot,io)
!----------------------------------------------------------------------------------------
! calculate dFdU_check based on dFdV_adi and dVdU_ana

  if(abs(jac_method)==2) then
    write(io,*) 'Calculating dFdU_check based on dFdV_num and dVdU_ana using matmul...'
    write(io,*)
  else
    write(io,*) 'Calculating dFdU_check based on dFdV_adi and dVdU_ana using matmul...'
    write(io,*)
  endif

  dFdV=0.0

  do i=1,n_spec-1
    do j=1,n_spec+1
      if(abs(jac_method)==2) then
        dFdV(5+i,3+j)=dFdV_num_in(j,i)
      else
        dFdV(5+i,3+j)=dFdV_adi_in(j,i)
      endif
    enddo
  enddo

  if(abs(jac_method)==2) then
    write(io,*) 'dFdV_num is given by:'
    call write_matrix(dFdV_num_in,n_spec+2,n_spec,io)
  else
    write(io,*) 'dFdV_adi is given by:'
    call write_matrix(dFdV_adi_in,n_spec+2,n_spec,io)
  endif

  write(io,*) 'dFdV is given by:'
  call write_matrix(dFdV,nvar_tot,nvar_tot,io)

  write(io,*) 'dVdU_ana is given by:'
  call write_matrix(dVdU_ana,nvar_tot,nvar_tot,io)

  dFdU_check=matmul(dFdV,dVdU_ana)

  write(io,*) 'dFdU_check is given by:'
  call write_matrix(dFdU_check,nvar_tot,nvar_tot,io)
  call write_header(io,'-')
!----------------------------------------------------------------------------------------
! calculate dFdU_prim

  if(abs(jac_method)==2) then
    write(io,*) 'Calculating dFdU_prim based on CAK expression with dFdV_num...'
    write(io,*)
  else
    write(io,*) 'Calculating dFdU_prim based on CAK expression with dFdV_adi...'
    write(io,*)
  endif

  call get_dFdU_prim(dFdU_prim,U_in,T_in)

! write dFdU_prim to screen

  write(io,*) 'dFdU_prim is given by:'
  call write_matrix(dFdU_prim,nvar_tot,nvar_tot,io)
  call write_header(io,'-')
!----------------------------------------------------------------------------------------
! compare dFdU_prim with dFdU_check

  write(io,*) 'Checking dFdU_prim against dFdU_check...'
  write(io,*)

  call compare_matrix(dFdU_prim,dFdU_check,nvar_tot,nvar_tot,io)
!----------------------------------------------------------------------------------------
! write finisher

  call write_header(io,'=')
  write(io,*) 'Stopping here after testing of Jacobians!'
  call terminate_run(io,0)
!----------------------------------------------------------------------------------------
  return
  end
!=========================================================================================
  subroutine get_dUdV_ana(dUdV,U_in,T_in)
!=========================================================================================
! Calculates analytical Jacobian conversion matrix based on input U and T_in.

! Note that input and output arrays are non-dimensional.

! For reference, see "Analytical Jacobian for S3D DNS Code using ADIFOR: Compressible 
!   Reacting Flows" by Christoper A. Kennedy.
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec, nvar_tot
  use variables_m, only : temp, q, yspecies
  use thermchem_m, only : mixCp, specEnth  !routine references
  use chemkin_m, only : molwt, molwt_c
  use reference_m, only : univ_gascon, cp_ref

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real dUdV(nvar_tot,nvar_tot)      !analytical Jacobian conversion matrix
  real U_in(nvar_tot)               !conservative variables array
  real T_in                         !temperature guess required for routine get_V_node

! local declarations

  real V(nvar_tot+1)                !primative variables array
  real u(3), p, T, rho, Y(n_spec)   !primative variables in usual notation
  real cp, cv                       !mixture specific heats
  real avmolwt, r_gas               !mixture molecular weight (inverse) and gas constant
  real lowH(n_spec)                 !species enthalpies
  real capH(n_spec)                 !species enthalpies minus pressure term
  real scrH(n_spec-1)               !species enthalpies (minus pressure term) minus H(n_spec)
  real W                            !mixture molecular weight
  real e0                           !total energy
  real WiN(n_spec-1)                !combination of molecular weights

  integer i, j
!----------------------------------------------------------------------------------------
! get primitive variables from

  call get_V_node(V,U_in,T_in)
!----------------------------------------------------------------------------------------
! set primitive variables in usual notation

  u(1:3)=V(1:3)
  p=V(4)
  T=V(5)
  Y(1:n_spec)=V(6:nvar_tot+1)
  rho=U_in(4)
!-----------------------------------------------------------------------------------------
! compute basic quantities
!----------------------------------------------------------------------------------------
! set mixture specific heat at constant pressure

  cp = mixCp( Y, T )

! set species enthalpies

  do i=1,n_spec
    lowH(i)=specEnth( i, T )
  enddo

! set (inverse of) average molecular weight

  avmolwt = sum( Y(:)*molwt_c(:) )

! set average molecular weight

  W=1.0/avmolwt

! set r_gas

  r_gas = avmolwt * univ_gascon / cp_ref  !univ_gascon is dimensional

! set mixture specific heat at constant volume

  cv=cp-r_gas

! set e0 (units of velocity squared)

  e0=U_in(5)/U_in(4)

! set enthalpies minus pressure term (units of molecular weights cancel)

  capH(:)=lowH(:)-((p*W)/(rho*molwt(:)))

! enthalpies (minus pressure term) minus H(n_spec)

  do i=1,n_spec-1
    scrH(i)=capH(i)-capH(n_spec)
  enddo

! set WiN (units of molecular weights cancel)

  do i=1,n_spec-1
    WiN(i) = W * ( (1.0/molwt(i)) - (1.0/molwt(n_spec)) )
  enddo
!----------------------------------------------------------------------------------------
! fill dUdV matrix

  dUdV=0.0

! block 1

  do i=1,3
    do j=1,3
      if(i==j) dUdV(i,j)=rho
    enddo
  enddo

! block 2

  do j=1,3
    dUdV(5,j)=rho*u(j)
  enddo

! block 3

  do i=1,3
    dUdV(i,4)= rho*u(i)/p
    dUdV(i,5)=-rho*u(i)/T
  enddo

! block 4

  dUdV(4,4)= rho/p
  dUdV(4,5)=-rho/T
  dUdV(5,4)= rho*e0/p
  dUdV(5,5)=(rho*cv)-(rho*e0/T)

! block 5

  do i=1,n_spec-1
    dUdV(5+i,4)= rho*y(i)/p
    dUdV(5+i,5)=-rho*y(i)/T
  enddo

! block 6

  do i=1,3
    do j=1,n_spec-1
      dUdV(i,5+j)=-rho*u(i)*WiN(j)
    enddo
  enddo

! block 7

  do j=1,n_spec-1
    dUdV(4,5+j)=-rho*WiN(j)
    dUdV(5,5+j)= rho*scrH(j)-rho*e0*WiN(j)
  enddo

! block 8

  do i=1,n_spec-1
    do j=1,n_spec-1
      if(i==j) then
        dUdV(5+i,5+j)=rho*(1.0-Y(i)*WiN(j))
      else
        dUdV(5+i,5+j)=-rho*Y(i)*WiN(j)
      endif
    enddo
  enddo
!----------------------------------------------------------------------------------------
  return
  end
!=========================================================================================
  subroutine get_dVdU_ana(dVdU,U_in,T_in)
!=========================================================================================
! Calculates analytical inverse of Jacobian conversion matrix based on input U and T_in.

! Note that input and output arrays are non-dimensional.

! For reference, see "Analytical Jacobian for S3D DNS Code using ADIFOR: Compressible 
!   Reacting Flows" by Christoper A. Kennedy.
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec, nvar_tot
  use variables_m, only : temp, q, yspecies
  use thermchem_m, only : mixCp, specEnth  !routine references
  use chemkin_m, only : molwt, molwt_c
  use reference_m, only : univ_gascon, cp_ref

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real dVdU(nvar_tot,nvar_tot)      !analytical inverse of Jacobian conversion matrix
  real U_in(nvar_tot)               !conservative variables array
  real T_in                         !temperature guess required for routine get_V_node

! local declarations

  real V(nvar_tot+1)                !primative variables array
  real u(3), p, T, rho, Y(n_spec)   !primative variables in usual notation
  real cp, cv                       !mixture specific heats
  real avmolwt, r_gas               !mixture molecular weight (inverse) and gas constant
  real lowH(n_spec)                 !species enthalpies
  real capH(n_spec)                 !species enthalpies minus pressure term
  real scrH(n_spec-1)               !species enthalpies (minus pressure term) minus H(n_spec)
  real W                            !mixture molecular weight
  real e0                           !total energy
  real u_mag                        !velocity magnitude
  real WiN(n_spec-1)                !combination of molecular weights
  real phi
  real zeta
  real b44
  real b54

  integer i, j
!----------------------------------------------------------------------------------------
! get primitive variables from

  call get_V_node(V,U_in,T_in)
!----------------------------------------------------------------------------------------
! set primitive variables in usual notation

  u(1:3)=V(1:3)
  p=V(4)
  T=V(5)
  Y(1:n_spec)=V(6:nvar_tot+1)
  rho=U_in(4)
!-----------------------------------------------------------------------------------------
! compute basic quantities
!----------------------------------------------------------------------------------------
! set mixture specific heat at constant pressure

  cp = mixCp( Y, T )

! set species enthalpies

  do i=1,n_spec
    lowH(i)=specEnth( i, T )
  enddo

! set (inverse of) average molecular weight

  avmolwt = sum( Y(:)*molwt_c(:) )

! set average molecular weight

  W=1.0/avmolwt

! set r_gas

  r_gas = avmolwt * univ_gascon / cp_ref  !univ_gascon is dimensional

! set mixture specific heat at constant volume

  cv=cp-r_gas

! set e0 (units of velocity squared)

  e0=U_in(5)/U_in(4)

! set u_mag

  u_mag = u(1)**2 + u(2)**2 + u(3)**2

! set enthalpies minus pressure term (units of molecular weights cancel)

  capH(:)=lowH(:)-((p*W)/(rho*molwt(:)))

! enthalpies (minus pressure term) minus H(n_spec)

  do i=1,n_spec-1
    scrH(i)=capH(i)-capH(n_spec)
  enddo

! set WiN (units of molecular weights cancel)

  do i=1,n_spec-1
    WiN(i) = W * ( (1.0/molwt(i)) - (1.0/molwt(n_spec)) )
  enddo

! set phi

  phi=0.0
  do i=1,n_spec-1
    phi=phi+Y(i)*WiN(i)
  enddo

! set zeta

  zeta=0.0
  do i=1,n_spec-1
    zeta=zeta+scrH(i)*Y(i)
  enddo

! set b44

  b44=p*(cv*T*(1.0-phi)+zeta+(u_mag-e0))/(rho*cv*T)

! set b54

  b54=(zeta+(u_mag-e0))/(rho*cv)
!----------------------------------------------------------------------------------------
! fill dVdU matrix

  dVdU=0.0

! block 1

  do i=1,3
    do j=1,3
      if(i==j) dVdU(i,j)=1.0/rho
    enddo
  enddo

! block 2

  do j=1,3
    dVdU(4,j)=-p*u(j)/(rho*cv*T)
    dVdU(5,j)=-u(j)/(rho*cv)
  enddo

! block 3

  do i=1,3
    dVdU(i,4)=-u(i)/rho
  enddo

! block 4

  dVdU(4,4)=b44
  dVdU(5,4)=b54
  dVdU(4,5)=p/(rho*cv*T)
  dVdU(5,5)=1.0/(rho*cv)

! block 5

  do i=1,n_spec-1
    dVdU(5+i,4)=-Y(i)/rho
  enddo

! block 6

  do j=1,n_spec-1
    dVdU(4,5+j)=p*(WiN(j)*cv*T-scrH(j))/(rho*cv*T)
    dVdU(5,5+j)=-scrH(j)/(rho*cv)
  enddo

! block 7

  do i=1,n_spec-1
    do j=1,n_spec-1
      if(i==j) dVdU(5+i,5+j)=1.0/rho
    enddo
  enddo  
!----------------------------------------------------------------------------------------
  return
  end
!=========================================================================================
  subroutine get_dVdU_num(dVdU,dUdV)
!=========================================================================================
! Calculates numerical inverse of Jacobian conversion matrix.

! Note that input and output arrays are non-dimensional.

! For reference, see "Analytical Jacobian for S3D DNS Code using ADIFOR: Compressible 
!   Reacting Flows" by Christoper A. Kennedy.
!-----------------------------------------------------------------------------------------
  use param_m, only : n_spec, nvar_tot

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  real dVdU(nvar_tot,nvar_tot)      !inverse of Jacobian conversion matrix
  real dUdV(nvar_tot,nvar_tot)      !Jacobian conversion matrix
!-----------------------------------------------------------------------------------------
! perform numerical inverse

  dVdU=dUdV
  call get_inv(dVdU,nvar_tot)  !get_inv returns inverse in original array location
!-----------------------------------------------------------------------------------------
  return
  end
!========================================================================================
  subroutine compare_matrix(mat1,mat2,nn,mm,io)
!========================================================================================
  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer nn, mm
  real mat1(nn,mm), mat2(nn,mm), comp(nn,mm)
  integer io

! local declarations

  integer n, m
  real small
!----------------------------------------------------------------------------------------
! compare matrices

  small=10.0**(-precision(small))

  do n=1,nn
    do m=1,mm
      if(abs(mat2(n,m)) <= small) then
        comp(n,m)=0.0
      else
        comp(n,m)=mat1(n,m)/mat2(n,m)
      endif
    enddo
  enddo

! write comparison to screen

  call write_matrix(comp,nn,mm,io)
!----------------------------------------------------------------------------------------
  return
  end
!========================================================================================
  subroutine write_matrix(mat,nn,mm,io)
!========================================================================================
  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer nn, mm
  real mat(nn,mm)
  integer io

! local declarations

  integer i, j
!----------------------------------------------------------------------------------------
! write matrix to screen

  write(io,*)
  do i=1,nn
    write(io,1) (mat(i,j), j=1,mm)
  enddo
  1 format(1x,100(1pe10.2,1x))
  write(io,*)
!----------------------------------------------------------------------------------------
  return
  end

#include "globalDefines.h"
!=========================================================================================
  module rk_m
!=========================================================================================
! Module for Additive Runge-Kutta Time Integration

  implicit none
!-----------------------------------------------------------------------------------------
! set integration type

  character(len=3), parameter :: rk_type ='ark'
!-----------------------------------------------------------------------------------------
! input integers

  integer rk_method       !ARK method
  integer cont_switch     !switch for controller
  integer jac_order       !order of accuracy for numerical jacobian
  integer newton_iter_min !minumum number of Newton iterations
  integer newton_iter_max !maximum number of Newton iterations
  integer redo_limit      !limit on number of redos during modified Newton iteration
  integer i_time_cont     !timestep frequency to write controller info to timestep.dat
  integer cfl_switch      !switch for cfl check
  integer jac_fresh       !switch to enforce jacobian be calculated every timestep
!  integer jac_age_max     !maximum timestep age of jacobian with iteration control
  integer jac_method      !switch for jacobian method
  integer max_iter        !maximum number of iterations over all stages and iterations

! other integers

  integer nstage          !number of stages in ARK
  integer rk_q            !order of main method
  integer rk_p            !order of the embedded method
  integer rk_pstar        !order of the dense method
  integer cont_n_reg      !number of registers in q_err array based on type
  integer redo_count      !counter for redo of timesteps
  integer ijkeq_loc(4)    !for locating global nodes
  integer ijkeq_id        !for locating global nodes

! integer arrays

  integer, allocatable :: jac_age(:,:,:)  !age of jacobian
  integer, allocatable :: IPiv(:,:,:,:,:) !pivot points for LU decomposition of Jacobian

! input reals

  real jac_eps        !perturbation for numerical jacobian
  real tstep_init     !initial Runge-Kutta timestep in seconds
  real tstep_min      !miminum timestep in seconds
  real tstep_max      !maximum timestep in seconds
  real cont_factor    !overall maximum change in timestep
  real cont_safety    !factor of safety for controller
  real k_I            !integral gain
  real k_P            !proportional gain
  real k_D            !derivative gain
  real k_D2           !second derivative gain
  real rk_rtol        !RK relative tolerance
  real rk_atol        !RK absolute tolerance
  real rk_itol        !RK iteration tolerance
!  real alpha_opt      !optimum value for convergence rate
!  real alpha_LU_opt   !optimum value for step change convergence rate
!  real alpha_jac_opt  !optimum value for jacobian convergence rate

! other reals

  real cfl_no           !invicid cfl limit
  real fo_no            !viscous cfl limit
  real tstep_iter       !timestep predicted by modified Newton iteration procedure
  real q_err_max        !maximum Runge-Kutta error

! real arrays

  real, allocatable :: rk_time(:)       !exact time at ith-stage => t=t^{n} + rk_time(i)*dt, for ARK: rk_time(i)=c_{i}
  real, allocatable :: rk_ae(:,:)       !RK A-Matrix for the explicit method
  real, allocatable :: rk_ai(:,:)       !RK A-Matrix for the implicit (ESDIRK) method in IMEX
  real, allocatable :: rk_b(:)          !RK b-Vector for both methods in IMEX
  real, allocatable :: rk_bh(:)         !RK embedded b-Vector for both methods in IMEX
  real, allocatable :: rk_err(:)        !(b_i - \hat{b}_i}) to determine error at each grid point
  real, allocatable :: rk_dense(:,:)    !RK b(theta)-Vector for both methods in IMEX
  real, allocatable :: tstep_vec(:)     !vector of timesteps (current and previous)

  real, allocatable :: Fs(:,:,:,:,:)      !stiff contribution to right-hand-side
  real, allocatable :: Fns(:,:,:,:,:)     !non-stiff contribution to right-hand-side
  real, allocatable :: Un(:,:,:,:)        !value of solution vector at time n
  real, allocatable :: q_err(:,:,:,:,:)   !solution vector error

  real, allocatable :: Jac(:,:,:,:,:)     !jacobian matrix
  real, allocatable :: iM(:,:,:,:,:)      !iteration matrix
  real, allocatable :: tstep_LU(:,:,:)    !timestep used to create current value of iM

  real, allocatable :: alphad(:,:,:)      !actual convergence rate which is a running max ove
                                          !overall all stages and newton iterations
! characters

  character*100, allocatable :: iter_status(:,:,:)  !iteration status
  character*4 cont_type                             !controller type (PID only for ARK)
  character*30 tstep_limit                          !for setting limit

! logicals

  logical redo_step                       !logical for redo of timestep
!-----------------------------------------------------------------------------------------
  contains
!=========================================================================================
  subroutine allocate_rk_arrays(flag)
!=========================================================================================
! allocate ARK arrays
!-----------------------------------------------------------------------------------------
  use param_m, only : nx,ny,nz,nvar_tot

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer flag
!-----------------------------------------------------------------------------------------
! ARK arrays

  if(flag.eq.1) then

    allocate(jac_age(nx,ny,nz));                      jac_age=0
    allocate(IPiv(nx,ny,nz,nvar_tot,nvar_tot));       IPiv=0

    allocate(rk_time(nstage));              rk_time=0.0
    allocate(rk_ae(nstage,nstage));         rk_ae=0.0
    allocate(rk_ai(nstage,nstage));         rk_ai=0.0
    allocate(rk_b(nstage));                 rk_b=0.0
    allocate(rk_bh(nstage));                rk_bh=0.0
    allocate(rk_err(nstage));               rk_err=0.0
    allocate(rk_dense(nstage,6));           rk_dense=0.0
    allocate(tstep_vec(max(3,cont_n_reg))); tstep_vec=0.0  !always need minimum of three

    allocate(Fs(nx,ny,nz,nvar_tot,nstage));           Fs=0.0
    allocate(Fns(nx,ny,nz,nvar_tot,nstage));          Fns=0.0
    allocate(Un(nx,ny,nz,nvar_tot));                  Un=0.0
    allocate(q_err(nx,ny,nz,nvar_tot,cont_n_reg));    q_err=0.0

    allocate(Jac(nx,ny,nz,nvar_tot,nvar_tot));        Jac=0.0
    allocate(iM(nx,ny,nz,nvar_tot,nvar_tot));         iM=0.0
    allocate(tstep_LU(nx,ny,nz));                     tstep_LU=0.0

    allocate(alphad(nx,ny,nz));                       alphad=0.0
    allocate(iter_status(nx,ny,nz));                  iter_status=' '

  elseif(flag.eq.-1) then

    deallocate(jac_age)
    deallocate(IPiv)

    deallocate(rk_time)
    deallocate(rk_ae)
    deallocate(rk_ai)
    deallocate(rk_b)
    deallocate(rk_bh)
    deallocate(rk_err)
    deallocate(rk_dense)
    deallocate(tstep_vec)

    deallocate(Fs)
    deallocate(Fns)
    deallocate(Un)
    deallocate(q_err)

    deallocate(Jac)
    deallocate(iM)
    deallocate(tstep_LU)

    deallocate(alphad)
    deallocate(iter_status)

  endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine allocate_rk_arrays
!=========================================================================================
  subroutine initialize_rk(io)
!=========================================================================================
! This routine sets up the addivitve low-storage Runge-Kutta coefficients.
! Tote that he cfl numbers depend on the Runge-Kutta method and the derivative operator.
!
! All additive methods here have the abcissae and final scheme 
! weights equal for the implicit and explicit methods. That is,
! b_{i}^{(E)}=b_{i}^{(I)} and c_{i}^{(E)}=c_{i}^{(I)}. There isn't
! much documentation at this time but dense coefficients may be used to
! guess the value of the integration vector at the next step.
!----------------------------------------------------------------------------------------
  use param_m, only : n_reg, nx_g, ny_g, nz_g
  use topology_m

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  character*100 filename
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    write(io,*) 'initializing ARK module...'
    write(io,*)
  endif
!-----------------------------------------------------------------------------------------
! set file name and inquire

  filename='../input/ark.in'
  call inquire_about_input_file(filename,io)
!-----------------------------------------------------------------------------------------
! read ARK input file

  if(myid.eq.0) then

!   open file

    open(unit=1,file=trim(filename),status='old')

!   read ARK file

    read(1,*) rk_method
    read(1,*) cont_switch
    read(1,*) tstep_init
    read(1,*) tstep_min
    read(1,*) tstep_max
    read(1,*) rk_rtol
    read(1,*) rk_atol
    read(1,*) rk_itol
    read(1,*) cont_safety
    read(1,*) k_I
    read(1,*) k_P
    read(1,*) k_D
    read(1,*) k_D2
    read(1,*) cont_factor
    read(1,*) cfl_switch
    read(1,*) i_time_cont
    read(1,*) newton_iter_min
    read(1,*) newton_iter_max
    read(1,*) redo_limit
!    read(1,*) alpha_opt
!    read(1,*) alpha_LU_opt
!    read(1,*) alpha_jac_opt
    read(1,*) jac_fresh
!    read(1,*) jac_age_max
    read(1,*) jac_method
    read(1,*) jac_order
    read(1,*) jac_eps

!   close file

    close(1)

!   determine controller type and set number of controller registers

    if(cont_switch==0) then

      cont_type='NONE'
      cont_n_reg=1

    elseif((nx_g==1).and.(ny_g==1).and.(nz_g==1)) then

      cont_type='H321'
      cont_n_reg=3

    else

      cont_type='PID2'
      cont_n_reg=4

      if(k_D2==0.0) then
        cont_type='PID'
        cont_n_reg=3
      endif

      if((k_D2==0.0).and.(k_D==0.0)) then
        cont_type='PI'
        cont_n_reg=2
      endif

      if((k_D2==0.0).and.(k_D==0.0).and.(K_P==0.0)) then
        cont_type='I'
        cont_n_reg=1
      endif

      if((k_D2==0.0).and.(k_D==0.0).and.(K_P==0.0).and.(K_I==0.0)) then
        write(io,*) 'Improper setting in controller.in file!'
        write(io,*) 'Controller is on but gains are set to zero.'
        term_status=1
      endif

    endif

  endif

  call check_term_status(io,0)
!----------------------------------------------------------------------------------------
! broadcast ARK variables

  call MPI_Bcast(rk_method      ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(cont_switch    ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(tstep_init     ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(tstep_min      ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(tstep_max      ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(rk_rtol        ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(rk_atol        ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(rk_itol        ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(cont_safety    ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(k_I            ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(k_P            ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(k_D            ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(k_D2           ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(cont_factor    ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(cfl_switch     ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(i_time_cont    ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(newton_iter_min,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(newton_iter_max,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(redo_limit     ,1,MPI_INTEGER  ,0,gcomm,ierr)
!  call MPI_Bcast(alpha_opt      ,1,MPI_REAL8    ,0,gcomm,ierr)
!  call MPI_Bcast(alpha_LU_opt   ,1,MPI_REAL8    ,0,gcomm,ierr)
!  call MPI_Bcast(alpha_jac_opt  ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(jac_fresh      ,1,MPI_INTEGER  ,0,gcomm,ierr)
!  call MPI_Bcast(jac_age_max    ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(jac_method     ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(jac_order      ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(jac_eps        ,1,MPI_REAL8    ,0,gcomm,ierr)

  call MPI_Bcast(cont_n_reg     ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(cont_type      ,4,MPI_CHARACTER,0,gcomm,ierr)
!----------------------------------------------------------------------------------------
! error checking for jac_order input

  if((jac_order < 2).or.(jac_order > 6).or.(mod(jac_order,2) /= 0)) then
    if(myid==0) then
      write(io,*) 'Improper setting for variable jac_order in file ',trim(filename), '.'
    endif
    call terminate_run(io,0)
  endif
!----------------------------------------------------------------------------------------
! write ARK info

  if(myid.eq.0) then

    write(io,*) 'ARK module information is as follows:'
    write(io,*)
    write(io,1) 'rk_method                = ',rk_method
    write(io,1) 'cont_switch              = ',cont_switch
    write(io,2) 'tstep_init (seconds)     = ',tstep_init
    write(io,2) 'tstep_min (seconds)      = ',tstep_min
    write(io,2) 'tstep_max (seconds)      = ',tstep_max
    write(io,2) 'rk_rtol                  = ',rk_rtol
    write(io,2) 'rk_atol                  = ',rk_atol
    write(io,2) 'rk_itol                  = ',rk_itol
    write(io,2) 'cont_safety              = ',cont_safety
    write(io,2) 'k_I                      = ',k_I
    write(io,2) 'k_P                      = ',k_P
    write(io,2) 'k_D                      = ',k_D
    write(io,2) 'k_D2                     = ',k_D2
    write(io,2) 'cont_factor              = ',cont_factor
    write(io,1) 'cfl_switch               = ',cfl_switch
    write(io,1) 'i_time_cont              = ',i_time_cont
    write(io,1) 'newton_iter_min          = ',newton_iter_min
    write(io,1) 'newton_iter_max          = ',newton_iter_max
    write(io,1) 'redo_limit               = ',redo_limit
!    write(io,2) 'alpha_opt                = ',alpha_opt
!    write(io,2) 'alpha_LU_opt             = ',alpha_LU_opt
!    write(io,2) 'alpha_jac_opt            = ',alpha_jac_opt
    write(io,1) 'jac_fresh                = ',jac_fresh
!    write(io,1) 'jac_age_max              = ',jac_age_max
    write(io,1) 'jac_method               = ',jac_method
    write(io,1) 'jac_order                = ',jac_order
    write(io,2) 'jac_eps                  = ',jac_eps

    write(io,*)
    write(io,1) 'cont_n_reg               = ',cont_n_reg
    write(io,3) 'controller type          = ',trim(cont_type)
    write(io,*)

    if(trim(cont_type)=='H321') then
      write(io,'(1x,a)') 'Note: controller input parameters have been over-ridden'
      write(io,'(1x,a)') 'with H321 values since the simulation is Zero-D.'
      write(io,*)
    endif

    1 format(a30,i4)
    2 format(a30,1pe9.2)
    3 format(a30,a4)

  endif
!----------------------------------------------------------------------------------------
! ARK Methods
!----------------------------------------------------------------------------------------
! RK3(2)4L[2]SA

  if(rk_method==431) then

    if(myid.eq.0) then
      write(io,3610)
    endif

    nstage=4
    call allocate_rk_arrays(1)
    cfl_no=1.43
    fo_no=1.22

    rk_q=3
    rk_p=2
    rk_pstar=2

    rk_b(1)   =  1471266399579.0/ 7840856788654.0
    rk_b(2)   = -4482444167858.0/ 7529755066697.0
    rk_b(3)   = 11266239266428.0/11593286722821.0
    rk_b(4)   =  1767732205903.0/ 4055673282236.0

    rk_bh(1)  = +2756255671327.0/12835298489170.0
    rk_bh(2)  =-10771552573575.0/22201958757719.0
    rk_bh(3)  = +9247589265047.0/10645013368117.0
    rk_bh(4)  = +2193209047091.0/ 5459859503100.0

    rk_ae(2,1)=  1767732205903.0/ 2027836641118.0
    rk_ae(3,1)=  5535828885825.0/10492691773637.0
    rk_ae(3,2)=   788022342437.0/10882634858940.0
    rk_ae(4,1)=  6485989280629.0/16251701735622.0
    rk_ae(4,2)= -4246266847089.0/ 9704473918619.0
    rk_ae(4,3)= 10755448449292.0/10357097424841.0

    rk_ai(2,1)=  1767732205903.0/ 4055673282236.0
    rk_ai(2,2)=  1767732205903.0/ 4055673282236.0
    rk_ai(3,1)=  2746238789719.0/10658868560708.0
    rk_ai(3,2)=  -640167445237.0/ 6845629431997.0
    rk_ai(3,3)=  1767732205903.0/ 4055673282236.0
    rk_ai(4,1)=  1471266399579.0/ 7840856788654.0
    rk_ai(4,2)= -4482444167858.0/ 7529755066697.0
    rk_ai(4,3)= 11266239266428.0/11593286722821.0
    rk_ai(4,4)=  1767732205903.0/ 4055673282236.0

    rk_time(1)=+             0.0
    rk_time(2)=+ 1767732205903.0/ 2027836641118.0
    rk_time(3)=+             3.0/             5.0
    rk_time(4)=+             1.0

    rk_err(1)= + rk_b(1) - rk_bh(1)
    rk_err(2)= + rk_b(2) - rk_bh(2)
    rk_err(3)= + rk_b(3) - rk_bh(3)
    rk_err(4)= + rk_b(4) - rk_bh(4)

!   second-order dense output -> b_{i}^{*}=sum_{j=1}^{p*} b_{ij}^{*}\theta^{j}

    rk_dense(1,1)= 4655552711362.0/22874653954995.0
    rk_dense(2,1) =-18682724506714.0/ 9892148508045.0
    rk_dense(3,1)=34259539580243.0/13192909600954.0
    rk_dense(4,1)=  584795268549.0/ 6622622206610.0
    rk_dense(1,2)= -215264564351.0/13552729205753.0
    rk_dense(2,2)=17870216137069.0/13817060693119.0
    rk_dense(3,2) =-28141676662227.0/17317692491321.0
    rk_dense(4,2)= 2508943948391.0/ 7218656332882.0

! RK4(3)6L[2]SA

  elseif(rk_method==641) then

    if(myid.eq.0) then
      write(io,3615)
    endif

    nstage=6
    call allocate_rk_arrays(1)
    cfl_no=2.31
    fo_no=fo_no  * 1.41

    rk_q=4
    rk_p=3
    rk_pstar=2

    rk_b(1) =+            82889.0/        524892.0
    rk_b(2) =+                0.0
    rk_b(3) =+            15625.0/         83664.0
    rk_b(4) =+            69875.0/        102672.0
    rk_b(5) =-             2260.0/          8211.0
    rk_b(6) =+                1.0/             4.0

    rk_bh(1)=+     4586570599.0/   29645900160.0
    rk_bh(2)=               0.0
    rk_bh(3)=+      178811875.0/     945068544.0
    rk_bh(4)=+      814220225.0/    1159782912.0
    rk_bh(5)=-        3700637.0/      11593932.0
    rk_bh(6)=+          61727.0/        225920.0

    rk_ae(2,1)=             1.0/             2.0
    rk_ae(3,1)=         13861.0/         62500.0
    rk_ae(3,2)=          6889.0/         62500.0
    rk_ae(4,1) =-  116923316275.0/ 2393684061468.0
    rk_ae(4,2) =- 2731218467317.0/15368042101831.0
    rk_ae(4,3)= 9408046702089.0/11113171139209.0
    rk_ae(5,1) =-  451086348788.0/ 2902428689909.0
    rk_ae(5,2) =- 2682348792572.0/ 7519795681897.0
    rk_ae(5,3)=12662868775082.0/11960479115383.0
    rk_ae(5,4)= 3355817975965.0/11060851509271.0
    rk_ae(6,1)=  647845179188.0/ 3216320057751.0
    rk_ae(6,2)=   73281519250.0/ 8382639484533.0
    rk_ae(6,3)=  552539513391.0/ 3454668386233.0
    rk_ae(6,4)= 3354512671639.0/ 8306763924573.0
    rk_ae(6,5)=          4040.0/         17871.0

    rk_ai(2,1)=             1.0/4.0
    rk_ai(2,2)=             1.0/4.0
    rk_ai(3,1)=          8611.0/62500.0
    rk_ai(3,2) =-          1743.0/31250.0
    rk_ai(3,3)=             1.0/4.0
    rk_ai(4,1)=       5012029.0/34652500.0
    rk_ai(4,2) =-        654441.0/2922500.0
    rk_ai(4,3)=        174375.0/388108.0
    rk_ai(4,4)=             1.0/4.0
    rk_ai(5,1)=   15267082809.0/155376265600.0
    rk_ai(5,2) =-      71443401.0/120774400.0
    rk_ai(5,3)=     730878875.0/902184768.0
    rk_ai(5,4)=       2285395.0/8070912.0
    rk_ai(5,5)=             1.0/4.0
    rk_ai(6,1)=         82889.0/524892.0
    rk_ai(6,2)=             0.0
    rk_ai(6,3)=         15625.0/83664.0
    rk_ai(6,4)=         69875.0/102672.0
    rk_ai(6,5) =-          2260.0/8211.0
    rk_ai(6,6)=             1.0/4.0

    rk_time(1)=+             0.0
    rk_time(2)=+             1.0/             2.0
    rk_time(3)=+            83.0/           250.0
    rk_time(4)=+            31.0/            50.0
    rk_time(5)=+            17.0/            20.0
    rk_time(6)=+             1.0

    rk_err(1)= + rk_b(1) - rk_bh(1)
    rk_err(2)= + rk_b(2) - rk_bh(2)
    rk_err(3)= + rk_b(3) - rk_bh(3)
    rk_err(4)= + rk_b(4) - rk_bh(4)
    rk_err(5)= + rk_b(5) - rk_bh(5)
    rk_err(6)= + rk_b(6) - rk_bh(6)

!   third-order dense output -> b_{i}^{*}=sum_{j=1}^{p*} b_{ij}^{*}\theta^{j}

    if(rk_pstar==3) then
      rk_dense(1,1)=  6943876665148.0/ 7220017795957.0
      rk_dense(2,1)=              0.0
      rk_dense(3,1)=  7640104374378.0/ 9702883013639.0
      rk_dense(4,1)=-20649996744609.0/ 7521556579894.0
      rk_dense(5,1)=  8854892464581.0/ 2390941311638.0
      rk_dense(6,1) =- 11397109935349.0/ 6675773540249.0
      rk_dense(1,2) =-       54480133.0/      30881146.0
      rk_dense(2,2)=              0.0
      rk_dense(3,2) =-       11436875.0/      14766696.0
      rk_dense(4,2)=      174696575.0/      18121608.0
      rk_dense(5,2) =-       12120380.0/        966161.0
      rk_dense(6,2)=           3843.0/           706.0
      rk_dense(1,3)=  6818779379841.0/ 7100303317025.0
      rk_dense(2,3)=              0.0
      rk_dense(3,3)=  2173542590792.0/12501825683035.0
      rk_dense(4,3) =- 31592104683404.0/ 5083833661969.0
      rk_dense(5,3) =+ 61146701046299.0/ 7138195549469.0
      rk_dense(6,3) =- 17219254887155.0/ 4939391667607.0
    endif

!   second-order dense output -> b_{i}^{*}=sum_{j=1}^{p*} b_{ij}^{*}\theta^{j}

    if(rk_pstar==2) then
      rk_dense(1,1)=  5701579834848.0/ 6164663940925.0
      rk_dense(2,1)=              0.0
      rk_dense(3,1)= 13131138058924.0/17779730471019.0
      rk_dense(4,1) =- 28096677048929.0/11161768239540.0
      rk_dense(5,1)= 42062433452849.0/11720557422164.0
      rk_dense(6,1) =- 25841894007917.0/14894670528776.0
      rk_dense(1,2) =-  7364557999481.0/ 9602213853517.0
      rk_dense(2,2)=              0.0
      rk_dense(3,2) =-  6355522249597.0/11518083130066.0
      rk_dense(4,2)= 29755736407445.0/ 9305094404071.0
      rk_dense(5,2) =- 38886896333129.0/10063858340160.0
      rk_dense(6,2)= 22142945955077.0/11155272088250.0
    endif

! RK5(4)8L[2]SA

  elseif(rk_method==851) then

    if(myid.eq.0) then
      write(io,3620)
    endif

    nstage=8
    call allocate_rk_arrays(1)
    cfl_no=0.49
    fo_no=1.27

    rk_q=5
    rk_p=4
    rk_pstar=3

    rk_b(1) =-      872700587467.0/ 9133579230613.0
    rk_b(2)=                 0.0
    rk_b(3)=                 0.0
    rk_b(4)=    24752842989968.0/10584050607295.0
    rk_b(5) =-     1143369518992.0/ 8141816002931.0
    rk_b(6) =-    13732001328083.0/ 6631934148607.0
    rk_b(7)=    31972909776967.0/41911059379164.0
    rk_b(8)=                41.0/           200.0

    rk_bh(1) =-     499396509142.0/ 5015181072449.0
    rk_bh(2)=                0.0
    rk_bh(3)=                0.0
    rk_bh(4)=   20133279629761.0/ 8363904161634.0
    rk_bh(5) =-    1088358089595.0/ 6795944035243.0
    rk_bh(6) =-   16593423551072.0/ 7738615961777.0
    rk_bh(7)=    3913603334695.0/ 5020235913586.0
    rk_bh(8)=    2913530855887.0/13411993626143.0

    rk_ae(2,1)=             41.0/           100.0
    rk_ae(3,1)=   367902744464.0/ 2072280473677.0
    rk_ae(3,2)=   677623207551.0/ 8224143866563.0
    rk_ae(4,1)=  1268023523408.0/10340822734521.0
    rk_ae(4,2)=              0.0
    rk_ae(4,3)=  1029933939417.0/13636558850479.0
    rk_ae(5,1)= 29921928531207.0/13065330778082.0
    rk_ae(5,2)=              0.0
    rk_ae(5,3)=115140034464727.0/10239288090423.0
    rk_ae(5,4) =- 78522360150645.0/ 6224472171947.0
    rk_ae(6,1)=  4175963610463.0/10363619370981.0
    rk_ae(6,2)=              0.0
    rk_ae(6,3)=  5941611906955.0/ 4388151832759.0
    rk_ae(6,4) =- 14081064728885.0/ 9477725119419.0
    rk_ae(6,5) =-   146841095096.0/ 4698013173029.0
    rk_ae(7,1)= 20649979511554.0/14103843532755.0
    rk_ae(7,2)=              0.0
    rk_ae(7,3)= 46104976391489.0/ 6376485181372.0
    rk_ae(7,4) =- 68205481673867.0/ 8694569480018.0
    rk_ae(7,5) =-         1.0/8.0
    rk_ae(7,6) =-         1.0/8.0
    rk_ae(8,1) =- 22436580330729.0/13396508891632.0
    rk_ae(8,2)=              0.0
    rk_ae(8,3) =- 61519777358797.0/ 9628354034130.0
    rk_ae(8,4)=133952912771311.0/ 9117280366678.0
    rk_ae(8,5)=   819112427236.0/ 8652635578785.0
    rk_ae(8,6) =- 87740800058441.0/12167367327014.0
    rk_ae(8,7)=  9714094484631.0/ 6525933883406.0

    rk_ai(2,1)=             41.0/           200.0
    rk_ai(2,2)=             41.0/           200.0
    rk_ai(3,1)=             41.0/           400.0
    rk_ai(3,2) =-   567603406766.0/11931857230679.0
    rk_ai(3,3)=             41.0/           200.0
    rk_ai(4,1)=   683785636431.0/ 9252920307686.0
    rk_ai(4,2)=              0.0
    rk_ai(4,3) =-   110385047103.0/ 1367015193373.0
    rk_ai(4,4)=             41.0/           200.0
    rk_ai(5,1)=  3435385757185.0/11481209014384.0
    rk_ai(5,2)=              0.0
    rk_ai(5,3)= 11157427131511.0/ 4528506187550.0
    rk_ai(5,4) =-  9556729888537.0/ 4666283647179.0
    rk_ai(5,5)=             41.0/           200.0
    rk_ai(6,1)=   218866479029.0/ 1489978393911.0
    rk_ai(6,2)=              0.0
    rk_ai(6,3)=   638256894668.0/ 5436446318841.0
    rk_ai(6,4) =-  2405872765194.0/10851833147315.0
    rk_ai(6,5) =-    60928119172.0/ 8023461067671.0
    rk_ai(6,6)=             41.0/           200.0
    rk_ai(7,1)=  1020004230633.0/ 5715676835656.0
    rk_ai(7,2)=              0.0
    rk_ai(7,3)= 27712922358947.0/27176279295670.0
    rk_ai(7,4) =-  1316382662167.0/ 5941820308240.0
    rk_ai(7,5) =-   211217309593.0/ 5846859502534.0
    rk_ai(7,6) =-  4710367281160.0/ 8634419175717.0
    rk_ai(7,7)=             41.0/           200.0
    rk_ai(8,1) =-   872700587467.0/ 9133579230613.0
    rk_ai(8,2)=              0.0
    rk_ai(8,3)=              0.0
    rk_ai(8,4)= 24752842989968.0/10584050607295.0
    rk_ai(8,5) =-  1143369518992.0/ 8141816002931.0
    rk_ai(8,6) =- 13732001328083.0/ 6631934148607.0
    rk_ai(8,7)= 31972909776967.0/41911059379164.0
    rk_ai(8,8)=             41.0/           200.0

    rk_time(1)=+             0.0
    rk_time(2)=+            41.0/           100.0
    rk_time(3)=+ 2935347310677.0/11292855782101.0
    rk_time(4)=+ 1426016391358.0/ 7196633302097.0
    rk_time(5)=+            92.0/           100.0
    rk_time(6)=+            24.0/           100.0
    rk_time(7)=+            60.0/           100.0
    rk_time(8)=+             1.0

    rk_err(1)= + rk_b(1) - rk_bh(1)
    rk_err(2)= + rk_b(2) - rk_bh(2)
    rk_err(3)= + rk_b(3) - rk_bh(3)
    rk_err(4)= + rk_b(4) - rk_bh(4)
    rk_err(5)= + rk_b(5) - rk_bh(5)
    rk_err(6)= + rk_b(6) - rk_bh(6)
    rk_err(7)= + rk_b(7) - rk_bh(7)
    rk_err(8)= + rk_b(8) - rk_bh(8)

!   third-order dense output -> b_{i}^{*}=sum_{j=1}^{p*} b_{ij}^{*}\theta^{j}

    if(rk_pstar==3) then
      rk_dense(1,1) =- 17674230611817.0/10670229744614.0
      rk_dense(2,1)=              0.0
      rk_dense(3,1)=              0.0
      rk_dense(4,1) =+ 65168852399939.0/ 7868540260826.0
      rk_dense(5,1) =+ 15494834004392.0/ 5936557850923.0
      rk_dense(6,1) =- 99329723586156.0/26959484932159.0
      rk_dense(7,1) =- 19024464361622.0/ 5461577185407.0
      rk_dense(8,1) =-  6511271360970.0/ 6095937251113.0

      rk_dense(1,2) =+ 43486358583215.0/12773830924787.0
      rk_dense(2,2)=              0.0
      rk_dense(3,2)=              0.0
      rk_dense(4,2) =- 91478233927265.0/11067650958493.0
      rk_dense(5,2) =- 79368583304911.0/10890268929626.0
      rk_dense(6,2) =- 12239297817655.0/ 9152339842473.0
      rk_dense(7,2) =+115839755401235.0/10719374521269.0
      rk_dense(8,2) =+  5843115559534.0/ 2180450260947.0

      rk_dense(1,3) =-  9257016797708.0/ 5021505065439.0
      rk_dense(2,3)=              0.0
      rk_dense(3,3)=              0.0
      rk_dense(4,3) =+ 26096422576131.0/11239449250142.0
      rk_dense(5,3) =+ 92396832856987.0/20362823103730.0
      rk_dense(6,3) =+ 30029262896817.0/10175596800299.0
      rk_dense(7,3) =- 26136350496073.0/ 3983972220547.0
      rk_dense(8,3) =-  5289405421727.0/ 3760307252460.0
    endif

  endif
!----------------------------------------------------------------------------------------
! check to make sure proper initialization occured

  if(.not.allocated(rk_ae)) then  !rk_ae used as a simple test
    if(myid==0) then
      write(io,*) 'improper setting of ARK method in file ark.in...'
      write(io,'(1x,a,i4)') 'rk_method=',rk_method
    endif
    call terminate_run(io,0)
  endif
!----------------------------------------------------------------------------------------
! set misc stuff (must be done after allocation of ark arrays)

  n_reg=1               !number of registers for solution vector q is one
  jac_age(:,:,:)=-1     !set so that Jacobian is calculated right away
  tstep_lu(:,:,:)=-1.0  !set so that iM is calculated right away

  Jac=0.0               !zero jacobian
  IPiv=0                !zero pivot points

  tstep_limit='initial timestep'
!----------------------------------------------------------------------------------------
! write header

  if(myid.eq.0) then
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------------------------
! format statements

  3610 format(3x,'using RK3(2)4L[2]SA additive Runge-Kutta scheme!')
  3615 format(3x,'using RK4(3)6L[2]SA additive Runge-Kutta scheme!')
  3620 format(3x,'using RK5(4)8L[2]SA additive Runge-Kutta scheme!')
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_rk
!-----------------------------------------------------------------------------------------
  end module rk_m

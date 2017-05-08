#include "globalDefines.h"
!=========================================================================================
  module rk_m
!=========================================================================================
! Module for Explicit Runge-Kutta Time Integration
!
! BUG FIX
! Evatt Hawkes 11-AUG-04
! Divide by zero could occur in timestep calculation in rare cases.
! added parameter vsmall to avoid this.

  implicit none
!-----------------------------------------------------------------------------------------
! set integration type

  character(len=3), parameter :: rk_type ='erk'
!-----------------------------------------------------------------------------------------
! input integers

  integer rk_method       !ERK method (input)
  integer cont_switch     !switch for controller (input)
  integer cfl_switch      !switch for cfl check (input)
  integer i_time_cont     !timestep freqency to write controller info to ts.dat (input)

! other integers

  integer nstage          !number of stages
  integer cont_n_reg      !number of registers in q_err array based on type

  integer integrate_erk_johnmc ! flag to call john mellor crummy's changes

! input reals

  real rk_rtol            !RK relative tolerance (input)
  real rk_atol            !RK absolute tolerance (input)
  real k_I                !integral gain (input)
  real k_P                !proportional gain (input)
  real k_D                !derivative gain (input)
  real k_D2               !second derivative gain (input)
  real cont_safety        !factor of safety for controller (input)
  real tstep_init         !initial time step in seconds (input)
  real tstep_min          !minimum time step in seconds (input)
  real tstep_max          !maximum time step in seconds (input)
  real cont_factor        !overall maximum increase in timestep (input)

! other reals

  real rk_p               !order of embedded method
  real cfl_no             !invicid cfl limit
  real fo_no              !viscous cfl limit
  real q_err_max          !maximum Runge-Kutta error
  real rk_tol             !overall RK tolerance (based on rk_atol and rk_rtol)

! real arrays

  real, allocatable :: rk_alpha(:)          !rk integeration coefficients
  real, allocatable :: rk_beta(:)           !rk integeration coefficients
  real, allocatable :: rk_gamma(:)          !rk integeration coefficients
  real, allocatable :: rk_delta(:)          !rk integeration coefficients
  real, allocatable :: rk_eps(:)            !rk integeration coefficients
  real, allocatable :: rk_time(:)           !rk sub-stage time
  real, allocatable :: rk_err(:)            !rk error coefficients
  real, allocatable :: q_err(:,:,:,:,:)     !solution vector error
  real, allocatable :: tstep_vec(:)         !vector of timesteps (current and previous)

! characters

  character*4 cont_type     !controller type: I, PI, PID, PID2, etc...
!-----------------------------------------------------------------------------------------
  contains
!=========================================================================================
  subroutine allocate_rk_arrays(flag)
!=========================================================================================
! allocate ERK arrays
!-----------------------------------------------------------------------------------------
  use param_m, only : nx,ny,nz,nvar_tot

  implicit none
!-----------------------------------------------------------------------------------------
! declarations passed in

  integer flag
!-----------------------------------------------------------------------------------------
! ERK arrays

  if(flag==1) then

    allocate(rk_alpha(nstage));    rk_alpha=0.0
    allocate(rk_beta(nstage));     rk_beta=0.0
    allocate(rk_gamma(nstage));    rk_gamma=0.0
    allocate(rk_delta(nstage));    rk_delta=0.0
    allocate(rk_eps(nstage));      rk_eps=0.0
    allocate(rk_time(nstage));     rk_time=0.0
    allocate(rk_err(nstage));      rk_err=0.0

    allocate(q_err(nx,ny,nz,nvar_tot,cont_n_reg)); q_err=0.0
    allocate(tstep_vec(4)); tstep_vec=0.0

  elseif(flag==-1) then

    deallocate(rk_alpha)
    deallocate(rk_beta)
    deallocate(rk_gamma)
    deallocate(rk_delta)
    deallocate(rk_eps)
    deallocate(rk_time)
    deallocate(rk_err)

    deallocate(q_err)
    deallocate(tstep_vec)

  endif
!-----------------------------------------------------------------------------------------
  return
  end subroutine allocate_rk_arrays
!=========================================================================================
  subroutine initialize_rk(io)
!=========================================================================================
! this routine sets up the explicit low-storage Runge-Kutta coefficients
! note that he cfl numbers depend on the Runge-Kutta method and the derivative operator
!
! EXPLICIT RUNGE-KUTTA METHODS
!
! 2R schemes -
! alpha(i) = {   a_{21},     a_{32},  ..      a_{n,n-1}, b_{n}}
! beta(i)  = {(b_{1}-a_{21}), (b_{2}-a_{32}), .. (b_{n-1}-a_{n,n-1}), 0}
! time(i)  = {   c_{2 },     c_{3 },  ..      c_{n    },  1}
! error(i) = {b_i - \hat{b}_i}
!
! 3R schemes -
! alpha(i) = {   a_{21},     a_{32},  ..    a_{n,n-1}, b_{n}}
! beta(i)  = {(b_{1}-a_{21}), (b_{2}-a_{32}), .. (b_{n-1}-a_{n,n-1}), 0}
! gamma(i) = {(b_{1}-a_{31}), (b_{2}-a_{42}), .. (b_{n-2}-a_{n,n-2}), 0, 0}
! time(i)  = {   c_{2 },     c_{3 },  ..    c_{n    },  1   }
! error(i) = {b_i - \hat{b}_i}
!
! 4R schemes -
! alpha(i) = {   a_{21},     a_{32},  ..    a_{n,n-1}, b_{n}}
! beta(i)  = {(b_{1}-a_{21}), (b_{2}-a_{32}), .. (b_{n-1}-a_{n,n-1}), 0}
! gamma(i) = {(b_{1}-a_{31}), (b_{2}-a_{42}), .. (b_{n-2}-a_{n,n-2}), 0, 0}
! delta(i) = {(b_{1}-a_{41}), (b_{2}-a_{52}), .. (b_{n-3}-a_{n,n-3}), 0, 0, 0}
! time(i)  = {   c_{2 },     c_{3 },  ..    c_{n    },  1   }
! error(i) = {b_i - \hat{b}_i}
!
! 5R schemes -
! alpha(i) = {   a_{21},     a_{32},  ..    a_{n,n-1}, b_{n}}
! beta(i)  = {(b_{1}-a_{21}), (b_{2}-a_{32}), .. (b_{n-1}-a_{n,n-1}), 0}
! gamma(i) = {(b_{1}-a_{31}), (b_{2}-a_{42}), .. (b_{n-2}-a_{n,n-2}), 0, 0}
! delta(i) = {(b_{1}-a_{41}), (b_{2}-a_{52}), .. (b_{n-3}-a_{n,n-3}), 0, 0, 0}
! eps(i)   = {(b_{1}-a_{51}), (b_{2}-a_{62}), .. (b_{n-4}-a_{n,n-4}), 0, 0, 0, 0}
! time(i)  = {   c_{2 },     c_{3 },  ..    c_{n    },  1   }
! error(i) = {b_i - \hat{b}_i}
!
! For references, see the books by John Dormand (1996) or Ernst Hairer
! Vol. I (1993)& II (1996).
!----------------------------------------------------------------------------------------
  use param_m, only : n_reg
  use topology_m

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io

! local declarations

  character*100 filename
!----------------------------------------------------------------------------------------
! write header

  if(myid==0) then
    write(io,*) 'initializing ERK module...'
    write(io,*)
  endif
!-----------------------------------------------------------------------------------------
! set file name and inquire

  filename='../input/erk.in'
  call inquire_about_input_file(filename,io)
!-----------------------------------------------------------------------------------------
! read ERK input file

  if(myid==0) then

!   open file

    open(unit=1,file=trim(filename),status='old')

!   read ERK file

    read(1,*) rk_method
    read(1,*) cont_switch
    read(1,*) tstep_init
    read(1,*) tstep_min
    read(1,*) tstep_max
    read(1,*) rk_rtol
    read(1,*) rk_atol
    read(1,*) cont_safety
    read(1,*) k_I
    read(1,*) k_P
    read(1,*) k_D
    read(1,*) k_D2
    read(1,*) cont_factor
    read(1,*) cfl_switch
    read(1,*) i_time_cont
    read(1,*) integrate_erk_johnmc ! value = 0 for usual, other for johnmc stuff

!   close file

    close(1)

!   determine controller type and set number of controller registers

    if(cont_switch==0) then

      cont_type='NONE'
      cont_n_reg=1

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
! broadcast ERK variables

  call MPI_Bcast(rk_method      ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(cont_switch    ,3,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(tstep_init     ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(tstep_min      ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(tstep_max      ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(rk_rtol        ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(rk_atol        ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(cont_safety    ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(k_I            ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(k_P            ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(k_D            ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(k_D2           ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(cont_factor    ,1,MPI_REAL8    ,0,gcomm,ierr)
  call MPI_Bcast(cfl_switch     ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(i_time_cont    ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(integrate_erk_johnmc    ,1,MPI_INTEGER  ,0,gcomm,ierr)

  call MPI_Bcast(cont_n_reg     ,1,MPI_INTEGER  ,0,gcomm,ierr)
  call MPI_Bcast(cont_type      ,4,MPI_CHARACTER,0,gcomm,ierr)
!-----------------------------------------------------------------------------------------
! set rk_tol

  rk_tol=rk_atol/rk_rtol
!-----------------------------------------------------------------------------------------
! write ERK info

  if(myid==0) then

    write(io,*) 'ERK module information is as follows:'
    write(io,*)
    write(io,2) 'rk_method                = ',rk_method
    write(io,2) 'cont_switch              = ',cont_switch
    write(io,1) 'tstep_init (seconds)     = ',tstep_init
    write(io,1) 'tstep_min (seconds)      = ',tstep_min
    write(io,1) 'tstep_max (seconds)      = ',tstep_max
    write(io,1) 'rk_rtol                  = ',rk_rtol
    write(io,1) 'rk_atol                  = ',rk_atol
    write(io,1) 'cont_safety              = ',cont_safety
    write(io,1) 'k_I                      = ',k_I
    write(io,1) 'k_P                      = ',k_P
    write(io,1) 'k_D                      = ',k_D
    write(io,1) 'k_D2                     = ',k_D2
    write(io,1) 'cont_factor              = ',cont_factor
    write(io,2) 'cfl_switch               = ',cfl_switch
    write(io,2) 'i_time_cont              = ',i_time_cont
    write(io,*)
    write(io,2) 'cont_n_reg               = ',cont_n_reg
    write(io,3) 'controller type          = ',trim(cont_type)
    write(io,*)
    write(io,1) 'rk_tol                   = ',rk_tol
    write(io,*)

    1 format(a30,1pe9.2)
    2 format(a30,i4)
    3 format(a30,a4)

  endif
!----------------------------------------------------------------------------------------
! hard code number of registers (must be consistent with routine tstep!)
! the offset between n_reg set here and the "X-Register Methods" below is for the extra
! register in the current version of routine tstep

  n_reg = 3  !hard-coded for two-register methods
             !routine tstep must be rewritten to use other higher register methods
!----------------------------------------------------------------------------------------
! Two-Register ERK Methods
!----------------------------------------------------------------------------------------
  if(n_reg==3) then

!   RK3(2)3[2R+]Wray

    if(rk_method==-3321) then

      if(myid==0) then
        write(io,3205)
      endif

      nstage = 3
      call allocate_rk_arrays(1)
      cfl_no = 1.00
      fo_no = 0.83

      rk_alpha(1) = +  8.0d0/15.0d0
      rk_alpha(2) = +  5.0d0/12.0d0
      rk_alpha(3) = +  3.0d0/ 4.0d0

      rk_beta(1)  = - 17.0d0/60.0d0
      rk_beta(2)  = -  5.0d0/12.0d0
      rk_beta(3)  = +  0.0d0

      rk_time(1)  = +  8.0d0/15.0d0
      rk_time(2)  = +  2.0d0/ 3.0d0
      rk_time(3)  = +  1.0d0

      rk_err(1) = -  3.0d0/32.0d0
      rk_err(2) = + 15.0d0/32.0d0
      rk_err(3) = -  3.0d0/ 8.0d0

      rk_p  = +  2.0d0

!   RK3(2)4[2R+]C

    elseif(rk_method==-4321) then

      if(myid==0) then
        write(io,3210)
      endif

      nstage = 4
      call allocate_rk_arrays(1)
      cfl_no = 1.63
      fo_no = 0.92

      rk_alpha(1) = + 11847461282814.d0/36547543011857.d0
      rk_alpha(2) = +  3943225443063.d0/ 7078155732230.d0
      rk_alpha(3) = -   346793006927.d0/ 4029903576067.d0
      rk_alpha(4) = -101169746363290.d0/37734290219643.d0

      rk_beta(1)  = -  2303250428699.d0/10465235380037.d0
      rk_beta(2)  = +   317346580501.d0/ 7077109182695.d0
      rk_beta(3)  = + 97305193263306.d0/31787189150627.d0
      rk_beta(4)  = + 0.0d0

      rk_time(1)  = + 11847461282814.d0/36547543011857.d0
      rk_time(2)  = +  5979661281493.d0/ 9043954410414.d0
      rk_time(3)  = +  8918300161085.d0/14385188236547.d0
      rk_time(4)  = + 1.0d0

      rk_err(1) = + 15763415370699.d0/46270243929542.d0
      rk_err(2) = +   514528521746.d0/ 5659431552419.d0
      rk_err(3) = + 27030193851939.d0/ 9429696342944.d0
      rk_err(4) = - 69544964788955.d0/30262026368149.d0

      rk_p  = +  2.0d0

!   RK4(3)5[2R+]C

    elseif(rk_method==-54) then

      if(myid==0) then
        write(io,3215)
      endif

      nstage = 5
      call allocate_rk_arrays(1)
      cfl_no = 1.92
      fo_no = 1.60

      rk_alpha(1) = +   970286171893.d0/ 4311952581923.d0
      rk_alpha(2) = +  6584761158862.d0/12103376702013.d0
      rk_alpha(3) = +  2251764453980.d0/15575788980749.d0
      rk_alpha(4) = + 26877169314380.d0/34165994151039.d0
      rk_alpha(5) = +  5198255086312.d0/14908931495163.d0

      rk_beta(1)  = -   226579859958.d0/ 1303732956329.d0
      rk_beta(2)  = -  2989355243141.d0/18329652166169.d0
      rk_beta(3)  = -  3205125082718.d0/ 6188445870263.d0
      rk_beta(4)  = -  7817808822974.d0/40264141576885.d0
      rk_beta(5)  = +      0.d0

      rk_time(1)  = +   970286171893.d0/ 4311952581923.d0
      rk_time(2)  = +  4943174557973.d0/ 8304051614810.d0
      rk_time(3)  = +  7683171927423.d0/13321439579606.d0
      rk_time(4)  = +  6669838699123.d0/ 7888670863231.d0
      rk_time(5)  = +      1.d0

!     1/28

      rk_err(1) = -  1191458721140.d0/13856102094301.d0
      rk_err(2) = +  1211599465186.d0/ 6408068052369.d0
      rk_err(3) = -  1927722332077.d0/13373413071744.d0
      rk_err(4) = -  1523838422606.d0/47929407649563.d0
      rk_err(5) = +   544787529623.d0/ 7477878650730.d0

!     1/27

!      rk_err(1) = -1332271349864.d0/19920454004517.d0
!      rk_err(2) = +1525481604991.d0/10373360877176.d0
!      rk_err(3) = -400345210123.d0/3570893409478.d0
!      rk_err(4) = -111542107832.d0/4510726118011.d0
!      rk_err(5) = +568365972445.d0/10030527987096.d0

!     1/29
!
!      rk_err(1) = -2376824238107.d0/22902827142018.d0
!      rk_err(2) = +2640697896322.d0/11572220901535.d0
!      rk_err(3) = -1996382130517.d0/11475494468259.d0
!      rk_err(4) = -215780753069.d0/5623488350716.d0
!      rk_err(5) = +870243609787.d0/9897421180319.d0

!     1/30

!      rk_err(1) = -917191527655.d0/7618931567861.d0
!      rk_err(2) = +5048963513656.d0/19074020227627.d0
!      rk_err(3) = -2504312317931.d0/12409612900253.d0
!      rk_err(4) = -532878766337.d0/11971912151329.d0
!      rk_err(5) = +1395226978480.d0/13679432633277.d0

      rk_p  = +  3.0d0

!   RK4(3)6[2R+]C

    elseif(rk_method==-64) then

      if(myid==0) then
        write(io,3216)
      endif

      nstage = 6
      call allocate_rk_arrays(1)
      cfl_no = 2.26
      fo_no = 1.56

      rk_alpha(1) = +  3296351145737.d0/15110423921029.d0
      rk_alpha(2) = +  1879360555526.d0/ 7321162733569.d0
      rk_alpha(3) = + 10797097731880.d0/20472212111779.d0
      rk_alpha(4) = +   754636544611.d0/15563872110659.d0
      rk_alpha(5) = +  3260218886217.d0/ 2618290685819.d0
      rk_alpha(6) = +  5069185909380.d0/12292927838509.d0

      rk_beta(1)  = -  1204558336989.d0/10607789004752.d0
      rk_beta(2)  = -  3028468927040.d0/14078136890693.d0
      rk_beta(3)  = -   455570672869.d0/ 8930094212428.d0
      rk_beta(4)  = - 17275898420483.d0/15997285579755.d0
      rk_beta(5)  = -  2453906524165.d0/ 9868353053862.d0
      rk_beta(6)  = +      0.d0

      rk_time(1)  = +  3296351145737.d0/15110423921029.d0
      rk_time(2)  = +  2703592154963.d0/ 7482974295227.d0
      rk_time(3)  = +  7876405563010.d0/11693293791847.d0
      rk_time(4)  = + 12920213460229.d0/19253602032679.d0
      rk_time(5)  = +  7527523127717.d0/ 9001003553970.d0
      rk_time(6)  = +      1.d0

      rk_err(1) = -   530312978447.d0/ 9560368366154.d0
      rk_err(2) = +   473021958881.d0/ 2984707536468.d0
      rk_err(3) = -   947229622805.d0/10456009803779.d0
      rk_err(4) = -  2921473878215.d0/13334914072261.d0
      rk_err(5) = +  1519535112975.d0/ 9264196100452.d0
      rk_err(6) = +   167623581683.d0/ 3930932046784.d0

      rk_p  = +  3.0d0

!   RK5(4)9[2R+]S

    elseif(rk_method==-9521) then

      if(myid==0) then
        write(io,3220)
      endif

      nstage = 9
      call allocate_rk_arrays(1)
      cfl_no = 2.05
      fo_no = 2.10

      rk_alpha(1) = +  1107026461565.d0/ 5417078080134.d0
      rk_alpha(2) = + 38141181049399.d0/41724347789894.d0
      rk_alpha(3) = +   493273079041.d0/11940823631197.d0
      rk_alpha(4) = +  1851571280403.d0/ 6147804934346.d0
      rk_alpha(5) = + 11782306865191.d0/62590030070788.d0
      rk_alpha(6) = +  9452544825720.d0/13648368537481.d0
      rk_alpha(7) = +  4435885630781.d0/26285702406235.d0
      rk_alpha(8) = +  2357909744247.d0/11371140753790.d0
      rk_alpha(9) = +  3559252274877.d0/14424734981077.d0

      rk_beta(1)  = -  2268038920007.d0/20996276275396.d0
      rk_beta(2)  = - 13136732076355.d0/15308534194223.d0
      rk_beta(3)  = -   156070395457.d0/ 2427054173901.d0
      rk_beta(4)  = -  3265419331187.d0/12298426216220.d0
      rk_beta(5)  = -  2915739048707.d0/52254452998976.d0
      rk_beta(6)  = -  3185215656631.d0/ 8025929113381.d0
      rk_beta(7)  = -  1676339470221.d0/14156447850860.d0
      rk_beta(8)  = -  1143259659130.d0/11713683742071.d0
      rk_beta(9)  = +      0.d0

      rk_time(1)  = +  1107026461565.d0/ 5417078080134.d0
      rk_time(2)  = + 13233244757963.d0/13096251827034.d0
      rk_time(3)  = +  1281770952424.d0/ 6619384620391.d0
      rk_time(4)  = +  5154574557240.d0/11973167139457.d0
      rk_time(5)  = +  3443239632916.d0/ 9747563455579.d0
      rk_time(6)  = +  9947313773185.d0/10047597844976.d0
      rk_time(7)  = +  1998429378427.d0/ 2622920408377.d0
      rk_time(8)  = +  3590723662182.d0/ 4220141307065.d0
      rk_time(9)  = +      1.d0

      rk_err(1) = +    99566782291.d0/11637153525754.d0
      rk_err(2) = +   389707689077.d0/ 8754385404850.d0
      rk_err(3) = +   361936894535.d0/ 7610702813259.d0
      rk_err(4) = -   707617653631.d0/ 7387971018549.d0
      rk_err(5) = -  1413672996307.d0/15652038355037.d0
      rk_err(6) = +  1053426801028.d0/ 7809369790737.d0
      rk_err(7) = -   503679177533.d0/ 6251423380543.d0
      rk_err(8) = -  1032012568056.d0/15576518352263.d0
      rk_err(9) = +   924843045574.d0/ 9495060110467.d0

      rk_p  = +  4.0d0

    elseif(rk_method==-9522) then

!   RK5(4)9[2R+]M

      if(myid==0) then
        write(io,3225)
      endif

      nstage = 9
      call allocate_rk_arrays(1)
      cfl_no = 0.36
      fo_no = 1.36

      rk_alpha(1) = +  5573095071601.d0/11304125995793.d0
      rk_alpha(2) = +   315581365608.d0/ 4729744040249.d0
      rk_alpha(3) = +  8734064225157.d0/30508564569118.d0
      rk_alpha(4) = +  6457785058448.d0/14982850401353.d0
      rk_alpha(5) = +  5771559441664.d0/18187997215013.d0
      rk_alpha(6) = +  1906712129266.d0/ 6681214991155.d0
      rk_alpha(7) = +   311585568784.d0/ 2369973437185.d0
      rk_alpha(8) = +  4840285693886.d0/ 7758383361725.d0
      rk_alpha(9) = -   453873186647.d0/15285235680030.d0

      rk_beta(1)  = -  5041145316973.d0/12607658602113.d0
      rk_beta(2)  = -  3416992132334.d0/27316396222087.d0
      rk_beta(3)  = -   435806063293.d0/ 2762362355678.d0
      rk_beta(4)  = -  8003101162556.d0/30869555300813.d0
      rk_beta(5)  = -   577113638669.d0/ 6295949439516.d0
      rk_beta(6)  = -  3865069063204.d0/ 7410682751831.d0
      rk_beta(7)  = +  2469738089713.d0/ 9348974032345.d0
      rk_beta(8)  = + 31915626435981.d0/34194308054699.d0
      rk_beta(9)  = +      0.d0

      rk_time(1)  = +  5573095071601.d0/11304125995793.d0
      rk_time(2)  = +   356804355997.d0/ 2231574073113.d0
      rk_time(3)  = +  4298735119876.d0/13388276196161.d0
      rk_time(4)  = +  5236032723157.d0/ 8810003383318.d0
      rk_time(5)  = +  4153588703091.d0/ 6366623345744.d0
      rk_time(6)  = +  4993677543181.d0/ 5901852643020.d0
      rk_time(7)  = +  3719174227912.d0/ 8155389791149.d0
      rk_time(8)  = +   683341380240.d0/ 7093550143793.d0
      rk_time(9)  = +      1.d0
!
      rk_err(1) = +   713258517865.d0/11914270380646.d0
      rk_err(2) = +   108633274751.d0/ 5445970073775.d0
      rk_err(3) = -   580539457195.d0/11647571599654.d0
      rk_err(4) = -  6093198366837.d0/58087407556006.d0
      rk_err(5) = +    49125768382.d0/ 1867638027775.d0
      rk_err(6) = -  3357964085291.d0/13929948671170.d0
      rk_err(7) = +   771037523229.d0/12910231793759.d0
      rk_err(8) = +  1248377557213.d0/ 4456395929614.d0
      rk_err(9) = -   569571343944.d0/11352430566113.d0

      rk_p  = +  5.0d0

    elseif(rk_method==-9523) then

!   RK5(4)9[2R+]C

      if(myid==0) then
        write(io,3230)
      endif

      nstage = 9
      call allocate_rk_arrays(1)
      cfl_no = 1.21
      fo_no = 1.71

      rk_alpha(1) = +  2756167973529.d0/16886029417639.d0
      rk_alpha(2) = + 11436141375279.d0/13592993952163.d0
      rk_alpha(3) = +    88551658327.d0/ 2352971381260.d0
      rk_alpha(4) = +  1882111988787.d0/ 5590444193957.d0
      rk_alpha(5) = +   846820081679.d0/ 4754706910573.d0
      rk_alpha(6) = +  4475289710031.d0/ 6420120086209.d0
      rk_alpha(7) = +  1183947748311.d0/ 9144450320350.d0
      rk_alpha(8) = +  3307377157135.d0/13111544596386.d0
      rk_alpha(9) = +  2993490409874.d0/13266828321767.d0

      rk_beta(1)  = -  1278932538812.d0/14237560082479.d0
      rk_beta(2)  = -  9772267818305.d0/13770283459582.d0
      rk_beta(3)  = -   649448945036.d0/10126291883821.d0
      rk_beta(4)  = -  1485455737779.d0/ 3761834053396.d0
      rk_beta(5)  = +   195254452520.d0/ 9057935342371.d0
      rk_beta(6)  = -  4408934034841.d0/10740962926209.d0
      rk_beta(7)  = -   520136817751.d0/ 4844714249296.d0
      rk_beta(8)  = -   610927249771.d0/ 5731341066046.d0
      rk_beta(9)  = +      0.d0

      rk_time(1)  = +  2756167973529.d0/16886029417639.d0
      rk_time(2)  = + 21181016277930.d0/23155740623447.d0
      rk_time(3)  = +  2014102163047.d0/ 8299062198195.d0
      rk_time(4)  = +  4677437980981.d0/ 9078502362823.d0
      rk_time(5)  = +  3445012446537.d0/11543116441576.d0
      rk_time(6)  = +  6828926337941.d0/ 6714272701698.d0
      rk_time(7)  = +  7952179383653.d0/10803573434521.d0
      rk_time(8)  = +  3700956316273.d0/ 4201064057795.d0
      rk_time(9)  = +      1.d0

      rk_err(1) = -   163736145169.d0/13483741564702.d0
      rk_err(2) = +  2488380810692.d0/54277790331749.d0
      rk_err(3) = +   898455961003.d0/20501870256833.d0
      rk_err(4) = -  1317212137564.d0/ 9740967048653.d0
      rk_err(5) = -   240460950729.d0/ 3888321524314.d0
      rk_err(6) = +  5763560669950.d0/38577266971999.d0
      rk_err(7) = -   284677272749.d0/ 6232640538315.d0
      rk_err(8) = -   430383007763.d0/12355804311905.d0
      rk_err(9) = +   572177016298.d0/11297738781159.d0

      rk_p  = +  5.0d0

    endif
  endif
!----------------------------------------------------------------------------------------
! Three-Register ERK Methods
!----------------------------------------------------------------------------------------
  if(n_reg==4) then

!   RK4(3)5[3R+]M

    if(rk_method==-5431) then

      if(myid==0) then
        write(io,3305)
      endif

      nstage = 5
      call allocate_rk_arrays(1)
      cfl_no = 0.25
      fo_no = 1.07

      rk_alpha(1) = + 17396840518954.0d0/49788467287365.0d0
      rk_alpha(2) = + 21253110367599.0d0/14558944785238.0d0
      rk_alpha(3) = +  4293647616769.0d0/14519312872408.0d0
      rk_alpha(4) = -  8941886866937.0d0/ 7464816931160.0d0
      rk_alpha(5) = -   436008689643.0d0/ 9453681332953.0d0

      rk_beta(1)  = -   543821635660.0d0/ 2286984895763.0d0
      rk_beta(2)  = - 16420716932703.0d0/16081957064101.0d0
      rk_beta(3)  = -  3078876597289.0d0/14206239605852.0d0
      rk_beta(4)  = + 12850230315210.0d0/ 7958570144447.0d0
      rk_beta(5)  = +  0.0d0

      rk_gamma(1) = + 25122485338195.0d0/21609548370812.0d0
      rk_gamma(2) = -   151693240691.0d0/18079648857524.0d0
      rk_gamma(3) = - 69844092779037.0d0/47417574025274.0d0
      rk_gamma(4) = +  0.0d0
      rk_gamma(5) = +  0.0d0

      rk_time(1)  = + 17396840518954.0d0/49788467287365.0d0
      rk_time(2)  = +  8603822252127.0d0/21043514450018.0d0
      rk_time(3)  = +  1540208237857.0d0/ 1802534375082.0d0
      rk_time(4)  = +  2209434402541.0d0/ 2442881288025.0d0
      rk_time(5)  = +  1.0d0

      rk_err(1) = +  273940244.0d0/ 6231996938427.0d0
      rk_err(2) = -    56960205811.0d0/10322406612582.0d0
      rk_err(3) = -    20133252066.0d0/ 2938916102617.0d0
      rk_err(4) = -    57278763901.0d0/ 7692473479450.0d0
      rk_err(5) = +    48690226417.0d0/ 8021897883698.0d0

      rk_p  = +  3.0d0

!   RK4(3)5[3R+]N

    elseif(rk_method==-5432) then

      if(myid==0) then
        write(io,3310)
      endif

      nstage = 5
      call allocate_rk_arrays(1)
      cfl_no = 1.92
      fo_no = 1.58

      rk_alpha(1) = +  4745337637855.0d0/22386579876409.0d0
      rk_alpha(2) = +  6808157035527.0d0/13197844641179.0d0
      rk_alpha(3) = +  4367509502613.0d0/10454198590847.0d0
      rk_alpha(4) = +  1236962429870.0d0/ 3429868089329.0d0
      rk_alpha(5) = +  5597675544274.0d0/18784428342765.0d0

      rk_beta(1)  = -  2228170796640.0d0/13237424273081.0d0
      rk_beta(2)  = -  1609962364470.0d0/11601120165421.0d0
      rk_beta(3)  = -  4382647630219.0d0/11990016156220.0d0
      rk_beta(4)  = -   957265200217.0d0/ 7273122641088.0d0
      rk_beta(5)  = +  0.0d0

      rk_gamma(1) = -   122033598121.0d0/ 7596635636408.0d0
      rk_gamma(2) = +  3191810511535.0d0/12305105232106.0d0
      rk_gamma(3) = -   180663926024.0d0/ 6016639888699.0d0
      rk_gamma(4) = +  0.0d0
      rk_gamma(5) = +  0.0d0

      rk_time(1)  = +  4745337637855.0d0/22386579876409.0d0
      rk_time(2)  = +  4055790361496.0d0/ 7046603553185.0d0
      rk_time(3)  = +  4891280725733.0d0/ 8446165386886.0d0
      rk_time(4)  = + 19183050610406.0d0/22211636934507.0d0
      rk_time(5)  = +  1.0d0

      rk_err(1) = -   158826205349.0d0/ 5724549747200.0d0
      rk_err(2) = +   428957922490.0d0/ 7372359378817.0d0
      rk_err(3) = -   201002715799.0d0/ 4612226272873.0d0
      rk_err(4) = -   195435643717.0d0/39689546882022.0d0
      rk_err(5) = +   174303574743.0d0/ 9648875808155.0d0

      rk_p  = +  3.0d0

!   RK4(3)5[3R+]C

    elseif(rk_method==-5433) then

      if(myid==0) then
        write(io,3315)
      endif

      nstage = 5
      call allocate_rk_arrays(1)
      cfl_no = 1.90
      fo_no = 1.55

      rk_alpha(1) = +  2365592473904.0d0/ 8146167614645.0d0
      rk_alpha(2) = +  4278267785271.0d0/ 6823155464066.0d0
      rk_alpha(3) = +  2789585899612.0d0/ 8986505720531.0d0
      rk_alpha(4) = + 15310836689591.0d0/24358012670437.0d0
      rk_alpha(5) = +   707644755468.0d0/ 5028292464395.0d0

      rk_beta(1)  = -  2358703544796.0d0/14688652793899.0d0
      rk_beta(2)  = -  3946482604925.0d0/10319203051021.0d0
      rk_beta(3)  = -  1764931179206.0d0/ 8145700753681.0d0
      rk_beta(4)  = -  2267563244415.0d0/ 9549593500876.0d0
      rk_beta(5)  = +  0.0d0

      rk_gamma(1) = +  1536825915634.0d0/ 7830765155307.0d0
      rk_gamma(2) = +  1583623634402.0d0/18900840472057.0d0
      rk_gamma(3) = +  3481076545869.0d0/37687342627873.0d0
      rk_gamma(4) = +  0.0d0
      rk_gamma(5) = +  0.0d0

      rk_time(1)  = +  2365592473904.0d0/ 8146167614645.0d0
      rk_time(2)  = +  4451737179416.0d0/ 7941301265987.0d0
      rk_time(3)  = +  4635354999791.0d0/ 7712376515088.0d0
      rk_time(4)  = +  9220772503243.0d0/ 9180822989137.0d0
      rk_time(5)  = +  1.0d0

      rk_err(1) = -    24469017367.0d0/13624928587898.0d0
      rk_err(2) = +    99728691424.0d0/12798583989807.0d0
      rk_err(3) = -   315132139410.0d0/ 8782473047759.0d0
      rk_err(4) = +   603393617734.0d0/20007126526525.0d0
      rk_err(5) = -     4617952588.0d0/16899266533237.0d0

      rk_p  = +  3.0d0

!   RK5(4)8[3R+]C

    elseif(rk_method==-8531) then

      if(myid==0) then
        write(io,3320)
      endif

      nstage = 8
      call allocate_rk_arrays(1)
      cfl_no = 1.51
      fo_no = 2.03

      rk_alpha(1) = +   141236061735.0d0/ 3636543850841.0d0
      rk_alpha(2) = +  7367658691349.0d0/25881828075080.0d0
      rk_alpha(3) = +  6185269491390.0d0/13597512850793.0d0
      rk_alpha(4) = +  2669739616339.0d0/18583622645114.0d0
      rk_alpha(5) = + 42158992267337.0d0/ 9664249073111.0d0
      rk_alpha(6) = +   970532350048.0d0/ 4459675494195.0d0
      rk_alpha(7) = +  1415616989537.0d0/ 7108576874996.0d0
      rk_alpha(8) = +  2987336121747.0d0/15645656703944.0d0

      rk_beta(1)  = +   187879456477.0d0/ 2602683609611.0d0
      rk_beta(2)  = -  7367658691349.0d0/25881828075080.0d0
      rk_beta(3)  = -  6185269491390.0d0/13597512850793.0d0
      rk_beta(4)  = -  2669739616339.0d0/18583622645114.0d0
      rk_beta(5)  = - 29294504657632.0d0/ 7250067238221.0d0
      rk_beta(6)  = -  3219572471797.0d0/14839261481951.0d0
      rk_beta(7)  = +  2161808740835.0d0/12251476641306.0d0
      rk_beta(8)  = +  0.0d0

      rk_gamma(1) = +  1877422408573.0d0/ 7601201019681.0d0
      rk_gamma(2) = +  4057757969325.0d0/18246604264081.0d0
      rk_gamma(3) = -  1415180642415.0d0/13311741862438.0d0
      rk_gamma(4) = + 93461894168145.0d0/25333855312294.0d0
      rk_gamma(5) = -   595897353898.0d0/ 3061185787259.0d0
      rk_gamma(6) = +  1381555823403.0d0/ 4806498811535.0d0
      rk_gamma(7) = +  0.0d0
      rk_gamma(8) = +  0.0d0

      rk_time(1)  = +   141236061735.0d0/ 3636543850841.0d0
      rk_time(2)  = +  1815808778450.0d0/12211231171211.0d0
      rk_time(3)  = +  1507148670416.0d0/ 4387330392145.0d0
      rk_time(4)  = +  5222731013105.0d0/14467543252262.0d0
      rk_time(5)  = +  7872328278434.0d0/10038909666933.0d0
      rk_time(6)  = +  6140113582317.0d0/ 7265604600803.0d0
      rk_time(7)  = +  2950072484215.0d0/ 8546598676273.0d0
      rk_time(8)  = +  1.0d0

      rk_err(1) = +   294678339571.0d0/ 5878347889361.0d0
      rk_err(2) = -  0.0d0
      rk_err(3) = -   816134253487.0d0/ 5866374093317.0d0
      rk_err(4) = -  3314993354287.0d0/18614884498161.0d0
      rk_err(5) = -  7613100683967.0d0/35537520607223.0d0
      rk_err(6) = -  1163824473220.0d0/10662312156731.0d0
      rk_err(7) = +  1057450619660.0d0/12898740879513.0d0
      rk_err(8) = +   786973702639.0d0/ 1547718138224.0d0

      rk_p  = +  4.0d0

!   RK5(4)8[3R+]M

    elseif(rk_method==-8532) then

      if(myid==0) then
        write(io,3325)
      endif

      nstage = 8
      call allocate_rk_arrays(1)
      cfl_no = 0.36
      fo_no = 1.31

      rk_alpha(1) = +   967290102210.0d0/ 6283494269639.0d0
      rk_alpha(2) = +   852959821520.0d0/ 5603806251467.0d0
      rk_alpha(3) = +  8043261511347.0d0/ 8583649637008.0d0
      rk_alpha(4) = -   115941139189.0d0/ 8015933834062.0d0
      rk_alpha(5) = +  2151445634296.0d0/ 7749920058933.0d0
      rk_alpha(6) = + 15619711431787.0d0/74684159414562.0d0
      rk_alpha(7) = + 12444295717883.0d0/11188327299274.0d0
      rk_alpha(8) = +   517396786175.0d0/ 6104475356879.0d0

      rk_beta(1)  = -   762424657179.0d0/10627408097693.0d0
      rk_beta(2)  = -   852959821520.0d0/ 5603806251467.0d0
      rk_beta(3)  = -  8043261511347.0d0/ 8583649637008.0d0
      rk_beta(4)  = +   115941139189.0d0/ 8015933834062.0d0
      rk_beta(5)  = +  2519504519053.0d0/18547636010589.0d0
      rk_beta(6)  = -   777810118309.0d0/ 6606902036587.0d0
      rk_beta(7)  = -  4813302125316.0d0/ 6138796282919.0d0
      rk_beta(8)  = +  0.0d0

      rk_gamma(1) = +   100812510898.0d0/ 5621928522425.0d0
      rk_gamma(2) = +  8677837986029.0d0/16519245648862.0d0
      rk_gamma(3) = -  2224500752467.0d0/10812521810777.0d0
      rk_gamma(4) = -  1245361422071.0d0/ 3717287139065.0d0
      rk_gamma(5) = -   215080079683.0d0/ 9502638510621.0d0
      rk_gamma(6) = + 27253788224171.0d0/38927989026747.0d0
      rk_gamma(7) = +  0.0d0
      rk_gamma(8) = +  0.0d0

      rk_time(1)  = +   967290102210.0d0/ 6283494269639.0d0
      rk_time(2)  = +   789521982937.0d0/ 3647108949923.0d0
      rk_time(3)  = +  9893897414570.0d0/20031063574641.0d0
      rk_time(4)  = +  1854488271975.0d0/ 6781322511713.0d0
      rk_time(5)  = +  3479542759700.0d0/ 5007777688841.0d0
      rk_time(6)  = +  7403943625192.0d0/10178280578119.0d0
      rk_time(7)  = +  6658213476056.0d0/ 6663448320513.0d0
      rk_time(8)  = +  1.0d0

      rk_err(1) = + 46453749185087.0d0/23489081127147.0d0
      rk_err(2) = -  0.0d0
      rk_err(3) = -363896703489104.0d0/14616417357997.0d0
      rk_err(4) = -137724783052777.0d0/19091971364238.0d0
      rk_err(5) = +167280545580034.0d0/ 5901167245273.0d0
      rk_err(6) = + 31288789811128.0d0/ 9850420538565.0d0
      rk_err(7) = - 11244789644109.0d0/ 7972817148859.0d0
      rk_err(8) = +   338623754243.0d0/17375087082380.0d0

      rk_p  = +  4.0d0

!   RK5(4)8[3R+]P{8,7}

    elseif(rk_method==-8533) then

      if(myid==0) then
        write(io,3330)
      endif

      nstage = 8
      call allocate_rk_arrays(1)
      cfl_no = 1.16
      fo_no = 1.59

      rk_alpha(1) = +  1298271176151.0d0/60748409385661.0d0
      rk_alpha(2) = + 14078610000243.0d0/41877490110127.0d0
      rk_alpha(3) = +   553998884433.0d0/ 1150223130613.0d0
      rk_alpha(4) = + 15658478150918.0d0/92423611770207.0d0
      rk_alpha(5) = + 18843935397718.0d0/ 7227975568851.0d0
      rk_alpha(6) = +  6206560082614.0d0/27846110321329.0d0
      rk_alpha(7) = +  2841125392315.0d0/14844217636077.0d0
      rk_alpha(8) = +  1886338382073.0d0/ 9981671730680.0d0

      rk_beta(1)  = +   655286656979.0d0/ 7310837323153.0d0
      rk_beta(2)  = - 14078610000243.0d0/41877490110127.0d0
      rk_beta(3)  = -   553998884433.0d0/ 1150223130613.0d0
      rk_beta(4)  = - 15658478150918.0d0/92423611770207.0d0
      rk_beta(5)  = - 19237683104413.0d0/ 8422218656529.0d0
      rk_beta(6)  = -  1824537024609.0d0/ 8665353908108.0d0
      rk_beta(7)  = +  3125261535649.0d0/18027283412855.0d0
      rk_beta(8)  = +  0.0d0

      rk_gamma(1) = +  1797817386634.0d0/ 5492595268481.0d0
      rk_gamma(2) = +  3833614938189.0d0/14183712281236.0d0
      rk_gamma(3) = -   628609886693.0d0/ 8177399110319.0d0
      rk_gamma(4) = +  4943723744483.0d0/ 2558074780976.0d0
      rk_gamma(5) = -   644827534779.0d0/ 3401380661567.0d0
      rk_gamma(6) = +  1923736113757.0d0/ 6695021554321.0d0
      rk_gamma(7) = +  0.0d0
      rk_gamma(8) = +  0.0d0

      rk_time(1)  = +  1298271176151.0d0/60748409385661.0d0
      rk_time(2)  = +   812872379791.0d0/ 6781137496658.0d0
      rk_time(3)  = +  2784440354641.0d0/ 8637530878711.0d0
      rk_time(4)  = +  7913875958849.0d0/22149362410274.0d0
      rk_time(5)  = +  5660984214782.0d0/ 7206932739887.0d0
      rk_time(6)  = +  1980647985570.0d0/ 2340100545781.0d0
      rk_time(7)  = +  1696732656671.0d0/ 4843398346649.0d0
      rk_time(8)  = +  1.0d0

      rk_err(1) = +   347863759730.0d0/12184523474197.0d0
      rk_err(2) = +  0.0d0
      rk_err(3) = -   798472430005.0d0/ 13882421602211.0d0
      rk_err(4) = -   972791992243.0d0/ 13597677393897.0d0
      rk_err(5) = -  1302232637834.0d0/  7744360035665.0d0
      rk_err(6) = -   669321662029.0d0/ 10825609145658.0d0
      rk_err(7) = +   143854756229.0d0/  3126372926320.0d0
      rk_err(8) = +  2392527981944.0d0/  8410330656419.0d0

      rk_p  = +  4.0d0

    endif
  endif
!----------------------------------------------------------------------------------------
! Four-Register ERK Methods
!----------------------------------------------------------------------------------------
  if(n_reg==5) then

!   RK4(3)5[4R+]M

    if(rk_method==-5541) then

      if(myid==0) then
        write(io,3405)
      endif

      nstage = 5
      call allocate_rk_arrays(1)
      cfl_no = 0.25
      fo_no = 1.07

      rk_alpha(1) = +     7142524119.0d0/    20567653057.0d0
      rk_alpha(2) = +    20567653057.0d0/    89550000000.0d0
      rk_alpha(3) = +    7407775.0d0/    2008982.0d0
      rk_alpha(4) = -    4577300.0d0/  867302297.0d0
      rk_alpha(5) = -       2927.0d0/    546.0d0

      rk_beta(1)  = -  2260447542331.0d0/ 10165172441308.0d0
      rk_beta(2)  = -    20567653057.0d0/    89550000000.0d0
      rk_beta(3)  = -   846558604725.0d0/   272215052018.0d0
      rk_beta(4)  = +    14736541900.0d0/     2601906891.0d0
      rk_beta(5)  = +      0.0d0

      rk_gamma(1) = -  8015963035171.0d0/178831350000000.0d0
      rk_gamma(2) = +   226244183627.0d0/    80359280000.0d0
      rk_gamma(3) = -  2574019812500.0d0/   792021329281.0d0
      rk_gamma(4) = +      0.0d0
      rk_gamma(5) = +      0.0d0

      rk_delta(1) = +    87188745481.0d0/ 47974490160000.0d0
      rk_delta(2) = +    20567653057.0d0/     6979191486.0d0
      rk_delta(3) = +      0.0d0
      rk_delta(4) = +      0.0d0
      rk_delta(5) = +      0.0d0

      rk_time(1)  = +     7142524119.0d0/    20567653057.0d0
      rk_time(2)  = +       1997.0d0/       5000.0d0
      rk_time(3)  = +    199.0d0/    200.0d0
      rk_time(4)  = +      1.0d0
      rk_time(5)  = +      1.0d0

      rk_err(1) = -  7361405000.0d0/1397689797777.0d0
      rk_err(2) = + 0.0d0
      rk_err(3) = +  388713438301.0d0/ 26533624237027.0d0
      rk_err(4) = -  736140500000.0d0/ 1042143269349.0d0
      rk_err(5) = +  7361405000.0d0/ 10561728177.0d0

      rk_p  = +  3.0d0

!   RK4(3)5[4R+]N

    elseif(rk_method==-5442) then

      if(myid==0) then
        write(io,3410)
      endif

      nstage = 5
      call allocate_rk_arrays(1)
      cfl_no = 1.88
      fo_no = 1.85

      rk_alpha(1) = +  9435338793489.0d0/ 32856462503258.0d0
      rk_alpha(2) = +  6195609865473.0d0/ 14441396468602.0d0
      rk_alpha(3) = +  7502925572378.0d0/ 28098850972003.0d0
      rk_alpha(4) = +  4527781290407.0d0/  9280887680514.0d0
      rk_alpha(5) = +  2131913067577.0d0/  7868783702050.0d0

      rk_beta(1)  = -   918305384513.0d0/  5229444841912.0d0
      rk_beta(2)  = -   569121785107.0d0/  3719981314863.0d0
      rk_beta(3)  = -    54337323136.0d0/   312749318069.0d0
      rk_beta(4)  = -  8167387811321.0d0/ 34078439884979.0d0
      rk_beta(5)  = +  0.0d0

      rk_gamma(1) = -   631454356549.0d0/ 10211713347061.0d0
      rk_gamma(2) = +   795908811404.0d0/  6933490771551.0d0
      rk_gamma(3) = -   644143619849.0d0/  7151464013727.0d0
      rk_gamma(4) = +    0.0d0
      rk_gamma(5) = +    0.0d0

      rk_delta(1) = +   887428143793.0d0/ 19126827001792.0d0
      rk_delta(2) = +   744045437717.0d0/  4500795349128.0d0
      rk_delta(3) = +    0.0d0
      rk_delta(4) = +    0.0d0
      rk_delta(5) = +    0.0d0

      rk_time(1)  = +  9435338793489.0d0/ 32856462503258.0d0
      rk_time(2)  = +  7840135831403.0d0/ 13014418220407.0d0
      rk_time(3)  = +  3939727202549.0d0/  7984491496599.0d0
      rk_time(4)  = +  8872726684141.0d0/  9930429157430.0d0
      rk_time(5)  = +    1.0d0

      rk_err(1) = -   100459825139.0d0/  3474501208066.0d0
      rk_err(2) = +   181043710585.0d0/  2185878690161.0d0
      rk_err(3) = -   466847696626.0d0/  6198820625059.0d0
      rk_err(4) = -    70616659197.0d0/ 11470113551173.0d0
      rk_err(5) = +    29853337687.0d0/  1083285380868.0d0

      rk_p  = +  3.0d0

!   RK5(4)6[4R+]M

    elseif(rk_method==-6541) then

      if(myid==0) then
        write(io,3415)
      endif

      nstage = 6
      call allocate_rk_arrays(1)
      cfl_no = 0.36
      fo_no = 1.23

      rk_alpha(1) = +  1811061732419.0d0/  6538712036350.0d0
      rk_alpha(2) = +   936386506953.0d0/  6510757757683.0d0
      rk_alpha(3) = +  8253430823511.0d0/  9903985211908.0d0
      rk_alpha(4) = +  4157325866175.0d0/ 11306150349782.0d0
      rk_alpha(5) = +  3299942024581.0d0/ 13404534943033.0d0
      rk_alpha(6) = +  2571845656138.0d0/  6012342010435.0d0

      rk_beta(1)  = -  2498956064668.0d0/ 13987551168181.0d0
      rk_beta(2)  = -   936386506953.0d0/  6510757757683.0d0
      rk_beta(3)  = -  5191726833799.0d0/ 10301328346619.0d0
      rk_beta(4)  = -   638559022989.0d0/  7887636352550.0d0
      rk_beta(5)  = -  2357736707767.0d0/  6070886881575.0d0
      rk_beta(6)  = +  0.0d0

      rk_gamma(1) = -   1239981585247.0d0/30908605494880.0d0
      rk_gamma(2) = +   4242729801665.0d0/12001587034923.0d0
      rk_gamma(3) = -   4429937021284.0d0/13821336722899.0d0
      rk_gamma(4) = +   1411804058081.0d0/ 9799873841964.0d0
      rk_gamma(5) = +    0.0d0
      rk_gamma(6) = +    0.0d0

      rk_delta(1) = +   1055991192687.0d0/19512276830717.0d0
      rk_delta(2) = +  19590089343957.0d0/51581831082203.0d0
      rk_delta(3) = -    363097616279.0d0/12782539261113.0d0
      rk_delta(4) = +    0.0d0
      rk_delta(5) = +    0.0d0
      rk_delta(6) = +    0.0d0

      rk_time(1)  = +   1811061732419.0d0/ 6538712036350.0d0
      rk_time(2)  = +   2015262744377.0d0/ 7139772146148.0d0
      rk_time(3)  = +   7046145417802.0d0/13446056846065.0d0
      rk_time(4)  = +   3689037202129.0d0/ 5011527044260.0d0
      rk_time(5)  = +   9748229241971.0d0/11537062637773.0d0
      rk_time(6)  = +    1.0d0

      rk_err(1) = -    474526149155.0d0/11267234911172.0d0
      rk_err(2) = +   0.0d0
      rk_err(3) = +   2869796380963.0d0/12908252193106.0d0
      rk_err(4) = -   3062439520007.0d0/ 6815593375189.0d0
      rk_err(5) = +   6912662528037.0d0/13758281156936.0d0
      rk_err(6) = -   4059643303833.0d0/17399827433720.0d0

      rk_p  = +  4.0d0

!   RK5(4)8[4R+]FM

    elseif(rk_method==-8541) then

      if(myid==0) then
        write(io,3420)
      endif

      nstage = 7
      call allocate_rk_arrays(1)
      cfl_no = 1.14
      fo_no = 1.29

      rk_alpha(1) = +    319960152914.0d0/39034091721739.0d0
      rk_alpha(2) = +  16440040368765.0d0/ 7252463661539.0d0
      rk_alpha(3) = +   1381950791880.0d0/ 6599155371617.0d0
      rk_alpha(4) = +  18466735994895.0d0/ 7394178462407.0d0
      rk_alpha(5) = +   2786140924985.0d0/14262827431161.0d0
      rk_alpha(6) = +  28327099865656.0d0/21470840267743.0d0
      rk_alpha(7) = +    583593328277.0d0/ 7028929464160.0d0

      rk_beta(1)  = +    502803460989.0d0/ 6765368927269.0d0
      rk_beta(2)  = -  16440040368765.0d0/ 7252463661539.0d0
      rk_beta(3)  = -   2473127212433.0d0/12953302840403.0d0
      rk_beta(4)  = - 101965272791039.0d0/48609283777077.0d0
      rk_beta(5)  = -   9744635584492.0d0/54405093554917.0d0
      rk_beta(6)  = -   4024561864218.0d0/ 4377331201277.0d0
      rk_beta(7)  = +  0.0d0

      rk_gamma(1) = +  13595968464622.0d0/ 6304458288905.0d0
      rk_gamma(2) = +   1316066362688.0d0/10261382634081.0d0
      rk_gamma(3) = +  18641873735496.0d0/ 7446073763509.0d0
      rk_gamma(4) = -    447943374581.0d0/ 3628739336798.0d0
      rk_gamma(5) = +   2595619009140.0d0/ 2906342141773.0d0
      rk_gamma(6) = +    0.0d0
      rk_gamma(7) = +    0.0d0

      rk_delta(1) = -   2196407183887.0d0/18813574397839.0d0
      rk_delta(2) = -   4579492417936.0d0/ 7930641522963.0d0
      rk_delta(3) = +    870791039077.0d0/ 9311683500752.0d0
      rk_delta(4) = -    690424648491.0d0/12175664726881.0d0
      rk_delta(5) = +    0.0d0
      rk_delta(6) = +    0.0d0
      rk_delta(7) = +    0.0d0

      rk_time(1)  = +    319960152914.0d0/39034091721739.0d0
      rk_time(2)  = +   1290352467424.0d0/ 6693585127285.0d0
      rk_time(3)  = +   2094253883098.0d0/ 7468222743911.0d0
      rk_time(4)  = +   3695876846933.0d0/ 5497099592722.0d0
      rk_time(5)  = +  11666031399971.0d0/16066843095145.0d0
      rk_time(6)  = +   1.0d0
      rk_time(7)  = +   1.0d0

      rk_err(1) = +   2023383632057.0d0/26525303340911.0d0
      rk_err(2) = +   0.0d0
      rk_err(3) = +    480990062147.0d0/12694528747923.0d0
      rk_err(4) = +  14502014597821.0d0/36979005529861.0d0
      rk_err(5) = -   3883966523914.0d0/63014133260123.0d0
      rk_err(6) = +   1643296191892.0d0/ 3432451463915.0d0
      rk_err(7) = +   2576984903812.0d0/11692468803935.0d0

      rk_err(8) = -   2393889703871.0d0/16641202878460.0d0

      rk_p  = +  4.0d0

    endif
   endif
!----------------------------------------------------------------------------------------
! Five-Register ERK Methods
!----------------------------------------------------------------------------------------
  if(n_reg==6) then

!   RK5(4)7[5R+]FM

    if(rk_method==-7551) then

      if(myid==0) then
        write(io,3505)
      endif

      nstage = 7
      call allocate_rk_arrays(1)
      cfl_no = 1.06
      fo_no = 1.31

      rk_alpha(1) = +    984894634849.0d0/ 6216792334776.0d0
      rk_alpha(2) = +    984894634849.0d0/ 5526037630912.0d0
      rk_alpha(3) = +  13256335809797.0d0/10977774807827.0d0
      rk_alpha(4) = +   5386479425293.0d0/11045691190948.0d0
      rk_alpha(5) = -   1717767168952.0d0/11602237717369.0d0
      rk_alpha(6) = -  10054679524430.0d0/10306851287569.0d0
      rk_alpha(7) = +    599706619333.0d0/ 7161178965783.0d0

      rk_beta(1)  = -    762086376787.0d0/13547537819124.0d0
      rk_beta(2)  = -    984894634849.0d0/ 5526037630912.0d0
      rk_beta(3)  = -   5236632985735.0d0/ 6894382207903.0d0
      rk_beta(4)  = -   2216853586553.0d0/ 5172497918462.0d0
      rk_beta(5)  = +   2791627064364.0d0/ 5113091693363.0d0
      rk_beta(6)  = +   5685865005008.0d0/ 6427497867525.0d0
      rk_beta(7)  = +   0.0d0

      rk_gamma(1) = +    325572770784.0d0/ 7613458315117.0d0
      rk_gamma(2) = +  18544705752398.0d0/18426539884027.0d0
      rk_gamma(3) = +   5147883389045.0d0/12609389211517.0d0
      rk_gamma(4) = -   3251765459579.0d0/ 8873085246216.0d0
      rk_gamma(5) = -   6651080261449.0d0/ 8990416662241.0d0
      rk_gamma(6) = +  0.0d0
      rk_gamma(7) = +  0.0d0

      rk_delta(1) = -   7159355210199.0d0/35495799519628.0d0
      rk_delta(2) = -    342961171087.0d0/ 6505721096888.0d0
      rk_delta(3) = +   8640783587860.0d0/ 7314440083033.0d0
      rk_delta(4) = +  37240192927547.0d0/23794345321548.0d0
      rk_delta(5) = +  0.0d0
      rk_delta(6) = +  0.0d0
      rk_delta(7) = +  0.0d0

      rk_eps(1)   = -    546782077747.0d0/12304107641433.0d0
      rk_eps(2)   = -   2896263505307.0d0/ 6364015805096.0d0
      rk_eps(3)   = -   9814786935957.0d0/ 5471966597281.0d0
      rk_eps(4)   = +  0.0d0
      rk_eps(5)   = +  0.0d0
      rk_eps(6)   = +  0.0d0
      rk_eps(7)   = +  0.0d0

      rk_time(1)  = +    984894634849.0d0/ 6216792334776.0d0
      rk_time(2)  = +   4548303640769.0d0/19139684733553.0d0
      rk_time(3)  = +   9139118289356.0d0/18096674600063.0d0
      rk_time(4)  = +  55857529204697.0d0/76860938131493.0d0
      rk_time(5)  = +    537068878733.0d0/ 5294424325105.0d0
      rk_time(6)  = +  1.0d0
      rk_time(7)  = +  1.0d0

      rk_err(1) = -    107932874426.0d0/ 6258612452897.0d0
      rk_err(2) = + 0.0d0
      rk_err(3) = -   1576352498437.0d0/43096999420429.0d0
      rk_err(4) = +     87967753542.0d0/ 6646951058915.0d0
      rk_err(5) = -     45385583049.0d0/13027291261799.0d0
      rk_err(6) = +    549309204622.0d0/12485890511769.0d0
      rk_err(7) = +  1163627596.0d0/15007934652337.0d0

      rk_p  = +  4.0d0

    endif
  endif

!----------------------------------------------------------------------------------------
! check to make sure proper initialization occured

  if(.not.allocated(rk_alpha)) then  !rk_alpha used as a simple test
    if(myid==0) then
      write(io,*) 'improper setting of ERK method in file erk.in...'
      write(io,'(1x,a,i4)') 'rk_method = ',rk_method
    endif
    call terminate_run(io,0)
  endif
!----------------------------------------------------------------------------------------
! write header

  if(myid==0) then
    call write_header(io,'-')
  endif
!----------------------------------------------------------------------------------------
! format statements

  3205 format('   using RK3(2)3[2R+] Wray ERK scheme!')
  3210 format('   using RK3(2)4[2R+]C ERK scheme!')
  3215 format('   using RK4(3)5[2R+]C ERK scheme!')
  3216 format('   using RK4(3)6[2R+]C ERK scheme!')
  3220 format('   using RK5(4)9[2R+]C ERK scheme!')
  3225 format('   using RK5(4)9[2R+]M ERK scheme!')
  3230 format('   using RK5(4)9[2R+]S ERK scheme!')

  3305 format('   using RK4(3)5[3R+]C ERK scheme!')
  3310 format('   using RK4(3)5[3R+]M ERK scheme!')
  3315 format('   using RK4(3)5[3R+]N ERK scheme!')
  3320 format('   using RK5(4)8[3R+]C ERK scheme!')
  3325 format('   using RK5(4)8[3R+]M ERK scheme!')
  3330 format('   using RK5(4)8[3R+]P{8,7} ERK scheme!')

  3405 format('   using RK4(3)5[4R+]M ERK scheme!')
  3410 format('   using RK4(3)5[4R+]N ERK scheme!')
  3415 format('   using RK5(4)6[4R+]M ERK scheme!')
  3420 format('   using RK5(4)8[4R+]FM ERK scheme!')

  3505 format('   using RK5(4)7[5R+]M ERK scheme!')
!----------------------------------------------------------------------------------------
  return
  end subroutine initialize_rk
!========================================================================================
  subroutine controller(tstep,time,i_time,i_restart,time_restart,                   &
                        run_title,rk_p,q,u,volum,pressure,gamma,delx,dely,delz,     &
                        cfl_no,io)
!========================================================================================
! prepares the next timestep
! this must be done at every time step!
!----------------------------------------------------------------------------------------
  use param_m, only : nx, ny, nz, nvar_tot, nsc, n_reg
  use topology_m
  use reference_m, only : time_ref

  implicit none
!----------------------------------------------------------------------------------------
! declarations passed in

  integer io
  integer i_time, i_restart
  real time, tstep, time_restart
  character*20 run_title
  real q(nx,ny,nz,nvar_tot,n_reg)
  real rk_p
  real gamma(nx,ny,nz)
  real pressure(nx,ny,nz)
  real volum(nx,ny,nz)
  real u(nx,ny,nz,3)
  real delx,dely,delz,cfl_no

! local declarations

  real q_err_max_l
  real tstep_l(2), tstep_g(2)

  character*30 tstep_limit  !limit description
  real w(2)         !timestep ratios
  real alpha_cont   !hard-coded to incorporate up to PID2 controller
  real beta_cont    !hard-coded to incorporate up to PID2 controller
  real gamma_cont   !hard-coded to incorporate up to PID2 controller
  real delta_cont   !hard-coded to incorporate up to PID2 controller
  real exp_cont(4)  !hard-coded to incorporate up to PID2 controller
  real tstep_t, tstep_cfl

  integer ijkeq_loc(4)

  integer i,j,k,L,n

  character*100 filename

  logical exist

  integer :: mpistatus, mpireq

  real :: tol

  real :: vsmall
!----------------------------------------------------------------------------------------
! set initial stuff
!----------------------------------------------------------------------------------------
  tstep_limit='none'

  tstep_l=0.0
  tstep_g=0.0

  q_err_max_l=0.0

  ijkeq_loc(:)=0

  tstep=0.0
!----------------------------------------------------------------------------------------
! start controller calculations
!----------------------------------------------------------------------------------------
! normalize error

  if(i_time==1) then

    q_err = 0.0  !set the entire q_err vector equal to zero

  else

    q_err(:,:,:,:,1)=abs(q_err(:,:,:,:,1))/(abs(q(:,:,:,:,1))+rk_tol)

  endif
!----------------------------------------------------------------------------------------
! hard-code timestep here

  if(cont_switch==0) then
    tstep=tstep_init/time_ref
    tstep_limit='fixed timestep'
    goto 100
  endif
!----------------------------------------------------------------------------------------
! calculate tstep based on RK error control

  if(i_time.le.4) then  !hard-coded to incorporate up to PID2 controller

!   first four time steps set to input value

    tstep=tstep_init/time_ref
    tstep_limit='fixed timestep'

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

    exp_cont(1) =  alpha_cont/rk_p
    exp_cont(2) = - beta_cont/rk_p
    exp_cont(3) =  gamma_cont/rk_p
    exp_cont(4) = -delta_cont/rk_p

!   calculate local timestep based on error (tstep_l)

    tstep_l(1)=huge(tstep_l(1))

!   BUG FIX
!   Evatt Hawkes 11-AUG-04
    vsmall = tiny(tstep_l(1))


    ijkeq_loc(:)=1

    do L=1,nvar_tot
      do k=1,nz
        do j=1,ny
          do i=1,nx

            tstep_t=0.0

            if(minval(q_err(i,j,k,L,:)).gt.0.0) then
              tstep_t=tstep_vec(1)*cont_safety
              do n=1,cont_n_reg
!               BUG FIX
!               Evatt Hawkes 11-AUG-04
                tstep_t=tstep_t*(rk_rtol/(q_err(i,j,k,L,n)+vsmall))**(exp_cont(n))
              enddo
            endif

            if((tstep_t.gt.0.0).and.(tstep_t.lt.tstep_l(1))) then
              tstep_l(1)=tstep_t
              ijkeq_loc(1)=i+(xid*nx)
              ijkeq_loc(2)=j+(yid*ny)
              ijkeq_loc(3)=k+(zid*nz)
              ijkeq_loc(4)=L
            endif


          enddo
        enddo
      enddo
    enddo

!   check to make sure a reasonable tstep was found

    if(tstep_l(1).le.0.0) then
      write(io,*) 'tstep_l(1) is not reasonable for processor =',myid
      write(io,*) 'see routine controller in rk_m'
      write(io,*) 'tstep_l(1) =',tstep_l(1)
      call terminate_run(io,0)  !must be called by all processors
    endif

!   get global minimum of tstep_l

    tstep_l(2)=float(myid)

#if defined(ARCH_SGI) || defined(ARCH_SP2) || defined(ARCH_OPTERON) || defined(ARCH_PC) || defined(ARCH_X1)
      call MPI_Allreduce(tstep_l,tstep_g,1,MPI_2DOUBLE_PRECISION,MPI_MINLOC,gcomm,ierr)
#endif
#if defined(ARCH_T3E)
      call MPI_Allreduce(tstep_l,tstep_g,1,MPI_2REAL,MPI_MINLOC,gcomm,ierr)
#endif

!   Now tstep_g(2) contains the processor ID which has the grid
!   point that determined the minimum time-step.

    tstep=tstep_g(1)

    tstep_limit='error control'

!   check for max change

    if(tstep/tstep_vec(1).gt.cont_factor) then
      tstep=tstep_vec(1)*cont_factor
      tstep_limit = 'maximum change'
    endif

!   reset timestep to the specified minimum if necessary

    if(tstep < tstep_min/time_ref) then
       tstep = tstep_min/time_ref
       tstep_limit = 'minimum timestep'
    endif

!   reset timestep to the specified maximum if necessary

    if(tstep > tstep_max/time_ref) then
       tstep = tstep_max/time_ref
       tstep_limit = 'maximum timestep'
    endif

  endif
!----------------------------------------------------------------------------------------
! continue with advancing

  100 continue
!----------------------------------------------------------------------------------------
! pass off timestep

  do n=4,2,-1
    tstep_vec(n)=tstep_vec(n-1)
  enddo
  tstep_vec(1)=tstep  !essentially, tstep_vec(1) always equals tstep

! pass off error

  do n=cont_n_reg,2,-1
    q_err(:,:,:,:,n)=q_err(:,:,:,:,n-1)
  enddo
!----------------------------------------------------------------------------------------
! return now if no output is desired

  if(i_time_cont==0) return
!----------------------------------------------------------------------------------------
! write timestep data to output files if we are at the appropriate time step

  if((mod(i_time,i_time_cont)==0).or.(i_time.le.10)) then

!   determine maximum error from the previous iteration

    if(i_time.eq.1) then
      q_err_max=0.0
    else
      q_err_max_l=maxval(q_err(:,:,:,:,1))
!esr       call mpi_barrier(gcomm, ierr) ! the C2H4 lifted production case had these barrier statements,
!                                       I've added them here and commented them for future reference.
      call MPI_Allreduce(q_err_max_l,q_err_max,1,MPI_REAL8,MPI_MAX,gcomm,ierr)
!esr       call mpi_barrier(gcomm, ierr)
    endif

!   Processor 0 gets information about where the timestep was determined
!   from the processor which owns that timestep.  See the "Allreduce" on 
!   "tstep_l" and "tstep_g" above.

    if(myid==(nint(tstep_g(2))) .and. myid .ne. 0) then
      call MPI_Send(ijkeq_loc,4,MPI_INTEGER,0,9999,gcomm,ierr)
    endif

    if(myid==0 .and. myid .ne. (nint(tstep_g(2))) ) then
      call MPI_Recv(ijkeq_loc,4,MPI_INTEGER,nint(tstep_g(2)),9999,gcomm,mpistatus,ierr)
    endif

!   calculate cfl timestep limit (if desired)

    tstep_cfl=0.0
    if(cfl_switch==1) then
     call calc_cfl_limit(tstep_cfl,u,volum,gamma,pressure,time_ref,delx,dely,delz,cfl_no)
    endif

!   write diagnostic information to a file
!
!   NOTE that the location ijkeq_loc refers to the grid point and equation
!   which determined the timestep.  This does NOT necessarily coincide with
!   the grid point where the maximum error (q_err_max) occurs.  This is 
!   because the time step is determined using an error history, not just the
!   maximum error for this time step...

    if(myid==0) then

      filename='../data/'//trim(run_title)//'.ts.dat'
      inquire(file=trim(filename),exist=exist)

      if(exist) then

        open(unit=1,file=trim(filename),status='old',position='append')

        if((i_restart==1).and.(i_time==1)) then
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

!    Commented out by Ramanan - 04/18/05
!    call MPI_Barrier(gcomm,ierr)

  endif
!----------------------------------------------------------------------------------------
! format statements

  24 format(i9,1x,4(1pe10.3,1x),3x,4(i4,3x),a)
  25 format('#timestep    time     time step  time step   maximum   ',  &
            '-----time step location----     time step')
  26 format('#  number    (sec)      (sec)    cfl (sec)    error    ',  &
            'eq_loc  i_loc  j_loc  k_loc      control')
!----------------------------------------------------------------------------------------
  return
  end subroutine controller
!-----------------------------------------------------------------------------------------
  end module rk_m

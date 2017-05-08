#include "globalDefines.h"
!----------------------------------------------------------------------
! New routine by Ramanan Sankaran (rsankar@sandia.gov)
! Vector getrates for the Cray X1 platform
! Partly auto-generated using autogr.pl, partly hand coded. 
! Tianfeng Lu and CK Law's reduced CH4/Air mechanism
! Mechanism is for lean CH4/air flame
! 13 species, 4QSS, 73 reactions
!----------------------------------------------------------------------
! Comments
!    All pointers are turned into real arrays
!    Cray compiler does not like pointers.
!----------------------------------------------------------------------

subroutine vecrates(P, T, Y, RHO, WDOT)
use chemkin_m, only: molwt_c, n_species
use param_m, only: nx, ny, nz
use thermchem_m, only: avmolwt
use reference_m, only: t_ref, rho_ref
use gibbs_rxn_table_m, only: gibbs_compute_index, gibbs_lookup_all
implicit none

real, dimension(nx, ny, nz), intent(in):: P, T, RHO
real, dimension(nx, ny, nz, n_species), intent(in):: Y
real, dimension(nx, ny, nz, n_species), intent(out):: WDOT

!local declarations
integer, parameter :: n_qss = 17

real, dimension(nx, ny, nz, n_qss) :: cspl  !species molar concentrations
real, dimension(nx,ny,nz) :: vlntemp, ortc, dimT
real, dimension(nx,ny,nz) :: otc, prt, oprt
real, dimension(nx,ny,nz) :: flogpr, fdenom, fquan, fcent
real, dimension(nx,ny,nz) :: thbctemp2, thbctemp3, thbctemp4, thbctemp5, &
                             thbctemp6, thbctemp7, thbctemp8
real, dimension(nx,ny,nz) :: rr_f, rr_r, ropl, pr, dG

real, dimension(nx,ny,nz) :: &
           ABV1, DEN1,  &
           ABV2, DEN2,  &
           ABV3, DEN3,  &
           ABV4, DEN4

real prtcoeff, oprtcoeff
real logfcent

!Arrays to store the reaction rates. 
!To be used to compute the QSS relations
real, dimension(nx, ny, nz) ::  &
      rr_f_05, rr_r_05,             &
      rr_f_06, rr_r_06,             &
      rr_f_10, rr_r_10,             &
      rr_f_11, rr_r_11,             &
      rr_f_12, rr_r_12,             &
      rr_f_13, rr_r_13,             &
      rr_f_15, rr_r_15,             &
      rr_f_29, rr_r_29,             &
      rr_f_32, rr_r_32,             &
      rr_f_33, rr_r_33,             &
      rr_f_34, rr_r_34,             &
      rr_f_35, rr_r_35,             &
      rr_f_36, rr_r_36,             &
      rr_f_37, rr_r_37,             &
      rr_f_38, rr_r_38,             &
      rr_f_43, rr_r_43,             &
      rr_f_44, rr_r_44,             &
      rr_f_45, rr_r_45,             &
      rr_f_46, rr_r_46,             &
      rr_f_49, rr_r_49,             &
      rr_f_50, rr_r_50,             &
      rr_f_51, rr_r_51,             &
      rr_f_52, rr_r_52,             &
      rr_f_55, rr_r_55,             &
      rr_f_56, rr_r_56,             &
      rr_f_57, rr_r_57,             &
      rr_f_58, rr_r_58,             &
      rr_f_59, rr_r_59,             &
      rr_f_60, rr_r_60,             &
      rr_f_61, rr_r_61,             &
      rr_f_62, rr_r_62,             &
      rr_f_63, rr_r_63,             &
      rr_f_64, rr_r_64,             &
      rr_f_65, rr_r_65,             &
      rr_f_66, rr_r_66,             &
      rr_f_68, rr_r_68,             &
      rr_f_69, rr_r_69,             &
      rr_f_70, rr_r_70,             &
      rr_f_71, rr_r_71,             &
      rr_f_72, rr_r_72,             &
      rr_f_73, rr_r_73

! Arrays used for storage when computing QSS
real, dimension(nx, ny, nz) ::  &
      F1, A12, A13, F2, A21, A24, F3, A31, F4, A42, FF1, AA12, AA21, &
      TEMP, FF2

integer i, j, k, n

!Universal gas constant in erg/(mol K)
real, parameter:: Ru =8.31451e+07

!Universal gas constant in cal/(mol K)
real, parameter:: Ruc=1.9872155832

! atm. pressure; dyne/cm^2
real, parameter:: PA=1.013250e+06

real :: dln10

integer, parameter:: NC_H2     =  1
integer, parameter:: NC_H      =  2
integer, parameter:: NC_O      =  3
integer, parameter:: NC_O2     =  4
integer, parameter:: NC_OH     =  5
integer, parameter:: NC_H2O    =  6
integer, parameter:: NC_HO2    =  7
integer, parameter:: NC_CH3    =  8
integer, parameter:: NC_CH4    =  9
integer, parameter:: NC_CO     = 10
integer, parameter:: NC_CO2    = 11
integer, parameter:: NC_CH2O   = 12
integer, parameter:: NC_N2     = 13
integer, parameter:: NC_CH2    = 14
integer, parameter:: NC_CH2S   = 15
integer, parameter:: NC_HCO    = 16
integer, parameter:: NC_CH2OH  = 17

dln10 = log(10.0)

!1e-6 is to convert to cm^3 from m^3
!Molar concentrations of non-qss species
do n = 1, n_species
  cspl(:,:,:,n) = RHO(:,:,:)*Y(:,:,:,n)*molwt_c(n)*1e-6*rho_ref 
end do

dimT(:,:,:) = t_ref*T(:,:,:)
vlntemp(:,:,:) = log(t_ref) + log(T(:,:,:))
ortc(:,:,:) = (1.0/Ruc)*(1.0/t_ref)/T(:,:,:)

prtcoeff = PA*Ruc/RU
oprtcoeff = t_ref*RU/PA

!Since we dont worry abt these two extra arrays...
!otherwise... we can use otc = Ruc*ortc and prt = prtcoeff*ortc
! and oprt = oprtcoeff*T

otc = 1.0/dimT
prt = PA/RU/dimT
oprt = dimT*RU/PA

!call gibbs_compute_index(T)


!use flogpr to store the sum of all third bodies
!then increase by the known efficiencies. 

flogpr = rho(:,:,:)*avmolwt(:,:,:)*1e-6*rho_ref

thbctemp2 = flogpr & 
      + 1.4e+000*cspl(:,:,:,NC_H2)      &
      + 1.44e+001*cspl(:,:,:,NC_H2O)    &
      + cspl(:,:,:,NC_CH4)              &
      + 7.5e-001*cspl(:,:,:,NC_CO)      &
      + 2.6e+000*cspl(:,:,:,NC_CO2)

thbctemp3 = flogpr & 
      + cspl(:,:,:,NC_H2)  &
      + 5.0e+000*cspl(:,:,:,NC_H2O)  &
      + cspl(:,:,:,NC_CH4)  &
      + 5.0e-001*cspl(:,:,:,NC_CO)  &
      + cspl(:,:,:,NC_CO2) 

thbctemp4 = flogpr & 
      + cspl(:,:,:,NC_H2)  &
      + 5.0e+000*cspl(:,:,:,NC_O2)  &
      + 5.0e+000*cspl(:,:,:,NC_H2O)  &
      + cspl(:,:,:,NC_CH4)  &
      + 5.0e-001*cspl(:,:,:,NC_CO)  &
      + 2.5e+000*cspl(:,:,:,NC_CO2)

thbctemp5 = flogpr & 
      - cspl(:,:,:,NC_O2) &
      - cspl(:,:,:,NC_H2O) &
      - 2.5e-001* cspl(:,:,:,NC_CO) &
      + 5.0e-001*cspl(:,:,:,NC_CO2) &
      - cspl(:,:,:,NC_N2)

thbctemp6 = flogpr & 
      - cspl(:,:,:,NC_H2) &
      - cspl(:,:,:,NC_H2O) &
      + cspl(:,:,:,NC_CH4) &
      - cspl(:,:,:,NC_CO2)

thbctemp7 = flogpr &
      - 2.7e-001*cspl(:,:,:,NC_H2) &
      + 2.65e+000*cspl(:,:,:,NC_H2O)&
      + cspl(:,:,:,NC_CH4)

thbctemp8 = flogpr &
      + cspl(:,:,:,NC_H2) &
      - cspl(:,:,:,NC_H2O) &
      + cspl(:,:,:,NC_CH4) &
      + 5.0e-001*cspl(:,:,:,NC_CO) &
      + cspl(:,:,:,NC_CO2)

!----------------------------------------------------------------------
! ... loop over all rxns to evaluate rate-of-progress

!----------------------------------------------------------------------
!   5)  CH2 + O <=> HCO + H
      rr_f_05 = 8.0e+013
      call gibbs_lookup_all(T, 05, dG)
      rr_r_05 = rr_f_05 * exp(dG)

      rr_f_05 = rr_f_05 * cspl(:,:,:,NC_O) 
      rr_r_05 = rr_r_05 * cspl(:,:,:,NC_H) 
      
!----------------------------------------------------------------------
!   6)  CH2(S) + O <=> CO + H2
      rr_f_06 = 1.5e+013
      call gibbs_lookup_all(T, 06, dG)
      rr_r_06 = rr_f_06 * exp(dG)

      rr_f_06 = rr_f_06 * cspl(:,:,:,NC_O) 
      rr_r_06 = rr_r_06 * cspl(:,:,:,NC_H2) * cspl(:,:,:,NC_CO)
      
!----------------------------------------------------------------------
!  10)  HCO + O <=> CO + OH
      rr_f_10 = 3.0e+013
      call gibbs_lookup_all(T, 10, dG)
      rr_r_10 = rr_f_10 * exp(dG)
      
      rr_f_10 = rr_f_10 * cspl(:,:,:,NC_O) 
      rr_r_10 = rr_r_10 * cspl(:,:,:,NC_OH) * cspl(:,:,:,NC_CO)

!----------------------------------------------------------------------
!  11)  HCO + O <=> CO2 + H
      rr_f_11 = 3.0e+013
      call gibbs_lookup_all(T, 11, dG)
      rr_r_11 = rr_f_11 * exp(dG)
      
      rr_f_11 = rr_f_11 * cspl(:,:,:,NC_O) 
      rr_r_11 = rr_r_11 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_CO2)

!----------------------------------------------------------------------
!  12)  CH2O + O <=> HCO + OH
      rr_f_12 = 3.9e+013 * exp(-3.54e+003*ortc)
      call gibbs_lookup_all(T, 12, dG)
      rr_r_12 = rr_f_12 * exp(dG)
      
      rr_f_12 = rr_f_12 * cspl(:,:,:,NC_O) * cspl(:,:,:,NC_CH2O)
      rr_r_12 = rr_r_12 * cspl(:,:,:,NC_OH) 

!----------------------------------------------------------------------
!  13)  CH2OH + O <=> CH2O + OH
      rr_f_13 = 1.0e+013
      call gibbs_lookup_all(T, 13, dG)
      rr_r_13 = rr_f_13 * exp(dG)
      
      rr_f_13 = rr_f_13 * cspl(:,:,:,NC_O) 
      rr_r_13 = rr_r_13 * cspl(:,:,:,NC_OH) * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------
!  15)  CH2O + O2 <=> HCO + HO2
      rr_f_15 = 1.0e+014 * exp(-4.0e+004*ortc)
      call gibbs_lookup_all(T, 15, dG)
      rr_r_15 = rr_f_15 * exp(dG)
      
      rr_f_15 = rr_f_15 * cspl(:,:,:,NC_O2) * cspl(:,:,:,NC_CH2O)
      rr_r_15 = rr_r_15 * cspl(:,:,:,NC_HO2) 

!----------------------------------------------------------------------
!  29)  CH2 + H (+M) <=> CH3 (+M) 

      !rr_k0 is stored in rr_r
      rr_r = 3.2e+027 * exp(-3.14e+000*vlntemp - 1.23e+003*ortc)
      !rr_kinf is stored in rr_f
      rr_f = 2.5e+016 * exp(-8.0e-001*vlntemp)

      pr = rr_r / rr_f * thbctemp3

      fcent = log10(3.2e-001 * exp(-1.282051282051282e-002 * dimT) + &
        6.800000000000001e-001 * exp(-5.012531328320802e-004 * dimT)&
         + exp(-5.59e+003 * otc))
      flogpr = log10(pr) - 0.4 - 0.67 * fcent
      fdenom = 0.75 - 1.27 * fcent - 0.14 * flogpr
      fquan = flogpr / fdenom
      fquan = fcent / (1.0 + fquan * fquan)

      !rr_kinf is stored in rr_f
      rr_f_29 = rr_f * pr/(1.0 + pr) * exp(fquan*dln10)
      call gibbs_lookup_all(T, 29, dG)
      rr_r_29 = rr_f_29 * exp(dG) * prt
      
      rr_f_29 = rr_f_29 * cspl(:,:,:,NC_H) 
      rr_r_29 = rr_r_29 * cspl(:,:,:,NC_CH3)

!----------------------------------------------------------------------
!  32)  HCO + H (+M) <=> CH2O (+M) 
      !rr_k0 is stored in rr_r
      rr_r = 1.35e+024 * exp(-2.57e+000*vlntemp - 1.425e+003*ortc)
      !rr_kinf is stored in rr_f
      rr_f = 1.09e+012 * exp(4.8e-001*vlntemp + 2.6e+002*ortc)

      pr = rr_r / rr_f * thbctemp3

      fcent = log10(2.176e-001 * exp(-3.690036900369004e-003 * dimT) + &
        7.824e-001 * exp(-3.629764065335753e-004 * dimT) + exp(-6.57e+003 &
         * otc))
      flogpr = log10(pr) - 0.4 - 0.67 * fcent
      fdenom = 0.75 - 1.27 * fcent - 0.14 * flogpr
      fquan = flogpr / fdenom
      fquan = fcent / (1.0 + fquan * fquan)

      !rr_kinf is stored in rr_f
      rr_f_32 = rr_f * pr/(1.0 + pr) * exp(fquan*dln10)
      call gibbs_lookup_all(T, 32, dG)
      rr_r_32 = rr_f_32 * exp(dG) * prt
      
      rr_f_32 = rr_f_32 * cspl(:,:,:,NC_H) 
      rr_r_32 = rr_r_32 * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------
!  33)  HCO + H <=> CO + H2
      rr_f_33 = 7.34e+013
      call gibbs_lookup_all(T, 33, dG)
      rr_r_33 = rr_f_33 * exp(dG)
      
      rr_f_33 = rr_f_33 * cspl(:,:,:,NC_H) 
      rr_r_33 = rr_r_33 * cspl(:,:,:,NC_H2) * cspl(:,:,:,NC_CO)

!----------------------------------------------------------------------
!  34)  CH2O + H (+M) <=> CH2OH (+M) 
      !rr_k0 is stored in rr_r
      rr_r = 1.27e+032 * exp(-4.82e+000*vlntemp - 6.53e+003*ortc)
      !rr_kinf is stored in rr_f
      rr_f = 5.4e+011 * exp(4.54e-001*vlntemp - 3.6e+003*ortc)

      pr = rr_r / rr_f * thbctemp3

      fcent = log10(2.813e-001 * exp(-9.708737864077669e-003 * dimT) +    &
        7.187e-001 * exp(-7.74593338497289e-004 * dimT) + exp(-4.16e+003   &
         * otc))
      flogpr = log10(pr) - 0.4 - 0.67 * fcent
      fdenom = 0.75 - 1.27 * fcent - 0.14 * flogpr
      fquan = flogpr / fdenom
      fquan = fcent / (1.0 + fquan * fquan)

      !rr_kinf is stored in rr_f
      rr_f_34 = rr_f * pr/(1.0 + pr) * exp(fquan*dln10)
      call gibbs_lookup_all(T, 34, dG)
      rr_r_34 = rr_f_34 * exp(dG) * prt
      
      rr_f_34 = rr_f_34 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_CH2O)
!      rr_r_34 = rr_r_34 

!----------------------------------------------------------------------
!  35)  CH2O + H <=> H2 + HCO
      rr_f_35 = 2.3e+010 * exp(1.05e+000*vlntemp - 3.275e+003*ortc)
      call gibbs_lookup_all(T, 35, dG)
      rr_r_35 = rr_f_35 * exp(dG)
      
      rr_f_35 = rr_f_35 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_CH2O)
      rr_r_35 = rr_r_35 * cspl(:,:,:,NC_H2)

!----------------------------------------------------------------------
!  36)  CH2OH + H <=> CH2O + H2
      rr_f_36 = 2.0e+013
      call gibbs_lookup_all(T, 36, dG)
      rr_r_36 = rr_f_36 * exp(dG)
      
      rr_f_36 = rr_f_36 * cspl(:,:,:,NC_H) 
      rr_r_36 = rr_r_36 * cspl(:,:,:,NC_H2) * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------
!  37)  CH2OH + H <=> CH3 + OH
      rr_f_37 = 1.2e+013
      call gibbs_lookup_all(T, 37, dG)
      rr_r_37 = rr_f_37 * exp(dG)
      
      rr_f_37 = rr_f_37 * cspl(:,:,:,NC_H) 
      rr_r_37 = rr_r_37 * cspl(:,:,:,NC_OH) * cspl(:,:,:,NC_CH3)

!----------------------------------------------------------------------
!  38)  CH2OH + H <=> H2O + CH2(S)
      rr_f_38 = 6.0e+012
      call gibbs_lookup_all(T, 38, dG)
      rr_r_38 = rr_f_38 * exp(dG)
      
      rr_f_38 = rr_f_38 * cspl(:,:,:,NC_H) 
      rr_r_38 = rr_r_38 * cspl(:,:,:,NC_H2O)

!----------------------------------------------------------------------
!  43)  CH2 + OH <=> CH2O + H
      rr_f_43 = 2.0e+013
      call gibbs_lookup_all(T, 43, dG)
      rr_r_43 = rr_f_43 * exp(dG)
      
      rr_f_43 = rr_f_43 * cspl(:,:,:,NC_OH) 
      rr_r_43 = rr_r_43 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------
!  44)  CH2(S) + OH <=> CH2O + H
      rr_f_44 = 3.0e+013
      call gibbs_lookup_all(T, 44, dG)
      rr_r_44 = rr_f_44 * exp(dG)
      
      rr_f_44 = rr_f_44 * cspl(:,:,:,NC_OH) 
      rr_r_44 = rr_r_44 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------
!  45)  CH3 + OH <=> H2O + CH2
      rr_f_45 = 5.6e+007 * exp(1.6e+000*vlntemp - 5.42e+003*ortc)
      call gibbs_lookup_all(T, 45, dG)
      rr_r_45 = rr_f_45 * exp(dG)
      
      rr_f_45 = rr_f_45 * cspl(:,:,:,NC_OH) * cspl(:,:,:,NC_CH3)
      rr_r_45 = rr_r_45 * cspl(:,:,:,NC_H2O)

!----------------------------------------------------------------------
!  46)  CH3 + OH <=> H2O + CH2(S)
      rr_f_46 = 2.501e+013
      call gibbs_lookup_all(T, 46, dG)
      rr_r_46 = rr_f_46 * exp(dG)
      
      rr_f_46 = rr_f_46 * cspl(:,:,:,NC_OH) * cspl(:,:,:,NC_CH3)
      rr_r_46 = rr_r_46 * cspl(:,:,:,NC_H2O)

!----------------------------------------------------------------------
!  49)  HCO + OH <=> CO + H2O
      rr_f_49 = 5.0e+013
      call gibbs_lookup_all(T, 49, dG)
      rr_r_49 = rr_f_49 * exp(dG)
      
      rr_f_49 = rr_f_49 * cspl(:,:,:,NC_OH) 
      rr_r_49 = rr_r_49 * cspl(:,:,:,NC_H2O) * cspl(:,:,:,NC_CO)

!----------------------------------------------------------------------
!  50)  CH2O + OH <=> H2O + HCO
      rr_f_50 = 3.43e+009 * exp(1.18e+000*vlntemp + 4.47e+002*ortc)
      call gibbs_lookup_all(T, 50, dG)
      rr_r_50 = rr_f_50 * exp(dG)
      
      rr_f_50 = rr_f_50 * cspl(:,:,:,NC_OH) * cspl(:,:,:,NC_CH2O)
      rr_r_50 = rr_r_50 * cspl(:,:,:,NC_H2O)

!----------------------------------------------------------------------
!  51)  CH2OH + OH <=> CH2O + H2O
      rr_f_51 = 5.0e+012
      call gibbs_lookup_all(T, 51, dG)
      rr_r_51 = rr_f_51 * exp(dG)
      
      rr_f_51 = rr_f_51 * cspl(:,:,:,NC_OH) 
      rr_r_51 = rr_r_51 * cspl(:,:,:,NC_H2O) * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------
!  52)  CH2 + HO2 <=> CH2O + OH
      rr_f_52 = 2.0e+013
      call gibbs_lookup_all(T, 52, dG)
      rr_r_52 = rr_f_52 * exp(dG)
      
      rr_f_52 = rr_f_52 * cspl(:,:,:,NC_HO2) 
      rr_r_52 = rr_r_52 * cspl(:,:,:,NC_OH) * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------
!  55)  O2 + CH2 <=> HCO + OH
      rr_f_55 = 1.32e+013 * exp(-1.5e+003*ortc)
      call gibbs_lookup_all(T, 55, dG)
      rr_r_55 = rr_f_55 * exp(dG)
      
      rr_f_55 = rr_f_55 * cspl(:,:,:,NC_O2)
      rr_r_55 = rr_r_55 * cspl(:,:,:,NC_OH) 

!----------------------------------------------------------------------
!  56)  H2 + CH2 <=> CH3 + H
      rr_f_56 = 5.0e+005 * dimT * dimT * exp(-7.23e+003*ortc)
      call gibbs_lookup_all(T, 56, dG)
      rr_r_56 = rr_f_56 * exp(dG)
      
      rr_f_56 = rr_f_56 * cspl(:,:,:,NC_H2)
      rr_r_56 = rr_r_56 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_CH3)

!----------------------------------------------------------------------
!  57)  CH4 + CH2 <=> 2 CH3
      rr_f_57 = 2.46e+006 * dimT * dimT * exp(-8.27e+003*ortc)
      call gibbs_lookup_all(T, 57, dG)
      rr_r_57 = rr_f_57 * exp(dG)
      
      rr_f_57 = rr_f_57 * cspl(:,:,:,NC_CH4)
      rr_r_57 = rr_r_57 * cspl(:,:,:,NC_CH3) * cspl(:,:,:,NC_CH3)

!----------------------------------------------------------------------
!  58)  N2 + CH2(S) <=> N2 + CH2
      rr_f_58 = 1.5e+013 * exp(-6.0e+002*ortc)
      call gibbs_lookup_all(T, 58, dG)
      rr_r_58 = rr_f_58 * exp(dG)
      
      rr_f_58 = rr_f_58 * cspl(:,:,:,NC_N2)
      rr_r_58 = rr_r_58 * cspl(:,:,:,NC_N2)

!----------------------------------------------------------------------
!  59)  O2 + CH2(S) <=> CO + OH + H
      rr_f_59 = 2.8e+013
      call gibbs_lookup_all(T, 59, dG)
      rr_r_59 = rr_f_59 * exp(dG) * oprt
      
      rr_f_59 = rr_f_59 * cspl(:,:,:,NC_O2)
      rr_r_59 = rr_r_59 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_OH) * cspl(:,:,:,NC_CO)

!----------------------------------------------------------------------
!  60)  O2 + CH2(S) <=> H2O + CO
      rr_f_60 = 1.2e+013
      call gibbs_lookup_all(T, 60, dG)
      rr_r_60 = rr_f_60 * exp(dG)
      
      rr_f_60 = rr_f_60 * cspl(:,:,:,NC_O2)
      rr_r_60 = rr_r_60 * cspl(:,:,:,NC_CO) * cspl(:,:,:,NC_H2O)

!----------------------------------------------------------------------
!  61)  H2 + CH2(S) <=> H + CH3
      rr_f_61 = 7.0e+013
      call gibbs_lookup_all(T, 61, dG)
      rr_r_61 = rr_f_61 * exp(dG)
      
      rr_f_61 = rr_f_61 * cspl(:,:,:,NC_H2)
      rr_r_61 = rr_r_61 * cspl(:,:,:,NC_CH3) * cspl(:,:,:,NC_H)

!----------------------------------------------------------------------
!  62)  H2O + CH2(S) <=> H2O + CH2
      rr_f_62 = 3.0e+013
      call gibbs_lookup_all(T, 62, dG)
      rr_r_62 = rr_f_62 * exp(dG)
      
      rr_f_62 = rr_f_62 * cspl(:,:,:,NC_H2O)
      rr_r_62 = rr_r_62 * cspl(:,:,:,NC_H2O)

!----------------------------------------------------------------------
!  63)  CH4 + CH2(S) <=> 2 CH3
      rr_f_63 = 1.6e+013 * exp(5.7e+002*ortc)
      call gibbs_lookup_all(T, 63, dG)
      rr_r_63 = rr_f_63 * exp(dG)
      
      rr_f_63 = rr_f_63 * cspl(:,:,:,NC_CH4)
      rr_r_63 = rr_r_63 * cspl(:,:,:,NC_CH3) * cspl(:,:,:,NC_CH3)

!----------------------------------------------------------------------
!  64)  CO + CH2(S) <=> CO + CH2
      rr_f_64 = 9.0e+012
      call gibbs_lookup_all(T, 64, dG)
      rr_r_64 = rr_f_64 * exp(dG)
      
      rr_f_64 = rr_f_64 * cspl(:,:,:,NC_CO)
      rr_r_64 = rr_r_64 * cspl(:,:,:,NC_CO)

!----------------------------------------------------------------------
!  65)  CO2 + CH2(S) <=> CO2 + CH2
      rr_f_65 = 7.0e+012
      call gibbs_lookup_all(T, 65, dG)
      rr_r_65 = rr_f_65 * exp(dG)
      
      rr_f_65 = rr_f_65 * cspl(:,:,:,NC_CO2)
      rr_r_65 = rr_r_65 * cspl(:,:,:,NC_CO2)

!----------------------------------------------------------------------
!  66)  CO2 + CH2(S) <=> CH2O + CO
      rr_f_66 = 1.4e+013
      call gibbs_lookup_all(T, 66, dG)
      rr_r_66 = rr_f_66 * exp(dG)
      
      rr_f_66 = rr_f_66 * cspl(:,:,:,NC_CO2)
      rr_r_66 = rr_r_66 * cspl(:,:,:,NC_CO2) * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------
!  68)  HCO + CH3 <=> CO + CH4
      rr_f_68 = 2.648e+013
      call gibbs_lookup_all(T, 68, dG)
      rr_r_68 = rr_f_68 * exp(dG)
      
      rr_f_68 = rr_f_68 * cspl(:,:,:,NC_CH3) 
      rr_r_68 = rr_r_68 * cspl(:,:,:,NC_CH4) * cspl(:,:,:,NC_CO)

!----------------------------------------------------------------------
!  69)  CH2O + CH3 <=> CH4 + HCO
      rr_f_69 = 3.32e+003 * exp(2.81e+000*vlntemp - 5.86e+003*ortc)
      call gibbs_lookup_all(T, 69, dG)
      rr_r_69 = rr_f_69 * exp(dG)
      
      rr_f_69 = rr_f_69 * cspl(:,:,:,NC_CH3) * cspl(:,:,:,NC_CH2O)
      rr_r_69 = rr_r_69 * cspl(:,:,:,NC_CH4)

!----------------------------------------------------------------------
!  70)  H2O + HCO <=> H2O + CO + H
      rr_f_70 = 2.244e+018 * otc * exp(-1.7e+004*ortc)
      call gibbs_lookup_all(T, 70, dG)
      rr_r_70 = rr_f_70 * exp(dG) * oprt
      
      rr_f_70 = rr_f_70 * cspl(:,:,:,NC_H2O)
      rr_r_70 = rr_r_70 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_CO) * cspl(:,:,:,NC_H2O)

!----------------------------------------------------------------------
!  71)  HCO + M <=> CO + H + M 
      rr_f_71 = 1.87e+017 * otc * exp(-1.7e+004*ortc)
      rr_f_71 = rr_f_71 * thbctemp8
      call gibbs_lookup_all(T, 71, dG)
      rr_r_71 = rr_f_71 * exp(dG) * oprt
      
!      rr_f_71 = rr_f_71 
      rr_r_71 = rr_r_71 * cspl(:,:,:,NC_H) * cspl(:,:,:,NC_CO)

!----------------------------------------------------------------------
!  72)  O2 + HCO <=> CO + HO2
      rr_f_72 = 7.6e+012 * exp(-4.0e+002*ortc)
      call gibbs_lookup_all(T, 72, dG)
      rr_r_72 = rr_f_72 * exp(dG)
      
      rr_f_72 = rr_f_72 * cspl(:,:,:,NC_O2)
      rr_r_72 = rr_r_72 * cspl(:,:,:,NC_HO2) * cspl(:,:,:,NC_CO)

!----------------------------------------------------------------------
!  73)  O2 + CH2OH <=> CH2O + HO2
      rr_f_73 = 1.8e+013 * exp(-9.0e+002*ortc)
      call gibbs_lookup_all(T, 73, dG)
      rr_r_73 = rr_f_73 * exp(dG)

      rr_f_73 = rr_f_73 * cspl(:,:,:,NC_O2)
      rr_r_73 = rr_r_73 * cspl(:,:,:,NC_HO2) * cspl(:,:,:,NC_CH2O)

!----------------------------------------------------------------------


!Compute QSS concentrations
! Cut and paste code from TFL's ckwyp

!
!	 solving QSS species concentration
!	-------------------------------------------
!     CH2
      ABV1 = rr_r_29 + rr_r_43 + rr_f_45 &
                + rr_r_52 + rr_r_56 + rr_r_57
      DEN1 =  rr_f_05 +  rr_f_29 +  rr_f_43 &
                +  rr_r_45 +  rr_f_52 +  rr_f_55 &
                +  rr_f_56 +  rr_f_57 +  rr_r_58 &
                +  rr_r_62 +  rr_r_64 +  rr_r_65
!
!     CH2(S)
      ABV2 = rr_r_06 + rr_r_44 + rr_f_46 &
                + rr_r_59 + rr_r_60 + rr_r_61 &
                + rr_r_63 + rr_r_66
      DEN2 =  rr_f_06 +  rr_r_38 +  rr_f_44 &
                +  rr_r_46 +  rr_f_58 +  rr_f_59 &
                +  rr_f_60 +  rr_f_61 +  rr_f_62 &
                +  rr_f_63 +  rr_f_64 +  rr_f_65 &
                +  rr_f_66
!
!     HCO
      ABV3 = rr_r_10 + rr_r_11 + rr_f_12 &
                + rr_f_15 + rr_r_32 + rr_r_33 &
                + rr_f_35 + rr_r_49 + rr_f_50 &
                + rr_r_68 + rr_f_69 + rr_r_70 &
                + rr_r_71 + rr_r_72
      DEN3 =  rr_r_05 +  rr_f_10 +  rr_f_11 &
                +  rr_r_12 +  rr_r_15 +  rr_f_32 &
                +  rr_f_33 +  rr_r_35 +  rr_f_49 &
                +  rr_r_50 +  rr_r_55 +  rr_f_68 &
                +  rr_r_69 +  rr_f_70 +  rr_f_71 &
                +  rr_f_72
!
!     CH2OH
      ABV4 = rr_r_13 + rr_f_34 + rr_r_36 &
                + rr_r_37 + rr_r_51 + rr_r_73
      DEN4 =  rr_f_13 +  rr_r_34 +  rr_f_36 &
                +  rr_f_37 +  rr_f_38 +  rr_f_51 &
                +  rr_f_73
!
	F1 = ABV1/DEN1
	A12 = (rr_f_58+rr_f_62+rr_f_64+rr_f_65)/DEN1
	A13 = (rr_r_05+rr_r_55)/DEN1
!
	F2 = ABV2/DEN2
	A21 = (rr_r_58+rr_r_62+rr_r_64+rr_r_65)/DEN2
	A24 = rr_f_38/DEN2
!
	F3 = ABV3/DEN3
	A31 = (rr_f_05+rr_f_55)/DEN3
!
	F4 = ABV4/DEN4
	A42 = rr_r_38/DEN4
!
	TEMP = 1 - A13*A31
	FF1 = (F1 + A13*F3)/TEMP
	AA12 = A12/TEMP
!
	TEMP = 1 - A24*A42
	FF2 = (F2 + A24*F4)/TEMP
	AA21 = A21/TEMP
!
	cspl(:,:,:,NC_CH2) = (FF1 + AA12*FF2)/(1 - AA12*AA21)
	cspl(:,:,:,NC_CH2S) = FF2 + AA21*cspl(:,:,:,NC_CH2)
	cspl(:,:,:,NC_HCO) = F3 + A31*cspl(:,:,:,NC_CH2)
	cspl(:,:,:,NC_CH2OH) = F4 + A42*cspl(:,:,:,NC_CH2S)
!
!	---------------------------------------------

! update rates of reactions involving QSS species. 

      rr_f_05 = rr_f_05 * cspl(:,:,:,NC_CH2)
      rr_r_05 = rr_r_05 * cspl(:,:,:,NC_HCO)
 
      rr_f_06 = rr_f_06 * cspl(:,:,:,NC_CH2S)
 
      rr_f_10 = rr_f_10 * cspl(:,:,:,NC_HCO)
 
      rr_f_11 = rr_f_11 * cspl(:,:,:,NC_HCO)

      rr_r_12 = rr_r_12 * cspl(:,:,:,NC_HCO)
 
      rr_f_13 = rr_f_13 * cspl(:,:,:,NC_CH2OH)

      rr_r_15 = rr_r_15 * cspl(:,:,:,NC_HCO)
 
      rr_f_29 = rr_f_29 * cspl(:,:,:,NC_CH2)
 
      rr_f_32 = rr_f_32 * cspl(:,:,:,NC_HCO)
 
      rr_f_33 = rr_f_33 * cspl(:,:,:,NC_HCO)
 
      rr_r_34 = rr_r_34 * cspl(:,:,:,NC_CH2OH)
 
      rr_r_35 = rr_r_35 * cspl(:,:,:,NC_HCO)
 
      rr_f_36 = rr_f_36 * cspl(:,:,:,NC_CH2OH)
 
      rr_f_37 = rr_f_37 * cspl(:,:,:,NC_CH2OH)
 
      rr_f_38 = rr_f_38 * cspl(:,:,:,NC_CH2OH)
      rr_r_38 = rr_r_38 * cspl(:,:,:,NC_CH2S)
 
      rr_f_43 = rr_f_43 * cspl(:,:,:,NC_CH2)
 
      rr_f_44 = rr_f_44 * cspl(:,:,:,NC_CH2S)
 
      rr_r_45 = rr_r_45 * cspl(:,:,:,NC_CH2)
 
      rr_r_46 = rr_r_46 * cspl(:,:,:,NC_CH2S)
 
      rr_f_49 = rr_f_49 * cspl(:,:,:,NC_HCO)
 
      rr_r_50 = rr_r_50 * cspl(:,:,:,NC_HCO)
 
      rr_f_51 = rr_f_51 * cspl(:,:,:,NC_CH2OH)
 
      rr_f_52 = rr_f_52 * cspl(:,:,:,NC_CH2)
 
      rr_f_55 = rr_f_55 * cspl(:,:,:,NC_CH2)
      rr_r_55 = rr_r_55 * cspl(:,:,:,NC_HCO)
 
      rr_f_56 = rr_f_56 * cspl(:,:,:,NC_CH2)
 
      rr_f_57 = rr_f_57 * cspl(:,:,:,NC_CH2)
 
      rr_f_58 = rr_f_58 * cspl(:,:,:,NC_CH2S)
      rr_r_58 = rr_r_58 * cspl(:,:,:,NC_CH2)
 
      rr_f_59 = rr_f_59 * cspl(:,:,:,NC_CH2S)
 
      rr_f_60 = rr_f_60 * cspl(:,:,:,NC_CH2S)
 
      rr_f_61 = rr_f_61 * cspl(:,:,:,NC_CH2S)
 
      rr_f_62 = rr_f_62 * cspl(:,:,:,NC_CH2S)
      rr_r_62 = rr_r_62 * cspl(:,:,:,NC_CH2)
 
      rr_f_63 = rr_f_63 * cspl(:,:,:,NC_CH2S)
 
      rr_f_64 = rr_f_64 * cspl(:,:,:,NC_CH2S)
      rr_r_64 = rr_r_64 * cspl(:,:,:,NC_CH2)
 
      rr_f_65 = rr_f_65 * cspl(:,:,:,NC_CH2S)
      rr_r_65 = rr_r_65 * cspl(:,:,:,NC_CH2)
 
      rr_f_66 = rr_f_66 * cspl(:,:,:,NC_CH2S)
 
      rr_f_68 = rr_f_68 * cspl(:,:,:,NC_HCO)
 
      rr_r_69 = rr_r_69 * cspl(:,:,:,NC_HCO)
 
      rr_f_70 = rr_f_70 * cspl(:,:,:,NC_HCO)
 
      rr_f_71 = rr_f_71 * cspl(:,:,:,NC_HCO)
 
      rr_f_72 = rr_f_72 * cspl(:,:,:,NC_HCO)
 
      rr_f_73 = rr_f_73 * cspl(:,:,:,NC_CH2OH)

! Zero the Wdot vector
      wdot(:,:,:,:) = 0.0

! Now calc the reaction rates and add it to wdot the usual way.
! If precomputed, use it. 

!----------------------------------------------------
! ... loop over all rxns to evaluate rate-of-progress

!----------------------------------------------------------------------
!   1)  2 O + M <=> O2 + M 
      rr_f = 1.2e+017 * otc
      call gibbs_lookup_all(T, 01, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_O)*cspl(:,:,:,NC_O) - rr_r*cspl(:,:,:,NC_O2)
      ropl = ropl * thbctemp2

      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - 2.0 * ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl
      
!----------------------------------------------------------------------
!   2)  H + O + M <=> OH + M 
      rr_f = 5.0e+017 * otc
      call gibbs_lookup_all(T, 02, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_O) - rr_r*cspl(:,:,:,NC_OH)
      ropl = ropl * thbctemp3
      
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
 

!----------------------------------------------------------------------
!   3)  H2 + O <=> OH + H
      rr_f = 5.0e+004 * exp(2.67e+000*vlntemp - 6.29e+003*ortc)
      call gibbs_lookup_all(T, 03, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_H2)*cspl(:,:,:,NC_O)   & 
           - rr_r*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_H)

      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
 
!----------------------------------------------------------------------
!   4)  HO2 + O <=> O2 + OH
      rr_f = 2.0e+013
      call gibbs_lookup_all(T, 04, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_HO2)*cspl(:,:,:,NC_O)   &
           - rr_r*cspl(:,:,:,NC_O2) *cspl(:,:,:,NC_OH)
      
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl
 
!----------------------------------------------------------------------
!   5)  CH2 + O <=> HCO + H
!     reaction rates have been calculated already
      ropl = rr_f_05 - rr_r_05

      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) + ropl
      
!----------------------------------------------------------------------
!   6)  CH2(S) + O <=> CO + H2
!     reaction rates have been calculated already
      ropl = rr_f_06 - rr_r_06

      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!   7)  CH3 + O <=> CH2O + H
      rr_f = 8.43e+013
      call gibbs_lookup_all(T, 07, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_CH3)*cspl(:,:,:,NC_O) &
           - rr_r*cspl(:,:,:,NC_CH2O)*cspl(:,:,:,NC_H)
 
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
 
      
!----------------------------------------------------------------------
!   8)  CH4 + O <=> CH3 + OH
      rr_f = 1.02e+009 * exp(1.5e+000*vlntemp - 8.6e+003*ortc)
      call gibbs_lookup_all(T, 08, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_CH4)*cspl(:,:,:,NC_O) &
           - rr_r*cspl(:,:,:,NC_CH3)*cspl(:,:,:,NC_OH)
                   
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + ropl
      
!----------------------------------------------------------------------
!   9)  CO + O + M <=> CO2 + M 
      rr_f = 6.02e+014 * exp(-3.0e+003*ortc)
      call gibbs_lookup_all(T, 09, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_CO)*cspl(:,:,:,NC_O) - rr_r*cspl(:,:,:,NC_CO2)
      ropl = ropl * thbctemp4
                   
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl
      
!----------------------------------------------------------------------
!  10)  HCO + O <=> CO + OH
!     reaction rates have been calculated already
      ropl = rr_f_10 - rr_r_10

      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!  11)  HCO + O <=> CO2 + H
!     reaction rates have been calculated already
      ropl = rr_f_11 - rr_r_11

      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl
      
!----------------------------------------------------------------------
!  12)  CH2O + O <=> HCO + OH
!     reaction rates have been calculated already
      ropl = rr_f_12 - rr_r_12

      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) + ropl
      
!----------------------------------------------------------------------
!  13)  CH2OH + O <=> CH2O + OH
!     reaction rates have been calculated already
      ropl = rr_f_13 - rr_r_13
                   
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl
!QSS  wdot(:,:,:,NC_CH2OH) = wdot(:,:,:,NC_CH2OH) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  14)  CO + O2 <=> CO2 + O
      rr_f = 2.5e+012 * exp(-4.78e+004*ortc)
      call gibbs_lookup_all(T, 14, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_CO)*cspl(:,:,:,NC_O2) &
           - rr_r*cspl(:,:,:,NC_CO2)*cspl(:,:,:,NC_O)
                   
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) + ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl
      
!----------------------------------------------------------------------
!  15)  CH2O + O2 <=> HCO + HO2
!     reaction rates have been calculated already
      ropl = rr_f_15 - rr_r_15
                   
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) + ropl
      
!----------------------------------------------------------------------
!  16)  O2 + H + M <=> HO2 + M 
      rr_f = 2.8e+018 * exp(-8.6e-001*vlntemp)
      call gibbs_lookup_all(T, 16, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_H) - rr_r*cspl(:,:,:,NC_HO2)
      ropl = ropl * thbctemp5
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl
      
!----------------------------------------------------------------------
!  17)  2 O2 + H <=> O2 + HO2
      rr_f = 3.0e+020 * exp(-1.72e+000*vlntemp)
      call gibbs_lookup_all(T, 17, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_HO2)


      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - 2.0 * ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl
      
!----------------------------------------------------------------------
!  18)  H2O + O2 + H <=> H2O + HO2
      rr_f = 9.38e+018 * exp(-7.6e-001*vlntemp)
      call gibbs_lookup_all(T, 18, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_HO2)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  19)  N2 + O2 + H <=> N2 + HO2
      rr_f = 3.75e+020 * exp(-1.72e+000*vlntemp)
      call gibbs_lookup_all(T, 19, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_N2)*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_N2)*cspl(:,:,:,NC_HO2)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_N2) = wdot(:,:,:,NC_N2) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl
      wdot(:,:,:,NC_N2) = wdot(:,:,:,NC_N2) + ropl
      
!----------------------------------------------------------------------
!  20)  O2 + H <=> OH + O
      rr_f = 8.3e+013 * exp(-1.4413e+004*ortc)
      call gibbs_lookup_all(T, 20, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_O)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) + ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
                   
!----------------------------------------------------------------------
!  21)  2 H + M <=> H2 + M 
      rr_f = 1.0e+018 * otc
      call gibbs_lookup_all(T, 21, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_H) - rr_r*cspl(:,:,:,NC_H2)
      ropl = ropl * thbctemp6
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - 2.0 * ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
                   
!----------------------------------------------------------------------
!  22)  H2 + 2 H <=> 2 H2
      rr_f = 9.0e+016 * exp(-6.0e-001*vlntemp)
      call gibbs_lookup_all(T, 22, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_H2)*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_H2)*cspl(:,:,:,NC_H2)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - 2.0 * ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + 2.0 *ropl
                   
!----------------------------------------------------------------------
!  23)  H2O + 2 H <=> H2O + H2
      rr_f = 6.0e+019 * exp(-1.25e+000*vlntemp)
      call gibbs_lookup_all(T, 23, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_H2)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - 2.0 * ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
                   
!----------------------------------------------------------------------
!  24)  CO2 + 2 H <=> CO2 + H2
      rr_f = 5.5e+020 * otc * otc
      call gibbs_lookup_all(T, 24, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_CO2)*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_CO2)*cspl(:,:,:,NC_H2)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - 2.0 * ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl
      
!----------------------------------------------------------------------
!  25)  OH + H + M <=> H2O + M 
      rr_f = 2.2e+022 * otc * otc
      call gibbs_lookup_all(T, 25, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_H) - rr_r*cspl(:,:,:,NC_H2O)
      ropl = ropl * thbctemp7
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  26)  HO2 + H <=> H2O + O
      rr_f = 3.97e+012 * exp(-6.71e+002*ortc)
      call gibbs_lookup_all(T, 26, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_HO2)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_O)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  27)  HO2 + H <=> H2 + O2
      rr_f = 2.8e+013 * exp(-1.068e+003*ortc)
      call gibbs_lookup_all(T, 27, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_HO2)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_H2)*cspl(:,:,:,NC_O2)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
      
!----------------------------------------------------------------------
!  28)  HO2 + H <=> 2 OH
      rr_f = 1.34e+014 * exp(-6.35e+002*ortc)
      call gibbs_lookup_all(T, 28, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_HO2)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_OH)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + 2.0 *ropl
      
!----------------------------------------------------------------------
!  29)  CH2 + H (+M) <=> CH3 (+M) 
!     reaction rates have been calculated already
      ropl = rr_f_29 - rr_r_29

      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + ropl
      
!----------------------------------------------------------------------
!  30)  CH3 + H (+M) <=> CH4 (+M) 
      !rr_k0 is stored in rr_r
      rr_r = 2.477e+033 * exp(-4.76e+000*vlntemp - 2.44e+003*ortc)
      !rr_kinf is stored in rr_f
      rr_f = 1.27e+016 * exp(-6.3e-001*vlntemp - 3.83e+002*ortc)

      pr = rr_r / rr_f * thbctemp3

      fcent = log10(2.17e-001 * exp(-1.351351351351351e-002 * dimT) +     &
        7.83e-001 * exp(-3.400204012240735e-004 * dimT) + exp(-6.964e+003 &
         * otc))
      flogpr = log10(pr) - 0.4 - 0.67 * fcent
      fdenom = 0.75 - 1.27 * fcent - 0.14 * flogpr
      fquan = flogpr / fdenom
      fquan = fcent / (1.0 + fquan * fquan)

      !rr_kinf is stored in rr_f
      rr_f = rr_f * pr/(1.0 + pr) * exp(fquan*dln10)
      call gibbs_lookup_all(T, 30, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_CH3)*cspl(:,:,:,NC_H) - rr_r*cspl(:,:,:,NC_CH4)
                   
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) - ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) + ropl
      
!----------------------------------------------------------------------
!  31)  CH4 + H <=> H2 + CH3
      rr_f = 6.6e+008 * exp(1.62e+000*vlntemp - 1.084e+004*ortc)
      call gibbs_lookup_all(T, 31, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_CH4)*cspl(:,:,:,NC_H) &
           - rr_r*cspl(:,:,:,NC_H2)*cspl(:,:,:,NC_CH3)
 
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
      
!----------------------------------------------------------------------
!  32)  HCO + H (+M) <=> CH2O (+M) 
!     reaction rates have been calculated already
      ropl = rr_f_32 - rr_r_32

      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  33)  HCO + H <=> CO + H2
!     reaction rates have been calculated already
      ropl = rr_f_33 - rr_r_33

 
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!  34)  CH2O + H (+M) <=> CH2OH (+M) 
!     reaction rates have been calculated already
      ropl = rr_f_34 - rr_r_34

      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) - ropl
!QSS  wdot(:,:,:,NC_CH2OH) = wdot(:,:,:,NC_CH2OH) + ropl
      
!----------------------------------------------------------------------
!  35)  CH2O + H <=> H2 + HCO
!     reaction rates have been calculated already
      ropl = rr_f_35 - rr_r_35
 
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) + ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
      
!----------------------------------------------------------------------
!  36)  CH2OH + H <=> CH2O + H2
!     reaction rates have been calculated already
      ropl = rr_f_36 - rr_r_36
 
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
!QSS  wdot(:,:,:,NC_CH2OH) = wdot(:,:,:,NC_CH2OH) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  37)  CH2OH + H <=> CH3 + OH
!     reaction rates have been calculated already
      ropl = rr_f_37 - rr_r_37
 
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
!QSS  wdot(:,:,:,NC_CH2OH) = wdot(:,:,:,NC_CH2OH) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + ropl
      
!----------------------------------------------------------------------
!  38)  CH2OH + H <=> H2O + CH2(S)
!     reaction rates have been calculated already
      ropl = rr_f_38 - rr_r_38
 
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl
!QSS  wdot(:,:,:,NC_CH2OH) = wdot(:,:,:,NC_CH2OH) - ropl
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  39)  CO + H2 (+M) <=> CH2O (+M) 
      !rr_k0 is stored in rr_r
      rr_r = 5.07e+027 * exp(-3.42e+000*vlntemp - 8.435e+004*ortc)
      !rr_kinf is stored in rr_f
      rr_f = 4.3e+007 * exp(1.5e+000*vlntemp - 7.96e+004*ortc)

      pr = rr_r / rr_f * thbctemp3

      fcent = log10(6.799999999999995e-002 * exp(-5.076142131979695e-003 &
         * dimT) + 9.320000000000001e-001 * exp(-6.493506493506494e-004 &
         * dimT) + exp(-1.03e+004 * otc))
      flogpr = log10(pr) - 0.4 - 0.67 * fcent
      fdenom = 0.75 - 1.27 * fcent - 0.14 * flogpr
      fquan = flogpr / fdenom
      fquan = fcent / (1.0 + fquan * fquan)

      !rr_kinf is stored in rr_f
      rr_f = rr_f * pr/(1.0 + pr) * exp(fquan*dln10)
      call gibbs_lookup_all(T, 39, dG)
      rr_r = rr_f * exp(dG) * prt
      ropl = rr_f*cspl(:,:,:,NC_CO)*cspl(:,:,:,NC_H2) - rr_r*cspl(:,:,:,NC_CH2O)
 
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  40)  H2 + OH <=> H2O + H
      rr_f = 2.16e+008 * exp(1.51e+000*vlntemp - 3.43e+003*ortc)
      call gibbs_lookup_all(T, 40, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_H2)*cspl(:,:,:,NC_OH) &
           - rr_r*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_H)
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  41)  2 OH <=> H2O + O
      rr_f = 3.57e+004 * exp(2.4e+000*vlntemp + 2.11e+003*ortc)
      call gibbs_lookup_all(T, 41, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_OH) &
           - rr_r*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_O)
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - 2.0 * ropl
      wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  42)  HO2 + OH <=> H2O + O2
      rr_f = 2.9e+013 * exp(5.0e+002*ortc)
      call gibbs_lookup_all(T, 42, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_HO2)*cspl(:,:,:,NC_OH) &
           - rr_r*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_O2)
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  43)  CH2 + OH <=> CH2O + H
!     reaction rates have been calculated already
      ropl = rr_f_43 - rr_r_43
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  44)  CH2(S) + OH <=> CH2O + H
!     reaction rates have been calculated already
      ropl = rr_f_44 - rr_r_44
                   
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  45)  CH3 + OH <=> H2O + CH2
!     reaction rates have been calculated already
      ropl = rr_f_45 - rr_r_45
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  46)  CH3 + OH <=> H2O + CH2(S)
!     reaction rates have been calculated already
      ropl = rr_f_46 - rr_r_46
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) - ropl
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  47)  CH4 + OH <=> H2O + CH3
      rr_f = 1.0e+008 * exp(1.6e+000*vlntemp - 3.12e+003*ortc)
      call gibbs_lookup_all(T, 47, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_CH4)*cspl(:,:,:,NC_OH) &
           - rr_r*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_CH3)
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  48)  CO + OH <=> CO2 + H
      rr_f = 4.76e+007 * exp(1.228e+000*vlntemp - 7.0e+001*ortc)
      call gibbs_lookup_all(T, 48, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_CO)*cspl(:,:,:,NC_OH) &
           - rr_r*cspl(:,:,:,NC_CO2)*cspl(:,:,:,NC_H)
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl
      
!----------------------------------------------------------------------
!  49)  HCO + OH <=> CO + H2O
!     reaction rates have been calculated already
      ropl = rr_f_49 - rr_r_49
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!  50)  CH2O + OH <=> H2O + HCO
!     reaction rates have been calculated already
      ropl = rr_f_50 - rr_r_50
                   
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  51)  CH2OH + OH <=> CH2O + H2O
!     reaction rates have been calculated already
      ropl = rr_f_51 - rr_r_51
 
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl
!QSS  wdot(:,:,:,NC_CH2OH) = wdot(:,:,:,NC_CH2OH) - ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  52)  CH2 + HO2 <=> CH2O + OH
!     reaction rates have been calculated already
      ropl = rr_f_52 - rr_r_52
 
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  53)  CH3 + HO2 <=> CH4 + O2
      rr_f = 1.0e+012
      call gibbs_lookup_all(T, 53, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_CH3)*cspl(:,:,:,NC_HO2) &
           - rr_r*cspl(:,:,:,NC_CH4)*cspl(:,:,:,NC_O2)
 
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) + ropl
      
!----------------------------------------------------------------------
!  54)  CO + HO2 <=> CO2 + OH
      rr_f = 1.5e+014 * exp(-2.36e+004*ortc)
      call gibbs_lookup_all(T, 54, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_CO)*cspl(:,:,:,NC_HO2) &
           - rr_r*cspl(:,:,:,NC_CO2)*cspl(:,:,:,NC_OH)
 
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl
      
!----------------------------------------------------------------------
!  55)  O2 + CH2 <=> HCO + OH
!     reaction rates have been calculated already
      ropl = rr_f_55 - rr_r_55
 
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) + ropl
      
!----------------------------------------------------------------------
!  56)  H2 + CH2 <=> CH3 + H
!     reaction rates have been calculated already
      ropl = rr_f_56 - rr_r_56
                   
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + ropl
 
      
!----------------------------------------------------------------------
!  57)  CH4 + CH2 <=> 2 CH3
!     reaction rates have been calculated already
      ropl = rr_f_57 - rr_r_57

!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) - ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + 2.0 *ropl
      
!----------------------------------------------------------------------
!  58)  N2 + CH2(S) <=> N2 + CH2
!     reaction rates have been calculated already
      ropl = rr_f_58 - rr_r_58
 
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_N2) = wdot(:,:,:,NC_N2) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) + ropl
      wdot(:,:,:,NC_N2) = wdot(:,:,:,NC_N2) + ropl
      
!----------------------------------------------------------------------
!  59)  O2 + CH2(S) <=> CO + OH + H
!     reaction rates have been calculated already
      ropl = rr_f_59 - rr_r_59
 
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!  60)  O2 + CH2(S) <=> H2O + CO
!     reaction rates have been calculated already
      ropl = rr_f_60 - rr_r_60
 
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  61)  H2 + CH2(S) <=> H + CH3
!     reaction rates have been calculated already
      ropl = rr_f_61 - rr_r_61
 
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      
!----------------------------------------------------------------------
!  62)  H2O + CH2(S) <=> H2O + CH2
!     reaction rates have been calculated already
      ropl = rr_f_62 - rr_r_62
 
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  63)  CH4 + CH2(S) <=> 2 CH3
!     reaction rates have been calculated already
      ropl = rr_f_63 - rr_r_63
                   
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) - ropl
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) + 2.0 *ropl
      
!----------------------------------------------------------------------
!  64)  CO + CH2(S) <=> CO + CH2
!     reaction rates have been calculated already
      ropl = rr_f_64 - rr_r_64
                   
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!  65)  CO2 + CH2(S) <=> CO2 + CH2
!     reaction rates have been calculated already
      ropl = rr_f_65 - rr_r_65
                   
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) - ropl
!QSS  wdot(:,:,:,NC_CH2) = wdot(:,:,:,NC_CH2) + ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl
      
!----------------------------------------------------------------------
!  66)  CO2 + CH2(S) <=> CH2O + CO
!     reaction rates have been calculated already
      ropl = rr_f_66 - rr_r_66
                   
!QSS  wdot(:,:,:,NC_CH2S) = wdot(:,:,:,NC_CH2S) - ropl
      wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) - ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  67)  O2 + CH3 <=> CH2O + OH
      rr_f = 3.6e+010 * exp(-8.94e+003*ortc)
      call gibbs_lookup_all(T, 67, dG)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_CH3) &
           - rr_r*cspl(:,:,:,NC_CH2O)*cspl(:,:,:,NC_OH)
                   
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl
      
!----------------------------------------------------------------------
!  68)  HCO + CH3 <=> CO + CH4
!     reaction rates have been calculated already
      ropl = rr_f_68 - rr_r_68
                   
      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!  69)  CH2O + CH3 <=> CH4 + HCO
!     reaction rates have been calculated already
      ropl = rr_f_69 - rr_r_69

      wdot(:,:,:,NC_CH3) = wdot(:,:,:,NC_CH3) - ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) - ropl
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) + ropl
      wdot(:,:,:,NC_CH4) = wdot(:,:,:,NC_CH4) + ropl
      
!----------------------------------------------------------------------
!  70)  H2O + HCO <=> H2O + CO + H
!     reaction rates have been calculated already
      ropl = rr_f_70 - rr_r_70
                   
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl
      
!----------------------------------------------------------------------
!  71)  HCO + M <=> CO + H + M 
!     reaction rates have been calculated already
      ropl = rr_f_71 - rr_r_71
                   
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!  72)  O2 + HCO <=> CO + HO2
!     reaction rates have been calculated already
      ropl = rr_f_72 - rr_r_72
                   
!QSS  wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl
      wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl
      
!----------------------------------------------------------------------
!  73)  O2 + CH2OH <=> CH2O + HO2
!     reaction rates have been calculated already
      ropl = rr_f_73 - rr_r_73
 
!QSS  wdot(:,:,:,NC_CH2OH) = wdot(:,:,:,NC_CH2OH) - ropl
      wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl
      wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl
      wdot(:,:,:,NC_CH2O) = wdot(:,:,:,NC_CH2O) + ropl


!----------------------------------------------------------------------


return
end subroutine vecrates

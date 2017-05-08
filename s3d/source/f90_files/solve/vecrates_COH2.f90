#include "globalDefines.h"
!$Id: vecrates.f90,v 1.1.4.1 2006/04/04 01:27:26 rsankar Exp $
!----------------------------------------------------------------------
! New routine by Ramanan Sankaran (rsankar@sandia.gov)
! Vector getrates for the Cray X1 platform
! May be helpful on Seaborg IBM Sp2. 
! Partly auto-generated using autogr.pl, partly hand coded
! Dryer CO/H2 Mechanism
! H2O2 and all its rxns are ignored.
! Comments
!    All pointers are turned into real arrays
!    Cray compiler does not like pointers.
!----------------------------------------------------------------------

subroutine vecrates(P, T, Y, RHO, WDOT)
use chemkin_m, only: molwt_c, n_species
use param_m, only: nx, ny, nz
use thermchem_m, only: avmolwt
use reference_m, only: t_ref, rho_ref
use gibbs_rxn_table_m, only: gibbs_lookup_all
implicit none

real, dimension(nx, ny, nz), intent(in):: P, T, RHO
real, dimension(nx, ny, nz, n_species), intent(in):: Y
real, dimension(nx, ny, nz, n_species), intent(out):: WDOT

!local declarations
real, dimension(nx, ny, nz, n_species) :: cspl  !species molar concentrations
real, dimension(nx,ny,nz) :: vlntemp, ortc, dimT
real, dimension(nx,ny,nz) :: flogpr, fdenom, fquan
real, dimension(nx,ny,nz) :: thbctemp2, thbctemp3, thbctemp4
real, dimension(nx,ny,nz) :: rr_f, rr_r, ropl, pr, dG

real prtcoeff, oprtcoeff
real logfcent

integer i, j, k, n

!Universal gas constant in erg/(mol K)
real, parameter:: Ru =8.31451e+07

!Universal gas constant in cal/(mol K)
real, parameter:: Ruc=1.9872155832

! atm. pressure; dyne/cm^2
real, parameter:: PA=1.013250e+06

integer, parameter:: NC_H2     =  1
integer, parameter:: NC_O2     =  2
integer, parameter:: NC_O      =  3
integer, parameter:: NC_OH     =  4
integer, parameter:: NC_H2O    =  5
integer, parameter:: NC_H      =  6
integer, parameter:: NC_HO2    =  7
integer, parameter:: NC_CO     =  8
integer, parameter:: NC_CO2    =  9
integer, parameter:: NC_HCO    = 10
integer, parameter:: NC_N2     = 11

!1e-6 is to convert to cm^3 from m^3
do n = 1, n_species
  cspl(:,:,:,n) = RHO(:,:,:)*Y(:,:,:,n)*molwt_c(n)*1e-6*rho_ref 
end do

dimT(:,:,:) = t_ref*T(:,:,:)
vlntemp(:,:,:) = log(t_ref) + log(T(:,:,:))
ortc(:,:,:) = (1.0/Ruc)*(1.0/t_ref)/T(:,:,:)

prtcoeff = PA*Ruc/RU
oprtcoeff = t_ref*RU/PA

thbctemp2 =  &
   rho(:,:,:)*avmolwt(:,:,:)*1e-6*rho_ref  &
   + 1.5e+000*cspl(:,:,:,NC_H2) + 1.1e+001*cspl(:,:,:,NC_H2O)   &
   + 0.9*cspl(:,:,:,NC_CO) + 2.8e+000*cspl(:,:,:,NC_CO2)

thbctemp3 =  &
   rho(:,:,:)*avmolwt(:,:,:)*1e-6*rho_ref & 
   + cspl(:,:,:,NC_H2) - 2.2e-001*cspl(:,:,:,NC_O2)  & 
   + 1.0e+001 *cspl(:,:,:,NC_H2O) + 0.9*cspl(:,:,:,NC_CO)   &
   + 2.8e+000* cspl(:,:,:,NC_CO2)

thbctemp4 =  &
   rho(:,:,:)*avmolwt(:,:,:)*1e-6*rho_ref & 
   + 1.5e+000*cspl(:,:,:,NC_H2) + 5.0e+000*cspl(:,:,:,NC_H2O)  &
   + 0.9*cspl(:,:,:,NC_CO) + 2.8e+000*cspl(:,:,:,NC_CO2)

wdot(:,:,:,:) = 0.0

! POINTERS ARE NOT USED. CRAY COMPILER DOES NOT LIKE POINTERS.
! Note the pointer assignments here. 
! It is important to remember that different names 
! refer to the same memory location

! ropl is the same memory location as rr_f. 
! The name ropl is used in order to be able to cut and paste code 
! from the old getrates
!ropl => rr_f

!dG => rr_r

!pr => rr_k0

!----------------------------------------------------------------------
!   1. H+O2=O+OH                                     3.55E+15   -0.4    16599.0

call gibbs_lookup_all(T, 1, dG)

      rr_f = 3.547e+015*exp(-4.06e-001*vlntemp - 1.6599e+004*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_H) - rr_r*cspl(:,:,:,NC_OH)  *cspl(:,:,:,NC_O)

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl(:,:,:)
wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) + ropl(:,:,:)
wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl(:,:,:)

!----------------------------------------------------------------------
!   2. O+H2=H+OH                                     5.08E+04    2.7     6290.0

call gibbs_lookup_all(T, 2, dG)

      rr_f = 5.08e+004 * exp(2.67e+000*vlntemp - 6.29e+003*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_H2)*cspl(:,:,:,NC_O) - rr_r*cspl(:,:,:,NC_OH) *cspl(:,:,:,NC_H)

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl(:,:,:)
wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl(:,:,:)
wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl(:,:,:)


!----------------------------------------------------------------------
!   3. H2+OH=H2O+H                                   2.16E+08    1.5     3430.0

call gibbs_lookup_all(T, 3, dG)

      rr_f = 2.16e+008 * exp(1.51e+000*vlntemp - 3.43e+003*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_H2) - rr_r*cspl(:,:,:,NC_H) *cspl(:,:,:,NC_H2O)

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl(:,:,:)
wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl(:,:,:)
wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl(:,:,:)

!----------------------------------------------------------------------
!   4. O+H2O=OH+OH                                   2.97E+06    2.0    13400.0

call gibbs_lookup_all(T, 4, dG)

      rr_f = 2.97e+006 * exp(2.02e+000*vlntemp - 1.34e+004*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_H2O)*cspl(:,:,:,NC_O) - rr_r*cspl(:,:,:,NC_OH) *cspl(:,:,:,NC_OH)

wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl(:,:,:)
wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) - ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) +2.0 * ropl(:,:,:)


!----------------------------------------------------------------------
!   5. H2+M=H+H+M                                    4.58E+19   -1.4   104380.0

call gibbs_lookup_all(T, 5, dG)


      rr_f = 4.577e+019 * exp(-1.4e+000*vlntemp - 1.0438e+005*ortc)
      rr_r = rr_f * exp(dG) * oprtcoeff * T
      ropl = rr_f*cspl(:,:,:,NC_H2) - rr_r*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_H)
      ropl = ropl * thbctemp2
wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) +2.0 * ropl(:,:,:)
wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) - ropl(:,:,:)


!----------------------------------------------------------------------
!   6. O+O+M=O2+M                                    6.16E+15   -0.5        0.0

call gibbs_lookup_all(T, 6, dG)

      rr_f = 6.165e+015 * exp(-5.0e-001*vlntemp)
      rr_r = rr_f * exp(dG) * prtcoeff * ortc
      ropl = rr_f*cspl(:,:,:,NC_O)*cspl(:,:,:,NC_O) - rr_r*cspl(:,:,:,NC_O2)
      ropl = ropl * thbctemp2
wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) -2.0 * ropl(:,:,:)
wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl(:,:,:)


!----------------------------------------------------------------------
!   7. O+H+M=OH+M                                    4.71E+18   -1.0        0.0

call gibbs_lookup_all(T, 7, dG)

      rr_f = 4.714e+018 * ortc*Ruc
      rr_r = rr_f * exp(dG) * prtcoeff * ortc
      ropl = rr_f*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_O) - rr_r*cspl(:,:,:,NC_OH)
      ropl = ropl * thbctemp2
wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl(:,:,:)
wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl(:,:,:)



!----------------------------------------------------------------------
!   8. H+OH+M=H2O+M                                  3.80E+22   -2.0        0.0

call gibbs_lookup_all(T, 8, dG)

      rr_f = 3.8e+022 * ortc*Ruc * ortc*Ruc
      rr_r = rr_f * exp(dG) * prtcoeff * ortc
      ropl = rr_f*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_H) - rr_r*cspl(:,:,:,NC_H2O)
      ropl = ropl * thbctemp2
wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl(:,:,:)
wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl(:,:,:)


!----------------------------------------------------------------------
!   9. H+O2(+M)=HO2(+M)                              1.48E+12    0.6        0.0

call gibbs_lookup_all(T, 9, dG)

      !rr_k0 is stored in rr_r
      rr_r = 6.366e+020 * exp(-1.72e+000*vlntemp - 5.248e+002*ortc)
      !rr_kinf is stored in rr_f
      rr_f = 1.475e+012 * exp(6.0e-001*vlntemp)

      pr = rr_r / rr_f * thbctemp3

      !simplified troe. constant Fcent.
      logfcent = log10(0.8)

      flogpr = log10(pr) - 0.4 - 0.67 * logfcent
      fdenom = 0.75 - 1.27 * logfcent - 0.14 * flogpr
      fquan = flogpr / fdenom
      fquan = logfcent / (1.0 + fquan * fquan)

      !rr_kinf is stored in rr_f
      rr_f = rr_f * (pr/(1.0 + pr) )* exp(fquan*log(10.0))
      rr_r = rr_f * exp(dG) * prtcoeff * ortc
      ropl = rr_f*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_H) - rr_r*cspl(:,:,:,NC_HO2)
      
wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl(:,:,:)
wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl(:,:,:)
wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl(:,:,:)

!----------------------------------------------------------------------
!  10. HO2+H=H2+O2                                   1.66E+13    0.0      823.0

call gibbs_lookup_all(T, 10, dG)

      rr_f = 1.66e+013 * exp(-8.23e+002*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_HO2) - rr_r*cspl(:,:,:,NC_O2) *cspl(:,:,:,NC_H2)

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl(:,:,:)
wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl(:,:,:)
wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl(:,:,:)
wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl(:,:,:)


!----------------------------------------------------------------------
!  11. HO2+H=OH+OH                                   7.08E+13    0.0      295.0

call gibbs_lookup_all(T, 11, dG)

      rr_f = 7.079e+013 * exp(-2.95e+002*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_HO2) - rr_r*cspl(:,:,:,NC_OH) *cspl(:,:,:,NC_OH)

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl(:,:,:)
wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) +2.0 * ropl(:,:,:)


!----------------------------------------------------------------------
!  12. HO2+O=O2+OH                                   3.25E+13    0.0        0.0

call gibbs_lookup_all(T, 12, dG)

      rr_f = 3.25e+013
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_O)*cspl(:,:,:,NC_HO2) - rr_r*cspl(:,:,:,NC_OH) *cspl(:,:,:,NC_O2)
      

wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl(:,:,:)
wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl(:,:,:)
wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl(:,:,:)


!----------------------------------------------------------------------
!  13. HO2+OH=H2O+O2                                 2.89E+13    0.0     -497.0

call gibbs_lookup_all(T, 13, dG)


      rr_f = 2.89e+013 * exp(4.97e+002*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_HO2) - rr_r*cspl(:,:,:,NC_O2) *cspl(:,:,:,NC_H2O)
wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl(:,:,:)
wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) + ropl(:,:,:)
wdot(:,:,:,NC_H2O) = wdot(:,:,:,NC_H2O) + ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl(:,:,:)


!----------------------------------------------------------------------
!  14. CO+O(+M)=CO2(+M)                              1.80E+10    0.0     2384.0

call gibbs_lookup_all(T, 14, dG)

      !rr_k0 is stored in rr_r
      rr_r = 1.55e+024 * exp(-2.79e+000*vlntemp - 4.191e+003*ortc)
      !rr_kinf is stored in rr_f
      rr_f = 1.8e+010 * exp(-2.384e+003*ortc)
      pr = rr_r / rr_f * thbctemp2
      !rr_kinf is stored in rr_f
      rr_f = rr_f * pr/(1.0 + pr)
      rr_r = rr_f * exp(dG) * prtcoeff * ortc
      ropl = rr_f*cspl(:,:,:,NC_O)*cspl(:,:,:,NC_CO) - rr_r*cspl(:,:,:,NC_CO2)

wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl(:,:,:)
wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl(:,:,:)
wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl(:,:,:)


!----------------------------------------------------------------------
!  15. CO+O2=CO2+O                                   2.53E+12    0.0    47700.0


call gibbs_lookup_all(T, 15, dG)
     rr_f = 2.53e+012 * exp(-4.77e+004*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_CO) - rr_r*cspl(:,:,:,NC_O)*cspl(:,:,:,NC_CO2)

wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) + ropl(:,:,:)
wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl(:,:,:)
wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl(:,:,:)
wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl(:,:,:)


!----------------------------------------------------------------------
!  16. CO+HO2=CO2+OH                                 3.01E+13    0.0    23000.0


call gibbs_lookup_all(T, 16, dG)
     rr_f = 3.01e+013 * exp(-2.3e+004*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_HO2)*cspl(:,:,:,NC_CO) - rr_r*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_CO2)

wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) - ropl(:,:,:)
wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl(:,:,:)
wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) + ropl(:,:,:)


!----------------------------------------------------------------------
!  17. CO+OH=CO2+H                                   2.23E+05    1.9    -1158.7


call gibbs_lookup_all(T, 17, dG)
      rr_f = 2.229e+005 * exp(1.89e+000*vlntemp + 1.1587e+003*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_OH)*cspl(:,:,:,NC_CO) - rr_r*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_CO2)

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl(:,:,:)
wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl(:,:,:)
wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) - ropl(:,:,:)
wdot(:,:,:,NC_OH) = wdot(:,:,:,NC_OH) - ropl(:,:,:)


!----------------------------------------------------------------------
!  18. HCO+M=H+CO+M                                  4.75E+11    0.7    14874.0


call gibbs_lookup_all(T, 18, dG)
      rr_f = 4.7485e+011 * exp(6.59e-001*vlntemp - 1.4874e+004*ortc)
      rr_r = rr_f * exp(dG) * oprtcoeff * T
      ropl = rr_f*cspl(:,:,:,NC_HCO) - rr_r*cspl(:,:,:,NC_CO)*cspl(:,:,:,NC_H)
      ropl = ropl * thbctemp4

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl(:,:,:)
wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl(:,:,:)
wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl(:,:,:)


!----------------------------------------------------------------------
!  19. HCO+O2=CO+HO2                                 7.58E+12    0.0      410.0


call gibbs_lookup_all(T, 19, dG)
      rr_f = 7.58e+012 * exp(-4.1e+002*ortc)
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_O2)*cspl(:,:,:,NC_HCO) - rr_r*cspl(:,:,:,NC_HO2)*cspl(:,:,:,NC_CO)

wdot(:,:,:,NC_HO2) = wdot(:,:,:,NC_HO2) + ropl(:,:,:)
wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl(:,:,:)
wdot(:,:,:,NC_O2) = wdot(:,:,:,NC_O2) - ropl(:,:,:)
wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl(:,:,:)


!----------------------------------------------------------------------
!  20. HCO+H=CO+H2                                   7.23E+13    0.0        0.0


call gibbs_lookup_all(T, 20, dG)
      rr_f = 7.23e+013
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_HCO) - rr_r*cspl(:,:,:,NC_H2)*cspl(:,:,:,NC_CO)

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) - ropl(:,:,:)
wdot(:,:,:,NC_CO) = wdot(:,:,:,NC_CO) + ropl(:,:,:)
wdot(:,:,:,NC_H2) = wdot(:,:,:,NC_H2) + ropl(:,:,:)
wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl(:,:,:)


!----------------------------------------------------------------------
!  21. HCO+O=CO2+H                                   3.00E+13    0.0        0.0


call gibbs_lookup_all(T, 21, dG)
      rr_f = 3.0e+013
      rr_r = rr_f * exp(dG)
      ropl = rr_f*cspl(:,:,:,NC_O)*cspl(:,:,:,NC_HCO) - rr_r*cspl(:,:,:,NC_H)*cspl(:,:,:,NC_CO2)
      

wdot(:,:,:,NC_H) = wdot(:,:,:,NC_H) + ropl(:,:,:)
wdot(:,:,:,NC_O) = wdot(:,:,:,NC_O) - ropl(:,:,:)
wdot(:,:,:,NC_CO2) = wdot(:,:,:,NC_CO2) + ropl(:,:,:)
wdot(:,:,:,NC_HCO) = wdot(:,:,:,NC_HCO) - ropl(:,:,:)

end subroutine vecrates


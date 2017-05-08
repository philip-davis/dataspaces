!     !$========================================================================
!===============
      
!     New subroutine by Ramanan Sankaran - 01/24/05
!     This routine is based on the mcavis from chemkin's transport library
!     This routine was written for speed gains.
!     The old one had O(K^2) divisions and square roots.
!     This new routine does O(K) operations and are stored in memory.
      subroutine mcavis_new_looptool(t, x, rmcwrk, vismix)
!       
!       START PROLOGUE
!       
!       SUBROUTINE MCAVIS (T, X, RMCWRK, VISMIX)
!       Returns mixture viscosity, given temperature and species mole
!       fractions.  It uses modification of the Wilke semi-empirical
!       formulas.
!       
!       INPUT
!       T         - Real scalar, temperature.
!       cgs units, K
!       X(*)      - Real array, mole fractions of the mixture;
!       dimension at least KK, the total species count.
!       RMCWRK(*) - Real workspace array; dimension at least LENRMC.
!       
!       OUTPUT
!       VISMIX    - Real scalar, mixture viscosity.
!       cgs units, gm/cm*s
!       
!       END PROLOGUE
!       
        
        implicit none
        
        real zero, one, eight
        real ru, patmos, small
        integer nkk, no, nlite, inlin, iktdif, ipvt, nwt, neps, nsig, nd
     *ip, npol, nzrot, nlam, neta, ndif, ntdif, nxx, nvis, nxi, ncp, ncr
     *ot, ncint, nbind, neok, nsgm, nast, nbst, ncst, nxl, nr, nwrk, k3
        
        parameter (zero = 0.0, one = 1.0, eight = 8.0)
        common /mcmcmc/ ru, patmos, small, nkk, no, nlite, inlin, iktdif
     *, ipvt, nwt, neps, nsig, ndip, npol, nzrot, nlam, neta, ndif, ntdi
     *f, nxx, nvis, nxi, ncp, ncrot, ncint, nbind, neok, nsgm, nast, nbs
     *t, ncst, nxl, nr, nwrk, k3
        real, intent(in) :: x(*)
        real, intent(inout) :: rmcwrk(*)
        real, intent(in) :: t
        real, intent(out) :: vismix
        
!       !local declarations
        real, dimension(nkk) :: sqrt_eta, invsqrt_eta, quad_w, invquad_w
        real alogt, sumo
        real, dimension(nkk) :: sumi
        integer i, j, k
        integer kstrt
!       ------------------------------------
!       Temporaries for naive vectorizations
!       ------------------------------------
        integer lt__0
!       -------------------------------------
!       declarations for unroll-jam variables
!       -------------------------------------
        integer ujUpper30
!       
!       -----------------------------------
!       Temporaries for smart vectorization
!       -----------------------------------
!       
!       
        
!       !In the following call, the species molecular weights are stored
!       !in RMCWRK(NWT) and the pure species viscosities are in
!       !RMCWRK(NVIS)
        
        alogt = log(t)
        if (no .eq. 4) then
          call mceval4(alogt, nkk, rmcwrk(neta), rmcwrk(nvis))
        else
          call mceval(alogt, nkk, no, rmcwrk(neta), rmcwrk(nvis))
        endif
        
        do k = 1, nkk
          sqrt_eta(k) = exp(0.5 * rmcwrk(nvis + k - 1))
          rmcwrk(nvis + k - 1) = sqrt_eta(k) * sqrt_eta(k)
          invsqrt_eta(k) = 1.0 / sqrt_eta(k)
        enddo
!       !Modified by Mark Fahey, ORNL
!       !Fission the loop
!       !This might be better on other platforms too.
        do k = 1, nkk
          quad_w(k) = sqrt(sqrt(rmcwrk(nwt + k - 1)))
          invquad_w(k) = 1.0 / quad_w(k)
        enddo
        
        sumo = zero
        
        do lt__0 = 1, nkk
          sumi(lt__0) = zero
        enddo
        ujUpper30 = (nkk - 1 + 1) / 4 * 4 + 1 - 1
!       dir$ uj 4
        do k = 1, ujUpper30, 4
          do j = 1, nkk
            sumi(k) = sumi(k) + x(j) * (one + sqrt_eta(k) * invsqrt_eta(
     *j) * quad_w(j) * invquad_w(k)) ** 2 * sqrt(rmcwrk(nwt + j - 1) / (
     *rmcwrk(nwt + j - 1) + rmcwrk(nwt + k - 1)))
            sumi(k + 1) = sumi(k + 1) + x(j) * (one + sqrt_eta(k + 1) * 
     *invsqrt_eta(j) * quad_w(j) * invquad_w(k + 1)) ** 2 * sqrt(rmcwrk(
     *nwt + j - 1) / (rmcwrk(nwt + j - 1) + rmcwrk(nwt + k + 1 - 1)))
            sumi(k + 2) = sumi(k + 2) + x(j) * (one + sqrt_eta(k + 2) * 
     *invsqrt_eta(j) * quad_w(j) * invquad_w(k + 2)) ** 2 * sqrt(rmcwrk(
     *nwt + j - 1) / (rmcwrk(nwt + j - 1) + rmcwrk(nwt + k + 2 - 1)))
            sumi(k + 3) = sumi(k + 3) + x(j) * (one + sqrt_eta(k + 3) * 
     *invsqrt_eta(j) * quad_w(j) * invquad_w(k + 3)) ** 2 * sqrt(rmcwrk(
     *nwt + j - 1) / (rmcwrk(nwt + j - 1) + rmcwrk(nwt + k + 3 - 1)))
          enddo
        enddo
        do k = ujUpper30 + 1, nkk, 1
          do j = 1, nkk
            sumi(k) = sumi(k) + x(j) * (one + sqrt_eta(k) * invsqrt_eta(
     *j) * quad_w(j) * invquad_w(k)) ** 2 * sqrt(rmcwrk(nwt + j - 1) / (
     *rmcwrk(nwt + j - 1) + rmcwrk(nwt + k - 1)))
          enddo
        enddo
        do k = 1, nkk
          sumo = sumo + x(k) * rmcwrk(nvis + k - 1) / sumi(k)
        enddo
        
        vismix = sumo * sqrt(eight)
        
!       end of SUBROUTINE MCAVIS_new_looptool
        return
      end
!     ----------------------------------------------------------------------
      

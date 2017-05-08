C	 2-step mechanism for CH4/air
C	 Based on paper by: Franzelli et al.
C        to appear in her thesis 2011.
C
C        There are two variants BFER, which should be used with all Le=1.0,  and line 114 uncommented.
C                           and BFERstar, which should be used with Le=1.65, and line 115 uncommented.
C
C   *** Reaction #  1 : CH4+ 1.50O2=>CO+2H2O
C   4                     ! nspec (number of species involved in the reaction)
C   CH4    -1.00     +0.50       0.0
C   O2     -1.50     +0.65       0.0
C   CO     +1.00     +0.00       0.0
C   H2O    +2.00     +0.00       0.0
C    4.90d9                ! Pre-exponential factor (cgs)
C    35500.0d0              ! Activation Energy (cal/mol)
C    0.d0
C    0
C       
C   *** Reaction #  2 : CO+ 5.00E-01O2<=>CO2
C   3                     ! nspec (number of species involved in the reaction)
C   CO    -1.00     +1.00         0.0
C   O2    -0.50     +0.50         0.0
C   CO2   +1.00     +0.00         1.0
C   -2.0d8                ! Pre-exponential factor (cgs)
C    12000.0d0            ! Activation Energy (cal/mol)
C    0.7d0
C    0
C      
C
C    based on code provided by CERFACS (B.Cuenot)
C    implemented in S3D, May 2010 by E.S. Richardson
C    and D. Bergotti.
C
      SUBROUTINE GETRATES  (P, T, Y, ICKWRK, RCKWRK, WDOT)
      use thermchem_m, only: gibbsEnrg_all_dimT
      IMPLICIT NONE
C
      REAL RU, RUC
      PARAMETER (RU=8.314510D7, RUC=RU/4.184D7)
      INTEGER ICKWRK(*),K,I
      REAL Y(*), RCKWRK(*), WDOT(*)
      REAL C(6), Gibbs(6)
      REAL RF(2), RB(2), ROP(2), coeffstoic(6)
      REAL invweight(6),RFF(2),RBB(2)
      REAL deltaG0,P,T,TI,SUM,XKEQ
      REAL SMALL, RRR, sumcoeffstoic
      INTRINSIC EXP, LOG
      REAL FPEA1, FPEA2	
      REAL phi, phi01, sigma01,BB1, phi11,sigma11
      REAL CC1, phi21, sigma21, phi02, sigma02, BB2
      REAL phi12, sigma12, CC2, phi22, sigma22, phi32
      REAL sigma32, Yoxygen, Ycarbon, Yhydrogen 
C
      SMALL = 1.D-50
      RRR = RU*1.0D-7
      phi01 = 1.1
      sigma01 = 0.09
      BB1 = 0.37
      phi11 = 1.13
      sigma11 = 0.03
      CC1 = 6.7
      phi21 = 1.6
      sigma21 = 0.22
      phi02 = 0.95
      sigma02 = 0.08
      BB2 = 2.5d-5
      phi12 = 1.3
      sigma12 = 0.04
      CC2 = 0.0087
      phi22 = 1.2
      sigma22 = 0.04
      phi32 = 1.2
      sigma32 = 0.05
C      
      coeffstoic(1) = 0         ! CH4    stoichiometric coefficients in reaction #2
      coeffstoic(2) = -0.5      ! O2    
      coeffstoic(3) = -1        ! CO     
      coeffstoic(4) = +1        ! CO2    
      coeffstoic(5) = 0         ! H2O 
      coeffstoic(6) = 0         ! N2
      sumcoeffstoic = 0
C
      DO K = 1, 6
         sumcoeffstoic = sumcoeffstoic + coeffstoic(K)    ! calculates the global stoichiometric coefficient for reaction #2 (needed in Kuo law)
      ENDDO
C
C The inverse molecular weights in cgs units
      invweight(1) = 62.3352D-3    ! = 1.0D-3/16.0423D-3    ! CH4 molecular weights should be read. Here only copied from Cerfacs files cf input_species.dat @Cerfacs
      invweight(2) = 31.2512D-3    ! = 1.0D-3/31.9988D-3    ! O2
      invweight(3) = 35.6964D-3    ! = 1.0D-3/28.0140D-3    ! CO
      invweight(4) = 22.7222D-3    ! = 1.0D-3/44.0098D-3    ! CO2
      invweight(5) = 55.5084D-3    ! = 1.0D-3/18.0153D-3    ! H2O 
      invweight(6) = 35.6972D-3    ! = 1.0D-3/28.0134D-3    ! N2
C     
C     Convert mass fractions Y to concentrations C
      DO K = 1, 6
         C(K) = MAX(Y(K), SMALL)*invweight(K)                 ! C(K) is here Y(K)/weight(K)     weight is called W(K)
      ENDDO                                              
C
      SUM = 0.0
      DO K = 1, 6
         SUM = SUM + C(K)                                   ! SUM is now sum(Y(K)/mweight(K)) which is 1/W W is mean molecular weight
      ENDDO
      SUM = P/(SUM*T*8.314510D7)                            ! SUM is now rho (density)
C
      DO K = 1, 6
         C(K) = C(K) * SUM                                  ! C(K) is now rho*Y(K)/mweight(K), concentrations.
      ENDDO
C
      TI = 1.0D0/T
C
C     Compute forward reaction rate
C     Cerfacs pre-exponential factor and activation energy are in cgs units
      RFF(1) = 4.9D9*EXP(-35500.0D0*TI/RRR*4.1868)   ! this is the standard BFER mechanism  
C      RFF(1) = 3.96D9*EXP(-35500.0D0*TI/RRR*4.1868)   ! this is the BFERstar mechanism  
      RFF(2) = 2.0D8*EXP(-12000.0D0*TI/RRR*4.1868)     
      RFF(2) = RFF(2)*T**(0.7)				!it is not really an Arrhenius law
C
C     Initialize reverse reaction rates
      DO I = 1, 2
        RBB(I) = 0.0D0
      ENDDO
C
C     Equilibrium constant (Kuo model) for reaction 2 only
C  
C     calculation of entropies and enthalpies
C 
      deltaG0 = 0
C
C use a chemkin routine to return species enthalpy in molar units, then convert it to SI.
C
C
C here we need the standard state change of the Gibbs function: dG^o_T = dH^o - TdS^0, and we need it in J/mol.
C and we get it by calling the gibbsEnrg_all_dimT .... routine in themchem module.
C which returns it already divided by the Ru.
C
      call gibbsEnrg_all_dimT(T, Gibbs)    ! T need to be dimensional (K), G has units ????
C
      DO K = 1, 6
C
        deltaG0 = deltaG0 + Gibbs(K)*coeffstoic(K)
C
      ENDDO
C
      XKEQ = (1.0D5*TI/RRR)**(sumcoeffstoic)
      XKEQ = XKEQ*EXP(-deltaG0*TI)*1.0D3
C     
      Yoxygen = Y(2)+Y(4)*32.0/44.0+Y(3)*16.0/28.0+Y(5)*16.0/18.0
      Ycarbon = Y(1)*12.0/16.0+Y(4)*12.0/44.0+Y(3)*12.0/28.0
      Yhydrogen = Y(1)*4.0/16.0+Y(5)*2.0/18.0
      phi = (8.0/3.0*Ycarbon+8.0*Yhydrogen)/Yoxygen  !ok dans avbp donne bien 0.7 ???
      FPEA1 = 1+tanh((phi01-phi)/sigma01)
      FPEA1 = FPEA1+BB1*(1+tanh((phi-phi11)/sigma11))
      FPEA1 = FPEA1+CC1*(1+tanh((phi-phi21)/sigma21))
      FPEA1 = 2/FPEA1
      FPEA2 = 0.5*(1+tanh((phi02-phi)/sigma02))
      FPEA2 = FPEA2+BB2/2*(1+tanh((phi-phi12)/sigma12))
      CC2=CC2*(1+tanh((phi-phi22)/sigma22))
      CC2 = CC2*(1+tanh((phi32-phi)/sigma32))
      FPEA2 = FPEA2+CC2/2
C
      RFF(1) = RFF(1)*FPEA1
      RFF(2) = RFF(2)*FPEA2 
      RBB(2) = RFF(2)/XKEQ
C
      RF(1) = RFF(1)*(C(1))**0.5*(C(2))**0.65    !*(1.0D6)**(0.5+0.65)
      RF(2) = RFF(2)*(C(2))**(0.5)*C(3)          !*(1.0D6)**(0.5+1)
      RB(1) = 0.0D0
      RB(2) = RBB(2)*C(4)
C
C     Compute WDOT
C
      DO I = 1, 2
         ROP(I) = RF(I) - RB(I)
      ENDDO

      WDOT(1) = -ROP(1)  
      WDOT(2) = -1.5D0*ROP(1) -0.5D0*ROP(2)  
      WDOT(3) = +ROP(1) -ROP(2) 
      WDOT(4) = +ROP(2) 
      WDOT(5) = +ROP(1) +ROP(1)
      WDOT(6) = 0.0
C
      RETURN
      END

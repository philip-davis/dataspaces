C
C       4-step reduced mechanism for lean atmospheric CH4/air by Seshandri and Peters
C              ref: J.-Y. Chen and R.W. Dibble
C                   In Reduced Chemical Mechanisms and Asymptotic Approximations for Methane-Air Flames, 
C                   M. D. Smooke, Ed., Springer-Verlag, New York, Vol. 384, pp 193-226 (1991)
C       QSS species: OH, O, CH3, CH3O, HCO, HO2, H2O2
C
C       Implemented in S3D by E. Richardson.
C       (I have used as much of the original AVBP as possible, and 
C       this leads to the species order being different here than in 
C       the rest of the code, but the conversion is handled within this
C       subroutine hopefully it does not affect any one else. The original
C       CERFACS code is given at the bottom of this routine.)
C
C
      SUBROUTINE GETRATES  (P, T, Y, ICKWRK, RCKWRK, WDOT)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C      IMPLICIT NONE
C
      DOUBLE PRECISION RU, RUC, PATM
      PARAMETER (RU=8.314510D7, RUC=RU/4.184D7, PATM=1.01325D6)
      DIMENSION Y(*), ICKWRK(*), RCKWRK(*), WDOT(*)
      DIMENSION C(8)

      INTEGER I,K
      DOUBLE PRECISION P,T

!     LOCAL Variables knicked from AVBP
      DOUBLE PRECISION RRATE(4)
      INTEGER  N,IREAC,ISPEC,IND
      DOUBLE PRECISION ORDER_ABS
      DOUBLE PRECISION CSPEC, KF, KR,KEQ,YSPEC
      DOUBLE PRECISION RHOINV,FR,HK_R,SK_R
      DOUBLE PRECISION P0,DELTAHR,DELTASR
      DOUBLE PRECISION EFFICIENCY
      DOUBLE PRECISION PHI,Z,ZF,ZO,SPEC_0
!     PETERS
      DOUBLE PRECISION CLIP, M_7,M_14,M_15, CH4, O2, N2, CO2, H2O
      DOUBLE PRECISION FAKW, CO, H2, H, K1, K2, K6, K7, K8
      DOUBLE PRECISION K9, K10, K14, K15, K17, K18, KC12, K16
      DOUBLE PRECISION K9R, K10R, K11, K11R, K12, K12R, K14R, K15R
      DOUBLE PRECISION KC9, KC10, KC11
      DOUBLE PRECISION KII, KIV,  RATIO, R1, R2, ZW1, ZW2, R6
      DOUBLE PRECISION R8, R9, R10, R14, R15, FAKWR,RT

      DOUBLE PRECISION AA, XX, EA, A_INF, X_INF, E_INF, RKINF
      DOUBLE PRECISION RKLOW, FCENT, PR, SMALL_P, PRLOG, FCLOG
      DOUBLE PRECISION XN, CPRLOG, FLOG, FC, PCOR
      DOUBLE PRECISION OH, CHO, HO2, R14R, R15R, R16
      DOUBLE PRECISION K1R,K2R,K3,K3R,K4,K4R,K5,PRESSURE,ALPHAR
      DOUBLE PRECISION K13,K19,K20,K21,K22,K23,K24,K25,K22R,K23R,K24R
      DOUBLE PRECISION M,A_1,B_1,F_1,D_1,A_2,B_2,C_2,NUM,DEN
      DOUBLE PRECISION O, CH3,CH3O,CH2O,HCO,Z_P,H2O2
      DOUBLE PRECISION R1R, R5,R9R,R10R,R11,R11R,R12, R12R,R18,R19
      DOUBLE PRECISION R22,R22R,R24,R24R,R25

C
      SMALL = 1.D-50
C
C       convert Y to C
C S3D order
C 1 !H2
C 2 !H
C 3 !O2
C 4 !H2O
C 5 !CH4
C 6 !CO
C 7 !CO2
C 8 !N2

C AVBP order:
C 1 'H2'  
C 2 'O2'  
C 3 'H2O' 
C 4 'CH4' 
C 5 'CO'  
C 6 'CO2' 
C 7 'N2'  
C 8 'H'   
C
CESR: I have changed the order from what Benedetta provided since she 
CESR  the order did not correspond to the WDOT expressions below. 
CC      C(1) = MAX(Y(1), SMALL)*4.96046521D-1   !H2
CC      C(2) = MAX(Y(2), SMALL)*9.92093043D-1   !H
CC      C(3) = MAX(Y(3), SMALL)*3.12511716D-2   !O2
CC      C(4) = MAX(Y(4), SMALL)*5.55082499D-2   !H2O
CC      C(5) = MAX(Y(5), SMALL)*6.23323639D-2   !CH4
CC      C(6) = MAX(Y(6), SMALL)*3.56972032D-2   !CO
CC      C(7) = MAX(Y(7), SMALL)*2.27221341D-2   !CO2
CC      C(8) = MAX(Y(8), SMALL)*3.57008335D-2   !N2

      C(1) = MAX(Y(1), SMALL)*4.96046521D-1   !H2
      C(2) = MAX(Y(3), SMALL)*3.12511716D-2   !O2
      C(3) = MAX(Y(4), SMALL)*5.55082499D-2   !H2O
      C(4) = MAX(Y(5), SMALL)*6.23323639D-2   !CH4
      C(5) = MAX(Y(6), SMALL)*3.56972032D-2   !CO
      C(6) = MAX(Y(7), SMALL)*2.27221341D-2   !CO2
      C(7) = MAX(Y(8), SMALL)*3.57008335D-2   !N2
      C(8) = MAX(Y(2), SMALL)*9.92093043D-1   !H
C
      SUM = 0.0
      DO K = 1, 8 
         SUM = SUM + C(K)
      ENDDO
      SUM = P/(SUM*T*8.314510D7)
C
      DO K = 1,8 
         C(K) = C(K) * SUM
      ENDDO

        H2  = C(1)
        O2  = C(2)
        H2O = C(3)
        CH4 = C(4)
        CO  = C(5)
        CO2 = C(6)
        N2  = C(7)
        H   = C(8)

C The concentration is in cgs units of mol/cm^3

C
C     reaction rates
C
      ALOGT = LOG(T)
      TI = 1.0D0/T
      RT = TI/RUC
C
        K1  = 2.00000D+14*EXP(-16800.0 * RT)
        K1R = 1.57500D+13*EXP(-  690.0* RT)
        K2  = 1.80000D+10*EXP(- 8826.0* RT)*(T**1.00)
        K2R = 8.00000D+09*EXP(- 6760.0* RT)*(T**1.00)
        K3  = 1.17000D+09*EXP(- 3626.0* RT)*(T**1.30)
        K3R = 5.09000D+09*EXP(-18588.0* RT)*(T**1.30)
        K4  = 6.00000D+08*(T**1.30)
        K4R = 5.90000D+09*EXP(-17029.0* RT)*(T**1.30)
        K5  = 2.30000D+18/(T**0.8)
        K6  = 1.50000D+14*EXP(- 1004.0* RT)
        K7  = 2.50000D+13*EXP(-  700.0* RT)
        K8  = 2.00000D+13*EXP(- 1000.0* RT)
        K9  = 1.51000D+07*EXP(   758.0* RT)*(T**1.30)
        K9R = 1.57000D+09*EXP(-22337.0* RT)*(T**1.30)

        RKINF= 6.30000E+14*EXP(-104000.0* RT)
        PRESSURE = 1.0
        ALPHAR   = 0.517*EXP(-9000.0/T)
        PR  = PRESSURE / ALPHAR / T
        SMALL_P = 1.0E-20
        PR=MAX(PR,SMALL_P)
        PCOR = PR / (1.0 + PR)
        K10R = RKINF * PCOR
        RKINF= 5.20000E+12*EXP(  1310.0 * RT)
        PR  = PRESSURE / ALPHAR / T
        PR=MAX(PR,SMALL_P)
        PCOR = PR / (1.0 + PR)
        K10R = RKINF * PCOR

        K11  = 2.20000E+04*EXP(- 8750.0 * RT)*(T**3.00)
        K11R = 9.57000E+02*EXP(- 8750.0 * RT)*(T**3.00)
        K12  = 1.60000E+06*EXP(- 2460.0 * RT)*(T**2.10)
        K12R = 3.02000E+05*EXP(-17422.0 * RT)*(T**2.10)
        K13  = 6.80000E+13
        K14  = 2.50000E+13*EXP(- 3991.0 * RT)
        K15  = 3.00000E+13*EXP(- 1195.0 * RT)
        K16  = 4.00000E+13
        K17  = 1.60000E+14*EXP(-14700.0 * RT)
        K18  = 7.00000E+12*EXP(-25652.0 * RT)
        K19  = 2.00000E+13
        K20  = 2.40000E+13*EXP(-28812.0 * RT)
        K21  = 2.00000E+12
        K22  = 1.30000E+17*EXP(-45500.0 * RT)
        K22R = 9.86000E+14*EXP(  5070.0 * RT)
        K23  = 1.00000E+13*EXP(- 1800.0 * RT)
        K23R = 2.86000E+13*EXP(-32790.0 * RT)
        K24  = 2.20000E+22/(T**2.0)
        K24R = 5.657135E+22*EXP(-118498*RT)/(T**1.75351)
        K25  = 1.80000E+18/(T**1.0)

C/QSS species calculation
        M  =6.5*(CH4 + H2O)+1.5*CO2+H2+0.75*CO+0.4* ( N2 + O2) + H
        OH   = K3R * H2O * H / H2 / max(K3,small_P)
        A_1  = K1 * H * O2 + K2R * OH * H + K4 * OH * OH
        B_1  = K1R * OH + K2 * H2 + K4R * H2O
        F_1  = (K10 + K11 * H + K12 * OH) * CH4
        D_1  = K10R * H + K11R * H2 + K12R * H2O + K18 * O2
        A_2  = K13 * B_1
        B_2  = B_1 * d_1 + K13 * (f_1 - a_1)
        C_2  = a_1 * d_1 + K18 * f_1 * O2
        NUM  = (B_2 * B_2 + 4 * A_2 * C_2)**0.5 - B_2
        DEN  = 2 * A_2
        O    = NUM / max(DEN,small_P)
        NUM  = f_1
        DEN  = d_1 + K13 * O
        CH3  = NUM / max(DEN,small_P)
        NUM  = K18 * CH3 * O2
        DEN  = K19 * H + K20 * M
        CH3O = NUM / max(DEN,small_P)
        NUM  = K13 * O + ( K19 * H + K20 * M ) * CH3O
        DEN  = K14 * H + K15 * OH
        CH2O = NUM / max(DEN,small_P)
        NUM  = K13 * CH3 * O + ( K19 * H + K20 * M ) * CH3O
        DEN  = K16 * H + K17 * M
        HCO  = NUM / max(DEN,small_P)
        NUM  = K23 * OH
        DEN  = K22 * M + K23 * OH
        z_p    = NUM / max(DEN,small_P)
        A_2  = (2 - z_p) * K21
        B_2  = (1 - z_p) * K23R * H2O + (K6 + K7) * H + K8 * OH
        C_2  = K22R * OH * OH * M * z_p + K5 * H * O2 * M
        NUM  = (B_2 * B_2 + 4 * A_2 * C_2)**0.5 - B_2
        DEN  = 2 * A_2
        HO2  = NUM / max(DEN,small_P)
        NUM  = K21 * HO2 * HO2 + K22R * OH * OH * M + K23R * H2O * HO2
        DEN  = K22 * M + K23 * OH
        H2O2 = NUM / max(DEN,small_P)


C!/ Reaction rates of the skeletal mechanism
        R1  = K1 * H * O2
C!/ Correction for flame speed
        R1 = R1 * 0.7
        R1R  = K1R * O * OH
        R1   = R1 - R1R
        R5   = K5 * H * O2 * M
        R6   = K6 * H * HO2
        R9   = K9 * CO * OH
        R9R  = K9R * CO2 * H
        R9   = R9 - R9R
        R10 = K10 * CH4
        R10R = K10R * CH3 * H
        R10  = R10 - R10R
        R11 = K11 * CH4 * H
        R11R = K11R * CH3 * H2
        R11  = R11 - R11R
        R12 = K12 * CH4 * OH
        R12R = K12R * CH3 * H2O
        R12  = R12 - R12R
        R16  = K16 * HCO * H
        R18  = K18 * CH3 * O2
        R19  = K19 * CH3O * H
        R22 = K22 * H2O2 * M
        R22R = K22 * OH * OH * M
        R22  = R22 - R22R
        R24 = K24 * OH * H * M
        R24R = K24R * H2O * M
        R24  = R24 - R24R
        R25  = K25 * H * H * M

        RRATE(1) = R10 + R11 + R12
        RRATE(2) = R9
        RRATE(3) = R5 - R10 + R16 - R18 + R19 - R22 + R24 + R25
        RRATE(4) = R1 + R6 + R18 + R22

C S3D ORDER
C 1 !H2
C 2 !H
C 3 !O2
C 4 !H2O
C 5 !CH4
C 6 !CO
C 7 !CO2
C 8 !N2

C AVBP order:
C 1 'H2'  
C 2 'O2'  
C 3 'H2O' 
C 4 'CH4' 
C 5 'CO'  
C 6 'CO2' 
C 7 'N2'  
C 8 'H'   

C AVBP_original         WDOT(1) = 4*rrate(1)+rrate(2)
C AVBP_original     &                      +rrate(3)-3*rrate(4)
C AVBP_original         WDOT(2) = -rrate(4)
C AVBP_original         WDOT(3) = -rrate(1)-rrate(2)+2*rrate(4)
C AVBP_original         WDOT(4) = -rrate(1)
C AVBP_original         WDOT(5) = rrate(1)-rrate(2)
C AVBP_original         WDOT(6) = rrate(2)
C AVBP_original         WDOT(7) = 0.0
C AVBP_original         WDOT(8)=-2*rrate(1)-2*rrate(3)+2*rrate(4)

         WDOT(1) = 4*RRATE(1)+RRATE(2)
     &                      +RRATE(3)-3*RRATE(4)
         WDOT(3) = -RRATE(4)
         WDOT(4) = -RRATE(1)-RRATE(2)+2*RRATE(4)
         WDOT(5) = -RRATE(1)
         WDOT(6) = RRATE(1)-RRATE(2)
         WDOT(7) = RRATE(2)
         WDOT(8) = 0.0
         WDOT(2)=-2*RRATE(1)-2*RRATE(3)+2*RRATE(4)

         DO ISPEC=1,NEQS
           WDOT(ispec) = WDOT(ispec)  ! This is mol/cm^3/s
         END DO

C
      RETURN
      END




CAVBP_original  
CAVBP_original  C *******************************************************
CAVBP_original  
CAVBP_original  !     ===============================================================
CAVBP_original  !     Copyright (c) CERFACS (all rights reserved)
CAVBP_original  !     ===============================================================
CAVBP_original  
CAVBP_original  
CAVBP_original  include(avbp.inc)
CAVBP_original  
CAVBP_original        subroutine specsource ( neq,neqs,nnode,nreac,Astar,beta_T,
CAVBP_original       &                        Tact,stcoeff,stcoeffl,stcoefflr,
CAVBP_original       &                        efficiency_M,nb_M,
CAVBP_original       &                        w,thick,efcy,tempe,w_spec,
CAVBP_original       &                        wmol,Sigma_nuk,hk,sk,htform,ieq,
CAVBP_original       &                        rrate,rrate_r,source_spec,reac_mask,
CAVBP_original       &                        ipea,wmol_at,atoms,pea_phicst,
CAVBP_original       &                        pea_yf,pea_yo,npea,pea_coefs )
CAVBP_original  !
CAVBP_original  !***********************************************************************
CAVBP_original  !
CAVBP_original  !     Purpose : Subroutine for calculating the source terms
CAVBP_original  !               for the species equations; LES version using
CAVBP_original  !               the efficiency function.
CAVBP_original  !
CAVBP_original  !     Input:
CAVBP_original  !            neq          number of NS equations
CAVBP_original  !            neqs         number of species
CAVBP_original  !            nnode        number of nodes
CAVBP_original  !            nreac        number of reactions
CAVBP_original  !            Astar        reaction pre-exponential values
CAVBP_original  !            beta_T       reaction temperature exponents
CAVBP_original  !            Tact         reaction activation temperatures
CAVBP_original  !            stcoeff      stoichiometric reaction oefficients
CAVBP_original  !            stcoeffl     forward reaction coefficients
CAVBP_original  !            stcoefflr    backword reaction coefficients
CAVBP_original  !            efficiency_M third body efficiencies
CAVBP_original  !            nb_M         number of third bodies
CAVBP_original  !            w            NS variables
CAVBP_original  !            thick        thickening function
CAVBP_original  !            efcy         efficiency function
CAVBP_original  !            tempe        temperature values
CAVBP_original  !            w_spec       rhoYk variables
CAVBP_original  !            wmol         species molecular weight
CAVBP_original  !            Sigma_nuk    Convertion factors from cgs to SI units
CAVBP_original  !            hk           species enthalpy
CAVBP_original  !            sk           species entropy
CAVBP_original  !            htform       species enthalpy of formation
CAVBP_original  !            ieq          reversible reaction index
CAVBP_original  !            reack_mask   reaction mask
CAVBP_original  !            ipea         pea index
CAVBP_original  !            wmol_at      atom weight
CAVBP_original  !            atoms        species atoms
CAVBP_original  !            pea_phicst   global equivalence ratio
CAVBP_original  !            pea_yf       mass fractions in the fuel tank
CAVBP_original  !            pea_yo       mass fractions in the oxyder tank
CAVBP_original  !            npea         number of pea function coefficients
CAVBP_original  !            pea_coefs    pea function coefficients
CAVBP_original  !
CAVBP_original  !     Output:
CAVBP_original  !            rrate        forward reaction rates
CAVBP_original  !            rrate_r      backword reaction rates
CAVBP_original  !            source_spec  species source terms
CAVBP_original  !
CAVBP_original  !     Local:
CAVBP_original  !
CAVBP_original  !     Author:   C. Angelberger
CAVBP_original  !
CAVBP_original  !     Checked:
CAVBP_original  !
CAVBP_original  !     Last update: 02/10/2009 (Franzelli/Riber-PEA on both reactions
CAVBP_original  !                              Special Thanks to Marta Garcia)
CAVBP_original  !
CAVBP_original  !
CAVBP_original  !***********************************************************************
CAVBP_original  !
CAVBP_original        include 'avbp_constants.h'
CAVBP_original  
CAVBP_original  !     IN/OUT
CAVBP_original        avbp_int  neq,neqs,nnode,nreac
CAVBP_original        avbp_int  ieq(1:nreac)
CAVBP_original        avbp_real Astar(1:nreac)
CAVBP_original        avbp_real Tact(1:nreac)
CAVBP_original        avbp_int nb_M(1:nreac)
CAVBP_original        avbp_real beta_T(1:nreac)
CAVBP_original        avbp_real efficiency_M(1:nreac,1:neqs)
CAVBP_original        avbp_real Sigma_nuk(1:nreac)
CAVBP_original        avbp_real stcoeffl(1:nreac,1:neqs)
CAVBP_original        avbp_real stcoefflr(1:nreac,1:neqs)
CAVBP_original        avbp_real stcoeff(1:nreac,1:neqs)
CAVBP_original        avbp_real tempe(1:nnode)
CAVBP_original        avbp_real wmol(1:neqs)
CAVBP_original        avbp_real htform(1:neqs)
CAVBP_original        avbp_real hk(1:51,1:neqs)
CAVBP_original        avbp_real sk(1:51,1:neqs)
CAVBP_original        avbp_real w_spec(1:neqs,1:nnode)
CAVBP_original        avbp_real w(1:neq,1:nnode)
CAVBP_original        avbp_real source_spec(1:neqs,1:nnode)
CAVBP_original        avbp_real rrate(1:nreac,1:nnode)
CAVBP_original        avbp_real rrate_r(1:nreac,1:nnode)
CAVBP_original        avbp_real efcy(1:nnode)
CAVBP_original        avbp_real thick(1:nnode)
CAVBP_original        avbp_real reac_mask(1:nnode)
CAVBP_original        avbp_int  ipea,npea
CAVBP_original        avbp_real wmol_at,pea_phicst
CAVBP_original        avbp_real atoms(1:neqs),pea_yf(1:neqs),pea_yo(1:neqs)
CAVBP_original        avbp_real pea_coefs(1:npea)
CAVBP_original  
CAVBP_original  !     LOCAL
CAVBP_original        avbp_int  n,ireac,ispec,ind
CAVBP_original        avbp_real order_abs
CAVBP_original        avbp_real Cspec, kf, kr,Keq,Yspec
CAVBP_original        avbp_real rhoinv,fr,hk_r,sk_r
CAVBP_original        avbp_real P0,DeltaHr,DeltaSr
CAVBP_original        avbp_real efficiency
CAVBP_original        avbp_real phi,z,zf,zo,spec_0
CAVBP_original        avbp_real pea1_f
CAVBP_original        avbp_real pea2_f(1:2)
CAVBP_original  !     PETERS
CAVBP_original        avbp_real w_conc(1:neqs)
CAVBP_original        avbp_real clip, M_7,M_14,M_15, CH4, O2, N2, CO2, H2O 
CAVBP_original        avbp_real FAKW, CO, H2, H, K1, K2, K6, K7, K8
CAVBP_original        avbp_real K9, K10, K14, K15, K17, K18, KC12, K16
CAVBP_original        avbp_real K9R, K10R, K11, K11R, K12, K12R, K14R, K15R
CAVBP_original        avbp_real KC9, KC10, KC11
CAVBP_original        avbp_real KII, KIV,  RATIO, R1, R2, ZW1, ZW2, R6
CAVBP_original        avbp_real R8, R9, R10, R14, R15, FAKWR,RT
CAVBP_original  
CAVBP_original        avbp_real AA, XX, EA, A_inf, X_inf, E_inf, RKINF
CAVBP_original        avbp_real RKLOW, FCENT, PR, SMALL_P, PRLOG, FCLOG
CAVBP_original        avbp_real XN, CPRLOG, FLOG, FC, PCOR
CAVBP_original        avbp_real OH, CHO, HO2, R14R, R15R, R16
CAVBP_original        avbp_real K1R,K2R,K3,K3R,K4,K4R,K5,pressure,alphaR
CAVBP_original        avbp_real K13,K19,K20,K21,K22,K23,K24,K25,K22R,K23R,K24R
CAVBP_original        avbp_real M,A_1,B_1,F_1,D_1,A_2,B_2,C_2,NUM,DEN
CAVBP_original        avbp_real O, CH3,CH3O,CH2O,HCO,z_p,H2O2
CAVBP_original        avbp_real R1R, R5,R9R,R10R,R11,R11R,R12, R12R,R18,R19
CAVBP_original        avbp_real R22,R22R,R24,R24R,R25
CAVBP_original  
CAVBP_original  
CAVBP_original        avbp_real phi0,sigma0,B,phi1,sigma1
CAVBP_original        avbp_real phi2,sigma2
CAVBP_original        avbp_real phi01,sigma01,B1,phi11,sigma11
CAVBP_original        avbp_real C1,phi21,sigma21
CAVBP_original        avbp_real phi02,sigma02,B2,phi12,sigma12
CAVBP_original        avbp_real C2,phi22,sigma22,phi32,sigma32
CAVBP_original  
CAVBP_original        common /data_pea/ phi0,sigma0,B,phi1,sigma1,
CAVBP_original       &                  phi2,sigma2,
CAVBP_original       &                  phi01,sigma01,B1,phi11,sigma11,
CAVBP_original       &                  C1,phi21,sigma21,
CAVBP_original       &                  phi02,sigma02,B2,phi12,sigma12,
CAVBP_original       &                  C2,phi22,sigma22,phi32,sigma32
CAVBP_original        save /data_pea/
CAVBP_original  
CAVBP_original  
CAVBP_original        logical firsttime
CAVBP_original        data firsttime / .true. /
CAVBP_original        save firsttime
CAVBP_original  
CAVBP_original  
CAVBP_original        if ( firsttime ) then
CAVBP_original  
CAVBP_original          if ( ipea==1 ) then
CAVBP_original  
CAVBP_original            phi0     = pea_coefs(1)
CAVBP_original            sigma0   = pea_coefs(2)
CAVBP_original            B        = pea_coefs(3)
CAVBP_original            phi1     = pea_coefs(4)
CAVBP_original            sigma1   = pea_coefs(5)
CAVBP_original            phi2     = pea_coefs(6)
CAVBP_original            sigma2   = pea_coefs(7)
CAVBP_original  
CAVBP_original          else if ( ipea==2 ) then
CAVBP_original  
CAVBP_original            phi01    = pea_coefs(1)
CAVBP_original            sigma01  = pea_coefs(2)
CAVBP_original            B1       = pea_coefs(3)
CAVBP_original            phi11    = pea_coefs(4)
CAVBP_original            sigma11  = pea_coefs(5)
CAVBP_original            C1       = pea_coefs(6)
CAVBP_original            phi21    = pea_coefs(7)
CAVBP_original            sigma21  = pea_coefs(8)
CAVBP_original            phi02    = pea_coefs(9)
CAVBP_original            sigma02  = pea_coefs(10)
CAVBP_original            B2       = pea_coefs(11)
CAVBP_original            phi12    = pea_coefs(12)
CAVBP_original            sigma12  = pea_coefs(13)
CAVBP_original            C2       = pea_coefs(14)
CAVBP_original            phi22    = pea_coefs(15)
CAVBP_original            sigma22  = pea_coefs(16)
CAVBP_original            phi32    = pea_coefs(17)
CAVBP_original            sigma32  = pea_coefs(18)
CAVBP_original  
CAVBP_original          end if
CAVBP_original  
CAVBP_original          firsttime = .false.
CAVBP_original  
CAVBP_original        end if
CAVBP_original  
CAVBP_original  
CAVBP_original  
CAVBP_original  ! Reference pressure for reverse reaction constant
CAVBP_original        P0 = 100000.0d0
CAVBP_original  
CAVBP_original  
CAVBP_original        do n=1,nnode
CAVBP_original  !
CAVBP_original  ! Standard Arrhenius with efficiency function
CAVBP_original  !
CAVBP_original          ind = int(0.01d0*tempe(n))
CAVBP_original          fr = tempe(n)*0.01d0 - ind
CAVBP_original          rhoinv = one / w(1,n)
CAVBP_original  
CAVBP_original          clip = 1.0E-10
CAVBP_original          FAKW = 1.0E-6 
CAVBP_original          FAKWR = 1.0E+6
CAVBP_original  !  Clip on concentrations
CAVBP_original  !  + concentration in mole/cm units
CAVBP_original  
CAVBP_original          do ispec=1,neqs
CAVBP_original        
CAVBP_original            w_conc(ispec) = w_spec(ispec,n) * rhoinv
CAVBP_original            if ( w_conc(ispec) < clip) then
CAVBP_original              w_conc(ispec) = clip * w(1,n) / wmol(ispec) * FAKW
CAVBP_original            else if ( w_conc(ispec) > one) then
CAVBP_original              w_conc(ispec) = one * w(1,n) / wmol(ispec) * FAKW
CAVBP_original            else
CAVBP_original              w_conc(ispec) = w_conc(ispec)* w(1,n) / wmol(ispec) * FAKW
CAVBP_original            endif
CAVBP_original          enddo
CAVBP_original  
CAVBP_original          
CAVBP_original  ! Species assegnation => depend on input_premix!!!! 
CAVBP_original  
CAVBP_original          H2  = w_conc(1)
CAVBP_original          O2  = w_conc(2)
CAVBP_original          H2O = w_conc(3)
CAVBP_original          CH4 = w_conc(4)
CAVBP_original          CO  = w_conc(5)
CAVBP_original          CO2 = w_conc(6)
CAVBP_original          N2  = w_conc(7)
CAVBP_original          H   = w_conc(8)
CAVBP_original    
CAVBP_original          RT = one / tempe(n)/ 1.9859
CAVBP_original  
CAVBP_original          K1  = 2.00000E+14*exp(-16800 * rt)
CAVBP_original          K1R = 1.57500E+13*exp(-  690 * rt)
CAVBP_original          K2  = 1.80000E+10*exp(- 8826 * rt)*(tempe(n)**1.00)
CAVBP_original          K2R = 8.00000E+09*exp(- 6760 * rt)*(tempe(n)**1.00)
CAVBP_original          K3  = 1.17000E+09*exp(- 3626 * rt)*(tempe(n)**1.30)
CAVBP_original          K3R = 5.09000E+09*exp(-18588 * rt)*(tempe(n)**1.30)
CAVBP_original          K4  = 6.00000E+08*(tempe(n)**1.30)
CAVBP_original          K4R = 5.90000E+09*exp(-17029 * rt)*(tempe(n)**1.30)
CAVBP_original          K5  = 2.30000E+18/(tempe(n)**0.8)
CAVBP_original          K6  = 1.50000E+14*exp(- 1004 * rt)
CAVBP_original          K7  = 2.50000E+13*exp(-  700 * rt)
CAVBP_original          K8  = 2.00000E+13*exp(- 1000 * rt)
CAVBP_original          K9  = 1.51000E+07*exp(   758 * rt)*(tempe(n)**1.30)
CAVBP_original          K9R = 1.57000E+09*exp(-22337 * rt)*(tempe(n)**1.30)
CAVBP_original  
CAVBP_original          RKINF= 6.30000E+14*exp(-104000* rt)
CAVBP_original          pressure = 1.0
CAVBP_original          alphaR   = 0.517*exp(-9000/tempe(n))
CAVBP_original          PR  = pressure / alphaR / tempe(n)
CAVBP_original          SMALL_P = 1.0e-20
CAVBP_original          PR=max(PR,SMALL_P)
CAVBP_original          PCOR = PR / (1.0 + PR)
CAVBP_original          K10R = RKINF * PCOR
CAVBP_original          RKINF= 5.20000E+12*exp(  1310 * rt)
CAVBP_original          PR  = pressure / alphaR / tempe(n)
CAVBP_original          PR=max(PR,SMALL_P)
CAVBP_original          PCOR = PR / (1.0 + PR)
CAVBP_original          K10R = RKINF * PCOR
CAVBP_original          
CAVBP_original          K11  = 2.20000E+04*exp(- 8750 * rt)*(tempe(n)**3.00)
CAVBP_original          K11R = 9.57000E+02*exp(- 8750 * rt)*(tempe(n)**3.00)
CAVBP_original          K12  = 1.60000E+06*exp(- 2460 * rt)*(tempe(n)**2.10)
CAVBP_original          K12R = 3.02000E+05*exp(-17422 * rt)*(tempe(n)**2.10)
CAVBP_original          K13  = 6.80000E+13
CAVBP_original          K14  = 2.50000E+13*exp(- 3991 * rt)
CAVBP_original          K15  = 3.00000E+13*exp(- 1195 * rt)
CAVBP_original          K16  = 4.00000E+13
CAVBP_original          K17  = 1.60000E+14*exp(-14700 * rt)
CAVBP_original          K18  = 7.00000E+12*exp(-25652 * rt)
CAVBP_original          K19  = 2.00000E+13
CAVBP_original          K20  = 2.40000E+13*exp(-28812 * rt)
CAVBP_original          K21  = 2.00000E+12
CAVBP_original          K22  = 1.30000E+17*exp(-45500 * rt)
CAVBP_original          K22R = 9.86000E+14*exp(  5070 * rt)
CAVBP_original          K23  = 1.00000E+13*exp(- 1800 * rt)
CAVBP_original          K23R = 2.86000E+13*exp(-32790 * rt)
CAVBP_original          K24  = 2.20000E+22/(tempe(n)**2.0)
CAVBP_original          K24R = 5.657135E+22*exp(-118498*rt)/(tempe(n)**1.75351)
CAVBP_original          K25  = 1.80000E+18/(tempe(n)**1.0)
CAVBP_original   
CAVBP_original  !/QSS species calculation
CAVBP_original          M  =6.5*(CH4 + H2O)+1.5*CO2+H2+0.75*CO+0.4* ( N2 + O2) + H
CAVBP_original          OH   = K3R * H2O * H / H2 / max(K3,small_P)
CAVBP_original          A_1  = K1 * H * O2 + K2R * OH * H + K4 * OH * OH
CAVBP_original          B_1  = K1R * OH + K2 * H2 + K4R * H2O
CAVBP_original          F_1  = (K10 + K11 * H + K12 * OH) * CH4
CAVBP_original          D_1  = K10R * H + K11R * H2 + K12R * H2O + K18 * O2
CAVBP_original          A_2  = K13 * B_1
CAVBP_original          B_2  = B_1 * d_1 + K13 * (f_1 - a_1)
CAVBP_original          C_2  = a_1 * d_1 + K18 * f_1 * O2
CAVBP_original          NUM  = (B_2 * B_2 + 4 * A_2 * C_2)**0.5 - B_2
CAVBP_original          DEN  = 2 * A_2
CAVBP_original          O    = NUM / max(DEN,small_P)
CAVBP_original          NUM  = f_1
CAVBP_original          DEN  = d_1 + K13 * O
CAVBP_original          CH3  = NUM / max(DEN,small_P)
CAVBP_original          NUM  = K18 * CH3 * O2
CAVBP_original          DEN  = K19 * H + K20 * M
CAVBP_original          CH3O = NUM / max(DEN,small_P)
CAVBP_original          NUM  = K13 * O + ( K19 * H + K20 * M ) * CH3O
CAVBP_original          DEN  = K14 * H + K15 * OH
CAVBP_original          CH2O = NUM / max(DEN,small_P)
CAVBP_original          NUM  = K13 * CH3 * O + ( K19 * H + K20 * M ) * CH3O
CAVBP_original          DEN  = K16 * H + K17 * M
CAVBP_original          HCO  = NUM / max(DEN,small_P)
CAVBP_original          NUM  = K23 * OH
CAVBP_original          DEN  = K22 * M + K23 * OH
CAVBP_original          z_p    = NUM / max(DEN,small_P)
CAVBP_original          A_2  = (2 - z_p) * K21
CAVBP_original          B_2  = (1 - z_p) * K23R * H2O + (K6 + K7) * H + K8 * OH
CAVBP_original          C_2  = K22R * OH * OH * M * z_p + K5 * H * O2 * M
CAVBP_original          NUM  = (B_2 * B_2 + 4 * A_2 * C_2)**0.5 - B_2
CAVBP_original          DEN  = 2 * A_2
CAVBP_original          HO2  = NUM / max(DEN,small_P)
CAVBP_original          NUM  = K21 * HO2 * HO2 + K22R * OH * OH * M + K23R * H2O * HO2
CAVBP_original          DEN  = K22 * M + K23 * OH
CAVBP_original          H2O2 = NUM / max(DEN,small_P)
CAVBP_original   
CAVBP_original  
CAVBP_original  !/ Reaction rates of the skeletal mechanism
CAVBP_original          R1  = FAKWR * K1 * H * O2
CAVBP_original  !/ Correction for flame speed
CAVBP_original          R1 = R1 * 0.7
CAVBP_original          R1R  = FAKWR * K1R * O * OH
CAVBP_original          R1   = R1 - R1R
CAVBP_original          R5   = FAKWR * K5 * H * O2 * M
CAVBP_original          R6   = FAKWR * K6 * H * HO2
CAVBP_original          R9   = FAKWR * K9 * CO * OH
CAVBP_original          R9R  = FAKWR * K9R * CO2 * H
CAVBP_original          R9   = R9 - R9R
CAVBP_original          R10 = FAKWR * K10 * CH4
CAVBP_original          R10R = FAKWR * K10R * CH3 * H
CAVBP_original          R10  = R10 - R10R
CAVBP_original          R11 = FAKWR * K11 * CH4 * H
CAVBP_original          R11R = FAKWR * K11R * CH3 * H2
CAVBP_original          R11  = R11 - R11R
CAVBP_original          R12 = FAKWR * K12 * CH4 * OH
CAVBP_original          R12R = FAKWR * K12R * CH3 * H2O
CAVBP_original          R12  = R12 - R12R
CAVBP_original          R16  = FAKWR * K16 * HCO * H
CAVBP_original          R18  = FAKWR * K18 * CH3 * O2
CAVBP_original          R19  = FAKWR * K19 * CH3O * H
CAVBP_original          R22 = FAKWR * K22 * H2O2 * M
CAVBP_original          R22R = FAKWR * K22 * OH * OH * M
CAVBP_original          R22  = R22 - R22R
CAVBP_original          R24 = FAKWR * K24 * OH * H * M
CAVBP_original          R24R = FAKWR * K24R * H2O * M
CAVBP_original          R24  = R24 - R24R
CAVBP_original          R25  = FAKWR * K25 * H * H * M
CAVBP_original   
CAVBP_original  
CAVBP_original          rrate(1,n) = R10 + R11 + R12 
CAVBP_original          rrate_r(1,n) = zero
CAVBP_original          rrate(2,n) = R9
CAVBP_original          rrate_r(2,n) = zero
CAVBP_original          rrate(3,n) = R5 - R10 + R16 - R18 + R19 - R22 + R24 + R25 
CAVBP_original          rrate_r(3,n) = zero
CAVBP_original          rrate(4,n) = R1 + R6 + R18 + R22
CAVBP_original          rrate_r(4,n) = zero  
CAVBP_original  
CAVBP_original  
CAVBP_original  !
CAVBP_original  ! Calculation of source term for each specie from the reaction rates
CAVBP_original  !
CAVBP_original  !        do ispec=1,neqs
CAVBP_original  !          source_spec(ispec,n) = zero
CAVBP_original  !          do ireac=1,nreac
CAVBP_original  !            source_spec(ispec,n) = source_spec(ispec,n)
CAVBP_original  !     &                           + stcoeff(ireac,ispec)*wmol(ispec)
CAVBP_original  !     &                             * (rrate(ireac,n)+rrate_r(ireac,n))
CAVBP_original  !          end do
CAVBP_original  !        end do
CAVBP_original           
CAVBP_original           source_spec(1,n) = 4*rrate(1,n)+rrate(2,n)
CAVBP_original       &                      +rrate(3,n)-3*rrate(4,n)
CAVBP_original           source_spec(2,n) = -rrate(4,n)
CAVBP_original           source_spec(3,n) = -rrate(1,n)-rrate(2,n)+2*rrate(4,n)
CAVBP_original           source_spec(4,n) = -rrate(1,n)
CAVBP_original           source_spec(5,n) = rrate(1,n)-rrate(2,n)
CAVBP_original           source_spec(6,n) = rrate(2,n)
CAVBP_original           source_spec(7,n) = 0.0
CAVBP_original           source_spec(8,n)=-2*rrate(1,n)-2*rrate(3,n)+2*rrate(4,n) 
CAVBP_original  
CAVBP_original           do ispec=1,neqs
CAVBP_original             source_spec(ispec,n) = source_spec(ispec,n)*wmol(ispec)
CAVBP_original           end do
CAVBP_original  
CAVBP_original        end do
CAVBP_original  
CAVBP_original  
CAVBP_original        return
CAVBP_original        end
CAVBP_original  

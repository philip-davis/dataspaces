C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE RATX (VL, T, C, RF, RB, RKLOW)
      USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      PARAMETER (SMALL = 1D-200)
      DIMENSION C(MAXVL,*),RF(MAXVL,*),RB(MAXVL,*),RKLOW(MAXVL,*)
      DIMENSION T(MAXVL),CTOT(MAXVL),CTB(MAXVL)
      DIMENSION PR(MAXVL),PRLOG(MAXVL),PCOR(MAXVL)
      DIMENSION FCLOG(MAXVL),FC(MAXVL),XN(MAXVL)
      DIMENSION FCENT0(MAXVL),FCENT1(MAXVL),FCENT2(MAXVL)
      DIMENSION CPRLOG(MAXVL),FLOG(MAXVL),FCENT(MAXVL)
        INTEGER VL
C
      CTOT(:VL) = 0.0
      DO K = 1, 30
         CTOT(:VL) = CTOT(:VL) + C(:VL,K)
      ENDDO
C
C      R1: H + O2 = O + OH
      RF(:VL,1) = RF(:VL,1)*C(:VL,1)*C(:VL,15)
      RB(:VL,1) = RB(:VL,1)*C(:VL,4)*C(:VL,6)
C      R2: H2 + O = H + OH
      RF(:VL,2) = RF(:VL,2)*C(:VL,2)*C(:VL,4)
      RB(:VL,2) = RB(:VL,2)*C(:VL,1)*C(:VL,6)
C      R3: H2 + OH = H + H2O
      RF(:VL,3) = RF(:VL,3)*C(:VL,2)*C(:VL,6)
      RB(:VL,3) = RB(:VL,3)*C(:VL,1)*C(:VL,7)
C      R4: O + H2O = 2OH
      RF(:VL,4) = RF(:VL,4)*C(:VL,4)*C(:VL,7)
      RB(:VL,4) = RB(:VL,4)*C(:VL,6)*C(:VL,6)
C      R5: H2 = 2H
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+1.1D1*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      RF(:VL,5) = RF(:VL,5)*CTB(:VL)*C(:VL,2)
      RB(:VL,5) = RB(:VL,5)*CTB(:VL)*C(:VL,1)*C(:VL,1)
C      R6: 2O = O2
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+1.1D1*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      RF(:VL,6) = RF(:VL,6)*CTB(:VL)*C(:VL,4)*C(:VL,4)
      RB(:VL,6) = RB(:VL,6)*CTB(:VL)*C(:VL,15)
C      R7: H + O = OH
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+1.1D1*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      RF(:VL,7) = RF(:VL,7)*CTB(:VL)*C(:VL,1)*C(:VL,4)
      RB(:VL,7) = RB(:VL,7)*CTB(:VL)*C(:VL,6)
C      R8: H + OH = H2O
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+1.1D1*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      RF(:VL,8) = RF(:VL,8)*CTB(:VL)*C(:VL,1)*C(:VL,6)
      RB(:VL,8) = RB(:VL,8)*CTB(:VL)*C(:VL,7)
C      R9: H + O2 = HO2
      CTB(:VL) = CTOT(:VL)+C(:VL,2)+1D1*C(:VL,7)-2.2D-1*C(:VL,15)
     * +9D-1*C(:VL,9)+2.8D0*C(:VL,18)
      PR(:VL) = RKLOW(:VL,1) * CTB(:VL) / RF(:VL,9)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FC = (PRLOG(:VL) -3.35070291D-1)
     *     /(8.73075717D-1 -0.14D0*(PRLOG(:VL) -3.35070291D-1))
      FC(:VL) = -2.23143551D-1 /(1.0D0 + FC(:VL)*FC(:VL))
      CALL VRDA_EXP(VL, FC, FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,9) = RF(:VL,9) * PCOR(:VL)
      RB(:VL,9) = RB(:VL,9) * PCOR(:VL)
      RF(:VL,9) = RF(:VL,9)*C(:VL,1)*C(:VL,15)
      RB(:VL,9) = RB(:VL,9)*C(:VL,16)
C      R10: H + HO2 = H2 + O2
      RF(:VL,10) = RF(:VL,10)*C(:VL,1)*C(:VL,16)
      RB(:VL,10) = RB(:VL,10)*C(:VL,2)*C(:VL,15)
C      R11: H + HO2 = 2OH
      RF(:VL,11) = RF(:VL,11)*C(:VL,1)*C(:VL,16)
      RB(:VL,11) = RB(:VL,11)*C(:VL,6)*C(:VL,6)
C      R12: O + HO2 = OH + O2
      RF(:VL,12) = RF(:VL,12)*C(:VL,4)*C(:VL,16)
      RB(:VL,12) = RB(:VL,12)*C(:VL,6)*C(:VL,15)
C      R13: OH + HO2 = H2O + O2
      RF(:VL,13) = RF(:VL,13)*C(:VL,6)*C(:VL,16)
      RB(:VL,13) = RB(:VL,13)*C(:VL,7)*C(:VL,15)
C      R14: 2HO2 = O2 + H2O2
      RF(:VL,14) = RF(:VL,14)*C(:VL,16)*C(:VL,16)
      RB(:VL,14) = RB(:VL,14)*C(:VL,15)*C(:VL,17)
C      R15: 2HO2 = O2 + H2O2
      RF(:VL,15) = RF(:VL,15)*C(:VL,16)*C(:VL,16)
      RB(:VL,15) = RB(:VL,15)*C(:VL,15)*C(:VL,17)
C      R16: H2O2 = 2OH
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+1.1D1*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      PR(:VL) = RKLOW(:VL,2) * CTB(:VL) / RF(:VL,16)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FC = (PRLOG(:VL) -1.98309903D-1)
     *     /(1.13230809D0 -0.14D0*(PRLOG(:VL) -1.98309903D-1))
      FC(:VL) = -6.93147181D-1 /(1.0D0 + FC(:VL)*FC(:VL))
      CALL VRDA_EXP(VL, FC, FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,16) = RF(:VL,16) * PCOR(:VL)
      RB(:VL,16) = RB(:VL,16) * PCOR(:VL)
      RF(:VL,16) = RF(:VL,16)*C(:VL,17)
      RB(:VL,16) = RB(:VL,16)*C(:VL,6)*C(:VL,6)
C      R17: H + H2O2 = OH + H2O
      RF(:VL,17) = RF(:VL,17)*C(:VL,1)*C(:VL,17)
      RB(:VL,17) = RB(:VL,17)*C(:VL,6)*C(:VL,7)
C      R18: H + H2O2 = H2 + HO2
      RF(:VL,18) = RF(:VL,18)*C(:VL,1)*C(:VL,17)
      RB(:VL,18) = RB(:VL,18)*C(:VL,2)*C(:VL,16)
C      R19: O + H2O2 = OH + HO2
      RF(:VL,19) = RF(:VL,19)*C(:VL,4)*C(:VL,17)
      RB(:VL,19) = RB(:VL,19)*C(:VL,6)*C(:VL,16)
C      R20: OH + H2O2 = H2O + HO2
      RF(:VL,20) = RF(:VL,20)*C(:VL,6)*C(:VL,17)
      RB(:VL,20) = RB(:VL,20)*C(:VL,7)*C(:VL,16)
C      R21: OH + H2O2 = H2O + HO2
      RF(:VL,21) = RF(:VL,21)*C(:VL,6)*C(:VL,17)
      RB(:VL,21) = RB(:VL,21)*C(:VL,7)*C(:VL,16)
C      R22: O + CO = CO2
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+1.1D1*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      PR(:VL) = RKLOW(:VL,3) * CTB(:VL) / RF(:VL,22)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      RF(:VL,22) = RF(:VL,22) * PCOR(:VL)
      RB(:VL,22) = RB(:VL,22) * PCOR(:VL)
      RF(:VL,22) = RF(:VL,22)*C(:VL,4)*C(:VL,9)
      RB(:VL,22) = RB(:VL,22)*C(:VL,18)
C      R23: CO + O2 = O + CO2
      RF(:VL,23) = RF(:VL,23)*C(:VL,9)*C(:VL,15)
      RB(:VL,23) = RB(:VL,23)*C(:VL,4)*C(:VL,18)
C      R24: CO + HO2 = OH + CO2
      RF(:VL,24) = RF(:VL,24)*C(:VL,9)*C(:VL,16)
      RB(:VL,24) = RB(:VL,24)*C(:VL,6)*C(:VL,18)
C      R25: OH + CO = H + CO2
      RF(:VL,25) = RF(:VL,25)*C(:VL,6)*C(:VL,9)
      RB(:VL,25) = RB(:VL,25)*C(:VL,1)*C(:VL,18)
C      R26: HCO = H + CO
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+5D0*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      RF(:VL,26) = RF(:VL,26)*CTB(:VL)
      RB(:VL,26) = RB(:VL,26)*CTB(:VL)*C(:VL,1)*C(:VL,9)
C      R27: HCO + O2 = CO + HO2
      RF(:VL,27) = RF(:VL,27)*C(:VL,15)
      RB(:VL,27) = RB(:VL,27)*C(:VL,9)*C(:VL,16)
C      R28: H + HCO = H2 + CO
      RF(:VL,28) = RF(:VL,28)*C(:VL,1)
      RB(:VL,28) = RB(:VL,28)*C(:VL,2)*C(:VL,9)
C      R29: O + HCO = OH + CO
      RF(:VL,29) = RF(:VL,29)*C(:VL,4)
      RB(:VL,29) = RB(:VL,29)*C(:VL,6)*C(:VL,9)
C      R30: OH + HCO = H2O + CO
      RF(:VL,30) = RF(:VL,30)*C(:VL,6)
      RB(:VL,30) = RB(:VL,30)*C(:VL,7)*C(:VL,9)
C      R31: O + HCO = H + CO2
      RF(:VL,31) = RF(:VL,31)*C(:VL,4)
      RB(:VL,31) = RB(:VL,31)*C(:VL,1)*C(:VL,18)
C      R32: HCO + HO2 = H + OH + CO2
      RF(:VL,32) = RF(:VL,32)*C(:VL,16)
      RB(:VL,32) = RB(:VL,32)*C(:VL,1)*C(:VL,6)*C(:VL,18)
C      R33: 2HCO = H2 + 2CO
      RB(:VL,33) = RB(:VL,33)*C(:VL,2)*C(:VL,9)*C(:VL,9)
C      R34: CH3 + HCO = CH4 + CO
      RF(:VL,34) = RF(:VL,34)*C(:VL,3)
      RB(:VL,34) = RB(:VL,34)*C(:VL,5)*C(:VL,9)
C      R35: 2HCO = CO + CH2O
      RB(:VL,35) = RB(:VL,35)*C(:VL,9)*C(:VL,12)
C      R36: CH2O = H + HCO
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+1.1D1*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      RF(:VL,36) = RF(:VL,36)*CTB(:VL)*C(:VL,12)
      RB(:VL,36) = RB(:VL,36)*CTB(:VL)*C(:VL,1)
C      R37: CH2O = H2 + CO
      CTB(:VL) = CTOT(:VL)+1.5D0*C(:VL,2)+1.1D1*C(:VL,7)+9D-1*C(:VL,9)
     * +2.8D0*C(:VL,18)
      RF(:VL,37) = RF(:VL,37)*CTB(:VL)*C(:VL,12)
      RB(:VL,37) = RB(:VL,37)*CTB(:VL)*C(:VL,2)*C(:VL,9)
C      R38: H + CH2O = H2 + HCO
      RF(:VL,38) = RF(:VL,38)*C(:VL,1)*C(:VL,12)
      RB(:VL,38) = RB(:VL,38)*C(:VL,2)
C      R39: O + CH2O = OH + HCO
      RF(:VL,39) = RF(:VL,39)*C(:VL,4)*C(:VL,12)
      RB(:VL,39) = RB(:VL,39)*C(:VL,6)
C      R40: OH + CH2O = H2O + HCO
      RF(:VL,40) = RF(:VL,40)*C(:VL,6)*C(:VL,12)
      RB(:VL,40) = RB(:VL,40)*C(:VL,7)
C      R41: CH2O + O2 = HCO + HO2
      RF(:VL,41) = RF(:VL,41)*C(:VL,12)*C(:VL,15)
      RB(:VL,41) = RB(:VL,41)*C(:VL,16)
C      R42: CH2O + HO2 = HCO + H2O2
      RF(:VL,42) = RF(:VL,42)*C(:VL,12)*C(:VL,16)
      RB(:VL,42) = RB(:VL,42)*C(:VL,17)
C      R43: CH3 + CH2O = CH4 + HCO
      RF(:VL,43) = RF(:VL,43)*C(:VL,3)*C(:VL,12)
      RB(:VL,43) = RB(:VL,43)*C(:VL,5)
C      R44: CH3 + O = H + CH2O
      RF(:VL,44) = RF(:VL,44)*C(:VL,3)*C(:VL,4)
      RB(:VL,44) = RB(:VL,44)*C(:VL,1)*C(:VL,12)
C      R45: CH3 + O2 = O + CH3O
      RF(:VL,45) = RF(:VL,45)*C(:VL,3)*C(:VL,15)
      RB(:VL,45) = RB(:VL,45)*C(:VL,4)*C(:VL,14)
C      R46: CH3 + O2 = OH + CH2O
      RF(:VL,46) = RF(:VL,46)*C(:VL,3)*C(:VL,15)
      RB(:VL,46) = RB(:VL,46)*C(:VL,6)*C(:VL,12)
C      R47: CH3 + HO2 = OH + CH3O
      RF(:VL,47) = RF(:VL,47)*C(:VL,3)*C(:VL,16)
      RB(:VL,47) = RB(:VL,47)*C(:VL,6)*C(:VL,14)
C      R48: 2CH3 = C2H6
      CTB(:VL) = CTOT(:VL)+4D0*C(:VL,7)+C(:VL,9)+2D0*C(:VL,18)
      PR(:VL) = RKLOW(:VL,4) * CTB(:VL) / RF(:VL,48)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT0(:VL) = T(:VL)/(-5.7D2)
      CALL VRDA_EXP(VL,FCENT0,FCENT0)
      FCENT1(:VL) = 0D0
      FCENT2(:VL) = -1D30/T(:VL)
      CALL VRDA_EXP(VL,FCENT2,FCENT2)
      FCENT(:VL) = 1D0*FCENT0(:VL) +0D0*FCENT1(:VL)
     1             +FCENT2(:VL)
      FCLOG(:VL) = MAX(FCENT(:VL),SMALL)
      CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN(:VL)    = 0.75 - 1.27*FCLOG(:VL)
      CPRLOG(:VL) = PRLOG(:VL) - (0.4 + 0.67*FCLOG(:VL))
      FLOG(:VL) = CPRLOG(:VL)/(XN(:VL)-0.14*CPRLOG(:VL))
      FLOG(:VL) = 1D0+FLOG(:VL)*FLOG(:VL)
      FLOG(:VL) = FCLOG(:VL)/FLOG(:VL)
      FC(:VL) = 2.3025850929940459D0*FLOG(:VL)
      CALL VRDA_EXP(VL,FC,FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,48) = RF(:VL,48) * PCOR(:VL)
      RB(:VL,48) = RB(:VL,48) * PCOR(:VL)
      RF(:VL,48) = RF(:VL,48)*C(:VL,3)*C(:VL,3)
      RB(:VL,48) = RB(:VL,48)*C(:VL,13)
C      R49: H + CH3 = CH4
      CTB(:VL) = CTOT(:VL)+C(:VL,2)+5D0*C(:VL,7)+C(:VL,5)
     * +5D-1*C(:VL,9)+C(:VL,18)+2D0*C(:VL,13)
      PR(:VL) = RKLOW(:VL,5) * CTB(:VL) / RF(:VL,49)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT0(:VL) = T(:VL)/(-7.4D1)
      CALL VRDA_EXP(VL,FCENT0,FCENT0)
      FCENT1(:VL) = T(:VL)/(-2.941D3)
      CALL VRDA_EXP(VL,FCENT1,FCENT1)
      FCENT2(:VL) = -6.964D3/T(:VL)
      CALL VRDA_EXP(VL,FCENT2,FCENT2)
      FCENT(:VL) = 2.17D-1*FCENT0(:VL) +7.83D-1*FCENT1(:VL)
     1             +FCENT2(:VL)
      FCLOG(:VL) = MAX(FCENT(:VL),SMALL)
      CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN(:VL)    = 0.75 - 1.27*FCLOG(:VL)
      CPRLOG(:VL) = PRLOG(:VL) - (0.4 + 0.67*FCLOG(:VL))
      FLOG(:VL) = CPRLOG(:VL)/(XN(:VL)-0.14*CPRLOG(:VL))
      FLOG(:VL) = 1D0+FLOG(:VL)*FLOG(:VL)
      FLOG(:VL) = FCLOG(:VL)/FLOG(:VL)
      FC(:VL) = 2.3025850929940459D0*FLOG(:VL)
      CALL VRDA_EXP(VL,FC,FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,49) = RF(:VL,49) * PCOR(:VL)
      RB(:VL,49) = RB(:VL,49) * PCOR(:VL)
      RF(:VL,49) = RF(:VL,49)*C(:VL,1)*C(:VL,3)
      RB(:VL,49) = RB(:VL,49)*C(:VL,5)
C      R50: H + CH4 = H2 + CH3
      RF(:VL,50) = RF(:VL,50)*C(:VL,1)*C(:VL,5)
      RB(:VL,50) = RB(:VL,50)*C(:VL,2)*C(:VL,3)
C      R51: O + CH4 = CH3 + OH
      RF(:VL,51) = RF(:VL,51)*C(:VL,4)*C(:VL,5)
      RB(:VL,51) = RB(:VL,51)*C(:VL,3)*C(:VL,6)
C      R52: CH4 + OH = CH3 + H2O
      RF(:VL,52) = RF(:VL,52)*C(:VL,5)*C(:VL,6)
      RB(:VL,52) = RB(:VL,52)*C(:VL,3)*C(:VL,7)
C      R53: CH3 + HO2 = CH4 + O2
      RF(:VL,53) = RF(:VL,53)*C(:VL,3)*C(:VL,16)
      RB(:VL,53) = RB(:VL,53)*C(:VL,5)*C(:VL,15)
C      R54: CH4 + HO2 = CH3 + H2O2
      RF(:VL,54) = RF(:VL,54)*C(:VL,5)*C(:VL,16)
      RB(:VL,54) = RB(:VL,54)*C(:VL,3)*C(:VL,17)
C      R55: CH2OH = H + CH2O
      CTB(:VL) = CTOT(:VL)
      RF(:VL,55) = RF(:VL,55)*CTB(:VL)
      RB(:VL,55) = RB(:VL,55)*CTB(:VL)*C(:VL,1)*C(:VL,12)
C      R56: H + CH2OH = H2 + CH2O
      RF(:VL,56) = RF(:VL,56)*C(:VL,1)
      RB(:VL,56) = RB(:VL,56)*C(:VL,2)*C(:VL,12)
C      R57: H + CH2OH = CH3 + OH
      RF(:VL,57) = RF(:VL,57)*C(:VL,1)
      RB(:VL,57) = RB(:VL,57)*C(:VL,3)*C(:VL,6)
C      R58: O + CH2OH = OH + CH2O
      RF(:VL,58) = RF(:VL,58)*C(:VL,4)
      RB(:VL,58) = RB(:VL,58)*C(:VL,6)*C(:VL,12)
C      R59: OH + CH2OH = H2O + CH2O
      RF(:VL,59) = RF(:VL,59)*C(:VL,6)
      RB(:VL,59) = RB(:VL,59)*C(:VL,7)*C(:VL,12)
C      R60: CH2OH + O2 = CH2O + HO2
      RF(:VL,60) = RF(:VL,60)*C(:VL,15)
      RB(:VL,60) = RB(:VL,60)*C(:VL,12)*C(:VL,16)
C      R61: CH2OH + O2 = CH2O + HO2
      RF(:VL,61) = RF(:VL,61)*C(:VL,15)
      RB(:VL,61) = RB(:VL,61)*C(:VL,12)*C(:VL,16)
C      R62: CH2OH + HO2 = CH2O + H2O2
      RF(:VL,62) = RF(:VL,62)*C(:VL,16)
      RB(:VL,62) = RB(:VL,62)*C(:VL,12)*C(:VL,17)
C      R63: HCO + CH2OH = 2CH2O
      RB(:VL,63) = RB(:VL,63)*C(:VL,12)*C(:VL,12)
C      R64: CH3O = H + CH2O
      CTB(:VL) = CTOT(:VL)
      RF(:VL,64) = RF(:VL,64)*CTB(:VL)*C(:VL,14)
      RB(:VL,64) = RB(:VL,64)*CTB(:VL)*C(:VL,1)*C(:VL,12)
C      R65: H + CH3O = CH3 + OH
      RF(:VL,65) = RF(:VL,65)*C(:VL,1)*C(:VL,14)
      RB(:VL,65) = RB(:VL,65)*C(:VL,3)*C(:VL,6)
C      R66: O + CH3O = OH + CH2O
      RF(:VL,66) = RF(:VL,66)*C(:VL,4)*C(:VL,14)
      RB(:VL,66) = RB(:VL,66)*C(:VL,6)*C(:VL,12)
C      R67: OH + CH3O = H2O + CH2O
      RF(:VL,67) = RF(:VL,67)*C(:VL,6)*C(:VL,14)
      RB(:VL,67) = RB(:VL,67)*C(:VL,7)*C(:VL,12)
C      R68: CH3O + O2 = CH2O + HO2
      RF(:VL,68) = RF(:VL,68)*C(:VL,14)*C(:VL,15)
      RB(:VL,68) = RB(:VL,68)*C(:VL,12)*C(:VL,16)
C      R69: CH3O + O2 = CH2O + HO2
      RF(:VL,69) = RF(:VL,69)*C(:VL,14)*C(:VL,15)
      RB(:VL,69) = RB(:VL,69)*C(:VL,12)*C(:VL,16)
C      R70: CH3O + HO2 = CH2O + H2O2
      RF(:VL,70) = RF(:VL,70)*C(:VL,14)*C(:VL,16)
      RB(:VL,70) = RB(:VL,70)*C(:VL,12)*C(:VL,17)
C      R71: CO + CH3O = CH3 + CO2
      RF(:VL,71) = RF(:VL,71)*C(:VL,9)*C(:VL,14)
      RB(:VL,71) = RB(:VL,71)*C(:VL,3)*C(:VL,18)
C      R72: 2CH3 = H + C2H5
      RF(:VL,72) = RF(:VL,72)*C(:VL,3)*C(:VL,3)
      RB(:VL,72) = RB(:VL,72)*C(:VL,1)*C(:VL,11)
C      R73: CH2 + CH4 = 2CH3
      RF(:VL,73) = RF(:VL,73)*C(:VL,5)
      RB(:VL,73) = RB(:VL,73)*C(:VL,3)*C(:VL,3)
C      R74: CH2(S) + CH4 = 2CH3
      RF(:VL,74) = RF(:VL,74)*C(:VL,5)
      RB(:VL,74) = RB(:VL,74)*C(:VL,3)*C(:VL,3)
C      R75: CH3 + OH = CH2 + H2O
      RF(:VL,75) = RF(:VL,75)*C(:VL,3)*C(:VL,6)
      RB(:VL,75) = RB(:VL,75)*C(:VL,7)
C      R76: CH3 + OH = CH2(S) + H2O
      RF(:VL,76) = RF(:VL,76)*C(:VL,3)*C(:VL,6)
      RB(:VL,76) = RB(:VL,76)*C(:VL,7)
C      R77: CH2 + CH3 = H + C2H4
      RF(:VL,77) = RF(:VL,77)*C(:VL,3)
      RB(:VL,77) = RB(:VL,77)*C(:VL,1)*C(:VL,10)
C      R78: CH2(S) + CH3 = H + C2H4
      RF(:VL,78) = RF(:VL,78)*C(:VL,3)
      RB(:VL,78) = RB(:VL,78)*C(:VL,1)*C(:VL,10)
C      R79: H + CH3O = CH2(S) + H2O
      RF(:VL,79) = RF(:VL,79)*C(:VL,1)*C(:VL,14)
      RB(:VL,79) = RB(:VL,79)*C(:VL,7)
C      R80: H + C2H6 = H2 + C2H5
      RF(:VL,80) = RF(:VL,80)*C(:VL,1)*C(:VL,13)
      RB(:VL,80) = RB(:VL,80)*C(:VL,2)*C(:VL,11)
C      R81: O + C2H6 = OH + C2H5
      RF(:VL,81) = RF(:VL,81)*C(:VL,4)*C(:VL,13)
      RB(:VL,81) = RB(:VL,81)*C(:VL,6)*C(:VL,11)
C      R82: OH + C2H6 = H2O + C2H5
      RF(:VL,82) = RF(:VL,82)*C(:VL,6)*C(:VL,13)
      RB(:VL,82) = RB(:VL,82)*C(:VL,7)*C(:VL,11)
C      R83: C2H6 + O2 = C2H5 + HO2
      RF(:VL,83) = RF(:VL,83)*C(:VL,13)*C(:VL,15)
      RB(:VL,83) = RB(:VL,83)*C(:VL,11)*C(:VL,16)
C      R84: C2H6 + HO2 = C2H5 + H2O2
      RF(:VL,84) = RF(:VL,84)*C(:VL,13)*C(:VL,16)
      RB(:VL,84) = RB(:VL,84)*C(:VL,11)*C(:VL,17)
C      R85: CH3 + C2H6 = CH4 + C2H5
      RF(:VL,85) = RF(:VL,85)*C(:VL,3)*C(:VL,13)
      RB(:VL,85) = RB(:VL,85)*C(:VL,5)*C(:VL,11)
C      R86: H + C2H5 = C2H6
      CTB(:VL) = CTOT(:VL)+C(:VL,2)+5D0*C(:VL,7)+C(:VL,5)
     * +5D-1*C(:VL,9)+C(:VL,18)+2D0*C(:VL,13)
      PR(:VL) = RKLOW(:VL,6) * CTB(:VL) / RF(:VL,86)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT0(:VL) = T(:VL)/(-1.25D2)
      CALL VRDA_EXP(VL,FCENT0,FCENT0)
      FCENT1(:VL) = T(:VL)/(-2.219D3)
      CALL VRDA_EXP(VL,FCENT1,FCENT1)
      FCENT2(:VL) = -6.882D3/T(:VL)
      CALL VRDA_EXP(VL,FCENT2,FCENT2)
      FCENT(:VL) = 1.578D-1*FCENT0(:VL) +8.422D-1*FCENT1(:VL)
     1             +FCENT2(:VL)
      FCLOG(:VL) = MAX(FCENT(:VL),SMALL)
      CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN(:VL)    = 0.75 - 1.27*FCLOG(:VL)
      CPRLOG(:VL) = PRLOG(:VL) - (0.4 + 0.67*FCLOG(:VL))
      FLOG(:VL) = CPRLOG(:VL)/(XN(:VL)-0.14*CPRLOG(:VL))
      FLOG(:VL) = 1D0+FLOG(:VL)*FLOG(:VL)
      FLOG(:VL) = FCLOG(:VL)/FLOG(:VL)
      FC(:VL) = 2.3025850929940459D0*FLOG(:VL)
      CALL VRDA_EXP(VL,FC,FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,86) = RF(:VL,86) * PCOR(:VL)
      RB(:VL,86) = RB(:VL,86) * PCOR(:VL)
      RF(:VL,86) = RF(:VL,86)*C(:VL,1)*C(:VL,11)
      RB(:VL,86) = RB(:VL,86)*C(:VL,13)
C      R87: H + C2H5 = H2 + C2H4
      RF(:VL,87) = RF(:VL,87)*C(:VL,1)*C(:VL,11)
      RB(:VL,87) = RB(:VL,87)*C(:VL,2)*C(:VL,10)
C      R88: O + C2H5 = CH3 + CH2O
      RF(:VL,88) = RF(:VL,88)*C(:VL,4)*C(:VL,11)
      RB(:VL,88) = RB(:VL,88)*C(:VL,3)*C(:VL,12)
C      R89: C2H5 + O2 = C2H4 + HO2
      RF(:VL,89) = RF(:VL,89)*C(:VL,11)*C(:VL,15)
      RB(:VL,89) = RB(:VL,89)*C(:VL,10)*C(:VL,16)
C      R90: 2C2H5 = C2H4 + C2H6
      RF(:VL,90) = RF(:VL,90)*C(:VL,11)*C(:VL,11)
      RB(:VL,90) = RB(:VL,90)*C(:VL,10)*C(:VL,13)
C      R91: HCO + C2H5 = CO + C2H6
      RF(:VL,91) = RF(:VL,91)*C(:VL,11)
      RB(:VL,91) = RB(:VL,91)*C(:VL,9)*C(:VL,13)
C      R92: O + C2H5 = H + CH3HCO
      RF(:VL,92) = RF(:VL,92)*C(:VL,4)*C(:VL,11)
      RB(:VL,92) = RB(:VL,92)*C(:VL,1)*C(:VL,19)
C      R93: C2H4 = H2 + C2H2
      CTB(:VL) = CTOT(:VL)+C(:VL,2)+5D0*C(:VL,7)+C(:VL,5)
     * +5D-1*C(:VL,9)+C(:VL,18)+2D0*C(:VL,13)
      PR(:VL) = RKLOW(:VL,7) * CTB(:VL) / RF(:VL,93)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT0(:VL) = T(:VL)/(-1.8D2)
      CALL VRDA_EXP(VL,FCENT0,FCENT0)
      FCENT1(:VL) = T(:VL)/(-1.035D3)
      CALL VRDA_EXP(VL,FCENT1,FCENT1)
      FCENT2(:VL) = -5.417D3/T(:VL)
      CALL VRDA_EXP(VL,FCENT2,FCENT2)
      FCENT(:VL) = 2.655D-1*FCENT0(:VL) +7.345D-1*FCENT1(:VL)
     1             +FCENT2(:VL)
      FCLOG(:VL) = MAX(FCENT(:VL),SMALL)
      CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN(:VL)    = 0.75 - 1.27*FCLOG(:VL)
      CPRLOG(:VL) = PRLOG(:VL) - (0.4 + 0.67*FCLOG(:VL))
      FLOG(:VL) = CPRLOG(:VL)/(XN(:VL)-0.14*CPRLOG(:VL))
      FLOG(:VL) = 1D0+FLOG(:VL)*FLOG(:VL)
      FLOG(:VL) = FCLOG(:VL)/FLOG(:VL)
      FC(:VL) = 2.3025850929940459D0*FLOG(:VL)
      CALL VRDA_EXP(VL,FC,FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,93) = RF(:VL,93) * PCOR(:VL)
      RB(:VL,93) = RB(:VL,93) * PCOR(:VL)
      RF(:VL,93) = RF(:VL,93)*C(:VL,10)
      RB(:VL,93) = RB(:VL,93)*C(:VL,2)*C(:VL,8)
C      R94: H + C2H4 = C2H5
      CTB(:VL) = CTOT(:VL)+C(:VL,2)+5D0*C(:VL,7)+C(:VL,5)
     * +5D-1*C(:VL,9)+C(:VL,18)+2D0*C(:VL,13)
      PR(:VL) = RKLOW(:VL,8) * CTB(:VL) / RF(:VL,94)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT0(:VL) = T(:VL)/(-2.1D2)
      CALL VRDA_EXP(VL,FCENT0,FCENT0)
      FCENT1(:VL) = T(:VL)/(-9.84D2)
      CALL VRDA_EXP(VL,FCENT1,FCENT1)
      FCENT2(:VL) = -4.374D3/T(:VL)
      CALL VRDA_EXP(VL,FCENT2,FCENT2)
      FCENT(:VL) = 2.47D-2*FCENT0(:VL) +9.753D-1*FCENT1(:VL)
     1             +FCENT2(:VL)
      FCLOG(:VL) = MAX(FCENT(:VL),SMALL)
      CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN(:VL)    = 0.75 - 1.27*FCLOG(:VL)
      CPRLOG(:VL) = PRLOG(:VL) - (0.4 + 0.67*FCLOG(:VL))
      FLOG(:VL) = CPRLOG(:VL)/(XN(:VL)-0.14*CPRLOG(:VL))
      FLOG(:VL) = 1D0+FLOG(:VL)*FLOG(:VL)
      FLOG(:VL) = FCLOG(:VL)/FLOG(:VL)
      FC(:VL) = 2.3025850929940459D0*FLOG(:VL)
      CALL VRDA_EXP(VL,FC,FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,94) = RF(:VL,94) * PCOR(:VL)
      RB(:VL,94) = RB(:VL,94) * PCOR(:VL)
      RF(:VL,94) = RF(:VL,94)*C(:VL,1)*C(:VL,10)
      RB(:VL,94) = RB(:VL,94)*C(:VL,11)
C      R95: H + C2H3 = C2H4
      CTB(:VL) = CTOT(:VL)+C(:VL,2)+5D0*C(:VL,7)+C(:VL,5)
     * +5D-1*C(:VL,9)+C(:VL,18)+2D0*C(:VL,13)
      PR(:VL) = RKLOW(:VL,9) * CTB(:VL) / RF(:VL,95)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT0(:VL) = T(:VL)/(-2.075D2)
      CALL VRDA_EXP(VL,FCENT0,FCENT0)
      FCENT1(:VL) = T(:VL)/(-2.663D3)
      CALL VRDA_EXP(VL,FCENT1,FCENT1)
      FCENT2(:VL) = -6.095D3/T(:VL)
      CALL VRDA_EXP(VL,FCENT2,FCENT2)
      FCENT(:VL) = 2.18D-1*FCENT0(:VL) +7.82D-1*FCENT1(:VL)
     1             +FCENT2(:VL)
      FCLOG(:VL) = MAX(FCENT(:VL),SMALL)
      CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN(:VL)    = 0.75 - 1.27*FCLOG(:VL)
      CPRLOG(:VL) = PRLOG(:VL) - (0.4 + 0.67*FCLOG(:VL))
      FLOG(:VL) = CPRLOG(:VL)/(XN(:VL)-0.14*CPRLOG(:VL))
      FLOG(:VL) = 1D0+FLOG(:VL)*FLOG(:VL)
      FLOG(:VL) = FCLOG(:VL)/FLOG(:VL)
      FC(:VL) = 2.3025850929940459D0*FLOG(:VL)
      CALL VRDA_EXP(VL,FC,FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,95) = RF(:VL,95) * PCOR(:VL)
      RB(:VL,95) = RB(:VL,95) * PCOR(:VL)
      RF(:VL,95) = RF(:VL,95)*C(:VL,1)
      RB(:VL,95) = RB(:VL,95)*C(:VL,10)
C      R96: H + C2H4 = H2 + C2H3
      RF(:VL,96) = RF(:VL,96)*C(:VL,1)*C(:VL,10)
      RB(:VL,96) = RB(:VL,96)*C(:VL,2)
C      R97: OH + C2H4 = H2O + C2H3
      RF(:VL,97) = RF(:VL,97)*C(:VL,6)*C(:VL,10)
      RB(:VL,97) = RB(:VL,97)*C(:VL,7)
C      R98: CH3 + C2H4 = CH4 + C2H3
      RF(:VL,98) = RF(:VL,98)*C(:VL,3)*C(:VL,10)
      RB(:VL,98) = RB(:VL,98)*C(:VL,5)
C      R99: O + C2H4 = CH3 + HCO
      RF(:VL,99) = RF(:VL,99)*C(:VL,4)*C(:VL,10)
      RB(:VL,99) = RB(:VL,99)*C(:VL,3)
C      R100: OH + C2H3 = H2O + C2H2
      RF(:VL,100) = RF(:VL,100)*C(:VL,6)
      RB(:VL,100) = RB(:VL,100)*C(:VL,7)*C(:VL,8)
C      R101: O + C2H4 = OH + C2H3
      RF(:VL,101) = RF(:VL,101)*C(:VL,4)*C(:VL,10)
      RB(:VL,101) = RB(:VL,101)*C(:VL,6)
C      R102: C2H4 + O2 = C2H3 + HO2
      RF(:VL,102) = RF(:VL,102)*C(:VL,10)*C(:VL,15)
      RB(:VL,102) = RB(:VL,102)*C(:VL,16)
C      R103: H + C2H3 = H2 + C2H2
      RF(:VL,103) = RF(:VL,103)*C(:VL,1)
      RB(:VL,103) = RB(:VL,103)*C(:VL,2)*C(:VL,8)
C      R104: C2H3 + H2O2 = C2H4 + HO2
      RF(:VL,104) = RF(:VL,104)*C(:VL,17)
      RB(:VL,104) = RB(:VL,104)*C(:VL,10)*C(:VL,16)
C      R105: CH3 + C2H3 = CH4 + C2H2
      RF(:VL,105) = RF(:VL,105)*C(:VL,3)
      RB(:VL,105) = RB(:VL,105)*C(:VL,5)*C(:VL,8)
C      R106: 2C2H3 = C2H2 + C2H4
      RB(:VL,106) = RB(:VL,106)*C(:VL,8)*C(:VL,10)
C      R107: C2H3 + O2 = HCO + CH2O
      RF(:VL,107) = RF(:VL,107)*C(:VL,15)
      RB(:VL,107) = RB(:VL,107)*C(:VL,12)
C      R108: C2H3 + O2 = C2H2 + HO2
      RF(:VL,108) = RF(:VL,108)*C(:VL,15)
      RB(:VL,108) = RB(:VL,108)*C(:VL,8)*C(:VL,16)
C      R109: H + C2H2 = C2H3
      CTB(:VL) = CTOT(:VL)+C(:VL,2)+5D0*C(:VL,7)+C(:VL,5)
     * +5D-1*C(:VL,9)+C(:VL,18)+2D0*C(:VL,13)
      PR(:VL) = RKLOW(:VL,10) * CTB(:VL) / RF(:VL,109)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT0(:VL) = T(:VL)/(-9.85D1)
      CALL VRDA_EXP(VL,FCENT0,FCENT0)
      FCENT1(:VL) = T(:VL)/(-1.302D3)
      CALL VRDA_EXP(VL,FCENT1,FCENT1)
      FCENT2(:VL) = -4.167D3/T(:VL)
      CALL VRDA_EXP(VL,FCENT2,FCENT2)
      FCENT(:VL) = 2.493D-1*FCENT0(:VL) +7.507D-1*FCENT1(:VL)
     1             +FCENT2(:VL)
      FCLOG(:VL) = MAX(FCENT(:VL),SMALL)
      CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN(:VL)    = 0.75 - 1.27*FCLOG(:VL)
      CPRLOG(:VL) = PRLOG(:VL) - (0.4 + 0.67*FCLOG(:VL))
      FLOG(:VL) = CPRLOG(:VL)/(XN(:VL)-0.14*CPRLOG(:VL))
      FLOG(:VL) = 1D0+FLOG(:VL)*FLOG(:VL)
      FLOG(:VL) = FCLOG(:VL)/FLOG(:VL)
      FC(:VL) = 2.3025850929940459D0*FLOG(:VL)
      CALL VRDA_EXP(VL,FC,FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,109) = RF(:VL,109) * PCOR(:VL)
      RB(:VL,109) = RB(:VL,109) * PCOR(:VL)
      RF(:VL,109) = RF(:VL,109)*C(:VL,1)*C(:VL,8)
C      R110: O + C2H2 = CH2 + CO
      RF(:VL,110) = RF(:VL,110)*C(:VL,4)*C(:VL,8)
      RB(:VL,110) = RB(:VL,110)*C(:VL,9)
C      R111: OH + C2H2 = CH3 + CO
      RF(:VL,111) = RF(:VL,111)*C(:VL,6)*C(:VL,8)
      RB(:VL,111) = RB(:VL,111)*C(:VL,3)*C(:VL,9)
C      R112: H + CH2 = CH3
      CTB(:VL) = CTOT(:VL)+C(:VL,2)+5D0*C(:VL,7)+C(:VL,5)
     * +5D-1*C(:VL,9)+C(:VL,18)+2D0*C(:VL,13)
      PR(:VL) = RKLOW(:VL,11) * CTB(:VL) / RF(:VL,112)
      PCOR(:VL) = PR(:VL) / (1.0 + PR(:VL))
      PRLOG(:VL) = MAX(PR(:VL),SMALL)
      CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT0(:VL) = T(:VL)/(-7.8D1)
      CALL VRDA_EXP(VL,FCENT0,FCENT0)
      FCENT1(:VL) = T(:VL)/(-1.995D3)
      CALL VRDA_EXP(VL,FCENT1,FCENT1)
      FCENT2(:VL) = -5.59D3/T(:VL)
      CALL VRDA_EXP(VL,FCENT2,FCENT2)
      FCENT(:VL) = 3.2D-1*FCENT0(:VL) +6.8D-1*FCENT1(:VL)
     1             +FCENT2(:VL)
      FCLOG(:VL) = MAX(FCENT(:VL),SMALL)
      CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN(:VL)    = 0.75 - 1.27*FCLOG(:VL)
      CPRLOG(:VL) = PRLOG(:VL) - (0.4 + 0.67*FCLOG(:VL))
      FLOG(:VL) = CPRLOG(:VL)/(XN(:VL)-0.14*CPRLOG(:VL))
      FLOG(:VL) = 1D0+FLOG(:VL)*FLOG(:VL)
      FLOG(:VL) = FCLOG(:VL)/FLOG(:VL)
      FC(:VL) = 2.3025850929940459D0*FLOG(:VL)
      CALL VRDA_EXP(VL,FC,FC)
      PCOR(:VL) = FC(:VL) * PCOR(:VL)
      RF(:VL,112) = RF(:VL,112) * PCOR(:VL)
      RB(:VL,112) = RB(:VL,112) * PCOR(:VL)
      RF(:VL,112) = RF(:VL,112)*C(:VL,1)
      RB(:VL,112) = RB(:VL,112)*C(:VL,3)
C      R113: CH2 + O = H + HCO
      RF(:VL,113) = RF(:VL,113)*C(:VL,4)
      RB(:VL,113) = RB(:VL,113)*C(:VL,1)
C      R114: CH2 + OH = H + CH2O
      RF(:VL,114) = RF(:VL,114)*C(:VL,6)
      RB(:VL,114) = RB(:VL,114)*C(:VL,1)*C(:VL,12)
C      R115: H2 + CH2 = H + CH3
      RF(:VL,115) = RF(:VL,115)*C(:VL,2)
      RB(:VL,115) = RB(:VL,115)*C(:VL,1)*C(:VL,3)
C      R116: CH2 + O2 = OH + HCO
      RF(:VL,116) = RF(:VL,116)*C(:VL,15)
      RB(:VL,116) = RB(:VL,116)*C(:VL,6)
C      R117: CH2 + HO2 = OH + CH2O
      RF(:VL,117) = RF(:VL,117)*C(:VL,16)
      RB(:VL,117) = RB(:VL,117)*C(:VL,6)*C(:VL,12)
C      R118: 2CH2 = H2 + C2H2
      RB(:VL,118) = RB(:VL,118)*C(:VL,2)*C(:VL,8)
C      R119: CH2(S) = CH2
      CTB(:VL) = CTOT(:VL)-C(:VL,7)-C(:VL,9)-C(:VL,18)
      RF(:VL,119) = RF(:VL,119)*CTB(:VL)
      RB(:VL,119) = RB(:VL,119)*CTB(:VL)
C      R120: CH2(S) = CH2
      RF(:VL,120) = RF(:VL,120)*C(:VL,7)
      RB(:VL,120) = RB(:VL,120)*C(:VL,7)
C      R121: CH2(S) = CH2
      RF(:VL,121) = RF(:VL,121)*C(:VL,9)
      RB(:VL,121) = RB(:VL,121)*C(:VL,9)
C      R122: CH2(S) = CH2
      RF(:VL,122) = RF(:VL,122)*C(:VL,18)
      RB(:VL,122) = RB(:VL,122)*C(:VL,18)
C      R123: CH2(S) + O = H2 + CO
      RF(:VL,123) = RF(:VL,123)*C(:VL,4)
      RB(:VL,123) = RB(:VL,123)*C(:VL,2)*C(:VL,9)
C      R124: CH2(S) + O = H + HCO
      RF(:VL,124) = RF(:VL,124)*C(:VL,4)
      RB(:VL,124) = RB(:VL,124)*C(:VL,1)
C      R125: CH2(S) + OH = H + CH2O
      RF(:VL,125) = RF(:VL,125)*C(:VL,6)
      RB(:VL,125) = RB(:VL,125)*C(:VL,1)*C(:VL,12)
C      R126: H2 + CH2(S) = H + CH3
      RF(:VL,126) = RF(:VL,126)*C(:VL,2)
      RB(:VL,126) = RB(:VL,126)*C(:VL,1)*C(:VL,3)
C      R127: CH2(S) + O2 = H + OH + CO
      RF(:VL,127) = RF(:VL,127)*C(:VL,15)
      RB(:VL,127) = RB(:VL,127)*C(:VL,1)*C(:VL,6)*C(:VL,9)
C      R128: CH2(S) + O2 = H2O + CO
      RF(:VL,128) = RF(:VL,128)*C(:VL,15)
      RB(:VL,128) = RB(:VL,128)*C(:VL,7)*C(:VL,9)
C      R129: CH2(S) + CO2 = CO + CH2O
      RF(:VL,129) = RF(:VL,129)*C(:VL,18)
      RB(:VL,129) = RB(:VL,129)*C(:VL,9)*C(:VL,12)
C      R130: CH3HCO = CH3 + HCO
      RF(:VL,130) = RF(:VL,130)*C(:VL,19)
      RB(:VL,130) = RB(:VL,130)*C(:VL,3)
C      R131: CH3OCH3 = CH3 + CH3O
      RF(:VL,131) = RF(:VL,131)*C(:VL,21)
      RB(:VL,131) = RB(:VL,131)*C(:VL,3)*C(:VL,14)
C      R132: OH + CH3OCH3 = H2O + CH3OCH2
      RF(:VL,132) = RF(:VL,132)*C(:VL,6)*C(:VL,21)
      RB(:VL,132) = RB(:VL,132)*C(:VL,7)
C      R133: H + CH3OCH3 = H2 + CH3OCH2
      RF(:VL,133) = RF(:VL,133)*C(:VL,1)*C(:VL,21)
      RB(:VL,133) = RB(:VL,133)*C(:VL,2)
C      R134: CH3 + CH3OCH3 = CH4 + CH3OCH2
      RF(:VL,134) = RF(:VL,134)*C(:VL,3)*C(:VL,21)
      RB(:VL,134) = RB(:VL,134)*C(:VL,5)
C      R135: O + CH3OCH3 = OH + CH3OCH2
      RF(:VL,135) = RF(:VL,135)*C(:VL,4)*C(:VL,21)
      RB(:VL,135) = RB(:VL,135)*C(:VL,6)
C      R136: HO2 + CH3OCH3 = H2O2 + CH3OCH2
      RF(:VL,136) = RF(:VL,136)*C(:VL,16)*C(:VL,21)
      RB(:VL,136) = RB(:VL,136)*C(:VL,17)
C      R137: O2 + CH3OCH3 = HO2 + CH3OCH2
      RF(:VL,137) = RF(:VL,137)*C(:VL,15)*C(:VL,21)
      RB(:VL,137) = RB(:VL,137)*C(:VL,16)
C      R138: CH3OCH2 = CH3 + CH2O
      RB(:VL,138) = RB(:VL,138)*C(:VL,3)*C(:VL,12)
C      R139: CH3O + CH3OCH2 = CH2O + CH3OCH3
      RF(:VL,139) = RF(:VL,139)*C(:VL,14)
      RB(:VL,139) = RB(:VL,139)*C(:VL,12)*C(:VL,21)
C      R140: CH2O + CH3OCH2 = HCO + CH3OCH3
      RF(:VL,140) = RF(:VL,140)*C(:VL,12)
      RB(:VL,140) = RB(:VL,140)*C(:VL,21)
C      R141: HO2 + CH3OCH2 = OH + CH3OCH2O
      RF(:VL,141) = RF(:VL,141)*C(:VL,16)
      RB(:VL,141) = RB(:VL,141)*C(:VL,6)
C      R142: CH3OCH2O = H + CH3OCHO
      RB(:VL,142) = RB(:VL,142)*C(:VL,1)*C(:VL,23)
C      R143: O2 + CH3OCHO = HO2 + CH3OCO
      RF(:VL,143) = RF(:VL,143)*C(:VL,15)*C(:VL,23)
      RB(:VL,143) = RB(:VL,143)*C(:VL,16)*C(:VL,22)
C      R144: OH + CH3OCHO = H2O + CH3OCO
      RF(:VL,144) = RF(:VL,144)*C(:VL,6)*C(:VL,23)
      RB(:VL,144) = RB(:VL,144)*C(:VL,7)*C(:VL,22)
C      R145: HO2 + CH3OCHO = H2O2 + CH3OCO
      RF(:VL,145) = RF(:VL,145)*C(:VL,16)*C(:VL,23)
      RB(:VL,145) = RB(:VL,145)*C(:VL,17)*C(:VL,22)
C      R146: O + CH3OCHO = OH + CH3OCO
      RF(:VL,146) = RF(:VL,146)*C(:VL,4)*C(:VL,23)
      RB(:VL,146) = RB(:VL,146)*C(:VL,6)*C(:VL,22)
C      R147: H + CH3OCHO = H2 + CH3OCO
      RF(:VL,147) = RF(:VL,147)*C(:VL,1)*C(:VL,23)
      RB(:VL,147) = RB(:VL,147)*C(:VL,2)*C(:VL,22)
C      R148: CH3 + CH3OCHO = CH4 + CH3OCO
      RF(:VL,148) = RF(:VL,148)*C(:VL,3)*C(:VL,23)
      RB(:VL,148) = RB(:VL,148)*C(:VL,5)*C(:VL,22)
C      R149: CH3OCO = CO + CH3O
      RF(:VL,149) = RF(:VL,149)*C(:VL,22)
      RB(:VL,149) = RB(:VL,149)*C(:VL,9)*C(:VL,14)
C      R150: CH3OCO = CH3 + CO2
      RF(:VL,150) = RF(:VL,150)*C(:VL,22)
      RB(:VL,150) = RB(:VL,150)*C(:VL,3)*C(:VL,18)
C      R151: O2 + CH3OCH2 = CH3OCH2O2
      RF(:VL,151) = RF(:VL,151)*C(:VL,15)
      RB(:VL,151) = RB(:VL,151)*C(:VL,27)
C      R152: 2CH3OCH2O2 = O2 + 2CH3OCH2O
      RF(:VL,152) = RF(:VL,152)*C(:VL,27)*C(:VL,27)
      RB(:VL,152) = RB(:VL,152)*C(:VL,15)
C      R153: 2CH3OCH2O2 = O2 + CH3OCHO + CH3OCH2OH
      RF(:VL,153) = RF(:VL,153)*C(:VL,27)*C(:VL,27)
      RB(:VL,153) = RB(:VL,153)*C(:VL,15)*C(:VL,23)*C(:VL,24)
C      R154: CH3OCH2O = CH2O + CH3O
      RB(:VL,154) = RB(:VL,154)*C(:VL,12)*C(:VL,14)
C      R155: O2 + CH3OCH2O = HO2 + CH3OCHO
      RF(:VL,155) = RF(:VL,155)*C(:VL,15)
      RB(:VL,155) = RB(:VL,155)*C(:VL,16)*C(:VL,23)
C      R156: CH3OCH2O2 = CH2OCH2O2H
      RF(:VL,156) = RF(:VL,156)*C(:VL,27)
C      R157: CH2OCH2O2H = OH + 2CH2O
      RB(:VL,157) = RB(:VL,157)*C(:VL,6)*C(:VL,12)*C(:VL,12)
C      R158: O2 + CH2OCH2O2H = O2CH2OCH2O2H
      RF(:VL,158) = RF(:VL,158)*C(:VL,15)
      RB(:VL,158) = RB(:VL,158)*C(:VL,29)
C      R159: O2CH2OCH2O2H = OH + HO2CH2OCHO
      RF(:VL,159) = RF(:VL,159)*C(:VL,29)
      RB(:VL,159) = RB(:VL,159)*C(:VL,6)*C(:VL,28)
C      R160: HO2CH2OCHO = OH + OCH2OCHO
      RF(:VL,160) = RF(:VL,160)*C(:VL,28)
      RB(:VL,160) = RB(:VL,160)*C(:VL,6)*C(:VL,25)
C      R161: OCH2OCHO = HOCH2OCO
      RF(:VL,161) = RF(:VL,161)*C(:VL,25)
      RB(:VL,161) = RB(:VL,161)*C(:VL,26)
C      R162: HOCH2OCO = CO + HOCH2O
      RF(:VL,162) = RF(:VL,162)*C(:VL,26)
      RB(:VL,162) = RB(:VL,162)*C(:VL,9)
C      R163: HOCH2OCO = CH2OH + CO2
      RF(:VL,163) = RF(:VL,163)*C(:VL,26)
      RB(:VL,163) = RB(:VL,163)*C(:VL,18)
C      R164: HOCH2O = H + HCOOH
      RB(:VL,164) = RB(:VL,164)*C(:VL,1)*C(:VL,20)
C      R165: OH + CH2O = HOCH2O
      RF(:VL,165) = RF(:VL,165)*C(:VL,6)*C(:VL,12)
C      R166: HCOOH = H2O + CO
      CTB(:VL) = CTOT(:VL)
      RF(:VL,166) = RF(:VL,166)*CTB(:VL)*C(:VL,20)
      RB(:VL,166) = RB(:VL,166)*CTB(:VL)*C(:VL,7)*C(:VL,9)
C      R167: HCOOH = H2 + CO2
      CTB(:VL) = CTOT(:VL)
      RF(:VL,167) = RF(:VL,167)*CTB(:VL)*C(:VL,20)
      RB(:VL,167) = RB(:VL,167)*CTB(:VL)*C(:VL,2)*C(:VL,18)
C      R168: HCOOH = OH + HCO
      RF(:VL,168) = RF(:VL,168)*C(:VL,20)
      RB(:VL,168) = RB(:VL,168)*C(:VL,6)
C      R169: OH + HCOOH = H + H2O + CO2
      RF(:VL,169) = RF(:VL,169)*C(:VL,6)*C(:VL,20)
      RB(:VL,169) = RB(:VL,169)*C(:VL,1)*C(:VL,7)*C(:VL,18)
C      R170: HCOOH = H2O + CO
      RF(:VL,170) = RF(:VL,170)*C(:VL,6)*C(:VL,20)
      RB(:VL,170) = RB(:VL,170)*C(:VL,6)*C(:VL,7)*C(:VL,9)
C      R171: HCOOH = H2 + CO2
      RF(:VL,171) = RF(:VL,171)*C(:VL,1)*C(:VL,20)
      RB(:VL,171) = RB(:VL,171)*C(:VL,1)*C(:VL,2)*C(:VL,18)
C      R172: H + HCOOH = H2 + OH + CO
      RF(:VL,172) = RF(:VL,172)*C(:VL,1)*C(:VL,20)
      RB(:VL,172) = RB(:VL,172)*C(:VL,2)*C(:VL,6)*C(:VL,9)
C      R173: CH3 + HCOOH = CH4 + OH + CO
      RF(:VL,173) = RF(:VL,173)*C(:VL,3)*C(:VL,20)
      RB(:VL,173) = RB(:VL,173)*C(:VL,5)*C(:VL,6)*C(:VL,9)
C      R174: HO2 + HCOOH = OH + CO + H2O2
      RF(:VL,174) = RF(:VL,174)*C(:VL,16)*C(:VL,20)
      RB(:VL,174) = RB(:VL,174)*C(:VL,6)*C(:VL,9)*C(:VL,17)
C      R175: O + HCOOH = 2OH + CO
      RF(:VL,175) = RF(:VL,175)*C(:VL,4)*C(:VL,20)
      RB(:VL,175) = RB(:VL,175)*C(:VL,6)*C(:VL,6)*C(:VL,9)
C
      END

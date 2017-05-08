C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE RATT (VL, T, RF, RB, RKLOW)
      USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      PARAMETER (RU=8.31451D7, SMALL=1.D-200, PATM=1.01325D6)
      DIMENSION RF(MAXVL,*), RB(MAXVL,*), RKLOW(MAXVL,*)
      DIMENSION EQK(MAXVL), PFAC1(MAXVL), PFAC2(MAXVL), PFAC3(MAXVL)
      DIMENSION T(MAXVL),ALOGT(MAXVL)
        INTEGER VL
      DIMENSION SMH(MAXVL,38), EG(MAXVL,38)
C
      CALL VRDA_LOG(VL,T,ALOGT)
C
      CALL RDSMH (VL, T, SMH)
      CALL VRDA_EXP(VL,SMH(1,1),EG(1,1))
      CALL VRDA_EXP(VL,SMH(1,2),EG(1,2))
      CALL VRDA_EXP(VL,SMH(1,3),EG(1,3))
      CALL VRDA_EXP(VL,SMH(1,4),EG(1,4))
      CALL VRDA_EXP(VL,SMH(1,5),EG(1,5))
      CALL VRDA_EXP(VL,SMH(1,6),EG(1,6))
      CALL VRDA_EXP(VL,SMH(1,7),EG(1,7))
      CALL VRDA_EXP(VL,SMH(1,8),EG(1,8))
      CALL VRDA_EXP(VL,SMH(1,9),EG(1,9))
      CALL VRDA_EXP(VL,SMH(1,10),EG(1,10))
      CALL VRDA_EXP(VL,SMH(1,11),EG(1,11))
      CALL VRDA_EXP(VL,SMH(1,12),EG(1,12))
      CALL VRDA_EXP(VL,SMH(1,13),EG(1,13))
      CALL VRDA_EXP(VL,SMH(1,14),EG(1,14))
      CALL VRDA_EXP(VL,SMH(1,15),EG(1,15))
      CALL VRDA_EXP(VL,SMH(1,16),EG(1,16))
      CALL VRDA_EXP(VL,SMH(1,17),EG(1,17))
      CALL VRDA_EXP(VL,SMH(1,18),EG(1,18))
      CALL VRDA_EXP(VL,SMH(1,19),EG(1,19))
      CALL VRDA_EXP(VL,SMH(1,20),EG(1,20))
      CALL VRDA_EXP(VL,SMH(1,21),EG(1,21))
      CALL VRDA_EXP(VL,SMH(1,22),EG(1,22))
      CALL VRDA_EXP(VL,SMH(1,23),EG(1,23))
      CALL VRDA_EXP(VL,SMH(1,24),EG(1,24))
      CALL VRDA_EXP(VL,SMH(1,25),EG(1,25))
      CALL VRDA_EXP(VL,SMH(1,26),EG(1,26))
      CALL VRDA_EXP(VL,SMH(1,27),EG(1,27))
      CALL VRDA_EXP(VL,SMH(1,28),EG(1,28))
      CALL VRDA_EXP(VL,SMH(1,29),EG(1,29))
      CALL VRDA_EXP(VL,SMH(1,30),EG(1,30))
      CALL VRDA_EXP(VL,SMH(1,31),EG(1,31))
      CALL VRDA_EXP(VL,SMH(1,32),EG(1,32))
      CALL VRDA_EXP(VL,SMH(1,33),EG(1,33))
      CALL VRDA_EXP(VL,SMH(1,34),EG(1,34))
      CALL VRDA_EXP(VL,SMH(1,35),EG(1,35))
      CALL VRDA_EXP(VL,SMH(1,36),EG(1,36))
      CALL VRDA_EXP(VL,SMH(1,37),EG(1,37))
      CALL VRDA_EXP(VL,SMH(1,38),EG(1,38))
      PFAC1(:VL) = PATM / (RU*T(:VL))
      PFAC2(:VL) = PFAC1(:VL)*PFAC1(:VL)
      PFAC3(:VL) = PFAC2(:VL)*PFAC1(:VL)
C
C
C      R1: H + O2 = O + OH
      RF(:VL,1) = 3.58048786D1 -4.06D-1*ALOGT(:VL)
     * -8.35289347D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,1),RF(1,1))
      EQK(:VL) = EG(:VL,6)*EG(:VL,8)/EG(:VL,1)/EG(:VL,20)
      RB(:VL,1) = RF(:VL,1) / MAX(EQK(:VL), SMALL)
C      R2: H2 + O = H + OH
      RF(:VL,2) = 1.08356516D1 +2.67D0*ALOGT(:VL)
     * -3.16523284D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,2),RF(1,2))
      EQK(:VL) = EG(:VL,1)*EG(:VL,8)/EG(:VL,2)/EG(:VL,6)
      RB(:VL,2) = RF(:VL,2) / MAX(EQK(:VL), SMALL)
C      R3: H2 + OH = H + H2O
      RF(:VL,3) = 1.9190789D1 +1.51D0*ALOGT(:VL)
     * -1.72603317D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,3),RF(1,3))
      EQK(:VL) = EG(:VL,1)*EG(:VL,9)/EG(:VL,2)/EG(:VL,8)
      RB(:VL,3) = RF(:VL,3) / MAX(EQK(:VL), SMALL)
C      R4: O + H2O = 2OH
      RF(:VL,4) = 1.49040725D1 +2.02D0*ALOGT(:VL)
     * -6.74310335D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,4),RF(1,4))
      EQK(:VL) = EG(:VL,8)*EG(:VL,8)/EG(:VL,6)/EG(:VL,9)
      RB(:VL,4) = RF(:VL,4) / MAX(EQK(:VL), SMALL)
C      R5: H2 = 2H
      RF(:VL,5) = 4.52701605D1 -1.4D0*ALOGT(:VL)
     * -5.25257558D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,5),RF(1,5))
      EQK(:VL) = EG(:VL,1)*EG(:VL,1)/EG(:VL,2)*PFAC1(:VL)
      RB(:VL,5) = RF(:VL,5) / MAX(EQK(:VL), SMALL)
C      R6: 2O = O2
      RF(:VL,6) = 3.63576645D1 -5D-1*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,6),RF(1,6))
      EQK(:VL) = EG(:VL,20)/EG(:VL,6)/EG(:VL,6)/PFAC1(:VL)
      RB(:VL,6) = RF(:VL,6) / MAX(EQK(:VL), SMALL)
C      R7: H + O = OH
      RF(:VL,7) = 4.29970685D1 -1D0*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,7),RF(1,7))
      EQK(:VL) = EG(:VL,8)/EG(:VL,1)/EG(:VL,6)/PFAC1(:VL)
      RB(:VL,7) = RF(:VL,7) / MAX(EQK(:VL), SMALL)
C      R8: H + OH = H2O
      RF(:VL,8) = 5.19918731D1 -2D0*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,8),RF(1,8))
      EQK(:VL) = EG(:VL,9)/EG(:VL,1)/EG(:VL,8)/PFAC1(:VL)
      RB(:VL,8) = RF(:VL,8) / MAX(EQK(:VL), SMALL)
C      R9: H + O2 = HO2
      RF(:VL,9) = 2.80196791D1 +6D-1*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,9),RF(1,9))
      EQK(:VL) = EG(:VL,21)/EG(:VL,1)/EG(:VL,20)/PFAC1(:VL)
      RB(:VL,9) = RF(:VL,9) / MAX(EQK(:VL), SMALL)
C      R10: H + HO2 = H2 + O2
      RF(:VL,10) = 3.04404238D1 -4.14147317D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,10),RF(1,10))
      EQK(:VL) = EG(:VL,2)*EG(:VL,20)/EG(:VL,1)/EG(:VL,21)
      RB(:VL,10) = RF(:VL,10) / MAX(EQK(:VL), SMALL)
C      R11: H + HO2 = 2OH
      RF(:VL,11) = 3.18907389D1 -1.48448917D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,11),RF(1,11))
      EQK(:VL) = EG(:VL,8)*EG(:VL,8)/EG(:VL,1)/EG(:VL,21)
      RB(:VL,11) = RF(:VL,11) / MAX(EQK(:VL), SMALL)
C      R12: O + HO2 = OH + O2
      RF(:VL,12) = 3.25D13
      EQK(:VL) = EG(:VL,8)*EG(:VL,20)/EG(:VL,6)/EG(:VL,21)
      RB(:VL,12) = RF(:VL,12) / MAX(EQK(:VL), SMALL)
C      R13: OH + HO2 = H2O + O2
      RF(:VL,13) = 3.09948627D1 +2.50098684D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,13),RF(1,13))
      EQK(:VL) = EG(:VL,9)*EG(:VL,20)/EG(:VL,8)/EG(:VL,21)
      RB(:VL,13) = RF(:VL,13) / MAX(EQK(:VL), SMALL)
C      R14: 2HO2 = O2 + H2O2
      RF(:VL,14) = 3.36712758D1 -6.02954211D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,14),RF(1,14))
      EQK(:VL) = EG(:VL,20)*EG(:VL,22)/EG(:VL,21)/EG(:VL,21)
      RB(:VL,14) = RF(:VL,14) / MAX(EQK(:VL), SMALL)
C      R15: 2HO2 = O2 + H2O2
      RF(:VL,15) = 2.55908003D1 +8.19890917D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,15),RF(1,15))
      EQK(:VL) = EG(:VL,20)*EG(:VL,22)/EG(:VL,21)/EG(:VL,21)
      RB(:VL,15) = RF(:VL,15) / MAX(EQK(:VL), SMALL)
C      R16: H2O2 = 2OH
      RF(:VL,16) = 3.33183354D1 -2.43707832D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,16),RF(1,16))
      EQK(:VL) = EG(:VL,8)*EG(:VL,8)/EG(:VL,22)*PFAC1(:VL)
      RB(:VL,16) = RF(:VL,16) / MAX(EQK(:VL), SMALL)
C      R17: H + H2O2 = OH + H2O
      RF(:VL,17) = 3.0813233D1 -1.99777017D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,17),RF(1,17))
      EQK(:VL) = EG(:VL,8)*EG(:VL,9)/EG(:VL,1)/EG(:VL,22)
      RB(:VL,17) = RF(:VL,17) / MAX(EQK(:VL), SMALL)
C      R18: H + H2O2 = H2 + HO2
      RF(:VL,18) = 3.15063801D1 -4.00057251D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,18),RF(1,18))
      EQK(:VL) = EG(:VL,2)*EG(:VL,21)/EG(:VL,1)/EG(:VL,22)
      RB(:VL,18) = RF(:VL,18) / MAX(EQK(:VL), SMALL)
C      R19: O + H2O2 = OH + HO2
      RF(:VL,19) = 1.60720517D1 +2D0*ALOGT(:VL)
     * -1.99777017D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,19),RF(1,19))
      EQK(:VL) = EG(:VL,8)*EG(:VL,21)/EG(:VL,6)/EG(:VL,22)
      RB(:VL,19) = RF(:VL,19) / MAX(EQK(:VL), SMALL)
C      R20: OH + H2O2 = H2O + HO2
      RF(:VL,20) = 1D12
      EQK(:VL) = EG(:VL,9)*EG(:VL,21)/EG(:VL,8)/EG(:VL,22)
      RB(:VL,20) = RF(:VL,20) / MAX(EQK(:VL), SMALL)
C      R21: OH + H2O2 = H2O + HO2
      RF(:VL,21) = 3.39940492D1 -4.80924169D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,21),RF(1,21))
      EQK(:VL) = EG(:VL,9)*EG(:VL,21)/EG(:VL,8)/EG(:VL,22)
      RB(:VL,21) = RF(:VL,21) / MAX(EQK(:VL), SMALL)
C      R22: O + CO = CO2
      RF(:VL,22) = 2.36136376D1 -1.19966854D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,22),RF(1,22))
      EQK(:VL) = EG(:VL,23)/EG(:VL,6)/EG(:VL,12)/PFAC1(:VL)
      RB(:VL,22) = RF(:VL,22) / MAX(EQK(:VL), SMALL)
C      R23: CO + O2 = O + CO2
      RF(:VL,23) = 2.85592404D1 -2.4003435D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,23),RF(1,23))
      EQK(:VL) = EG(:VL,6)*EG(:VL,23)/EG(:VL,12)/EG(:VL,20)
      RB(:VL,23) = RF(:VL,23) / MAX(EQK(:VL), SMALL)
C      R24: CO + HO2 = OH + CO2
      RF(:VL,24) = 3.10355463D1 -1.15739834D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,24),RF(1,24))
      EQK(:VL) = EG(:VL,8)*EG(:VL,23)/EG(:VL,12)/EG(:VL,21)
      RB(:VL,24) = RF(:VL,24) / MAX(EQK(:VL), SMALL)
C      R25: OH + CO = H + CO2
      RF(:VL,25) = 1.23144785D1 +1.89D0*ALOGT(:VL)
     * +5.83077153D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,25),RF(1,25))
      EQK(:VL) = EG(:VL,1)*EG(:VL,23)/EG(:VL,8)/EG(:VL,12)
      RB(:VL,25) = RF(:VL,25) / MAX(EQK(:VL), SMALL)
C      R26: HCO = H + CO
      RF(:VL,26) = 2.68862648D1 +6.59D-1*ALOGT(:VL)
     * -7.48484471D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,26),RF(1,26))
      EQK(:VL) = EG(:VL,1)*EG(:VL,12)/EG(:VL,14)*PFAC1(:VL)
      RB(:VL,26) = RF(:VL,26) / MAX(EQK(:VL), SMALL)
C      R27: HCO + O2 = CO + HO2
      RF(:VL,27) = 2.96565343D1 -2.06318834D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,27),RF(1,27))
      EQK(:VL) = EG(:VL,12)*EG(:VL,21)/EG(:VL,14)/EG(:VL,20)
      RB(:VL,27) = RF(:VL,27) / MAX(EQK(:VL), SMALL)
C      R28: H + HCO = H2 + CO
      RF(:VL,28) = 7.23D13
      EQK(:VL) = EG(:VL,2)*EG(:VL,12)/EG(:VL,1)/EG(:VL,14)
      RB(:VL,28) = RF(:VL,28) / MAX(EQK(:VL), SMALL)
C      R29: O + HCO = OH + CO
      RF(:VL,29) = 3.02D13
      EQK(:VL) = EG(:VL,8)*EG(:VL,12)/EG(:VL,6)/EG(:VL,14)
      RB(:VL,29) = RF(:VL,29) / MAX(EQK(:VL), SMALL)
C      R30: OH + HCO = H2O + CO
      RF(:VL,30) = 3.02D13
      EQK(:VL) = EG(:VL,9)*EG(:VL,12)/EG(:VL,8)/EG(:VL,14)
      RB(:VL,30) = RF(:VL,30) / MAX(EQK(:VL), SMALL)
C      R31: O + HCO = H + CO2
      RF(:VL,31) = 3D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,23)/EG(:VL,6)/EG(:VL,14)
      RB(:VL,31) = RF(:VL,31) / MAX(EQK(:VL), SMALL)
C      R32: HCO + HO2 = H + OH + CO2
      RF(:VL,32) = 3D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,8)*EG(:VL,23)/EG(:VL,14)/EG(:VL,21)
     1           *PFAC1(:VL)
      RB(:VL,32) = RF(:VL,32) / MAX(EQK(:VL), SMALL)
C      R33: 2HCO = H2 + 2CO
      RF(:VL,33) = 3D12
      EQK(:VL) = EG(:VL,2)*EG(:VL,12)*EG(:VL,12)/EG(:VL,14)/EG(:VL,14)
     1           *PFAC1(:VL)
      RB(:VL,33) = RF(:VL,33) / MAX(EQK(:VL), SMALL)
C      R34: CH3 + HCO = CH4 + CO
      RF(:VL,34) = 2.65D13
      EQK(:VL) = EG(:VL,7)*EG(:VL,12)/EG(:VL,5)/EG(:VL,14)
      RB(:VL,34) = RF(:VL,34) / MAX(EQK(:VL), SMALL)
C      R35: 2HCO = CO + CH2O
      RF(:VL,35) = 3D13
      EQK(:VL) = EG(:VL,12)*EG(:VL,16)/EG(:VL,14)/EG(:VL,14)
      RB(:VL,35) = RF(:VL,35) / MAX(EQK(:VL), SMALL)
C      R36: CH2O = H + HCO
      RF(:VL,36) = 9.09947411D1 -6.3D0*ALOGT(:VL)
     * -5.02713451D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,36),RF(1,36))
      EQK(:VL) = EG(:VL,1)*EG(:VL,14)/EG(:VL,16)*PFAC1(:VL)
      RB(:VL,36) = RF(:VL,36) / MAX(EQK(:VL), SMALL)
C      R37: CH2O = H2 + CO
      RF(:VL,37) = 1.04747731D2 -8D0*ALOGT(:VL)
     * -4.90686573D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,37),RF(1,37))
      EQK(:VL) = EG(:VL,2)*EG(:VL,12)/EG(:VL,16)*PFAC1(:VL)
      RB(:VL,37) = RF(:VL,37) / MAX(EQK(:VL), SMALL)
C      R38: H + CH2O = H2 + HCO
      RF(:VL,38) = 1.78655549D1 +1.9D0*ALOGT(:VL)
     * -1.38314133D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,38),RF(1,38))
      EQK(:VL) = EG(:VL,2)*EG(:VL,14)/EG(:VL,1)/EG(:VL,16)
      RB(:VL,38) = RF(:VL,38) / MAX(EQK(:VL), SMALL)
C      R39: O + CH2O = OH + HCO
      RF(:VL,39) = 3.05269331D1 -1.54990734D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,39),RF(1,39))
      EQK(:VL) = EG(:VL,8)*EG(:VL,14)/EG(:VL,6)/EG(:VL,16)
      RB(:VL,39) = RF(:VL,39) / MAX(EQK(:VL), SMALL)
C      R40: OH + CH2O = H2O + HCO
      RF(:VL,40) = 2.19558261D1 +1.18D0*ALOGT(:VL)
     * +2.2493785D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,40),RF(1,40))
      EQK(:VL) = EG(:VL,9)*EG(:VL,14)/EG(:VL,8)/EG(:VL,16)
      RB(:VL,40) = RF(:VL,40) / MAX(EQK(:VL), SMALL)
C      R41: CH2O + O2 = HCO + HO2
      RF(:VL,41) = 1.40225247D1 +3D0*ALOGT(:VL)
     * -2.61672667D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,41),RF(1,41))
      EQK(:VL) = EG(:VL,14)*EG(:VL,21)/EG(:VL,16)/EG(:VL,20)
      RB(:VL,41) = RF(:VL,41) / MAX(EQK(:VL), SMALL)
C      R42: CH2O + HO2 = HCO + H2O2
      RF(:VL,42) = 1.06237634D1 +2.5D0*ALOGT(:VL)
     * -5.13784218D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,42),RF(1,42))
      EQK(:VL) = EG(:VL,14)*EG(:VL,22)/EG(:VL,16)/EG(:VL,21)
      RB(:VL,42) = RF(:VL,42) / MAX(EQK(:VL), SMALL)
C      R43: CH3 + CH2O = CH4 + HCO
      RF(:VL,43) = -1.25246264D1 +5.42D0*ALOGT(:VL)
     * -5.02210234D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,43),RF(1,43))
      EQK(:VL) = EG(:VL,7)*EG(:VL,14)/EG(:VL,5)/EG(:VL,16)
      RB(:VL,43) = RF(:VL,43) / MAX(EQK(:VL), SMALL)
C      R44: CH3 + O = H + CH2O
      RF(:VL,44) = 8.43D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,16)/EG(:VL,5)/EG(:VL,6)
      RB(:VL,44) = RF(:VL,44) / MAX(EQK(:VL), SMALL)
C      R45: CH3 + O2 = O + CH3O
      RF(:VL,45) = 4.21346663D1 -1.57D0*ALOGT(:VL)
     * -1.47090232D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,45),RF(1,45))
      EQK(:VL) = EG(:VL,6)*EG(:VL,19)/EG(:VL,5)/EG(:VL,20)
      RB(:VL,45) = RF(:VL,45) / MAX(EQK(:VL), SMALL)
C      R46: CH3 + O2 = OH + CH2O
      RF(:VL,46) = 2.66475216D1 -7.36709201D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,46),RF(1,46))
      EQK(:VL) = EG(:VL,8)*EG(:VL,16)/EG(:VL,5)/EG(:VL,20)
      RB(:VL,46) = RF(:VL,46) / MAX(EQK(:VL), SMALL)
C      R47: CH3 + HO2 = OH + CH3O
      RF(:VL,47) = 2.39054777D1 +7.6D-1*ALOGT(:VL)
     * +1.16997875D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,47),RF(1,47))
      EQK(:VL) = EG(:VL,8)*EG(:VL,19)/EG(:VL,5)/EG(:VL,21)
      RB(:VL,47) = RF(:VL,47) / MAX(EQK(:VL), SMALL)
C      R48: 2CH3 = C2H6
      RF(:VL,48) = 3.53616352D1 -6.9D-1*ALOGT(:VL)
     * -8.79924665D1/T(:VL)
      CALL VRDA_EXP(VL,RF(1,48),RF(1,48))
      EQK(:VL) = EG(:VL,17)/EG(:VL,5)/EG(:VL,5)/PFAC1(:VL)
      RB(:VL,48) = RF(:VL,48) / MAX(EQK(:VL), SMALL)
C      R49: H + CH3 = CH4
      RF(:VL,49) = 3.70803784D1 -6.3D-1*ALOGT(:VL)
     * -1.92731984D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,49),RF(1,49))
      EQK(:VL) = EG(:VL,7)/EG(:VL,1)/EG(:VL,5)/PFAC1(:VL)
      RB(:VL,49) = RF(:VL,49) / MAX(EQK(:VL), SMALL)
C      R50: H + CH4 = H2 + CH3
      RF(:VL,50) = 1.78173743D1 +1.97D0*ALOGT(:VL)
     * -5.64105884D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,50),RF(1,50))
      EQK(:VL) = EG(:VL,2)*EG(:VL,5)/EG(:VL,1)/EG(:VL,7)
      RB(:VL,50) = RF(:VL,50) / MAX(EQK(:VL), SMALL)
C      R51: O + CH4 = CH3 + OH
      RF(:VL,51) = 2.87784236D1 +5D-1*ALOGT(:VL)
     * -5.17809951D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,51),RF(1,51))
      EQK(:VL) = EG(:VL,5)*EG(:VL,8)/EG(:VL,6)/EG(:VL,7)
      RB(:VL,51) = RF(:VL,51) / MAX(EQK(:VL), SMALL)
C      R52: CH4 + OH = CH3 + H2O
      RF(:VL,52) = 1.55594794D1 +1.96D0*ALOGT(:VL)
     * -1.32798879D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,52),RF(1,52))
      EQK(:VL) = EG(:VL,5)*EG(:VL,9)/EG(:VL,7)/EG(:VL,8)
      RB(:VL,52) = RF(:VL,52) / MAX(EQK(:VL), SMALL)
C      R53: CH3 + HO2 = CH4 + O2
      RF(:VL,53) = 3.16D12
      EQK(:VL) = EG(:VL,7)*EG(:VL,20)/EG(:VL,5)/EG(:VL,21)
      RB(:VL,53) = RF(:VL,53) / MAX(EQK(:VL), SMALL)
C      R54: CH4 + HO2 = CH3 + H2O2
      RF(:VL,54) = 2.59217629D1 -9.34976568D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,54),RF(1,54))
      EQK(:VL) = EG(:VL,5)*EG(:VL,22)/EG(:VL,7)/EG(:VL,21)
      RB(:VL,54) = RF(:VL,54) / MAX(EQK(:VL), SMALL)
C      R55: CH2OH = H + CH2O
      RF(:VL,55) = 3.22361913D1 -1.26307384D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,55),RF(1,55))
      EQK(:VL) = EG(:VL,1)*EG(:VL,16)/EG(:VL,18)*PFAC1(:VL)
      RB(:VL,55) = RF(:VL,55) / MAX(EQK(:VL), SMALL)
C      R56: H + CH2OH = H2 + CH2O
      RF(:VL,56) = 6D12
      EQK(:VL) = EG(:VL,2)*EG(:VL,16)/EG(:VL,1)/EG(:VL,18)
      RB(:VL,56) = RF(:VL,56) / MAX(EQK(:VL), SMALL)
C      R57: H + CH2OH = CH3 + OH
      RF(:VL,57) = 9.635D13
      EQK(:VL) = EG(:VL,5)*EG(:VL,8)/EG(:VL,1)/EG(:VL,18)
      RB(:VL,57) = RF(:VL,57) / MAX(EQK(:VL), SMALL)
C      R58: O + CH2OH = OH + CH2O
      RF(:VL,58) = 4.2D13
      EQK(:VL) = EG(:VL,8)*EG(:VL,16)/EG(:VL,6)/EG(:VL,18)
      RB(:VL,58) = RF(:VL,58) / MAX(EQK(:VL), SMALL)
C      R59: OH + CH2OH = H2O + CH2O
      RF(:VL,59) = 2.4D13
      EQK(:VL) = EG(:VL,9)*EG(:VL,16)/EG(:VL,8)/EG(:VL,18)
      RB(:VL,59) = RF(:VL,59) / MAX(EQK(:VL), SMALL)
C      R60: CH2OH + O2 = CH2O + HO2
      RF(:VL,60) = 3.3115818D1 -2.52463802D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,60),RF(1,60))
      EQK(:VL) = EG(:VL,16)*EG(:VL,21)/EG(:VL,18)/EG(:VL,20)
      RB(:VL,60) = RF(:VL,60) / MAX(EQK(:VL), SMALL)
C      R61: CH2OH + O2 = CH2O + HO2
      RF(:VL,61) = 3.20322444D-4*RF(:VL,7)
      EQK(:VL) = EG(:VL,16)*EG(:VL,21)/EG(:VL,18)/EG(:VL,20)
      RB(:VL,61) = RF(:VL,61) / MAX(EQK(:VL), SMALL)
C      R62: CH2OH + HO2 = CH2O + H2O2
      RF(:VL,62) = 1.2D13
      EQK(:VL) = EG(:VL,16)*EG(:VL,22)/EG(:VL,18)/EG(:VL,21)
      RB(:VL,62) = RF(:VL,62) / MAX(EQK(:VL), SMALL)
C      R63: HCO + CH2OH = 2CH2O
      RF(:VL,63) = 1.5D13
      EQK(:VL) = EG(:VL,16)*EG(:VL,16)/EG(:VL,14)/EG(:VL,18)
      RB(:VL,63) = RF(:VL,63) / MAX(EQK(:VL), SMALL)
C      R64: CH3O = H + CH2O
      RF(:VL,64) = 4.12602021D1 -1.2D0*ALOGT(:VL)
     * -7.79985835D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,64),RF(1,64))
      EQK(:VL) = EG(:VL,1)*EG(:VL,16)/EG(:VL,19)*PFAC1(:VL)
      RB(:VL,64) = RF(:VL,64) / MAX(EQK(:VL), SMALL)
C      R65: H + CH3O = CH3 + OH
      RF(:VL,65) = 3.2D13
      EQK(:VL) = EG(:VL,5)*EG(:VL,8)/EG(:VL,1)/EG(:VL,19)
      RB(:VL,65) = RF(:VL,65) / MAX(EQK(:VL), SMALL)
C      R66: O + CH3O = OH + CH2O
      RF(:VL,66) = 6D12
      EQK(:VL) = EG(:VL,8)*EG(:VL,16)/EG(:VL,6)/EG(:VL,19)
      RB(:VL,66) = RF(:VL,66) / MAX(EQK(:VL), SMALL)
C      R67: OH + CH3O = H2O + CH2O
      RF(:VL,67) = 1.8D13
      EQK(:VL) = EG(:VL,9)*EG(:VL,16)/EG(:VL,8)/EG(:VL,19)
      RB(:VL,67) = RF(:VL,67) / MAX(EQK(:VL), SMALL)
C      R68: CH3O + O2 = CH2O + HO2
      RF(:VL,68) = 3.21344907D1 -6.02853568D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,68),RF(1,68))
      EQK(:VL) = EG(:VL,16)*EG(:VL,21)/EG(:VL,19)/EG(:VL,20)
      RB(:VL,68) = RF(:VL,68) / MAX(EQK(:VL), SMALL)
C      R69: CH3O + O2 = CH2O + HO2
      RF(:VL,69) = 2.38143083D1 -8.79622735D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,69),RF(1,69))
      EQK(:VL) = EG(:VL,16)*EG(:VL,21)/EG(:VL,19)/EG(:VL,20)
      RB(:VL,69) = RF(:VL,69) / MAX(EQK(:VL), SMALL)
C      R70: CH3O + HO2 = CH2O + H2O2
      RF(:VL,70) = 3D11
      EQK(:VL) = EG(:VL,16)*EG(:VL,22)/EG(:VL,19)/EG(:VL,21)
      RB(:VL,70) = RF(:VL,70) / MAX(EQK(:VL), SMALL)
C      R71: CO + CH3O = CH3 + CO2
      RF(:VL,71) = 3.04036098D1 -5.93795668D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,71),RF(1,71))
      EQK(:VL) = EG(:VL,5)*EG(:VL,23)/EG(:VL,12)/EG(:VL,19)
      RB(:VL,71) = RF(:VL,71) / MAX(EQK(:VL), SMALL)
C      R72: 2CH3 = H + C2H5
      RF(:VL,72) = 2.9238457D1 +1D-1*ALOGT(:VL)
     * -5.33409668D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,72),RF(1,72))
      EQK(:VL) = EG(:VL,1)*EG(:VL,15)/EG(:VL,5)/EG(:VL,5)
      RB(:VL,72) = RF(:VL,72) / MAX(EQK(:VL), SMALL)
C      R73: CH2 + CH4 = 2CH3
      RF(:VL,73) = 1.47156719D1 +2D0*ALOGT(:VL)
     * -4.16160184D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,73),RF(1,73))
      EQK(:VL) = EG(:VL,5)*EG(:VL,5)/EG(:VL,3)/EG(:VL,7)
      RB(:VL,73) = RF(:VL,73) / MAX(EQK(:VL), SMALL)
C      R74: CH2(S) + CH4 = 2CH3
      RF(:VL,74) = 3.04036098D1 +2.86833501D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,74),RF(1,74))
      EQK(:VL) = EG(:VL,5)*EG(:VL,5)/EG(:VL,4)/EG(:VL,7)
      RB(:VL,74) = RF(:VL,74) / MAX(EQK(:VL), SMALL)
C      R75: CH3 + OH = CH2 + H2O
      RF(:VL,75) = 1.78408622D1 +1.6D0*ALOGT(:VL)
     * -2.72743434D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,75),RF(1,75))
      EQK(:VL) = EG(:VL,3)*EG(:VL,9)/EG(:VL,5)/EG(:VL,8)
      RB(:VL,75) = RF(:VL,75) / MAX(EQK(:VL), SMALL)
C      R76: CH3 + OH = CH2(S) + H2O
      RF(:VL,76) = 2.501D13
      EQK(:VL) = EG(:VL,4)*EG(:VL,9)/EG(:VL,5)/EG(:VL,8)
      RB(:VL,76) = RF(:VL,76) / MAX(EQK(:VL), SMALL)
C      R77: CH2 + CH3 = H + C2H4
      RF(:VL,77) = 4D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,13)/EG(:VL,3)/EG(:VL,5)
      RB(:VL,77) = RF(:VL,77) / MAX(EQK(:VL), SMALL)
C      R78: CH2(S) + CH3 = H + C2H4
      RF(:VL,78) = 7.5D-1*RF(:VL,74)
      EQK(:VL) = EG(:VL,1)*EG(:VL,13)/EG(:VL,4)/EG(:VL,5)
      RB(:VL,78) = RF(:VL,78) / MAX(EQK(:VL), SMALL)
C      R79: H + CH3O = CH2(S) + H2O
      RF(:VL,79) = 1.6D13
      EQK(:VL) = EG(:VL,4)*EG(:VL,9)/EG(:VL,1)/EG(:VL,19)
      RB(:VL,79) = RF(:VL,79) / MAX(EQK(:VL), SMALL)
C      R80: H + C2H6 = H2 + C2H5
      RF(:VL,80) = 1.85604427D1 +1.9D0*ALOGT(:VL)
     * -3.78922151D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,80),RF(1,80))
      EQK(:VL) = EG(:VL,2)*EG(:VL,15)/EG(:VL,1)/EG(:VL,17)
      RB(:VL,80) = RF(:VL,80) / MAX(EQK(:VL), SMALL)
C      R81: O + C2H6 = OH + C2H5
      RF(:VL,81) = 1.83130955D1 +1.92D0*ALOGT(:VL)
     * -2.86330284D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,81),RF(1,81))
      EQK(:VL) = EG(:VL,8)*EG(:VL,15)/EG(:VL,6)/EG(:VL,17)
      RB(:VL,81) = RF(:VL,81) / MAX(EQK(:VL), SMALL)
C      R82: OH + C2H6 = H2O + C2H5
      RF(:VL,82) = 1.50796373D1 +2.12D0*ALOGT(:VL)
     * -4.37798501D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,82),RF(1,82))
      EQK(:VL) = EG(:VL,9)*EG(:VL,15)/EG(:VL,8)/EG(:VL,17)
      RB(:VL,82) = RF(:VL,82) / MAX(EQK(:VL), SMALL)
C      R83: C2H6 + O2 = C2H5 + HO2
      RF(:VL,83) = 3.13199006D1 -2.56137284D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,83),RF(1,83))
      EQK(:VL) = EG(:VL,15)*EG(:VL,21)/EG(:VL,17)/EG(:VL,20)
      RB(:VL,83) = RF(:VL,83) / MAX(EQK(:VL), SMALL)
C      R84: C2H6 + HO2 = C2H5 + H2O2
      RF(:VL,84) = 2.64068456D1 -7.51805701D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,84),RF(1,84))
      EQK(:VL) = EG(:VL,15)*EG(:VL,22)/EG(:VL,17)/EG(:VL,21)
      RB(:VL,84) = RF(:VL,84) / MAX(EQK(:VL), SMALL)
C      R85: CH3 + C2H6 = CH4 + C2H5
      RF(:VL,85) = 1.56303353D1 +1.74D0*ALOGT(:VL)
     * -5.25861418D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,85),RF(1,85))
      EQK(:VL) = EG(:VL,7)*EG(:VL,15)/EG(:VL,5)/EG(:VL,17)
      RB(:VL,85) = RF(:VL,85) / MAX(EQK(:VL), SMALL)
C      R86: H + C2H5 = C2H6
      RF(:VL,86) = 4.07945264D1 -9.9D-1*ALOGT(:VL)
     * -7.95082335D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,86),RF(1,86))
      EQK(:VL) = EG(:VL,17)/EG(:VL,1)/EG(:VL,15)/PFAC1(:VL)
      RB(:VL,86) = RF(:VL,86) / MAX(EQK(:VL), SMALL)
C      R87: H + C2H5 = H2 + C2H4
      RF(:VL,87) = 2D12
      EQK(:VL) = EG(:VL,2)*EG(:VL,13)/EG(:VL,1)/EG(:VL,15)
      RB(:VL,87) = RF(:VL,87) / MAX(EQK(:VL), SMALL)
C      R88: O + C2H5 = CH3 + CH2O
      RF(:VL,88) = 1.32D14
      EQK(:VL) = EG(:VL,5)*EG(:VL,16)/EG(:VL,6)/EG(:VL,15)
      RB(:VL,88) = RF(:VL,88) / MAX(EQK(:VL), SMALL)
C      R89: C2H5 + O2 = C2H4 + HO2
      RF(:VL,89) = 2D10
      EQK(:VL) = EG(:VL,13)*EG(:VL,21)/EG(:VL,15)/EG(:VL,20)
      RB(:VL,89) = RF(:VL,89) / MAX(EQK(:VL), SMALL)
C      R90: 2C2H5 = C2H4 + C2H6
      RF(:VL,90) = 1.4D12
      EQK(:VL) = EG(:VL,13)*EG(:VL,17)/EG(:VL,15)/EG(:VL,15)
      RB(:VL,90) = RF(:VL,90) / MAX(EQK(:VL), SMALL)
C      R91: HCO + C2H5 = CO + C2H6
      RF(:VL,91) = 1.2D14
      EQK(:VL) = EG(:VL,12)*EG(:VL,17)/EG(:VL,14)/EG(:VL,15)
      RB(:VL,91) = RF(:VL,91) / MAX(EQK(:VL), SMALL)
C      R92: O + C2H5 = H + CH3HCO
      RF(:VL,92) = 8.02D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,24)/EG(:VL,6)/EG(:VL,15)
      RB(:VL,92) = RF(:VL,92) / MAX(EQK(:VL), SMALL)
C      R93: C2H4 = H2 + C2H2
      RF(:VL,93) = 2.97104627D1 +4.4D-1*ALOGT(:VL)
     * -4.46705436D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,93),RF(1,93))
      EQK(:VL) = EG(:VL,2)*EG(:VL,10)/EG(:VL,13)*PFAC1(:VL)
      RB(:VL,93) = RF(:VL,93) / MAX(EQK(:VL), SMALL)
C      R94: H + C2H4 = C2H5
      RF(:VL,94) = 2.77079822D1 +4.54D-1*ALOGT(:VL)
     * -9.15854335D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,94),RF(1,94))
      EQK(:VL) = EG(:VL,15)/EG(:VL,1)/EG(:VL,13)/PFAC1(:VL)
      RB(:VL,94) = RF(:VL,94) / MAX(EQK(:VL), SMALL)
C      R95: H + C2H3 = C2H4
      RF(:VL,95) = 2.94360258D1 +2.7D-1*ALOGT(:VL)
     * -1.40900667D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,95),RF(1,95))
      EQK(:VL) = EG(:VL,13)/EG(:VL,1)/EG(:VL,11)/PFAC1(:VL)
      RB(:VL,95) = RF(:VL,95) / MAX(EQK(:VL), SMALL)
C      R96: H + C2H4 = H2 + C2H3
      RF(:VL,96) = 1.4096923D1 +2.53D0*ALOGT(:VL)
     * -6.15937201D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,96),RF(1,96))
      EQK(:VL) = EG(:VL,2)*EG(:VL,11)/EG(:VL,1)/EG(:VL,13)
      RB(:VL,96) = RF(:VL,96) / MAX(EQK(:VL), SMALL)
C      R97: OH + C2H4 = H2O + C2H3
      RF(:VL,97) = 1.44032972D1 +2D0*ALOGT(:VL)
     * -1.25804167D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,97),RF(1,97))
      EQK(:VL) = EG(:VL,9)*EG(:VL,11)/EG(:VL,8)/EG(:VL,13)
      RB(:VL,97) = RF(:VL,97) / MAX(EQK(:VL), SMALL)
C      R98: CH3 + C2H4 = CH4 + C2H3
      RF(:VL,98) = 1.23327053D1 +2D0*ALOGT(:VL)
     * -4.62959334D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,98),RF(1,98))
      EQK(:VL) = EG(:VL,7)*EG(:VL,11)/EG(:VL,5)/EG(:VL,13)
      RB(:VL,98) = RF(:VL,98) / MAX(EQK(:VL), SMALL)
C      R99: O + C2H4 = CH3 + HCO
      RF(:VL,99) = 1.67704208D1 +1.83D0*ALOGT(:VL)
     * -1.10707667D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,99),RF(1,99))
      EQK(:VL) = EG(:VL,5)*EG(:VL,14)/EG(:VL,6)/EG(:VL,13)
      RB(:VL,99) = RF(:VL,99) / MAX(EQK(:VL), SMALL)
C      R100: OH + C2H3 = H2O + C2H2
      RF(:VL,100) = 5D12
      EQK(:VL) = EG(:VL,9)*EG(:VL,10)/EG(:VL,8)/EG(:VL,11)
      RB(:VL,100) = RF(:VL,100) / MAX(EQK(:VL), SMALL)
C      R101: O + C2H4 = OH + C2H3
      RF(:VL,101) = 1.65302053D1 +1.91D0*ALOGT(:VL)
     * -1.88203034D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,101),RF(1,101))
      EQK(:VL) = EG(:VL,8)*EG(:VL,11)/EG(:VL,6)/EG(:VL,13)
      RB(:VL,101) = RF(:VL,101) / MAX(EQK(:VL), SMALL)
C      R102: C2H4 + O2 = C2H3 + HO2
      RF(:VL,102) = 3.13722558D1 -2.89852801D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,102),RF(1,102))
      EQK(:VL) = EG(:VL,11)*EG(:VL,21)/EG(:VL,13)/EG(:VL,20)
      RB(:VL,102) = RF(:VL,102) / MAX(EQK(:VL), SMALL)
C      R103: H + C2H3 = H2 + C2H2
      RF(:VL,103) = 9.64D13
      EQK(:VL) = EG(:VL,2)*EG(:VL,10)/EG(:VL,1)/EG(:VL,11)
      RB(:VL,103) = RF(:VL,103) / MAX(EQK(:VL), SMALL)
C      R104: C2H3 + H2O2 = C2H4 + HO2
      RF(:VL,104) = 2.32164713D1 +2.99917134D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,104),RF(1,104))
      EQK(:VL) = EG(:VL,13)*EG(:VL,21)/EG(:VL,11)/EG(:VL,22)
      RB(:VL,104) = RF(:VL,104) / MAX(EQK(:VL), SMALL)
C      R105: CH3 + C2H3 = CH4 + C2H2
      RF(:VL,105) = 3.9D11
      EQK(:VL) = EG(:VL,7)*EG(:VL,10)/EG(:VL,5)/EG(:VL,11)
      RB(:VL,105) = RF(:VL,105) / MAX(EQK(:VL), SMALL)
C      R106: 2C2H3 = C2H2 + C2H4
      RF(:VL,106) = 9.6D11
      EQK(:VL) = EG(:VL,10)*EG(:VL,13)/EG(:VL,11)/EG(:VL,11)
      RB(:VL,106) = RF(:VL,106) / MAX(EQK(:VL), SMALL)
C      R107: C2H3 + O2 = HCO + CH2O
      RF(:VL,107) = 3.83630605D1 -1.39D0*ALOGT(:VL)
     * -5.10764918D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,107),RF(1,107))
      EQK(:VL) = EG(:VL,14)*EG(:VL,16)/EG(:VL,11)/EG(:VL,20)
      RB(:VL,107) = RF(:VL,107) / MAX(EQK(:VL), SMALL)
C      R108: C2H3 + O2 = C2H2 + HO2
      RF(:VL,108) = 1.41059389D1 +1.61D0*ALOGT(:VL)
     * +1.932352D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,108),RF(1,108))
      EQK(:VL) = EG(:VL,10)*EG(:VL,21)/EG(:VL,11)/EG(:VL,20)
      RB(:VL,108) = RF(:VL,108) / MAX(EQK(:VL), SMALL)
C      R109: H + C2H2 = C2H3
      RF(:VL,109) = 2.93537877D1 -1.20772D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,109),RF(1,109))
      EQK(:VL) = EG(:VL,11)/EG(:VL,1)/EG(:VL,10)/PFAC1(:VL)
      RB(:VL,109) = RF(:VL,109) / MAX(EQK(:VL), SMALL)
C      R110: O + C2H2 = CH2 + CO
      RF(:VL,110) = 1.52216075D1 +2D0*ALOGT(:VL)
     * -9.56111669D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,110),RF(1,110))
      EQK(:VL) = EG(:VL,3)*EG(:VL,12)/EG(:VL,6)/EG(:VL,10)
      RB(:VL,110) = RF(:VL,110) / MAX(EQK(:VL), SMALL)
C      R111: OH + C2H2 = CH3 + CO
      RF(:VL,111) = -7.6354939D0 +4D0*ALOGT(:VL)
     * +1.00643334D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,111),RF(1,111))
      EQK(:VL) = EG(:VL,5)*EG(:VL,12)/EG(:VL,8)/EG(:VL,10)
      RB(:VL,111) = RF(:VL,111) / MAX(EQK(:VL), SMALL)
C      R112: H + CH2 = CH3
      RF(:VL,112) = 3.77576522D1 -8D-1*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,112),RF(1,112))
      EQK(:VL) = EG(:VL,5)/EG(:VL,1)/EG(:VL,3)/PFAC1(:VL)
      RB(:VL,112) = RF(:VL,112) / MAX(EQK(:VL), SMALL)
C      R113: CH2 + O = H + HCO
      RF(:VL,113) = 8D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,14)/EG(:VL,3)/EG(:VL,6)
      RB(:VL,113) = RF(:VL,113) / MAX(EQK(:VL), SMALL)
C      R114: CH2 + OH = H + CH2O
      RF(:VL,114) = 2D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,16)/EG(:VL,3)/EG(:VL,8)
      RB(:VL,114) = RF(:VL,114) / MAX(EQK(:VL), SMALL)
C      R115: H2 + CH2 = H + CH3
      RF(:VL,115) = 1.31223634D1 +2D0*ALOGT(:VL)
     * -3.63825651D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,115),RF(1,115))
      EQK(:VL) = EG(:VL,1)*EG(:VL,5)/EG(:VL,2)/EG(:VL,3)
      RB(:VL,115) = RF(:VL,115) / MAX(EQK(:VL), SMALL)
C      R116: CH2 + O2 = OH + HCO
      RF(:VL,116) = 3.02112379D1 -7.54825001D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,116),RF(1,116))
      EQK(:VL) = EG(:VL,8)*EG(:VL,14)/EG(:VL,3)/EG(:VL,20)
      RB(:VL,116) = RF(:VL,116) / MAX(EQK(:VL), SMALL)
C      R117: CH2 + HO2 = OH + CH2O
      RF(:VL,117) = 2D13
      EQK(:VL) = EG(:VL,8)*EG(:VL,16)/EG(:VL,3)/EG(:VL,21)
      RB(:VL,117) = RF(:VL,117) / MAX(EQK(:VL), SMALL)
C      R118: 2CH2 = H2 + C2H2
      RF(:VL,118) = 3.2D13
      EQK(:VL) = EG(:VL,2)*EG(:VL,10)/EG(:VL,3)/EG(:VL,3)
      RB(:VL,118) = RF(:VL,118) / MAX(EQK(:VL), SMALL)
C      R119: CH2(S) = CH2
      RF(:VL,119) = 2.98282457D1 -3.01930001D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,119),RF(1,119))
      EQK(:VL) = EG(:VL,3)/EG(:VL,4)
      RB(:VL,119) = RF(:VL,119) / MAX(EQK(:VL), SMALL)
C      R120: CH2(S) = CH2
      RF(:VL,120) = 3D13
      EQK(:VL) = EG(:VL,3)/EG(:VL,4)
      RB(:VL,120) = RF(:VL,120) / MAX(EQK(:VL), SMALL)
C      R121: CH2(S) = CH2
      RF(:VL,121) = 9D12
      EQK(:VL) = EG(:VL,3)/EG(:VL,4)
      RB(:VL,121) = RF(:VL,121) / MAX(EQK(:VL), SMALL)
C      R122: CH2(S) = CH2
      RF(:VL,122) = 7D12
      EQK(:VL) = EG(:VL,3)/EG(:VL,4)
      RB(:VL,122) = RF(:VL,122) / MAX(EQK(:VL), SMALL)
C      R123: CH2(S) + O = H2 + CO
      RF(:VL,123) = 1.5D13
      EQK(:VL) = EG(:VL,2)*EG(:VL,12)/EG(:VL,4)/EG(:VL,6)
      RB(:VL,123) = RF(:VL,123) / MAX(EQK(:VL), SMALL)
C      R124: CH2(S) + O = H + HCO
      RF(:VL,124) = 1.5D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,14)/EG(:VL,4)/EG(:VL,6)
      RB(:VL,124) = RF(:VL,124) / MAX(EQK(:VL), SMALL)
C      R125: CH2(S) + OH = H + CH2O
      RF(:VL,125) = 3D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,16)/EG(:VL,4)/EG(:VL,8)
      RB(:VL,125) = RF(:VL,125) / MAX(EQK(:VL), SMALL)
C      R126: H2 + CH2(S) = H + CH3
      RF(:VL,126) = 7D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,5)/EG(:VL,2)/EG(:VL,4)
      RB(:VL,126) = RF(:VL,126) / MAX(EQK(:VL), SMALL)
C      R127: CH2(S) + O2 = H + OH + CO
      RF(:VL,127) = 2.8D13
      EQK(:VL) = EG(:VL,1)*EG(:VL,8)*EG(:VL,12)/EG(:VL,4)/EG(:VL,20)
     1           *PFAC1(:VL)
      RB(:VL,127) = RF(:VL,127) / MAX(EQK(:VL), SMALL)
C      R128: CH2(S) + O2 = H2O + CO
      RF(:VL,128) = 1.2D13
      EQK(:VL) = EG(:VL,9)*EG(:VL,12)/EG(:VL,4)/EG(:VL,20)
      RB(:VL,128) = RF(:VL,128) / MAX(EQK(:VL), SMALL)
C      R129: CH2(S) + CO2 = CO + CH2O
      RF(:VL,129) = 1.4D13
      EQK(:VL) = EG(:VL,12)*EG(:VL,16)/EG(:VL,4)/EG(:VL,23)
      RB(:VL,129) = RF(:VL,129) / MAX(EQK(:VL), SMALL)
C      R130: CH3HCO = CH3 + HCO
      RF(:VL,130) = 3.64846865D1 -4.10997181D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,130),RF(1,130))
      EQK(:VL) = EG(:VL,5)*EG(:VL,14)/EG(:VL,24)*PFAC1(:VL)
      RB(:VL,130) = RF(:VL,130) / MAX(EQK(:VL), SMALL)
C      R131: CH3OCH3 = CH3 + CH3O
      RF(:VL,131) = 9.72385961D1 -7.95359D0*ALOGT(:VL)
     * -4.61986113D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,131),RF(1,131))
      EQK(:VL) = EG(:VL,5)*EG(:VL,19)/EG(:VL,27)*PFAC1(:VL)
      RB(:VL,131) = RF(:VL,131) / MAX(EQK(:VL), SMALL)
C      R132: OH + CH3OCH3 = H2O + CH3OCH2
      RF(:VL,132) = 1.57191095D1 +2D0*ALOGT(:VL)
     * +3.16966115D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,132),RF(1,132))
      EQK(:VL) = EG(:VL,9)*EG(:VL,25)/EG(:VL,8)/EG(:VL,27)
      RB(:VL,132) = RF(:VL,132) / MAX(EQK(:VL), SMALL)
C      R133: H + CH3OCH3 = H2 + CH3OCH2
      RF(:VL,133) = 1.72066576D1 +2D0*ALOGT(:VL)
     * -2.02977978D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,133),RF(1,133))
      EQK(:VL) = EG(:VL,2)*EG(:VL,25)/EG(:VL,1)/EG(:VL,27)
      RB(:VL,133) = RF(:VL,133) / MAX(EQK(:VL), SMALL)
C      R134: CH3 + CH3OCH3 = CH4 + CH3OCH2
      RF(:VL,134) = 3.28840189D0 +3.7779D0*ALOGT(:VL)
     * -4.84663069D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,134),RF(1,134))
      EQK(:VL) = EG(:VL,7)*EG(:VL,25)/EG(:VL,5)/EG(:VL,27)
      RB(:VL,134) = RF(:VL,134) / MAX(EQK(:VL), SMALL)
C      R135: O + CH3OCH3 = OH + CH3OCH2
      RF(:VL,135) = -6.28987058D0 +5.29D0*ALOGT(:VL)
     * +5.48506168D1/T(:VL)
      CALL VRDA_EXP(VL,RF(1,135),RF(1,135))
      EQK(:VL) = EG(:VL,8)*EG(:VL,25)/EG(:VL,6)/EG(:VL,27)
      RB(:VL,135) = RF(:VL,135) / MAX(EQK(:VL), SMALL)
C      R136: HO2 + CH3OCH3 = H2O2 + CH3OCH2
      RF(:VL,136) = 3.06267534D1 -8.30307502D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,136),RF(1,136))
      EQK(:VL) = EG(:VL,22)*EG(:VL,25)/EG(:VL,21)/EG(:VL,27)
      RB(:VL,136) = RF(:VL,136) / MAX(EQK(:VL), SMALL)
C      R137: O2 + CH3OCH3 = HO2 + CH3OCH2
      RF(:VL,137) = 3.13445932D1 -2.25994605D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,137),RF(1,137))
      EQK(:VL) = EG(:VL,21)*EG(:VL,25)/EG(:VL,20)/EG(:VL,27)
      RB(:VL,137) = RF(:VL,137) / MAX(EQK(:VL), SMALL)
C      R138: CH3OCH2 = CH3 + CH2O
      RF(:VL,138) = 3.01159278D1 -1.29578292D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,138),RF(1,138))
      EQK(:VL) = EG(:VL,5)*EG(:VL,16)/EG(:VL,25)*PFAC1(:VL)
      RB(:VL,138) = RF(:VL,138) / MAX(EQK(:VL), SMALL)
C      R139: CH3O + CH3OCH2 = CH2O + CH3OCH3
      RF(:VL,139) = 2.41D13
      EQK(:VL) = EG(:VL,16)*EG(:VL,27)/EG(:VL,19)/EG(:VL,25)
      RB(:VL,139) = RF(:VL,139) / MAX(EQK(:VL), SMALL)
C      R140: CH2O + CH3OCH2 = HCO + CH3OCH3
      RF(:VL,140) = 8.61068353D0 +2.8D0*ALOGT(:VL)
     * -2.94985611D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,140),RF(1,140))
      EQK(:VL) = EG(:VL,14)*EG(:VL,27)/EG(:VL,16)/EG(:VL,25)
      RB(:VL,140) = RF(:VL,140) / MAX(EQK(:VL), SMALL)
C      R141: HO2 + CH3OCH2 = OH + CH3OCH2O
      RF(:VL,141) = 9D12
      EQK(:VL) = EG(:VL,8)*EG(:VL,31)/EG(:VL,21)/EG(:VL,25)
      RB(:VL,141) = RF(:VL,141) / MAX(EQK(:VL), SMALL)
C      R142: CH3OCH2O = H + CH3OCHO
      RF(:VL,142) = 3.7398116D1 -6.6D-1*ALOGT(:VL)
     * -5.89769934D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,142),RF(1,142))
      EQK(:VL) = EG(:VL,1)*EG(:VL,30)/EG(:VL,31)*PFAC1(:VL)
      RB(:VL,142) = RF(:VL,142) / MAX(EQK(:VL), SMALL)
C      R143: O2 + CH3OCHO = HO2 + CH3OCO
      RF(:VL,143) = 2.99336062D1 -2.50098684D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,143),RF(1,143))
      EQK(:VL) = EG(:VL,21)*EG(:VL,29)/EG(:VL,20)/EG(:VL,30)
      RB(:VL,143) = RF(:VL,143) / MAX(EQK(:VL), SMALL)
C      R144: OH + CH3OCHO = H2O + CH3OCO
      RF(:VL,144) = 1.69682466D1 +1.61D0*ALOGT(:VL)
     * +1.76125834D1/T(:VL)
      CALL VRDA_EXP(VL,RF(1,144),RF(1,144))
      EQK(:VL) = EG(:VL,9)*EG(:VL,29)/EG(:VL,8)/EG(:VL,30)
      RB(:VL,144) = RF(:VL,144) / MAX(EQK(:VL), SMALL)
C      R145: HO2 + CH3OCHO = H2O2 + CH3OCO
      RF(:VL,145) = 2.7829872D1 -8.55468335D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,145),RF(1,145))
      EQK(:VL) = EG(:VL,22)*EG(:VL,29)/EG(:VL,21)/EG(:VL,30)
      RB(:VL,145) = RF(:VL,145) / MAX(EQK(:VL), SMALL)
C      R146: O + CH3OCHO = OH + CH3OCO
      RF(:VL,146) = 1.23673408D1 +2.5D0*ALOGT(:VL)
     * -1.12217317D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,146),RF(1,146))
      EQK(:VL) = EG(:VL,8)*EG(:VL,29)/EG(:VL,6)/EG(:VL,30)
      RB(:VL,146) = RF(:VL,146) / MAX(EQK(:VL), SMALL)
C      R147: H + CH3OCHO = H2 + CH3OCO
      RF(:VL,147) = 1.53306378D1 +2D0*ALOGT(:VL)
     * -2.51608334D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,147),RF(1,147))
      EQK(:VL) = EG(:VL,2)*EG(:VL,29)/EG(:VL,1)/EG(:VL,30)
      RB(:VL,147) = RF(:VL,147) / MAX(EQK(:VL), SMALL)
C      R148: CH3 + CH3OCHO = CH4 + CH3OCO
      RF(:VL,148) = -2.8103753D-1 +3.46D0*ALOGT(:VL)
     * -2.75813056D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,148),RF(1,148))
      EQK(:VL) = EG(:VL,7)*EG(:VL,29)/EG(:VL,5)/EG(:VL,30)
      RB(:VL,148) = RF(:VL,148) / MAX(EQK(:VL), SMALL)
C      R149: CH3OCO = CO + CH3O
      RF(:VL,149) = 2.96393694D1 -1.76D0*ALOGT(:VL)
     * -8.63016585D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,149),RF(1,149))
      EQK(:VL) = EG(:VL,12)*EG(:VL,19)/EG(:VL,29)*PFAC1(:VL)
      RB(:VL,149) = RF(:VL,149) / MAX(EQK(:VL), SMALL)
C      R150: CH3OCO = CH3 + CO2
      RF(:VL,150) = 2.80457763D1 -1.78D0*ALOGT(:VL)
     * -6.95445435D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,150),RF(1,150))
      EQK(:VL) = EG(:VL,5)*EG(:VL,23)/EG(:VL,29)*PFAC1(:VL)
      RB(:VL,150) = RF(:VL,150) / MAX(EQK(:VL), SMALL)
C      R151: O2 + CH3OCH2 = CH3OCH2O2
      RF(:VL,151) = 2D12
      EQK(:VL) = EG(:VL,35)/EG(:VL,20)/EG(:VL,25)/PFAC1(:VL)
      RB(:VL,151) = RF(:VL,151) / MAX(EQK(:VL), SMALL)
C      R152: 2CH3OCH2O2 = O2 + 2CH3OCH2O
      RF(:VL,152) = 5.3427584D1 -4.5D0*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,152),RF(1,152))
      EQK(:VL) = EG(:VL,20)*EG(:VL,31)*EG(:VL,31)/EG(:VL,35)/EG(:VL,35)
     1           *PFAC1(:VL)
      RB(:VL,152) = RF(:VL,152) / MAX(EQK(:VL), SMALL)
C      R153: 2CH3OCH2O2 = O2 + CH3OCHO + CH3OCH2OH
      RF(:VL,153) = 4.28553538D-1*RF(:VL,152)
      EQK(:VL) = EG(:VL,20)*EG(:VL,30)*EG(:VL,32)/EG(:VL,35)/EG(:VL,35)
     1           *PFAC1(:VL)
      RB(:VL,153) = RF(:VL,153) / MAX(EQK(:VL), SMALL)
C      R154: CH3OCH2O = CH2O + CH3O
      RF(:VL,154) = 3.68131678D1 -1.1D0*ALOGT(:VL)
     * -1.0386392D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,154),RF(1,154))
      EQK(:VL) = EG(:VL,16)*EG(:VL,19)/EG(:VL,31)*PFAC1(:VL)
      RB(:VL,154) = RF(:VL,154) / MAX(EQK(:VL), SMALL)
C      R155: O2 + CH3OCH2O = HO2 + CH3OCHO
      RF(:VL,155) = 2.46352888D1 -2.51608334D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,155),RF(1,155))
      EQK(:VL) = EG(:VL,21)*EG(:VL,30)/EG(:VL,20)/EG(:VL,31)
      RB(:VL,155) = RF(:VL,155) / MAX(EQK(:VL), SMALL)
C      R156: CH3OCH2O2 = CH2OCH2O2H
      RF(:VL,156) = 2.48176104D1 -1.08191584D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,156),RF(1,156))
      EQK(:VL) = EG(:VL,36)/EG(:VL,35)
      RB(:VL,156) = RF(:VL,156) / MAX(EQK(:VL), SMALL)
C      R157: CH2OCH2O2H = OH + 2CH2O
      RF(:VL,157) = 3.03390713D1 -1.03159417D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,157),RF(1,157))
      EQK(:VL) = EG(:VL,8)*EG(:VL,16)*EG(:VL,16)/EG(:VL,36)*PFAC2(:VL)
      RB(:VL,157) = RF(:VL,157) / MAX(EQK(:VL), SMALL)
C      R158: O2 + CH2OCH2O2H = O2CH2OCH2O2H
      RF(:VL,158) = 7D11
      EQK(:VL) = EG(:VL,38)/EG(:VL,20)/EG(:VL,36)/PFAC1(:VL)
      RB(:VL,158) = RF(:VL,158) / MAX(EQK(:VL), SMALL)
C      R159: O2CH2OCH2O2H = OH + HO2CH2OCHO
      RF(:VL,159) = 2.44121453D1 -9.30950835D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,159),RF(1,159))
      EQK(:VL) = EG(:VL,8)*EG(:VL,37)/EG(:VL,38)*PFAC1(:VL)
      RB(:VL,159) = RF(:VL,159) / MAX(EQK(:VL), SMALL)
C      R160: HO2CH2OCHO = OH + OCH2OCHO
      RF(:VL,160) = 3.79399738D1 -2.01286667D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,160),RF(1,160))
      EQK(:VL) = EG(:VL,8)*EG(:VL,33)/EG(:VL,37)*PFAC1(:VL)
      RB(:VL,160) = RF(:VL,160) / MAX(EQK(:VL), SMALL)
C      R161: OCH2OCHO = HOCH2OCO
      RF(:VL,161) = 2.5328436D1 -7.04503335D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,161),RF(1,161))
      EQK(:VL) = EG(:VL,34)/EG(:VL,33)
      RB(:VL,161) = RF(:VL,161) / MAX(EQK(:VL), SMALL)
C      R162: HOCH2OCO = CO + HOCH2O
      RF(:VL,162) = 3.76193093D1 -2.69D0*ALOGT(:VL)
     * -8.65532668D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,162),RF(1,162))
      EQK(:VL) = EG(:VL,12)*EG(:VL,28)/EG(:VL,34)*PFAC1(:VL)
      RB(:VL,162) = RF(:VL,162) / MAX(EQK(:VL), SMALL)
C      R163: HOCH2OCO = CH2OH + CO2
      RF(:VL,163) = 3.62085565D1 -2.61D0*ALOGT(:VL)
     * -1.04719389D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,163),RF(1,163))
      EQK(:VL) = EG(:VL,18)*EG(:VL,23)/EG(:VL,34)*PFAC1(:VL)
      RB(:VL,163) = RF(:VL,163) / MAX(EQK(:VL), SMALL)
C      R164: HOCH2O = H + HCOOH
      RF(:VL,164) = 3.22361913D1 -7.49792835D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,164),RF(1,164))
      EQK(:VL) = EG(:VL,1)*EG(:VL,26)/EG(:VL,28)*PFAC1(:VL)
      RB(:VL,164) = RF(:VL,164) / MAX(EQK(:VL), SMALL)
C      R165: OH + CH2O = HOCH2O
      RF(:VL,165) = 3.60428538D1 -1.11D0*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,165),RF(1,165))
      EQK(:VL) = EG(:VL,28)/EG(:VL,8)/EG(:VL,16)/PFAC1(:VL)
      RB(:VL,165) = RF(:VL,165) / MAX(EQK(:VL), SMALL)
C      R166: HCOOH = H2O + CO
      RF(:VL,166) = 3.07665153D1 -2.51608334D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,166),RF(1,166))
      EQK(:VL) = EG(:VL,9)*EG(:VL,12)/EG(:VL,26)*PFAC1(:VL)
      RB(:VL,166) = RF(:VL,166) / MAX(EQK(:VL), SMALL)
C      R167: HCOOH = H2 + CO2
      RF(:VL,167) = 3.72468266D1 -2.86833501D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,167),RF(1,167))
      EQK(:VL) = EG(:VL,2)*EG(:VL,23)/EG(:VL,26)*PFAC1(:VL)
      RB(:VL,167) = RF(:VL,167) / MAX(EQK(:VL), SMALL)
C      R168: HCOOH = OH + HCO
      RF(:VL,168) = 4.29710651D1 -4.6D-1*ALOGT(:VL)
     * -5.44983651D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,168),RF(1,168))
      EQK(:VL) = EG(:VL,8)*EG(:VL,14)/EG(:VL,26)*PFAC1(:VL)
      RB(:VL,168) = RF(:VL,168) / MAX(EQK(:VL), SMALL)
C      R169: OH + HCOOH = H + H2O + CO2
      RF(:VL,169) = 1.47786849D1 +2.06D0*ALOGT(:VL)
     * -4.60946468D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,169),RF(1,169))
      EQK(:VL) = EG(:VL,1)*EG(:VL,9)*EG(:VL,23)/EG(:VL,8)/EG(:VL,26)
     1           *PFAC1(:VL)
      RB(:VL,169) = RF(:VL,169) / MAX(EQK(:VL), SMALL)
C      R170: HCOOH = H2O + CO
      RF(:VL,170) = 1.67332813D1 +1.51D0*ALOGT(:VL)
     * +4.84094434D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,170),RF(1,170))
      EQK(:VL) = EG(:VL,9)*EG(:VL,12)/EG(:VL,26)*PFAC1(:VL)
      RB(:VL,170) = RF(:VL,170) / MAX(EQK(:VL), SMALL)
C      R171: HCOOH = H2 + CO2
      RF(:VL,171) = 1.52600738D1 +2.1D0*ALOGT(:VL)
     * -2.44965874D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,171),RF(1,171))
      EQK(:VL) = EG(:VL,2)*EG(:VL,23)/EG(:VL,26)*PFAC1(:VL)
      RB(:VL,171) = RF(:VL,171) / MAX(EQK(:VL), SMALL)
C      R172: H + HCOOH = H2 + OH + CO
      RF(:VL,172) = 3.17303532D1 -3.5D-1*ALOGT(:VL)
     * -1.5036114D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,172),RF(1,172))
      EQK(:VL) = EG(:VL,2)*EG(:VL,8)*EG(:VL,12)/EG(:VL,1)/EG(:VL,26)
     1           *PFAC1(:VL)
      RB(:VL,172) = RF(:VL,172) / MAX(EQK(:VL), SMALL)
C      R173: CH3 + HCOOH = CH4 + OH + CO
      RF(:VL,173) = -1.47571191D1 +5.8D0*ALOGT(:VL)
     * -1.10707667D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,173),RF(1,173))
      EQK(:VL) = EG(:VL,7)*EG(:VL,8)*EG(:VL,12)/EG(:VL,5)/EG(:VL,26)
     1           *PFAC1(:VL)
      RB(:VL,173) = RF(:VL,173) / MAX(EQK(:VL), SMALL)
C      R174: HO2 + HCOOH = OH + CO + H2O2
      RF(:VL,174) = 2.76310211D1 -5.99834268D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,174),RF(1,174))
      EQK(:VL) = EG(:VL,8)*EG(:VL,12)*EG(:VL,22)/EG(:VL,21)/EG(:VL,26)
     1           *PFAC1(:VL)
      RB(:VL,174) = RF(:VL,174) / MAX(EQK(:VL), SMALL)
C      R175: O + HCOOH = 2OH + CO
      RF(:VL,175) = 4.20175112D1 -1.9D0*ALOGT(:VL)
     * -1.49706959D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,175),RF(1,175))
      EQK(:VL) = EG(:VL,8)*EG(:VL,8)*EG(:VL,12)/EG(:VL,6)/EG(:VL,26)
     1           *PFAC1(:VL)
      RB(:VL,175) = RF(:VL,175) / MAX(EQK(:VL), SMALL)
C
      RKLOW(:VL,1) = 4.79026732D1 -1.72D0*ALOGT(:VL) 
     * -2.64088107D2/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,1),RKLOW(1,1))
      RKLOW(:VL,2) = 3.93279334D1 -2.28963584D4/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,2),RKLOW(1,2))
      RKLOW(:VL,3) = 5.57002972D1 -2.79D0*ALOGT(:VL) 
     * -2.10898105D3/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,3),RKLOW(1,3))
      RKLOW(:VL,4) = 7.34663067D1 -3.75D0*ALOGT(:VL) 
     * -4.93957481D2/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,4),RKLOW(1,4))
      RKLOW(:VL,5) = 7.68923562D1 -4.76D0*ALOGT(:VL) 
     * -1.22784867D3/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,5),RKLOW(1,5))
      RKLOW(:VL,6) = 9.50941235D1 -7.08D0*ALOGT(:VL) 
     * -3.36400342D3/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,6),RKLOW(1,6))
      RKLOW(:VL,7) = 1.17075165D2 -9.31D0*ALOGT(:VL) 
     * -5.02512164D4/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,7),RKLOW(1,7))
      RKLOW(:VL,8) = 9.68908955D1 -7.62D0*ALOGT(:VL) 
     * -3.50742017D3/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,8),RKLOW(1,8))
      RKLOW(:VL,9) = 6.9414025D1 -3.86D0*ALOGT(:VL) 
     * -1.67067934D3/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,9),RKLOW(1,9))
      RKLOW(:VL,10) = 9.34384048D1 -7.27D0*ALOGT(:VL) 
     * -3.63322434D3/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,10),RKLOW(1,10))
      RKLOW(:VL,11) = 6.33329483D1 -3.14D0*ALOGT(:VL) 
     * -6.18956501D2/T(:VL)
      CALL VRDA_EXP(VL,RKLOW(1,11),RKLOW(1,11))
C
      END

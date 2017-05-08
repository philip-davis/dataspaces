      SUBROUTINE RATT3 (VL, T, RF, RB, RKLOW, EG, PFAC1, 
     &                   PFAC2, PFAC3, ALOGT)
      USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      PARAMETER (RU=8.31451D7, SMALL=1.D-200, PATM=1.01325D6)
      DIMENSION RF(MAXVL,*), RB(MAXVL,*), RKLOW(MAXVL,*)
      DIMENSION EQK(MAXVL), PFAC1(MAXVL), PFAC2(MAXVL), PFAC3(MAXVL)
      DIMENSION T(MAXVL),ALOGT(MAXVL)
      INTEGER VL
      DIMENSION SMH(MAXVL,26), EG(MAXVL,26)


C      R61: oh + ch3oh = h2o + ch2oh
      RF(:VL,61) = 1.57756053D1 +1.8D0*ALOGT(:VL)
     * +2.99917134D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,61),RF(1,61))
      RB(:VL,61) = 3.4943841D0 +3.46D0*ALOGT(:VL)
     * -1.14330827D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,61),RB(1,61))
C      R62: h + ch3oh = h2 + ch3o
      RF(:VL,62) = 2.8911955D1 -3.06710559D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,62),RF(1,62))
      RB(:VL,62) = 2.96415144D1 -2D-2*ALOGT(:VL)
     * -3.93767042D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,62),RB(1,62))
C      R63: h + ch3oh = h2 + ch2oh
      RF(:VL,63) = 4D0*RF(:VL,62)
      RB(:VL,63) = 1.65518242D1 +1.66D0*ALOGT(:VL)
     * -7.17083751D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,63),RB(1,63))
C      R64: o + ch3oh = oh + ch2oh
      RF(:VL,64) = 1.28687606D1 +2.5D0*ALOGT(:VL)
     * -1.54990734D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,64),RF(1,64))
      RB(:VL,64) = -1.70045731D0 +4.16D0*ALOGT(:VL)
     * -4.60090999D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,64),RB(1,64))
C      R65: o2 + ch2oh = ho2 + ch2o
      RF(:VL,65) = 1.33862649D1 +2.27D0*ALOGT(:VL)
     * +3.87476834D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,65),RF(1,65))
      RB(:VL,65) = 2.41311078D1 +9.4D-1*ALOGT(:VL)
     * -1.09550269D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,65),RB(1,65))
C      R66: ch2oh = h + ch2o
      RF(:VL,66) = 3.32658107D1 -7.3D-1*ALOGT(:VL)
     * -1.6515571D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,66),RF(1,66))
      EQK(:VL) = EG(:VL,1)*EG(:VL,14)/EG(:VL,23)*PFAC1(:VL)
      RB(:VL,66) = RF(:VL,66) / MAX(EQK(:VL), SMALL)
C      R67: o2 + c2h3 = ho2 + c2h2
      RF(:VL,67) = -1.30640945D1 +6D0*ALOGT(:VL)
     * -4.77250688D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,67),RF(1,67))
      RB(:VL,67) = -1.60101385D1 +6.33D0*ALOGT(:VL)
     * -8.84151685D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,67),RB(1,67))
C      R68: o + h2o2 = oh + ho2
      RF(:VL,68) = 1.60720517D1 +2D0*ALOGT(:VL)
     * -1.99777017D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,68),RF(1,68))
      RB(:VL,68) = 1.20027317D1 +2.33D0*ALOGT(:VL)
     * -9.14847902D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,68),RB(1,68))
C      R69: o + c2h2 = h + hcco
      RF(:VL,69) = 1.64757701D1 +2D0*ALOGT(:VL)
     * -9.56111669D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,69),RF(1,69))
      RB(:VL,69) = 1.22165179D1 +2D0*ALOGT(:VL)
     * -6.69781385D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,69),RB(1,69))
C      R70: oh + c2h2 = h + ch2co
      RF(:VL,70) = -8.42643883D0 +4.5D0*ALOGT(:VL)
     * +5.03216668D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,70),RF(1,70))
      RB(:VL,70) = -6.1371842D0 +4.5D0*ALOGT(:VL)
     * -9.89323969D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,70),RB(1,70))
C      R71: oh + ch2co = h2o + hcco
      RF(:VL,71) = 2.99336062D1 -1.00643334D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,71),RF(1,71))
      RB(:VL,71) = 2.56734432D1 -5.02965059D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,71),RB(1,71))
C      R72: h + ch2co = h2 + hcco
      RF(:VL,72) = 3.29293385D1 -4.02573334D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,72),RF(1,72))
      RB(:VL,72) = 2.72036171D1 -4.22702001D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,72),RB(1,72))
C      R73: oh + hcco = 2hco
      RF(:VL,73) = 1D13
      RB(:VL,73) = 3.31162329D1 -2.03098247D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,73),RB(1,73))
C      R74: h + hcco = co + ch2(s)
      RF(:VL,74) = 1.1D14
      RB(:VL,74) = 2.83469078D1 +8.9D-1*ALOGT(:VL)
     * -1.40045199D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,74),RB(1,74))
C      R75: o + hcco = h + 2co
      RF(:VL,75) = 8D13
      RB(:VL,75) = 0D0
C      R76: o2 + c2h6 = ho2 + c2h5
      RF(:VL,76) = 3.17303532D1 -2.61018486D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,76),RF(1,76))
      RB(:VL,76) = 1.94061243D1 +8.9D-1*ALOGT(:VL)
     * +9.67182435D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,76),RB(1,76))
C      R77: ho2 + c2h6 = h2o2 + c2h5
      RF(:VL,77) = 3.02112379D1 -1.03008452D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,77),RF(1,77))
      RB(:VL,77) = 2.51418655D1 +2.4D-1*ALOGT(:VL)
     * -3.95125727D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,77),RB(1,77))
C      R78: ch3 + c2h3 = ch4 + c2h2
      RF(:VL,78) = 3.92D11
      RB(:VL,78) = 3.10194709D1 -3.32374609D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,78),RB(1,78))
C      R79: ch2o + ch3oh = 2ch3o
      RF(:VL,79) = 3.12777756D1 +5D-2*ALOGT(:VL)
     * -4.26325161D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,79),RF(1,79))
      RB(:VL,79) = 6.03D13
C      R80: ch2o + ch3o = hco + ch3oh
      RF(:VL,80) = 2.53482387D1 -1.49958567D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,80),RF(1,80))
      RB(:VL,80) = 2.20908869D1 +2D-2*ALOGT(:VL)
     * -8.01120935D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,80),RB(1,80))
C      R81: h + c2h3 = h2 + c2h2
      RF(:VL,81) = 3.06267534D1 -1.25804167D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,81),RF(1,81))
      RB(:VL,81) = 3.02195367D1 -3.42589907D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,81),RB(1,81))
C      R82: oh + ch3oh = h2o + ch3o
      RF(:VL,82) = 1.38155106D1 +2.1D0*ALOGT(:VL)
     * -2.49947719D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,82),RF(1,82))
      RB(:VL,82) = 1.60106218D1 +2.08D0*ALOGT(:VL)
     * -8.74590568D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,82),RB(1,82))
C      R83: h + c2h5 = 2ch3
      RF(:VL,83) = 3.61D13
      RB(:VL,83) = 3.85362429D1 -1.03D0*ALOGT(:VL)
     * -8.54461902D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,83),RB(1,83))
C      R84: o2 + c2h3 = hco + ch2o
      RF(:VL,84) = 6.73055959D1 -5.31D0*ALOGT(:VL)
     * -3.27090834D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,84),RF(1,84))
      RB(:VL,84) = 6.72799764D1 -5.31D0*ALOGT(:VL)
     * -4.68243109D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,84),RB(1,84))
C      R85: c2h6 = h + c2h5
      RF(:VL,85) = 4.93778164D1 -1.56D0*ALOGT(:VL)
     * -5.22338901D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,85),RF(1,85))
      RB(:VL,85) = 3.61D13
C      R86: ch3 + c2h4 = ch4 + c2h3
      RF(:VL,86) = 1.89009537D0 +3.7D0*ALOGT(:VL)
     * -4.78055834D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,86),RF(1,86))
      RB(:VL,86) = 3.64643114D-1 +4.02D0*ALOGT(:VL)
     * -2.75360161D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,86),RB(1,86))
C      R87: ch3co = co + ch3
      RF(:VL,87) = 2.87296334D1 -8.41378268D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,87),RF(1,87))
      EQK(:VL) = EG(:VL,7)*EG(:VL,10)/EG(:VL,26)*PFAC1(:VL)
      RB(:VL,87) = RF(:VL,87) / MAX(EQK(:VL), SMALL)
C      R88: ch3cho = hco + ch3
      RF(:VL,88) = 3.5499658D1 +1.5D-1*ALOGT(:VL)
     * -4.05341026D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,88),RF(1,88))
      RB(:VL,88) = 2D13
C      R89: o2 + ch3cho = ho2 + ch3co
      RF(:VL,89) = 3.10355463D1 -1.97009325D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,89),RF(1,89))
      RB(:VL,89) = 2.51720161D1 +3.2D-1*ALOGT(:VL)
     * +9.76240335D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,89),RB(1,89))
      END

      SUBROUTINE RATT4 (VL, T, RF, RB, RKLOW, EG, PFAC1, 
     &                   PFAC2, PFAC3, ALOGT)
      USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      PARAMETER (RU=8.31451D7, SMALL=1.D-200, PATM=1.01325D6)
      DIMENSION RF(MAXVL,*), RB(MAXVL,*), RKLOW(MAXVL,*)
      DIMENSION EQK(MAXVL), PFAC1(MAXVL), PFAC2(MAXVL), PFAC3(MAXVL)
      DIMENSION T(MAXVL),ALOGT(MAXVL)
      INTEGER VL
      DIMENSION SMH(MAXVL,26), EG(MAXVL,26)

C      R90: oh + ch3cho = h2o + ch3co
      RF(:VL,90) = 1.45086577D1 +1.8D0*ALOGT(:VL)
     * -6.54181668D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,90),RF(1,90))
      RB(:VL,90) = 1.41185737D1 +1.79D0*ALOGT(:VL)
     * -1.65306675D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,90),RB(1,90))
C      R91: h + ch3cho = h2 + ch3co
      RF(:VL,91) = 3.02262758D1 -1.660615D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,91),RF(1,91))
      RB(:VL,91) = 2.83710519D1 -1D-2*ALOGT(:VL)
     * -9.90833619D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,91),RB(1,91))
C      R92: o + ch3cho = oh + ch3co
      RF(:VL,92) = 2.94127302D1 -9.40008735D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,92),RF(1,92))
      RB(:VL,92) = 2.6734533D1 -1D-2*ALOGT(:VL)
     * -8.13701352D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,92),RB(1,92))
C      R93: ho2 + ch3cho = h2o2 + ch3co
      RF(:VL,93) = 2.87329612D1 -5.99834268D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,93),RF(1,93))
      RB(:VL,93) = 3.01242266D1 -3.4D-1*ALOGT(:VL)
     * -6.04363218D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,93),RB(1,93))
C      R94: ch3 + ch3cho = ch4 + ch3co
      RF(:VL,94) = 1.47740942D1 +1.78D0*ALOGT(:VL)
     * -2.97451372D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,94),RF(1,94))
      RB(:VL,94) = 1.6182009D1 +1.77D0*ALOGT(:VL)
     * -1.14632757D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,94),RB(1,94))
C      R95: ch3 + c2h2 = h + c3h4-p
      RF(:VL,95) = 3.9335393D1 -1.2D0*ALOGT(:VL)
     * -8.39365402D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,95),RF(1,95))
      RB(:VL,95) = 3.22361913D1 -2.01286667D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,95),RB(1,95))
C      R96: c3h5-a = ch3 + c2h2
      RF(:VL,96) = 1.11398302D2 -9.9D0*ALOGT(:VL)
     * -4.13040241D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,96),RF(1,96))
      RB(:VL,96) = 1.06878264D2 -9.82D0*ALOGT(:VL)
     * -1.85938559D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,96),RB(1,96))
C      R97: c3h6 = ch3 + c2h3
      RF(:VL,97) = 1.43764577D2 -1.328D1*ALOGT(:VL)
     * -6.19962935D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,97),RF(1,97))
      RB(:VL,97) = 1.37402633D2 -1.319D1*ALOGT(:VL)
     * -1.48650204D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,97),RB(1,97))
C      R98: c3h6 = h + c3h5-a
      RF(:VL,98) = 1.41155825D2 -1.326D1*ALOGT(:VL)
     * -5.96311751D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,98),RF(1,98))
      RB(:VL,98) = 1.30531344D2 -1.225D1*ALOGT(:VL)
     * -1.4130324D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,98),RB(1,98))
C      R99: o + c3h6 = h + ch3 + ch2co
      RF(:VL,99) = 1.70343864D1 +1.76D0*ALOGT(:VL)
     * -3.82444667D1/T(:VL)
      CALL VRDA_EXP(VL,RF(1,99),RF(1,99))
      RB(:VL,99) = 1D0
C      R100: o + c3h6 = hco + c2h5
      RF(:VL,100) = 1.65755205D1 +1.76D0*ALOGT(:VL)
     * +6.11911468D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,100),RF(1,100))
      RB(:VL,100) = 1.18508253D1 +1.88D0*ALOGT(:VL)
     * -1.33402739D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,100),RB(1,100))
C      R101: o + c3h6 = 2h + ch3chco
      RF(:VL,101) = RF(:VL,99)
      RB(:VL,101) = 0D0
C      R102: oh + c3h6 = h2o + c3h5-a
      RF(:VL,102) = 1.49533436D1 +2D0*ALOGT(:VL)
     * +1.49958567D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,102),RF(1,102))
      RB(:VL,102) = 1.56390916D1 +2.01D0*ALOGT(:VL)
     * -1.60425474D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,102),RB(1,102))
C      R103: c4h6 = 2c2h3
      RF(:VL,103) = 4.51421384D1 -1D0*ALOGT(:VL)
     * -4.93907159D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,103),RF(1,103))
      RB(:VL,103) = 1.26D13
C      R104: oh + c4h6 = c2h5 + ch2co
      RF(:VL,104) = 1D12
      RB(:VL,104) = 2.89474293D1 -1.51065644D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,104),RB(1,104))
C      R105: oh + c4h6 = ch2o + c3h5-a
      RF(:VL,105) = 1D12
      RB(:VL,105) = 1.50685592D1 -3.57585764D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,105),RB(1,105))
C      R106: oh + c4h6 = c2h3 + ch3cho
      RF(:VL,106) = 1D12
      RB(:VL,106) = 2.70216635D1 -9.33466918D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,106),RB(1,106))
C      R107: o + c4h6 = ch2o + c3h4-a
      RF(:VL,107) = 1D12
      RB(:VL,107) = 2.77033418D1 -3.97792776D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,107),RB(1,107))
C      R108: o2 + c2h4 = ho2 + c2h3
      RF(:VL,108) = 3.13199006D1 -2.92872101D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,108),RF(1,108))
      RB(:VL,108) = 3.15307691D1 -5D-1*ALOGT(:VL)
     * -6.88400401D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,108),RB(1,108))
C      R109: nc3h7 = ch3 + c2h4
      RF(:VL,109) = 3.30621196D1 -5.5D-1*ALOGT(:VL)
     * -1.42913534D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,109),RF(1,109))
      RB(:VL,109) = 2.6739423D1 -3.62517287D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,109),RB(1,109))
C      R110: nc3h7 = h + c3h6
      RF(:VL,110) = 3.55197306D1 -6.4D-1*ALOGT(:VL)
     * -1.85284377D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,110),RF(1,110))
      RB(:VL,110) = 5D-1*RF(:VL,81)
C      R111: o2 + nc3h7 = ho2 + c3h6
      RF(:VL,111) = 2.64270483D1 -1.50965D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,111),RF(1,111))
      RB(:VL,111) = 2.60215832D1 -8.80629168D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,111),RB(1,111))
C      R112: o + c3h6 = oh + c3h5-a
      RF(:VL,112) = 2.69847575D1 +7D-1*ALOGT(:VL)
     * -2.96092687D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,112),RF(1,112))
      RB(:VL,112) = 2.53819768D1 +7.1D-1*ALOGT(:VL)
     * -1.0476971D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,112),RB(1,112))
C      R113: h + c3h6 = h2 + c3h5-a
      RF(:VL,113) = 1.20610469D1 +2.5D0*ALOGT(:VL)
     * -1.25401594D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,113),RF(1,113))
      RB(:VL,113) = 1.12813716D1 +2.51D0*ALOGT(:VL)
     * -9.82278935D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,113),RB(1,113))
C      R114: h + c3h6 = ch3 + c2h4
      RF(:VL,114) = 7.75601545D1 -5.81D0*ALOGT(:VL)
     * -9.30950835D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,114),RF(1,114))
      RB(:VL,114) = 7.68238535D1 -5.9D0*ALOGT(:VL)
     * -1.5911711D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,114),RB(1,114))
C      R115: c5h9 = c2h4 + c3h5-a
      RF(:VL,115) = 3.08498969D1 -2.264475D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,115),RF(1,115))
      RB(:VL,115) = 0D0
C      R116: c5h9 = ch3 + c4h6
      RF(:VL,116) = 3.48306995D1 -5.2D-1*ALOGT(:VL)
     * -1.92832627D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,116),RF(1,116))
      RB(:VL,116) = 2.58880518D1 -3.82444667D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,116),RB(1,116))
C      R117: c4h7 = h + c4h6
      RF(:VL,117) = 3.24185129D1 -2.48085817D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,117),RF(1,117))
      RB(:VL,117) = 3.13199006D1 -6.54181668D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,117),RB(1,117))
C      R118: c4h7 = c2h4 + c2h3
      RF(:VL,118) = 2.5328436D1 -1.86190167D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,118),RF(1,118))
      RB(:VL,118) = 2.46352888D1 -3.52251667D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,118),RB(1,118))
C      R119: ch3 + c4h7 = ch4 + c4h6
      RF(:VL,119) = 8D12
      RB(:VL,119) = 3.18866338D1 -2.88242507D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,119),RB(1,119))

      END

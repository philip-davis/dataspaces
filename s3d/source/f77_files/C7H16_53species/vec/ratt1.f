C                                                                      C
      SUBROUTINE RATT (VL, T, RF, RB, RKLOW)
      USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      PARAMETER (RU=8.31451D7, SMALL=1.D-200, PATM=1.01325D6)
      DIMENSION RF(MAXVL,*), RB(MAXVL,*), RKLOW(MAXVL,*)
      DIMENSION EQK(MAXVL), PFAC1(MAXVL), PFAC2(MAXVL), PFAC3(MAXVL)
      DIMENSION T(MAXVL),ALOGT(MAXVL)
      INTEGER VL
      DIMENSION SMH(MAXVL,26), EG(MAXVL,26)
C
      CALL VRDA_LOG(VL,T,ALOGT)
C
      CALL RDSMH (VL, T, SMH)
      CALL VRDA_EXP(VL,SMH(1,1),EG(1,1))
      CALL VRDA_EXP(VL,SMH(1,2),EG(1,2))
      CALL VRDA_EXP(VL,SMH(1,3),EG(1,3))
      CALL VRDA_EXP(VL,SMH(1,4),EG(1,4))
      CALL VRDA_EXP(VL,SMH(1,5),EG(1,5))
      CALL VRDA_EXP(VL,SMH(1,7),EG(1,7))
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
      CALL VRDA_EXP(VL,SMH(1,20),EG(1,20))
      CALL VRDA_EXP(VL,SMH(1,21),EG(1,21))
      CALL VRDA_EXP(VL,SMH(1,22),EG(1,22))
      CALL VRDA_EXP(VL,SMH(1,23),EG(1,23))
      CALL VRDA_EXP(VL,SMH(1,26),EG(1,26))
      PFAC1(:VL) = PATM / (RU*T(:VL))
      PFAC2(:VL) = PFAC1(:VL)*PFAC1(:VL)
      PFAC3(:VL) = PFAC2(:VL)*PFAC1(:VL)

      CALL RATT1( VL, T, RF, RB, RKLOW, EG, PFAC1, PFAC2, PFAC3, ALOGT )
      CALL RATT2( VL, T, RF, RB, RKLOW, EG, PFAC1, PFAC2, PFAC3, ALOGT )
      CALL RATT3( VL, T, RF, RB, RKLOW, EG, PFAC1, PFAC2, PFAC3, ALOGT )
      CALL RATT4( VL, T, RF, RB, RKLOW, EG, PFAC1, PFAC2, PFAC3, ALOGT )
      CALL RATT5( VL, T, RF, RB, RKLOW, EG, PFAC1, PFAC2, PFAC3, ALOGT )
      CALL RATT6( VL, T, RF, RB, RKLOW, EG, PFAC1, PFAC2, PFAC3, ALOGT )
      CALL RATT7( VL, T, RF, RB, RKLOW, EG, PFAC1, PFAC2, PFAC3, ALOGT )
      CALL RATT8( VL, T, RF, RB, RKLOW, EG, PFAC1, PFAC2, PFAC3, ALOGT )

      END
C----------------------------------------------------------------------C
      SUBROUTINE RATT1(VL, T, RF, RB, RKLOW, EG, PFAC1, 
     &                   PFAC2, PFAC3, ALOGT)
      USE chemkin_m, only : MAXVL
      !IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      IMPLICIT NONE
      DOUBLE PRECISION RU, SMALL, PATM
      DOUBLE PRECISION RF, RB, RKLOW
      DOUBLE PRECISION EQK, PFAC1, PFAC2, PFAC3, T, ALOGT
      DOUBLE PRECISION SMH, EG
      PARAMETER (RU=8.31451D7, SMALL=1.D-200, PATM=1.01325D6)
      DIMENSION RF(MAXVL,*), RB(MAXVL,*), RKLOW(MAXVL,*)
      DIMENSION EQK(MAXVL), PFAC1(MAXVL), PFAC2(MAXVL), PFAC3(MAXVL)
      DIMENSION T(MAXVL),ALOGT(MAXVL)
      INTEGER VL
      DIMENSION SMH(MAXVL,26), EG(MAXVL,26)

C      R1: h + ch3 = ch4
      RF(:VL,1) = 3.52986472D1 -4D-1*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,1),RF(1,1))
      EQK(:VL) = EG(:VL,11)/EG(:VL,1)/EG(:VL,10)/PFAC1(:VL)
      RB(:VL,1) = RF(:VL,1) / MAX(EQK(:VL), SMALL)
C      R2: h + ch4 = h2 + ch3
      RF(:VL,2) = 9.75672617D0 +3D0*ALOGT(:VL)
     * -4.13845387D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,2),RF(1,2))
      RB(:VL,2) = 6.49375384D0 +3D0*ALOGT(:VL)
     * -3.89690987D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,2),RB(1,2))
C      R3: oh + ch4 = h2o + ch3
      RF(:VL,3) = 1.21704455D1 +2.4D0*ALOGT(:VL)
     * -1.0597743D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,3),RF(1,3))
      RB(:VL,3) = 1.03731786D1 +2.4D0*ALOGT(:VL)
     * -8.44397568D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,3),RB(1,3))
C      R4: o + ch4 = oh + ch3
      RF(:VL,4) = 2.87784236D1 +5D-1*ALOGT(:VL)
     * -5.17809951D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,4),RF(1,4))
      RB(:VL,4) = 2.46928027D1 +5D-1*ALOGT(:VL)
     * -3.88231659D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,4),RB(1,4))
C      R5: oh + hco = h2o + co
      RF(:VL,5) = 1.02D14
      RB(:VL,5) = 3.56021069D1 -5.29383934D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,5),RB(1,5))
C      R6: oh + co = h + co2
      RF(:VL,6) = 1.18493977D1 +1.95D0*ALOGT(:VL)
     * +6.77832851D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,6),RF(1,6))
      RB(:VL,6) = 1.65678966D1 +1.95D0*ALOGT(:VL)
     * -1.05625179D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,6),RB(1,6))
C      R7: h + o2 = o + oh
      RF(:VL,7) = 3.29142248D1 -8.32320368D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,7),RF(1,7))
      RB(:VL,7) = 3.03750818D1 -2.13867084D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,7),RB(1,7))
C      R8: h2 + o = h + oh
      RF(:VL,8) = 1.08356516D1 +2.67D0*ALOGT(:VL)
     * -3.16623927D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,8),RF(1,8))
      RB(:VL,8) = 1.00127903D1 +2.67D0*ALOGT(:VL)
     * -2.11200035D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,8),RB(1,8))
C      R9: o + h2o = 2oh
      RF(:VL,9) = 1.49040725D1 +2.02D0*ALOGT(:VL)
     * -6.74310335D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,9),RF(1,9))
      RB(:VL,9) = 1.26158617D1 +2.02D0*ALOGT(:VL)
     * +1.93738417D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,9),RB(1,9))
C      R10: h2 + oh = h + h2o
      RF(:VL,10) = 1.9190789D1 +1.51D0*ALOGT(:VL)
     * -1.72603317D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,10),RF(1,10))
      RB(:VL,10) = 2.0656271D1 +1.51D0*ALOGT(:VL)
     * -9.34976568D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,10),RB(1,10))
C      R11: hco = h + co
      RF(:VL,11) = 3.97645231D1 -1D0*ALOGT(:VL)
     * -8.55468335D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,11),RF(1,11))
      RB(:VL,11) = 3.18003185D1 +2.22421767D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,11),RB(1,11))
C      R12: oh + h2o2 = h2o + ho2
      RF(:VL,12) = 1D12
      RB(:VL,12) = 2.58502016D1 +3.3D-1*ALOGT(:VL)
     * -1.58311964D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,12),RB(1,12))
C      R13: o + c2h4 = hco + ch3
      RF(:VL,13) = 1.61378983D1 +1.88D0*ALOGT(:VL)
     * -9.00757835D1/T(:VL)
      CALL VRDA_EXP(VL,RF(1,13),RF(1,13))
      RB(:VL,13) = 1.94683506D1 +1.05D0*ALOGT(:VL)
     * -1.59871935D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,13),RB(1,13))
C      R14: h + c2h4 = c2h5
      RF(:VL,14) = 2.77089077D1 +4.5D-1*ALOGT(:VL)
     * -9.16860768D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,14),RF(1,14))
      EQK(:VL) = EG(:VL,18)/EG(:VL,1)/EG(:VL,17)/PFAC1(:VL)
      RB(:VL,14) = RF(:VL,14) / MAX(EQK(:VL), SMALL)
C      R15: ch3oh = oh + ch3
      RF(:VL,15) = 3.74832154D1 -4.61600649D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,15),RF(1,15))
      EQK(:VL) = EG(:VL,5)*EG(:VL,10)/EG(:VL,22)*PFAC1(:VL)
      RB(:VL,15) = RF(:VL,15) / MAX(EQK(:VL), SMALL)
C      R16: h + c2h6 = h2 + c2h5
      RF(:VL,16) = 6.31716469D0 +3.5D0*ALOGT(:VL)
     * -2.60012052D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,16),RF(1,16))
      RB(:VL,16) = -1.99878364D0 +4.06D0*ALOGT(:VL)
     * -4.45699003D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,16),RB(1,16))
C      R17: ho2 + ch3oh = h2o2 + ch2oh
      RF(:VL,17) = 3.1314888D1 -9.76240335D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,17),RF(1,17))
      RB(:VL,17) = 2.0814933D1 +1.33D0*ALOGT(:VL)
     * -5.66118751D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,17),RB(1,17))
C      R18: o2 + c2h5 = ho2 + c2h4
      RF(:VL,18) = 6.92764036D1 -5.76D0*ALOGT(:VL)
     * -5.08248834D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,18),RF(1,18))
      RB(:VL,18) = 6.93078705D1 -5.63D0*ALOGT(:VL)
     * -1.12217317D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,18),RB(1,18))
C      R19: oh + c2h6 = h2o + c2h5
      RF(:VL,19) = 1.78759536D1 +1.73D0*ALOGT(:VL)
     * -5.83731334D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,19),RF(1,19))
      RB(:VL,19) = 1.10254908D1 +2.29D0*ALOGT(:VL)
     * -1.00643334D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,19),RB(1,19))
C      R20: o + c2h6 = oh + c2h5
      RF(:VL,20) = 1.63804599D1 +2.13D0*ALOGT(:VL)
     * -2.61169451D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,20),RF(1,20))
      RB(:VL,20) = 7.24208236D0 +2.69D0*ALOGT(:VL)
     * -3.41432509D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,20),RB(1,20))
C      R21: ch3 + ho2 = oh + ch3o
      RF(:VL,21) = 1.1D13
      RB(:VL,21) = 3.38006318D1 -3.5D-1*ALOGT(:VL)
     * -1.23539692D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,21),RB(1,21))
C      R22: co + ho2 = oh + co2
      RF(:VL,22) = 3.10355463D1 -1.15739834D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,22),RF(1,22))
      RB(:VL,22) = 3.64005282D1 -3.3D-1*ALOGT(:VL)
     * -4.25771622D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,22),RB(1,22))
C      R23: 2ch3 = c2h6
      RF(:VL,23) = 3.90620856D1 -1.17D0*ALOGT(:VL)
     * -3.19945157D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,23),RF(1,23))
      EQK(:VL) = EG(:VL,16)/EG(:VL,10)/EG(:VL,10)/PFAC1(:VL)
      RB(:VL,23) = RF(:VL,23) / MAX(EQK(:VL), SMALL)
C      R24: h2o = h + oh
      RF(:VL,24) = 6.27779313D1 -3D0*ALOGT(:VL)
     * -6.16943635D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,24),RF(1,24))
      RB(:VL,24) = 5.14678023D1 -2D0*ALOGT(:VL)
      CALL VRDA_EXP(VL,RB(1,24),RB(1,24))
C      R25: h + o2 = ho2
      RF(:VL,25) = 2.80196791D1 +6D-1*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,25),RF(1,25))
      EQK(:VL) = EG(:VL,12)/EG(:VL,1)/EG(:VL,4)/PFAC1(:VL)
      RB(:VL,25) = RF(:VL,25) / MAX(EQK(:VL), SMALL)
C      R26: o + co = co2
      RF(:VL,26) = 2.36136376D1 -1.19966854D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,26),RF(1,26))
      EQK(:VL) = EG(:VL,9)/EG(:VL,3)/EG(:VL,7)/PFAC1(:VL)
      RB(:VL,26) = RF(:VL,26) / MAX(EQK(:VL), SMALL)
C      R27: o2 + co = o + co2
      RF(:VL,27) = 3.04160324D1 -2.4003435D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,27),RF(1,27))
      RB(:VL,27) = 3.25959615D1 -2.71334427D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,27),RB(1,27))
C      R28: h + hco = h2 + co
      RF(:VL,28) = 7.34D13
      RB(:VL,28) = 3.38075119D1 -4.52895001D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,28),RB(1,28))
C      R29: ch2o = h + hco
      RF(:VL,29) = 6.86128153D1 -3.57D0*ALOGT(:VL)
     * -4.68997934D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,29),RF(1,29))
      RB(:VL,29) = 5.62403684D1 -2.57D0*ALOGT(:VL)
     * -2.14873517D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,29),RB(1,29))
      END
C----------------------------------------------------------------------C
      SUBROUTINE RATT2(VL, T, RF, RB, RKLOW, EG, PFAC1, 
     &                   PFAC2, PFAC3, ALOGT)
      USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      PARAMETER (RU=8.31451D7, SMALL=1.D-200, PATM=1.01325D6)
      DIMENSION RF(MAXVL,*), RB(MAXVL,*), RKLOW(MAXVL,*)
      DIMENSION EQK(MAXVL), PFAC1(MAXVL), PFAC2(MAXVL), PFAC3(MAXVL)
      DIMENSION T(MAXVL),ALOGT(MAXVL)
      INTEGER VL
      DIMENSION SMH(MAXVL,26), EG(MAXVL,26)

C      R30: oh + ch2o = h2o + hco
      RF(:VL,30) = 2.19558261D1 +1.18D0*ALOGT(:VL)
     * +2.2493785D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,30),RF(1,30))
      RB(:VL,30) = 2.08938521D1 +1.18D0*ALOGT(:VL)
     * -1.47845057D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,30),RB(1,30))
C      R31: h + ch2o = h2 + hco
      RF(:VL,31) = 2.06543444D1 +1.5D0*ALOGT(:VL)
     * -1.4975728D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,31),RF(1,31))
      RB(:VL,31) = 1.81267123D1 +1.5D0*ALOGT(:VL)
     * -8.88177418D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,31),RB(1,31))
C      R32: o + ch2o = oh + hco
      RF(:VL,32) = 2.67539511D1 +5.7D-1*ALOGT(:VL)
     * -1.38988444D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,32),RF(1,32))
      RB(:VL,32) = 2.34036022D1 +5.7D-1*ALOGT(:VL)
     * -7.71934368D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,32),RB(1,32))
C      R33: oh + ch3 = h2 + ch2o
      RF(:VL,33) = 3.07445364D1 -2.16383167D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,33),RF(1,33))
      RB(:VL,33) = 3.41466223D1 -3.82545311D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,33),RB(1,33))
C      R34: o + ch3 = h + ch2o
      RF(:VL,34) = 8D13
      RB(:VL,34) = 3.45923172D1 -3.50389766D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,34),RB(1,34))
C      R35: o2 + ch3 = o + ch3o
      RF(:VL,35) = 4.21371757D1 -1.57D0*ALOGT(:VL)
     * -1.46989589D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,35),RF(1,35))
      RB(:VL,35) = 4.27232901D1 -1.59D0*ALOGT(:VL)
     * +8.20746385D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,35),RB(1,35))
C      R36: ch3 + ch2o = hco + ch4
      RF(:VL,36) = -1.25246264D1 +5.42D0*ALOGT(:VL)
     * -5.02210234D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,36),RF(1,36))
      RB(:VL,36) = -1.17894698D1 +5.42D0*ALOGT(:VL)
     * -8.12694918D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,36),RB(1,36))
C      R37: hco + ch3 = co + ch4
      RF(:VL,37) = 1.21D14
      RB(:VL,37) = 3.75703583D1 -4.55310441D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,37),RB(1,37))
C      R38: ch3o = h + ch2o
      RF(:VL,38) = 3.16292218D1 -6.79342501D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,38),RF(1,38))
      EQK(:VL) = EG(:VL,1)*EG(:VL,14)/EG(:VL,15)*PFAC1(:VL)
      RB(:VL,38) = RF(:VL,38) / MAX(EQK(:VL), SMALL)
C      R39: c2h4 = h2 + c2h2
      RF(:VL,39) = 3.05213929D1 -3.82444667D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,39),RF(1,39))
      EQK(:VL) = EG(:VL,2)*EG(:VL,20)/EG(:VL,17)*PFAC1(:VL)
      RB(:VL,39) = RF(:VL,39) / MAX(EQK(:VL), SMALL)
C      R40: o + ho2 = o2 + oh
      RF(:VL,40) = 3.25D13
      RB(:VL,40) = 3.42975962D1 -3.3D-1*ALOGT(:VL)
     * -2.78731712D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,40),RB(1,40))
C      R41: hco + ho2 = o2 + ch2o
      RF(:VL,41) = 2.41157588D1 +3.3D-1*ALOGT(:VL)
     * +1.94291955D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,41),RF(1,41))
      RB(:VL,41) = 3.0651446D1 -1.96002892D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,41),RB(1,41))
C      R42: o2 + ch3o = ho2 + ch2o
      RF(:VL,42) = 2.4730599D1 -1.2197972D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,42),RF(1,42))
      RB(:VL,42) = 2.09993813D1 +3.5D-1*ALOGT(:VL)
     * -1.57959712D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,42),RB(1,42))
C      R43: ch3 + ho2 = o2 + ch4
      RF(:VL,43) = 3.6D12
      RB(:VL,43) = 3.61830021D1 -3.3D-1*ALOGT(:VL)
     * -2.91664381D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,43),RB(1,43))
C      R44: o2 + hco = co + ho2
      RF(:VL,44) = 2.96565343D1 -2.06318834D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,44),RF(1,44))
      RB(:VL,44) = 2.75288776D1 +3.3D-1*ALOGT(:VL)
     * -1.65709249D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,44),RB(1,44))
C      R45: h + ho2 = 2oh
      RF(:VL,45) = 3.18908801D1 -1.50965D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,45),RF(1,45))
      RB(:VL,45) = 3.25377763D1 -3.3D-1*ALOGT(:VL)
     * -1.99122835D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,45),RB(1,45))
C      R46: h + ho2 = h2 + o2
      RF(:VL,46) = 3.04404238D1 -4.12637667D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,46),RF(1,46))
      RB(:VL,46) = 3.44486328D1 -3.3D-1*ALOGT(:VL)
     * -2.93375317D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,46),RB(1,46))
C      R47: oh + ho2 = o2 + h2o
      RF(:VL,47) = 3.09948627D1 +2.51608334D2/T(:VL)
      CALL VRDA_EXP(VL,RF(1,47),RF(1,47))
      RB(:VL,47) = 3.64685572D1 -3.3D-1*ALOGT(:VL)
     * -3.63020504D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,47),RB(1,47))
C      R48: o2 + h2o2 = 2ho2
      RF(:VL,48) = 4.09259924D1 -6.6D-1*ALOGT(:VL)
     * -2.67459659D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,48),RF(1,48))
      RB(:VL,48) = 3.36712758D1 -6.02853568D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,48),RB(1,48))
C      R49: 2oh = h2o2
      RF(:VL,49) = 3.24480717D1 -3.7D-1*ALOGT(:VL)
      CALL VRDA_EXP(VL,RF(1,49),RF(1,49))
      EQK(:VL) = EG(:VL,13)/EG(:VL,5)/EG(:VL,5)/PFAC1(:VL)
      RB(:VL,49) = RF(:VL,49) / MAX(EQK(:VL), SMALL)
C      R50: h + h2o2 = oh + h2o
      RF(:VL,50) = 3.0813233D1 -1.99777017D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,50),RF(1,50))
      RB(:VL,50) = 2.9678714D1 -3.75902851D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,50),RB(1,50))
C      R51: ch4 + ho2 = ch3 + h2o2
      RF(:VL,51) = 2.65580766D1 -9.70704952D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,51),RF(1,51))
      RB(:VL,51) = 2.6541864D1 -3.3D-1*ALOGT(:VL)
     * -1.2590481D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,51),RB(1,51))
C      R52: ho2 + ch2o = hco + h2o2
      RF(:VL,52) = -5.14645502D0 +4.53D0*ALOGT(:VL)
     * -3.29959169D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,52),RF(1,52))
      RB(:VL,52) = -4.42786117D0 +4.2D0*ALOGT(:VL)
     * -2.47632922D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,52),RB(1,52))
C      R53: h + c2h2 = c2h3
      RF(:VL,53) = 2.64630587D1 +5.8D-1*ALOGT(:VL)
     * -1.30282795D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,53),RF(1,53))
      EQK(:VL) = EG(:VL,21)/EG(:VL,1)/EG(:VL,20)/PFAC1(:VL)
      RB(:VL,53) = RF(:VL,53) / MAX(EQK(:VL), SMALL)
C      R54: h + c2h4 = h2 + c2h3
      RF(:VL,54) = -4.77714545D0 +4.62D0*ALOGT(:VL)
     * -1.29980865D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,54),RF(1,54))
      RB(:VL,54) = -5.5809195D-1 +3.79D0*ALOGT(:VL)
     * -1.62689949D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,54),RB(1,54))
C      R55: oh + c2h4 = h2o + c2h3
      RF(:VL,55) = 3.0651446D1 -2.99413917D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,55),RF(1,55))
      RB(:VL,55) = 3.63360208D1 -8.3D-1*ALOGT(:VL)
     * -1.09499947D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,55),RB(1,55))
C      R56: c2h2 = h + c2h
      RF(:VL,56) = 3.8276446D1 -5.38441834D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,56),RF(1,56))
      RB(:VL,56) = 1.80824069D1 +2.08D0*ALOGT(:VL)
     * +1.45479939D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,56),RB(1,56))
C      R57: o2 + c2h2 = oh + hcco
      RF(:VL,57) = 1.91138279D1 +1.5D0*ALOGT(:VL)
     * -1.51468217D4/T(:VL)
      CALL VRDA_EXP(VL,RF(1,57),RF(1,57))
      RB(:VL,57) = 1.23158235D1 +1.5D0*ALOGT(:VL)
     * -1.27817034D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,57),RB(1,57))
C      R58: oh + c2h2 = h2o + c2h
      RF(:VL,58) = 1.73330084D1 +2D0*ALOGT(:VL)
     * -7.04503335D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,58),RF(1,58))
      RB(:VL,58) = 8.44912846D0 +3.08D0*ALOGT(:VL)
     * -3.44703417D2/T(:VL)
      CALL VRDA_EXP(VL,RB(1,58),RB(1,58))
C      R59: o + c2h2 = oh + c2h
      RF(:VL,59) = 3.56893484D1 -6D-1*ALOGT(:VL)
     * -7.54825001D3/T(:VL)
      CALL VRDA_EXP(VL,RF(1,59),RF(1,59))
      RB(:VL,59) = 2.45171808D1 +4.8D-1*ALOGT(:VL)
     * +7.83005135D3/T(:VL)
      CALL VRDA_EXP(VL,RB(1,59),RB(1,59))
C      R60: o2 + c2h = co + hco
      RF(:VL,60) = 2.41D12
      RB(:VL,60) = 3.71250355D1 -1.08D0*ALOGT(:VL)
     * -7.75456885D4/T(:VL)
      CALL VRDA_EXP(VL,RB(1,60),RB(1,60))
      END

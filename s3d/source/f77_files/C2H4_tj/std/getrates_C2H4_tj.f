C
C     15-step Reduced Mechanism for C2H4/Air
C
C     August 25, 2005
C
C     Developed by Tianfeng Lu
C     MAE Dept., Princeton Univ.
C     D103 E-Quad, Olden St
C     Princeton, NJ 08544
C     Email: tlu@princeton.edu
C
C     Applicable parameter range: 
C          Equivalence ratio: 0.5-1.7 (for premixed mixtures)
C          Atmospheric or slightly higer pressure
C          Ingition, extinction, and flames
C                                                                      C
C----------------------------------------------------------------------C
C
      SUBROUTINE getrates  (P, T, Y, ICKWRK, RCKWRK, WDOT)
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C
      DIMENSION Y(*), ICKWRK(*), RCKWRK(*), WDOT(*)
      PARAMETER (IREAC=167,KK=19,KSS=10,KTOTAL=29)
      DIMENSION RF(IREAC), RB(IREAC), C(KK), ROP(IREAC)
      DIMENSION XQ(KSS), ABV(KSS), DEN(KSS)
      DIMENSION RKLOW(18)
      DATA SMALL/1.D-50/, RU/8.314510D7/
      DATA RF/IREAC*0.D0/, RB/IREAC*0.D0/
      DATA XQ/KSS*0.D0/
C
C     Convert mass fraction to mole concentration
C
      C(1) = Y(1)*4.96046521D-1
      C(2) = Y(2)*9.92093043D-1
      C(3) = Y(3)*6.25023433D-2
      C(4) = Y(4)*3.12511716D-2
      C(5) = Y(5)*5.87980383D-2
      C(6) = Y(6)*5.55082499D-2
      C(7) = Y(7)*3.02968146D-2
      C(8) = Y(8)*2.93990192D-2
      C(9) = Y(9)*6.65112065D-2
      C(10) = Y(10)*6.23323639D-2
      C(11) = Y(11)*3.57008335D-2
      C(12) = Y(12)*2.27221341D-2
      C(13) = Y(13)*3.33039255D-2
      C(14) = Y(14)*3.84050525D-2
      C(15) = Y(15)*3.56453112D-2
      C(16) = Y(16)*3.32556033D-2
      C(17) = Y(17)*2.37882046D-2
      C(18) = Y(18)*2.37635408D-2
      C(19) = Y(19)*3.56972032D-2
C
      SUM = 0.0
      DO K = 1, 19
         SUM = SUM + C(K)
      ENDDO
      SUM = P/(SUM*T*8.314510D7)
C
      DO K = 1, 19
         C(K) = MAX(C(K), SMALL)
         C(K) = C(K) * SUM
      ENDDO
C
C     Compute T- and P- dependent elementary reaction rates
C
      ALOGT = LOG(T)
      CALL RATT(T, ALOGT, RF, RB, RKLOW)
      CALL RATX(T, ALOGT, C, RF, RB, RKLOW)
C
C     Compute QSS species concetrations
C
C     C
      ABV(1) = RB(73)+RB(98)+RB(99)
      DEN(1) = RB(43)+RF(73)+RF(98)+RF(99)
C     CH
      ABV(2) = RB(6)+RB(102)+RB(103)+RB(105)+RB(108)+RB(109)+ RB(150)
      DEN(2) = RF(6)+RF(43)+RB(45)+RF(74)+RB(76)+RF(100)+RF(101)+
     *         RF(102)+RF(105)+RF(106)+RF(107)+RF(108)+RF(150)
C     CH2
      ABV(3) = RF(18)+RF(25)+RB(44)+RB(75)+RF(78)+RB(93)+RB(103)+
     *         RB(110)+RB(111)+RB(112)+RB(112)+RB(113)+RB(114)+
     *         RB(115)+RB(151)+RB(152)+RB(153)+RB(153)
      DEN(3) = RF(7)+RB(18)+RB(25)+RF(44)+RF(75)+RF(76)+RB(78)+
     *         RF(93)+RB(101)+RF(110)+RF(111)+RF(113)+RF(114)+
     *         RF(115)+RB(117)+RB(121)+RB(124)+RB(125)+
     *         RF(151)+RF(152)
C     CH2
      ABV(4) = RB(8)+RB(77)+RF(79)+RB(118)+RB(119)+RB(120)+
     *         RB(122)+RB(123)+RB(126)+RB(154)
      DEN(4) = RF(8)+RF(9)+RF(45)+RB(54)+RB(63)+RF(77)+
     *         RB(79)+RF(117)+RF(118)+RF(119)+RF(120)+RF(121)+
     *         RF(122)+RF(123)+RF(124)+RF(125)+RF(126)+RF(154)
C     HCO
      ABV(5) = RB(13)+RB(14)+RF(15)+RF(20)+RF(27)+RB(48)+RB(49)+RF(51)+
     *         RB(82)+RF(83)+RF(97)+RB(133)+RF(134)+RB(137)+RB(138)+
     *         RB(139)+RF(166)
      DEN(5) = RB(7)+RB(9)+RF(13)+RF(14)+RB(15)+RB(20)+RB(27)+
     *         RF(48)+RF(49)+RB(51)+RB(74)+RF(82)+RB(83)+RB(97)+
     *         RB(100)+RB(107)+RF(133)+RB(134)+RF(137)+RF(138)+
     *         RF(139)+RB(161)
C     CH3O
      ABV(6) = RB(16)+RF(50)+RB(52)+RB(53)+RB(84)+RF(95)+RF(128)+RB(140)
      DEN(6) = RF(16)+RB(50)+RF(52)+RF(53)+RF(54)+RF(84)+RB(95)+RB(128)+
     *         RF(140)
C     C2H3
      ABV(7) = RB(19)+RF(55)+RB(56)+RB(57)+RF(59)+RB(87)+RF(88)+RF(135)+
     *         RB(156)+RB(167)
      DEN(7) = RF(19)+RB(55)+RF(56)+RF(57)+RB(59)+RF(87)+RB(88)+
     *         RB(135)+RF(141)+RF(155)+RF(156)+RF(167)
C     C2H5
      ABV(8) = RB(21)+RF(22)+RF(58)+RB(60)+RB(61)+RF(62)+RF(89)+RF(132)+
     *         RF(136)+RB(143)+RF(166)
      DEN(8) = RF(21)+RB(22)+RB(58)+RF(60)+RF(61)+RB(62)+RB(89)+
     *         RB(132)+RB(136)+RF(143)
C     HCCO
      ABV(9) = RF(17)+RB(23)+RF(24)+RF(64)+RF(90)+RB(109)+RB(144)+
     *         RB(145)+RB(145)
      DEN(9) = RB(17)+RF(23)+RB(24)+RF(63)+RB(64)+RB(90)+RB(106)+RF(144)
C     CH2CHO
      ABV(10) = RF(147)+RF(157)+RB(159)+RB(162)+RB(163)
      DEN(10) = RB(147)+RB(157)+RF(159)+
     *          RF(162)+RF(163)
C
C     C2H3
      XQ(7) = ABV(7)/DEN(7)
C     C2H5
      XQ(8) = ABV(8)/DEN(8)
C
      A1  =  ABV(1)/DEN(1)
      A12 =  RF(43)/DEN(1)
      A2  =  ABV(2)/DEN(2)
      A21 =  RB(43)/DEN(2)
      A23 =  (RF(76)+RB(101))/DEN(2)
     A24 =  RF(45)/DEN(2)
     A25 =  (RB(74)+RB(100)+RB(107))/DEN(2)
      A29 =  RB(106)/DEN(2)
      A3  =  ABV(3)/DEN(3)
      A32 =  (RB(76)+RF(101))/DEN(3)
      A34 =  (RF(117)+RF(121)+RF(124)+RF(125))/DEN(3)
     A35 =  RB(7)/DEN(3)
      A4  =  ABV(4)/DEN(4)
      A42 =  RB(45)/DEN(4)
      A43 =  (RB(117)+RB(121)+RB(124)+RB(125))/DEN(4)
     A45 =  RB(9)/DEN(4)
      A46 =  RF(54)/DEN(4)
      A49 =  RF(63)/DEN(4)
     A5  =  (ABV(5)+RF(141)*XQ(7))/DEN(5)
     A52 =  (RF(74)+RF(100)+RF(107))/DEN(5)
     A53 =  RF(7)/DEN(5)
     A54 =  RF(9)/DEN(5)
      A6  =  ABV(6)/DEN(6)
      A64 =  RB(54)/DEN(6)
      A9  =  ABV(9)/DEN(9)
      A92 =  RF(106)/DEN(9)
      A94 =  RB(63)/DEN(9)
C
      B10 =  A2+A21*A1+A5*A25+A9*A29
      B12 =  A21*A12+A25*A52+A29*A92-1
      B13 =  A23+A25*A53
      B14 =  A24+A25*A54+A29*A94
      B20 =  A3+A35*A5
      B22 =  A32+A35*A52
      B23 =  A35*A53-1
      B24 =  A34+A35*A54
      B30 =  A4+A45*A5+A46*A6+A49*A9
      B32 =  A42+A45*A52+A49*A92
      B33 =  A43+A45*A53
      B34 =  A45*A54+A46*A64+A49*A94-1
      C12 =  B34*B12-B14*B32
      C13 =  B34*B13-B14*B33
      C10 =  B34*B10-B14*B30
      C22 =  B34*B22-B24*B32
      C23 =  B34*B23-B24*B33
      C20 =  B34*B20-B24*B30
C
C     CH
      XQ(2) = ABS(C13*C20-C23*C10)/MAX(ABS(C23*C12-C13*C22), SMALL)
C     CH2      
      XQ(3) = ABS(C22*XQ(2) + C20)/MAX(ABS(C23), SMALL)
C     CH2*      
      XQ(4) = ABS(B32*XQ(2)+B33*XQ(3)+B30)/MAX(ABS(B34), SMALL)
C     C      
      XQ(1) = A1 + A12*XQ(2)
C     HCO
     XQ(5) = A5 + A52*XQ(2) + A53*XQ(3) + A54*XQ(4)
C     CH3O      
      XQ(6) = A6 + A64*XQ(4)
C     HCCO      
      XQ(9) = A9 + A92*XQ(2) + A94*XQ(4)
C     CH2CHO
      XQ(10) = (ABV(10) + RB(161)*XQ(5) + RF(155)*XQ(7))/DEN(10)
C
C     Multiply QSS species concetratios to involved reactions
C
      RF(6) = RF(6) * XQ(2)
      RF(7) = RF(7) * XQ(3)
      RB(7) = RB(7) * XQ(5)
      RF(8) = RF(8) * XQ(4)
      RF(9) = RF(9) * XQ(4)
      RB(9) = RB(9) * XQ(5)
      RF(13) = RF(13) * XQ(5)
      RF(14) = RF(14) * XQ(5)
      RB(15) = RB(15) * XQ(5)
      RF(16) = RF(16) * XQ(6)
      RB(17) = RB(17) * XQ(9)
      RB(18) = RB(18) * XQ(3)
      RF(19) = RF(19) * XQ(7)
      RB(20) = RB(20) * XQ(5)
      RF(21) = RF(21) * XQ(8)
      RB(22) = RB(22) * XQ(8)
      RF(23) = RF(23) * XQ(9)
      RB(24) = RB(24) * XQ(9)
      RB(25) = RB(25) * XQ(3)
      RB(27) = RB(27) * XQ(5)
      RF(43) = RF(43) * XQ(2)
      RB(43) = RB(43) * XQ(1)
      RF(44) = RF(44) * XQ(3)
      RF(45) = RF(45) * XQ(4)
      RB(45) = RB(45) * XQ(2)
      RF(48) = RF(48) * XQ(5)
      RF(49) = RF(49) * XQ(5)
      RB(50) = RB(50) * XQ(6)
      RB(51) = RB(51) * XQ(5)
      RF(52) = RF(52) * XQ(6)
      RF(53) = RF(53) * XQ(6)
      RF(54) = RF(54) * XQ(6)
      RB(54) = RB(54) * XQ(4)
      RB(55) = RB(55) * XQ(7)
      RF(56) = RF(56) * XQ(7)
      RF(57) = RF(57) * XQ(7)
      RB(58) = RB(58) * XQ(8)
      RB(59) = RB(59) * XQ(7)
      RF(60) = RF(60) * XQ(8)
      RF(61) = RF(61) * XQ(8)
      RB(62) = RB(62) * XQ(8)
      RF(63) = RF(63) * XQ(9)
      RB(63) = RB(63) * XQ(4)
      RB(64) = RB(64) * XQ(9)
      RF(73) = RF(73) * XQ(1)
      RF(74) = RF(74) * XQ(2)
      RB(74) = RB(74) * XQ(5)
      RF(75) = RF(75) * XQ(3)
      RF(76) = RF(76) * XQ(3)
      RB(76) = RB(76) * XQ(2)
      RF(77) = RF(77) * XQ(4)
      RB(78) = RB(78) * XQ(3)
      RB(79) = RB(79) * XQ(4)
      RF(82) = RF(82) * XQ(5)
      RB(83) = RB(83) * XQ(5)
      RF(84) = RF(84) * XQ(6)
      RF(87) = RF(87) * XQ(7)
      RB(88) = RB(88) * XQ(7)
      RB(89) = RB(89) * XQ(8)
      RB(90) = RB(90) * XQ(9)
      RF(93) = RF(93) * XQ(3)
      RB(95) = RB(95) * XQ(6)
      RB(97) = RB(97) * XQ(5)
      RF(98) = RF(98) * XQ(1)
      RF(99) = RF(99) * XQ(1)
      RF(100) = RF(100) * XQ(2)
      RB(100) = RB(100) * XQ(5)
      RF(101) = RF(101) * XQ(2)
      RB(101) = RB(101) * XQ(3)
      RF(102) = RF(102) * XQ(2)
      RF(105) = RF(105) * XQ(2)
      RF(106) = RF(106) * XQ(2)
      RB(106) = RB(106) * XQ(9)
      RF(107) = RF(107) * XQ(2)
      RB(107) = RB(107) * XQ(5)
      RF(108) = RF(108) * XQ(2)
      RF(110) = RF(110) * XQ(3)
      RF(111) = RF(111) * XQ(3)
      RF(113) = RF(113) * XQ(3)
      RF(114) = RF(114) * XQ(3)
      RF(115) = RF(115) * XQ(3)
      RF(117) = RF(117) * XQ(4)
      RB(117) = RB(117) * XQ(3)
      RF(118) = RF(118) * XQ(4)
      RF(119) = RF(119) * XQ(4)
      RF(120) = RF(120) * XQ(4)
      RF(121) = RF(121) * XQ(4)
      RB(121) = RB(121) * XQ(3)
      RF(122) = RF(122) * XQ(4)
      RF(123) = RF(123) * XQ(4)
      RF(124) = RF(124) * XQ(4)
      RB(124) = RB(124) * XQ(3)
      RF(125) = RF(125) * XQ(4)
      RB(125) = RB(125) * XQ(3)
      RF(126) = RF(126) * XQ(4)
      RB(128) = RB(128) * XQ(6)
      RB(132) = RB(132) * XQ(8)
      RF(133) = RF(133) * XQ(5)
      RB(134) = RB(134) * XQ(5)
      RB(135) = RB(135) * XQ(7)
      RB(136) = RB(136) * XQ(8)
      RF(137) = RF(137) * XQ(5)
      RF(138) = RF(138) * XQ(5)
      RF(139) = RF(139) * XQ(5)
      RF(140) = RF(140) * XQ(6)
      RF(141) = RF(141) * XQ(7)
      RF(143) = RF(143) * XQ(8)
      RF(144) = RF(144) * XQ(9)
      RB(147) = RB(147) * XQ(10)
      RF(150) = RF(150) * XQ(2)
      RF(151) = RF(151) * XQ(3)
      RF(152) = RF(152) * XQ(3)
      RF(154) = RF(154) * XQ(4)
      RF(155) = RF(155) * XQ(7)
      RF(156) = RF(156) * XQ(7)
      RB(157) = RB(157) * XQ(10)
      RF(159) = RF(159) * XQ(10)
      RB(161) = RB(161) * XQ(5)
      RF(162) = RF(162) * XQ(10)
      RF(163) = RF(163) * XQ(10)
      RF(167) = RF(167) * XQ(7)
C
      DO I = 1, IREAC
         ROP(I) = RF(I) - RB(I)
      ENDDO
C
      ROP(28) = ROP(28) + ROP(29)
      ROP(28) = ROP(28) + ROP(30)
      ROP(28) = ROP(28) + ROP(31)
      ROP(33) = ROP(33) + ROP(34)
      ROP(33) = ROP(33) + ROP(35)
      ROP(33) = ROP(33) + ROP(36)
      ROP(70) = ROP(70) + ROP(148)
      ROP(71) = ROP(71) + ROP(72)
      ROP(91) = ROP(91) + ROP(92)
      ROP(117) = ROP(117) + ROP(121)
      ROP(117) = ROP(117) + ROP(124)
      ROP(117) = ROP(117) + ROP(125)
      ROP(137) = ROP(137) + ROP(138)
C
      WDOT(1) = -ROP(3) +ROP(8) +ROP(33) +ROP(39) 
     *          +ROP(41) +ROP(43) +ROP(45) +ROP(47) 
     *          +ROP(49) +ROP(51) +ROP(52) +ROP(57) 
     *          +ROP(59) +ROP(61) +ROP(62) +ROP(64) 
     *          -ROP(66) -ROP(67) -ROP(101) -ROP(111) 
     *          +ROP(112) -ROP(120) +ROP(142) +ROP(146) 
     *          +ROP(149) -ROP(150) +ROP(154) +ROP(162) 
      WDOT(2) = -ROP(2) +ROP(3) +ROP(6) +ROP(7) 
     *          +ROP(9) +ROP(10) +ROP(14) +ROP(17) 
     *          +ROP(19) +ROP(23) -ROP(28) -ROP(32) 
     *          -ROP(33) -ROP(33) -ROP(37) -ROP(38) 
     *          -ROP(39) -ROP(40) -ROP(41) -ROP(42) 
     *          -ROP(43) -ROP(44) -ROP(45) -ROP(46) 
     *          -ROP(47) -ROP(48) -ROP(49) -ROP(50) 
     *          -ROP(51) -ROP(52) -ROP(53) -ROP(54) 
     *          -ROP(55) -ROP(56) -ROP(57) -ROP(58) 
     *          -ROP(59) -ROP(60) -ROP(61) -ROP(62) 
     *          -ROP(63) -ROP(64) -ROP(65) +ROP(67) 
     *          +ROP(73) +ROP(74) +ROP(75) +ROP(77) 
     *          +ROP(81) +ROP(85) +ROP(99) +ROP(101) 
     *          +ROP(102) +ROP(103) +ROP(105) 
     *          +ROP(108) +ROP(110) +ROP(111) +ROP(113) 
     *          +ROP(118) +ROP(120) +ROP(122) +ROP(132) 
     *          +ROP(137) +ROP(146) +ROP(147) +ROP(151) +ROP(151) 
     *          +ROP(153) +ROP(153) -ROP(157)  
     *          -ROP(161) -ROP(162) -ROP(164) +ROP(165) 
      WDOT(3) = -ROP(1) -ROP(1) -ROP(2) -ROP(3) 
     *          -ROP(4) -ROP(5) -ROP(6) -ROP(7) 
     *          -ROP(8) -ROP(9) -ROP(10) -ROP(11) 
     *          -ROP(12) -ROP(13) -ROP(14) -ROP(15) 
     *          -ROP(16) -ROP(17) -ROP(18) -ROP(19) 
     *          -ROP(20) -ROP(21) -ROP(22) -ROP(23) 
     *          -ROP(24) -ROP(25) +ROP(26) +ROP(32) 
     *          +ROP(38) +ROP(69) +ROP(98) +ROP(100) 
     *          +ROP(128) -ROP(146) -ROP(147) +ROP(152) 
     *          +ROP(155) -ROP(165) -ROP(166) 
      WDOT(4) = +ROP(1) +ROP(4) -ROP(26) -ROP(27) 
     *          -ROP(28) -ROP(32) +ROP(39) +ROP(70) 
     *          +ROP(91) +ROP(94) -ROP(98) -ROP(100) 
     *          -ROP(110) -ROP(118) -ROP(119) -ROP(128) 
     *          -ROP(129) -ROP(139) -ROP(140) -ROP(141) 
     *          -ROP(143) -ROP(144) -ROP(151) -ROP(152) 
     *          -ROP(155) -ROP(156) -ROP(159)  
      WDOT(5) = +ROP(2) +ROP(3) +ROP(4) +ROP(5) 
     *          +ROP(11) +ROP(13) +ROP(15) +ROP(16) 
     *          +ROP(22) +ROP(24) +ROP(32) -ROP(37) 
     *          +ROP(40) +ROP(40) +ROP(42) +ROP(53) 
     *          -ROP(67) -ROP(68) -ROP(68) -ROP(69) -ROP(69) 
     *          -ROP(70) -ROP(71) -ROP(73) -ROP(74) 
     *          -ROP(75) -ROP(76) -ROP(77) -ROP(78) 
     *          -ROP(79) -ROP(80) -ROP(81) -ROP(82) 
     *          -ROP(83) -ROP(84) -ROP(85) -ROP(86) 
     *          -ROP(87) -ROP(88) -ROP(89) -ROP(90) 
     *          +ROP(93) +ROP(95) +ROP(96) +ROP(110) 
     *          +ROP(118) +ROP(129) +ROP(144) -ROP(149) 
     *          +ROP(159) -ROP(163) 
      WDOT(6) = +ROP(37) +ROP(38) +ROP(42) +ROP(54) 
     *          +ROP(67) +ROP(69) +ROP(70) +ROP(71) 
     *          +ROP(76) +ROP(78) +ROP(79) +ROP(80) 
     *          +ROP(82) +ROP(83) +ROP(84) +ROP(87) 
     *          +ROP(88) +ROP(89) +ROP(90) -ROP(102) 
     *          +ROP(119) -ROP(154) +ROP(163) 
      WDOT(7) = -ROP(4) +ROP(5) +ROP(27) +ROP(28) 
     *          -ROP(38) -ROP(39) -ROP(40) +ROP(41) 
     *          -ROP(70) +ROP(71) -ROP(91) -ROP(91) 
     *          -ROP(93) -ROP(94) -ROP(95) -ROP(96) 
     *          -ROP(97) +ROP(130) +ROP(139) +ROP(140) 
     *          +ROP(143) +ROP(156) 
      WDOT(8) = -ROP(5) -ROP(41) -ROP(42) +ROP(68) 
     *          -ROP(71) +ROP(91) +ROP(97) -ROP(130) 
      WDOT(9) = -ROP(10) +ROP(11) +ROP(20) +ROP(21) 
     *          +ROP(44) -ROP(46) +ROP(47) +ROP(53) 
     *          +ROP(65) -ROP(78) -ROP(79) +ROP(80) 
     *          +ROP(86) -ROP(94) -ROP(95) -ROP(99) 
     *          +ROP(111) -ROP(113) +ROP(114) +ROP(114) 
     *          +ROP(120) -ROP(122) +ROP(123) +ROP(123) 
     *          -ROP(128) -ROP(129) -ROP(130) 
     *          -ROP(131) -ROP(131) -ROP(132) -ROP(132) 
     *          -ROP(133) -ROP(134) -ROP(135) -ROP(136) 
     *          -ROP(146) -ROP(149) +ROP(150) +ROP(161) 
     *          +ROP(164) +ROP(165) -ROP(167) 
      WDOT(10) = -ROP(11) +ROP(46) -ROP(47) -ROP(80) 
     *          +ROP(94) -ROP(105) -ROP(114) -ROP(123) 
     *          +ROP(130) +ROP(133) +ROP(134) +ROP(135) 
     *          +ROP(136) 
      WDOT(11) = +ROP(6) +ROP(8) -ROP(12) +ROP(13) 
     *          +ROP(18) +ROP(23) +ROP(23) -ROP(26) 
     *          +ROP(49) +ROP(63) +ROP(65) -ROP(66) 
     *          +ROP(73) -ROP(81) +ROP(82) +ROP(86) 
     *          -ROP(96) +ROP(98) -ROP(106) +ROP(107) 
     *          +ROP(109) +ROP(110) -ROP(115)  
     *          +ROP(118) +ROP(119) +ROP(126) +ROP(133) 
     *          +ROP(137) +ROP(139) +ROP(144) +ROP(144) 
     *          +ROP(145) +ROP(145) +ROP(146) +ROP(159) 
      WDOT(12) = +ROP(12) +ROP(14) +ROP(25) +ROP(26) 
     *          +ROP(81) +ROP(96) -ROP(107) -ROP(126) 
     *          +ROP(151) 
      WDOT(13) = +ROP(10) -ROP(15) +ROP(16) +ROP(21) 
     *          -ROP(27) +ROP(48) -ROP(50) -ROP(51) 
     *          +ROP(52) +ROP(66) +ROP(75) +ROP(77) 
     *          -ROP(83) +ROP(84) +ROP(93) -ROP(97) 
     *          +ROP(102) -ROP(108) +ROP(126) +ROP(129) 
     *          -ROP(134) +ROP(140) +ROP(141) +ROP(149) 
     *          +ROP(152) +ROP(154) +ROP(159) 
      WDOT(14) = -ROP(17) -ROP(18) -ROP(55) +ROP(57) 
     *          -ROP(85) -ROP(86) +ROP(87) +ROP(99) 
     *          +ROP(103) +ROP(109) +ROP(112) +ROP(142) 
     *          +ROP(145) +ROP(153) +ROP(156) 
      WDOT(15) = -ROP(20) +ROP(56) -ROP(58) -ROP(59) 
     *          +ROP(61) -ROP(88) +ROP(105) +ROP(113) 
     *          +ROP(122) -ROP(135) -ROP(142) +ROP(143) 
     *          -ROP(147) +ROP(164) 
      WDOT(16) = -ROP(22) +ROP(60) -ROP(62) -ROP(89) 
     *           +ROP(131) -ROP(136) 
      WDOT(17) = +ROP(19) -ROP(24) -ROP(25) -ROP(64) 
     *          -ROP(65) +ROP(85) -ROP(90) +ROP(108) 
     *          +ROP(115) -ROP(157) +ROP(162) +ROP(163) 
     *          +ROP(165) 
      WDOT(18) = -ROP(164) -ROP(165) -ROP(166) +ROP(167) 
      WDOT(19) = 0.0
C
      RETURN
      END
C
C----------------------------------------------------------------------C
C
      SUBROUTINE RATT (T, ALOGT, RF, RB, RKLOW)
C
C*****precision > double
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C
      PARAMETER (RU=8.314510D7, RUC=RU/4.184D7, PATM=1.01325D6)
      DIMENSION RF(*), RB(*), EQK(167), SMH(29)
      DIMENSION EG(29)
      DIMENSION RKLOW(*)
      DATA SMALL/1.D-50/
C
      TI = 1.0D0/T
      TI2 = TI*TI
C
      RF(1) = 1.2D17*TI
      RF(2) = 5.D17*TI
      RF(3) = EXP(1.05635949D1 +2.7D0*ALOGT -3.15013634D3*TI)
      RF(4) = 2.D13
      RF(5) = EXP(1.60803938D1 +2.D0*ALOGT -2.01286667D3*TI)
      RF(6) = 5.7D13
      RF(7) = 8.D13
      RF(8) = 1.5D13
      RF(9) = 1.5D13
      RF(10) = 5.06D13
      RF(11) = EXP(2.07430685D1 +1.5D0*ALOGT -4.32766334D3*TI)
      RF(12) = EXP(2.36136376D1 -1.20017175D3*TI)
      RF(13) = 3.D13
      RF(14) = 3.D13
      RF(15) = EXP(3.12945828D1 -1.781387D3*TI)
      RF(16) = 1.D13
      TMP = EXP(2.D0*ALOGT -9.56111669D2*TI )
      RF(17) = 1.35D7 * TMP
      RF(18) = 6.94D6 * TMP
      RF(19) = 3.D13
      TMP = EXP(1.83D0*ALOGT -1.10707667D2*TI )
      RF(20) = 2.5D7 * TMP
      RF(147) = 3.35D6 * TMP
      RF(21) = 2.24D13
      RF(22) = EXP(1.83130955D1 +1.92D0*ALOGT -2.86330284D3*TI)
      RF(23) = 1.D14
      TMP = EXP(-4.02573334D3*TI)
      RF(24) = 1.D13 * TMP
      RF(64) = 5.D13 * TMP
      RF(25) = EXP(2.81906369D1 -6.79342501D2*TI)
      RF(26) = EXP(2.85473118D1 -2.40537567D4*TI)
      RF(27) = EXP(3.22361913D1 -2.01286667D4*TI)
      RF(28) = EXP(4.24761511D1 -8.6D-1*ALOGT)
      TMP = EXP(-1.24D0*ALOGT)
      RF(29) = 2.08D19 * TMP
      RF(31) = 2.6D19 * TMP
      RF(30) = EXP(4.38677883D1 -7.6D-1*ALOGT)
      RF(32) = EXP(3.78159211D1 -6.707D-1*ALOGT -8.57531523D3*TI)
      RF(33) = 1.D18*TI
      RF(34) = EXP(3.90385861D1 -6.D-1*ALOGT)
      RF(35) = EXP(4.55408762D1 -1.25D0*ALOGT)
      RF(36) = 5.5D20*TI2
      RF(37) = 2.2D22*TI2
      RF(38) = EXP(2.90097872D1 -3.37658384D2*TI)
      RF(39) = EXP(3.19812991D1 -5.37435401D2*TI)
      RF(40) = EXP(3.13686907D1 -3.19542584D2*TI)
      RF(41) = EXP(1.6308716D1 +2.D0*ALOGT -2.61672667D3*TI)
      RF(42) = EXP(2.99336062D1 -1.81158D3*TI)
      RF(43) = 1.65D14
      RF(44) = 6.D14
      RF(45) = 3.D13
      RF(46) = EXP(3.71706652D1 -5.34D-1*ALOGT -2.69724134D2*TI)
      RF(47) = EXP(2.03077504D1 +1.62D0*ALOGT -5.45486868D3*TI)
      RF(48) = EXP(2.77171988D1 +4.8D-1*ALOGT +1.30836334D2*TI)
      RF(49) = 7.34D13
      RF(50) = EXP(2.7014835D1 +4.54D-1*ALOGT -1.30836334D3*TI)
      RF(51) = EXP(1.78655549D1 +1.9D0*ALOGT -1.3798201D3*TI)
      RF(52) = 2.D13
      RF(53) = EXP(2.80364862D1 +5.D-1*ALOGT +5.53538334D1*TI)
      RF(54) = EXP(3.31993656D1 -2.3D-1*ALOGT -5.38441834D2*TI)
      RF(55) = EXP(2.93537877D1 -1.20772D3*TI)
      RF(56) = EXP(2.94360258D1 +2.7D-1*ALOGT -1.40900667D2*TI)
      RF(57) = 3.03D13
      RF(58) = EXP(2.73863985D1 +4.54D-1*ALOGT -9.15854335D2*TI)
      RF(59) = EXP(1.4096923D1 +2.53D0*ALOGT -6.15937201D3*TI)
      RF(60) = EXP(4.07945264D1 -9.9D-1*ALOGT -7.95082335D2*TI)
      RF(61) = 2.D12
      RF(62) = EXP(1.85604427D1 +1.9D0*ALOGT -3.78922151D3*TI)
      RF(63) = 1.D14
      RF(65) = EXP(3.00558238D1 -1.72502674D3*TI)
      RF(66) = EXP(1.75767107D1 +1.5D0*ALOGT -4.00560467D4*TI)
      RF(67) = EXP(1.9190789D1 +1.51D0*ALOGT -1.72603317D3*TI)
      RF(68) = EXP(3.19350862D1 -3.7D-1*ALOGT)
      RF(69) = EXP(1.0482906D1 +2.4D0*ALOGT +1.06178717D3*TI)
      RF(70) = EXP(3.03051698D1 +2.51608334D2*TI)
      RF(71) = EXP(2.83241683D1 -2.14873517D2*TI)
      RF(72) = EXP(4.19771599D1 -1.47996022D4*TI)
      RF(73) = 5.D13
      RF(74) = 3.D13
      RF(75) = 2.D13
      RF(76) = EXP(1.62403133D1 +2.D0*ALOGT -1.50965D3*TI)
      RF(77) = 3.D13
      RF(78) = EXP(1.78408622D1 +1.6D0*ALOGT -2.72743434D3*TI)
      RF(79) = EXP(4.10064751D1 -1.34D0*ALOGT -7.13058018D2*TI)
      RF(80) = EXP(1.84206807D1 +1.6D0*ALOGT -1.570036D3*TI)
      RF(81) = EXP(1.76783433D1 +1.228D0*ALOGT -3.52251667D1*TI)
      RF(82) = 5.D13
      RF(83) = EXP(2.19558261D1 +1.18D0*ALOGT +2.2493785D2*TI)
      RF(84) = 5.D12
      RF(85) = EXP(-8.4310155D0 +4.5D0*ALOGT +5.03216668D2*TI)
      RF(86) = EXP(-7.6354939D0 +4.D0*ALOGT +1.00643334D3*TI)
      RF(87) = 5.D12
      RF(88) = EXP(1.44032972D1 +2.D0*ALOGT -1.25804167D3*TI)
      RF(89) = EXP(1.50796373D1 +2.12D0*ALOGT -4.37798501D2*TI)
      RF(90) = EXP(2.96459241D1 -1.00643334D3*TI)
      RF(91) = EXP(2.55908003D1 +8.20243168D2*TI)
      RF(92) = EXP(3.36712758D1 -6.03860001D3*TI)
      RF(93) = 2.D13
      RF(94) = 1.D12
      RF(95) = 2.87D13
      RF(96) = EXP(3.26416564D1 -1.18759134D4*TI)
      RF(97) = EXP(1.55382772D1 +2.D0*ALOGT -6.03860001D3*TI)
      RF(98) = EXP(3.16914641D1 -2.89852801D2*TI)
      RF(99) = 5.D13
      RF(100) = 6.71D13
      RF(101) = EXP(3.23131523D1 -1.56500384D3*TI)
      RF(102) = EXP(2.93732401D1 +3.79928584D2*TI)
      RF(103) = 4.D13
      RF(105) = 6.D13
      RF(106) = 5.D13
      RF(107) = EXP(3.28780452D1 -7.94679762D3*TI)
      RF(108) = EXP(3.21806786D1 +2.59156584D2*TI)
      RF(109) = 5.D13
      TMP = EXP(-7.54825001D2*TI)
      RF(110) = 5.D12 * TMP
      RF(151) = 5.8D12 * TMP
      RF(152) = 2.4D12 * TMP
      RF(111) = EXP(1.31223634D1 +2.D0*ALOGT -3.63825651D3*TI)
      RF(112) = EXP(3.500878D1 -6.01041988D3*TI)
      RF(113) = 4.D13
      RF(114) = EXP(1.47156719D1 +2.D0*ALOGT -4.16160184D3*TI)
      RF(115) = EXP(2.74203001D1 +5.D-1*ALOGT -2.26950717D3*TI)
      RF(117) = EXP(3.03390713D1 -3.01930001D2*TI)
      RF(118) = 2.8D13
      RF(119) = 1.2D13
      RF(120) = 7.D13
      RF(121) = 3.D13
      TMP = EXP(2.86833501D2*TI)
      RF(122) = 1.2D13 * TMP
      RF(123) = 1.6D13 * TMP
      RF(124) = 9.D12
      RF(125) = 7.D12
      RF(126) = 1.4D13
      RF(128) = EXP(3.12033668D1 -1.5338044D4*TI)
      RF(129) = EXP(2.84682686D1 -1.02228466D4*TI)
      RF(130) = EXP(1.01064284D1 +2.47D0*ALOGT -2.60666234D3*TI)
      RF(131) = EXP(3.87538626D1 -1.18D0*ALOGT -3.29103701D2*TI)
      RF(132) = EXP(2.95538088D1 +1.D-1*ALOGT -5.33409668D3*TI)
      RF(133) = 2.648D13
      RF(134) = EXP(8.10772006D0 +2.81D0*ALOGT -2.94884967D3*TI)
      RF(135) = EXP(1.23327053D1 +2.D0*ALOGT -4.62959334D3*TI)
      RF(136) = EXP(1.56303353D1 +1.74D0*ALOGT -5.25861418D3*TI)
      TMP = EXP(-1.D0*ALOGT -8.55468335D3*TI )
      RF(137) = 1.5D18 * TMP
      RF(138) = 1.87D17 * TMP
      RF(139) = EXP(3.02300002D1 -2.01286667D2*TI)
      RF(140) = EXP(-2.84796532D1 +7.6D0*ALOGT +1.77635484D3*TI)
      RF(141) = EXP(3.83630605D1 -1.39D0*ALOGT -5.10764918D2*TI)
      RF(142) = EXP(2.97104627D1 +4.4D-1*ALOGT -4.36641103D4*TI)
      RF(143) = EXP(2.74566677D1 -1.94996459D3*TI)
      RF(144) = EXP(2.87941719D1 -4.29747034D2*TI)
      RF(145) = 1.D13
      RF(146) = 3.37D13
      RF(148) = EXP(3.61482143D1 -8.72074485D3*TI)
      RF(149) = EXP(2.28027074D1 +5.D-1*ALOGT +8.83145252D2*TI)
      RF(150) = EXP(2.83090547D1 +4.3D-1*ALOGT +1.86190167D2*TI)
      RF(154) = EXP(2.49457104D1 +2.5D-1*ALOGT +4.70507584D2*TI)
      RF(155) = EXP(2.55207079D1 +2.9D-1*ALOGT -5.53538334D0*TI)
      RF(156) = EXP(1.41059389D1 +1.61D0*ALOGT +1.932352D2*TI)
      RF(157) = EXP(2.69105027D1 +4.22D-1*ALOGT +8.83145252D2*TI)
      RF(159) = 1.81D10
      RF(161) = 2.2D13
      RF(162) = 1.1D13
      RF(163) = 1.2D13
      RF(164) = EXP(5.16031099D1 -2.39D0*ALOGT -5.62596234D3*TI)
      RF(165) = EXP(1.86030023D1 +1.65D0*ALOGT -1.6455185D2*TI)
      RF(166) = EXP(1.73708586D1 +1.65D0*ALOGT +4.89126601D2*TI)
      RF(167) = 2.5D13
C
      CALL RDSMH (T, ALOGT, SMH)
      DO N = 1, 28
          EG(N) = EXP(SMH(N))
      ENDDO
C
      PFAC = PATM / (RU*T)
      PFAC2 = PFAC*PFAC
      PFAC3 = PFAC2*PFAC
C
      EQK(1)=EG(4)/EG(3)/EG(3)/PFAC
      EQK(2)=EG(5)/EG(2)/EG(3)/PFAC
      EQK(3)=EG(2)*EG(5)/EG(1)/EG(3)
      EQK(4)=EG(4)*EG(5)/EG(3)/EG(7)
      EQK(5)=EG(5)*EG(7)/EG(3)/EG(8)
      EQK(6)=EG(2)*EG(15)/EG(3)/EG(10)
      EQK(7)=EG(2)*EG(17)/EG(3)/EG(11)
      EQK(8)=EG(1)*EG(15)/EG(3)/EG(12)
      EQK(9)=EG(2)*EG(17)/EG(3)/EG(12)
      EQK(10)=EG(2)*EG(18)/EG(3)/EG(13)
      EQK(11)=EG(5)*EG(13)/EG(3)/EG(14)
      EQK(12)=EG(16)/EG(3)/EG(15)/PFAC
      EQK(13)=EG(5)*EG(15)/EG(3)/EG(17)
      EQK(14)=EG(2)*EG(16)/EG(3)/EG(17)
      EQK(15)=EG(5)*EG(17)/EG(3)/EG(18)
      EQK(16)=EG(5)*EG(18)/EG(3)/EG(19)
      EQK(17)=EG(2)*EG(25)/EG(3)/EG(20)
      EQK(18)=EG(11)*EG(15)/EG(3)/EG(20)
      EQK(19)=EG(2)*EG(26)/EG(3)/EG(21)
      EQK(20)=EG(13)*EG(17)/EG(3)/EG(22)
      EQK(21)=EG(13)*EG(18)/EG(3)/EG(23)
      EQK(22)=EG(5)*EG(23)/EG(3)/EG(24)
      EQK(23)=EG(2)*EG(15)*EG(15)/EG(3)/EG(25)*PFAC
      EQK(24)=EG(5)*EG(25)/EG(3)/EG(26)
      EQK(25)=EG(11)*EG(16)/EG(3)/EG(26)
      EQK(26)=EG(3)*EG(16)/EG(4)/EG(15)
      EQK(27)=EG(7)*EG(17)/EG(4)/EG(18)
      EQK(28)=EG(7)/EG(2)/EG(4)/PFAC
      EQK(29)=EQK(28)
      EQK(30)=EQK(28)
      EQK(31)=EQK(28)
      EQK(32)=EG(3)*EG(5)/EG(2)/EG(4)
      EQK(33)=EG(1)/EG(2)/EG(2)/PFAC
      EQK(34)=EQK(33)
      EQK(35)=EQK(33)
      EQK(36)=EQK(33)
      EQK(37)=EG(6)/EG(2)/EG(5)/PFAC
      EQK(38)=EG(3)*EG(6)/EG(2)/EG(7)
      EQK(39)=EG(1)*EG(4)/EG(2)/EG(7)
      EQK(40)=EG(5)*EG(5)/EG(2)/EG(7)
      EQK(41)=EG(1)*EG(7)/EG(2)/EG(8)
      EQK(42)=EG(5)*EG(6)/EG(2)/EG(8)
      EQK(43)=EG(1)*EG(9)/EG(2)/EG(10)
      EQK(44)=EG(13)/EG(2)/EG(11)/PFAC
      EQK(45)=EG(1)*EG(10)/EG(2)/EG(12)
      EQK(46)=EG(14)/EG(2)/EG(13)/PFAC
      EQK(47)=EG(1)*EG(13)/EG(2)/EG(14)
      EQK(48)=EG(18)/EG(2)/EG(17)/PFAC
      EQK(49)=EG(1)*EG(15)/EG(2)/EG(17)
      EQK(50)=EG(19)/EG(2)/EG(18)/PFAC
      EQK(51)=EG(1)*EG(17)/EG(2)/EG(18)
      EQK(52)=EG(1)*EG(18)/EG(2)/EG(19)
      EQK(53)=EG(5)*EG(13)/EG(2)/EG(19)
      EQK(54)=EG(6)*EG(12)/EG(2)/EG(19)
      EQK(55)=EG(21)/EG(2)/EG(20)/PFAC
      EQK(56)=EG(22)/EG(2)/EG(21)/PFAC
      EQK(57)=EG(1)*EG(20)/EG(2)/EG(21)
      EQK(58)=EG(23)/EG(2)/EG(22)/PFAC
      EQK(59)=EG(1)*EG(21)/EG(2)/EG(22)
      EQK(60)=EG(24)/EG(2)/EG(23)/PFAC
      EQK(61)=EG(1)*EG(22)/EG(2)/EG(23)
      EQK(62)=EG(1)*EG(23)/EG(2)/EG(24)
      EQK(63)=EG(12)*EG(15)/EG(2)/EG(25)
      EQK(64)=EG(1)*EG(25)/EG(2)/EG(26)
      EQK(65)=EG(13)*EG(15)/EG(2)/EG(26)
      EQK(66)=EG(18)/EG(1)/EG(15)/PFAC
      EQK(67)=EG(2)*EG(6)/EG(1)/EG(5)
      EQK(68)=EG(8)/EG(5)/EG(5)/PFAC
      EQK(69)=EG(3)*EG(6)/EG(5)/EG(5)
      EQK(70)=EG(4)*EG(6)/EG(5)/EG(7)
      EQK(148)=EQK(70)
      EQK(71)=EG(6)*EG(7)/EG(5)/EG(8)
      EQK(72)=EQK(71)
      EQK(73)=EG(2)*EG(15)/EG(5)/EG(9)
      EQK(74)=EG(2)*EG(17)/EG(5)/EG(10)
      EQK(75)=EG(2)*EG(18)/EG(5)/EG(11)
      EQK(76)=EG(6)*EG(10)/EG(5)/EG(11)
      EQK(77)=EG(2)*EG(18)/EG(5)/EG(12)
      EQK(78)=EG(6)*EG(11)/EG(5)/EG(13)
      EQK(79)=EG(6)*EG(12)/EG(5)/EG(13)
      EQK(80)=EG(6)*EG(13)/EG(5)/EG(14)
      EQK(81)=EG(2)*EG(16)/EG(5)/EG(15)
      EQK(82)=EG(6)*EG(15)/EG(5)/EG(17)
      EQK(83)=EG(6)*EG(17)/EG(5)/EG(18)
      EQK(84)=EG(6)*EG(18)/EG(5)/EG(19)
      EQK(85)=EG(2)*EG(26)/EG(5)/EG(20)
      EQK(86)=EG(13)*EG(15)/EG(5)/EG(20)
      EQK(87)=EG(6)*EG(20)/EG(5)/EG(21)
      EQK(88)=EG(6)*EG(21)/EG(5)/EG(22)
      EQK(89)=EG(6)*EG(23)/EG(5)/EG(24)
      EQK(90)=EG(6)*EG(25)/EG(5)/EG(26)
      EQK(91)=EG(4)*EG(8)/EG(7)/EG(7)
      EQK(92)=EQK(91)
      EQK(93)=EG(5)*EG(18)/EG(7)/EG(11)
      EQK(94)=EG(4)*EG(14)/EG(7)/EG(13)
      EQK(95)=EG(5)*EG(19)/EG(7)/EG(13)
      EQK(96)=EG(5)*EG(16)/EG(7)/EG(15)
      EQK(97)=EG(8)*EG(17)/EG(7)/EG(18)
      EQK(98)=EG(3)*EG(15)/EG(4)/EG(9)
      EQK(99)=EG(2)*EG(20)/EG(9)/EG(13)
      EQK(100)=EG(3)*EG(17)/EG(4)/EG(10)
      EQK(101)=EG(2)*EG(11)/EG(1)/EG(10)
      EQK(102)=EG(2)*EG(18)/EG(6)/EG(10)
      EQK(103)=EG(2)*EG(20)/EG(10)/EG(11)
      EQK(105)=EG(2)*EG(22)/EG(10)/EG(14)
      EQK(106)=EG(25)/EG(10)/EG(15)/PFAC
      EQK(107)=EG(15)*EG(17)/EG(10)/EG(16)
      EQK(108)=EG(2)*EG(26)/EG(10)/EG(18)
      EQK(109)=EG(15)*EG(20)/EG(10)/EG(25)
      EQK(111)=EG(2)*EG(13)/EG(1)/EG(11)
      EQK(112)=EG(1)*EG(20)/EG(11)/EG(11)
      EQK(113)=EG(2)*EG(22)/EG(11)/EG(13)
      EQK(114)=EG(13)*EG(13)/EG(11)/EG(14)
      EQK(115)=EG(26)/EG(11)/EG(15)/PFAC
      EQK(117)=EG(11)/EG(12)
      EQK(121)=EQK(117)
      EQK(124)=EQK(117)
      EQK(125)=EQK(117)
      EQK(118)=EG(2)*EG(5)*EG(15)/EG(4)/EG(12)*PFAC
      EQK(119)=EG(6)*EG(15)/EG(4)/EG(12)
      EQK(120)=EG(2)*EG(13)/EG(1)/EG(12)
      EQK(122)=EG(2)*EG(22)/EG(12)/EG(13)
      EQK(123)=EG(13)*EG(13)/EG(12)/EG(14)
      EQK(126)=EG(15)*EG(18)/EG(12)/EG(16)
      EQK(128)=EG(3)*EG(19)/EG(4)/EG(13)
      EQK(129)=EG(5)*EG(18)/EG(4)/EG(13)
      EQK(130)=EG(7)*EG(14)/EG(8)/EG(13)
      EQK(131)=EG(24)/EG(13)/EG(13)/PFAC
      EQK(132)=EG(2)*EG(23)/EG(13)/EG(13)
      EQK(133)=EG(14)*EG(15)/EG(13)/EG(17)
      EQK(134)=EG(14)*EG(17)/EG(13)/EG(18)
      EQK(135)=EG(14)*EG(21)/EG(13)/EG(22)
      EQK(136)=EG(14)*EG(23)/EG(13)/EG(24)
      EQK(137)=EG(2)*EG(15)/EG(17)*PFAC
      EQK(138)=EQK(137)
      EQK(139)=EG(7)*EG(15)/EG(4)/EG(17)
      EQK(140)=EG(7)*EG(18)/EG(4)/EG(19)
      EQK(141)=EG(17)*EG(18)/EG(4)/EG(21)
      EQK(142)=EG(1)*EG(20)/EG(22)*PFAC
      EQK(143)=EG(7)*EG(22)/EG(4)/EG(23)
      EQK(144)=EG(5)*EG(15)*EG(15)/EG(4)/EG(25)*PFAC
      EQK(145)=EG(15)*EG(15)*EG(20)/EG(25)/EG(25)*PFAC
      EQK(147)=EG(2)*EG(27)/EG(3)/EG(22)
      EQK(150)=EG(13)/EG(1)/EG(10)/PFAC
      EQK(152)=EG(3)*EG(18)/EG(4)/EG(11)
      EQK(156)=EG(7)*EG(20)/EG(4)/EG(21)
      EQK(157)=EG(27)/EG(2)/EG(26)/PFAC
      EQK(161)=EG(13)*EG(17)/EG(2)/EG(27)
      EQK(162)=EG(1)*EG(26)/EG(2)/EG(27)
      EQK(163)=EG(6)*EG(26)/EG(5)/EG(27)
      EQK(164)=EG(13)*EG(22)/EG(2)/EG(28)
      EQK(165)=EG(2)*EG(13)*EG(26)/EG(3)/EG(28)*PFAC
      EQK(167)=EG(28)/EG(13)/EG(21)/PFAC
C
      RB(1) = RF(1) / MAX(EQK(1),SMALL)
      RB(2) = RF(2) / MAX(EQK(2),SMALL)
      RB(3) = RF(3) / MAX(EQK(3),SMALL)
      RB(4) = RF(4) / MAX(EQK(4),SMALL)
      RB(5) = RF(5) / MAX(EQK(5),SMALL)
      RB(6) = RF(6) / MAX(EQK(6),SMALL)
      RB(7) = RF(7) / MAX(EQK(7),SMALL)
      RB(8) = RF(8) / MAX(EQK(8),SMALL)
      RB(9) = RF(9) / MAX(EQK(9),SMALL)
      RB(10) = RF(10) / MAX(EQK(10),SMALL)
      RB(11) = RF(11) / MAX(EQK(11),SMALL)
      RB(12) = RF(12) / MAX(EQK(12),SMALL)
      RB(13) = RF(13) / MAX(EQK(13),SMALL)
      RB(14) = RF(14) / MAX(EQK(14),SMALL)
      RB(15) = RF(15) / MAX(EQK(15),SMALL)
      RB(16) = RF(16) / MAX(EQK(16),SMALL)
      RB(17) = RF(17) / MAX(EQK(17),SMALL)
      RB(18) = RF(18) / MAX(EQK(18),SMALL)
      RB(19) = RF(19) / MAX(EQK(19),SMALL)
      RB(20) = RF(20) / MAX(EQK(20),SMALL)
      RB(21) = RF(21) / MAX(EQK(21),SMALL)
      RB(22) = RF(22) / MAX(EQK(22),SMALL)
      RB(23) = RF(23) / MAX(EQK(23),SMALL)
      RB(24) = RF(24) / MAX(EQK(24),SMALL)
      RB(25) = RF(25) / MAX(EQK(25),SMALL)
      RB(26) = RF(26) / MAX(EQK(26),SMALL)
      RB(27) = RF(27) / MAX(EQK(27),SMALL)
      RB(28) = RF(28) / MAX(EQK(28),SMALL)
      RB(29) = RF(29) / MAX(EQK(29),SMALL)
      RB(30) = RF(30) / MAX(EQK(30),SMALL)
      RB(31) = RF(31) / MAX(EQK(31),SMALL)
      RB(32) = RF(32) / MAX(EQK(32),SMALL)
      RB(33) = RF(33) / MAX(EQK(33),SMALL)
      RB(34) = RF(34) / MAX(EQK(34),SMALL)
      RB(35) = RF(35) / MAX(EQK(35),SMALL)
      RB(36) = RF(36) / MAX(EQK(36),SMALL)
      RB(37) = RF(37) / MAX(EQK(37),SMALL)
      RB(38) = RF(38) / MAX(EQK(38),SMALL)
      RB(39) = RF(39) / MAX(EQK(39),SMALL)
      RB(40) = RF(40) / MAX(EQK(40),SMALL)
      RB(41) = RF(41) / MAX(EQK(41),SMALL)
      RB(42) = RF(42) / MAX(EQK(42),SMALL)
      RB(43) = RF(43) / MAX(EQK(43),SMALL)
      RB(44) = RF(44) / MAX(EQK(44),SMALL)
      RB(45) = RF(45) / MAX(EQK(45),SMALL)
      RB(46) = RF(46) / MAX(EQK(46),SMALL)
      RB(47) = RF(47) / MAX(EQK(47),SMALL)
      RB(48) = RF(48) / MAX(EQK(48),SMALL)
      RB(49) = RF(49) / MAX(EQK(49),SMALL)
      RB(50) = RF(50) / MAX(EQK(50),SMALL)
      RB(51) = RF(51) / MAX(EQK(51),SMALL)
      RB(52) = RF(52) / MAX(EQK(52),SMALL)
      RB(53) = RF(53) / MAX(EQK(53),SMALL)
      RB(54) = RF(54) / MAX(EQK(54),SMALL)
      RB(55) = RF(55) / MAX(EQK(55),SMALL)
      RB(56) = RF(56) / MAX(EQK(56),SMALL)
      RB(57) = RF(57) / MAX(EQK(57),SMALL)
      RB(58) = RF(58) / MAX(EQK(58),SMALL)
      RB(59) = RF(59) / MAX(EQK(59),SMALL)
      RB(60) = RF(60) / MAX(EQK(60),SMALL)
      RB(61) = RF(61) / MAX(EQK(61),SMALL)
      RB(62) = RF(62) / MAX(EQK(62),SMALL)
      RB(63) = RF(63) / MAX(EQK(63),SMALL)
      RB(64) = RF(64) / MAX(EQK(64),SMALL)
      RB(65) = RF(65) / MAX(EQK(65),SMALL)
      RB(66) = RF(66) / MAX(EQK(66),SMALL)
      RB(67) = RF(67) / MAX(EQK(67),SMALL)
      RB(68) = RF(68) / MAX(EQK(68),SMALL)
      RB(69) = RF(69) / MAX(EQK(69),SMALL)
      RB(70) = RF(70) / MAX(EQK(70),SMALL)
      RB(71) = RF(71) / MAX(EQK(71),SMALL)
      RB(72) = RF(72) / MAX(EQK(72),SMALL)
      RB(73) = RF(73) / MAX(EQK(73),SMALL)
      RB(74) = RF(74) / MAX(EQK(74),SMALL)
      RB(75) = RF(75) / MAX(EQK(75),SMALL)
      RB(76) = RF(76) / MAX(EQK(76),SMALL)
      RB(77) = RF(77) / MAX(EQK(77),SMALL)
      RB(78) = RF(78) / MAX(EQK(78),SMALL)
      RB(79) = RF(79) / MAX(EQK(79),SMALL)
      RB(80) = RF(80) / MAX(EQK(80),SMALL)
      RB(81) = RF(81) / MAX(EQK(81),SMALL)
      RB(82) = RF(82) / MAX(EQK(82),SMALL)
      RB(83) = RF(83) / MAX(EQK(83),SMALL)
      RB(84) = RF(84) / MAX(EQK(84),SMALL)
      RB(85) = RF(85) / MAX(EQK(85),SMALL)
      RB(86) = RF(86) / MAX(EQK(86),SMALL)
      RB(87) = RF(87) / MAX(EQK(87),SMALL)
      RB(88) = RF(88) / MAX(EQK(88),SMALL)
      RB(89) = RF(89) / MAX(EQK(89),SMALL)
      RB(90) = RF(90) / MAX(EQK(90),SMALL)
      RB(91) = RF(91) / MAX(EQK(91),SMALL)
      RB(92) = RF(92) / MAX(EQK(92),SMALL)
      RB(93) = RF(93) / MAX(EQK(93),SMALL)
      RB(94) = RF(94) / MAX(EQK(94),SMALL)
      RB(95) = RF(95) / MAX(EQK(95),SMALL)
      RB(96) = RF(96) / MAX(EQK(96),SMALL)
      RB(97) = RF(97) / MAX(EQK(97),SMALL)
      RB(98) = RF(98) / MAX(EQK(98),SMALL)
      RB(99) = RF(99) / MAX(EQK(99),SMALL)
      RB(100) = RF(100) / MAX(EQK(100),SMALL)
      RB(101) = RF(101) / MAX(EQK(101),SMALL)
      RB(102) = RF(102) / MAX(EQK(102),SMALL)
      RB(103) = RF(103) / MAX(EQK(103),SMALL)
     RF(103) = 0
      RB(105) = RF(105) / MAX(EQK(105),SMALL)
      RB(106) = RF(106) / MAX(EQK(106),SMALL)
      RB(107) = RF(107) / MAX(EQK(107),SMALL)
      RB(108) = RF(108) / MAX(EQK(108),SMALL)
      RB(109) = RF(109) / MAX(EQK(109),SMALL)
     RF(109) = 0
      RB(110) = 0
      RB(111) = RF(111) / MAX(EQK(111),SMALL)
      RB(112) = RF(112) / MAX(EQK(112),SMALL)
     RF(112) = 0
      RB(113) = RF(113) / MAX(EQK(113),SMALL)
      RB(114) = RF(114) / MAX(EQK(114),SMALL)
      RB(115) = RF(115) / MAX(EQK(115),SMALL)
      RB(117) = RF(117) / MAX(EQK(117),SMALL)
      RB(118) = RF(118) / MAX(EQK(118),SMALL)
      RB(119) = RF(119) / MAX(EQK(119),SMALL)
      RB(120) = RF(120) / MAX(EQK(120),SMALL)
      RB(121) = RF(121) / MAX(EQK(121),SMALL)
      RB(122) = RF(122) / MAX(EQK(122),SMALL)
      RB(123) = RF(123) / MAX(EQK(123),SMALL)
      RB(124) = RF(124) / MAX(EQK(124),SMALL)
      RB(125) = RF(125) / MAX(EQK(125),SMALL)
      RB(126) = RF(126) / MAX(EQK(126),SMALL)
      RB(128) = RF(128) / MAX(EQK(128),SMALL)
      RB(129) = RF(129) / MAX(EQK(129),SMALL)
      RB(130) = RF(130) / MAX(EQK(130),SMALL)
      RB(131) = RF(131) / MAX(EQK(131),SMALL)
      RB(132) = RF(132) / MAX(EQK(132),SMALL)
      RB(133) = RF(133) / MAX(EQK(133),SMALL)
      RB(134) = RF(134) / MAX(EQK(134),SMALL)
      RB(135) = RF(135) / MAX(EQK(135),SMALL)
      RB(136) = RF(136) / MAX(EQK(136),SMALL)
      RB(137) = RF(137) / MAX(EQK(137),SMALL)
      RB(138) = RF(138) / MAX(EQK(138),SMALL)
      RB(139) = RF(139) / MAX(EQK(139),SMALL)
      RB(140) = RF(140) / MAX(EQK(140),SMALL)
      RB(142) = RF(142) / MAX(EQK(142),SMALL)
      RB(143) = RF(143) / MAX(EQK(143),SMALL)
      RB(144) = RF(144) / MAX(EQK(144),SMALL)
      RB(145) = RF(145) / MAX(EQK(145),SMALL)
     RF(145) = 0
      RB(146) = 0.0
      RB(147) = RF(147) / MAX(EQK(147),SMALL)
      RB(148) = RF(148) / MAX(EQK(148),SMALL)
      RB(149) = 0.0
      RB(150) = RF(150) / MAX(EQK(150),SMALL)
      RB(151) = 0.0
      RB(152) = RF(152) / MAX(EQK(152),SMALL)
      RB(153) = 0.0
      RB(154) = 0.0
      RB(156) = RF(156) / MAX(EQK(156),SMALL)
      RB(157) = RF(157) / MAX(EQK(157),SMALL)
      RB(159) = 0.0
      RB(161) = RF(161) / MAX(EQK(161),SMALL)
      RF(161) = 0
      RB(162) = RF(162) / MAX(EQK(162),SMALL)
      RB(163) = RF(163) / MAX(EQK(163),SMALL)
      RB(164) = RF(164) / MAX(EQK(164),SMALL)
      RB(165) = RF(165) / MAX(EQK(165),SMALL)
      RB(167) = RF(167) / MAX(EQK(167),SMALL)
C
      RKLOW(1) = EXP(3.40312786D1 -1.50965D3/T)
      RKLOW(2) = EXP(5.99064331D1 -2.76D0*ALOGT -8.05146668D2/T)
      RKLOW(3) = EXP(7.69484824D1 -4.76D0*ALOGT -1.22784867D3/T)
      RKLOW(4) = EXP(5.61662604D1 -2.57D0*ALOGT -2.13867084D2/T)
      RKLOW(5) = EXP(6.98660102D1 -4.8D0*ALOGT -2.79788467D3/T)
      RKLOW(6) = EXP(9.34384048D1 -7.27D0*ALOGT -3.63322434D3/T)
      RKLOW(7) = EXP(6.9414025D1 -3.86D0*ALOGT -1.67067934D3/T)
      RKLOW(8) = EXP(9.61977483D1 -7.62D0*ALOGT -3.50742017D3/T)
      RKLOW(9) = EXP(9.50941235D1 -7.08D0*ALOGT -3.36400342D3/T)
      RKLOW(10) = EXP(6.37931383D1 -3.42D0*ALOGT -4.24463259D4/T)
      RKLOW(11) = EXP(4.22794408D1 -9.D-1*ALOGT +8.55468335D2/T)
      RKLOW(12) = EXP(6.54619238D1 -3.74D0*ALOGT -9.74227469D2/T)
      RKLOW(13) = EXP(7.69748493D1 -5.11D0*ALOGT -3.57032226D3/T)
      RKLOW(14) = EXP(9.56297642D1 -7.03D0*ALOGT -1.38988444D3/T)
      RKLOW(15) = EXP(1.17889265D2 -9.3D0*ALOGT -4.92145901D4/T)
      RKLOW(16) = EXP(5.91374013D1 -2.8D0*ALOGT -2.96897834D2/T)
      RKLOW(17) = EXP(9.67205025D1 -7.63D0*ALOGT -1.93939704D3/T)
      RKLOW(18) = EXP(1.35001549D2 -1.194D1*ALOGT -4.9163262D3/T)
C
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE RDSMH  (T, TLOG, SMH)
C
C  START PROLOGUE
C
C  SUBROUTINE DKSMH  (T, ICKWRK, RCKWRK, SMH)*
C     Returns the array of entropies minus enthalpies for the species.
C     It is normally not called directly by the user.
C
C  INPUT
C     T      - Temperature.
C                   cgs units - K
C                   Data type - real scalar
C     TLOG      - Log Temperature.
C  OUTPUT
C     SMH    - Entropy minus enthalpy for the species,
C              SMH(K) = S(K)/R - H(K)/RT.
C                   cgs units - none
C                   Data type - real array
C                   Dimension SMH(*) at least KK, the total number of
C                   species.
C
C  END PROLOGUE
C
C*****precision > double
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C*****precision > single
C        IMPLICIT REAL (A-H, O-Z), INTEGER (I-N)
C*****END precision > single
C
      DIMENSION SMH(*), TN(5)
C
      TI = 1.0D0/T
C
      TN(1) = TLOG - 1.0
      TN(2) = T
      TN(3) = TN(2)*T
      TN(4) = TN(3)*T
      TN(5) = TN(4)*T
C
      IF (T .GT. 1.D3) THEN
C
      SMH(1) = -3.20502331D+00 + 9.50158922D+02*TI 
     *         + 3.33727920D+00*TN(1) - 2.47012365D-05*TN(2) 
     *         + 8.32427963D-08*TN(3) - 1.49638662D-11*TN(4) 
     *         + 1.00127688D-15*TN(5) 
      SMH(2) = -4.46682914D-01 - 2.54736599D+04*TI 
     *         + 2.50000001D+00*TN(1) - 1.15421486D-11*TN(2) 
     *         + 2.69269913D-15*TN(3) - 3.94596029D-19*TN(4) 
     *         + 2.49098679D-23*TN(5) 
      SMH(3) = 4.78433864D+00 - 2.92175791D+04*TI 
     *         + 2.56942078D+00*TN(1) - 4.29870569D-05*TN(2) 
     *         + 6.99140982D-09*TN(3) - 8.34814992D-13*TN(4) 
     *         + 6.14168455D-17*TN(5) 
      SMH(4) = 5.45323129D+00 + 1.08845772D+03*TI 
     *         + 3.28253784D+00*TN(1) + 7.41543770D-04*TN(2) 
     *         - 1.26327778D-07*TN(3) + 1.74558796D-11*TN(4) 
     *         - 1.08358897D-15*TN(5) 
      SMH(5) = 4.47669610D+00 - 3.85865700D+03*TI 
     *         + 3.09288767D+00*TN(1) + 2.74214858D-04*TN(2) 
     *         + 2.10842047D-08*TN(3) - 7.32884630D-12*TN(4) 
     *         + 5.87061880D-16*TN(5) 
      SMH(6) = 4.96677010D+00 + 3.00042971D+04*TI 
     *         + 3.03399249D+00*TN(1) + 1.08845902D-03*TN(2) 
     *         - 2.73454197D-08*TN(3) - 8.08683225D-12*TN(4) 
     *         + 8.41004960D-16*TN(5) 
      SMH(7) = 3.78510215D+00 - 1.11856713D+02*TI 
     *         + 4.01721090D+00*TN(1) + 1.11991007D-03*TN(2) 
     *         - 1.05609692D-07*TN(3) + 9.52053083D-12*TN(4) 
     *         - 5.39542675D-16*TN(5) 
      SMH(8) = 2.91615662D+00 + 1.78617877D+04*TI 
     *         + 4.16500285D+00*TN(1) + 2.45415847D-03*TN(2) 
     *         - 3.16898708D-07*TN(3) + 3.09321655D-11*TN(4) 
     *         - 1.43954153D-15*TN(5) 
      SMH(9) = 4.80150373D+00 - 8.54512953D+04*TI 
     *         + 2.49266888D+00*TN(1) + 2.39944642D-05*TN(2) 
     *         - 1.20722503D-08*TN(3) + 3.11909191D-12*TN(4) 
     *         - 2.43638946D-16*TN(5) 
      SMH(10) = 5.48497999D+00 - 7.10124364D+04*TI 
     *         + 2.87846473D+00*TN(1) + 4.85456841D-04*TN(2) 
     *         + 2.40742758D-08*TN(3) - 1.08906541D-11*TN(4) 
     *         + 8.80396915D-16*TN(5) 
      SMH(11) = 6.17119324D+00 - 4.62636040D+04*TI 
     *         + 2.87410113D+00*TN(1) + 1.82819646D-03*TN(2) 
     *         - 2.34824328D-07*TN(3) + 2.16816291D-11*TN(4) 
     *         - 9.38637835D-16*TN(5) 
      SMH(12) = 8.62650169D+00 - 5.09259997D+04*TI 
     *         + 2.29203842D+00*TN(1) + 2.32794319D-03*TN(2) 
     *         - 3.35319912D-07*TN(3) + 3.48255000D-11*TN(4) 
     *         - 1.69858183D-15*TN(5) 
      SMH(13) = 8.48007179D+00 - 1.67755843D+04*TI 
     *         + 2.28571772D+00*TN(1) + 3.61995018D-03*TN(2) 
     *         - 4.97857247D-07*TN(3) + 4.96403870D-11*TN(4) 
     *         - 2.33577197D-15*TN(5) 
      SMH(14) = 1.84373180D+01 + 9.46834459D+03*TI 
     *         + 7.48514950D-02*TN(1) + 6.69547335D-03*TN(2) 
     *         - 9.55476348D-07*TN(3) + 1.01910446D-10*TN(4) 
     *         - 5.09076150D-15*TN(5) 
      SMH(15) = 7.81868772D+00 + 1.41518724D+04*TI 
     *         + 2.71518561D+00*TN(1) + 1.03126372D-03*TN(2) 
     *         - 1.66470962D-07*TN(3) + 1.91710840D-11*TN(4) 
     *         - 1.01823858D-15*TN(5) 
      SMH(16) = 2.27163806D+00 + 4.87591660D+04*TI 
     *         + 3.85746029D+00*TN(1) + 2.20718513D-03*TN(2) 
     *         - 3.69135673D-07*TN(3) + 4.36241823D-11*TN(4) 
     *         - 2.36042082D-15*TN(5) 
      SMH(17) = 9.79834492D+00 - 4.01191815D+03*TI 
     *         + 2.77217438D+00*TN(1) + 2.47847763D-03*TN(2) 
     *         - 4.14076022D-07*TN(3) + 4.90968148D-11*TN(4) 
     *         - 2.66754356D-15*TN(5) 
      SMH(18) = 1.36563230D+01 + 1.39958323D+04*TI 
     *         + 1.76069008D+00*TN(1) + 4.60000041D-03*TN(2) 
     *         - 7.37098022D-07*TN(3) + 8.38676767D-11*TN(4) 
     *         - 4.41927820D-15*TN(5) 
      SMH(19) = 2.92957500D+00 - 1.27832520D+02*TI 
     *         + 3.77079900D+00*TN(1) + 3.93574850D-03*TN(2) 
     *         - 4.42730667D-07*TN(3) + 3.28702583D-11*TN(4) 
     *         - 1.05630800D-15*TN(5) 
      SMH(20) = -1.23028121D+00 - 2.59359992D+04*TI 
     *         + 4.14756964D+00*TN(1) + 2.98083332D-03*TN(2) 
     *         - 3.95491420D-07*TN(3) + 3.89510143D-11*TN(4) 
     *         - 1.80617607D-15*TN(5) 
      SMH(21) = 7.78732378D+00 - 3.46128739D+04*TI 
     *         + 3.01672400D+00*TN(1) + 5.16511460D-03*TN(2) 
     *         - 7.80137248D-07*TN(3) + 8.48027400D-11*TN(4) 
     *         - 4.31303520D-15*TN(5) 
      SMH(22) = 1.03053693D+01 - 4.93988614D+03*TI 
     *         + 2.03611116D+00*TN(1) + 7.32270755D-03*TN(2) 
     *         - 1.11846319D-06*TN(3) + 1.22685769D-10*TN(4) 
     *         - 6.28530305D-15*TN(5) 
      SMH(23) = 1.34624343D+01 - 1.28575200D+04*TI 
     *         + 1.95465642D+00*TN(1) + 8.69863610D-03*TN(2) 
     *         - 1.33034445D-06*TN(3) + 1.46014741D-10*TN(4) 
     *         - 7.48207880D-15*TN(5) 
      SMH(24) = 1.51156107D+01 + 1.14263932D+04*TI 
     *         + 1.07188150D+00*TN(1) + 1.08426339D-02*TN(2) 
     *         - 1.67093445D-06*TN(3) + 1.84510001D-10*TN(4) 
     *         - 9.50014450D-15*TN(5) 
      SMH(25) = -3.93025950D+00 - 1.93272150D+04*TI 
     *         + 5.62820580D+00*TN(1) + 2.04267005D-03*TN(2) 
     *         - 2.65575783D-07*TN(3) + 2.38550433D-11*TN(4) 
     *         - 9.70391600D-16*TN(5) 
      SMH(26) = 6.32247205D-01 + 7.55105311D+03*TI 
     *         + 4.51129732D+00*TN(1) + 4.50179872D-03*TN(2) 
     *         - 6.94899392D-07*TN(3) + 7.69454902D-11*TN(4) 
     *         - 3.97419100D-15*TN(5) 
      SMH(27) = -5.03208790D+00 - 4.90321780D+02*TI 
     *         + 5.97566990D+00*TN(1) + 4.06529570D-03*TN(2) 
     *         - 4.57270750D-07*TN(3) + 3.39192008D-11*TN(4) 
     *         - 1.08800855D-15*TN(5) 
      SMH(28) = -1.33133500D+01 + 9.23570300D+02*TI 
     *         + 6.73225700D+00*TN(1) + 7.45417000D-03*TN(2) 
     *         - 8.24983167D-07*TN(3) + 6.01001833D-11*TN(4) 
     *         - 1.88310200D-15*TN(5) 
C
      ELSE
C
      SMH(1) = 6.83010238D-01 + 9.17935173D+02*TI 
     *         + 2.34433112D+00*TN(1) + 3.99026037D-03*TN(2) 
     *         - 3.24635850D-06*TN(3) + 1.67976745D-09*TN(4) 
     *         - 3.68805881D-13*TN(5) 
      SMH(2) = -4.46682853D-01 - 2.54736599D+04*TI 
     *         + 2.50000000D+00*TN(1) + 3.52666409D-13*TN(2) 
     *         - 3.32653273D-16*TN(3) + 1.91734693D-19*TN(4) 
     *         - 4.63866166D-23*TN(5) 
      SMH(3) = 2.05193346D+00 - 2.91222592D+04*TI 
     *         + 3.16826710D+00*TN(1) - 1.63965942D-03*TN(2) 
     *         + 1.10717733D-06*TN(3) - 5.10672187D-10*TN(4) 
     *         + 1.05632986D-13*TN(5) 
      SMH(4) = 3.65767573D+00 + 1.06394356D+03*TI 
     *         + 3.78245636D+00*TN(1) - 1.49836708D-03*TN(2) 
     *         + 1.64121700D-06*TN(3) - 8.06774591D-10*TN(4) 
     *         + 1.62186419D-13*TN(5) 
      SMH(5) = -1.03925458D-01 - 3.61508056D+03*TI 
     *         + 3.99201543D+00*TN(1) - 1.20065876D-03*TN(2) 
     *         + 7.69656402D-07*TN(3) - 3.23427778D-10*TN(4) 
     *         + 6.82057350D-14*TN(5) 
      SMH(6) = -8.49032208D-01 + 3.02937267D+04*TI 
     *         + 4.19864056D+00*TN(1) - 1.01821705D-03*TN(2) 
     *         + 1.08673369D-06*TN(3) - 4.57330885D-10*TN(4) 
     *         + 8.85989085D-14*TN(5) 
      SMH(7) = 3.71666245D+00 - 2.94808040D+02*TI 
     *         + 4.30179801D+00*TN(1) - 2.37456025D-03*TN(2) 
     *         + 3.52638152D-06*TN(3) - 2.02303245D-09*TN(4) 
     *         + 4.64612562D-13*TN(5) 
      SMH(8) = 3.43505074D+00 + 1.77025821D+04*TI 
     *         + 4.27611269D+00*TN(1) - 2.71411208D-04*TN(2) 
     *         + 2.78892835D-06*TN(3) - 1.79809011D-09*TN(4) 
     *         + 4.31227182D-13*TN(5) 
      SMH(9) = 4.53130848D+00 - 8.54438832D+04*TI 
     *         + 2.55423955D+00*TN(1) - 1.60768862D-04*TN(2) 
     *         + 1.22298708D-07*TN(3) - 6.10195741D-11*TN(4) 
     *         + 1.33260723D-14*TN(5) 
      SMH(10) = 2.08401108D+00 - 7.07972934D+04*TI 
     *         + 3.48981665D+00*TN(1) + 1.61917771D-04*TN(2) 
     *         - 2.81498442D-07*TN(3) + 2.63514439D-10*TN(4) 
     *         - 7.03045335D-14*TN(5) 
      SMH(11) = 1.56253185D+00 - 4.60040401D+04*TI 
     *         + 3.76267867D+00*TN(1) + 4.84436072D-04*TN(2) 
     *         + 4.65816402D-07*TN(3) - 3.20909294D-10*TN(4) 
     *         + 8.43708595D-14*TN(5) 
      SMH(12) = -7.69118967D-01 - 5.04968163D+04*TI 
     *         + 4.19860411D+00*TN(1) - 1.18330710D-03*TN(2) 
     *         + 1.37216037D-06*TN(3) - 5.57346651D-10*TN(4) 
     *         + 9.71573685D-14*TN(5) 
      SMH(13) = 1.60456433D+00 - 1.64449988D+04*TI 
     *         + 3.67359040D+00*TN(1) + 1.00547588D-03*TN(2) 
     *         + 9.55036427D-07*TN(3) - 5.72597854D-10*TN(4) 
     *         + 1.27192867D-13*TN(5) 
      SMH(14) = -4.64130376D+00 + 1.02466476D+04*TI 
     *         + 5.14987613D+00*TN(1) - 6.83548940D-03*TN(2) 
     *         + 8.19667665D-06*TN(3) - 4.03952522D-09*TN(4) 
     *         + 8.33469780D-13*TN(5) 
      SMH(15) = 3.50840928D+00 + 1.43440860D+04*TI 
     *         + 3.57953347D+00*TN(1) - 3.05176840D-04*TN(2) 
     *         + 1.69469055D-07*TN(3) + 7.55838237D-11*TN(4) 
     *         - 4.52212249D-14*TN(5) 
      SMH(16) = 9.90105222D+00 + 4.83719697D+04*TI 
     *         + 2.35677352D+00*TN(1) + 4.49229839D-03*TN(2) 
     *         - 1.18726045D-06*TN(3) + 2.04932518D-10*TN(4) 
     *         - 7.18497740D-15*TN(5) 
      SMH(17) = 3.39437243D+00 - 3.83956496D+03*TI 
     *         + 4.22118584D+00*TN(1) - 1.62196266D-03*TN(2) 
     *         + 2.29665743D-06*TN(3) - 1.10953411D-09*TN(4) 
     *         + 2.16884433D-13*TN(5) 
      SMH(18) = 6.02812900D-01 + 1.43089567D+04*TI 
     *         + 4.79372315D+00*TN(1) - 4.95416685D-03*TN(2) 
     *         + 6.22033347D-06*TN(3) - 3.16071051D-09*TN(4) 
     *         + 6.58863260D-13*TN(5) 
      SMH(19) = 1.31521770D+01 - 9.78601100D+02*TI 
     *         + 2.10620400D+00*TN(1) + 3.60829750D-03*TN(2) 
     *         + 8.89745333D-07*TN(3) - 6.14803000D-10*TN(4) 
     *         + 1.03780500D-13*TN(5) 
      SMH(20) = 1.39397051D+01 - 2.64289807D+04*TI 
     *         + 8.08681094D-01*TN(1) + 1.16807815D-02*TN(2) 
     *         - 5.91953025D-06*TN(3) + 2.33460364D-09*TN(4) 
     *         - 4.25036487D-13*TN(5) 
      SMH(21) = 8.51054025D+00 - 3.48598468D+04*TI 
     *         + 3.21246645D+00*TN(1) + 7.57395810D-04*TN(2) 
     *         + 4.32015687D-06*TN(3) - 2.98048206D-09*TN(4) 
     *         + 7.35754365D-13*TN(5) 
      SMH(22) = 4.09733096D+00 - 5.08977593D+03*TI 
     *         + 3.95920148D+00*TN(1) - 3.78526124D-03*TN(2) 
     *         + 9.51650487D-06*TN(3) - 5.76323961D-09*TN(4) 
     *         + 1.34942187D-12*TN(5) 
      SMH(23) = 4.70720924D+00 - 1.28416265D+04*TI 
     *         + 4.30646568D+00*TN(1) - 2.09329446D-03*TN(2) 
     *         + 8.28571345D-06*TN(3) - 4.99272172D-09*TN(4) 
     *         + 1.15254502D-12*TN(5) 
      SMH(24) = 2.66682316D+00 + 1.15222055D+04*TI 
     *         + 4.29142492D+00*TN(1) - 2.75077135D-03*TN(2) 
     *         + 9.99063813D-06*TN(3) - 5.90388571D-09*TN(4) 
     *         + 1.34342886D-12*TN(5) 
      SMH(25) = 1.24904170D+01 - 2.00594490D+04*TI 
     *         + 2.25172140D+00*TN(1) + 8.82751050D-03*TN(2) 
     *         - 3.95485017D-06*TN(3) + 1.43964658D-09*TN(4) 
     *         - 2.53324055D-13*TN(5) 
      SMH(26) = 1.22156480D+01 + 7.04291804D+03*TI 
     *         + 2.13583630D+00*TN(1) + 9.05943605D-03*TN(2) 
     *         - 2.89912457D-06*TN(3) + 7.78664640D-10*TN(4) 
     *         - 1.00728807D-13*TN(5) 
      SMH(27) = 9.57145350D+00 - 1.52147660D+03*TI 
     *         + 3.40906240D+00*TN(1) + 5.36928700D-03*TN(2) 
     *         + 3.15248750D-07*TN(3) + 5.96548592D-10*TN(4) 
     *         + 1.43369255D-13*TN(5) 
      SMH(28) = 1.61453400D+01 - 1.07482600D+03*TI 
     *         + 1.49330700D+00*TN(1) + 1.04625900D-02*TN(2) 
     *         + 7.47799000D-07*TN(3) - 1.39076000D-09*TN(4) 
     *         + 3.57907300D-13*TN(5) 
      ENDIF
C
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE RATX (T, ALOGT, C, RF, RB, RKLOW)
C
C*****precision > double
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C*****END precision > double
C
      DIMENSION C(*), RF(*), RB(*), CTB(167)
      DIMENSION RKLOW(*)
      DATA SMALL/1.D-50/
C
C     third-body reactions
C
      CTOT = 0.0
      DO K = 1, 19
         CTOT = CTOT + C(K)
      ENDDO
C
      CTB(1) =  CTOT + 1.4D0*C(1) + 1.44D1*C(6) + C(10) + 7.5D-1*C(11) +
     *          2.6D0*C(12) + 2.D0*C(16) + 2.D0*C(15) + 3.D0*C(18) 
      CTB(2) =  CTOT + C(1) + 5.D0*C(6) + C(10) + 5.D-1*C(11) + C(12) + 
     *          2.D0*C(16) + 2.D0*C(15) + 3.D0*C(18) 
      CTB(12) = CTOT + C(1) + 5.D0*C(4) + 5.D0*C(6) + C(10) + 
     *          5.D-1*C(11) + 2.5D0*C(12) + 2.D0*C(16) + 2.D0*C(15) + 
     *          3.D0*C(18) 
      CTB(28) = CTOT - C(4) - C(6) - 2.5D-1*C(11) + 5.D-1*C(12) + 
     *          5.D-1*C(16) - C(19) + 2.D0*C(15) + 3.D0*C(18) 
      CTB(33) = CTOT - C(1) - C(6) + C(10) - C(12) + 2.D0*C(16) + 
     *          2.D0*C(15) + 3.D0*C(18) 
      CTB(37) = CTOT - 2.7D-1*C(1) + 2.65D0*C(6) + C(10) + 2.D0*C(16) + 
     *          2.D0*C(15) + 3.D0*C(18) 
      CTB(44) = CTB(2)
      CTB(46) = CTOT + C(1) + 5.D0*C(6) + 2.D0*C(10) + 5.D-1*C(11) + 
     *          C(12) + 2.D0*C(16) + 2.D0*C(15) + 3.D0*C(18) 
      CTB(48) = CTB(2)
      CTB(50) = CTB(2)
      CTB(55) = CTB(2)
      CTB(56) = CTB(2)
      CTB(58) = CTB(2)
      CTB(60) = CTB(2)
      CTB(66) = CTB(2)
      CTB(68) = CTB(2)
      CTB(106) =CTB(2)
      CTB(115) =CTB(2)
      CTB(131) =CTB(2)
      CTB(138) =CTOT + C(1) - C(6) + C(10) + 5.D-1*C(11) + C(12) + 
     *          2.D0*C(16) 
      CTB(142) =CTB(2)
      CTB(150) =CTB(2)
      CTB(157) =CTB(2)
      CTB(167) =CTB(2)
C
C     If fall-off (pressure correction):
C
C
      PR = RKLOW(1) * CTB(12) / RF(12)
      PCOR = PR / (1.0 + PR)
      RF(12) = RF(12) * PCOR
      RB(12) = RB(12) * PCOR
C
      PR = RKLOW(2) * CTB(44) / RF(44)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 4.38D-1*EXP(-T/9.1D1) + 5.62D-1*EXP(-T/5.836D3)
     *     + EXP(-8.552D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(44) = RF(44) * PCOR
      RB(44) = RB(44) * PCOR
C
      PR = RKLOW(3) * CTB(46) / RF(46)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 2.17D-1*EXP(-T/7.4D1) + 7.83D-1*EXP(-T/2.941D3)
     *     + EXP(-6.964D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(46) = RF(46) * PCOR
      RB(46) = RB(46) * PCOR
C
      PR = RKLOW(4) * CTB(48) / RF(48)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 2.176D-1*EXP(-T/2.71D2) + 7.824D-1*EXP(-T/2.755D3)
     *     + EXP(-6.57D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(48) = RF(48) * PCOR
      RB(48) = RB(48) * PCOR
C
      PR = RKLOW(5) * CTB(50) / RF(50)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 2.42D-1*EXP(-T/9.4D1) + 7.58D-1*EXP(-T/1.555D3)
     *     + EXP(-4.2D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(50) = RF(50) * PCOR
      RB(50) = RB(50) * PCOR
C
      PR = RKLOW(6) * CTB(55) / RF(55)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 2.493D-1*EXP(-T/9.85D1) + 7.507D-1*EXP(-T/1.302D3)
     *     + EXP(-4.167D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(55) = RF(55) * PCOR
      RB(55) = RB(55) * PCOR
C
      PR = RKLOW(7) * CTB(56) / RF(56)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 2.18D-1*EXP(-T/2.075D2) + 7.82D-1*EXP(-T/2.663D3)
     *     + EXP(-6.095D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(56) = RF(56) * PCOR
      RB(56) = RB(56) * PCOR
C
      PR = RKLOW(8) * CTB(58) / RF(58)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 2.47D-2*EXP(-T/2.1D2) + 9.753D-1*EXP(-T/9.84D2)
     *     + EXP(-4.374D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(58) = RF(58) * PCOR
      RB(58) = RB(58) * PCOR
C
      PR = RKLOW(9) * CTB(60) / RF(60)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 1.578D-1*EXP(-T/1.25D2) + 8.422D-1*EXP(-T/2.219D3)
     *     + EXP(-6.882D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(60) = RF(60) * PCOR
      RB(60) = RB(60) * PCOR
C
      PR = RKLOW(10) * CTB(66) / RF(66)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 6.8D-2*EXP(-T/1.97D2) + 9.32D-1*EXP(-T/1.54D3)
     *     + EXP(-1.03D4/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(66) = RF(66) * PCOR
      RB(66) = RB(66) * PCOR
C
      PR = RKLOW(11) * CTB(68) / RF(68)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 2.654D-1*EXP(-T/9.4D1) + 7.346D-1*EXP(-T/1.756D3)
     *     + EXP(-5.182D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(68) = RF(68) * PCOR
      RB(68) = RB(68) * PCOR
C
      PR = RKLOW(12) * CTB(106) / RF(106)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 4.243D-1*EXP(-T/2.37D2) + 5.757D-1*EXP(-T/1.652D3)
     *     + EXP(-5.069D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(106) = RF(106) * PCOR
      RB(106) = RB(106) * PCOR
C
      PR = RKLOW(13) * CTB(115) / RF(115)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 4.093D-1*EXP(-T/2.75D2) + 5.907D-1*EXP(-T/1.226D3)
     *     + EXP(-5.185D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(115) = RF(115) * PCOR
      RB(115) = RB(115) * PCOR
C
      PR = RKLOW(14) * CTB(131) / RF(131)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 3.81D-1*EXP(-T/7.32D1) + 6.19D-1*EXP(-T/1.18D3)
     *     + EXP(-9.999D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(131) = RF(131) * PCOR
      RB(131) = RB(131) * PCOR
C
      PR = RKLOW(15) * CTB(142) / RF(142)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 2.655D-1*EXP(-T/1.8D2) + 7.345D-1*EXP(-T/1.035D3)
     *     + EXP(-5.417D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(142) = RF(142) * PCOR
      RB(142) = RB(142) * PCOR
C
      PR = RKLOW(16) * CTB(150) / RF(150)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 4.22D-1*EXP(-T/1.22D2) + 5.78D-1*EXP(-T/2.535D3)
     *     + EXP(-9.365D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(150) = RF(150) * PCOR
      RB(150) = RB(150) * PCOR
C
      PR = RKLOW(17) * CTB(157) / RF(157)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 5.35D-1*EXP(-T/2.01D2) + 4.65D-1*EXP(-T/1.773D3)
     *     + EXP(-5.333D3/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(157) = RF(157) * PCOR
      RB(157) = RB(157) * PCOR
C
      PR = RKLOW(18) * CTB(167) / RF(167)
      PCOR = PR / (1.0 + PR)
      PRLOG = LOG10(MAX(PR,SMALL))
      FCENT = 8.25D-1*EXP(-T/1.3406D3) + 1.75D-1*EXP(-T/6.D4)
     *     + EXP(-1.01398D4/T)
      FCLOG = LOG10(MAX(FCENT,SMALL))
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(167) = RF(167) * PCOR
      RB(167) = RB(167) * PCOR
C
      RF(1) = RF(1)*CTB(1)*C(3)*C(3)
      RF(2) = RF(2)*CTB(2)*C(3)*C(2)
      RF(3) = RF(3)*C(3)*C(1)
      RF(4) = RF(4)*C(3)*C(7)
      RF(5) = RF(5)*C(3)*C(8)
      RF(6) = RF(6)*C(3)
      RF(7) = RF(7)*C(3)
      RF(8) = RF(8)*C(3)
      RF(9) = RF(9)*C(3)
      RF(10) = RF(10)*C(3)*C(9)
      RF(11) = RF(11)*C(3)*C(10)
      RF(12) = RF(12)*C(3)*C(11)
      RF(13) = RF(13)*C(3)
      RF(14) = RF(14)*C(3)
      RF(15) = RF(15)*C(3)*C(13)
      RF(16) = RF(16)*C(3)
      RF(17) = RF(17)*C(3)*C(14)
      RF(18) = RF(18)*C(3)*C(14)
      RF(19) = RF(19)*C(3)
      RF(20) = RF(20)*C(3)*C(15)
      RF(21) = RF(21)*C(3)
      RF(22) = RF(22)*C(3)*C(16)
      RF(23) = RF(23)*C(3)
      RF(24) = RF(24)*C(3)*C(17)
      RF(25) = RF(25)*C(3)*C(17)
      RF(26) = RF(26)*C(4)*C(11)
      RF(27) = RF(27)*C(4)*C(13)
      RF(28) = RF(28)*CTB(28)*C(2)*C(4)
      RF(29) = RF(29)*C(2)*C(4)*C(4)
      RF(30) = RF(30)*C(2)*C(4)*C(6)
      RF(31) = RF(31)*C(2)*C(4)*C(19)
      RF(32) = RF(32)*C(2)*C(4)
      RF(33) = RF(33)*CTB(33)*C(2)*C(2)
      RF(34) = RF(34)*C(2)*C(2)*C(1)
      RF(35) = RF(35)*C(2)*C(2)*C(6)
      RF(36) = RF(36)*C(2)*C(2)*C(12)
      RF(37) = RF(37)*CTB(37)*C(2)*C(5)
      RF(38) = RF(38)*C(2)*C(7)
      RF(39) = RF(39)*C(2)*C(7)
      RF(40) = RF(40)*C(2)*C(7)
      RF(41) = RF(41)*C(2)*C(8)
      RF(42) = RF(42)*C(2)*C(8)
      RF(43) = RF(43)*C(2)
      RF(44) = RF(44)*C(2)
      RF(45) = RF(45)*C(2)
      RF(46) = RF(46)*C(2)*C(9)
      RF(47) = RF(47)*C(2)*C(10)
      RF(48) = RF(48)*C(2)
      RF(49) = RF(49)*C(2)
      RF(50) = RF(50)*C(2)*C(13)
      RF(51) = RF(51)*C(2)*C(13)
      RF(52) = RF(52)*C(2)
      RF(53) = RF(53)*C(2)
      RF(54) = RF(54)*C(2)
      RF(55) = RF(55)*C(2)*C(14)
      RF(56) = RF(56)*C(2)
      RF(57) = RF(57)*C(2)
      RF(58) = RF(58)*C(2)*C(15)
      RF(59) = RF(59)*C(2)*C(15)
      RF(60) = RF(60)*C(2)
      RF(61) = RF(61)*C(2)
      RF(62) = RF(62)*C(2)*C(16)
      RF(63) = RF(63)*C(2)
      RF(64) = RF(64)*C(2)*C(17)
      RF(65) = RF(65)*C(2)*C(17)
      RF(66) = RF(66)*C(1)*C(11)
      RF(67) = RF(67)*C(5)*C(1)
      RF(68) = RF(68)*C(5)*C(5)
      RF(69) = RF(69)*C(5)*C(5)
      RF(70) = RF(70)*C(5)*C(7)
      RF(71) = RF(71)*C(5)*C(8)
      RF(72) = RF(72)*C(5)*C(8)
      RF(73) = RF(73)*C(5)
      RF(74) = RF(74)*C(5)
      RF(75) = RF(75)*C(5)
      RF(76) = RF(76)*C(5)
      RF(77) = RF(77)*C(5)
      RF(78) = RF(78)*C(5)*C(9)
      RF(79) = RF(79)*C(5)*C(9)
      RF(80) = RF(80)*C(5)*C(10)
      RF(81) = RF(81)*C(5)*C(11)
      RF(82) = RF(82)*C(5)
      RF(83) = RF(83)*C(5)*C(13)
      RF(84) = RF(84)*C(5)
      RF(85) = RF(85)*C(5)*C(14)
      RF(86) = RF(86)*C(5)*C(14)
      RF(87) = RF(87)*C(5)
      RF(88) = RF(88)*C(5)*C(15)
      RF(89) = RF(89)*C(5)*C(16)
      RF(90) = RF(90)*C(5)*C(17)
      RF(91) = RF(91)*C(7)*C(7)
      RF(92) = RF(92)*C(7)*C(7)
      RF(93) = RF(93)*C(7)
      RF(94) = RF(94)*C(7)*C(9)
      RF(95) = RF(95)*C(7)*C(9)
      RF(96) = RF(96)*C(7)*C(11)
      RF(97) = RF(97)*C(7)*C(13)
      RF(98) = RF(98)*C(4)
      RF(99) = RF(99)*C(9)
      RF(100) = RF(100)*C(4)
      RF(101) = RF(101)*C(1)
      RF(102) = RF(102)*C(6)
      RF(105) = RF(105)*C(10)
      RF(106) = RF(106)*C(11)
      RF(107) = RF(107)*C(12)
      RF(108) = RF(108)*C(13)
      RF(110) = RF(110)*C(4)
      RF(111) = RF(111)*C(1)
      RF(113) = RF(113)*C(9)
      RF(114) = RF(114)*C(10)
      RF(115) = RF(115)*C(11)
      RF(117) = RF(117)*C(19)
      RF(118) = RF(118)*C(4)
      RF(119) = RF(119)*C(4)
      RF(120) = RF(120)*C(1)
      RF(121) = RF(121)*C(6)
      RF(122) = RF(122)*C(9)
      RF(123) = RF(123)*C(10)
      RF(124) = RF(124)*C(11)
      RF(125) = RF(125)*C(12)
      RF(126) = RF(126)*C(12)
      RF(128) = RF(128)*C(9)*C(4)
      RF(129) = RF(129)*C(9)*C(4)
      RF(130) = RF(130)*C(9)*C(8)
      RF(131) = RF(131)*C(9)*C(9)
      RF(132) = RF(132)*C(9)*C(9)
      RF(133) = RF(133)*C(9)
      RF(134) = RF(134)*C(9)*C(13)
      RF(135) = RF(135)*C(9)*C(15)
      RF(136) = RF(136)*C(9)*C(16)
      RF(137) = RF(137)*C(6)
      RF(138) = RF(138)*CTB(138)
      RF(139) = RF(139)*C(4)
      RF(140) = RF(140)*C(4)
      RF(141) = RF(141)*C(4)
      RF(142) = RF(142)*C(15)
      RF(143) = RF(143)*C(4)
      RF(144) = RF(144)*C(4)
      RF(146) = RF(146)*C(3)*C(9)
      RF(147) = RF(147)*C(3)*C(15)
      RF(148) = RF(148)*C(5)*C(7)
      RF(149) = RF(149)*C(5)*C(9)
      RF(150) = RF(150)*C(1)
      RF(151) = RF(151)*C(4)
      RF(152) = RF(152)*C(4)
      RF(154) = RF(154)*C(6)
      RF(155) = RF(155)*C(4)
      RF(156) = RF(156)*C(4)
      RF(157) = RF(157)*C(2)*C(17)
      RF(159) = RF(159)*C(4)
      RF(162) = RF(162)*C(2)
      RF(163) = RF(163)*C(5)
      RF(164) = RF(164)*C(18)*C(2)
      RF(165) = RF(165)*C(18)*C(3)
      RF(166) = RF(166)*C(18)*C(3)
      RF(167) = RF(167)*C(9)
      RB(1) = RB(1)*CTB(1)*C(4)
      RB(2) = RB(2)*CTB(2)*C(5)
      RB(3) = RB(3)*C(2)*C(5)
      RB(4) = RB(4)*C(5)*C(4)
      RB(5) = RB(5)*C(5)*C(7)
      RB(6) = RB(6)*C(2)*C(11)
      RB(7) = RB(7)*C(2)
      RB(8) = RB(8)*C(1)*C(11)
      RB(9) = RB(9)*C(2)
      RB(10) = RB(10)*C(2)*C(13)
      RB(11) = RB(11)*C(5)*C(9)
      RB(12) = RB(12)*C(12)
      RB(13) = RB(13)*C(5)*C(11)
      RB(14) = RB(14)*C(2)*C(12)
      RB(15) = RB(15)*C(5)
      RB(16) = RB(16)*C(5)*C(13)
      RB(17) = RB(17)*C(2)
      RB(18) = RB(18)*C(11)
      RB(19) = RB(19)*C(2)*C(17)
      RB(20) = RB(20)*C(9)
      RB(21) = RB(21)*C(9)*C(13)
      RB(22) = RB(22)*C(5)
      RB(23) = RB(23)*C(2)*C(11)*C(11)
      RB(24) = RB(24)*C(5)
      RB(25) = RB(25)*C(12)
      RB(26) = RB(26)*C(3)*C(12)
      RB(27) = RB(27)*C(7)
      RB(28) = RB(28)*CTB(28)*C(7)
      RB(29) = RB(29)*C(7)*C(4)
      RB(30) = RB(30)*C(7)*C(6)
      RB(31) = RB(31)*C(7)*C(19)
      RB(32) = RB(32)*C(3)*C(5)
      RB(33) = RB(33)*CTB(33)*C(1)
      RB(34) = RB(34)*C(1)*C(1)
      RB(35) = RB(35)*C(1)*C(6)
      RB(36) = RB(36)*C(1)*C(12)
      RB(37) = RB(37)*CTB(37)*C(6)
      RB(38) = RB(38)*C(3)*C(6)
      RB(39) = RB(39)*C(4)*C(1)
      RB(40) = RB(40)*C(5)*C(5)
      RB(41) = RB(41)*C(7)*C(1)
      RB(42) = RB(42)*C(5)*C(6)
      RB(43) = RB(43)*C(1)
      RB(44) = RB(44)*C(9)
      RB(45) = RB(45)*C(1)
      RB(46) = RB(46)*C(10)
      RB(47) = RB(47)*C(9)*C(1)
      RB(48) = RB(48)*C(13)
      RB(49) = RB(49)*C(1)*C(11)
      RB(51) = RB(51)*C(1)
      RB(52) = RB(52)*C(1)*C(13)
      RB(53) = RB(53)*C(5)*C(9)
      RB(54) = RB(54)*C(6)
      RB(56) = RB(56)*C(15)
      RB(57) = RB(57)*C(1)*C(14)
      RB(59) = RB(59)*C(1)
      RB(60) = RB(60)*C(16)
      RB(61) = RB(61)*C(1)*C(15)
      RB(62) = RB(62)*C(1)
      RB(63) = RB(63)*C(11)
      RB(64) = RB(64)*C(1)
      RB(65) = RB(65)*C(9)*C(11)
      RB(66) = RB(66)*C(13)
      RB(67) = RB(67)*C(2)*C(6)
      RB(68) = RB(68)*C(8)
      RB(69) = RB(69)*C(3)*C(6)
      RB(70) = RB(70)*C(4)*C(6)
      RB(71) = RB(71)*C(7)*C(6)
      RB(72) = RB(72)*C(7)*C(6)
      RB(73) = RB(73)*C(2)*C(11)
      RB(74) = RB(74)*C(2)
      RB(75) = RB(75)*C(2)*C(13)
      RB(76) = RB(76)*C(6)
      RB(77) = RB(77)*C(2)*C(13)
      RB(78) = RB(78)*C(6)
      RB(79) = RB(79)*C(6)
      RB(80) = RB(80)*C(9)*C(6)
      RB(81) = RB(81)*C(2)*C(12)
      RB(82) = RB(82)*C(6)*C(11)
      RB(83) = RB(83)*C(6)
      RB(84) = RB(84)*C(6)*C(13)
      RB(85) = RB(85)*C(2)*C(17)
      RB(86) = RB(86)*C(9)*C(11)
      RB(87) = RB(87)*C(6)*C(14)
      RB(88) = RB(88)*C(6)
      RB(89) = RB(89)*C(6)
      RB(90) = RB(90)*C(6)
      RB(91) = RB(91)*C(4)*C(8)
      RB(92) = RB(92)*C(4)*C(8)
      RB(93) = RB(93)*C(5)*C(13)
      RB(94) = RB(94)*C(4)*C(10)
      RB(95) = RB(95)*C(5)
      RB(96) = RB(96)*C(5)*C(12)
      RB(97) = RB(97)*C(8)
      RB(98) = RB(98)*C(3)*C(11)
      RB(99) = RB(99)*C(2)*C(14)
      RB(100) = RB(100)*C(3)
      RB(101) = RB(101)*C(2)
      RB(102) = RB(102)*C(2)*C(13)
      RB(103) = RB(103)*C(2)*C(14)
      RB(105) = RB(105)*C(2)*C(15)
      RB(107) = RB(107)*C(11)
      RB(108) = RB(108)*C(2)*C(17)
      RB(109) = RB(109)*C(11)*C(14)
      RB(111) = RB(111)*C(2)*C(9)
      RB(112) = RB(112)*C(1)*C(14)
      RB(113) = RB(113)*C(2)*C(15)
      RB(114) = RB(114)*C(9)*C(9)
      RB(115) = RB(115)*C(17)
      RB(117) = RB(117)*C(19)
      RB(118) = RB(118)*C(2)*C(5)*C(11)
      RB(119) = RB(119)*C(11)*C(6)
      RB(120) = RB(120)*C(9)*C(2)
      RB(121) = RB(121)*C(6)
      RB(122) = RB(122)*C(2)*C(15)
      RB(123) = RB(123)*C(9)*C(9)
      RB(124) = RB(124)*C(11)
      RB(125) = RB(125)*C(12)
      RB(126) = RB(126)*C(11)*C(13)
      RB(128) = RB(128)*C(3)
      RB(129) = RB(129)*C(5)*C(13)
      RB(130) = RB(130)*C(7)*C(10)
      RB(131) = RB(131)*C(16)
      RB(132) = RB(132)*C(2)
      RB(133) = RB(133)*C(10)*C(11)
      RB(134) = RB(134)*C(10)
      RB(135) = RB(135)*C(10)
      RB(136) = RB(136)*C(10)
      RB(137) = RB(137)*C(2)*C(11)*C(6)
      RB(138) = RB(138)*CTB(138)*C(2)*C(11)
      RB(139) = RB(139)*C(7)*C(11)
      RB(140) = RB(140)*C(7)*C(13)
      RB(142) = RB(142)*C(1)*C(14)
      RB(143) = RB(143)*C(7)*C(15)
      RB(144) = RB(144)*C(5)*C(11)*C(11)
      RB(145) = RB(145)*C(11)*C(11)*C(14)
      RB(147) = RB(147)*C(2)
      RB(148) = RB(148)*C(4)*C(6)
      RB(150) = RB(150)*C(9)
      RB(152) = RB(152)*C(3)*C(13)
      RB(156) = RB(156)*C(7)*C(14)
      RB(161) = RB(161)*C(9)
      RB(162) = RB(162)*C(17)*C(1)
      RB(163) = RB(163)*C(6)*C(17)
      RB(164) = RB(164)*C(15)*C(9)
      RB(165) = RB(165)*C(17)*C(9)*C(2)
      RB(167) = RB(167)*C(18)
C
      RETURN
      END

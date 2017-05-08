C $Id:$
C----------------------------------------------------------------------C
C V1.3 Nathan Wichmann made further improvements
C V1.2 Ramanan fixed few bugs Apr08
C V1.1 John Levesque vectorized the code Feb08
C V1.0 was from Sandia
C----------------------------------------------------------------------C
C     March 09, 2007
C
C     18-step reduced mechanism for ehtylene-air
C
C     By Tianfeng Lu
C     Princeton University
C     Email: tlu@princeton.edu
C----------------------------------------------------------------------C
C                                                                      C
CDOL      SUBROUTINE CKWYP  (P, T, Y, ICKWRK, RCKWRK, WDOT)
      SUBROUTINE GETRATES_I  (P, T, VL, Y,ICKWRK, RCKWRK, WDOT)
        USE chemkin_m, only : MAXVL
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C
      PARAMETER (IREAC=206,KK=22,KSS=10,KTOTAL=32)
      DIMENSION Y(MAXVL,*), ICKWRK(*), RCKWRK(*), WDOT(MAXVL,*)
      DIMENSION C(MAXVL,22), RF(MAXVL,IREAC), RB(MAXVL,IREAC)
      DIMENSION RKLOW(MAXVL,21), XQ(MAXVL,KSS)
      DIMENSION TI(MAXVL),TI2(MAXVL)
      DIMENSION P(MAXVL),T(MAXVL),SUM(MAXVL)
      INTEGER VL
      DATA SMALL/1.D-50/
C
      DO I=1,VL
      C(I,1) = Y(I,1)*4.96046521D-1
      C(I,2) = Y(I,2)*9.92093043D-1
      C(I,3) = Y(I,3)*6.25023433D-2
      C(I,4) = Y(I,4)*3.12511716D-2
      C(I,5) = Y(I,5)*5.87980383D-2
      C(I,6) = Y(I,6)*5.55082499D-2
      C(I,7) = Y(I,7)*3.02968146D-2
      C(I,8) = Y(I,8)*2.93990192D-2
      C(I,9) = Y(I,9)*6.65112065D-2
      C(I,10) = Y(I,10)*6.23323639D-2
      C(I,11) = Y(I,11)*3.57008335D-2
      C(I,12) = Y(I,12)*2.27221341D-2
      C(I,13) = Y(I,13)*3.33039255D-2
      C(I,14) = Y(I,14)*3.84050525D-2
      C(I,15) = Y(I,15)*3.56453112D-2
      C(I,16) = Y(I,16)*3.32556033D-2
      C(I,17) = Y(I,17)*2.4372606D-2
      C(I,18) = Y(I,18)*2.37882046D-2
      C(I,19) = Y(I,19)*2.26996304D-2
      C(I,20) = Y(I,20)*2.43467162D-2
      C(I,21) = Y(I,21)*2.37635408D-2
      C(I,22) = Y(I,22)*3.56972032D-2
      ENDDO
C
      SUM(:VL) = 0.0
      DO K = 1, 22
      DO I=1,VL
         SUM(I) = SUM(I) + C(I,K)
      ENDDO
      ENDDO

      SUM(:VL) = P(:VL)/(SUM(:VL)*T(:VL)*8.314510D7)
C
      DO K = 1, 22
      DO I=1,VL
         C(I,K) = MAX(C(I,K), SMALL) * SUM(I)
      ENDDO
      ENDDO
C
      CALL RATT_I(VL,T, RF, RB, RKLOW)
      CALL RATX_I(VL,T, C, RF, RB, RKLOW)
      CALL QSSA_I(VL,RF, RB, XQ)
      CALL RDWDOT_I(VL,RF, RB, WDOT)
C
      RETURN
      END
C
C----------------------------------------------------------------------C
C
      SUBROUTINE RATT_I (VL,T, RF, RB, RKLOW)
        USE chemkin_m, only : MAXVL
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C
      PARAMETER (RU=8.314510D7, RUC=RU/4.184D7, PATM=1.01325D6)
      DIMENSION RF(MAXVL,*), RB(MAXVL,*), RKLOW(MAXVL,*)
      DIMENSION SMH(MAXVL,32), EG(MAXVL,32)
      DIMENSION EQK(MAXVL,206)
      DIMENSION T(MAXVL),ALOGT(MAXVL),TI(MAXVL)
      DIMENSION TI2(MAXVL),TMP(MAXVL)
      INTEGER VL
      DATA SMALL/1.D-300/
C
      call vrda_log(VL,t,alogt)
      TI = 1.0D0/T
      TI2 = TI*TI
C
      RF(:VL,1) = (3.20498617D1 -7.25286183D3*TI)
      call vrda_exp(VL,RF(1,1),RF(1,1))
      RF(:VL,2) = (1.08197783D1 +2.67D0*ALOGT -3.16523284D3*TI)
      call vrda_exp(VL,RF(1,2),RF(1,2))
      RF(:VL,3) = (1.9190789D1 +1.51D0*ALOGT -1.72603317D3*TI)
      call vrda_exp(VL,RF(1,3),RF(1,3))
      RF(:VL,4) = (1.0482906D1 +2.4D0*ALOGT +1.06178717D3*TI)
      call vrda_exp(VL,RF(1,4),RF(1,4))
      RF(:VL,5) = 1.D18*TI
      RF(:VL,6) = (3.90385861D1 -6.D-1*ALOGT)
      call vrda_exp(VL,RF(1,6),RF(1,6))
      RF(:VL,7) = (4.55408762D1 -1.25D0*ALOGT)
      call vrda_exp(VL,RF(1,7),RF(1,7))
      RF(:VL,8) = 5.5D20*TI2
      RF(:VL,9) = 2.2D22*TI2
      RF(:VL,10) = 5.D17*TI
      RF(:VL,11) = 1.2D17*TI
      RF(:VL,12) = (4.24761511D1 -8.6D-1*ALOGT)
      call vrda_exp(VL,RF(1,12),RF(1,12))
      RF(:VL,13) = (4.71503141D1 -1.72D0*ALOGT)
      call vrda_exp(VL,RF(1,13),RF(1,13))
      RF(:VL,14) = (4.42511034D1 -7.6D-1*ALOGT)
      call vrda_exp(VL,RF(1,14),RF(1,14))
      RF(:VL,15) = (4.47046282D1 -1.24D0*ALOGT)
      call vrda_exp(VL,RF(1,15),RF(1,15))
      RF(:VL,16) = (3.19350862D1 -3.7D-1*ALOGT)
      call vrda_exp(VL,RF(1,16),RF(1,16))
      RF(:VL,17) = (2.90097872D1 -3.37658384D2*TI)
      call vrda_exp(VL,RF(1,17),RF(1,17))
      RF(:VL,18) = (3.04404238D1 -4.12637667D2*TI)
      call vrda_exp(VL,RF(1,18),RF(1,18))
      RF(:VL,19) = (3.18908801D1 -1.50965D2*TI)
      call vrda_exp(VL,RF(1,19),RF(1,19))
      RF(:VL,20) = 2.D13
      RF(:VL,21) = (3.14683206D1 +2.51608334D2*TI)
      call vrda_exp(VL,RF(1,21),RF(1,21))
      RF(:VL,22) = (2.55908003D1 +8.20243168D2*TI)
      call vrda_exp(VL,RF(1,22),RF(1,22))
      RF(:VL,23) = (3.36712758D1 -6.03860001D3*TI)
      call vrda_exp(VL,RF(1,23),RF(1,23))
      RF(:VL,24) = (1.6308716D1 +2.D0*ALOGT -2.61672667D3*TI)
      call vrda_exp(VL,RF(1,24),RF(1,24))
      RF(:VL,25) = (2.99336062D1 -1.81158D3*TI)
      call vrda_exp(VL,RF(1,25),RF(1,25))
      RF(:VL,26) = (1.60803938D1 +2.D0*ALOGT -2.01286667D3*TI)
      call vrda_exp(VL,RF(1,26),RF(1,26))
      RF(:VL,27) = (2.81906369D1 -1.61029334D2*TI)
      call vrda_exp(VL,RF(1,27),RF(1,27))
      RF(:VL,28) = (3.39940492D1 -4.81075134D3*TI)
      call vrda_exp(VL,RF(1,28),RF(1,28))
      RF(:VL,29) = (3.40312786D1 -1.50965D3*TI)
      call vrda_exp(VL,RF(1,29),RF(1,29))
      RF(:VL,30) = (1.76783433D1 +1.228D0*ALOGT -3.52251667D1*TI)
      call vrda_exp(VL,RF(1,30),RF(1,30))
      RF(:VL,31) = (1.75767107D1 +1.5D0*ALOGT -4.00560467D4*TI)
      call vrda_exp(VL,RF(1,31),RF(1,31))
      RF(:VL,32) = (2.85473118D1 -2.40537567D4*TI)
      call vrda_exp(VL,RF(1,32),RF(1,32))
      RF(:VL,33) = (3.26416564D1 -1.18759134D4*TI)
      call vrda_exp(VL,RF(1,33),RF(1,33))
      RF(:VL,34) = 5.7D13
      RF(:VL,35) = 3.D13
      RF(:VL,36) = (1.85223344D1 +1.79D0*ALOGT -8.40371835D2*TI)
      call vrda_exp(VL,RF(1,36),RF(1,36))
      RF(:VL,37) = (2.93732401D1 +3.79928584D2*TI)
      call vrda_exp(VL,RF(1,37),RF(1,37))
      RF(:VL,38) = 3.3D13
      RF(:VL,39) = 5.D13
      RF(:VL,40) = (2.88547965D1 -3.47219501D2*TI)
      call vrda_exp(VL,RF(1,40),RF(1,40))
      RF(:VL,41) = (2.77171988D1 +4.8D-1*ALOGT +1.30836334D2*TI)
      call vrda_exp(VL,RF(1,41),RF(1,41))
      RF(:VL,42) = 7.34D13
      RF(:VL,43) = 3.D13
      RF(:VL,44) = 3.D13
      RF(:VL,45) = 5.D13
      RF(:VL,46) = (3.9769885D1 -1.D0*ALOGT -8.55468335D3*TI)
      call vrda_exp(VL,RF(1,46),RF(1,46))
      RF(:VL,47) = (2.96591694D1 -2.01286667D2*TI)
      call vrda_exp(VL,RF(1,47),RF(1,47))
      RF(:VL,48) = (3.77576522D1 -8.D-1*ALOGT)
      call vrda_exp(VL,RF(1,48),RF(1,48))
      RF(:VL,49) = (1.31223634D1 +2.D0*ALOGT -3.63825651D3*TI)
      call vrda_exp(VL,RF(1,49),RF(1,49))
      RF(:VL,50) = 8.D13
      TMP = (-7.54825001D2*TI)
      call vrda_exp(VL,TMP,TMP)
      RF(:VL,51) = 1.056D13 * TMP
      RF(:VL,52) = 2.64D12 * TMP
      RF(:VL,53) = 2.D13
      RF(:VL,54) = (1.62403133D1 +2.D0*ALOGT -1.50965D3*TI)
      call vrda_exp(VL,RF(1,54),RF(1,54))
      RF(:VL,55) = 2.D13
      RF(:VL,56) = (2.74203001D1 +5.D-1*ALOGT -2.26950717D3*TI)
      call vrda_exp(VL,RF(1,56),RF(1,56))
      RF(:VL,57) = 4.D13
      RF(:VL,58) = 3.2D13
      RF(:VL,59) = (3.03390713D1 -3.01930001D2*TI)
      call vrda_exp(VL,RF(1,59),RF(1,59))
      RF(:VL,60) = 3.D13
      RF(:VL,61) = 1.5D13
      RF(:VL,62) = 1.5D13
      RF(:VL,63) = 3.D13
      RF(:VL,64) = 7.D13
      RF(:VL,65) = 2.8D13
      RF(:VL,66) = 1.2D13
      RF(:VL,67) = 3.D13
      RF(:VL,68) = 9.D12
      RF(:VL,69) = 7.D12
      RF(:VL,70) = 1.4D13
      RF(:VL,71) = (2.7014835D1 +4.54D-1*ALOGT -1.30836334D3*TI)
      call vrda_exp(VL,RF(1,71),RF(1,71))
      RF(:VL,72) = (2.38587601D1 +1.05D0*ALOGT -1.64803459D3*TI)
      call vrda_exp(VL,RF(1,72),RF(1,72))
      RF(:VL,73) = (3.12945828D1 -1.781387D3*TI)
      call vrda_exp(VL,RF(1,73),RF(1,73))
      RF(:VL,74) = (2.19558261D1 +1.18D0*ALOGT +2.2493785D2*TI)
      call vrda_exp(VL,RF(1,74),RF(1,74))
      RF(:VL,75) = (3.22361913D1 -2.01286667D4*TI)
      call vrda_exp(VL,RF(1,75),RF(1,75))
      TMP = (-4.02573334D3*TI)
      call vrda_exp(VL,TMP,TMP)
      RF(:VL,76) = 1.D12 * TMP
      RF(:VL,127) = 5.D13 * TMP
      RF(:VL,129) = 1.D13 * TMP
      RF(:VL,77) = (3.21806786D1 +2.59156584D2*TI)
      call vrda_exp(VL,RF(1,77),RF(1,77))
      RF(:VL,78) = (3.70803784D1 -6.3D-1*ALOGT -1.92731984D2*TI)
      call vrda_exp(VL,RF(1,78),RF(1,78))
      RF(:VL,79) = 8.43D13
      RF(:VL,80) = (1.78408622D1 +1.6D0*ALOGT -2.72743434D3*TI)
      call vrda_exp(VL,RF(1,80),RF(1,80))
      RF(:VL,81) = 2.501D13
      RF(:VL,82) = (3.10595094D1 -1.449264D4*TI)
      call vrda_exp(VL,RF(1,82),RF(1,82))
      RF(:VL,83) = (2.43067848D1 -4.49875701D3*TI)
      call vrda_exp(VL,RF(1,83),RF(1,83))
      RF(:VL,84) = 1.D12
      RF(:VL,85) = 1.34D13
      RF(:VL,86) = (1.01064284D1 +2.47D0*ALOGT -2.60666234D3*TI)
      call vrda_exp(VL,RF(1,86),RF(1,86))
      RF(:VL,87) = 3.D13
      RF(:VL,88) = 8.48D12
      RF(:VL,89) = 1.8D13
      RF(:VL,90) = (8.10772006D0 +2.81D0*ALOGT -2.94884967D3*TI)
      call vrda_exp(VL,RF(1,90),RF(1,90))
      RF(:VL,91) = 4.D13
      TMP = (2.86833501D2*TI)
      call vrda_exp(VL,TMP,TMP)
      RF(:VL,92) = 1.2D13 * TMP
      RF(:VL,107) = 1.6D13 * TMP
      RF(:VL,93) = (3.75927776D1 -9.7D-1*ALOGT -3.11994334D2*TI)
      call vrda_exp(VL,RF(1,93),RF(1,93))
      RF(:VL,94) = (2.9238457D1 +1.D-1*ALOGT -5.33409668D3*TI)
      call vrda_exp(VL,RF(1,94),RF(1,94))
      RF(:VL,95) = 5.D13
      RF(:VL,96) = 2.D13
      RF(:VL,97) = 3.2D13
      RF(:VL,98) = 1.6D13
      RF(:VL,99) = 1.D13
      RF(:VL,100) = 5.D12
      RF(:VL,101) = (-2.84796532D1 +7.6D0*ALOGT +1.77635484D3*TI)
      call vrda_exp(VL,RF(1,101),RF(1,101))
      RF(:VL,102) = (2.03077504D1 +1.62D0*ALOGT -5.45486868D3*TI)
      call vrda_exp(VL,RF(1,102),RF(1,102))
      RF(:VL,103) = (2.07430685D1 +1.5D0*ALOGT -4.32766334D3*TI)
      call vrda_exp(VL,RF(1,103),RF(1,103))
      RF(:VL,104) = (1.84206807D1 +1.6D0*ALOGT -1.570036D3*TI)
      call vrda_exp(VL,RF(1,104),RF(1,104))
      RF(:VL,105) = 6.D13
      RF(:VL,106) = (1.47156719D1 +2.D0*ALOGT -4.16160184D3*TI)
      call vrda_exp(VL,RF(1,106),RF(1,106))
      RF(:VL,108) = 1.D14
      RF(:VL,109) = 1.D14
      RF(:VL,110) = (2.81010247D1 -4.29747034D2*TI)
      call vrda_exp(VL,RF(1,110),RF(1,110))
      RF(:VL,111) = 5.D13
      RF(:VL,112) = 3.D13
      RF(:VL,113) = 1.D13
      RF(:VL,114) = (3.43156328D1 -5.2D-1*ALOGT -2.55382459D4*TI)
      call vrda_exp(VL,RF(1,114),RF(1,114))
      RF(:VL,115) = (1.97713479D1 +1.62D0*ALOGT -1.86432818D4*TI)
      call vrda_exp(VL,RF(1,115),RF(1,115))
      TMP = (2.D0*ALOGT -9.56111669D2*TI )
      call vrda_exp(VL,TMP,TMP)
      RF(:VL,116) = 1.632D7 * TMP
      RF(:VL,117) = 4.08D6 * TMP
      RF(:VL,118) = (-8.4310155D0 +4.5D0*ALOGT +5.03216668D2*TI)
      call vrda_exp(VL,RF(1,118),RF(1,118))
      RF(:VL,119) = (-7.6354939D0 +4.D0*ALOGT +1.00643334D3*TI)
      call vrda_exp(VL,RF(1,119),RF(1,119))
      RF(:VL,120) = (1.61180957D1 +2.D0*ALOGT -3.01930001D3*TI)
      call vrda_exp(VL,RF(1,120),RF(1,120))
      RF(:VL,121) = (1.27430637D2 -1.182D1*ALOGT -1.79799315D4*TI)
      call vrda_exp(VL,RF(1,121),RF(1,121))
      RF(:VL,122) = 1.D14
      RF(:VL,123) = 1.D14
      RF(:VL,124) = 2.D13
      RF(:VL,125) = 1.D13
      RF(:VL,126) = (3.34301138D1 -6.D-2*ALOGT -4.27734167D3*TI)
      call vrda_exp(VL,RF(1,126),RF(1,126))
      RF(:VL,128) = (2.11287309D1 +1.43D0*ALOGT -1.35365284D3*TI)
      call vrda_exp(VL,RF(1,128),RF(1,128))
      RF(:VL,130) = (2.81906369D1 -6.79342501D2*TI)
      call vrda_exp(VL,RF(1,130),RF(1,130))
      TMP = (-1.00643334D3*TI)
      call vrda_exp(VL,TMP,TMP)
      RF(:VL,131) = 7.5D12 * TMP
      RF(:VL,152) = 1.D13 * TMP
      RF(:VL,186) = 2.D13 * TMP
      RF(:VL,132) = (2.94360258D1 +2.7D-1*ALOGT -1.40900667D2*TI)
      call vrda_exp(VL,RF(1,132),RF(1,132))
      RF(:VL,133) = 3.D13
      RF(:VL,134) = 6.D13
      RF(:VL,135) = 4.8D13
      RF(:VL,136) = 4.8D13
      RF(:VL,137) = 3.011D13
      RF(:VL,138) = (1.41081802D1 +1.61D0*ALOGT +1.9293327D2*TI)
      call vrda_exp(VL,RF(1,138),RF(1,138))
      RF(:VL,139) = (2.64270483D1 +2.9D-1*ALOGT -5.53538334D0*TI)
      call vrda_exp(VL,RF(1,139),RF(1,139))
      RF(:VL,140) = (3.83674178D1 -1.39D0*ALOGT -5.08248834D2*TI)
      call vrda_exp(VL,RF(1,140),RF(1,140))
      RF(:VL,141) = 1.D13
      RF(:VL,142) = (2.32164713D1 +2.99917134D2*TI)
      call vrda_exp(VL,RF(1,142),RF(1,142))
      RF(:VL,143) = 9.033D13
      RF(:VL,144) = 3.92D11
      RF(:VL,145) = 2.5D13
      RF(:VL,146) = (5.56675073D1 -2.83D0*ALOGT -9.36888792D3*TI)
      call vrda_exp(VL,RF(1,146),RF(1,146))
      RF(:VL,147) = (9.64601125D1 -9.147D0*ALOGT -2.36008617D4*TI)
      call vrda_exp(VL,RF(1,147),RF(1,147))
      RF(:VL,148) = 1.D14
      RF(:VL,149) = 9.D13
      TMP = (-2.01286667D3*TI)
      call vrda_exp(VL,TMP,TMP)
      RF(:VL,150) = 2.D13 * TMP
      RF(:VL,151) = 2.D13 * TMP
      RF(:VL,153) = 1.4D11
      RF(:VL,154) = 1.8D10
      RF(:VL,155) = (2.97104627D1 +4.4D-1*ALOGT -4.46705436D4*TI)
      call vrda_exp(VL,RF(1,155),RF(1,155))
      RF(:VL,156) = (2.77079822D1 +4.54D-1*ALOGT -9.15854335D2*TI)
      call vrda_exp(VL,RF(1,156),RF(1,156))
      RF(:VL,157) = (1.77414365D1 +1.93D0*ALOGT -6.51665585D3*TI)
      call vrda_exp(VL,RF(1,157),RF(1,157))
      RF(:VL,158) = (1.65302053D1 +1.91D0*ALOGT -1.88203034D3*TI)
      call vrda_exp(VL,RF(1,158),RF(1,158))
      TMP = (1.83D0*ALOGT -1.10707667D2*TI )
      call vrda_exp(VL,TMP,TMP)
      RF(:VL,159) = 1.92D7 * TMP
      RF(:VL,160) = 3.84D5 * TMP
      RF(:VL,161) = (1.50964444D1 +2.D0*ALOGT -1.25804167D3*TI)
      call vrda_exp(VL,RF(1,161),RF(1,161))
      RF(:VL,162) = (3.13734413D1 -3.05955734D4*TI)
      call vrda_exp(VL,RF(1,162),RF(1,162))
      RF(:VL,163) = (2.83241683D1 -7.04503335D3*TI)
      call vrda_exp(VL,RF(1,163),RF(1,163))
      RF(:VL,164) = (1.61180957D1 +2.D0*ALOGT -4.02573334D3*TI)
      call vrda_exp(VL,RF(1,164),RF(1,164))
      RF(:VL,165) = (3.06267534D1 -3.01930001D3*TI)
      call vrda_exp(VL,RF(1,165),RF(1,165))
      RF(:VL,166) = 5.D13
      RF(:VL,167) = 5.D13
      RF(:VL,168) = (1.23327053D1 +2.D0*ALOGT -4.62959334D3*TI)
      call vrda_exp(VL,RF(1,168),RF(1,168))
      RF(:VL,169) = (2.65223585D1 -3.87476834D3*TI)
      call vrda_exp(VL,RF(1,169),RF(1,169))
      RF(:VL,170) = (4.07945264D1 -9.9D-1*ALOGT -7.95082335D2*TI)
      call vrda_exp(VL,RF(1,170),RF(1,170))
      RF(:VL,171) = 2.D12
      RF(:VL,172) = 1.604D13
      RF(:VL,173) = 8.02D13
      RF(:VL,174) = 2.D10
      RF(:VL,175) = 3.D11
      RF(:VL,176) = 3.D11
      RF(:VL,177) = 2.4D13
      RF(:VL,178) = (2.28865889D1 -4.90133034D2*TI)
      call vrda_exp(VL,RF(1,178),RF(1,178))
      RF(:VL,179) = 1.2D14
      RF(:VL,180) = (1.85604427D1 +1.9D0*ALOGT -3.78922151D3*TI)
      call vrda_exp(VL,RF(1,180),RF(1,180))
      RF(:VL,181) = (1.83130955D1 +1.92D0*ALOGT -2.86330284D3*TI)
      call vrda_exp(VL,RF(1,181),RF(1,181))
      RF(:VL,182) = (1.50796373D1 +2.12D0*ALOGT -4.37798501D2*TI)
      call vrda_exp(VL,RF(1,182),RF(1,182))
      RF(:VL,183) = (3.13199006D1 +2.76769167D2*TI)
      call vrda_exp(VL,RF(1,183),RF(1,183))
      RF(:VL,184) = (1.56303353D1 +1.74D0*ALOGT -5.25861418D3*TI)
      call vrda_exp(VL,RF(1,184),RF(1,184))
      RF(:VL,185) = 2.D14
      RF(:VL,187) = 2.66D12
      RF(:VL,188) = 6.6D12
      RF(:VL,189) = 6.D13
      RF(:VL,190) = (3.02187852D1 -1.64083859D3*TI)
      call vrda_exp(VL,RF(1,190),RF(1,190))
      RF(:VL,191) = (5.11268757D1 -2.39D0*ALOGT -5.62596234D3*TI)
      call vrda_exp(VL,RF(1,191),RF(1,191))
      RF(:VL,192) = (1.20435537D1 +2.5D0*ALOGT -1.2530095D3*TI)
      call vrda_exp(VL,RF(1,192),RF(1,192))
      RF(:VL,193) = (1.86030023D1 +1.65D0*ALOGT -1.6455185D2*TI)
      call vrda_exp(VL,RF(1,193),RF(1,193))
      RF(:VL,194) = (1.73708586D1 +1.65D0*ALOGT +4.89126601D2*TI)
      call vrda_exp(VL,RF(1,194),RF(1,194))
      RF(:VL,195) = (2.59162227D1 +7.D-1*ALOGT -2.95891401D3*TI)
      call vrda_exp(VL,RF(1,195),RF(1,195))
      RF(:VL,196) = (1.49469127D1 +2.D0*ALOGT +1.49958567D2*TI)
      call vrda_exp(VL,RF(1,196),RF(1,196))
      RF(:VL,197) = (9.16951838D0 +2.6D0*ALOGT -6.99974385D3*TI)
      call vrda_exp(VL,RF(1,197),RF(1,197))
      RF(:VL,198) = (7.8845736D-1 +3.5D0*ALOGT -2.85575459D3*TI)
      call vrda_exp(VL,RF(1,198),RF(1,198))
      RF(:VL,199) = (5.65703751D1 -2.92D0*ALOGT -6.29272443D3*TI)
      call vrda_exp(VL,RF(1,199),RF(1,199))
      RF(:VL,200) = 1.8D12
      RF(:VL,201) = 9.6D13
      RF(:VL,202) = 2.4D13
      RF(:VL,203) = 9.D10
      RF(:VL,204) = 2.4D13
      RF(:VL,205) = 1.1D13
      RF(:VL,206) = (7.50436995D1 -5.22D0*ALOGT -9.93701954D3*TI)
      call vrda_exp(VL,RF(1,206),RF(1,206))
C
      CALL RDSMH_I (VL,T, SMH)
      if(VL==MAXVL)then
      CALL VRDA_exp(31*MAXVL,SMH(1,1),EG(1,1))
C          EG(N) = EXP(SMH(N))
      else
        DO N = 1, 31
        CALL VRDA_exp(VL,SMH(1,N),EG(1,N))
C          EG(N) = EXP(SMH(N))
        ENDDO
      endif
C
      DO I=1,VL
      PFAC = PATM / (RU*T(I))
      PFAC2 = PFAC*PFAC
      PFAC3 = PFAC2*PFAC
C
      EQK(I,1)=EG(I,3)*EG(I,5)/EG(I,2)/EG(I,4)
      EQK(I,2)=EG(I,2)*EG(I,5)/EG(I,1)/EG(I,3)
      EQK(I,3)=EG(I,2)*EG(I,6)/EG(I,1)/EG(I,5)
      EQK(I,4)=EG(I,3)*EG(I,6)/EG(I,5)/EG(I,5)
      EQK(I,5)=EG(I,1)/EG(I,2)/EG(I,2)/PFAC
      EQK(I,6)=EQK(I,5)
      EQK(I,7)=EQK(I,5)
      EQK(I,8)=EQK(I,5)
      EQK(I,9)=EG(I,6)/EG(I,2)/EG(I,5)/PFAC
      EQK(I,10)=EG(I,5)/EG(I,2)/EG(I,3)/PFAC
      EQK(I,11)=EG(I,4)/EG(I,3)/EG(I,3)/PFAC
      EQK(I,12)=EG(I,7)/EG(I,2)/EG(I,4)/PFAC
      EQK(I,13)=EQK(I,12)
      EQK(I,14)=EQK(I,12)
      EQK(I,15)=EQK(I,12)
      EQK(I,16)=EG(I,8)/EG(I,5)/EG(I,5)/PFAC
      EQK(I,17)=EG(I,3)*EG(I,6)/EG(I,2)/EG(I,7)
      EQK(I,18)=EG(I,1)*EG(I,4)/EG(I,2)/EG(I,7)
      EQK(I,19)=EG(I,5)*EG(I,5)/EG(I,2)/EG(I,7)
      EQK(I,20)=EG(I,4)*EG(I,5)/EG(I,3)/EG(I,7)
      EQK(I,21)=EG(I,4)*EG(I,6)/EG(I,5)/EG(I,7)
      EQK(I,22)=EG(I,4)*EG(I,8)/EG(I,7)/EG(I,7)
      EQK(I,23)=EQK(I,22)
      EQK(I,24)=EG(I,1)*EG(I,7)/EG(I,2)/EG(I,8)
      EQK(I,25)=EG(I,5)*EG(I,6)/EG(I,2)/EG(I,8)
      EQK(I,26)=EG(I,5)*EG(I,7)/EG(I,3)/EG(I,8)
      EQK(I,27)=EG(I,6)*EG(I,7)/EG(I,5)/EG(I,8)
      EQK(I,28)=EQK(I,27)
      EQK(I,29)=EG(I,15)/EG(I,3)/EG(I,14)/PFAC
      EQK(I,30)=EG(I,2)*EG(I,15)/EG(I,5)/EG(I,14)
      EQK(I,31)=EG(I,17)/EG(I,1)/EG(I,14)/PFAC
      EQK(I,32)=EG(I,3)*EG(I,15)/EG(I,4)/EG(I,14)
      EQK(I,33)=EG(I,5)*EG(I,15)/EG(I,7)/EG(I,14)
      EQK(I,34)=EG(I,2)*EG(I,14)/EG(I,3)/EG(I,9)
      EQK(I,35)=EG(I,2)*EG(I,16)/EG(I,5)/EG(I,9)
      EQK(I,36)=EG(I,2)*EG(I,10)/EG(I,1)/EG(I,9)
      EQK(I,37)=EG(I,2)*EG(I,17)/EG(I,6)/EG(I,9)
      EQK(I,38)=EG(I,3)*EG(I,16)/EG(I,4)/EG(I,9)
      EQK(I,39)=EG(I,25)/EG(I,9)/EG(I,14)/PFAC
      EQK(I,40)=EG(I,14)*EG(I,16)/EG(I,9)/EG(I,15)
      EQK(I,41)=EG(I,17)/EG(I,2)/EG(I,16)/PFAC
      EQK(I,42)=EG(I,1)*EG(I,14)/EG(I,2)/EG(I,16)
      EQK(I,43)=EG(I,5)*EG(I,14)/EG(I,3)/EG(I,16)
      EQK(I,44)=EG(I,2)*EG(I,15)/EG(I,3)/EG(I,16)
      EQK(I,45)=EG(I,6)*EG(I,14)/EG(I,5)/EG(I,16)
      EQK(I,46)=EG(I,2)*EG(I,14)/EG(I,16)*PFAC
      EQK(I,47)=EG(I,7)*EG(I,14)/EG(I,4)/EG(I,16)
      EQK(I,48)=EG(I,12)/EG(I,2)/EG(I,10)/PFAC
      EQK(I,49)=EG(I,2)*EG(I,12)/EG(I,1)/EG(I,10)
      EQK(I,50)=EG(I,2)*EG(I,16)/EG(I,3)/EG(I,10)
      EQK(I,51)=EG(I,5)*EG(I,16)/EG(I,4)/EG(I,10)
      EQK(I,52)=EG(I,2)*EG(I,2)*EG(I,15)/EG(I,4)/EG(I,10)*PFAC
      EQK(I,53)=EG(I,2)*EG(I,17)/EG(I,5)/EG(I,10)
      EQK(I,54)=EG(I,6)*EG(I,9)/EG(I,5)/EG(I,10)
      EQK(I,55)=EG(I,5)*EG(I,17)/EG(I,7)/EG(I,10)
      EQK(I,56)=EG(I,26)/EG(I,10)/EG(I,14)/PFAC
      EQK(I,57)=EG(I,2)*EG(I,19)/EG(I,9)/EG(I,10)
      EQK(I,58)=EG(I,1)*EG(I,19)/EG(I,10)/EG(I,10)
      EQK(I,59)=EG(I,10)/EG(I,11)
      EQK(I,67)=EQK(I,59)
      EQK(I,68)=EQK(I,59)
      EQK(I,69)=EQK(I,59)
      EQK(I,60)=EG(I,1)*EG(I,9)/EG(I,2)/EG(I,11)
      EQK(I,61)=EG(I,1)*EG(I,14)/EG(I,3)/EG(I,11)
      EQK(I,62)=EG(I,2)*EG(I,16)/EG(I,3)/EG(I,11)
      EQK(I,63)=EG(I,2)*EG(I,17)/EG(I,5)/EG(I,11)
      EQK(I,64)=EG(I,2)*EG(I,12)/EG(I,1)/EG(I,11)
      EQK(I,65)=EG(I,2)*EG(I,5)*EG(I,14)/EG(I,4)/EG(I,11)*PFAC
      EQK(I,66)=EG(I,6)*EG(I,14)/EG(I,4)/EG(I,11)
      EQK(I,70)=EG(I,14)*EG(I,17)/EG(I,11)/EG(I,15)
      EQK(I,71)=EG(I,18)/EG(I,2)/EG(I,17)/PFAC
      EQK(I,72)=EG(I,1)*EG(I,16)/EG(I,2)/EG(I,17)
      EQK(I,73)=EG(I,5)*EG(I,16)/EG(I,3)/EG(I,17)
      EQK(I,74)=EG(I,6)*EG(I,16)/EG(I,5)/EG(I,17)
      EQK(I,75)=EG(I,7)*EG(I,16)/EG(I,4)/EG(I,17)
      EQK(I,76)=EG(I,8)*EG(I,16)/EG(I,7)/EG(I,17)
      EQK(I,77)=EG(I,2)*EG(I,26)/EG(I,9)/EG(I,17)
      EQK(I,78)=EG(I,13)/EG(I,2)/EG(I,12)/PFAC
      EQK(I,79)=EG(I,2)*EG(I,17)/EG(I,3)/EG(I,12)
      EQK(I,80)=EG(I,6)*EG(I,10)/EG(I,5)/EG(I,12)
      EQK(I,81)=EG(I,6)*EG(I,11)/EG(I,5)/EG(I,12)
      EQK(I,82)=EG(I,3)*EG(I,18)/EG(I,4)/EG(I,12)
      EQK(I,83)=EG(I,5)*EG(I,17)/EG(I,4)/EG(I,12)
      EQK(I,84)=EG(I,4)*EG(I,13)/EG(I,7)/EG(I,12)
      EQK(I,85)=EG(I,5)*EG(I,18)/EG(I,7)/EG(I,12)
      EQK(I,86)=EG(I,7)*EG(I,13)/EG(I,8)/EG(I,12)
      EQK(I,87)=EG(I,2)*EG(I,21)/EG(I,9)/EG(I,12)
      EQK(I,88)=EG(I,13)*EG(I,14)/EG(I,12)/EG(I,16)
      EQK(I,89)=EG(I,28)/EG(I,12)/EG(I,16)/PFAC
      EQK(I,90)=EG(I,13)*EG(I,16)/EG(I,12)/EG(I,17)
      EQK(I,91)=EG(I,2)*EG(I,22)/EG(I,10)/EG(I,12)
      EQK(I,92)=EG(I,2)*EG(I,22)/EG(I,11)/EG(I,12)
      EQK(I,93)=EG(I,24)/EG(I,12)/EG(I,12)/PFAC
      EQK(I,94)=EG(I,2)*EG(I,23)/EG(I,12)/EG(I,12)
      EQK(I,95)=EG(I,14)*EG(I,22)/EG(I,12)/EG(I,25)
      EQK(I,96)=EG(I,1)*EG(I,17)/EG(I,2)/EG(I,18)
      EQK(I,97)=EG(I,5)*EG(I,12)/EG(I,2)/EG(I,18)
      EQK(I,98)=EG(I,6)*EG(I,11)/EG(I,2)/EG(I,18)
      EQK(I,99)=EG(I,5)*EG(I,17)/EG(I,3)/EG(I,18)
      EQK(I,100)=EG(I,6)*EG(I,17)/EG(I,5)/EG(I,18)
      EQK(I,101)=EG(I,7)*EG(I,17)/EG(I,4)/EG(I,18)
      EQK(I,102)=EG(I,1)*EG(I,12)/EG(I,2)/EG(I,13)
      EQK(I,103)=EG(I,5)*EG(I,12)/EG(I,3)/EG(I,13)
      EQK(I,104)=EG(I,6)*EG(I,12)/EG(I,5)/EG(I,13)
      EQK(I,105)=EG(I,2)*EG(I,22)/EG(I,9)/EG(I,13)
      EQK(I,106)=EG(I,12)*EG(I,12)/EG(I,10)/EG(I,13)
      EQK(I,107)=EG(I,12)*EG(I,12)/EG(I,11)/EG(I,13)
      EQK(I,108)=EG(I,11)*EG(I,14)/EG(I,2)/EG(I,25)
      EQK(I,109)=EG(I,2)*EG(I,14)*EG(I,14)/EG(I,3)/EG(I,25)*PFAC
      EQK(I,110)=EG(I,5)*EG(I,14)*EG(I,14)/EG(I,4)/EG(I,25)*PFAC
      EQK(I,111)=EG(I,14)*EG(I,19)/EG(I,9)/EG(I,25)
      EQK(I,112)=EG(I,14)*EG(I,21)/EG(I,10)/EG(I,25)
      EQK(I,113)=EG(I,14)*EG(I,14)*EG(I,19)/EG(I,25)/EG(I,25)*PFAC
      EQK(I,114)=EG(I,20)/EG(I,19)
      EQK(I,122)=1.0/EQK(I,114)
      EQK(I,115)=EG(I,2)*EG(I,19)/EG(I,21)*PFAC
      EQK(I,116)=EG(I,2)*EG(I,25)/EG(I,3)/EG(I,19)
      EQK(I,117)=EG(I,10)*EG(I,14)/EG(I,3)/EG(I,19)
      EQK(I,118)=EG(I,2)*EG(I,26)/EG(I,5)/EG(I,19)
      EQK(I,119)=EG(I,12)*EG(I,14)/EG(I,5)/EG(I,19)
      EQK(I,120)=EG(I,14)*EG(I,21)/EG(I,16)/EG(I,19)
      EQK(I,121)=EG(I,29)/EG(I,12)/EG(I,19)/PFAC
      EQK(I,123)=EG(I,10)*EG(I,14)/EG(I,3)/EG(I,20)
      EQK(I,124)=EG(I,2)*EG(I,26)/EG(I,5)/EG(I,20)
      EQK(I,125)=EG(I,10)*EG(I,15)/EG(I,4)/EG(I,20)
      EQK(I,126)=EG(I,27)/EG(I,2)/EG(I,26)/PFAC
      EQK(I,127)=EG(I,1)*EG(I,25)/EG(I,2)/EG(I,26)
      EQK(I,128)=EG(I,12)*EG(I,14)/EG(I,2)/EG(I,26)
      EQK(I,129)=EG(I,5)*EG(I,25)/EG(I,3)/EG(I,26)
      EQK(I,130)=EG(I,10)*EG(I,15)/EG(I,3)/EG(I,26)
      EQK(I,131)=EG(I,6)*EG(I,25)/EG(I,5)/EG(I,26)
      EQK(I,132)=EG(I,22)/EG(I,2)/EG(I,21)/PFAC
      EQK(I,133)=EG(I,1)*EG(I,19)/EG(I,2)/EG(I,21)
      EQK(I,134)=EG(I,1)*EG(I,20)/EG(I,2)/EG(I,21)
      EQK(I,135)=EG(I,2)*EG(I,26)/EG(I,3)/EG(I,21)
      EQK(I,136)=EG(I,12)*EG(I,14)/EG(I,3)/EG(I,21)
      EQK(I,137)=EG(I,6)*EG(I,19)/EG(I,5)/EG(I,21)
      EQK(I,138)=EG(I,7)*EG(I,19)/EG(I,4)/EG(I,21)
      EQK(I,139)=EG(I,3)*EG(I,27)/EG(I,4)/EG(I,21)
      EQK(I,140)=EG(I,16)*EG(I,17)/EG(I,4)/EG(I,21)
      EQK(I,141)=EG(I,5)*EG(I,27)/EG(I,7)/EG(I,21)
      EQK(I,142)=EG(I,7)*EG(I,22)/EG(I,8)/EG(I,21)
      EQK(I,143)=EG(I,14)*EG(I,22)/EG(I,16)/EG(I,21)
      EQK(I,144)=EG(I,13)*EG(I,19)/EG(I,12)/EG(I,21)
      EQK(I,145)=EG(I,30)/EG(I,12)/EG(I,21)/PFAC
      EQK(I,146)=EG(I,2)*EG(I,29)/EG(I,12)/EG(I,21)
      EQK(I,147)=EG(I,12)*EG(I,14)/EG(I,27)*PFAC
      EQK(I,148)=EG(I,28)/EG(I,2)/EG(I,27)/PFAC
      EQK(I,149)=EG(I,12)*EG(I,16)/EG(I,2)/EG(I,27)
      EQK(I,150)=EG(I,1)*EG(I,26)/EG(I,2)/EG(I,27)
      EQK(I,151)=EG(I,5)*EG(I,26)/EG(I,3)/EG(I,27)
      EQK(I,152)=EG(I,6)*EG(I,26)/EG(I,5)/EG(I,27)
      EQK(I,153)=EG(I,7)*EG(I,26)/EG(I,4)/EG(I,27)
      EQK(I,154)=EG(I,5)*EG(I,14)*EG(I,17)/EG(I,4)/EG(I,27)*PFAC
      EQK(I,155)=EG(I,1)*EG(I,20)/EG(I,22)*PFAC
      EQK(I,156)=EG(I,23)/EG(I,2)/EG(I,22)/PFAC
      EQK(I,157)=EG(I,1)*EG(I,21)/EG(I,2)/EG(I,22)
      EQK(I,158)=EG(I,5)*EG(I,21)/EG(I,3)/EG(I,22)
      EQK(I,159)=EG(I,12)*EG(I,16)/EG(I,3)/EG(I,22)
      EQK(I,160)=EG(I,10)*EG(I,17)/EG(I,3)/EG(I,22)
      EQK(I,161)=EG(I,6)*EG(I,21)/EG(I,5)/EG(I,22)
      EQK(I,162)=EG(I,7)*EG(I,21)/EG(I,4)/EG(I,22)
      EQK(I,163)=EG(I,5)*EG(I,28)/EG(I,7)/EG(I,22)
      EQK(I,164)=EG(I,14)*EG(I,23)/EG(I,16)/EG(I,22)
      EQK(I,165)=EG(I,2)*EG(I,29)/EG(I,10)/EG(I,22)
      EQK(I,166)=EG(I,13)*EG(I,20)/EG(I,11)/EG(I,22)
      EQK(I,167)=EG(I,2)*EG(I,29)/EG(I,11)/EG(I,22)
      EQK(I,168)=EG(I,13)*EG(I,21)/EG(I,12)/EG(I,22)
      EQK(I,169)=EG(I,31)/EG(I,12)/EG(I,22)/PFAC
      EQK(I,170)=EG(I,24)/EG(I,2)/EG(I,23)/PFAC
      EQK(I,171)=EG(I,1)*EG(I,22)/EG(I,2)/EG(I,23)
      EQK(I,172)=EG(I,12)*EG(I,17)/EG(I,3)/EG(I,23)
      EQK(I,173)=EG(I,2)*EG(I,28)/EG(I,3)/EG(I,23)
      EQK(I,174)=EG(I,7)*EG(I,22)/EG(I,4)/EG(I,23)
      EQK(I,175)=EG(I,4)*EG(I,24)/EG(I,7)/EG(I,23)
      EQK(I,176)=EG(I,8)*EG(I,22)/EG(I,7)/EG(I,23)
      EQK(I,177)=EG(I,5)*EG(I,12)*EG(I,17)/EG(I,7)/EG(I,23)*PFAC
      EQK(I,178)=EG(I,7)*EG(I,24)/EG(I,8)/EG(I,23)
      EQK(I,179)=EG(I,14)*EG(I,24)/EG(I,16)/EG(I,23)
      EQK(I,180)=EG(I,1)*EG(I,23)/EG(I,2)/EG(I,24)
      EQK(I,181)=EG(I,5)*EG(I,23)/EG(I,3)/EG(I,24)
      EQK(I,182)=EG(I,6)*EG(I,23)/EG(I,5)/EG(I,24)
      EQK(I,183)=EG(I,12)*EG(I,23)/EG(I,11)/EG(I,24)
      EQK(I,184)=EG(I,13)*EG(I,23)/EG(I,12)/EG(I,24)
      EQK(I,185)=EG(I,30)/EG(I,2)/EG(I,29)/PFAC
      EQK(I,186)=EG(I,13)*EG(I,20)/EG(I,2)/EG(I,29)
      EQK(I,187)=EG(I,4)*EG(I,30)/EG(I,7)/EG(I,29)
      EQK(I,188)=EG(I,5)*EG(I,17)*EG(I,21)/EG(I,7)/EG(I,29)*PFAC
      EQK(I,189)=EG(I,14)*EG(I,30)/EG(I,16)/EG(I,29)
      EQK(I,190)=EG(I,31)/EG(I,2)/EG(I,30)/PFAC
      EQK(I,191)=EG(I,12)*EG(I,22)/EG(I,2)/EG(I,30)
      EQK(I,192)=EG(I,1)*EG(I,29)/EG(I,2)/EG(I,30)
      EQK(I,193)=EG(I,2)*EG(I,12)*EG(I,26)/EG(I,3)/EG(I,30)*PFAC
      EQK(I,194)=EG(I,16)*EG(I,23)/EG(I,3)/EG(I,30)
      EQK(I,195)=EG(I,5)*EG(I,29)/EG(I,3)/EG(I,30)
      EQK(I,196)=EG(I,6)*EG(I,29)/EG(I,5)/EG(I,30)
      EQK(I,197)=EG(I,8)*EG(I,29)/EG(I,7)/EG(I,30)
      EQK(I,198)=EG(I,13)*EG(I,29)/EG(I,12)/EG(I,30)
      EQK(I,199)=EG(I,12)*EG(I,23)/EG(I,2)/EG(I,31)
      EQK(I,200)=EG(I,1)*EG(I,30)/EG(I,2)/EG(I,31)
      EQK(I,201)=EG(I,17)*EG(I,23)/EG(I,3)/EG(I,31)
      EQK(I,202)=EG(I,6)*EG(I,30)/EG(I,5)/EG(I,31)
      EQK(I,203)=EG(I,7)*EG(I,30)/EG(I,4)/EG(I,31)
      EQK(I,204)=EG(I,5)*EG(I,17)*EG(I,23)/EG(I,7)/EG(I,31)*PFAC
      EQK(I,205)=EG(I,13)*EG(I,30)/EG(I,12)/EG(I,31)
      EQK(I,206)=EG(I,12)*EG(I,29)/EG(I,21)/EG(I,23)
     ENDDO
C
      DO I = 1, 206
        RB(:VL,I) = RF(:VL,I) / MAX(EQK(:VL,I),SMALL)
      ENDDO
C
      RKLOW(:VL,1) = (4.22794408D1 -9.D-1*ALOGT +8.55468335D2*TI)
      call VRDA_exp(VL,RKLOW(1,1),RKLOW(1,1))
      RKLOW(:VL,2) = (6.37931383D1 -3.42D0*ALOGT -4.24463259D4*TI)
      call VRDA_exp(VL,RKLOW(1,2),RKLOW(1,2))
      RKLOW(:VL,3) = (6.54619238D1 -3.74D0*ALOGT -9.74227469D2*TI)
      call VRDA_exp(VL,RKLOW(1,3),RKLOW(1,3))
      RKLOW(:VL,4) = (5.55621468D1 -2.57D0*ALOGT -7.17083751D2*TI)
      call VRDA_exp(VL,RKLOW(1,4),RKLOW(1,4))
      RKLOW(:VL,5) = (6.33329483D1 -3.14D0*ALOGT -6.18956501D2*TI)
      call VRDA_exp(VL,RKLOW(1,5),RKLOW(1,5))
      RKLOW(:VL,6) = (7.69748493D1 -5.11D0*ALOGT -3.57032226D3*TI)
      call VRDA_exp(VL,RKLOW(1,6),RKLOW(1,6))
      RKLOW(:VL,7) = (6.98660102D1 -4.8D0*ALOGT -2.79788467D3*TI)
      call VRDA_exp(VL,RKLOW(1,7),RKLOW(1,7))
      RKLOW(:VL,8) = (7.68923562D1 -4.76D0*ALOGT -1.22784867D3*TI)
      call VRDA_exp(VL,RKLOW(1,8),RKLOW(1,8))
      RKLOW(:VL,9) = (1.11312542D2 -9.588D0*ALOGT -2.566405D3*TI)
      call VRDA_exp(VL,RKLOW(1,9),RKLOW(1,9))
      RKLOW(:VL,10) = (1.15700234D2 -9.67D0*ALOGT -3.13000767D3*TI)
      call VRDA_exp(VL,RKLOW(1,10),RKLOW(1,10))
      RKLOW(:VL,11) = (3.54348644D1 -6.4D-1*ALOGT -2.50098684D4*TI)
      call VRDA_exp(VL,RKLOW(1,11),RKLOW(1,11))
      RKLOW(:VL,12) = (6.3111756D1 -3.4D0*ALOGT -1.80145126D4*TI)
      call VRDA_exp(VL,RKLOW(1,12),RKLOW(1,12))
      RKLOW(:VL,13) = (9.57409899D1 -7.64D0*ALOGT -5.98827834D3*TI)
      call VRDA_exp(VL,RKLOW(1,13),RKLOW(1,13))
      RKLOW(:VL,14) = (6.9414025D1 -3.86D0*ALOGT -1.67067934D3*TI)
      call VRDA_exp(VL,RKLOW(1,14),RKLOW(1,14))
      RKLOW(:VL,15) = (1.35001549D2 -1.194D1*ALOGT -4.9163262D3*TI)
      call VRDA_exp(VL,RKLOW(1,15),RKLOW(1,15))
      RKLOW(:VL,16) = (9.14494773D1 -7.297D0*ALOGT -2.36511834D3*TI)
      call VRDA_exp(VL,RKLOW(1,16),RKLOW(1,16))
      RKLOW(:VL,17) = (1.17075165D2 -9.31D0*ALOGT -5.02512164D4*TI)
      call VRDA_exp(VL,RKLOW(1,17),RKLOW(1,17))
      RKLOW(:VL,18) = (9.68908955D1 -7.62D0*ALOGT -3.50742017D3*TI)
      call VRDA_exp(VL,RKLOW(1,18),RKLOW(1,18))
      RKLOW(:VL,19) = (9.50941235D1 -7.08D0*ALOGT -3.36400342D3*TI)
      call VRDA_exp(VL,RKLOW(1,19),RKLOW(1,19))
      RKLOW(:VL,20) = (1.38440285D2 -1.2D1*ALOGT -3.00309643D3*TI)
      call VRDA_exp(VL,RKLOW(1,20),RKLOW(1,20))
      RKLOW(:VL,21) = (8.93324137D1 -6.66D0*ALOGT -3.52251667D3*TI)
      call VRDA_exp(VL,RKLOW(1,21),RKLOW(1,21))
C
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE RDSMH_I  (VL,T, SMH)
        USE chemkin_m, only : MAXVL
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C
      DIMENSION SMH(MAXVL,*), TN(MAXVL,5),T(MAXVL)
      DIMENSION TLOG(MAXVL),TI(MAXVL)
        INTEGER VL
C
C      TLOG = LOG(T)
      CALL VRDA_LOG(VL,T,TLOG)
      TI = 1.0D0/T
C
      TN(:VL,1) = TLOG - 1.0
      TN(:VL,2) = T
      TN(:VL,3) = TN(:VL,2)*T
      TN(:VL,4) = TN(:VL,3)*T
      TN(:VL,5) = TN(:VL,4)*T
C
      DO I=1,VL

      IF (T(I) .GT. 1.D3) THEN
C
      SMH(I,1) = -3.20502331D+00 + 9.50158922D+02*TI(I) 
     *         + 3.33727920D+00*TN(I,1) - 2.47012365D-05*TN(I,2) 
     *         + 8.32427963D-08*TN(I,3) - 1.49638662D-11*TN(I,4) 
     *         + 1.00127688D-15*TN(I,5) 
      SMH(I,2) = -4.46682914D-01 - 2.54736599D+04*TI(I) 
     *         + 2.50000001D+00*TN(I,1) - 1.15421486D-11*TN(I,2) 
     *         + 2.69269913D-15*TN(I,3) - 3.94596029D-19*TN(I,4) 
     *         + 2.49098679D-23*TN(I,5) 
      SMH(I,3) = 4.78433864D+00 - 2.92175791D+04*TI(I) 
     *         + 2.56942078D+00*TN(I,1) - 4.29870569D-05*TN(I,2) 
     *         + 6.99140982D-09*TN(I,3) - 8.34814992D-13*TN(I,4) 
     *         + 6.14168455D-17*TN(I,5) 
      SMH(I,4) = 5.45323129D+00 + 1.08845772D+03*TI(I) 
     *         + 3.28253784D+00*TN(I,1) + 7.41543770D-04*TN(I,2) 
     *         - 1.26327778D-07*TN(I,3) + 1.74558796D-11*TN(I,4) 
     *         - 1.08358897D-15*TN(I,5) 
      SMH(I,5) = 4.47669610D+00 - 3.85865700D+03*TI(I) 
     *         + 3.09288767D+00*TN(I,1) + 2.74214858D-04*TN(I,2) 
     *         + 2.10842047D-08*TN(I,3) - 7.32884630D-12*TN(I,4) 
     *         + 5.87061880D-16*TN(I,5) 
      SMH(I,6) = 4.96677010D+00 + 3.00042971D+04*TI(I) 
     *         + 3.03399249D+00*TN(I,1) + 1.08845902D-03*TN(I,2) 
     *         - 2.73454197D-08*TN(I,3) - 8.08683225D-12*TN(I,4) 
     *         + 8.41004960D-16*TN(I,5) 
      SMH(I,7) = 3.78510215D+00 - 1.11856713D+02*TI(I) 
     *         + 4.01721090D+00*TN(I,1) + 1.11991007D-03*TN(I,2) 
     *         - 1.05609692D-07*TN(I,3) + 9.52053083D-12*TN(I,4) 
     *         - 5.39542675D-16*TN(I,5) 
      SMH(I,8) = 2.91615662D+00 + 1.78617877D+04*TI(I) 
     *         + 4.16500285D+00*TN(I,1) + 2.45415847D-03*TN(I,2) 
     *         - 3.16898708D-07*TN(I,3) + 3.09321655D-11*TN(I,4) 
     *         - 1.43954153D-15*TN(I,5) 
      SMH(I,9) = 5.48497999D+00 - 7.10124364D+04*TI(I) 
     *         + 2.87846473D+00*TN(I,1) + 4.85456841D-04*TN(I,2) 
     *         + 2.40742758D-08*TN(I,3) - 1.08906541D-11*TN(I,4) 
     *         + 8.80396915D-16*TN(I,5) 
      SMH(I,10) = 6.17119324D+00 - 4.62636040D+04*TI(I) 
     *         + 2.87410113D+00*TN(I,1) + 1.82819646D-03*TN(I,2) 
     *         - 2.34824328D-07*TN(I,3) + 2.16816291D-11*TN(I,4) 
     *         - 9.38637835D-16*TN(I,5) 
      SMH(I,11) = 8.62650169D+00 - 5.09259997D+04*TI(I) 
     *         + 2.29203842D+00*TN(I,1) + 2.32794319D-03*TN(I,2) 
     *         - 3.35319912D-07*TN(I,3) + 3.48255000D-11*TN(I,4) 
     *         - 1.69858183D-15*TN(I,5) 
      SMH(I,12) = 8.48007179D+00 - 1.67755843D+04*TI(I) 
     *         + 2.28571772D+00*TN(I,1) + 3.61995018D-03*TN(I,2) 
     *         - 4.97857247D-07*TN(I,3) + 4.96403870D-11*TN(I,4) 
     *         - 2.33577197D-15*TN(I,5) 
      SMH(I,13) = 1.84373180D+01 + 9.46834459D+03*TI(I) 
     *         + 7.48514950D-02*TN(I,1) + 6.69547335D-03*TN(I,2) 
     *         - 9.55476348D-07*TN(I,3) + 1.01910446D-10*TN(I,4) 
     *         - 5.09076150D-15*TN(I,5) 
      SMH(I,14) = 7.81868772D+00 + 1.41518724D+04*TI(I) 
     *         + 2.71518561D+00*TN(I,1) + 1.03126372D-03*TN(I,2) 
     *         - 1.66470962D-07*TN(I,3) + 1.91710840D-11*TN(I,4) 
     *         - 1.01823858D-15*TN(I,5) 
      ENDIF
      ENDDO
      DO I=1,VL

      IF (T(I) .GT. 1.D3) THEN
      SMH(I,15) = 2.27163806D+00 + 4.87591660D+04*TI(I) 
     *         + 3.85746029D+00*TN(I,1) + 2.20718513D-03*TN(I,2) 
     *         - 3.69135673D-07*TN(I,3) + 4.36241823D-11*TN(I,4) 
     *         - 2.36042082D-15*TN(I,5) 
      SMH(I,16) = 9.79834492D+00 - 4.01191815D+03*TI(I) 
     *         + 2.77217438D+00*TN(I,1) + 2.47847763D-03*TN(I,2) 
     *         - 4.14076022D-07*TN(I,3) + 4.90968148D-11*TN(I,4) 
     *         - 2.66754356D-15*TN(I,5) 
      SMH(I,17) = 1.36563230D+01 + 1.39958323D+04*TI(I) 
     *         + 1.76069008D+00*TN(I,1) + 4.60000041D-03*TN(I,2) 
     *         - 7.37098022D-07*TN(I,3) + 8.38676767D-11*TN(I,4) 
     *         - 4.41927820D-15*TN(I,5) 
      SMH(I,18) = 2.92957500D+00 - 1.27832520D+02*TI(I) 
     *         + 3.77079900D+00*TN(I,1) + 3.93574850D-03*TN(I,2) 
     *         - 4.42730667D-07*TN(I,3) + 3.28702583D-11*TN(I,4) 
     *         - 1.05630800D-15*TN(I,5) 
      SMH(I,19) = -1.23028121D+00 - 2.59359992D+04*TI(I) 
     *         + 4.14756964D+00*TN(I,1) + 2.98083332D-03*TN(I,2) 
     *         - 3.95491420D-07*TN(I,3) + 3.89510143D-11*TN(I,4) 
     *         - 1.80617607D-15*TN(I,5) 
      SMH(I,20) = 6.40237010D-01 - 4.83166880D+04*TI(I) 
     *         + 4.27803400D+00*TN(I,1) + 2.37814020D-03*TN(I,2) 
     *         - 2.71683483D-07*TN(I,3) + 2.12190050D-11*TN(I,4) 
     *         - 7.44318950D-16*TN(I,5) 
      SMH(I,21) = 7.78732378D+00 - 3.46128739D+04*TI(I) 
     *         + 3.01672400D+00*TN(I,1) + 5.16511460D-03*TN(I,2) 
     *         - 7.80137248D-07*TN(I,3) + 8.48027400D-11*TN(I,4) 
     *         - 4.31303520D-15*TN(I,5) 
      SMH(I,22) = 1.03053693D+01 - 4.93988614D+03*TI(I) 
     *         + 2.03611116D+00*TN(I,1) + 7.32270755D-03*TN(I,2) 
     *         - 1.11846319D-06*TN(I,3) + 1.22685769D-10*TN(I,4) 
     *         - 6.28530305D-15*TN(I,5) 
      SMH(I,23) = 1.34624343D+01 - 1.28575200D+04*TI(I) 
     *         + 1.95465642D+00*TN(I,1) + 8.69863610D-03*TN(I,2) 
     *         - 1.33034445D-06*TN(I,3) + 1.46014741D-10*TN(I,4) 
     *         - 7.48207880D-15*TN(I,5) 
      SMH(I,24) = 1.51156107D+01 + 1.14263932D+04*TI(I) 
     *         + 1.07188150D+00*TN(I,1) + 1.08426339D-02*TN(I,2) 
     *         - 1.67093445D-06*TN(I,3) + 1.84510001D-10*TN(I,4) 
     *         - 9.50014450D-15*TN(I,5) 
      SMH(I,25) = -3.93025950D+00 - 1.93272150D+04*TI(I) 
     *         + 5.62820580D+00*TN(I,1) + 2.04267005D-03*TN(I,2) 
     *         - 2.65575783D-07*TN(I,3) + 2.38550433D-11*TN(I,4) 
     *         - 9.70391600D-16*TN(I,5) 
      SMH(I,26) = 6.32247205D-01 + 7.55105311D+03*TI(I) 
     *         + 4.51129732D+00*TN(I,1) + 4.50179872D-03*TN(I,2) 
     *         - 6.94899392D-07*TN(I,3) + 7.69454902D-11*TN(I,4) 
     *         - 3.97419100D-15*TN(I,5) 
      SMH(I,27) = -5.03208790D+00 - 4.90321780D+02*TI(I) 
     *         + 5.97566990D+00*TN(I,1) + 4.06529570D-03*TN(I,2) 
     *         - 4.57270750D-07*TN(I,3) + 3.39192008D-11*TN(I,4) 
     *         - 1.08800855D-15*TN(I,5) 
      SMH(I,28) = -3.48079170D+00 + 2.25931220D+04*TI(I) 
     *         + 5.40411080D+00*TN(I,1) + 5.86152950D-03*TN(I,2) 
     *         - 7.04385617D-07*TN(I,3) + 5.69770425D-11*TN(I,4) 
     *         - 2.04924315D-15*TN(I,5) 
      SMH(I,29) = -1.12430500D+01 - 1.74824490D+04*TI(I) 
     *         + 6.50078770D+00*TN(I,1) + 7.16236550D-03*TN(I,2) 
     *         - 9.46360533D-07*TN(I,3) + 9.23400083D-11*TN(I,4) 
     *         - 4.51819435D-15*TN(I,5) 
      SMH(I,30) = -1.33133500D+01 + 9.23570300D+02*TI(I) 
     *         + 6.73225700D+00*TN(I,1) + 7.45417000D-03*TN(I,2) 
     *         - 8.24983167D-07*TN(I,3) + 6.01001833D-11*TN(I,4) 
     *         - 1.88310200D-15*TN(I,5) 
      SMH(I,31) = -1.55152970D+01 - 7.97622360D+03*TI(I) 
     *         + 7.70974790D+00*TN(I,1) + 8.01574250D-03*TN(I,2) 
     *         - 8.78670633D-07*TN(I,3) + 6.32402933D-11*TN(I,4) 
     *         - 1.94313595D-15*TN(I,5) 
C
      ENDIF
      ENDDO
      DO I=1,VL
      IF (T(I) .LE. 1.D3) THEN
C
      SMH(I,1) = 6.83010238D-01 + 9.17935173D+02*TI(I) 
     *         + 2.34433112D+00*TN(I,1) + 3.99026037D-03*TN(I,2) 
     *         - 3.24635850D-06*TN(I,3) + 1.67976745D-09*TN(I,4) 
     *         - 3.68805881D-13*TN(I,5) 
      SMH(I,2) = -4.46682853D-01 - 2.54736599D+04*TI(I) 
     *         + 2.50000000D+00*TN(I,1) + 3.52666409D-13*TN(I,2) 
     *         - 3.32653273D-16*TN(I,3) + 1.91734693D-19*TN(I,4) 
     *         - 4.63866166D-23*TN(I,5) 
      SMH(I,3) = 2.05193346D+00 - 2.91222592D+04*TI(I) 
     *         + 3.16826710D+00*TN(I,1) - 1.63965942D-03*TN(I,2) 
     *         + 1.10717733D-06*TN(I,3) - 5.10672187D-10*TN(I,4) 
     *         + 1.05632986D-13*TN(I,5) 
      SMH(I,4) = 3.65767573D+00 + 1.06394356D+03*TI(I) 
     *         + 3.78245636D+00*TN(I,1) - 1.49836708D-03*TN(I,2) 
     *         + 1.64121700D-06*TN(I,3) - 8.06774591D-10*TN(I,4) 
     *         + 1.62186419D-13*TN(I,5) 
      SMH(I,5) = -1.03925458D-01 - 3.61508056D+03*TI(I) 
     *         + 3.99201543D+00*TN(I,1) - 1.20065876D-03*TN(I,2) 
     *         + 7.69656402D-07*TN(I,3) - 3.23427778D-10*TN(I,4) 
     *         + 6.82057350D-14*TN(I,5) 
      SMH(I,6) = -8.49032208D-01 + 3.02937267D+04*TI(I) 
     *         + 4.19864056D+00*TN(I,1) - 1.01821705D-03*TN(I,2) 
     *         + 1.08673369D-06*TN(I,3) - 4.57330885D-10*TN(I,4) 
     *         + 8.85989085D-14*TN(I,5) 
      SMH(I,7) = 3.71666245D+00 - 2.94808040D+02*TI(I) 
     *         + 4.30179801D+00*TN(I,1) - 2.37456025D-03*TN(I,2) 
     *         + 3.52638152D-06*TN(I,3) - 2.02303245D-09*TN(I,4) 
     *         + 4.64612562D-13*TN(I,5) 
      SMH(I,8) = 3.43505074D+00 + 1.77025821D+04*TI(I) 
     *         + 4.27611269D+00*TN(I,1) - 2.71411208D-04*TN(I,2) 
     *         + 2.78892835D-06*TN(I,3) - 1.79809011D-09*TN(I,4) 
     *         + 4.31227182D-13*TN(I,5) 
      SMH(I,9) = 2.08401108D+00 - 7.07972934D+04*TI(I) 
     *         + 3.48981665D+00*TN(I,1) + 1.61917771D-04*TN(I,2) 
     *         - 2.81498442D-07*TN(I,3) + 2.63514439D-10*TN(I,4) 
     *         - 7.03045335D-14*TN(I,5) 
      SMH(I,10) = 1.56253185D+00 - 4.60040401D+04*TI(I) 
     *         + 3.76267867D+00*TN(I,1) + 4.84436072D-04*TN(I,2) 
     *         + 4.65816402D-07*TN(I,3) - 3.20909294D-10*TN(I,4) 
     *         + 8.43708595D-14*TN(I,5) 
      SMH(I,11) = -7.69118967D-01 - 5.04968163D+04*TI(I) 
     *         + 4.19860411D+00*TN(I,1) - 1.18330710D-03*TN(I,2) 
     *         + 1.37216037D-06*TN(I,3) - 5.57346651D-10*TN(I,4) 
     *         + 9.71573685D-14*TN(I,5) 
      SMH(I,12) = 1.60456433D+00 - 1.64449988D+04*TI(I) 
     *         + 3.67359040D+00*TN(I,1) + 1.00547588D-03*TN(I,2) 
     *         + 9.55036427D-07*TN(I,3) - 5.72597854D-10*TN(I,4) 
     *         + 1.27192867D-13*TN(I,5) 
      SMH(I,13) = -4.64130376D+00 + 1.02466476D+04*TI(I) 
     *         + 5.14987613D+00*TN(I,1) - 6.83548940D-03*TN(I,2) 
     *         + 8.19667665D-06*TN(I,3) - 4.03952522D-09*TN(I,4) 
     *         + 8.33469780D-13*TN(I,5) 
      SMH(I,14) = 3.50840928D+00 + 1.43440860D+04*TI(I) 
     *         + 3.57953347D+00*TN(I,1) - 3.05176840D-04*TN(I,2) 
     *         + 1.69469055D-07*TN(I,3) + 7.55838237D-11*TN(I,4) 
     *         - 4.52212249D-14*TN(I,5) 
      SMH(I,15) = 9.90105222D+00 + 4.83719697D+04*TI(I) 
     *         + 2.35677352D+00*TN(I,1) + 4.49229839D-03*TN(I,2) 
     *         - 1.18726045D-06*TN(I,3) + 2.04932518D-10*TN(I,4) 
     *         - 7.18497740D-15*TN(I,5) 
      ENDIF
     ENDDO
      DO I=1,VL
      IF (T(I) .LE. 1.D3) THEN
      SMH(I,16) = 3.39437243D+00 - 3.83956496D+03*TI(I) 
     *         + 4.22118584D+00*TN(I,1) - 1.62196266D-03*TN(I,2) 
     *         + 2.29665743D-06*TN(I,3) - 1.10953411D-09*TN(I,4) 
     *         + 2.16884433D-13*TN(I,5) 
      SMH(I,17) = 6.02812900D-01 + 1.43089567D+04*TI(I) 
     *         + 4.79372315D+00*TN(I,1) - 4.95416685D-03*TN(I,2) 
     *         + 6.22033347D-06*TN(I,3) - 3.16071051D-09*TN(I,4) 
     *         + 6.58863260D-13*TN(I,5) 
      SMH(I,18) = 1.31521770D+01 - 9.78601100D+02*TI(I) 
     *         + 2.10620400D+00*TN(I,1) + 3.60829750D-03*TN(I,2) 
     *         + 8.89745333D-07*TN(I,3) - 6.14803000D-10*TN(I,4) 
     *         + 1.03780500D-13*TN(I,5) 
      SMH(I,19) = 1.39397051D+01 - 2.64289807D+04*TI(I) 
     *         + 8.08681094D-01*TN(I,1) + 1.16807815D-02*TN(I,2) 
     *         - 5.91953025D-06*TN(I,3) + 2.33460364D-09*TN(I,4) 
     *         - 4.25036487D-13*TN(I,5) 
      SMH(I,20) = 5.92039100D+00 - 4.86217940D+04*TI(I) 
     *         + 3.28154830D+00*TN(I,1) + 3.48823955D-03*TN(I,2) 
     *         - 3.97587400D-07*TN(I,3) - 1.00870267D-10*TN(I,4) 
     *         + 4.90947725D-14*TN(I,5) 
      SMH(I,21) = 8.51054025D+00 - 3.48598468D+04*TI(I) 
     *         + 3.21246645D+00*TN(I,1) + 7.57395810D-04*TN(I,2) 
     *         + 4.32015687D-06*TN(I,3) - 2.98048206D-09*TN(I,4) 
     *         + 7.35754365D-13*TN(I,5) 
      SMH(I,22) = 4.09733096D+00 - 5.08977593D+03*TI(I) 
     *         + 3.95920148D+00*TN(I,1) - 3.78526124D-03*TN(I,2) 
     *         + 9.51650487D-06*TN(I,3) - 5.76323961D-09*TN(I,4) 
     *         + 1.34942187D-12*TN(I,5) 
      SMH(I,23) = 4.70720924D+00 - 1.28416265D+04*TI(I) 
     *         + 4.30646568D+00*TN(I,1) - 2.09329446D-03*TN(I,2) 
     *         + 8.28571345D-06*TN(I,3) - 4.99272172D-09*TN(I,4) 
     *         + 1.15254502D-12*TN(I,5) 
      SMH(I,24) = 2.66682316D+00 + 1.15222055D+04*TI(I) 
     *         + 4.29142492D+00*TN(I,1) - 2.75077135D-03*TN(I,2) 
     *         + 9.99063813D-06*TN(I,3) - 5.90388571D-09*TN(I,4) 
     *         + 1.34342886D-12*TN(I,5) 
      SMH(I,25) = 1.24904170D+01 - 2.00594490D+04*TI(I) 
     *         + 2.25172140D+00*TN(I,1) + 8.82751050D-03*TN(I,2) 
     *         - 3.95485017D-06*TN(I,3) + 1.43964658D-09*TN(I,4) 
     *         - 2.53324055D-13*TN(I,5) 
      SMH(I,26) = 1.22156480D+01 + 7.04291804D+03*TI(I) 
     *         + 2.13583630D+00*TN(I,1) + 9.05943605D-03*TN(I,2) 
     *         - 2.89912457D-06*TN(I,3) + 7.78664640D-10*TN(I,4) 
     *         - 1.00728807D-13*TN(I,5) 
      SMH(I,27) = 9.57145350D+00 - 1.52147660D+03*TI(I) 
     *         + 3.40906240D+00*TN(I,1) + 5.36928700D-03*TN(I,2) 
     *         + 3.15248750D-07*TN(I,3) + 5.96548592D-10*TN(I,4) 
     *         + 1.43369255D-13*TN(I,5) 
      SMH(I,28) = 4.10301590D+00 + 2.15728780D+04*TI(I) 
     *         + 4.72945950D+00*TN(I,1) - 1.59664290D-03*TN(I,2) 
     *         + 7.92248683D-06*TN(I,3) - 4.78821758D-09*TN(I,4) 
     *         + 1.09655560D-12*TN(I,5) 
      SMH(I,29) = 1.71732140D+01 - 1.92456290D+04*TI(I) 
     *         + 1.36318350D+00*TN(I,1) + 9.90691050D-03*TN(I,2) 
     *         + 2.08284333D-06*TN(I,3) - 2.77962958D-09*TN(I,4) 
     *         + 7.92328550D-13*TN(I,5) 
      SMH(I,30) = 1.61453400D+01 - 1.07482600D+03*TI(I) 
     *         + 1.49330700D+00*TN(I,1) + 1.04625900D-02*TN(I,2) 
     *         + 7.47799000D-07*TN(I,3) - 1.39076000D-09*TN(I,4) 
     *         + 3.57907300D-13*TN(I,5) 
      SMH(I,31) = 2.11360340D+01 - 1.03123460D+04*TI(I) 
     *         + 1.04911730D+00*TN(I,1) + 1.30044865D-02*TN(I,2) 
     *         + 3.92375267D-07*TN(I,3) - 1.63292767D-09*TN(I,4) 
     *         + 4.68601035D-13*TN(I,5) 
      ENDIF
     ENDDO
C
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE RATX_I (VL,T, C, RF, RB, RKLOW)
        USE chemkin_m, only : MAXVL
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C
      DIMENSION T(MAXVL),TI(MAXVL)
      DIMENSION C(MAXVL,*), RF(MAXVL,*), RB(MAXVL,*)
      DIMENSION RKLOW(MAXVL,*), CTB(MAXVL,206)
      DIMENSION CTOT(MAXVL),PR(MAXVL),PCOR(MAXVL)
      DIMENSION PRLOG(MAXVL),FCENT0(MAXVL),FCENT1(MAXVL)
      DIMENSION FCENT2(MAXVL),FCLOG(MAXVL),XN(MAXVL)
      DIMENSION CPRLOG(MAXVL),FLOG(MAXVL),FC(MAXVL)
      DIMENSION FCENT(MAXVL),ALOGT(MAXVL)
      INTEGER VL
      double precision vecof10(MAXVL)

      DATA SMALL/1.D-200/
C
      vecof10=10.
      TI=1./T
C     ALOGT = LOG(T)
      CALL VRDA_LOG(VL,T,ALOGT)
C
C     third-body reactions
C
      CTOT = 0.0
      DO K = 1, 22
      DO I=1,VL
         CTOT(I) = CTOT(I) + C(I,K)
      ENDDO
      ENDDO
C
      CTB(:VL,5)  = CTOT - C(:VL,1) - C(:VL,6) + C(:VL,10) - C(:VL,12) 
     *                              + 2.D0*C(:VL,16) 
     *               + 2.D0*C(:VL,14) + 2.D0*C(:VL,15) 
      CTB(:VL,9)  = CTOT - 2.7D-1*C(:VL,1) + 2.65D0*C(:VL,6) + C(:VL,10) 
     *                              + 2.D0*C(:VL,16)  
     *               + 2.D0*C(:VL,14) + 2.D0*C(:VL,15) 
      CTB(:VL,10) = CTOT + C(:VL,1) + 5.D0*C(:VL,6) + C(:VL,10) 
     *                              + 5.D-1*C(:VL,11) + C(:VL,12)  
     *               + 2.D0*C(:VL,16) + 2.D0*C(:VL,14) + 2.D0*C(:VL,15) 
      CTB(:VL,31) = CTB(:VL,10)
      CTB(:VL,39) = CTB(:VL,10)
      CTB(:VL,41) = CTB(:VL,10)
      CTB(:VL,46) = CTB(:VL,10)
      CTB(:VL,48) = CTB(:VL,10)
      CTB(:VL,56) = CTB(:VL,10)
      CTB(:VL,71) = CTB(:VL,10)
      CTB(:VL,78) = CTB(:VL,10)
      CTB(:VL,89) = CTB(:VL,10)
      CTB(:VL,93) = CTB(:VL,10)
      CTB(:VL,115)= CTB(:VL,10)
      CTB(:VL,126)= CTB(:VL,10)
      CTB(:VL,132)= CTB(:VL,10)
      CTB(:VL,145)= CTB(:VL,10)
      CTB(:VL,148)= CTB(:VL,10)
      CTB(:VL,155)= CTB(:VL,10)
      CTB(:VL,156)= CTB(:VL,10)
      CTB(:VL,170)= CTB(:VL,10)
      CTB(:VL,185)= CTB(:VL,10)
      CTB(:VL,114)= CTOT+C(:VL,1)+5.D0*C(:VL,6)+C(:VL,10) 
     *              +5.D-1*C(:VL,11)+C(:VL,12)  
     *              +2.D0*C(:VL,16)+1.5D0*C(:VL,14)+1.5D0*C(:VL,15) 
      CTB(:VL,11) = CTOT+1.4D0*C(:VL,1)+1.44D1*C(:VL,6)+C(:VL,10) 
     *              +7.5D-1*C(:VL,11)  
     *              +2.6D0*C(:VL,12)+2.D0*C(:VL,16)+2.D0*C(:VL,14)   
     *              +2.D0*C(:VL,15) 
      CTB(:VL,12) = CTOT - C(:VL,4) - C(:VL,6) - 2.5D-1*C(:VL,11) 
     *               +5.D-1*C(:VL,12)  
     *              +5.D-1*C(:VL,16) - C(:VL,22)+2.D0*C(:VL,14) 
     *              +2.D0*C(:VL,15) 
      CTB(:VL,16) = CTOT+C(:VL,1)+5.D0*C(:VL,6)+C(:VL,10)  
     *              +5.D-1*C(:VL,11) +C(:VL,12)  
     *              +2.D0*C(:VL,16)+2.D0*C(:VL,14)+2.D0*C(:VL,15) 
      CTB(:VL,29) = CTOT+C(:VL,1)+5.D0*C(:VL,4)+5.D0*C(:VL,6)+C(:VL,10)  
     *              +5.D-1*C(:VL,11)+2.5D0*C(:VL,12)+2.D0*C(:VL,16)  
     *              +2.D0*C(:VL,14)+2.D0*C(:VL,15) 
      CTB(:VL,121)= CTOT 
      CTB(:VL,190)= CTOT+C(:VL,1)+5.D0*C(:VL,6)+C(:VL,10)
     *              +5.D-1*C(:VL,11)+C(:VL,12)+2.D0*C(:VL,16) 
C
C     If fall-off (pressure correction):
C
C
      PR = RKLOW(:VL,1) * CTB(:VL,16) / RF(:VL,16)
      PCOR = PR / (1.0 + PR)
      PRLOG =MAX(PR,SMALL)
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 2.654D-1*EXP(-T/9.4D1) + 7.346D-1*EXP(-T/1.756D3)
C     *     + EXP(-5.182D3*TI)
     FCENT0 = -T/9.4D1
     FCENT1 = -T/1.756D3
     FCENT2 = -5.182D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 2.654D-1*FCENT0 +7.346D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,16) = RF(:VL,16) * PCOR
      RB(:VL,16) = RB(:VL,16) * PCOR
C
      PR = RKLOW(:VL,2) * CTB(:VL,31) / RF(:VL,31)
      PCOR = PR / (1.0 + PR)
      PRLOG = MAX(PR,SMALL)
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 6.8D-2*EXP(-T/1.97D2) + 9.32D-1*EXP(-T/1.54D3)
C     *     + EXP(-1.03D4*TI)
     FCENT0 = -T/1.97D2
     FCENT1 = -T/1.54D3
     FCENT2 = -1.03D4*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 6.8D-2*FCENT0 +9.32D-1*FCENT1 + FCENT2
       FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,31) = RF(:VL,31) * PCOR
      RB(:VL,31) = RB(:VL,31) * PCOR
C
      PR = RKLOW(:VL,3) * ctb(:VL,39) / RF(:VL,39)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 4.243D-1*EXP(-T/2.37D2) + 5.757D-1*EXP(-T/1.652D3)
C     *     + EXP(-5.069D3*TI)
     FCENT0 = -T/2.37D2
     FCENT1 = -T/1.652D3
     FCENT2 = -5.069D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 4.243D-1*FCENT0 +5.757D-1*FCENT1 + FCENT2
       FCLOG = MAX(FCENT,SMALL)
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,39) = RF(:VL,39) * PCOR
      RB(:VL,39) = RB(:VL,39) * PCOR
C
      PR = RKLOW(:VL,4) * ctb(:VL,41) / RF(:VL,41)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 2.176D-1*EXP(-T/2.71D2) + 7.824D-1*EXP(-T/2.755D3)
C     *     + EXP(-6.57D3*TI)
     FCENT0 = -T/2.71D2
     FCENT1 = -T/2.755D3
     FCENT2 = -6.57D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 2.176D-1*FCENT0 +7.824D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,41) = RF(:VL,41) * PCOR
      RB(:VL,41) = RB(:VL,41) * PCOR
C
      PR = RKLOW(:VL,5) * ctb(:VL,48) / RF(:VL,48)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 3.2D-1*EXP(-T/7.8D1) + 6.8D-1*EXP(-T/1.995D3)
C     *     + EXP(-5.59D3*TI)
     FCENT0 = -T/7.8D1
     FCENT1 = -T/1.995D3
     FCENT2 = -5.59D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 3.2D-1*FCENT0 +6.8D-1*FCENT1 + FCENT2
      FCLOG =(MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,48) = RF(:VL,48) * PCOR
      RB(:VL,48) = RB(:VL,48) * PCOR
C
      PR = RKLOW(:VL,6) * ctb(:VL,56) / RF(:VL,56)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 4.093D-1*EXP(-T/2.75D2) + 5.907D-1*EXP(-T/1.226D3)
C     *     + EXP(-5.185D3*TI)
     FCENT0 = -T/2.75D2
     FCENT1 = -T/1.226D3
     FCENT2 = -5.185D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 4.093D-1*FCENT0 +5.907D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,56) = RF(:VL,56) * PCOR
      RB(:VL,56) = RB(:VL,56) * PCOR
C
      PR = RKLOW(:VL,7) * ctb(:VL,71) / RF(:VL,71)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 2.42D-1*EXP(-T/9.4D1) + 7.58D-1*EXP(-T/1.555D3)
C     *     + EXP(-4.2D3*TI)
     FCENT0 = -T/9.4D1
     FCENT1 = -T/1.555D3
     FCENT2 = -4.2D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 2.42D-1*FCENT0 +7.58D-1*FCENT1 + FCENT2
       FCLOG = MAX(FCENT,SMALL)
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,71) = RF(:VL,71) * PCOR
      RB(:VL,71) = RB(:VL,71) * PCOR
C
      PR = RKLOW(:VL,8) * ctb(:VL,78) / RF(:VL,78)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 2.17D-1*EXP(-T/7.4D1) + 7.83D-1*EXP(-T/2.941D3)
C     *     + EXP(-6.964D3*TI)
     FCENT0 = -T/7.4D1
     FCENT1 = -T/2.941D3
     FCENT2 = -6.964D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 2.17D-1*FCENT0 +7.83D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,78) = RF(:VL,78) * PCOR
      RB(:VL,78) = RB(:VL,78) * PCOR
C
      PR = RKLOW(:VL,9) * ctb(:VL,89) / RF(:VL,89)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 3.827D-1*EXP(-T/1.3076D1) + 6.173D-1*EXP(-T/2.078D3)
C     *     + EXP(-5.093D3*TI)
     FCENT0 = -T/1.3076D1
     FCENT1 = -T/2.078D3
     FCENT2 = -5.093D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 3.827D-1*FCENT0 +6.173D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,89) = RF(:VL,89) * PCOR
      RB(:VL,89) = RB(:VL,89) * PCOR
C
      PR = RKLOW(:VL,10) * ctb(:VL,93) / RF(:VL,93)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 4.675D-1*EXP(-T/1.51D2) + 5.325D-1*EXP(-T/1.038D3)
C     *     + EXP(-4.97D3*TI)
     FCENT0 = -T/1.51D2
     FCENT1 = -T/1.038D3
     FCENT2 = -4.97D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 4.675D-1*FCENT0 +5.325D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,93) = RF(:VL,93) * PCOR
      RB(:VL,93) = RB(:VL,93) * PCOR
C
      PR = RKLOW(:VL,11) * ctb(:VL,114) / RF(:VL,114)
      PCOR = PR / (1.0 + PR)
      RF(:VL,114) = RF(:VL,114) * PCOR
      RB(:VL,114) = RB(:VL,114) * PCOR
C
      PR = RKLOW(:VL,12) * ctb(:VL,115) / RF(:VL,115)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = -9.816D-1*EXP(-T/5.3837D3) + 1.9816D0*EXP(-T/4.2932D0)
C     *     + EXP(7.95D-2*TI)
     FCENT0 = -T/5.3837D3
     FCENT1 = -T/4.2932D0
     FCENT2 = 7.95D-2*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = -9.816D-1*FCENT0 +1.9816D0*FCENT1 + FCENT2
       FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,115) = RF(:VL,115) * PCOR
      RB(:VL,115) = RB(:VL,115) * PCOR
C
      PR = RKLOW(:VL,13) * ctb(:VL,126) / RF(:VL,126)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
      FCENT = 6.63D-1*EXP(-T/1.707D3) + 3.37D-1*EXP(-T/3.2D3)
     *     + EXP(-4.131D3*TI)
     FCENT0 = -T/1.707D3
     FCENT1 = -T/3.2D3
     FCENT2 = -4.131D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 6.63D-1*FCENT0 +3.37D-1*FCENT1 + FCENT2
       FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,126) = RF(:VL,126) * PCOR
      RB(:VL,126) = RB(:VL,126) * PCOR
C
      PR = RKLOW(:VL,14) * ctb(:VL,132) / RF(:VL,132)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 2.18D-1*EXP(-T/2.075D2) + 7.82D-1*EXP(-T/2.663D3)
C     *     + EXP(-6.095D3*TI)
     FCENT0 = -T/2.075D2
     FCENT1 = -T/2.663D3
     FCENT2 = -6.095D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 2.18D-1*FCENT0 +7.82D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,132) = RF(:VL,132) * PCOR
      RB(:VL,132) = RB(:VL,132) * PCOR
C
      PR = RKLOW(:VL,15) * ctb(:VL,145) / RF(:VL,145)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 8.25D-1*EXP(-T/1.3406D3) + 1.75D-1*EXP(-T/6.D4)
C     *     + EXP(-1.01398D4*TI)
     FCENT0 = -T/1.3406D3
     FCENT1 = -T/6.D4
     FCENT2 = -1.01398D4*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 8.25D-1*FCENT0 + 1.75D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,145) = RF(:VL,145) * PCOR
      RB(:VL,145) = RB(:VL,145) * PCOR
C
      PR = RKLOW(:VL,16) * ctb(:VL,148) / RF(:VL,148)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 4.5D-1*EXP(-T/8.9D3) + 5.5D-1*EXP(-T/4.35D3)
C     *     + EXP(-7.244D3*TI)
     FCENT0 = -T/8.9D3
     FCENT1 = -T/4.35D3
     FCENT2 = -7.244D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 4.5D-1*FCENT0 + 5.5D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,148) = RF(:VL,148) * PCOR
      RB(:VL,148) = RB(:VL,148) * PCOR
C
      PR = RKLOW(:VL,17) * ctb(:VL,155) / RF(:VL,155)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 2.655D-1*EXP(-T/1.8D2) + 7.345D-1*EXP(-T/1.035D3)
C     *     + EXP(-5.417D3*TI)
     FCENT0 = -T/1.8D2
     FCENT1 = -T/1.035D3
     FCENT2 = -5.417D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 2.655D-1*FCENT0 + 7.345D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,155) = RF(:VL,155) * PCOR
      RB(:VL,155) = RB(:VL,155) * PCOR
C
      PR = RKLOW(:VL,18) * ctb(:VL,156) / RF(:VL,156)
      PCOR = PR / (1.0 + PR)
      PRLOG = MAX(PR,SMALL)
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 2.47D-2*EXP(-T/2.1D2) + 9.753D-1*EXP(-T/9.84D2)
C     *     + EXP(-4.374D3*TI)
     FCENT0 = -T/2.1D2
     FCENT1 = -T/9.84D2
     FCENT2 = -4.374D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 2.47D-2*FCENT0 + 9.753D-1*FCENT1 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,156) = RF(:VL,156) * PCOR
      RB(:VL,156) = RB(:VL,156) * PCOR
C
      PR = RKLOW(:VL,19) * ctb(:VL,170) / RF(:VL,170)
      PCOR = PR / (1.0 + PR)
      PRLOG = (MAX(PR,SMALL))
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 1.578D-1*EXP(-T/1.25D2) + 8.422D-1*EXP(-T/2.219D3)
C     *     + EXP(-6.882D3*TI)
     FCENT0 = -T/1.25D2
     FCENT1 = -T/2.219D3
     FCENT2 = -6.882D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 1.578D-1*FCENT0 + 8.422D-1*FCENT1 + FCENT2

      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,170) = RF(:VL,170) * PCOR
      RB(:VL,170) = RB(:VL,170) * PCOR
C
      PR = RKLOW(:VL,20) * ctb(:VL,185) / RF(:VL,185)
      PCOR = PR / (1.0 + PR)
      PRLOG = MAX(PR,SMALL)
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 9.8D-1*EXP(-T/1.0966D3) + 2.D-2*EXP(-T/1.0966D3)
C    *     + EXP(-6.8595D3*TI)
     FCENT0 = -T/1.0966D3
C     FCENT1 = -T/1.0966D3
     FCENT2 = -6.8595D3*TI
     CALL VRDA_EXP(VL,FCENT0,FCENT0)
C     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = 9.8D-1*FCENT0 + 2.D-2*FCENT0 + FCENT2
      FCLOG = (MAX(FCENT,SMALL))
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,185) = RF(:VL,185) * PCOR
      RB(:VL,185) = RB(:VL,185) * PCOR
C
      PR = RKLOW(:VL,21) * ctb(:VL,190) / RF(:VL,190)
      PCOR = PR / (1.0 + PR)
      PRLOG = MAX(PR,SMALL)
     CALL VRDA_LOG10(VL,PRLOG,PRLOG)
C      FCENT = 0.D0*EXP(-T/1.D3) + 1.D0*EXP(-T/1.31D3)
C     *     + EXP(-4.8097D4*TI)
     FCENT1 = -T/1.31D3
     FCENT2 = -4.8097D4*TI
     CALL VRDA_EXP(VL,FCENT1,FCENT1)
     CALL VRDA_EXP(VL,FCENT2,FCENT2)
     FCENT = FCENT1+FCENT2
      FCLOG = MAX(FCENT,SMALL)
     CALL VRDA_LOG10(VL,FCLOG,FCLOG)
      XN    = 0.75 - 1.27*FCLOG
      CPRLOG= PRLOG - (0.4 + 0.67*FCLOG)
      FLOG = FCLOG/(1.0 + (CPRLOG/(XN-0.14*CPRLOG))**2)
      FC = alog(10._8)*FLOG; call vrda_exp(VL,FC,FC) !FC = 10.0**FLOG
      PCOR = FC * PCOR
      RF(:VL,190) = RF(:VL,190) * PCOR
      RB(:VL,190) = RB(:VL,190) * PCOR
C
      DO I=1,VL
      RF(I,1) = RF(I,1)*C(I,2)*C(I,4)
      RF(I,2) = RF(I,2)*C(I,3)*C(I,1)
      RF(I,3) = RF(I,3)*C(I,5)*C(I,1)
      RF(I,4) = RF(I,4)*C(I,5)*C(I,5)
      RF(I,5) = RF(I,5)*CTB(I,5)*C(I,2)*C(I,2)
      RF(I,6) = RF(I,6)*C(I,2)*C(I,2)*C(I,1)
      RF(I,7) = RF(I,7)*C(I,2)*C(I,2)*C(I,6)
      RF(I,8) = RF(I,8)*C(I,2)*C(I,2)*C(I,12)
      RF(I,9) = RF(I,9)*ctb(I,9)*C(I,2)*C(I,5)
      RF(I,10) = RF(I,10)*ctb(I,10)*C(I,3)*C(I,2)
      RF(I,11) = RF(I,11)*ctb(I,11)*C(I,3)*C(I,3)
      RF(I,12) = RF(I,12)*ctb(I,12)*C(I,2)*C(I,4)
      RF(I,13) = RF(I,13)*C(I,2)*C(I,4)*C(I,4)
      RF(I,14) = RF(I,14)*C(I,2)*C(I,4)*C(I,6)
      RF(I,15) = RF(I,15)*C(I,2)*C(I,4)*C(I,22)
      RF(I,16) = RF(I,16)*C(I,5)*C(I,5)
      RF(I,17) = RF(I,17)*C(I,7)*C(I,2)
      RF(I,18) = RF(I,18)*C(I,7)*C(I,2)
      RF(I,19) = RF(I,19)*C(I,7)*C(I,2)
      RF(I,20) = RF(I,20)*C(I,7)*C(I,3)
      RF(I,21) = RF(I,21)*C(I,7)*C(I,5)
      RF(I,22) = RF(I,22)*C(I,7)*C(I,7)
      RF(I,23) = RF(I,23)*C(I,7)*C(I,7)
      RF(I,24) = RF(I,24)*C(I,8)*C(I,2)
      RF(I,25) = RF(I,25)*C(I,8)*C(I,2)
      RF(I,26) = RF(I,26)*C(I,8)*C(I,3)
      RF(I,27) = RF(I,27)*C(I,8)*C(I,5)
      RF(I,28) = RF(I,28)*C(I,8)*C(I,5)
      RF(I,29) = RF(I,29)*ctb(I,29)*C(I,11)*C(I,3)
      RF(I,30) = RF(I,30)*C(I,11)*C(I,5)
      RF(I,31) = RF(I,31)*C(I,11)*C(I,1)
      RF(I,32) = RF(I,32)*C(I,11)*C(I,4)
      RF(I,33) = RF(I,33)*C(I,11)*C(I,7)
      RF(I,34) = RF(I,34)*C(I,3)
      RF(I,35) = RF(I,35)*C(I,5)
      RF(I,36) = RF(I,36)*C(I,1)
      RF(I,37) = RF(I,37)*C(I,6)
      RF(I,38) = RF(I,38)*C(I,4)
      RF(I,39) = RF(I,39)*C(I,11)
      RF(I,40) = RF(I,40)*C(I,12)
      RF(I,41) = RF(I,41)*C(I,2)
      RF(I,42) = RF(I,42)*C(I,2)
      RF(I,43) = RF(I,43)*C(I,3)
      RF(I,44) = RF(I,44)*C(I,3)
      RF(I,45) = RF(I,45)*C(I,5)
      RF(I,46) = RF(I,46)*ctb(I,46)
      RF(I,47) = RF(I,47)*C(I,4)
      RF(I,48) = RF(I,48)*C(I,2)
      RF(I,49) = RF(I,49)*C(I,1)
      RF(I,50) = RF(I,50)*C(I,3)
      RF(I,51) = RF(I,51)*C(I,4)
      RF(I,52) = RF(I,52)*C(I,4)
      RF(I,53) = RF(I,53)*C(I,5)
      RF(I,54) = RF(I,54)*C(I,5)
      RF(I,55) = RF(I,55)*C(I,7)
      RF(I,56) = RF(I,56)*C(I,11)
      RF(I,59) = RF(I,59)*C(I,22)
      RF(I,60) = RF(I,60)*C(I,2)
      RF(I,61) = RF(I,61)*C(I,3)
      RF(I,62) = RF(I,62)*C(I,3)
      RF(I,63) = RF(I,63)*C(I,5)
      RF(I,64) = RF(I,64)*C(I,1)
      RF(I,65) = RF(I,65)*C(I,4)
      RF(I,66) = RF(I,66)*C(I,4)
      RF(I,67) = RF(I,67)*C(I,6)
      RF(I,68) = RF(I,68)*C(I,11)
      RF(I,69) = RF(I,69)*C(I,12)
      RF(I,70) = RF(I,70)*C(I,12)
      RF(I,71) = RF(I,71)*C(I,13)*C(I,2)
      RF(I,72) = RF(I,72)*C(I,13)*C(I,2)
      RF(I,73) = RF(I,73)*C(I,13)*C(I,3)
      RF(I,74) = RF(I,74)*C(I,13)*C(I,5)
      RF(I,75) = RF(I,75)*C(I,13)*C(I,4)
      RF(I,76) = RF(I,76)*C(I,13)*C(I,7)
      RF(I,77) = RF(I,77)*C(I,13)
      RF(I,78) = RF(I,78)*C(I,9)*C(I,2)
      RF(I,79) = RF(I,79)*C(I,9)*C(I,3)
      RF(I,80) = RF(I,80)*C(I,9)*C(I,5)
      RF(I,81) = RF(I,81)*C(I,9)*C(I,5)
      RF(I,82) = RF(I,82)*C(I,9)*C(I,4)
      RF(I,83) = RF(I,83)*C(I,9)*C(I,4)
      RF(I,84) = RF(I,84)*C(I,9)*C(I,7)
      RF(I,85) = RF(I,85)*C(I,9)*C(I,7)
      RF(I,86) = RF(I,86)*C(I,9)*C(I,8)
      RF(I,87) = RF(I,87)*C(I,9)
      RF(I,88) = RF(I,88)*C(I,9)
      RF(I,89) = RF(I,89)*C(I,9)
      RF(I,90) = RF(I,90)*C(I,9)*C(I,13)
      RF(I,91) = RF(I,91)*C(I,9)
      RF(I,92) = RF(I,92)*C(I,9)
      RF(I,93) = RF(I,93)*C(I,9)*C(I,9)
      RF(I,94) = RF(I,94)*C(I,9)*C(I,9)
      RF(I,95) = RF(I,95)*C(I,9)*C(I,17)
      RF(I,96) = RF(I,96)*C(I,2)
      RF(I,97) = RF(I,97)*C(I,2)
      RF(I,98) = RF(I,98)*C(I,2)
      RF(I,99) = RF(I,99)*C(I,3)
      RF(I,100) = RF(I,100)*C(I,5)
      RF(I,101) = RF(I,101)*C(I,4)
      RF(I,102) = RF(I,102)*C(I,10)*C(I,2)
      RF(I,103) = RF(I,103)*C(I,10)*C(I,3)
      RF(I,104) = RF(I,104)*C(I,10)*C(I,5)
      RF(I,105) = RF(I,105)*C(I,10)
      RF(I,106) = RF(I,106)*C(I,10)
      RF(I,107) = RF(I,107)*C(I,10)
      RF(I,108) = RF(I,108)*C(I,17)*C(I,2)
      RF(I,109) = RF(I,109)*C(I,17)*C(I,3)
      RF(I,110) = RF(I,110)*C(I,17)*C(I,4)
      RF(I,111) = RF(I,111)*C(I,17)
      RF(I,112) = RF(I,112)*C(I,17)
      RF(I,113) = RF(I,113)*C(I,17)*C(I,17)
      RF(I,114) = RF(I,114)*C(I,14)
      RF(I,116) = RF(I,116)*C(I,14)*C(I,3)
      RF(I,117) = RF(I,117)*C(I,14)*C(I,3)
      RF(I,118) = RF(I,118)*C(I,14)*C(I,5)
      RF(I,119) = RF(I,119)*C(I,14)*C(I,5)
      RF(I,120) = RF(I,120)*C(I,14)
      RF(I,121) = RF(I,121)*ctb(I,121)*C(I,14)*C(I,9)
      RF(I,122) = RF(I,122)*C(I,2)
      RF(I,123) = RF(I,123)*C(I,3)
      RF(I,124) = RF(I,124)*C(I,5)
      RF(I,125) = RF(I,125)*C(I,4)
      RF(I,126) = RF(I,126)*C(I,18)*C(I,2)
      RF(I,127) = RF(I,127)*C(I,18)*C(I,2)
      RF(I,128) = RF(I,128)*C(I,18)*C(I,2)
      RF(I,129) = RF(I,129)*C(I,18)*C(I,3)
      RF(I,130) = RF(I,130)*C(I,18)*C(I,3)
      RF(I,131) = RF(I,131)*C(I,18)*C(I,5)
      RF(I,132) = RF(I,132)*C(I,2)
      RF(I,133) = RF(I,133)*C(I,2)
      RF(I,134) = RF(I,134)*C(I,2)
      RF(I,135) = RF(I,135)*C(I,3)
      RF(I,136) = RF(I,136)*C(I,3)
      RF(I,137) = RF(I,137)*C(I,5)
      RF(I,138) = RF(I,138)*C(I,4)
      RF(I,139) = RF(I,139)*C(I,4)
      RF(I,140) = RF(I,140)*C(I,4)
      RF(I,141) = RF(I,141)*C(I,7)
      RF(I,142) = RF(I,142)*C(I,8)
      RF(I,144) = RF(I,144)*C(I,9)
      RF(I,145) = RF(I,145)*C(I,9)
      RF(I,146) = RF(I,146)*C(I,9)
      RF(I,148) = RF(I,148)*C(I,2)
      RF(I,149) = RF(I,149)*C(I,2)
      RF(I,150) = RF(I,150)*C(I,2)
      RF(I,151) = RF(I,151)*C(I,3)
      RF(I,152) = RF(I,152)*C(I,5)
      RF(I,153) = RF(I,153)*C(I,4)
      RF(I,154) = RF(I,154)*C(I,4)
      RF(I,155) = RF(I,155)*C(I,15)
      RF(I,156) = RF(I,156)*C(I,15)*C(I,2)
      RF(I,157) = RF(I,157)*C(I,15)*C(I,2)
      RF(I,158) = RF(I,158)*C(I,15)*C(I,3)
      RF(I,159) = RF(I,159)*C(I,15)*C(I,3)
      RF(I,160) = RF(I,160)*C(I,15)*C(I,3)
      RF(I,161) = RF(I,161)*C(I,15)*C(I,5)
      RF(I,162) = RF(I,162)*C(I,15)*C(I,4)
      RF(I,163) = RF(I,163)*C(I,15)*C(I,7)
      RF(I,164) = RF(I,164)*C(I,15)
      RF(I,165) = RF(I,165)*C(I,15)
      RF(I,166) = RF(I,166)*C(I,15)
      RF(I,167) = RF(I,167)*C(I,15)
      RF(I,168) = RF(I,168)*C(I,15)*C(I,9)
      RF(I,169) = RF(I,169)*C(I,15)*C(I,9)
      RF(I,170) = RF(I,170)*C(I,2)
      RF(I,171) = RF(I,171)*C(I,2)
      RF(I,172) = RF(I,172)*C(I,3)
      RF(I,173) = RF(I,173)*C(I,3)
      RF(I,174) = RF(I,174)*C(I,4)
      RF(I,175) = RF(I,175)*C(I,7)
      RF(I,176) = RF(I,176)*C(I,7)
      RF(I,177) = RF(I,177)*C(I,7)
      RF(I,178) = RF(I,178)*C(I,8)
      RF(I,180) = RF(I,180)*C(I,16)*C(I,2)
      RF(I,181) = RF(I,181)*C(I,16)*C(I,3)
      RF(I,182) = RF(I,182)*C(I,16)*C(I,5)
      RF(I,183) = RF(I,183)*C(I,16)
      RF(I,184) = RF(I,184)*C(I,16)*C(I,9)
      RF(I,185) = RF(I,185)*C(I,20)*C(I,2)
      RF(I,186) = RF(I,186)*C(I,20)*C(I,2)
      RF(I,187) = RF(I,187)*C(I,20)*C(I,7)
      RF(I,188) = RF(I,188)*C(I,20)*C(I,7)
      RF(I,189) = RF(I,189)*C(I,20)
      RF(I,190) = RF(I,190)*C(I,21)*C(I,2)
      RF(I,191) = RF(I,191)*C(I,21)*C(I,2)
      RF(I,192) = RF(I,192)*C(I,21)*C(I,2)
      RF(I,193) = RF(I,193)*C(I,21)*C(I,3)
      RF(I,194) = RF(I,194)*C(I,21)*C(I,3)
      RF(I,195) = RF(I,195)*C(I,21)*C(I,3)
      RF(I,196) = RF(I,196)*C(I,21)*C(I,5)
      RF(I,197) = RF(I,197)*C(I,21)*C(I,7)
      RF(I,198) = RF(I,198)*C(I,21)*C(I,9)
      RF(I,199) = RF(I,199)*C(I,2)
      RF(I,200) = RF(I,200)*C(I,2)
      RF(I,201) = RF(I,201)*C(I,3)
      RF(I,202) = RF(I,202)*C(I,5)
      RF(I,203) = RF(I,203)*C(I,4)
      RF(I,204) = RF(I,204)*C(I,7)
      RF(I,205) = RF(I,205)*C(I,9)
      ENDDO
      DO I=1,VL
      RB(I,1) = RB(I,1)*C(I,3)*C(I,5)
      RB(I,2) = RB(I,2)*C(I,2)*C(I,5)
      RB(I,3) = RB(I,3)*C(I,2)*C(I,6)
      RB(I,4) = RB(I,4)*C(I,3)*C(I,6)
      RB(I,5) = RB(I,5)*ctb(I,5)*C(I,1)
      RB(I,6) = RB(I,6)*C(I,1)*C(I,1)
      RB(I,7) = RB(I,7)*C(I,1)*C(I,6)
      RB(I,8) = RB(I,8)*C(I,1)*C(I,12)
      RB(I,9) = RB(I,9)*ctb(I,9)*C(I,6)
      RB(I,10) = RB(I,10)*ctb(I,10)*C(I,5)
      RB(I,11) = RB(I,11)*ctb(I,11)*C(I,4)
      RB(I,12) = RB(I,12)*ctb(I,12)*C(I,7)
      RB(I,13) = RB(I,13)*C(I,7)*C(I,4)
      RB(I,14) = RB(I,14)*C(I,7)*C(I,6)
      RB(I,15) = RB(I,15)*C(I,7)*C(I,22)
      RB(I,16) = RB(I,16)*C(I,8)
      RB(I,17) = RB(I,17)*C(I,3)*C(I,6)
      RB(I,18) = RB(I,18)*C(I,4)*C(I,1)
      RB(I,19) = RB(I,19)*C(I,5)*C(I,5)
      RB(I,20) = RB(I,20)*C(I,5)*C(I,4)
      RB(I,21) = RB(I,21)*C(I,4)*C(I,6)
      RB(I,22) = RB(I,22)*C(I,4)*C(I,8)
      RB(I,23) = RB(I,23)*C(I,4)*C(I,8)
      RB(I,24) = RB(I,24)*C(I,7)*C(I,1)
      RB(I,25) = RB(I,25)*C(I,5)*C(I,6)
      RB(I,26) = RB(I,26)*C(I,5)*C(I,7)
      RB(I,27) = RB(I,27)*C(I,7)*C(I,6)
      RB(I,28) = RB(I,28)*C(I,7)*C(I,6)
      RB(I,29) = RB(I,29)*ctb(I,29)*C(I,12)
      RB(I,30) = RB(I,30)*C(I,12)*C(I,2)
      RB(I,31) = RB(I,31)*C(I,13)
      RB(I,32) = RB(I,32)*C(I,12)*C(I,3)
      RB(I,33) = RB(I,33)*C(I,12)*C(I,5)
      RB(I,34) = RB(I,34)*C(I,11)*C(I,2)
      RB(I,35) = RB(I,35)*C(I,2)
      RB(I,36) = RB(I,36)*C(I,2)
      RB(I,37) = RB(I,37)*C(I,13)*C(I,2)
      RB(I,38) = RB(I,38)*C(I,3)
      RB(I,39) = RB(I,39)*C(I,17)
      RB(I,40) = RB(I,40)*C(I,11)
      RB(I,41) = RB(I,41)*C(I,13)
      RB(I,42) = RB(I,42)*C(I,11)*C(I,1)
      RB(I,43) = RB(I,43)*C(I,11)*C(I,5)
      RB(I,44) = RB(I,44)*C(I,12)*C(I,2)
      RB(I,45) = RB(I,45)*C(I,11)*C(I,6)
      RB(I,46) = RB(I,46)*ctb(I,46)*C(I,11)*C(I,2)
      RB(I,47) = RB(I,47)*C(I,11)*C(I,7)
      RB(I,48) = RB(I,48)*C(I,9)
      RB(I,49) = RB(I,49)*C(I,2)*C(I,9)
      RB(I,50) = RB(I,50)*C(I,2)
      RB(I,51) = RB(I,51)*C(I,5)
      RB(I,52) = RB(I,52)*C(I,12)*C(I,2)*C(I,2)
      RB(I,53) = RB(I,53)*C(I,13)*C(I,2)
      RB(I,54) = RB(I,54)*C(I,6)
      RB(I,55) = RB(I,55)*C(I,13)*C(I,5)
      RB(I,56) = RB(I,56)*C(I,18)
      RB(I,57) = RB(I,57)*C(I,14)*C(I,2)
      RB(I,58) = RB(I,58)*C(I,14)*C(I,1)
      RB(I,59) = RB(I,59)*C(I,22)
      RB(I,60) = RB(I,60)*C(I,1)
      RB(I,61) = RB(I,61)*C(I,11)*C(I,1)
      RB(I,62) = RB(I,62)*C(I,2)
      RB(I,63) = RB(I,63)*C(I,13)*C(I,2)
      RB(I,64) = RB(I,64)*C(I,9)*C(I,2)
      RB(I,65) = RB(I,65)*C(I,2)*C(I,5)*C(I,11)
      RB(I,66) = RB(I,66)*C(I,11)*C(I,6)
      RB(I,67) = RB(I,67)*C(I,6)
      RB(I,68) = RB(I,68)*C(I,11)
      RB(I,69) = RB(I,69)*C(I,12)
      RB(I,70) = RB(I,70)*C(I,13)*C(I,11)
      RB(I,72) = RB(I,72)*C(I,1)
      RB(I,73) = RB(I,73)*C(I,5)
      RB(I,74) = RB(I,74)*C(I,6)
      RB(I,75) = RB(I,75)*C(I,7)
      RB(I,76) = RB(I,76)*C(I,8)
      RB(I,77) = RB(I,77)*C(I,18)*C(I,2)
      RB(I,78) = RB(I,78)*C(I,10)
      RB(I,79) = RB(I,79)*C(I,13)*C(I,2)
      RB(I,80) = RB(I,80)*C(I,6)
      RB(I,81) = RB(I,81)*C(I,6)
      RB(I,82) = RB(I,82)*C(I,3)
      RB(I,83) = RB(I,83)*C(I,5)*C(I,13)
      RB(I,84) = RB(I,84)*C(I,10)*C(I,4)
      RB(I,85) = RB(I,85)*C(I,5)
      RB(I,86) = RB(I,86)*C(I,10)*C(I,7)
      RB(I,87) = RB(I,87)*C(I,2)
      RB(I,88) = RB(I,88)*C(I,10)*C(I,11)
      RB(I,89) = RB(I,89)*C(I,19)
      RB(I,90) = RB(I,90)*C(I,10)
      RB(I,91) = RB(I,91)*C(I,15)*C(I,2)
      RB(I,92) = RB(I,92)*C(I,15)*C(I,2)
      RB(I,93) = RB(I,93)*C(I,16)
      RB(I,94) = RB(I,94)*C(I,2)
      RB(I,95) = RB(I,95)*C(I,15)*C(I,11)
      RB(I,96) = RB(I,96)*C(I,13)*C(I,1)
      RB(I,97) = RB(I,97)*C(I,9)*C(I,5)
      RB(I,98) = RB(I,98)*C(I,6)
      RB(I,99) = RB(I,99)*C(I,13)*C(I,5)
      RB(I,100) = RB(I,100)*C(I,13)*C(I,6)
      RB(I,101) = RB(I,101)*C(I,13)*C(I,7)
      RB(I,102) = RB(I,102)*C(I,9)*C(I,1)
      RB(I,103) = RB(I,103)*C(I,9)*C(I,5)
      RB(I,104) = RB(I,104)*C(I,9)*C(I,6)
      RB(I,105) = RB(I,105)*C(I,15)*C(I,2)
      RB(I,106) = RB(I,106)*C(I,9)*C(I,9)
      RB(I,107) = RB(I,107)*C(I,9)*C(I,9)
      RB(I,108) = RB(I,108)*C(I,11)
      RB(I,109) = RB(I,109)*C(I,2)*C(I,11)*C(I,11)
      RB(I,110) = RB(I,110)*C(I,5)*C(I,11)*C(I,11)
      RB(I,111) = RB(I,111)*C(I,14)*C(I,11)
      RB(I,112) = RB(I,112)*C(I,11)
      RB(I,113) = RB(I,113)*C(I,14)*C(I,11)*C(I,11)
      RB(I,115) = RB(I,115)*C(I,14)*C(I,2)
      RB(I,116) = RB(I,116)*C(I,17)*C(I,2)
      RB(I,117) = RB(I,117)*C(I,11)
      RB(I,118) = RB(I,118)*C(I,18)*C(I,2)
      RB(I,119) = RB(I,119)*C(I,9)*C(I,11)
      RB(I,120) = RB(I,120)*C(I,11)
      RB(I,121) = RB(I,121)*ctb(I,121)*C(I,20)
      RB(I,122) = RB(I,122)*C(I,14)*C(I,2)
      RB(I,123) = RB(I,123)*C(I,11)
      RB(I,124) = RB(I,124)*C(I,18)*C(I,2)
      RB(I,125) = RB(I,125)*C(I,12)
      RB(I,127) = RB(I,127)*C(I,17)*C(I,1)
      RB(I,128) = RB(I,128)*C(I,9)*C(I,11)
      RB(I,129) = RB(I,129)*C(I,17)*C(I,5)
      RB(I,130) = RB(I,130)*C(I,12)
      RB(I,131) = RB(I,131)*C(I,17)*C(I,6)
      RB(I,132) = RB(I,132)*C(I,15)
      RB(I,133) = RB(I,133)*C(I,14)*C(I,1)
      RB(I,134) = RB(I,134)*C(I,1)
      RB(I,135) = RB(I,135)*C(I,18)*C(I,2)
      RB(I,136) = RB(I,136)*C(I,9)*C(I,11)
      RB(I,137) = RB(I,137)*C(I,14)*C(I,6)
      RB(I,138) = RB(I,138)*C(I,14)*C(I,7)
      RB(I,139) = RB(I,139)*C(I,3)
      RB(I,140) = RB(I,140)*C(I,13)
      RB(I,141) = RB(I,141)*C(I,5)
      RB(I,142) = RB(I,142)*C(I,15)*C(I,7)
      RB(I,143) = RB(I,143)*C(I,15)*C(I,11)
      RB(I,144) = RB(I,144)*C(I,14)*C(I,10)
      RB(I,145) = RB(I,145)*C(I,21)
      RB(I,146) = RB(I,146)*C(I,20)*C(I,2)
      RB(I,147) = RB(I,147)*C(I,9)*C(I,11)
      RB(I,148) = RB(I,148)*C(I,19)
      RB(I,149) = RB(I,149)*C(I,9)
      RB(I,150) = RB(I,150)*C(I,18)*C(I,1)
      RB(I,151) = RB(I,151)*C(I,18)*C(I,5)
      RB(I,152) = RB(I,152)*C(I,18)*C(I,6)
      RB(I,153) = RB(I,153)*C(I,18)*C(I,7)
      RB(I,154) = RB(I,154)*C(I,13)*C(I,11)*C(I,5)
      RB(I,155) = RB(I,155)*C(I,1)
      RB(I,157) = RB(I,157)*C(I,1)
      RB(I,158) = RB(I,158)*C(I,5)
      RB(I,159) = RB(I,159)*C(I,9)
      RB(I,160) = RB(I,160)*C(I,13)
      RB(I,161) = RB(I,161)*C(I,6)
      RB(I,162) = RB(I,162)*C(I,7)
      RB(I,163) = RB(I,163)*C(I,19)*C(I,5)
      RB(I,164) = RB(I,164)*C(I,11)
      RB(I,165) = RB(I,165)*C(I,20)*C(I,2)
      RB(I,166) = RB(I,166)*C(I,10)
      RB(I,167) = RB(I,167)*C(I,20)*C(I,2)
      RB(I,168) = RB(I,168)*C(I,10)
      RB(I,170) = RB(I,170)*C(I,16)
      RB(I,171) = RB(I,171)*C(I,15)*C(I,1)
      RB(I,172) = RB(I,172)*C(I,9)*C(I,13)
      RB(I,173) = RB(I,173)*C(I,19)*C(I,2)
      RB(I,174) = RB(I,174)*C(I,15)*C(I,7)
      RB(I,175) = RB(I,175)*C(I,16)*C(I,4)
      RB(I,176) = RB(I,176)*C(I,15)*C(I,8)
      RB(I,177) = RB(I,177)*C(I,9)*C(I,13)*C(I,5)
      RB(I,178) = RB(I,178)*C(I,16)*C(I,7)
      RB(I,179) = RB(I,179)*C(I,16)*C(I,11)
      RB(I,180) = RB(I,180)*C(I,1)
      RB(I,181) = RB(I,181)*C(I,5)
      RB(I,182) = RB(I,182)*C(I,6)
      RB(I,183) = RB(I,183)*C(I,9)
      RB(I,184) = RB(I,184)*C(I,10)
      RB(I,185) = RB(I,185)*C(I,21)
      RB(I,186) = RB(I,186)*C(I,10)
      RB(I,187) = RB(I,187)*C(I,21)*C(I,4)
      RB(I,188) = RB(I,188)*C(I,5)*C(I,13)
      RB(I,189) = RB(I,189)*C(I,21)*C(I,11)
      RB(I,191) = RB(I,191)*C(I,15)*C(I,9)
      RB(I,192) = RB(I,192)*C(I,20)*C(I,1)
      RB(I,193) = RB(I,193)*C(I,18)*C(I,9)*C(I,2)
      RB(I,195) = RB(I,195)*C(I,20)*C(I,5)
      RB(I,196) = RB(I,196)*C(I,20)*C(I,6)
      RB(I,197) = RB(I,197)*C(I,20)*C(I,8)
      RB(I,198) = RB(I,198)*C(I,20)*C(I,10)
      RB(I,199) = RB(I,199)*C(I,9)
      RB(I,200) = RB(I,200)*C(I,21)*C(I,1)
      RB(I,201) = RB(I,201)*C(I,13)
      RB(I,202) = RB(I,202)*C(I,21)*C(I,6)
      RB(I,203) = RB(I,203)*C(I,21)*C(I,7)
      RB(I,204) = RB(I,204)*C(I,5)*C(I,13)
      RB(I,205) = RB(I,205)*C(I,10)*C(I,21)
      RB(I,206) = RB(I,206)*C(I,20)*C(I,9)
     ENDDO
C
      RETURN
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE RDWDOT_I (VL,RKF, RKR, WDOT)
        USE chemkin_m, only : MAXVL
        IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
C
      DIMENSION RKF(MAXVL,*), RKR(MAXVL,*), WDOT(MAXVL,*)
      DIMENSION ROP(MAXVL,206)
        INTEGER VL
C
      DO I = 1, 206
         ROP(:VL,I) = RKF(:VL,I) - RKR(:VL,I)
      ENDDO
C
      DO I=1,VL
      ROP(I,5) = ROP(I,5) + ROP(I,6)
      ROP(I,5) = ROP(I,5) + ROP(I,7)
      ROP(I,5) = ROP(I,5) + ROP(I,8)
      ROP(I,12) = ROP(I,12) + ROP(I,13)
      ROP(I,12) = ROP(I,12) + ROP(I,14)
      ROP(I,12) = ROP(I,12) + ROP(I,15)
      ROP(I,22) = ROP(I,22) + ROP(I,23)
      ROP(I,27) = ROP(I,27) + ROP(I,28)
      ROP(I,59) = ROP(I,59) + ROP(I,67)
      ROP(I,59) = ROP(I,59) + ROP(I,68)
      ROP(I,59) = ROP(I,59) + ROP(I,69)
      ROP(I,114) = ROP(I,114) - ROP(I,122)
C
      WDOT(I,1) = -ROP(I,2) -ROP(I,3) +ROP(I,5) +ROP(I,18) 
     *          +ROP(I,24) -ROP(I,31) -ROP(I,36) +ROP(I,42) 
     *          -ROP(I,49) +ROP(I,58) +ROP(I,60) +ROP(I,61) 
     *          -ROP(I,64) +ROP(I,72) +ROP(I,96) +ROP(I,102) 
     *          +ROP(I,127) +ROP(I,133) +ROP(I,134) +ROP(I,150) 
     *          +ROP(I,155) +ROP(I,157) +ROP(I,171) +ROP(I,180) 
     *          +ROP(I,192) +ROP(I,200) 
      WDOT(I,2) = -ROP(I,1) +ROP(I,2) +ROP(I,3) -ROP(I,5) -ROP(I,5) 
     *          -ROP(I,9) -ROP(I,10) -ROP(I,12) -ROP(I,17) 
     *          -ROP(I,18) -ROP(I,19) -ROP(I,24) -ROP(I,25) 
     *          +ROP(I,30) +ROP(I,34) +ROP(I,35) +ROP(I,36) 
     *          +ROP(I,37) -ROP(I,41) -ROP(I,42) +ROP(I,44) 
     *          +ROP(I,46) -ROP(I,48) +ROP(I,49) +ROP(I,50) 
     *          +ROP(I,52) +ROP(I,52) +ROP(I,53) +ROP(I,57) 
     *          -ROP(I,60) +ROP(I,62) +ROP(I,63) +ROP(I,64) 
     *          +ROP(I,65) -ROP(I,71) -ROP(I,72) +ROP(I,77) 
     *          -ROP(I,78) +ROP(I,79) +ROP(I,87) +ROP(I,91) 
     *          +ROP(I,92) +ROP(I,94) -ROP(I,96) -ROP(I,97) 
     *          -ROP(I,98) -ROP(I,102) +ROP(I,105) -ROP(I,108) 
     *          +ROP(I,109) +ROP(I,115) +ROP(I,116) +ROP(I,118) 
     *          +ROP(I,124) -ROP(I,126) -ROP(I,127) -ROP(I,128) 
     *          -ROP(I,132) -ROP(I,133) -ROP(I,134) +ROP(I,135) 
     *          +ROP(I,146) -ROP(I,148) -ROP(I,149) -ROP(I,150) 
     *          -ROP(I,156) -ROP(I,157) +ROP(I,165) +ROP(I,167) 
     *          -ROP(I,170) -ROP(I,171) +ROP(I,173) -ROP(I,180) 
     *          -ROP(I,185) -ROP(I,186) -ROP(I,190) -ROP(I,191) 
     *          -ROP(I,192) +ROP(I,193) -ROP(I,199) -ROP(I,200) 
      WDOT(I,3) = +ROP(I,1) -ROP(I,2) +ROP(I,4) -ROP(I,10) 
     *          -ROP(I,11) -ROP(I,11) +ROP(I,17) -ROP(I,20) 
     *          -ROP(I,26) -ROP(I,29) +ROP(I,32) -ROP(I,34) 
     *          +ROP(I,38) -ROP(I,43) -ROP(I,44) -ROP(I,50) 
     *          -ROP(I,61) -ROP(I,62) -ROP(I,73) -ROP(I,79) 
     *          +ROP(I,82) -ROP(I,99) -ROP(I,103) -ROP(I,109) 
     *          -ROP(I,116) -ROP(I,117) -ROP(I,123) -ROP(I,129) 
     *          -ROP(I,130) -ROP(I,135) -ROP(I,136) +ROP(I,139) 
     *          -ROP(I,151) -ROP(I,158) -ROP(I,159) -ROP(I,160) 
     *          -ROP(I,172) -ROP(I,173) -ROP(I,181) -ROP(I,193) 
     *          -ROP(I,194) -ROP(I,195) -ROP(I,201) 
      WDOT(I,4) = -ROP(I,1) +ROP(I,11) -ROP(I,12) +ROP(I,18) 
     *          +ROP(I,20) +ROP(I,21) +ROP(I,22) -ROP(I,32) 
     *          -ROP(I,38) -ROP(I,47) -ROP(I,51) -ROP(I,52) 
     *          -ROP(I,65) -ROP(I,66) -ROP(I,75) -ROP(I,82) 
     *          -ROP(I,83) +ROP(I,84) -ROP(I,101) -ROP(I,110) 
     *          -ROP(I,125) -ROP(I,138) -ROP(I,139) -ROP(I,140) 
     *          -ROP(I,153) -ROP(I,154) -ROP(I,162) -ROP(I,174) 
     *          +ROP(I,175) +ROP(I,187) -ROP(I,203) 
      WDOT(I,5) = +ROP(I,1) +ROP(I,2) -ROP(I,3) -ROP(I,4) -ROP(I,4) 
     *          -ROP(I,9) +ROP(I,10) -ROP(I,16) -ROP(I,16) 
     *          +ROP(I,19) +ROP(I,19) +ROP(I,20) -ROP(I,21) 
     *          +ROP(I,25) +ROP(I,26) -ROP(I,27) -ROP(I,30) 
     *          +ROP(I,33) -ROP(I,35) +ROP(I,43) -ROP(I,45) 
     *          +ROP(I,51) -ROP(I,53) -ROP(I,54) +ROP(I,55) 
     *          -ROP(I,63) +ROP(I,65) +ROP(I,73) -ROP(I,74) 
     *          -ROP(I,80) -ROP(I,81) +ROP(I,83) +ROP(I,85) 
     *          +ROP(I,97) +ROP(I,99) -ROP(I,100) +ROP(I,103) 
     *          -ROP(I,104) +ROP(I,110) -ROP(I,118) -ROP(I,119) 
     *          -ROP(I,124) +ROP(I,129) -ROP(I,131) -ROP(I,137) 
     *          +ROP(I,141) +ROP(I,151) -ROP(I,152) +ROP(I,154) 
     *          +ROP(I,158) -ROP(I,161) +ROP(I,163) +ROP(I,177) 
     *          +ROP(I,181) -ROP(I,182) +ROP(I,188) +ROP(I,195) 
     *          -ROP(I,196) -ROP(I,202) +ROP(I,204) 
      WDOT(I,6) = +ROP(I,3) +ROP(I,4) +ROP(I,9) +ROP(I,17) 
     *          +ROP(I,21) +ROP(I,25) +ROP(I,27) -ROP(I,37) 
     *          +ROP(I,45) +ROP(I,54) +ROP(I,66) +ROP(I,74) 
     *          +ROP(I,80) +ROP(I,81) +ROP(I,98) +ROP(I,100) 
     *          +ROP(I,104) +ROP(I,131) +ROP(I,137) +ROP(I,152) 
     *          +ROP(I,161) +ROP(I,182) +ROP(I,196) +ROP(I,202) 
      WDOT(I,7) = +ROP(I,12) -ROP(I,17) -ROP(I,18) -ROP(I,19) 
     *          -ROP(I,20) -ROP(I,21) -ROP(I,22) -ROP(I,22) 
     *          +ROP(I,24) +ROP(I,26) +ROP(I,27) -ROP(I,33) 
     *          +ROP(I,47) -ROP(I,55) +ROP(I,75) -ROP(I,76) 
     *          -ROP(I,84) -ROP(I,85) +ROP(I,86) +ROP(I,101) 
     *          +ROP(I,138) -ROP(I,141) +ROP(I,142) +ROP(I,153) 
     *          +ROP(I,162) -ROP(I,163) +ROP(I,174) -ROP(I,175) 
     *          -ROP(I,176) -ROP(I,177) +ROP(I,178) -ROP(I,187) 
     *          -ROP(I,188) -ROP(I,197) +ROP(I,203) -ROP(I,204) 
      WDOT(I,8) = +ROP(I,16) +ROP(I,22) -ROP(I,24) -ROP(I,25) 
     *          -ROP(I,26) -ROP(I,27) +ROP(I,76) -ROP(I,86) 
     *          -ROP(I,142) +ROP(I,176) -ROP(I,178) +ROP(I,197) 
      WDOT(I,9) = +ROP(I,48) +ROP(I,49) +ROP(I,64) -ROP(I,78) 
     *          -ROP(I,79) -ROP(I,80) -ROP(I,81) -ROP(I,82) 
     *          -ROP(I,83) -ROP(I,84) -ROP(I,85) -ROP(I,86) 
     *          -ROP(I,87) -ROP(I,88) -ROP(I,89) -ROP(I,90) 
     *          -ROP(I,91) -ROP(I,92) -ROP(I,93) -ROP(I,93) 
     *          -ROP(I,94) -ROP(I,94) -ROP(I,95) +ROP(I,97) 
     *          +ROP(I,102) +ROP(I,103) +ROP(I,104) +ROP(I,106) 
     *          +ROP(I,106) 
     *          +ROP(I,107) +ROP(I,107) +ROP(I,119) -ROP(I,121) 
     *          +ROP(I,128) +ROP(I,136) -ROP(I,144) -ROP(I,145) 
     *          -ROP(I,146) +ROP(I,147) +ROP(I,149) +ROP(I,159) 
     *          -ROP(I,168) -ROP(I,169) +ROP(I,172) +ROP(I,177) 
     *          +ROP(I,183) -ROP(I,184) +ROP(I,191) +ROP(I,193) 
     *          -ROP(I,198) +ROP(I,199) -ROP(I,205) +ROP(I,206) 
      WDOT(I,10) = +ROP(I,78) +ROP(I,84) +ROP(I,86) +ROP(I,88) 
     *          +ROP(I,90) -ROP(I,102) -ROP(I,103) -ROP(I,104) 
     *          -ROP(I,105) -ROP(I,106) -ROP(I,107) +ROP(I,144) 
     *          +ROP(I,166) +ROP(I,168) +ROP(I,184) +ROP(I,186) 
     *          +ROP(I,198) +ROP(I,205) 
      WDOT(I,11) = -ROP(I,29) -ROP(I,30) -ROP(I,31) -ROP(I,32) 
     *          -ROP(I,33) +ROP(I,34) -ROP(I,39) +ROP(I,40) 
     *          +ROP(I,42) +ROP(I,43) +ROP(I,45) +ROP(I,46) 
     *          +ROP(I,47) -ROP(I,56) +ROP(I,61) +ROP(I,65) 
     *          +ROP(I,66) +ROP(I,70) +ROP(I,88) +ROP(I,95) 
     *          +ROP(I,108) +ROP(I,109) +ROP(I,109) +ROP(I,110) 
     *          +ROP(I,110) 
     *          +ROP(I,111) +ROP(I,112) +ROP(I,113) +ROP(I,113) 
     *          +ROP(I,117) +ROP(I,119) +ROP(I,120) +ROP(I,123) 
     *          +ROP(I,128) +ROP(I,136) +ROP(I,143) +ROP(I,147) 
     *          +ROP(I,154) +ROP(I,164) +ROP(I,179) +ROP(I,189) 
      WDOT(I,12) = +ROP(I,29) +ROP(I,30) +ROP(I,32) +ROP(I,33) 
     *          -ROP(I,40) +ROP(I,44) +ROP(I,52) -ROP(I,70) 
     *          +ROP(I,125) +ROP(I,130) 
      WDOT(I,13) = +ROP(I,31) +ROP(I,37) +ROP(I,41) +ROP(I,53) 
     *          +ROP(I,55) +ROP(I,63) +ROP(I,70) -ROP(I,71) 
     *          -ROP(I,72) -ROP(I,73) -ROP(I,74) -ROP(I,75) 
     *          -ROP(I,76) -ROP(I,77) +ROP(I,79) +ROP(I,83) 
     *          -ROP(I,90) +ROP(I,96) +ROP(I,99) +ROP(I,100) 
     *          +ROP(I,101) +ROP(I,140) +ROP(I,154) +ROP(I,160) 
     *          +ROP(I,172) +ROP(I,177) +ROP(I,188) +ROP(I,201) 
     *          +ROP(I,204) 
      WDOT(I,14) = +ROP(I,57) +ROP(I,58) +ROP(I,111) +ROP(I,113) 
     *          -ROP(I,114) +ROP(I,115) -ROP(I,116) -ROP(I,117) 
     *          -ROP(I,118) -ROP(I,119) -ROP(I,120) -ROP(I,121) 
     *          +ROP(I,133) +ROP(I,137) +ROP(I,138) +ROP(I,144) 
      WDOT(I,15) = +ROP(I,91) +ROP(I,92) +ROP(I,95) +ROP(I,105) 
     *          +ROP(I,132) +ROP(I,142) +ROP(I,143) -ROP(I,155) 
     *          -ROP(I,156) -ROP(I,157) -ROP(I,158) -ROP(I,159) 
     *          -ROP(I,160) -ROP(I,161) -ROP(I,162) -ROP(I,163) 
     *          -ROP(I,164) -ROP(I,165) -ROP(I,166) -ROP(I,167) 
     *          -ROP(I,168) -ROP(I,169) +ROP(I,171) +ROP(I,174) 
     *          +ROP(I,176) +ROP(I,191) 
      WDOT(I,16) = +ROP(I,93) +ROP(I,170) +ROP(I,175) +ROP(I,178) 
     *          +ROP(I,179) -ROP(I,180) -ROP(I,181) -ROP(I,182) 
     *          -ROP(I,183) -ROP(I,184) 
      WDOT(I,17) = +ROP(I,39) -ROP(I,95) -ROP(I,108) -ROP(I,109) 
     *          -ROP(I,110) -ROP(I,111) -ROP(I,112) -ROP(I,113) 
     *           -ROP(I,113) 
     *          +ROP(I,116) +ROP(I,127) +ROP(I,129) +ROP(I,131) 
      WDOT(I,18) = +ROP(I,56) +ROP(I,77) +ROP(I,118) +ROP(I,124) 
     *          -ROP(I,126) -ROP(I,127) -ROP(I,128) -ROP(I,129) 
     *          -ROP(I,130) -ROP(I,131) +ROP(I,135) +ROP(I,150) 
     *          +ROP(I,151) +ROP(I,152) +ROP(I,153) +ROP(I,193) 
      WDOT(I,19) = +ROP(I,89) +ROP(I,148) +ROP(I,163) +ROP(I,173) 
      WDOT(I,20) = +ROP(I,121) +ROP(I,146) +ROP(I,165) +ROP(I,167) 
     *          -ROP(I,185) -ROP(I,186) -ROP(I,187) -ROP(I,188) 
     *          -ROP(I,189) +ROP(I,192) +ROP(I,195) +ROP(I,196) 
     *          +ROP(I,197) +ROP(I,198) +ROP(I,206) 
      WDOT(I,21) = +ROP(I,145) +ROP(I,185) +ROP(I,187) +ROP(I,189) 
     *          -ROP(I,190) -ROP(I,191) -ROP(I,192) -ROP(I,193) 
     *          -ROP(I,194) -ROP(I,195) -ROP(I,196) -ROP(I,197) 
     *          -ROP(I,198) +ROP(I,200) +ROP(I,202) +ROP(I,203) 
     *          +ROP(I,205) 
      WDOT(I,22) = 0.0
      ENDDO
      END
C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE QSSA_I(VL,RF, RB, XQ)
        USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      DIMENSION RF(MAXVL,*), RB(MAXVL,*), XQ(MAXVL,*)
        INTEGER VL
C
      DO I=1,VL
      RF(I,57) = 0.D0
      RF(I,58) = 0.D0
      RF(I,143) = 0.D0
      RF(I,179) = 0.D0
      RB(I,194) = 0.D0
      RF(I,206) = 0.D0
C
C     CH
      DEN = +RF(I, 34) +RF(I, 35) +RF(I, 36) +RF(I, 37) +RF(I, 38) 
     *  +RF(I, 39) +RF(I, 40) +RF(I, 77) +RF(I, 87) +RF(I,105) 
     *           +RF(I,111) 
     *  +RB(I, 54) +RB(I, 60) 
      A1_0 = ( +RB(I, 34) +RB(I, 37) +RB(I, 39) +RB(I, 57) +RB(I, 77) 
     *  +RB(I,105) +RB(I,111) )/DEN
      A1_2 = ( +RB(I, 36) +RF(I, 54) )/DEN
      A1_3 = ( +RF(I, 60) )/DEN
      A1_4 = ( +RB(I, 35) +RB(I, 38) +RB(I, 40) )/DEN
      A1_7 = ( +RB(I, 87) )/DEN
C     CH2
      DEN = +RF(I, 48) +RF(I, 49) +RF(I, 50) +RF(I, 51) +RF(I, 52) 
     *  +RF(I, 53) +RF(I, 54) +RF(I, 55) +RF(I, 56) +RF(I, 91) 
     *           +RF(I,106) 
     *  +RF(I,112) +RF(I,165) +RB(I, 36) +RB(I, 59) +RB(I, 67) 
     *           +RB(I, 68) 
     *  +RB(I, 69) +RB(I, 80) +RB(I,117) +RB(I,123) +RB(I,125) 
     *           +RB(I,130) 
     *  +RB(I,160) 
      A2_0 = ( +RB(I, 48) +RB(I, 49) +RB(I, 52) +RB(I, 53) +RB(I, 55) 
     *  +RB(I, 56) +RB(I, 57) +RB(I, 58) +RB(I, 58) +RF(I, 80) 
     *           +RB(I, 91) 
     *  +RB(I,106) +RF(I,117) +RF(I,130) +RF(I,160) +RB(I,165) )/DEN
      A2_1 = ( +RF(I, 36) +RB(I, 54) )/DEN
      A2_3 = ( +RF(I, 59) +RF(I, 67) +RF(I, 68) +RF(I, 69) )/DEN
      A2_4 = ( +RB(I, 50) +RB(I, 51) )/DEN
      A2_6 = ( +RF(I,123) +RF(I,125) )/DEN
      A2_7 = ( +RB(I,112) )/DEN
C     CH2*
      DEN = +RF(I, 59) +RF(I, 60) +RF(I, 61) +RF(I, 62) +RF(I, 63) 
     *  +RF(I, 64) +RF(I, 65) +RF(I, 66) +RF(I, 67) +RF(I, 68)
     *            +RF(I, 69) 
     *  +RF(I, 70) +RF(I, 92) +RF(I,107) +RF(I,166) +RF(I,167)
     *            +RF(I,183) 
     *  +RB(I, 81) +RB(I, 98) +RB(I,108) 
      A3_0 = ( +RB(I, 61) +RB(I, 63) +RB(I, 64) +RB(I, 65) +RB(I, 66) 
     *  +RB(I, 70) +RF(I, 81) +RB(I, 92) +RB(I,107) +RF(I,108) 
     *           +RB(I,167) )/DEN
      A3_1 = ( +RB(I, 60) )/DEN
      A3_2 = ( +RB(I, 59) +RB(I, 67) +RB(I, 68) +RB(I, 69) )/DEN
      A3_4 = ( +RB(I, 62) )/DEN
      A3_5 = ( +RF(I, 98) )/DEN
      A3_6 = ( +RB(I,166) )/DEN
      A3_8 = ( +RB(I,183) )/DEN
C     HCO
      DEN = +RF(I, 41) +RF(I, 42) +RF(I, 43) +RF(I, 44) +RF(I, 45) 
     *  +RF(I, 46) +RF(I, 47) +RF(I, 88) +RF(I, 89) +RF(I,120) 
     *           +RF(I,164) 
     *  +RF(I,189) +RB(I, 35) +RB(I, 38) +RB(I, 40) +RB(I, 50) 
     *           +RB(I, 51) 
     *  +RB(I, 62) +RB(I, 72) +RB(I, 73) +RB(I, 74) +RB(I, 75) 
     *           +RB(I, 76) 
     *  +RB(I, 90) +RB(I,140) +RB(I,149) +RB(I,159) 
      A4_0 = ( +RB(I, 41) +RB(I, 42) +RB(I, 43) +RB(I, 44) +RB(I, 45) 
     *  +RB(I, 46) +RB(I, 47) +RF(I, 72) +RF(I, 73) +RF(I, 74) 
     *           +RF(I, 75) 
     *  +RF(I, 76) +RB(I, 88) +RB(I, 89) +RF(I, 90) +RB(I,143) 
     *           +RF(I,159) 
     *  +RB(I,179) +RB(I,189) +RF(I,194) )/DEN
      A4_1 = ( +RF(I, 35) +RF(I, 38) +RF(I, 40) )/DEN
      A4_2 = ( +RF(I, 50) +RF(I, 51) )/DEN
      A4_3 = ( +RF(I, 62) )/DEN
      A4_7 = ( +RB(I,120) +RF(I,140) )/DEN
      A4_8 = ( +RB(I,164) )/DEN
      A4_9 = ( +RF(I,149) )/DEN
C     CH3O
      DEN = +RF(I, 96) +RF(I, 97) +RF(I, 98) +RF(I, 99) +RF(I,100) 
     *  +RF(I,101) +RB(I, 71) +RB(I, 82) +RB(I, 85) 
      A5_0 = ( +RF(I, 71) +RF(I, 82) +RF(I, 85) +RB(I, 96) +RB(I, 97) 
     *  +RB(I, 99) +RB(I,100) +RB(I,101) )/DEN
      A5_3 = ( +RB(I, 98) )/DEN
C     H2CC
      DEN = +RF(I,122) +RF(I,123) +RF(I,124) +RF(I,125) +RB(I,114) 
     *  +RB(I,134) +RB(I,155) +RB(I,166) +RB(I,186) 
      A6_0 = ( +RF(I,114) +RB(I,122) +RB(I,124) +RF(I,155) 
     *           +RF(I,186) )/DEN
      A6_2 = ( +RB(I,123) +RB(I,125) )/DEN
      A6_3 = ( +RF(I,166) )/DEN
      A6_7 = ( +RF(I,134) )/DEN
C     C2H3
      DEN = +RF(I,115) +RF(I,132) +RF(I,133) +RF(I,134) +RF(I,135) 
     *  +RF(I,136) +RF(I,137) +RF(I,138) +RF(I,139) +RF(I,140) 
     *           +RF(I,141) 
     *  +RF(I,142) +RF(I,144) +RF(I,145) +RF(I,146) +RB(I, 87) 
     *           +RB(I,112) 
     *  +RB(I,120) +RB(I,157) +RB(I,158) +RB(I,161) +RB(I,162) 
     *           +RB(I,168) 
     *  +RB(I,188) 
      A7_0 = ( +RB(I,115) +RB(I,132) +RB(I,133) +RB(I,135) +RB(I,136) 
     *  +RB(I,137) +RB(I,138) +RB(I,142) +RB(I,143) +RB(I,144) 
     *           +RB(I,145) 
     *  +RB(I,146) +RF(I,157) +RF(I,158) +RF(I,161) +RF(I,162) 
     *           +RF(I,168) 
     *  +RF(I,188) +RB(I,206) )/DEN
      A7_1 = ( +RF(I, 87) )/DEN
      A7_2 = ( +RF(I,112) )/DEN
      A7_4 = ( +RF(I,120) +RB(I,140) )/DEN
      A7_6 = ( +RB(I,134) )/DEN
      A7_9 = ( +RB(I,139) +RB(I,141) )/DEN
C     C2H5
      DEN = +RF(I,170) +RF(I,171) +RF(I,172) +RF(I,173) +RF(I,174) 
     *  +RF(I,175) +RF(I,176) +RF(I,177) +RF(I,178) +RB(I, 94) 
     *           +RB(I,156) 
     *  +RB(I,164) +RB(I,180) +RB(I,181) +RB(I,182) +RB(I,183) 
     *           +RB(I,184) 
     *  +RB(I,199) +RB(I,201) +RB(I,204) 
      A8_0 = ( +RF(I, 94) +RF(I,156) +RB(I,170) +RB(I,171) +RB(I,172) 
     *  +RB(I,173) +RB(I,174) +RB(I,175) +RB(I,176) +RB(I,177) 
     *           +RB(I,178) 
     *  +RB(I,179) +RF(I,180) +RF(I,181) +RF(I,182) +RF(I,184) 
     *           +RF(I,194) 
     *  +RB(I,206) )/DEN
      A8_3 = ( +RF(I,183) )/DEN
      A8_4 = ( +RF(I,164) )/DEN
      A8_10 = ( +RF(I,199) +RF(I,201) +RF(I,204) )/DEN
C     CH2CHO
      DEN = +RF(I,147) +RF(I,148) +RF(I,149) +RF(I,150) +RF(I,151) 
     *  +RF(I,152) +RF(I,153) +RF(I,154) +RB(I,126) +RB(I,139)
     *            +RB(I,141) 
      A9_0 = ( +RF(I,126) +RB(I,147) +RB(I,148) +RB(I,150) +RB(I,151) 
     *  +RB(I,152) +RB(I,153) +RB(I,154) )/DEN
      A9_4 = ( +RB(I,149) )/DEN
      A9_7 = ( +RF(I,139) +RF(I,141) )/DEN
C     nC3H7
      DEN = +RF(I,199) +RF(I,200) +RF(I,201) +RF(I,202) +RF(I,203) 
     *  +RF(I,204) +RF(I,205) +RB(I,169) +RB(I,190) 
      A10_0 = ( +RF(I,169) +RF(I,190) +RB(I,200) +RB(I,202) +RB(I,203) 
     *  +RB(I,205) )/DEN
      A10_8 = ( +RB(I,199) +RB(I,201) +RB(I,204) )/DEN
C
      A8_0 = A8_0 + A8_10*A10_0
      DEN = 1 -A8_10*A10_8
      A8_0 = A8_0/DEN
      A8_4 = A8_4/DEN
      A8_3 = A8_3/DEN
      A3_0 = A3_0 + A3_5*A5_0
      DEN = 1 -A3_5*A5_3
      A3_0 = A3_0/DEN
      A3_4 = A3_4/DEN
      A3_2 = A3_2/DEN
      A3_1 = A3_1/DEN
      A3_8 = A3_8/DEN
      A3_6 = A3_6/DEN
      A4_0 = A4_0 + A4_9*A9_0
      A4_7 = A4_7 + A4_9*A9_7
      DEN = 1 -A4_9*A9_4
      A4_0 = A4_0/DEN
      A4_3 = A4_3/DEN
      A4_7 = A4_7/DEN
      A4_2 = A4_2/DEN
      A4_1 = A4_1/DEN
      A4_8 = A4_8/DEN
      A7_0 = A7_0 + A7_9*A9_0
      A7_4 = A7_4 + A7_9*A9_4
      DEN = 1 -A7_9*A9_7
      A7_0 = A7_0/DEN
      A7_4 = A7_4/DEN
      A7_2 = A7_2/DEN
      A7_1 = A7_1/DEN
      A7_6 = A7_6/DEN
      A3_0 = A3_0 + A3_6*A6_0
      A3_7 = A3_6*A6_7
      A3_2 = A3_2 + A3_6*A6_2
      DEN = 1 -A3_6*A6_3
      A3_0 = A3_0/DEN
      A3_4 = A3_4/DEN
      A3_7 = A3_7/DEN
      A3_2 = A3_2/DEN
      A3_1 = A3_1/DEN
      A3_8 = A3_8/DEN
      A7_0 = A7_0 + A7_6*A6_0
      A7_3 = A7_6*A6_3
      A7_2 = A7_2 + A7_6*A6_2
      DEN = 1 -A7_6*A6_7
      A7_0 = A7_0/DEN
      A7_4 = A7_4/DEN
      A7_3 = A7_3/DEN
      A7_2 = A7_2/DEN
      A7_1 = A7_1/DEN
      A2_0 = A2_0 + A2_6*A6_0
      A2_3 = A2_3 + A2_6*A6_3
      A2_7 = A2_7 + A2_6*A6_7
      DEN = 1 -A2_6*A6_2
      A2_0 = A2_0/DEN
      A2_4 = A2_4/DEN
      A2_3 = A2_3/DEN
      A2_7 = A2_7/DEN
      A2_1 = A2_1/DEN
      A4_0 = A4_0 + A4_8*A8_0
      A4_3 = A4_3 + A4_8*A8_3
      DEN = 1 -A4_8*A8_4
      A4_0 = A4_0/DEN
      A4_3 = A4_3/DEN
      A4_7 = A4_7/DEN
      A4_2 = A4_2/DEN
      A4_1 = A4_1/DEN
      A3_0 = A3_0 + A3_8*A8_0
      A3_4 = A3_4 + A3_8*A8_4
      DEN = 1 -A3_8*A8_3
      A3_0 = A3_0/DEN
      A3_4 = A3_4/DEN
      A3_7 = A3_7/DEN
      A3_2 = A3_2/DEN
      A3_1 = A3_1/DEN
      A4_0 = A4_0 + A4_1*A1_0
      A4_3 = A4_3 + A4_1*A1_3
      A4_7 = A4_7 + A4_1*A1_7
      A4_2 = A4_2 + A4_1*A1_2
      DEN = 1 -A4_1*A1_4
      A4_0 = A4_0/DEN
      A4_3 = A4_3/DEN
      A4_7 = A4_7/DEN
      A4_2 = A4_2/DEN
      A3_0 = A3_0 + A3_1*A1_0
      A3_4 = A3_4 + A3_1*A1_4
      A3_7 = A3_7 + A3_1*A1_7
      A3_2 = A3_2 + A3_1*A1_2
      DEN = 1 -A3_1*A1_3
      A3_0 = A3_0/DEN
      A3_4 = A3_4/DEN
      A3_7 = A3_7/DEN
      A3_2 = A3_2/DEN
      A7_0 = A7_0 + A7_1*A1_0
      A7_4 = A7_4 + A7_1*A1_4
      A7_3 = A7_3 + A7_1*A1_3
      A7_2 = A7_2 + A7_1*A1_2
      DEN = 1 -A7_1*A1_7
      A7_0 = A7_0/DEN
      A7_4 = A7_4/DEN
      A7_3 = A7_3/DEN
      A7_2 = A7_2/DEN
      A2_0 = A2_0 + A2_1*A1_0
      A2_4 = A2_4 + A2_1*A1_4
      A2_3 = A2_3 + A2_1*A1_3
      A2_7 = A2_7 + A2_1*A1_7
      DEN = 1 -A2_1*A1_2
      A2_0 = A2_0/DEN
      A2_4 = A2_4/DEN
      A2_3 = A2_3/DEN
      A2_7 = A2_7/DEN
      A4_0 = A4_0 + A4_2*A2_0
      A4_3 = A4_3 + A4_2*A2_3
      A4_7 = A4_7 + A4_2*A2_7
      DEN = 1 -A4_2*A2_4
      A4_0 = A4_0/DEN
      A4_3 = A4_3/DEN
      A4_7 = A4_7/DEN
      A3_0 = A3_0 + A3_2*A2_0
      A3_4 = A3_4 + A3_2*A2_4
      A3_7 = A3_7 + A3_2*A2_7
      DEN = 1 -A3_2*A2_3
      A3_0 = A3_0/DEN
      A3_4 = A3_4/DEN
      A3_7 = A3_7/DEN
      A7_0 = A7_0 + A7_2*A2_0
      A7_4 = A7_4 + A7_2*A2_4
      A7_3 = A7_3 + A7_2*A2_3
      DEN = 1 -A7_2*A2_7
      A7_0 = A7_0/DEN
      A7_4 = A7_4/DEN
      A7_3 = A7_3/DEN
      A4_0 = A4_0 + A4_7*A7_0
      A4_3 = A4_3 + A4_7*A7_3
      DEN = 1 -A4_7*A7_4
      A4_0 = A4_0/DEN
      A4_3 = A4_3/DEN
      A3_0 = A3_0 + A3_7*A7_0
      A3_4 = A3_4 + A3_7*A7_4
      DEN = 1 -A3_7*A7_3
      A3_0 = A3_0/DEN
      A3_4 = A3_4/DEN
      A4_0 = A4_0 + A4_3*A3_0
      DEN = 1 -A4_3*A3_4
      A4_0 = A4_0/DEN
      XQ(I,4) = A4_0
      XQ(I,3) = A3_0 +A3_4*XQ(I,4)
      XQ(I,7) = A7_0 +A7_4*XQ(I,4) +A7_3*XQ(I,3)
      XQ(I,2) = A2_0 +A2_4*XQ(I,4) +A2_3*XQ(I,3) +A2_7*XQ(I,7)
      XQ(I,1) = A1_0 +A1_4*XQ(I,4) +A1_3*XQ(I,3) +A1_7*XQ(I,7) 
     *           +A1_2*XQ(I,2)
      XQ(I,8) = A8_0 +A8_4*XQ(I,4) +A8_3*XQ(I,3)
      XQ(I,6) = A6_0 +A6_3*XQ(I,3) +A6_7*XQ(I,7) +A6_2*XQ(I,2)
      XQ(I,9) = A9_0 +A9_4*XQ(I,4) +A9_7*XQ(I,7)
      XQ(I,5) = A5_0 +A5_3*XQ(I,3)
      XQ(I,10) = A10_0 +A10_8*XQ(I,8)
C
      RF(I, 34) = RF(I, 34)*XQ(I, 1)
      RF(I, 35) = RF(I, 35)*XQ(I, 1)
      RB(I, 35) = RB(I, 35)*XQ(I, 4)
      RF(I, 36) = RF(I, 36)*XQ(I, 1)
      RB(I, 36) = RB(I, 36)*XQ(I, 2)
      RF(I, 37) = RF(I, 37)*XQ(I, 1)
      RF(I, 38) = RF(I, 38)*XQ(I, 1)
      RB(I, 38) = RB(I, 38)*XQ(I, 4)
      RF(I, 39) = RF(I, 39)*XQ(I, 1)
      RF(I, 40) = RF(I, 40)*XQ(I, 1)
      RB(I, 40) = RB(I, 40)*XQ(I, 4)
      RF(I, 41) = RF(I, 41)*XQ(I, 4)
      RF(I, 42) = RF(I, 42)*XQ(I, 4)
      RF(I, 43) = RF(I, 43)*XQ(I, 4)
      RF(I, 44) = RF(I, 44)*XQ(I, 4)
      RF(I, 45) = RF(I, 45)*XQ(I, 4)
      RF(I, 46) = RF(I, 46)*XQ(I, 4)
      RF(I, 47) = RF(I, 47)*XQ(I, 4)
      RF(I, 48) = RF(I, 48)*XQ(I, 2)
      RF(I, 49) = RF(I, 49)*XQ(I, 2)
      RF(I, 50) = RF(I, 50)*XQ(I, 2)
      RB(I, 50) = RB(I, 50)*XQ(I, 4)
      RF(I, 51) = RF(I, 51)*XQ(I, 2)
      RB(I, 51) = RB(I, 51)*XQ(I, 4)
      RF(I, 52) = RF(I, 52)*XQ(I, 2)
      RF(I, 53) = RF(I, 53)*XQ(I, 2)
      RF(I, 54) = RF(I, 54)*XQ(I, 2)
      RB(I, 54) = RB(I, 54)*XQ(I, 1)
      RF(I, 55) = RF(I, 55)*XQ(I, 2)
      RF(I, 56) = RF(I, 56)*XQ(I, 2)
      RF(I, 59) = RF(I, 59)*XQ(I, 3)
      RB(I, 59) = RB(I, 59)*XQ(I, 2)
      RF(I, 60) = RF(I, 60)*XQ(I, 3)
      RB(I, 60) = RB(I, 60)*XQ(I, 1)
      RF(I, 61) = RF(I, 61)*XQ(I, 3)
      RF(I, 62) = RF(I, 62)*XQ(I, 3)
      RB(I, 62) = RB(I, 62)*XQ(I, 4)
      RF(I, 63) = RF(I, 63)*XQ(I, 3)
      RF(I, 64) = RF(I, 64)*XQ(I, 3)
      RF(I, 65) = RF(I, 65)*XQ(I, 3)
      RF(I, 66) = RF(I, 66)*XQ(I, 3)
      RF(I, 67) = RF(I, 67)*XQ(I, 3)
      RB(I, 67) = RB(I, 67)*XQ(I, 2)
      RF(I, 68) = RF(I, 68)*XQ(I, 3)
      RB(I, 68) = RB(I, 68)*XQ(I, 2)
      RF(I, 69) = RF(I, 69)*XQ(I, 3)
      RB(I, 69) = RB(I, 69)*XQ(I, 2)
      RF(I, 70) = RF(I, 70)*XQ(I, 3)
      RB(I, 71) = RB(I, 71)*XQ(I, 5)
      RB(I, 72) = RB(I, 72)*XQ(I, 4)
      RB(I, 73) = RB(I, 73)*XQ(I, 4)
      RB(I, 74) = RB(I, 74)*XQ(I, 4)
      RB(I, 75) = RB(I, 75)*XQ(I, 4)
      RB(I, 76) = RB(I, 76)*XQ(I, 4)
      RF(I, 77) = RF(I, 77)*XQ(I, 1)
      RB(I, 80) = RB(I, 80)*XQ(I, 2)
      RB(I, 81) = RB(I, 81)*XQ(I, 3)
      RB(I, 82) = RB(I, 82)*XQ(I, 5)
      RB(I, 85) = RB(I, 85)*XQ(I, 5)
      RF(I, 87) = RF(I, 87)*XQ(I, 1)
      RB(I, 87) = RB(I, 87)*XQ(I, 7)
      RF(I, 88) = RF(I, 88)*XQ(I, 4)
      RF(I, 89) = RF(I, 89)*XQ(I, 4)
      RB(I, 90) = RB(I, 90)*XQ(I, 4)
      RF(I, 91) = RF(I, 91)*XQ(I, 2)
      RF(I, 92) = RF(I, 92)*XQ(I, 3)
      RB(I, 94) = RB(I, 94)*XQ(I, 8)
      RF(I, 96) = RF(I, 96)*XQ(I, 5)
      RF(I, 97) = RF(I, 97)*XQ(I, 5)
      RF(I, 98) = RF(I, 98)*XQ(I, 5)
      RB(I, 98) = RB(I, 98)*XQ(I, 3)
      RF(I, 99) = RF(I, 99)*XQ(I, 5)
      RF(I,100) = RF(I,100)*XQ(I, 5)
      RF(I,101) = RF(I,101)*XQ(I, 5)
      RF(I,105) = RF(I,105)*XQ(I, 1)
      RF(I,106) = RF(I,106)*XQ(I, 2)
      RF(I,107) = RF(I,107)*XQ(I, 3)
      RB(I,108) = RB(I,108)*XQ(I, 3)
      RF(I,111) = RF(I,111)*XQ(I, 1)
      RF(I,112) = RF(I,112)*XQ(I, 2)
      RB(I,112) = RB(I,112)*XQ(I, 7)
      RB(I,114) = RB(I,114)*XQ(I, 6)
      RF(I,115) = RF(I,115)*XQ(I, 7)
      RB(I,117) = RB(I,117)*XQ(I, 2)
      RF(I,120) = RF(I,120)*XQ(I, 4)
      RB(I,120) = RB(I,120)*XQ(I, 7)
      RF(I,122) = RF(I,122)*XQ(I, 6)
      RF(I,123) = RF(I,123)*XQ(I, 6)
      RB(I,123) = RB(I,123)*XQ(I, 2)
      RF(I,124) = RF(I,124)*XQ(I, 6)
      RF(I,125) = RF(I,125)*XQ(I, 6)
      RB(I,125) = RB(I,125)*XQ(I, 2)
      RB(I,126) = RB(I,126)*XQ(I, 9)
      RB(I,130) = RB(I,130)*XQ(I, 2)
      RF(I,132) = RF(I,132)*XQ(I, 7)
      RF(I,133) = RF(I,133)*XQ(I, 7)
      RF(I,134) = RF(I,134)*XQ(I, 7)
      RB(I,134) = RB(I,134)*XQ(I, 6)
      RF(I,135) = RF(I,135)*XQ(I, 7)
      RF(I,136) = RF(I,136)*XQ(I, 7)
      RF(I,137) = RF(I,137)*XQ(I, 7)
      RF(I,138) = RF(I,138)*XQ(I, 7)
      RF(I,139) = RF(I,139)*XQ(I, 7)
      RB(I,139) = RB(I,139)*XQ(I, 9)
      RF(I,140) = RF(I,140)*XQ(I, 7)
      RB(I,140) = RB(I,140)*XQ(I, 4)
      RF(I,141) = RF(I,141)*XQ(I, 7)
      RB(I,141) = RB(I,141)*XQ(I, 9)
      RF(I,142) = RF(I,142)*XQ(I, 7)
      RF(I,144) = RF(I,144)*XQ(I, 7)
      RF(I,145) = RF(I,145)*XQ(I, 7)
      RF(I,146) = RF(I,146)*XQ(I, 7)
      RF(I,147) = RF(I,147)*XQ(I, 9)
      RF(I,148) = RF(I,148)*XQ(I, 9)
      RF(I,149) = RF(I,149)*XQ(I, 9)
      RB(I,149) = RB(I,149)*XQ(I, 4)
      RF(I,150) = RF(I,150)*XQ(I, 9)
      RF(I,151) = RF(I,151)*XQ(I, 9)
      RF(I,152) = RF(I,152)*XQ(I, 9)
      RF(I,153) = RF(I,153)*XQ(I, 9)
      RF(I,154) = RF(I,154)*XQ(I, 9)
      RB(I,155) = RB(I,155)*XQ(I, 6)
      RB(I,156) = RB(I,156)*XQ(I, 8)
      RB(I,157) = RB(I,157)*XQ(I, 7)
      RB(I,158) = RB(I,158)*XQ(I, 7)
      RB(I,159) = RB(I,159)*XQ(I, 4)
      RB(I,160) = RB(I,160)*XQ(I, 2)
      RB(I,161) = RB(I,161)*XQ(I, 7)
      RB(I,162) = RB(I,162)*XQ(I, 7)
      RF(I,164) = RF(I,164)*XQ(I, 4)
      RB(I,164) = RB(I,164)*XQ(I, 8)
      RF(I,165) = RF(I,165)*XQ(I, 2)
      RF(I,166) = RF(I,166)*XQ(I, 3)
      RB(I,166) = RB(I,166)*XQ(I, 6)
      RF(I,167) = RF(I,167)*XQ(I, 3)
      RB(I,168) = RB(I,168)*XQ(I, 7)
      RB(I,169) = RB(I,169)*XQ(I,10)
      RF(I,170) = RF(I,170)*XQ(I, 8)
      RF(I,171) = RF(I,171)*XQ(I, 8)
      RF(I,172) = RF(I,172)*XQ(I, 8)
      RF(I,173) = RF(I,173)*XQ(I, 8)
      RF(I,174) = RF(I,174)*XQ(I, 8)
      RF(I,175) = RF(I,175)*XQ(I, 8)
      RF(I,176) = RF(I,176)*XQ(I, 8)
      RF(I,177) = RF(I,177)*XQ(I, 8)
      RF(I,178) = RF(I,178)*XQ(I, 8)
      RB(I,180) = RB(I,180)*XQ(I, 8)
      RB(I,181) = RB(I,181)*XQ(I, 8)
      RB(I,182) = RB(I,182)*XQ(I, 8)
      RF(I,183) = RF(I,183)*XQ(I, 3)
      RB(I,183) = RB(I,183)*XQ(I, 8)
      RB(I,184) = RB(I,184)*XQ(I, 8)
      RB(I,186) = RB(I,186)*XQ(I, 6)
      RB(I,188) = RB(I,188)*XQ(I, 7)
      RF(I,189) = RF(I,189)*XQ(I, 4)
      RB(I,190) = RB(I,190)*XQ(I,10)
      RF(I,199) = RF(I,199)*XQ(I,10)
      RB(I,199) = RB(I,199)*XQ(I, 8)
      RF(I,200) = RF(I,200)*XQ(I,10)
      RF(I,201) = RF(I,201)*XQ(I,10)
      RB(I,201) = RB(I,201)*XQ(I, 8)
      RF(I,202) = RF(I,202)*XQ(I,10)
      RF(I,203) = RF(I,203)*XQ(I,10)
      RF(I,204) = RF(I,204)*XQ(I,10)
      RB(I,204) = RB(I,204)*XQ(I, 8)
      RF(I,205) = RF(I,205)*XQ(I,10)
     ENDDO
C
      END


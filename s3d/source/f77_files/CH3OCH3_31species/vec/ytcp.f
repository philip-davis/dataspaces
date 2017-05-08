C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE YTCP (VL, P, T, Y, C)
      USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      DIMENSION Y(MAXVL,*), C(MAXVL,*)
      DIMENSION P(MAXVL),T(MAXVL),SUM(MAXVL)
        INTEGER VL
      DATA SMALL/1D-200/
C
C     H
      C(:VL, 1) = Y(:VL, 1)/1.00796998D0
C     H2
      C(:VL, 2) = Y(:VL, 2)/2.01593995D0
C     CH3
      C(:VL, 3) = Y(:VL, 3)/1.50350603D1
C     O
      C(:VL, 4) = Y(:VL, 4)/1.59994001D1
C     CH4
      C(:VL, 5) = Y(:VL, 5)/1.60430303D1
C     OH
      C(:VL, 6) = Y(:VL, 6)/1.70073701D1
C     H2O
      C(:VL, 7) = Y(:VL, 7)/1.80153401D1
C     C2H2
      C(:VL, 8) = Y(:VL, 8)/2.60382407D1
C     CO
      C(:VL, 9) = Y(:VL, 9)/2.80105505D1
C     C2H4
      C(:VL, 10) = Y(:VL, 10)/2.80541806D1
C     C2H5
      C(:VL, 11) = Y(:VL, 11)/2.90621506D1
C     CH2O
      C(:VL, 12) = Y(:VL, 12)/3.00264904D1
C     C2H6
      C(:VL, 13) = Y(:VL, 13)/3.00701206D1
C     CH3O
      C(:VL, 14) = Y(:VL, 14)/3.10344604D1
C     O2
      C(:VL, 15) = Y(:VL, 15)/3.19988003D1
C     HO2
      C(:VL, 16) = Y(:VL, 16)/3.30067703D1
C     H2O2
      C(:VL, 17) = Y(:VL, 17)/3.40147402D1
C     CO2
      C(:VL, 18) = Y(:VL, 18)/4.40099506D1
C     CH3HCO
      C(:VL, 19) = Y(:VL, 19)/4.40535808D1
C     HCOOH
      C(:VL, 20) = Y(:VL, 20)/4.60258906D1
C     CH3OCH3
      C(:VL, 21) = Y(:VL, 21)/4.60695207D1
C     CH3OCO
      C(:VL, 22) = Y(:VL, 22)/5.90450109D1
C     CH3OCHO
      C(:VL, 23) = Y(:VL, 23)/6.00529809D1
C     CH3OCH2OH
      C(:VL, 24) = Y(:VL, 24)/6.20689209D1
C     OCH2OCHO
      C(:VL, 25) = Y(:VL, 25)/7.50444111D1
C     HOCH2OCO
      C(:VL, 26) = Y(:VL, 26)/7.50444111D1
C     CH3OCH2O2
      C(:VL, 27) = Y(:VL, 27)/7.7060351D1
C     HO2CH2OCHO
      C(:VL, 28) = Y(:VL, 28)/9.20517812D1
C     O2CH2OCH2O2H
      C(:VL, 29) = Y(:VL, 29)/1.09059151D2
C     N2-1
      C(:VL, 30) = Y(:VL, 30)/2.80133991D1
C     N2
      C(:VL, 31) = Y(:VL, 31)/2.80133991D1
C
      SUM(:VL) = 0D0
      DO K = 1, 31
         SUM(:VL) = SUM(:VL) + C(:VL,K)
      ENDDO
      SUM(:VL) = P(:VL)/(SUM(:VL)*T(:VL)*8.314510D7)
C
      DO K = 1, 31
         C(:VL,K) = MAX(C(:VL,K),SMALL) * SUM(:VL)
      ENDDO
C
      END

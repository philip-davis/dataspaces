C                                                                      C
C----------------------------------------------------------------------C
C                                                                      C
      SUBROUTINE YTCP (VL, P, T, Y, C)
      USE chemkin_m, only : MAXVL
      IMPLICIT DOUBLE PRECISION (A-H, O-Z), INTEGER (I-N)
      DIMENSION Y(MAXVL,*), C(MAXVL,*)
      DIMENSION P(MAXVL),T(MAXVL),SUM(MAXVL)
        INTEGER VL
      DATA SMALL/1D-50/
C
C     h
      C(:VL, 1) = Y(:VL, 1)/1.00796998D0
C     h2
      C(:VL, 2) = Y(:VL, 2)/2.01593995D0
C     o
      C(:VL, 3) = Y(:VL, 3)/1.59994001D1
C     o2
      C(:VL, 4) = Y(:VL, 4)/3.19988003D1
C     oh
      C(:VL, 5) = Y(:VL, 5)/1.70073701D1
C     h2o
      C(:VL, 6) = Y(:VL, 6)/1.80153401D1
C     co
      C(:VL, 7) = Y(:VL, 7)/2.80105505D1
C     co2
      C(:VL, 8) = Y(:VL, 8)/4.40099506D1
C     ch3
      C(:VL, 9) = Y(:VL, 9)/1.50350603D1
C     ch4
      C(:VL, 10) = Y(:VL, 10)/1.60430303D1
C     ho2
      C(:VL, 11) = Y(:VL, 11)/3.30067703D1
C     h2o2
      C(:VL, 12) = Y(:VL, 12)/3.40147402D1
C     ch2o
      C(:VL, 13) = Y(:VL, 13)/3.00264904D1
C     c2h6
      C(:VL, 14) = Y(:VL, 14)/3.00701206D1
C     c2h4
      C(:VL, 15) = Y(:VL, 15)/2.80541806D1
C     c2h5
      C(:VL, 16) = Y(:VL, 16)/2.90621506D1
C     c2h
      C(:VL, 17) = Y(:VL, 17)/2.50302707D1
C     c2h2
      C(:VL, 18) = Y(:VL, 18)/2.60382407D1
C     ch3oh
      C(:VL, 19) = Y(:VL, 19)/3.20424304D1
C     ch2co
      C(:VL, 20) = Y(:VL, 20)/4.20376408D1
C     hcco
      C(:VL, 21) = Y(:VL, 21)/4.10296708D1
C     ch3cho
      C(:VL, 22) = Y(:VL, 22)/4.40535808D1
C     c3h4-a
      C(:VL, 23) = Y(:VL, 23)/4.0065331D1
C     c3h4-p
      C(:VL, 24) = Y(:VL, 24)/4.0065331D1
C     c3h6
      C(:VL, 25) = Y(:VL, 25)/4.20812709D1
C     c4h6
      C(:VL, 26) = Y(:VL, 26)/5.40924213D1
C     c4h7
      C(:VL, 27) = Y(:VL, 27)/5.51003913D1
C     c4h8-1
      C(:VL, 28) = Y(:VL, 28)/5.61083612D1
C     pc4h9
      C(:VL, 29) = Y(:VL, 29)/5.71163312D1
C     ch3coch2
      C(:VL, 30) = Y(:VL, 30)/5.70727011D1
C     c2h5cho
      C(:VL, 31) = Y(:VL, 31)/5.80806711D1
C     c5h9
      C(:VL, 32) = Y(:VL, 32)/6.91274816D1
C     c5h10-1
      C(:VL, 33) = Y(:VL, 33)/7.01354516D1
C     c5h11-1
      C(:VL, 34) = Y(:VL, 34)/7.11434215D1
C     ch3o2
      C(:VL, 35) = Y(:VL, 35)/4.70338606D1
C     ch3o2h
      C(:VL, 36) = Y(:VL, 36)/4.80418305D1
C     c2h3co
      C(:VL, 37) = Y(:VL, 37)/5.50567611D1
C     c2h3cho
      C(:VL, 38) = Y(:VL, 38)/5.60647311D1
C     c3h5-a
      C(:VL, 39) = Y(:VL, 39)/4.1073301D1
C     c3h3
      C(:VL, 40) = Y(:VL, 40)/3.9057361D1
C     nc3h7cho
      C(:VL, 41) = Y(:VL, 41)/7.21077614D1
C     c2h5coch2
      C(:VL, 42) = Y(:VL, 42)/7.10997914D1
C     ch3chco
      C(:VL, 43) = Y(:VL, 43)/5.60647311D1
C     nc3h7coch3
      C(:VL, 44) = Y(:VL, 44)/8.61348517D1
C     nc3h7coch2
      C(:VL, 45) = Y(:VL, 45)/8.51268817D1
C     nc7h16
      C(:VL, 46) = Y(:VL, 46)/1.00205572D2
C     c7h15-1
      C(:VL, 47) = Y(:VL, 47)/9.91976022D1
C     c7h15o2
      C(:VL, 48) = Y(:VL, 48)/1.31196402D2
C     c7h14ooho2
      C(:VL, 49) = Y(:VL, 49)/1.63195203D2
C     c7h14o
      C(:VL, 50) = Y(:VL, 50)/1.14189032D2
C     nc7ket
      C(:VL, 51) = Y(:VL, 51)/1.46187833D2
C     n2
      C(:VL, 52) = Y(:VL, 52)/2.80133991D1
C
      SUM(:VL) = 0D0
      DO K = 1, 52
         SUM(:VL) = SUM(:VL) + C(:VL,K)
      ENDDO
      SUM(:VL) = P(:VL)/(SUM(:VL)*T(:VL)*8.314510D7)
C
      DO K = 1, 52
         C(:VL,K) = MAX(C(:VL,K),SMALL) * SUM(:VL)
      ENDDO
C
      END


      SUBROUTINE SHAPE (X,Y,IN,L,M,XI,YI,F,DET,DGX,DGY,MAXNN,MAXNE,
    1	MXC,ME)
C
C     PURPOSE: TO EVALUATE 2-D SHAPE FUNCTIONS AND DETERMINANT
C**
      LEVEL 2, X,Y,IN
      DIMENSION X(MAXNN),Y(MAXNN),IN(ME,MAXNE)
      DIMENSION DGX(MXC),DGY(MXC),F(MXC)
      DIMENSION ALF(4),DAX(4),DAY(4),BTX(4),BTY(4),DBX(4),DBY(4),
    1	DFX(12),DFY(12)
C
      XI1 = 1. - XI
      XI2 = 1. + XI
      YI1 = 1. - YI
      YI2 = 1. + YI
C
C     CORNER NODE SHAPE FUNCTIONS, BASIC PART
      ALF(1) = .25*XI1*YI1
      ALF(2) = .25*XI2*YI1
      ALF(3) = .25*XI2*YI2
      ALF(4) = .25*XI1*YI2
      DAX(1) = -.25*YI1
      DAX(2) = .25*YI1
      DAX(3) = .25*YI2
      DAX(4) = -.25*YI2
      DAY(1) = -.25*XI1
      DAY(2) = -.25*XI2
      DAY(3) = .25*XI2
      DAY(4) = .25*XI1
C
C     CORNER NODE SHAPE FUNCTIONS, SIDE-DEPENDENT PART
      XQ1 = XI - .5
      XQ2 = -XI - .5
      YQ1 = YI - .5
      YQ2 = -YI - .5
      XC1 = 1.125*XI*XI - .625
      XC2 = 2.25*XI
      YC1 = 1.125*YI*YI - .625
      YC2 = 2.25*YI
      J1 = 1
      J2 = 2
      J3 = 5
          DO 50 J=1,2
          IF (IN(J3,L).EQ.0) GO TO 10
          IF (IN(J3+1,L).EQ.0) GO TO 20
          GO TO 30
   10 CONTINUE
      BTX(J1) = .5
      BTX(J2) = .5
      DBX(J1) = 0.
      DBX(J2) = 0.
          GO TO 40
   20 CONTINUE
      BTX(J1) = XQ2
      BTX(J2) = XQ1
      DBX(J1) = -1.
      DBX(J2) = 1.
          GO TO 40
   30 CONTINUE
      BTX(J1) = XC1
      BTX(J2) = XC1
      DBX(J1) = XC2
      DBX(J2) = XC2
   40 CONTINUE
      J1 = 4
      J2 = 3
      J3 = 9
   50 CONTINUE
      J1 = 2
      J2 = 3
      J3 = 7
          DO 100 J=1,2
          IF (IN(J3,L).EQ.0) GO TO 60
          IF (IN(J3+1,L).EQ.0) GO TO 70
          GO TO 80
   60 CONTINUE
      BTY(J1) = .5
      BTY(J2) = .5
      DBY(J1) = 0.
      DBY(J2) = 0.
          GO TO 90
   70 CONTINUE
      BTY(J1) = YQ2
      BTY(J2) = YQ1
      DBY(J1) = -1.
      DBY(J2) = 1.
          GO TO 90
   80 CONTINUE
      BTY(J1) = YC1
      BTY(J2) = YC1
      DBY(J1) = YC2
      DBY(J2) = YC2
   90 CONTINUE
      J1 = 1
      J2 = 4
      J3 = 11
  100 CONTINUE
C
C     SHAPE FUNCTION DERIVATIVE MATRIX - CORNER NODES
          DO 110 J=1,4
      DFX(J) = DAX(J)*(BTX(J)+BTY(J)) + DBX(J)*ALF(J)
      DFY(J) = DAY(J)*(BTX(J)+BTY(J)) + DBY(J)*ALF(J)
      F(J) = ALF(J)*(BTX(J)+BTY(J))
  110 CONTINUE
C
C     SHAPE FUNCTION DERIVATIVE MATRIX - EDGE NODES
          IF (M.EQ.4) GO TO 240
      J = 4
      XEQ = 1. -XI*XI
      YEQ = 1. -YI*YI
      XE1 = 1. -3.*XI
      XE2 = 1. +3.*XI
      YE1 = 1. -3.*YI
      YE2 = 1. +3.*YI
          IF (IN(5,L).EQ.0) GO TO 140
          IF (IN(6,L).EQ.0) GO TO 120
          GO TO 130
  120 J = J + 1
      DFX(J) = -XI*YI1
      DFY(J) = -.5*XEQ
      F(J) = .5*XEQ*YI1
          GO TO 140
  130 J = J + 1
      DFX(J) = -.28125*YI1*(3.*XEQ+2.*XI*XE1)
      DFY(J) = -.28125*XEQ*XE1
      F(J) = .28125*XEQ*XE1*YI1
      J = J + 1
      DFX(J) = .28125*YI1*(3.*XEQ-2.*XI*XE2)
      DFY(J) = -.28125*XEQ*XE2
      F(J) = .28125*XEQ*XE2*YI1
  140     IF (IN(7,L).EQ.0) GO TO 170
          IF (IN(8,L).EQ.0) GO TO 150
          GO TO 160
  150 J = J + 1
      DFX(J) = .5*YEQ
      DFY(J) = -YI*XI2
      F(J) = .5*XI2*YEQ
          GO TO 170
  160 J = J + 1
      DFX(J) = .28125*YEQ*YE1
      DFY(J) = -.28125*XI2*(3.*YEQ+2.*YI*YE1)
      F(J) = .28125*XI2*YEQ*YE1
      J = J + 1
      DFX(J) = .28125*YEQ*YE2
      DFY(J) = .28125*XI2*(3.*YEQ-2.*YI*YE2)
      F(J) = .28125*XI2*YEQ*YE2
  170     IF (IN(9,L).EQ.0) GO TO 200
          IF (IN(10,L).EQ.0) GO TO 180
          GO TO 190
  180 J = J + 1
      DFX(J) = -XI*YI2
      DFY(J) = .5*XEQ
      F(J) = .5*XEQ*YI2
          GO TO 200
  190 J = J + 1
      DFX(J) = .28125*YI2*(3.*XEQ-2.*XI*XE2)
      DFY(J) = .28125*XEQ*XE2
      F(J) = .28125*XEQ*XE2*YI2
      J = J + 1
      DFX(J) = -.28125*YI2*(3.*XEQ+2.*XI*XE1)
      DFY(J) = .28125*XEQ*XE1
      F(J) = .28125*XEQ*XE1*YI2
  200     IF (IN(11,L).EQ.0) GO TO 230
          IF (IN(12,L).EQ.0) GO TO 210
          GO TO 220
  210 J = J + 1
      DFX(J) = -.5*YEQ
      DFY(J) = -YI*XI1
      F(J) = .5*XI1*YEQ
          GO TO 230
  220 J = J + 1
      DFX(J) = -.28125*YEQ*YE2
      DFY(J) = .28125*XI1*(3.*YEQ-2.*YI*YE2)
      F(J) = .28125*XI1*YEQ*YE2
      J = J + 1
      DFX(J) = -.28125*YEQ*YE1
      DFY(J) = -.28125*XI1*(3.*YEQ+2.*YI*YE1)
      F(J) = .28125*YEQ*YE1*XI1
  230 CONTINUE
  240 CONTINUE
C
C     JACOBIAN
      SUM1 = 0.
      SUM2 = 0.
      SUM3 = 0.
      SUM4 = 0.
      K = 0
          DO 260 I=1,M
  250 K = K + 1
          IF (IN(K,L).EQ.0) GO TO 250
      KI = IN(K,L)
      SUM1 = SUM1 + DFX(I)*X(KI)
      SUM2 = SUM2 + DFX(I)*Y(KI)
      SUM3 = SUM3 + DFY(I)*X(KI)
      SUM4 = SUM4 + DFY(I)*Y(KI)
  260 CONTINUE
      DET = SUM1*SUM4 - SUM2*SUM3
      DET1 = 1./DET
      C11 = DET1*SUM4	^
      C12 = -DET1*SUM2
      C21 = -DET1*SUM3
      C22 = DET1*SUM1
C
C     SHAPE FUNCTION DERIVATIVES - GLOBAL
          DO 270 J=1,M
      DGX(J) = C11*DFX(J) + C12*DFY(J)
      DGY(J) = C21*DFX(J) + C22*DFY(J)
  270 CONTINUE
      RETURN
      END
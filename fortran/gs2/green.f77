      SUBROUTINE GREEN (X,Y,DETJ,AG,IN,KF,JD,IEQ,MS,MAXNN,MAXNE,MXT,
    1	ME,NP,L,ISTOP)
C
C     PURPOSE: TO TO EVALUATE 1-D SHAPE FUCTIONS AND DETERMINANT
C**
      LEVEL 2, X,Y,KF,IN
      DIMENSION X(MAXNN),Y(MAXNN),DETJ(MXT),IN(ME,MAXNE),KF(MAXNE),
    1	JD(12),IEQ(4),AG(6)
      DIMENSION G(4,4),DLX(4,4),ALF(2),JN(4),JS(4),KN(2)
C
C
      IC = 0
      KN(1) = 0
          GO TO 300
    1 KN(1) = KN(1) + 1
          IF (KF(L).NE.IC) GO TO 300
      KN(2) = KN(1) + 1
          IF (KN(1).EQ.4) KN(2)=1
    4 II = 0
      I = 0
          DO 10 K=1,2
    5	  IF (I.LT.4) GO TO 6
              ISTOP = ISTOP + 1
          GO TO 500
    6 I = I + 1
          IF (KN(K).NE.I) GO TO 5
      KNK = KN(K)
      II = II + 1
      JN(II) = JD(KNK)
      JS(II) = JD(KNK)
      IEQ(II) = I
      I = 0
   10 CONTINUE
      IP = 2
      IQ = 2
      ID = 2*KN(1) + 2
          DO 20 J=1,2
      IP = IP + 1
      ID = ID + 1
      JN(IP) = IN(ID,L)
          IF(IN(ID,L).EQ.0) GO TO 20
      II = II + 1
      JS(II) = IN(ID,L)
   20 CONTINUE
      MS = II
          IF (MS.EQ.2) GO TO 35
          DO 30 K=3,MS
   25 IQ = IQ + 1
          IF (JD(IQ).NE.JS(K)) GO TO 25
   30 IEQ(K) = IQ
   35 CONTINUE
          DO 80 K=1,NP
      XI = AG(K)
          IF (NP.GT.2) XI=AG(K+2)
      XI1 = 1.0 - XI
      XI2 = 1.0 + XI
C
C     CORNER NODE SHAPE FUNCTION, BASIC PART
      ALF(1) = 0.5*XI1
      ALF(2) = 0.5*XI2
C
C	  DETERMINE IF SIDE	IS LINEAR, QUADRATIC, OR CUBIC
          IF (MS.EQ.4) GO TO 45
          IF (MS.EQ.3) GO TO 40
C
C	  LINEAR SIDE
      G(1,K) = ALF(1)
      G(2,K) = ALF(2)
      DLX(1,K) = -0.5
      DLX(2,K) * 0.5
          GO TO 50
C
C	  QUADRATIC SIDE
   40 G(1,K) = -XI*ALF(1)
      G(2,K) = XI*ALF(2)
      G(3,K) = 1.0 - XI*XI
      DLX(1,K) = XI - 0.5
      DLX(2,K) = XI + 0.5
      DLX(3,K) = -2.0*XI
          GO TO 50
C
C	  CUBIC SIDE
   45 XI2 = XI*XI
      G(1,K) = ALF(1)*(1.125*XI2-0.125)
      G(2,K) = ALF(2)*(1.125*XI2-0.125)
      G(3,K) = 0.5625*(1.0-XI2)*(1.0-3.0*XI)
      G(4,K) = 0.5625*(1.0-XI2)*(1.0+3.0*XI)
      DLX(1,K) = 0.0625*(18.0*XI-27.0*XI2+1.0)
      DLX(2,K) = 0.0625*(18.0*XI+27.0*XI2-1.0)
      DLX(3,K) = 0.5625*(9.0*XI2-2.0*XI-3.0)
      DLX(4,K) = 0.5625*(-9.0*XI2-2.0*XI+3.0)
   50 CONTINUE
C
C	  JACOBIANS
      SUMX = 0.0
      SUMY = 0.0
          DO 60 LI=1,MS
      KS = JS(LI)
      SUMX = SUMX + DLX(LI,K)*X(KS)
   60 SUMY = SUMY + DLX(LI,K)*Y(KS)
      DETJ(K+NP) = SUMY
      DETJ(K) = SUMX
   80 CONTINUE
  300 IC = IC + 1
          IF (KN(1).LT.4) GO TO 1
  500 CONTINUE
      RETURN
      END
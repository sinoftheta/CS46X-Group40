
C File: DBAND.f77

      SUBROUTINE DBAND(S, N, NB, NDIM, MDIM, IEX)
C
C To triangularize a symmetric coefficent matrix
C**
      LEVEL 2, S
      DIMENSION S(NDIM, MDIM)

      IEX = 0
            DO 50 I=1, N
      IP = N - I + 1
            IF (NB.LT.IP) IP=NB
            DO 50 J=1,IP
      IQ = NB - J
            IF ((I-J).LT.IQ) IQ=I-1
      SUM = S(I,J)
            IF (IQ.LT.1) GO TO 20
            DO 10 K=1,IQ
      II = I - K
      SIIK = S(II,K+1)
      JZ = J + K
      SIIJZ = S(II,JZ)
            IF (SIIJZ.EQ.0.) GO TO 10
      SUM = SUM - SIIK*SIIJZ
10    CONTINUE
20          IF (J.NE.1) GO TO 40
            IF (SUM.LE.0) GO TO 30
      TEMP = 1./SQRT(SUM)
      S(I,J) = TEMP
            GO TO 50
30    WRITE (6,60) I
      WRITE (6,70) N,NB,IP,IQ,I,J,SUM
      IEX = 1
      RETURN
40    S(I,J) = SUM*TEMP
50    CONTINUE

60    FORMAT (1H1,OX,18HDBAND FAILS AT ROW, I4)
70    FORMAT (1H0,6I5,E20.8)
      RETURN
      END
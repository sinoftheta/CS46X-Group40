      SUBROUTINE SBAND (S,P,U,NB,NDIM,MDIM,NU)
C
C	TO SOLVE FLOW EQUATION
C
C**
      LEVEL 2, S
      DIMENSION S(NDIM,MDIM),P(NU),U(NU)
		   
	    DO 30 I=1,N
      J = I - NB + 1
	    IF ((I+1).LE.NB) J=1
      SUM = P(I)
      K1 = I - 1
          IF (J.GT.K1) GO TO 20
	    DO 10 K=J,K1
      II = I - K + 1
      SKI = S(K,II)
10    IF(SKI.NE.0.) SUM=SUM-SKI*U(k)
20    U(I) = SUM*S(I, 1)
30    CONTINUE
	    DO 60 I1=1,N
      I = N - I1 + 1
      J = I + NB - 1
	    IF (J.GT.N) J=N
      SUM = U(I)
      K2 = I + 1
	    IF (K2.GT.J) GO TO 50
	    DO 40 K=K2,J
      KK = K - I + 1
      SKI = S(I,KK)
40        IF (SKI.NE.0.) SUM=SUM-SKI*U(K)
50    U(I) = SUM*S(I,1)
60    CONTINUE
      RETURN
      END

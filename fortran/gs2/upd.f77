      SUBROUTINE UPD(A,B,KOD,LQ,NA,NB,NN)
C
C   purpose: to save the solution at time T
C
      LEVEL 2, A,KOD,LQ
      DIMENSION A(NA),B(NB),KOD(NA),LQ(NA)
C
           DO 10 I=1,NN
           IF (KOD(I).EQ.1) GO TO 10
      K = I - LQ(I)
      B(K) = A(I)
10    CONTINUE
      RETURN
      END
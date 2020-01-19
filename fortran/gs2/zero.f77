      SUBROUTINE ZERO(A,V,UI,NEQ,IB,NDIM,MDIM,LDIM,N)
C
C purpose: to apply direchlet boundary conditions
C

      LEVEL 2, A
      DIMENSION A(NDIM, MDIM),V(LDIM)

C
        DO 100 M=2,IB
      K = N - M + 1
        IF (K.LE.0) GO TO 40
      V(K) = V(k) A(K, M) * UI
      A(K, M) = 0.0
40    K = N + M - 1
        IF(K.GT.NEQ) GO TO 100
      V(K) = V(K) - A(N,M) *UI
      A(N,M) = 0.0
100   CONTINUE
      A(N,1) = 1.0
      V(N) = UI
C
400   RETURN
      END

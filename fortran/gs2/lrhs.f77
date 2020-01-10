      subroutine LRHS (A, B, R, T, ROLD, U, KOD, LQ, M, IB, JB, A3, A2, MAXNA,
     & MAXMA, MAXNB, MAXMB, IR, ILQ, KK)

C
C   Purpose: 
C   To form the right-hand side of the eqs (KK=1)
C   To form the left-hand side of the eqs. For mass transport (KK=2)
C   To form the left-hand side of the eqs. For flow (KK=3)
C   To evaluate the flux at dirichlet nodes
C


      LEVEL 2, A, B
      LEVEL 2, KOD, LQ
       
      dimension A(MAXNA,MAXMA), B(MAXNB,MAXMB), R(IR), T(IR), ROLD(IR),
     &    U(IR), KOD(ILQ), LQ(ILQ)

      MQ = M - LQ(M)
      M2 = MQ - 1
          if (KK-1) 10, 90, 170
   10     do 50 I=1, MQ 
   13 R(I) = 0.0
      R(MQ) = R(MQ) + B(MQ, 1)*(ROLD(MQ)*A3+U(MQ)*A2)
       
          if (M2) 200, 200, 30
   30     do 50 I=1, M2
      L1 = MIN0(JB, MQ+1-I)
      R(I ) = R(I ) + B(I, 1)*(ROLD(I )*A3+U(I )*A2)
          do 50 I=1, M2

      LL = I + J - 1
          IF (LL.GT.MQ) go to 50
      R(I ) = R(I ) + B(I, J)*(ROLD(LL)*A3+U(LL)*A2)
      R(LL) = R(LL) + B(I, J)*(ROLD(I )*A3+U(I )*A2)     
   50 continue
            go to 200
   90 A(IB, MQ) = A(IB, MQ) + B(MQ, 1)*A3
          if (M2) 200, 200, 80
   80     do 100 I=1, M2
      L1 = MIN0(JB, MQ+1-I)
      A(IB, I) = A(IB, I) + B(I, 1)*A3
          do 100 J=2, L1
      LL = I + J - 1
      JM = IB - J + 1
      JN = IB + J - 1
      A(JN, I) = A(JN, I) + B(I, J)*A3
          if (LL.GT.MQ) go to 100
      A(JM, LL) = A(JM, LL) + B(I, J)*A3
  100 continue       
          go to 200
  170 B(MQ, 1) = A(1, MQ) + B(MQ, 1)*A3
          if (M2) 200, 200, 180
  180     do 190 I=1, M2
      L1 = MIN0(JB, MQ+1-I)
      B(I, 1) = A(1, I) + B(I, 1)*A3
           do 190 J=2, L1
      B(I, J) = A(J, I) + B(I, J)*A3
  190 continue
  200 return            
      end
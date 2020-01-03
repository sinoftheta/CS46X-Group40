      subroutine ARRAY(A, Z, NEW, IB, JB, JB2, MDIM, NDIM, MX, JX)
C
C Purpose: to transform the matrix S into an equivalent 1D Array Z
C

        LEVEL 2, A, Z
        dimension A(MDIM, NDIM), Z(MX)

        J = 0

            DO 60 N=1, NEQ
        K = IB - N + 1
        LIM = IB - N + NEQ
        K0 = MAX0(IB-JB+1, K)
        K1 = MIN0(JB2, LIM)
            DO 50 M=K0, K1  
        J = J + 1
  
   50 Z(J) = A(M, N)
   60 continue
  300 JX = J

      return
      end
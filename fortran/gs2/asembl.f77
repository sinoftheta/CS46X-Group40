      subroutine asembl (A, B, EA, EB, R, U, RE, F, KOD, LQ, JD, M, L, MAXNA, MAXMA,
      &    MAXNB, MAXMB, MAXR, ILQ, MXC, IB, IB2, JB, JB2, ISTOP)

C
C     Purpose: To assemble the element matricies 
C

      level 2, A, B
      level 2, F, KOD, LQ
      dimension A(MAXNA, MAXMA), B(MAXNB, MAXMB), EQ(MXC, MXC), EB(MXC, MXC),
     & R(MAXR), U(MAXR), RE(MXC), F(ILQ), KOD(ILQ), LQ(ILQ), JD(12)  
C
      IH = IB2 - IB + 1
          do 100 I=1, M
      JDI = JD(I)
      IR = JDI - LQ(JDI)
          if (KOD(JDI).EQ.1) go to 60
      R(IR) = R(IR) + RE(I)
          do 50 J=1, M 
      JDJ = JD(J)
          if (KOD(JDI).EQ.1) go to 50
      JC = JDJ - IR + 1 - LQ(JDJ)
          if (JC.LE.IB) go to 20
      write (6, 1000) L, JC, IB          
                ISTOP = ISTOP + 1
          go to 999
   20     if (JC.LT.1) go to 30
      B(IR, JC) = B(IR, JC) + EB(I, J)
          if (JC.GT.JB) JB=JC
   30 NC = IB + JC - 1
          if (NC.LE.IB2) go to 40
      write (6, 2000) L, NC, IB2 
                ISTOP = ISTOP + 1
          go to 999
   40     if (NC.LT.1) go to 50
      A(NC, IR) = A(NC, IR) + EA(I, J)
          if (NC.GT.JB2) JB2=NC
   50 continue
          go to 100
   60     do 70 J=1, M
      JDJ = JD(J)
          if (KOD(JDJ).GT.0) go to 70
      JC = JDJ - LQ(JDJ)
      U(JC) = U(JC) - EA(J, I) * F(JDI)
   70 continue
  100 continue
 1000 format (////11X, 27HINSUFFICIENT HALF-BANDWIDTH/11X,
     1    7HELEMENT, I%, 2X, *HREQUIRES, I5, 2X, 10HINSTEAD OF, I5)
 2000 format (////11X, 22HINSUFFICIENT BANDWIDTH/11X,
     1    7HELEMENT, I5, 2X, 8HREQUIRES, I5, 2X, OHINSTEAD OF, I5)
  999 return                        
      end
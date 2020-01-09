      subroutine matgen (PR, SE, SRCR, Q, QP, SRCRT, MXC, MXT, I, J, L, M)
C
C Purpose: To perform surface integrations
C

      common /ONE/ BWD, CNTR, CHNG, TDR, STAT, STATP, OLDT, DELT, DPRDT,
     2    BETAP, CLOS1, DELP, DIFUSN, PCHNG, TYPE, VMAX, STIME, SSEC, H1, H2,
     4    PL, COEFI, EI, ITER1, ITMAX, ITCHNG, IGO, NE, NP, NK, NSEEP, INC, mE,
     5    KOD1, KOD2, KOD3, KOD4, KOD7, KOD8, KOD9, KOD10, KOD11, KOD12
      dimension PE(MXC, MXC), SE(MXC, MXC), SRCR(MXC),
     1      Q(MXT), QP(MXT), SRCRT(MXT)
C
      H11 = H1*H1
      H12 = H1*H2
      H22 = H2*H2
          if (NP.EQ.4) go to 30
      SE(I, J) = Q(1) + Q(2) + Q(3) + Q(4) 
   20 PE(I, J) = QP(1) + QP(2) + QP(3) + QP(4)  
          go to 50
   30 SE(I, J) = H11*(Q(1) + Q(4) + Q(13) + Q(16))
     1    + H12 * (Q(2) + Q(3) + Q(5) + Q(8) + Q(9) + Q(12) + Q(14) + Q(15))
     2    + H22 * (Q(6) + Q(7) + Q(10) + Q(11))
   40            
      end 
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
   40 PE(I, J) = H11*(QP(1) + QP(4) + QP(13) + QP(16))
     1    + H12 * (QP(2) + QP(3) + QP(5) + QP(8) + QP(9) + QP(12) + QP(14) + QP(15))
     2    + H22 * (QP(6) + QP(7) + QP(10) + QP(11))
   50 continue
      return
C
      entry second (SRCR, SRCRT, MXC, MXT, I)
          if (NP.EQ.4) go to 60
      SRCR(I) = SRCRT(1) + SRCRT(2) + SRCRT(3) + SRCRT(4)
          go to 70
   60 SRCR(I) = H11*(SRCRT(1) + SRCRT(4) + SRCRT(13) + SRCRT(16))
     1     + H12*(SRCRT(2) + SRCRT(3) + SRCRT(5) + SRCRT(8) + SRCRT(9)+SRCRT(12)  
     1     + SRCRT(14) + SRCRT(15))
     2     + H22*(SRCRT(6) + SRCRT(7) + SRCRT(10) + SRCRT(11))
   70 continue
      return
C
      entry third (PE, SE, MXC)
      write (6, 130) L
      MM2 = (M+7)/8*8 - 7
          do 90 K1=1, MM2, 8
      K2 = K1 + 7
          if (K1.EQ.MM2) K2=MM2
          do 100 IM=1, M 
  100 write (6, 140) IM, (PE(IM, JM), JM=K1, K2)
  110 write (6, 120)
C     
  120 format (//)
  130 format (////, 10X, 7HELEMENT, I4, 5X, 16HSTIFFNESS MATRIX/)
  140 format (I5, 8E15.6)
  150 format (1H, 9X, 7HELEMENT, I4, 5X, 14HSTORAGE MATRIX/)
      
      return

      end 
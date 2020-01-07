      subroutine QZ (U, OLD, PHII, X, Y, FMOBX, FMOBY, F, DX, DY,
     & DETJ, CPHI, VKX, VKY, DGX, DGY, FF, IN, KF, JD, IEQ, LC, LR,
     & MS, MAXNN, MAXM1, MAXNE, MXC, MXT, KASE, L, M, IK, ISPK,
     & ISPM, PSIK, ISTOP)
C
C     Purpose: To evaluate line integral terms
C
      LEVEL 2, PHII, X, Y, FMOBX, FMOBY, KF, LC, LR, IN
      common /ONE/ BWD, CNTR, CHNG, TDR, STAT, STATP, OLDT, DELT,
     & DPRDT, BETAP, CLOS1, DELP, DIFUSN, PCHNG, TYPE, VMAX,
     & STIME, SSEC, H1, H2, PL, COEFI, EI, ITER1, ITMAX, ITCHNG,
     & IGO, NE, NP, NK, NSEEP, INC, ME, KOD1, KOD2, KOD3, KOD4,
     & KOD7, KOD8, KOD9, KOD10, KOD11, KOD12

      common /TWO/ WK(179), XK(15, 20), XM(15, 20), XPSI(15, 20),
     & CKT(3, 14, 20), CTT(3, 14, 20), PSIO(20), ISPL(20)

      dimensions U(MAXM1), OLD(MAXM1), PHII(MAXNN), X(MAXNN),
     & Y(MAXNN), FMOBX(MAXNE), FMOBY(MAXNE), F(MXC, MXT),
     & DX(MXC, MXT), DY(MXC, MXT), DETJ(MXT), CPHI(MXT),
     & VKX(MXT), VKY(MXT), DGX(MXC), DGY(MXC), FF(MXC),
     & IN(ME, MAXNE), KF(MAXNE), LC(MAXNN), LR(MAXNN)

      dimension AG(6), BG(4), JD(12), IEQ(4)

      data AG/-.577350 , .577350 , -.861136 , -.339981 ,
     & .339981 , .861136 /

      data BG/-1., 1., 1., -1./

      ISPK = ISPL(IK)
      PSIK = PSIO(IK)
      ISPM = ISPK - 1
      IS1 = 0
          do 300 IS = 1, 4
          if (KF(L).NE.IS) go to 300
      IS1 = IS1 + 1
          if (KASE.EQ.0) go to 95
      NP = 2
          if (M.GT.4) NP = 4
      K = 0
          do 60 I = 1, M
   50 K = K + 1
          if (IN(K, L).EQ.0) go to 50
      JD(I) = IN(K, L)
   60 continue
   95 call GREEN (X, Y, DETJ, AG, IN, KF, JD, IEQ, MS, MAXNN,
     & MAXNE, MXT, ME, NP, L, ISTOP)
          do 200 K = 1, NP
      J1 = K
          if (NP.EQ.4) J1 = K + 2
          if (IS.NE.1.AND.IS.NE.3) go to 100
      XI = AG(J1)
      YI = BG(IS)
          go to 120
  100 XI = BG(IS)
      YI = AG(J1)
  120 continue
      call SHAPE ( X, Y, IN, L, M, XI, YI, FF, DET, DGX, DGY,
     & MAXNN, MAXNE, MXC, ME)
      do 130 JJ = 1, M
      F(JJ, K) = FF(JJ)
      DX(JJ, K) = DGX(JJ)
  130 DY(JJ, K) = DGY(JJ)

C     Unsaturated properties
      VKX(K) = 0.0
      VKY(K) = 0.0
      CPHI(K) = 0.0
          do 160 I = 1, M
      JDI = JD(I)
          if (LR(JDI).EQ.1) go to 140
      J = JDI - LC(JDI)
      PHI = TDR * U(J) + (1. - TDR) * OLD(J)
          go to 150
  140 PHI = PHII(JDI)
  150 CPHI(K) = CPHI(K) + F(I, K) * PHI
  160 continue
          if (CPHI(K).LT.PSIK.AND.ISPK.GT.0) go to 170
      PPK = 1.0
          go to 179
  170 HMZ = ABS (CPHI(K))
      HMZ = ALOG10 (HMZ)
          if (HMZ.GE.XPSI(J + 1, IK)) go to 176
  175 continue
      J = ISPM
  176 X1 = HMZ - XPSI(J, IK)
      X2 = X1 * X1
      X3 = X2 * X1
      PP = CKT(3, J, IK) * X3 + CKT(2, J, IK) * X2 + CKT(1, J, IK)
     & * X1 + XK(J, IK)
      PPK = 10. **(PP - XK(IK, IK))
          if (PPK.GT.1.0) PPK = 1.0
  179 XCOND = -PPK * FMOBX(L)
      YCOND = -PPK * FMOBY(L)

          do 190 I = 1, M
      JDI = JD(I)
          if (LR(JDI).EQ.1) go to 180
      J = JDI - LC(JDI)
      PHI = TDR * U(J) + (1. - TDR) * OLD(J)
          go to 185
  180 PHI = PHII(JDI)
  185 VKX(K) = VKX(K) + DX(I, K) * PHI
      VKY(K) = VKY(K) + DY(I, K) * PHI
  190 continue
      VKX(K) = XCOND * VKX(K)
      VKY(K) = YCOND * VKY(K) + YCOND
  200 continue
  300 continue
  420 KASE = IS1
      return
      end

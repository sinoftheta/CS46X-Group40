      subroutine COGEN (S, P, FM, RT, PHI, PHII, U, OLD, CFM, CRT,
     & CONC, CONCI, FX, CN, VN, COEF, EST, FQ, CFQ, X, Y, FMOBX,
     & FMOBY, POR, ELONG, ETRANS, ALPHA, TTA, KD, LAMBDA, RHO,
     & IN, KF, LR, KLR, LC, KLC, LP, KLP, NSF, NSK, IE, MAXNN,
     & MAXNE, MAXBW2, MAXBW, MAXS, MAXM1, MAXM2, MAXM4, MXC, MXT,
     & KTCAL JTEST, NIT)
C
C     Purpose: To generate global coefficient matrices for flow
C     and mass transport
C
      real LAMBDA, KD
C**
      LEVEL 2, S, P
      LEVEL 2, CFQ, CONC, CONCI, FQ, KLC, KLR, LC, LR, PHI, PHII,
     & X, Y, ALPHA, ELONG, ETRANS, FMOBX, FMOBY, KD, KF, LAMBDA,
     & POR, RHO, TTA, IN, IE

      common /ONE/ BWD,CNTR, CHNG, TDR, STAT, STATP, OLDT, DELT,
     & DPRDT, BETAP, CLOS1, DELP, DIFUSN, PCHNG, TYPE, VMAX,
     & STIME, SSEC, H1, H2, PL, COEFI, EI, ITER1, ITMAX, ITCHNG,
     & IGO, NE, NP, NK, NSEEP, INC, ME, KOD1, KOD2, KOD3, KOD4,
     & KOD7,KOD8, KOD9, KOD10, KOD11, KOD12

      common /TWO/ WK(179), XK(15, 20), XM(15, 20), XPSI(15, 20),
     & CKT(3, 14, 20), CTT(3, 14, 20), PSIO(20), ISPL(20)

      common /THREE/ NN, MM, KM, NB, KNB, KNB2, MB, MB2, KMB,
     & KMB2, NSDN, IT, ISTOP

      common /FOUR/ PE(12, 12), SE(12, 12), F(12, 16), DX(12, 16),
     & DY(12, 16), Q(16), QP(16), DETJ(16), CPHI(16), VKX(16),
     & VKY(16), DPORDT(16), D0(16), DK(16), DH(16), SRCRT(16),
     & FF(12), DGX(12), DGY(12), SRCR(12)

      dimension S(MAXBW2, MAXS), P(MAXS, MAXBW), U(MAXM1),
     & OLD(MAXM1), FM(MAXM1), RT(MAXM1), PHI(MAXNN), PHII(MAXNN),
     & EST(MAXM1), CFM(MAXM2), CRT(MAXM2), CONC(MAXNN),
     & CONCI(MAXNN), FX(MAXM1), FQ(MAXNN), CFQ(MAXNN), X(MAXNN),
     & Y(MAXNN), VN(MAXM4), CN(MAXM4), COEF(MAXM4), FMOBX(MAXNE),
     & FMOBY(MAXNE), POR(MAXNE), ELONG(MAXNE), ETRANS(MAXNE),
     & ALPHA(MAXNE), TTA(MAXNE), KD(MAXNE), LAMBDA(MAXNE),
     & RHO(MAXNE), IN(ME, MAXNE), IE(2, MAXNE), KF(MAXNE),
     & LR(MAXNN), KLR(MAXNN), LC(MAXNN), KLC(MAXNN), LP(MAXM1),
     & KLP(MAXM2), NSF(MAXM4), NSK(MAXM4), AG(6), JD(12), IEQ(4)

      data AG/-.577350 , .577350 , -.861136 , -.339981 , .339981 ,
     & .861136 /


          if (JTEST.EQ.0) go to 50
          do 3 I = 1, MM
          do 3 J = 1, NB
      P(I, J) = 0.0
    3 S(J, I) = 0.0
          do 40 I = 1, NN
          if (LR(I).EQ.1) go to 40
      J = I - LC(I)
      FM(J) = 0.0
      RT(J) = 0.0
          if (LR(I).LT.2) U(J) = 0.0
      FX(J) = U(J)
          if (LR(I).NE.0) go to 40
      RT(J) = -FQ(I)
   40 continue
          go to 100
  50      do 65 I = 1, KM
          do 58 J = 1, KNB
   58 P(I, J) = 0.0
          do 60 J = 1, KNB2
   60 S(J, I) = 0.0
   65 continue
          do 90 I = 1, NN
          if (KLR(I).EQ.1) go to 90
      J = I - KLC(I)
      CFM(J) = 0.0
      CRT(J) = 0.0
          if (LR(I).NE.0) go to 90
          if (CFQ(I)) 80, 70, 70
   70 CRT(J) = -FQ(I) * CFQ(I)
          go to 90
   80 CRT(J) = -FQ(I) * CONC(I)
   90 continue
      VMAX = 0.0
  100 continue
      L = 0
  110 L = L + 1
      M = IN(ME, L)
      NP = 2
          if (M.GT.4) NP = 4
      NP2 = NP * NP
      K = 0
          do 170 I = 1, M
  160 K = K + 1
          if (IN(K, L).EQ.0) go to 160
      JD(I) = IN(K, L)
  170 continue

C
C     The array JD now contains the incidences of the active nodes
C       in element L.
C
C     Shape functions for integration points
C       Integration by Gaussian Quadrature
C     2 X 2 rule for fully linear elements
C     4 X 4 rule for all other elements
C
          do 150 I = 1, NP
          do 150 J = 1, NP
      K = (I - 1) * NP + J
          if (NP.EQ.4) go to 120
      XI = AG(J)
      YI = AG(I)
          go to 130
  120 XI = AG(J+2)
      YI = AG(I+2)
  130 continue

      call SHAPE (X, Y, IN, L, M, XI, YI, FF, DET, DGX, DGY,
     & MAXNN, MAXNE, MXC, ME)

          do 140 JJ = 1, M
      F(JJ, K) = FF(JJ)
      DX(JJ, K) = DGX(JJ)
  140 DY(JJ, K) = DGY(JJ)
      DETJ(K) = DET
  150 continue
C
C     Compute element matrices for flow
C
          if (JTEST.EQ.0) go to 290
      ST = TTA(L)/POR(L) * (ALPH(L) + POR(L) * BETAP)
      IE(I, L) = 0
          if (ISPL(1).EQ.0) go to 268
      IK = IE(2, L)
      ISPK = ISPL(IK)
      PSIK = PSIO(IK)
      ISPM = ISPK - 1
          do 265 I = 1, M
      JDI = JD(I)
          if (LR(JDI).EQ.1) go to 255
      IC = JDI - LC(JDI)
          if (LR(JDI).GE.2) go to 250
      SRCR(I) = EST(IC)
          go to 260
  250 SRCR(I) = U(IC)
          go to 260
  255 SRCR(I) = PHII(JDI)
  260     if (SRCR(I).GE.PSIK) go to 265
      IE(1, L) = L
  265 continue
          go to 270
  268     do 269 I = 1, M
  269 SRCR(I) = 0.0

  270     do 274 K = 1, NP2
      CPHI(K) = 0.0
          do 274 J = 1, M
  274 CPHI(K) = CPHI(K) + F(J, K) * SRCR(J)
  275 continue

C     Compute volume integrals
          if (IE(1, L).NE.0) go to 860
          do 850 I = 1, M
          do 850 J = I, M
          do 820 K = 1, NP2
      QP(K) = ST * F(I, K) * F(J, K) * DETJ(K)
      Q(K) = (FMOBX(L) * DX(I, K) * DX(J, K) + FMOBY(L) * DY(I, K))
     & * DETJ(K)
  820 continue
      call MATGEN (PE, SE, SRCR, Q, QP, SRCRT, MXC, MXT, I, J, L, M)
  850 continue
          do 858 I = 1, M
          do 855 K = 1, NP2
  855 SRCRT(K) = -FMOBY(L) * DY(I, K) * DETJ(K)
      call SECOND (SRCR, SRCRT, MXC, MXT, I)
  858 continue
          go to 890

  860     do 870 K = 1, NP2
          if (CPHI(K).LT.PSIK) go to 861
      VKX(K) = FMOBX(L)
      VKY(K) = FMOBY(L)
      CPHI(K) = ST
          go to 870
  861 HMZ = ABS(CPHI(K))
      HMZ = ALOG10(HMZ)
          if (HMZ.GE.XPSI(1, IK)) HMZ = XPSI(1, IK)
          do 862 J = 1, ISPM
          if (HMZ.GE.XPSI(J+1, IK)) go to 863
  862 continue
      TETA = 1.0
      PPK = 1.0
      CE = 0.0
          go to 866
  863 X1 = HMZ - XPSI(J, IK)
      X2 = X1 * X1
      X3 = X2 * X1
      TETA = (CTT(3, J, IK) * X3 + CTT(2, J, IK) * X2
     & + CTT(1, J, IK) * X1 + XM(J, IK)) / XM(ISPK, IK)

      CE = (3. * CTT(3, J, IK) * X2 + 2. * CTT(2, J, IK) * X1
     & + CTT(1, J, IK)) * 0.43429 / CPHI(K)

      PP = CKT(3, J, IK) * X3 + CKT(2, J, IK) * X2 + CKT(1, J, IK)
     & * X1 + XK(J, IK)

      PPK = 10.**(PP - XK(ISPK, IK))
          if (TETA.GT.1.0) TETA = 1.0
          if (PPK.GT.1.0) PPK = 1.0
      CEE = CE
      CE = ABS(CE)
  866 VKX(K) = PPK * FMOBX(L)
      VKY(K) = PPK * FMOBY(L)
      CPHI(K) = TETA * TTA(L) * BETAP + CE
  870 continue
          do 880 I = 1, M
          do 880 J = I, M
          do 875 K = 1, NP2
      QP(K) = CPHI(K) * F(I, K) * F(J, K) * DETJ(K)
      Q(K) = CPHI(K) * DX(J, K) + VKY(K) * DY(J, K) * DY(I, K)
     & * DETJ(K)
  875 continue
      call MATGEN (PE, SE, SRCR, Q, QP, SRCRT, MXC, MXT, I, J, L, M)
  880 continue
C
          do 910 I = 1, M
          do 900 K = 1, NP2
  900 SRCRT(K) = -VKY(K) * DY(I, K) * DETJ(K)
      call SECOND (SRCR, SRCRT, MXC, MXT, I)
  910 continue
  890     do 895 I = 2, M
      I1 = I - 1
          do 895 J = 1, I1
      PE(I, J) = PE(J, I)
  895 SE(I, J) = SE(J, I)

C
C     Print element matrices for flow

          if (KOD1.NE.1) go to 1235
      write (6, 495)
      call THIRD (PE, SE, MXC)
 1235 continue

C
C     Assembly
      call ASEMBL (S, P, SE, PE, RT, FX, SRCR, PHII, LR, LC, JD,
     & M, L, MAXBW2, MAXS, MAXS, MAXBW, MAXM1, MAXNN, MXC, NB,
     & MB, MB2, ISTOP)
          go to 380

C
C     compute concentration coefficient matrices
  290 continue
      VLX = 0.0
      VLY = 0.0
      RD = 1. + KD(L) * RHO(L) / POR(L)
          do 292 K = 1, NP2
      VKX(K) = 0.0
      VKY(K) = 0.0
      CPHI(K) = 0.0
      DPORDT(K) = 0.0
      DK(K) = 0.0
      DH(K) = 0.0
          do 294 J = 1, M
      JDJ = JD(J)
      JC = JD(J) - LC(JDJ)
          if (LR(JDJ).EQ.1) go to 293
      DPORDT(K) = DPORDT(K) + F(J, K) * PHI(JDJ)
  294 continue
          if (IE(1, L).EQ.0) go to 297
      IK = IE(2, L)
      ISPK = ISPL(IK)
      PSIK = PSIO(IK)
      ISPM = ISPK - 1
          if (CPHI(K).GE.PSIK) go to 297
      HMZ = ABS(CPHI(K))
      HMZ = ALOG10(HMZ)
          if (HMZ.GE.XPSI(1, IK)) go to 296
  295 continue
      J = ISPM
  296 X1 = HMZ - XPSI(J, IK)
      X2 = X1 * X1
      X3 = X2 * X1
      TETA = (CTT(3, J, IK) * X3 + CTT(2, J, IK) * X2
     & + CTT(1, J, IK) * X1 + XM(J, IK)) / XM(ISPK, IK)

      CE = (3. * CTT(3, J, IK) * X2 + 2. * CTT(2, J, IK) * X1
     & + CTT(1, J, IK)) * 0.43429 / CPHI(K)

      PP = CKT(3, J, IK) * X3 + CKT(2, J, IK) * X2 + CKT(1, J, IK)
     & X1 + XK(J, IK)

      PPK = 10.**(PP - XK(ISPK, IK))
          if (PPK.GT.1.0) PPK = 1.0
          if (TETA.GT.1.0) TETA = 1.0
      XCOND = -PPK * FMOBX(L) / (TETA * TTA(L))
      YCOND = -PPK * FMOBY(L) / (TETA * TTA(L))
      CPHI(K) TETA * TTA(L)
      D0(K) = EXP(10. * CPHI(K))
      DH(K) = RD * ALPHA(L) * CPHI(K) * DPORDT(K) / DELT
      DPORDT(K) = DPORDT(K) / DELT * CE
      DPORDT(K) = DPORDT(K) * DPRDT * RD
          go to 298
  297 CPHI(K) = POR(L)
      D0(K) = EXP(10. * POR(L))
      DH(K) = RD * ALPHA(L) * CPHI(K) * DPORDT(k) / DELT
      DPORDT(K) = 0.0
      XCOND = -FMOBX(L) / POR(L)
      YCOND = -FMOBY(L) / POR(L)
  298 DK(K) = RD * LAMBDA(L) * CPHI(K)
          do 291 J = 1, M
      JDJ = JD(J)
      VKX(K) = VKX(K) + DX(J, K) * PHI(JDJ)
  291 VKY(K) = VKY(K) + DY(J, K) * PHI(JDJ)
      VKY(K) = YCOND * (VKY(K) + 1.0)
      VKX(K) = XCOND * VKX(K)
      VLX = VLX + VKX(K)
      VLY = VLY + VKY(K)
  292 continue
      VLX = VLX / NP2
      VLY = VLY / NP2
      VMAX = AMAX1(VMAX, ABS(VLX), ABS(VLY))
          do 310 I = 1, M
      SRCR(I) = 0.0
          do 310 J = 1, M
          do 300 K = 1, NP2
          if (DIFUSN.GE.0) go to 722
      DISPX = ELONG(L)
      DISPY = ETRANS(L)
      DISPXY = 0.0
          go to 724
  722 VXSQRD = VKX(K) * VKX(K)
      VYSQRD = VKY(K) * VKY(K)
      VSQRD = VXSQRD + VYSQRD
          if (VSQRD.NE.0.) go to 723
      DISPX = DIFUSN
      DISPY = DIFUSN
      DISPXY = 0.0
          go to 724
  723 UU = SQRT(VSQRD)
      DL = ELONG(L) * UU
      DT = ETRANS(L) * UU
      DVX = VXQRD / VSQRD
      DISPX = DL * DVX + DT * DVY + DIFUSN * D0(K)
      DISPY = DT * DVX + DL * DVY + DIFUSN * D0(K)
      DISPXY = (DL - DT) * VKX(K) * VKY(K) / VSQRD + DIFUSN
     & * D0(K)

C
C     DISPXY = 0.0
C

  724 continue
      QP(K) = CPHI(K) * F(I, K) * F(J, K) * DETJ(K) * STAT * RD
      Q(K) = ((DISPX * DX(I, K) * DX(J, K) + DISPY * DY(I, K)
     & + DISPXY * (DX(I, K) * DY(J, K) * DX(J, K))
     & + (VKX(K) * DX(J, K) + VKY(K) * DY(J, K)) * F(I, K))
     & * CPHI(K) + (DPORDT(K) + DK(K) + DH(K)) * F(I, K)
     & * F(J, K)) * DETJ(K)
  300 continue
C
      call MATGEN (PE, SE, SRCR, Q, QP, SRCRT, MXC, MXT, I, J,
     & L, M)
C
  310 continue
C
      KASE = 0
      call QZ (U, OLD, PHII, X, Y, FMOBX, FMOBY, F, DX, DY, DETJ,
     & CPHI, VKX, VKY, DGX, DGY, FF, IN, KF, JD, IEQ, LC, LR MS,
     & MAXNN, MAXM1, MAXNE, MXC, MXT, KASE, L, M, IK, ISPK, ISPM,
     & PSIK, ISTOP)
          if (KASE.EQ.0) go to 315
          do 330 I = 1, MS
      II = IEQ(I)
      JDII = JD(II)
          if (KLR(JDII).NE.-4) go to 330
          do 325 J = 1, MS
      JJ = IEQ(J)
      JDJJ = JD(JJ)
          if (KLR(JDJJ).NE.-4) go to 325
          do 324 K = 1, NP
  324 Q(K) = (VKX(K) * DETJ(K + NP) - VKY(K) * DETJ(K)) * F(II, K)
          if (NP.EQ.4) go to 326
      AP = Q(1) + Q(2)
          go to 327
  326 AP = H1 * (Q(1) + Q(4)) + H2 * (Q(2) + Q(3))
  327 SE(II, JJ) = SE(II, JJ) - AP
  325 continue
  330 continue
      KC = 0
          do 313 I = 1, MS
      II = IEQ(I)
      JDII = JD(II)
          if (KLR(JDII).NE.-4) go to 313
      KC = KC + 1
          do 311 K = 1, NP
  311 SRCRT(K) = (VKX(K) * DETJ(K + NP) - VKY(K) * DETJ(K))
     & * F(II, K)
          if (NP.EQ.4) go to 312
      SRCR(II) = SRCRT(1) + SRCRT(2)
          go to 314
  312 SRCR(II) = H1 * (SRCRT(1) + SRCRT(4)) + H2 *(SRCRT(2)
     & + SRCRT(3))
  314 SRCR(II) = -SRCR(II) * CN(KC)
  313 continue

C
C     Print element matrices for concentration
C
  315     if (KOD3.NE.1) go to 320
      write (6, 500)

      call THIRD (PE, SE, MXC)
  320 continue

C
C     Assembly of global coefficient matrix
C
      call ASEMBL (S, P, SE, PE, CRT, CRT, SRCR, CONCI, KLR, KLC,
     & JD, M, L, MAXBW2, MAXS, MAXS, MAXBW, MAXM2, MAXNN, MXC,
     & KNB, KNB2, KMB, KMB2, ISTOP)

  380     if (L.EQ.NE) go to 385
          go to 110

C
C     Write block on tape
C
  385     if (JTEST.EQ.0) go to 430
          if (ISPL(1).NE.0) go to 386
          if (STATP.EQ.0) go to 388
      rewind 11
      write (11) ((P(I, J), J = 1, MB), I = 1, MM)
          if (MOD(IT + 1, ITCHNG).NE.0.AND.NSDN.EQ.0) go to 388
          go to 387
  386     if (NSDN.EQ.0) go to 388
  387 rewind 13
      write (13) ((S(I, J), J = 1, MM), I = 1, MB)
  388     if (KOD2 - 1) 480, 420, 410
  410 write (6, 530)
      write (6, 585)
      call SOS (S, MB, MM, 1, MAXBW2, MAXS)
      write (6, 580)
      call SOS (P, MM, MB, 1, MAXS, MAXBW)
  420 write (6, 590)
      write (6, 600) (RT(I), I = 1, MM)
          go to 480
  430     if (MOD (IT + 1, ITCHNG).NE.0) go to 435
      rewind 2
      KB1 = KNB - KMB + 1
      write (2) ((S(I, J), J = 1, KM ), I = KB1, KMB2)
  435     if (KOD4 - 1) 475, 450, 440
  440 write (6, 610)
      write (6, 620)
      call SOS (S, KMB2, KM, KNB - KMB + 1, MAXBW2, MAXS)
      write (6, 630)
      call SOS (P, KM, KMB, 1, MAXS, MAXBW)
  450 write (6, 590)
      write (6, 600) (CRT(I), I = 1, KM)
  475 write (6, 4800) VMAX
  480 continue

  495 format (1H1, ////11X,
     & 25HELEMENT MATRICES FOR FLOW/11X, 25(1H-))
  500 format (1H1, ////11X,
     & 34HELEMENT MATRICES FOR CONCENTRATION/11X,
     & 34(1H-))
  530 format (1H1, 10X,
     & 36HGLOBAL COEFFICIENT MATRICES FOR FLOW/11X, 36(1H-))
  580 format (1H0, 10X,
     & 37HP COEFFICIENT MATRIX - UPPER HALFBAND/11X, 37(1H-))
  585 format (1H0, 10X,
     & 37HS COEFFICIENT MATRIX - UPPER HALFBAND/11X, 37(1H-))
  590 format (/////, 11X, 20HF COEFFICIENT MATRIX/11X,
     & 20(1H-)//)
  600 format (/(5X, 10E12.4))
  610 format (1H1, 10X,
     & 37HGLOBAL COEFFICIENT MATRICES FOR CONC./11X, 37(1H-))
  620 format (1H0, 10X, 20HS COEFFICIENT MATRIX/11X, 20(1H-)//)
  630 format (1H0, 10X, 20HP COEFFICIENT MATRIX/11X, 20(1H-)//)
 4800 format (/11X, 4HVMAX, E15.5)
      return
      end

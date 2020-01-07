      Subroutine TS (S, P, W, FM, RT, PHI, PHII, OLD, CFM, CRT,
     & CONC, CONCI, COLD, FX, CN, VN, COEF, EST, U, FQ, CFQ,
     & X, Y, FMOBX, FMOBY, ELONG, ETRANS, POR, ALPHA, TTA, KD,
     & LAMBDA, RHO, IN, KF, LR, KLR, LC, KLC, LP, KLP, NSF,
     & NSK, IE, NSP, MSP, MAXNN, MAXNE, MAXM1, MAXM2, MAXM4,
     & MAXM5, MAXEEP, MAXBW, MAXBW2, MX, MAXS, MXC, MXT,
     & RDATE, RTIME)

C
C     Purpose: To formulate and solve flow and mass-transport equations
C
      Real LAMBDA, KD
      Character RDATE*10
      Character RTIME*10

      Level 2, S, P, W
      Level 2, CFQ, CONC, CONCI, FQ, KLC, KLR, LC, LR, PHI, PHII,
     & X, Y, ALPHA, ELONG, ETRANS, FMOBX, FMOBY, KD, KF, LAMBDA,
     & POR, RHO, TTA, IN, IE
      Common /ONE/ BWD, CNTR, CHNG, TDR, STAT, STATP, OLDT, DELT,
     & DPRDT, BETAP, CLOS1, DELP, DifUSN, PCHNG, TYPE, VMAX,
     & STIME, SSEC, H1, H2, PL, COEFI, EI, ITER1, ITMAX, ITCHNG,
     & IGO, NE, NP, NK, NSEEP, INC, ME, KOD1, KOD2, KOD3, KOD4,
     & KOD7, KOD8, KOD9, KOD10, KOD11, KOD12
      Common /TWO/ WK(179), XK(15, 20), XM(15, 20), XPSI(15, 20),
     & CKT(3, 14, 20), CTT(3, 14, 20), PSIO(20), ISPL(20)
      Common /THREE/ NN, MM, KM, NB, KNB, KNB2, MB, MB2, KMB,
     & KMB2, NSDN, IT, ISTOP
      Common /FOUR/ PE(12, 12), SE(12, 12), F(12, 16), DX(12, 16),
     & DY(12, 16), Q(16), QP(16), DETJ(16), CPHI(16), VKX(16),
     & VKY(16), DPORDT(16), D0(16), DK(16), DH(16), SRCRT(16),
     & FF(12), DGX(12), DGY(12), SRCR(12)
      Dimension S(MAXBW2, MAXS), P(MAXS, MAXBW), W(MX), FM(MAXM1),
     & RT(MAXM1), PHI(MAXNN), PHII(MAXNN), OLD(MAXM1), U(MAXM1),
     & EST(MAXM1), CFM(MAXM2), CRT(MAXM2), CONC(MAXNN),
     & CONCI(MAXNN), COLD(MAXM2), FX(MAXM1), FQ(MAXNN),
     & CFQ(MAXNN), X(MAXNN), Y(MAXNN), CN(MAXM4), VN(MAXM4),
     & COEF(MAXM4), FMOBX(MAXNE), FMOBY(MAXNE), POR(MAXNE),
     & ELONG(MAXNE), ETRANS(MAXNE), ALPHA(MAXNE), TTA(MAXNE),
     & KD(MAXNE), LAMBDA(MAXNE), RHO(MAXNE), IN(ME, MAXNE),
     & IE(2, MAXNE), KF(MAXNE), LR(MAXNN), KLR(MAXNN), LC(MAXNN),
     & KLC(MAXNN), LP(MAXM1), KLP(MAXM2), NSF(MAXM4), NSK(MAXM4),
     & NSP(MAXM5, MAXEEP), MSP(MAXEEP)
      Data KTCAL/0/, YES/0./,NO/1/

C     Transient Solution
C
  520 ADVANC = NO
          if (MOD(IT, IGO).EQ.0) ADVANC=YES
          if (ADVANC.EQ.NO) GO TO 595
C     Compute maximum value of delta p
          if (KTCAL.EQ.0) GO TO 530
      DELP = 0.0
          do 10 I=1, NN
          if (LR(I).EQ.1) GO TO 10
      J = I - LC(I)
      DELP = AMAX1(DELP, ABS(PHI(I) - OLD(J)))
   10 continue
  530 continue
          if (KTCAL.EQ.0) GO TO 580
C     Update old
          do 20 I = 1, MM
  20  EST(I) = OLD(I)
      call UPD (PHI, OLD, LR, LC, MAXNN, MAXM1, NN)
          if (MOD(IT, ITCHNG).EQ.0) GO TO 35
          do 30 I = 1, MM
      EST(I) = OLD(I) + DELT / OLDT / 2.0 * (OLD(I) - EST(I))
  30  continue
          GO TO 535

C
C     Time step modification
  35  DELT = CHNG * DELT
          if (DELT.GT.0.0) GO TO 52
      read (5, 1980) DELT
      DELT = DELT * 3600.
  52  write (6, 534) DELT
          do 40 I = 1, MM
  40  EST(I) = OLD(I) + DELT / OLDT / 2.0 * (OLD(I) - EST(I))
          if (STATP.EQ.0.) GO TO 538
          GO TO 580
  535     if (STATP.NE.0.) GO TO 580
  538     if (ISPL(1).EQ.0) GO TO 539
  51      if (STAT.LT.0.) GO TO 580
      write (6, 536) DELP
          GO TO 532
  539 write (6, 533)
  532 ADVANC = NO
          GO TO 595
  580 continue
          if (MOD(IT+1, KOD9).NE.0) GO TO 581
      write (6, 1900)
      write (6, 1080)
      write (6, 1090) (LP(I), EST(I), I = 1, MM)

C
C     Select approximation for time derivative
  581 DELTGO = DELT * IGO
      A3 = TDR/DELTGO
C
      NIT = 0
  2   NIT = NIT + 1
          if (KTCAL.EQ.0) GO TO 540
          if (ISPL(1).EQ.0) GO TO 541

C
C     Generate coef. matrices for flow
  540 JTEST = 1
      call COGEN (S, P, FM, RT, PHI, PHII, U, OLD, CFM, CRT,
     & CONC, CONCI, FX, CN, VN, COEF, EST, FQ, CFQ, X, Y, FMOBX,
     & FMOBY, POR, ELONG, ETRANS, ALPHA, TTA, KD, LAMBDA, RHO,
     & IN, KF, LR, KLR, LC, KLC, LP, KLP, NSF, NSK, IE, MAXNN,
     & MAXNE, MAXBW2, MAXBW, MAXS, MAXM1, MAXM2, MAXM4, MXC, MXT,
     & KTCAL, JTEST, NIT)
          if (ISTOP.GT.0) GO TO 821
      JTEST = -1
          GO TO 543

C
C     Add time-dependent parts
  541 Rewind 11
      read (11) ((P(I, J), J=1, MB), I = 1, MM)
          if (NIT.GT.1) GO TO 543
          if (MOD(IT, ITCHNG).EQ.0) GO TO 542
      JTEST = JTEST + 1
          GO TO 543
 542  Rewind 13
      read (13) ((S(I, J), J = 1, MM), I = 1, MB)
 543  continue
      call LRHS (S, P, FM, RT, OLD, U, LR, LC, NN, NB, MB, A3,
     & 0.0, MAXBW2, MAXS, MAXS, MAXBW, MAXM1, MAXNN, 1)
          if (KOD8.LT.1) GO TO 544
      write (6, 1325)
      write (6, 1398) (FM(I), I = 1, MM)
 544      if (JTEST.GT.1) GO TO 555
C
      call LRHS (S, P, FM, RT, OLD, U, LR, LC, NN, NB, MB, A3,
     & 0.0, MAXBW2, MAXS, MAXS, MAXBW, MAXM1, MAXNN, 3)

C
C     Apply boundary conditions
          do 1675 I = 1, NN
          if (LR(I).LT.2) GO TO 1675
      J = I - LC(I)
      UI = U(J)
      call ZERO (P, FX, UI, MM, MB, MAXS, MAXBW, MAXM1, J)
      1675 continue
          if (KOD8-1) 1690, 1643, 1641
 1641 write(6, 1710)
      call SOS (S, MB, MM, 1, MAXBW2, MAXS)
      write (6, 1730)
      call SOS (P, MM, MB, 1, MAXS, MAXBW)
 1643 write (6, 1725)
      write (6, 1398) (FX(I), I = 1, MM)
 1690 continue
      KKK = 1
 555  continue
C
      if (JTEST.GE.0) GO TO 558
      JTEST = 1
      if (KTCAL.EQ.0) WRITE (6, 1620) MB, MB2
      if(MOD(IT+1, KOD9).NE.0) GO TO 558
      write (6, 1880)
      write (6, 1890) (IE(1, L), L = 1, NE)
 558    Do 560 I = 1, NN
        if (LR(I).EQ.1) GO TO 560
      J = I - LC(I)
      if (LR(I).GE.2) GO TO 559
      FM(J) = FM(J) + RT(J) + FX(J)
        if (LR(I).LT.0) FM(J) = FM(J) + FQ(I)
        GO TO 560
 559  FM(J) = FX(J)
 560  continue
      if (KOD8.LT.1) GO TO 563
      write (6, 1320)
      write (6, 1398) (FM(I), I = 1, MM)

C
C     Solve for pressure
 563  if (KKK.GT.1) GO TO 565
      call DBAND (P, MM, MB, MAXS, MAXBW, IEX)
      if (IEX.EQ.0) GO TO 565
      write (6, 1810) IEX
      ISTOP = ISTOP + 1
      GO TO 821
 565  KKK = KKK + 1
      call SBAND (P, FM, U, MM, MB, MAXS, MAXBW, MAXM1)

C
C     Determine boundary flux
      if (NSDN.GT.0.AND.COEFI.EQ.1.) GO TO 105
      if (NSEEP.EQ.0) GO TO 350
 105  Rewind 13
 110  read (13) ((P(J, I), J = 1, MM), I = 1, NB)
      call LRHS (S, P, FM, RT, OLD, U, LR, LC, NN, NB, MB, 1.-TDR,
     & TDR, MAXBW2, MAXS, MAXS, MAXBW, MAXM1, MAXNN, 1)
 150  continue
C
 155  continue

C
C     Modify surface flux boundaries
      if (NSDN.EQ.0.OR.COEFI.NE.1) GO TO 292
      if (MOD(IT+1, KOD9).EQ.0) write (6, 1870)
          Do 280 K = 1, NSDN
      I = NSF(K)
      J = I - LC(I)
      FM(J) = FM(J) - RT(J)
          if (LR(I).NE.4) GO TO 230
          if (MOD(IT+1, KOD9).EQ.0) write (6, 1875) I, LR(I), U(J),
     & FM(J)
          if ((EI.LT.0.AND.FM(J).GE.EI*VN(K)).OR.(EI.GE.0..AND.FM(J).
     & LE.EI*VN(K))) GO TO 280
      LR(I) = -4
      FQ(I) = EI*VN(K)
          GO TO 280
 230  if (LR(I).NE.-4) GO TO 280
      PN = TDR * U(J) + (1. - TDR) * OLD(J)
          if (Mod(IT + 1, KOD9).EQ.0) write (6, 1876) I, LR(I),
     & PN, FQ(I), COEF(K)
          if (EI.GT.0.) GO TO 250
          if (PN.GT.PL) GO TO 240
      LR(I) = 4
      U(J) = PL
          GO TO 280
 240  if (Abs(PN).GT.-0.001 * PL) COEF(K) = COEF(K) * Abs(PL/PN)
      if (Abs(PN).LE.-0.001 * PL) COEF(K) = 1.0
      GO TO 270
 250  if (PN.LT.0.) GO TO 260
      LR(I) = 4
      U(J) = 0.
          GO TO 280
 260      if (Abs(PN - PL).GT.-0.001 * PL) COEF(K) = COEF(K) *
     & ABS(PL / (PN - PL))
          if (Abs(PN - PL).LE.-0.001 * PL) COEF(K) = 1.0
          if (PN.LT.PL) COEF(K) = 1.0
 270      if (COEF(K).GT.1.0) COEF(K) = 1.0
      FQ(I) = COEF(K) * EI * VN(K)
 280  continue
          if (Mod(IT + 1, KOD9).NE.0) GO TO 290
      write (6, 1877)
          Do 285 K = 1, NSDN
      I = NSF(K)
      J = I - LC(I)
          if (LR(I).EQ.4) write (6, 1878) I, LR(I), U(J)
          if (LR(I).EQ.-4) write (6, 1879) I, LR(I), FQ(I), COEF(K)
 285  continue
 290  continue

C
C     Modify conditions on seepage faces
          if (NSEEP.EQ.0) GO TO 350
 292  write (6, 1910)
          Do 320 K = 1, NSEEP
      ICHECK = 0
      NT = MSP(K)
          Do 310 JJ = 1, NT
      I = NSP(JJ, K)
      J = I - LC(I)
          if (LR(I).NE.-2) GO TO 295
      FM(J) = FM(J) - RT(J)
      PN = TDR * U(J) + I, LR(I), PN, FQ(I), K
          if (PN.LT.0.0) ICHECK = 1
          if (ICHECK.GT.0) GO TO 310
      LR(I) = 2
      U(J) = 0.0
          GO TO 310
 295      if (LR(I).NE.2) GO TO 310
      write (6, 1930) I, LR(I), U(J), FM(J), K
          if (ICHECK.GT.0) GO TO 300
          if (FM(J).LT.0.) GO TO 310
 300  LR(I) = -2
      FQ(I) = 0.0
      ICHECK = 1
 310  continue
 320  continue
      write (6, 1940)
          Do 340 K = 1, NSEEP
      NT = MSP(K)
          Do 330 JJ = 1, NT
      I = NSP(JJ, K)
      J = I - LC(I)
          if (LR(I).EQ.2) write (6, 1950) I, LR(I), U(J), K
          if (LR(I).EQ.-2) write (6, 1960) I, LR(I), FQ(I), K
 330  continue
 340  continue
C
 350  ISK = 0
          if(ITER1.EQ.1) GO TO 567
          Do 56 I = 1, MM
          if (EST(I).EQ.0.) GO TO 56
 54   UN = 0.5 * (TDR * U(I) + (1. - TDR) * OLD(I) + OLD(I))
          if (Abs((UN - EST(I))/EST(I)).GT.CLOS1) ISK = ISK + 1
 56   EST(I) = U(I)
          if (KOD12.EQ.0) GO TO 58
      write (6, 1820) NIT, ISK
      write (6, 1080)
      write (6, 1090) (LP(I), U(I), I = 1, MM)
 58       if (ISK.EQ.0.AND.KTCAL.GT.0) GO TO 567
          if (NIT.GE.ITER1) GO TO 567
          Do 57 I = 1, MM
      EST(I) = 0.5 * (TDR * U(I) + (1. - TDR) * OLD(I) + OLD(I))
 57   continue
          GO TO 2

C
C     Calculate new values
 567      Do 572 I = 1, NN
          if (LR(I).EQ.1) GO TO 568
      J = I - LC(I)
      PHI(I) = TDR * U(J) + (1. - TDR) * OLD(J)
          GO TO 572
 568  PHI(I) = PHII(I)
 572  continue
          if (STAT.GE.0.) GO TO 595
      DELT1 = DELTGO / 3600.
      OLDT = DELT
      KTCAL = KTCAL + 1
      IT = KTCAL
      SSEC = SSEC + DELTGO
      SMIN = SSEC / 60.
      STIME = SMIN / 60.
          GO TO 712
C
C     Select approximation for time derivative
 595  A3 = TDR / DELT
      Rewind 4
C
          if (ADVANC.EQ.NO) GO TO 600

C
C     Generate matrices for concentration
      JTEST = 0
      call COGEN (S, P, FM, RT, PHI, PHII, U, OLD, CFM, CRT, CONC,
     & CONCI, FX, CN, VN, COEF, EST, FQ, CFQ, X, Y, FMOBX, FMOBY,
     & POR, ELONG, ETRANS, ALPHA, TTA, KD, LAMBDA, RHO, IN, KF,
     & LR, KLR, LC, KLC, LP, KLP, NSF, NSK, IE, MAXNN, MAXNE,
     & MAXBW2, MAXBW, MAXS, MAXM1, MAXM2, MAXM4, MXC, MXT, KTCAL,
     & JTEST, NIT)
          if (ISTOP.GT.0) GO TO 821
      JTEST = -1
C
 660  continue
C
C     Update COLD
          if (KTCAL.EQ.0) GO TO 610
      call UPD (CONC, COLD, KLR, KLC, MAXNN, MAXM2, NN)
 610  continue

C
C     Add time-dependent part
          if (ADVANC.EQ.NO.AND.MOD(IT, ITCHNG).NE.0) GO TO 680
          if (STAT.EQ.0.) GO TO 615
          if (JTEST.LT.0) GO TO 612
      Rewind 2
      KB1 = KNB - KMB + 1
      read (2) ((S(I, J), J = 1, KM), I = KB1, KMB2)
 612  call LRHS (S, P, CFM, CRT, COLD, CRT, KLR, KLC, NN, KNB,
     & KMB, A3, 0.0, MAXBW2, MAXS, MAXS, MAXBW, MAXM2, MAXNN, 2)
 615      if (KOD7 - 1) 660, 660, 651
 651  write (6, 1720)
      call SOS (S, KMB2, KM, KNB - KMB+1, MAXBW2, MAXS)
      write (6, 1730)
      call SOS (P, KM, KMB, 1, MAXS, MAXBW)
 660  continue
C
      call Array (S, W, KM, KNB, KMB, KMB2, MAXBW2, MAXS, MX, JX)
      write (4) (W(I), I = 1, JX)
          GO TO 682
 680  read (4) (W(I), I = 1, JX)
 682      if (STAT.EQ.0.) GO TO 630
      call LRHS (S, P, CFM, CRT, KLR, KLC, NN, KNB, KMB, A3, 0.0,
     & MAXBW, MAXS, MAXS, MAXBW, MAXM2, MAXNN, 1)
      if (KOD7.LT.1) GO TO 630
      write (6, 1325)
      write (6, 1398) (CFM(I), I = 1, KM)
 630  continue
C
          if (JTEST.GE.0) GO TO 684
      JTEST = 1
          if (KTCAL.EQ.0) write (6, 1630) KMB, KMB2
 684      Do 685 I = 1, NN
          if (KLR(I).EQ.1) GO TO 685
      J = I - KLC(I)
      CFM(J) = CFM(J) + CRT(J)
 685  continue
          if (KOD7.LT.1) GO TO 686
      write (6, 1320)
      write (6, 1398) (CFM(I), I = 1, KM)
 686  continue

C
C     Solve concentration equations
      call GELB (CFM, W, KM, 1, KMB - 1, KMB - 1, 1.E-20, IER,
     & MAXM2, MX)
          if (IER.EQ.0) GO TO 695
      write (6, 1800) IER
              ISTOP = ISTOP + 1
          GO TO 821
 695  continue
C
      KTCAL = KTCAL + 1
      OLDT = DELT
      SSEC = SSEC + DELT
      SMIN = SSEC / 60.
      STIME = SMIN / 60.
      IT = IT + 1

C
C     Calculate new values
          Do 710 I = 1, NN
          if (KLR(I).GT.0) GO TO 709
      J = I - KLC(I)
      CONC(I) = TDR * CFM(J) + (1. - TDR) * COLD(J)
          GO TO 710
 709  CONC(I) = CONCI(I)
 710  continue

C
C     write computed values
      DELT1 = DELT / 3600.
 712      if (Mod(IT, KOD9).NE.0.AND.MOD(IT, KOD10).NE.0) GO TO 800
      write (6, 1051) RDATE, RTIME
      write (6, 1360) IT, DELT1, STIME, SMIN, SSEC
 800      if (Mod(IT, KOD9).NE.0) GO TO 810
      write (6, 1380)
      write (6, 1080)
      write (6, 1090) (I, PHI(I), I = 1, NN)
 810      if (STAT.LT.0.) GO TO 835
          if (Mod(IT, KOD10).NE.0) GO TO 820
      write (6, 1390)
      write (6, 1080)
      write( 6, 1090) (I, CONC(I), I = 1, NN)
 820  write (6, 1400)
      write (6, 1410)
 835      if (KOD11.EQ.0) GO TO 830
          if (Mod(IT, KOD11).EQ.0) call PO (CONC, PHI, NN, IT,
     &    STIME, MAXNN, RDATE, RTIME)
 830      if (IT.LT.ITMAX) GO TO 520
      write (6, 1430) IT
 836      if (KOD11.EQ.0) GO TO 821
          if (Mod(IT, KOD11).EQ.0) GO TO 821
      call PO (CONC, PHI, NN, IT, STIME, MAXNN, RDATE, RTIME)
 821  continue
C
 533  format (/11X,29HPRESSURE WAS NOT RE-EVALUATED)
 534  format (/11X,31HTIME STEP MODIFICATION: DELT = ,E15.5)
 536  format (/11X,38HPRESSURE WAS NOT RE-EVALUATED: DELP = ,E15.8)
 1051 format (1X,'RUN IDENTIFICATION: ', A10, 1X, A10)
 1080 format (/11X,6(4HNODE, 5X, 5HVALUE, 5X))
 1090 format (/(11X,6(I4, 2X, 1PE10.3, 3X)))
 1320 format (/11X, 28HRHS VECTOR (INPUT TO SOLVER)/11X, 28(1H-))
 1325 format (/11X,33HTIME-DEPENDENT PART OF RHS VECTOR/11X, 33(1H-))
 1360 format (1H0, //////11X, 16HTIME STEP NUMBER, I20/11X,
     & 17HTIME STEP (HOURS), 1PE19.3/11X, 12HELAPSED TIME,
     & 5X, 1PE19.3, 6H HOURS/28X, 1PE19.3, 8H MINUTES/28X, 1PE19.3,
     & 8H SECONDS)
 1380 format (//11X, 4HHEAD/11X, 4(1H-))
 1390 format (//11X,13HCONCENTRATION/11, 13(1H-))
 1398 format (/(5X, 10E12.4))
 1400 format (/11X, 114(1H*))
 1410 format (1H1)
 1430 format (////11X, 10(1H*),
     & 34HEXECUTION TERMINATED ON TIME STEPS, 8H AT STEP,
     & I10, 10(1H*))
 1620 format (////11X, 29HFINAL HALF-BANDWIDTH FOR FLOW, 2I5)
 1630 format(////11X,
     & 44HFINAL HALF-BANDWIDTH OF P FOR MASS TRANSPORT, I5/11X,
     & 39HFINAL BANDWIDTH OF S FOR MASS TRANSPORT, I5 )
 1710 (//1H1, 35H GLOBAL COEFFICIENT MATRIX FOR FLOW/
     & 17H INPUT TO SOLVER//)
 1720 format (//1H1, 44H GLOBAL COEFFICIENT MATRIX FOR CONCENTRATION/
     & 17H  INPUT TO SOLVER//)
 1725 format (/11X, 29HVECTOR OF BOUNDARY CONDITIONS/11X, 29(1H-))
 1730 format (1H0, 10X, 20HP COEFFICIENT MATRIX/11X, 20(1H-)//)
 1800 format (/7H IER = , I5, 5X, 5H STOP/)
 1810 format (/7H IEX = , I5, 5X, 5H STOP/)
 1820 format (////11X, 38HSOLUTION OF FLOW EQUATION AT ITERATION,
     & I5, 5X, 7H(ISK = , I5, 1H)/11X, 43(1H-))
 1870 format (//11X,
     & 44HCURRENT BOUNDARY CONDITIONS AT NEUMANN NODES/11X,
     & 44(1H-)/11X, 4HNODE, 3X, 12HTYPE OF B.C., 8X, 7HEAD IN,
     & 7X, 8HHEAD OUT, 8X, 7HFLUX IN, 7X, 8HFLUX OUT, 7X,
     & 8HFRACTION)
 1875 format (11X, I4, I15, E15.5, 30X, E15.5)
 1876 format (11X, I4, I15, 15X, 2E15.5, 15X, F15.2)
 1877 format (//11X,
     & 45HMODIFIED BOUNDARY CONDITIONS AT NEUMANN NODES/11X,
     & 45(1H-)/11X, 4HNODE, 3X, 12HTYPE OF B.C., 8X, 7HHEAD IN,
     & 23X, 7HFLUX IN, 22X, 8HFRACTION)
 1878 format (11X, I4, I15, E15.5)
 1879 format (11X, I4, I15, 30X, E15.5, 15X, F15.2)
 1880 format (//11X, 20HUNSATURATED ELEMENTS/)
 1890 format ((11X, 20I5))
 1900 format (//11X, 14HESTIMATED HEAD/11X, 14(1H-))
 1910 format (//11X,
     & 44HCURRENT BOUNDARY CONDITIONS ON SEEPAGE FACES/11X,
     & 44(1H-)/11X, 4HNODE, 3X, 12HTYPE OF B.C., 8X, 7HHEAD IN,
     & 7X, 8HHEAD OUT, 8X, 7HFLUX IN, 7X, 8HFLUX OUT, 5X,
     & 12HSEEPAGE FACE)
 1920 format (11X, I4, I15, 15X, 2E15.5, I32)
 1930 format (11X, I4, I15, E15.5, 30X, E15.5, I17)
 1940 format (//11X,
     & 45HMODIFIED BOUNDARY CONDITIONS ON SEEPAGE FACES/11X,
     & 45(1H-)/11X, 4HNODE, 3X, 12HTYPE OF B.C., 8X, 7HEAD IN,
     & 23X, 7HFLUX IN, 20X, 12HSEEPAGE FACE)
 1950 format (11X, I4, I15, E15.5, I62)
 1960 format (11X, I4, I15, 30X, E15.5, 15X, I17)
 1980 format (E10.4)

      Return
      End

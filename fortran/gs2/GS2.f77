program GS2(INPUT, OUTPUT, TAPE7, TAPE8, TAPE2, TAPE4,
     & TAPE11, TAPE13, TAPE5=INPUT, TAPE6 = OUTPUT)
C
C     GS2 is a two dimensional finite element mass-transport and
C     groundwater model. Statements that are not ANSI standard
C     Fortran are preceeded by C** comment statements.
C     These statements may require modification for other systems
C
      real LAMBDA, KD
      character RDATE*10, RTIME*10
      character DATE*10, TIME*10

C
C     The following variable should be dimensioned (MAXBW2, MAXS)
C     where MAXBW2 is the maximum half bandwidth of the mass transport
C     equations and is equal to (2 * MAXBW-1).
C     MAXS is the maximum of MAXM1 and MAXM2
C
      Level 2, /BLK1/
      Common /BLK1/ S(7, 52)

C     The following variable should be dimensioned (MAXS, MAXBW)
C     Where MAXBW is the maximum half bandwidth for the flow
C     equations. See the following data statements. MAXS is
C     the maximum of MAXM1 and MAXM2.
C
      Level 2, /BLK1A/
      Common /BLK1A/ P(52, 4)

C     The following variable should be dimensioned MX where
C     MX = MAXBW2 * MAXS
C
      Level 2, /BLK1B/
      Common /BLK1B/ W(364)

C     The following variables must be dimensioned to MAXNN.
C     MAXNN must be greater than or equal to the number of nodes
C     (NN) for the problem being considered.
C
      Level 2, /BLK2/
      Common /BLK2/ CFQ(52), CONC(52), CONCI(52), FQ(52),
     & KLC(52), KLR(52), LC(52), LR(52),
     & PHI(52), PHII(52), X(52), Y(52)

C     The following variables must be dimensioned to MAXNE.
C     MAXNE must be greater than or equal to the number of
C     elements (NE) for the problem being considered.
C
      Level 2, /BLK3/
      Common /BLK3/ ALPHA(25), ELONG(25), ETRANS(25),
     & FMOBX(25), FMOBY(25), KD(25), KF(25),
     & LAMBDA(25), POR(25), RHO(25), TTA(25)

C     Dimensions of the following variables must be
C     (ME, MAXNE) and (2, MAXNE).
C
      Level 2, /BLK4/
      Common /BLK4/ IN(13, 25), IE(2,25)

      Common /ONE/ BWD, CNTR, CHNG, TDR, STAT, STATP, OLDT, DELT,
     & DPRDT, BETAP, CLOS1, DELP, DIFUSN, PCHNG, TYPE, VMAX,
     & STIME, SSEC, H1, H2, PL, COEFI, EI, ITER1, ITMAX, ITCHNG,
     & IGO, NE, NP, NK, NSEEP, INC, ME, KOD1, KOD2, KOD3, KOD4,
     & KOD7, KOD8, KOD9, KOD10, KOD11, KOD12

C     The maximum number of materials and unsaturated material
C     property interpolation points is controlled by the size
C     of the arrays in common block two which should be
C     dimensioned such that:
C       WK(12 * N-1), XK(N, M), XPSI(N, M), XM(N, M),
C       CKT(3, N-1, M), CTT(3, N-1, M), PSIO(M), ISPL(M)
C     Where,  M = maximum # of material types
C             N = maximum # of interpolation points provided
C                 in the unsaturated material property tables.
C
      Common /TWO/ WK(179), XK(15, 20), XPSI(15, 20), CKT(3, 14, 20),
     & CTT(3, 14, 20), PSIO(20), IPSL(20)
C
      Common /THREE/ NN, MM, KM, NB, KNB, KNB2, MB, MB2, KMB, KMB2,
     & NSDN, IT, ISTOP

C     Dimensions of variables in common block four should be
C     equal to MXC and MXT. These values are currently set to
C     MXC = 12 and MXT = 16. To save space, only in the case that
C     all quadrilateral elements are specified (input value of
C     INC = 4), both MXC and MXT can be set to 4 and all variables
C     in common block four changed to dimensions of 4.
C     Common block four appears in subroutines TS, COGEN, and MAIN

      Common /FOUR/ PE(12, 12), SE(12, 12), F(12, 16), DX(12, 16),
     & DY(12, 16), Q(16), DETJ(16), CPHI(16), VKX(16), VKY(16),
     & DPORDT(16), D0(16), DK(16), DH(16), SRCRT(16), FF(12),
     & DGX(12), DGY(12), SRCR(12)

C     The following variables are dimensioned to MAXM1 where,
C     MAXM1 = MAXNN - NS1.
C     MAXM1 is calculated by the program.
      Dimension EST(50), FM(50), FX(50), LP(50), OLD(50), RT(50),
     & U(50)

C     The following variables must be dimensioned MAXM2 where
C     MAXM2 = MAXNN - KNS1.
C     MAXM2 is calculated by the program.
      Dimension CFM(52), COLD(52), CRT(52), KLP(52)

C     The following variables must be dimensioned MAXM4.
C
      Dimension CN(1), COEF(1), NSF(1), NSK(1), VN(1)

C     The following variable must be dimensioned MAXEEP.
C
      Dimension MSP(1)

C     The following variable must be dimensioned (MAXM5, MAXEEP).
C
      Dimension NSP(1, 1)

      Data MXC    /12/
      Data MXT    /16/
      Data MAXNN  /52/
      Data MAXNE  /25/
      Data NS1    /2/
      Data KNS1   /0/
      Data MAXM4  /1/
      Data MAXM5  /1/
      Data MAXEEP /1/
      Data MAXBW  /4/
      Data MAXBW2 /7/
      MAXM1 = MAXNN - NS1
      MAXM2 = MAXNN - KNS1
      MAXS = MAX0(MAXM1, MAXM2)
      MX = MAXBW2 * MAXS
      ISTOP = 0
      MAXDIF = 0
C
C   Get Data and Time of run for run ID
C
      RDATE = DATE()
      RTIME = TIME()

C
C   Read and Write data and initialize
C

      Call DATAIN (PHI, PHII, OLD, CONC, CONCI, COLD, CN, VN,
     & COEF, U, EST, FQ, CFQ, X, Y, FMOBX, FMOBY, ELONG, ETRANS,
     & POR, ALPHA, TTA, KD, LAMBDA, RHO, IN, IE, KF, LR, KLR, LC,
     & KLC, LP, KLP, NSF, NSK, NSP, MSP, MAXNN, MAXNE, MAXM1,
     & MAXM2, MAXM4, MAXM5, MAXEEP, MAXBW, MAXDIF, RDATE, RTIME)

      IF (ISTOP.NE.0) GO TO 999
C
C   Transient Problem
C
      Call TS (S, P, W, FM, RT, PHI, PHII, OLD, CFM, CRT, CONC,
     & CONCI, COLD, FX, CN, VN, COEF, EST, U, FQ, CFQ, X, Y,
     & FMOBX, FMOBY, ELONG, ETRANS, POR, ALPHA, TTA, KD, LAMBDA,
     & RHO, IN, KF, LR, KLR, LC, KLC, LP, KLP, NSF, NSK, IE, NSP,
     & MSP, MAXNN, MAXNE, MAXM1, MAXM2, MAXM4, MAXM5, MAXEEP,
     & MAXBW, MAXBW2, MX, MAXS, MXC, MXT, RDATE, RTIME)

C
  999 STOP
      END
      Block Data
C
      Common /ONE/ BWD, CNTR, CHNG, TDR, STAT, STATP, OLDT, DELT,
     & DPRDT, BETAP, CLOS1, DELP, DIFUSN, PCHNG, TYPE1, VMAX,
     & STIME, SSEC, H1, H2, PL, COEFI, EI, ITER1, ITMAX,
     & ITCHNG, IGO, NE, NP, NK, NSEEP, INC, ME, KOD1, KOD2,
     & KOD3, KOD4, KOD7, KOD8, KOD9, KOD10, KOD11, KOD12

      Common /TWO/ WK(179), XK(15, 20), XM(15, 20), XPSI(15, 20),
     & CKT(3, 14, 20), CTT(3, 14, 20), PSIO(20), ISPL(20)
C
      Data BWD/4HBACK/, CNTR/4HCENT/, H1/0.347854/, H2/0.652145/,
     & ME/13/
C
      END

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

      subroutine DATAIN (PHI, PHII, OLD, CONC, CONCI, COLD, CN, VN,
     & COEF, U, EST, FQ, CFQ, X, Y, FMOBX, FMOBY, ELONG, ETRANS,
     & POR, ALPHA, TTA, KD, LAMBDA, RHO, IN, IE, KF, LR, KLR, LC,
     & KLC, LP, KLP, NSF, NSK, NSP, MSP, MAXNN, MAXNE, MAXM1,
     & MAXM2, MAXM4, MAXM5, MAXEEP, MAXBW, MAXDIF, RDATE, RTIME)
C
C     Purpose: To read and write data and to initialize
C
      real LAMBDA, KD
      character RDATE*10
      character RTIME*10
C
      LEVEL 2, CFQ, CONC, CONCI, FQ, KLC, KLR, LC, LR, PHI, PHII,
     & X, Y, ALPHA, ELONG, ETRANS, FMOBX, FMOBY, KD, KF, LAMBDA,
     & POR, RHO, TTA, IN, IE

      common /ONE/ BWD, CNTR, CHNG, TDR, STAT, STATP, OLDT, DELT,
     & DPRDT, BETAP, CLOS1, DELP, DIFUSN, PCHNG, TYPE, VMAX, STIME,
     & SSEC, H1, H2, PL, COEFI, EI, ITER1, ITMAX, ITCHNG, IGO, NE,
     & NP, NK, NSEEP, INC, ME, KOD1, KOD2, KOD3, KOD4, KOD7, KOD8,
     & KOD9, KOD10, KOD11, KOD12

      common /TWO/ WK(179), XK(15, 20), XM(15, 20), XPSI(15, 20),
     & CKT(3, 14, 20), CTT(3, 14, 20), PSIO(20), ISPL(20)

      common /THREE/ NN, MM, KM, NB, KNB, KNB2, MB, MB2, KMB, KMB2,
     & NSDN, IT, ISTOP

      dimension PHI(MAXNN), PHII(MAXNN), OLD(MAXM1), EST(MAXM1),
     & U(MAXM1), CONC(MAXNN), CONCI(MAXNN), COLD(MAXM2), CN(MAXM4),
     & VN(MAXM4), FQ(MAXNN), CFQ(MAXNN), X(MAXNN), Y(MAXNN),
     & COEF(MAXM4), FMOBX(MAXNE), FMOBY(MAXNE), POR(MAXNE),
     & ELONG(MAXNE), ETRANS(MAXNE), ALPHA(MAXNE), TTA(MAXNE),
     & KD(MAXNE), LAMBDA(MAXNE), RHO(MAXNE), IN(ME, MAXNE),
     & IE(2, MAXNE), KF(MAXNE), LR(MAXNN), KLR(MAXNN), LC(MAXNN),
     & KLC(MAXNN), LP(MAXM1), KLP(MAXM2), TITLE(20), NSF(MAXM4),
     & NSK(MAXM4), NSP(MAXM5, MAXEEP), MSP(MAXEEP)

      dimension WXPSI(20), WXM(20), WXK(20), CC(3, 19)

      read (5, 840) TITLE
      write (6, 850)
      write (6, 860) TITLE
      write (6, 1051) RDATE, RTIME

      read (5, 910) NN, NE, NS, KNS, NB, KNB, NF, INC, NK, NSEEP
      read (5, 915) NSDN, MQ4, KNSDN, PL, COEFI, EI, NVS
          if (NN.LE.MAXNN) go to 12
               ISTOP = ISTOP + 1
      write (6, 2000) NN, MAXNN
          go to 520
   12     if (NE.LE.MAXNE) go to 16
               ISTOP = ISTOP + 1
      write ( 6, 2001) NE, MAXNE
          go to 520
   16 continue

      write (6, 920)
      write (6, 930) NN, NE, NS, KNS, NF, NB, KNB, INC
      write (6, 935) NSDN, MQ4, PL, EI, KNSDN, NSEEP

      read (5, 940) DELT, CHNG, ITMAX, ITCHNG, PCHNG, BETAP, TYPE,
     & DIFUSN, DPRDT, STAT, STATP, CLOS1, ITER1, IGO

      write (6, 950) DELT, CHNG, ITMAX, ITCHNG, PCHNG, BETAP,
     & DIFUSN, DPRDT, ITER1, CLOS1, IGO

      if (TYPE.EQ.BWD) write (6, 960)
      if (TYPE.EQ.CNTR) write (6, 970)
      if (STAT.EQ.0.) write (6, 944)
      if (STATP.EQ.0.) write (6, 943)
      TDR = 2.0
          if (TYPE.EQ.BWD) TDR = 1.0
C
      read (5, 1440) AFMOBX, AFMOBY, APOR, AELONG, AETRAN, APHII,
     & ACONCI, XFACT, YFACT, ATETA, AAL, AKD, ALAM, ARHO

      write (6, 1450) AFMOBX, AFMOBY, APOR, AELONG, AETRAN, APHII,
     & ACONCI, XFACT, YFACT, ATETA, AAL, AKD, ALAM, ARHO

      read (5, 910) KOD1, KOD2, KOD3, KOD4, KOD7, KOD8, KOD9,
     & KOD10, KOD11, KOD12

      write (6, 911) KOD1, KOD2, KOD3, KOD4, KOD7, KOD8, KOD9,
     & KOD10, KOD11, KOD12

C
C     Node coordinates

      read (5, 980) (J, X(J), Y(J), K = 1, NN)
          do 19 J = 1, NN
      X(J) = X(J) * XFACT
   19 Y(J) = Y(J) * YFACT
      write (6, 990)
      write (6, 1000)
      write (6, 1010) (J, X(J), Y(J), J = 1, NN)

C
C     Source and sink nodes, FQ.

      write (6, 1020)
          do 20 I = 1, NN
   20 FQ(I) = 0.
          if (NF.EQ.0) go to 40
      read (5, 1056) (LR(I), PHI(I) , I = 1, NF)
      write (6, 1090) (LR(I), PHI(I) , I = 1, NF)
   40 continue
          do 31 I = 1, NF
      II = LR(I)
   31 FQ(II) = PHI(I)

C
C     Source and sink concentrations, CFQ
      write (6, 1040)
          do 50 I = 1, NN
   50 CFQ(I) = 0.
          if (NF.EQ.0) go to 70
      read (5, 1056) (KLR(I), PHI(I) , I = 1, NF)
      write (6, 1090) (KLR(I), PHI(I) , I = 1, NF)
          do 51 I = 1, NF
      II = LR(I)
   51 CFQ(II) = PHI(I)
   70 continue

C
C     Read initial pressure and concentration values

      read (5, 1053) STIME
      read (5, 1056) (I, CONCI(I), K = 1, NN)
      read (5, 1052) STIME

C****************************************************************
C     Do loop for generating initial pressure from hone read
C     in from data, instead of reading in initial pressure.
C     Hone, a new datum, is read in and used in the calculation.
C
C     read (5, 1056) (I, PHII(I), K = 1, NN)
      read (5, 1057) HONE
 1057 format (E10.3)
      write (6, 2010) HONE
 2010 format ('             HONE = ', 1PG15.6)
      if (HONE.NE.9999.) then
          do 75 I = 1, NN
            PHII(I) = HONE - Y(I)
   75     continue
          read (5, 1056) (I, PHII(I), K = 1, NS)
      else
          read (5, 1056) (I, PHII(I), K = 1, NN)
      endif
C
C****************************************************************
            if (STIME.GT.0.) go to 80
            do 1470 I = 1, NN
      CONCI(I) = CONCI(I) * ACONCI
C     if (HONE.NE.9999.) then
          PHII(I) = PHII(I) APHII
C     endif
 1470 continue
   80 write (6, 1070)
   90 write (6, 1080)
      write (6, 1090) (I, PHII(I), I = 1, NN)
      write (6, 1100)
      write (6, 1090) (I, CONCI(I), I = 1, NN)

C
C     Element incidences
          do 100 I = 1, ME
          do 100 L = 1, NE
  100 IN(I, L) = 0
          do 200 L = 1, NE
      read (5, 910) L, (IN(I, L), I = 1, INC)
      M = 4
          if (INC.EQ.4) go to 195

C
C     Count active nodes and compute max nodal difference
          do 190 I = 5, 12
          if (IN(I, L).EQ.0) go to 190
      M = M + 1
  190 continue
  195 IN(ME, L) = M
  200 continue

      write (6, 1140)
      write (6, 1150)
          do 210 L = 1, NE
      M = IN(ME, L)
      M1 = M - 1
      MND = 0
          do 205 I = 1, M1
          if (IN(I, L).EQ.0) go to 205
      IP = I + 1
          do 202 J = IP, M
          if (IN(J, L).EQ.0) go to 202
      ND = IABS(IN(I, L) - IN(J, L))
      MND = MAX0(ND, MAXDIF)
  202 continue
  205 continue
      write (6, 1160) L, MND, (IN(I, L), I = 1, INC)
  210 continue
      ND = MAXDIF + 1
          if (ND - NB) 211, 212, 213
  211 NB = ND
          go to 212
  213 write (6, 2003) ND, NB
  212     if (ND - KNB) 214, 217, 216
  214 KNB = ND
          go to 217
  216 write (6, 2004) ND, KNB
  217     if (NB.LE.MAXBW.AND.KNB.LE.MAXBW) go to 218
          write (6, 2002) NB, KNB, MAXBW
              ISTOP = ISTOP + 1
  218 KNB2 = 2 * KNB - 1
      MB = 0
      MB2 = 0
      KMB = 0
      KMB2 = 0
      write (6, 1215) NB, KNB

C
C     Read element parameters

      write (6, 1170)
  219 read (5, 910) I1, I2, ITYPE
            if (I1.LE.0) go to 222
      read (5, 1180) TX, TY, DSL, DST, HETA, TETA, AL, DIST,
     & DECAY, DENS

C
      DECAY = DECAY / (365.25 * 24. * 3600.)
C     Convert decay to second - 1
C

      do 220 L = I1, I2
      FMOBX(L) = TX * AFMOBX
      FMOBY(L) = TY * AFMOBY
      ELONG(L) = DSL * AELONG
      ETRANS(L) = DST * AETRAN
      POR(L) = HETA * APOR
      TTA(L) = TETA * ATETA
      ALPHA(L) = AL * AAL
      KD(L) = AKD * DIST
      LAMBDA(L) = ALAM * DECAY
      RHO(L) = ARHO * DENS
      IE(2, L) = ITYPE
  220 continue
          go to 219
  222     do 225 N = 1, NK
      K = 0
          do 224 L = 1, NE
          if (IE(2, L).NE.N) go to 224
      K = K + 1
      LR(K) = L
      LL = LR(K)

C     Use following If statement to check and write out L, IE, LR, K.
C     if (L.GT.50) go to 224
C     write (6, 1596) L, IE(2, L), LR(K), K

  224 continue
      write (6, 1191)
      write (6, 1190) FMOBX(LL), FMOBY(LL), POR(LL), ELONG(LL),
     & ETRANS(LL), TTA(LL), ALPHA(LL)
      write (6, 1192)
      write (6, 1193) KD(LL), LAMBDA(LL), RHO(LL)
      write (6, 1194)
      write (6, 1195) (LR(L), L = 1, K)
  225 continue
C
C     Dirichlet boundary nodes for following
      write (6, 1230)
          do 270 I = 1, NN
      KLR(I) = 0
  270 LR(I) = 0
          if (NS.EQ.0) go to 310
      call BC (LR, NS, 1, NN, MAXNN, ISTOP)

C
C     Dirichlet nodes for concentration
  310 write (6, 1280)
          if (KNS.EQ.0) go to 325
      call BC (KLR, KNS, 1, NN, MAXNN, ISTOP)
  325 continue

C
C
C     LC and KLC
      LC(1) = MAX0(LR(1), 0)
      KLC(1) = MAX0(KLR(1), 0)
          do 347 I = 2, NN
      LC(I) = LC(I-1) + MAX0(LR(I), 0)
  347 KLC(I) = KLC(I-1) + MAX0(KLR(I), 0)
      MM = NN - LC(NN)
      KM = NN - KLC(NN)
      write (6, 1290) MM, KM
          if (MM.EQ.(NN - NS)) go to 350
      write (6, 2006)
      write (6, 2008) (I, LC(I), I = 1, NN)
              ISTOP = ISTOP + 1
          go to 520
  350     if (KM.EQ.(NN - KNS)) go to 360
      write (6, 2007)
      write (6, 2008) (I, KLC(I), I = 1, NN)
              ISTOP = ISTOP + 1
          go to 520
  360 continue

C
C     Neumann boundary nodes for flow
      write (6, 1250)
          if (NSDN.EQ.0) go to 340
      MP4 = NSDN - MQ4
          if (MP4.EQ.0) go to 335
      call BC (LR, MP4, 4, NN, MAXNN, ISTOP)
          if (MQ4.EQ.0) go to 336
  335 call BC (LR, MQ4, -4, NN, MAXNN, ISTOP)
  336 read (5, 1030) (NSF(K), COEF(K), K = 1, NSDN)
      read (5, 1030) (NSF(K), VN(K), K = 1, NSDN)
      write (6, 1059)
      write (6, 1090) (NSF(K), COEF(K), K = 1, NSDN)
      write (6, 1580)
      write (6, 1080)
      write (6, 1090) (NSF(K), VN(K), K = 1, NSDN)
          do 337 K = 1, NSDN
      I = NSF(K)
          if (LR(I).NE.-4) go to 337
      FQ(I) = EI * VN(K) * COEF(K)
  337 continue

C
C     Neumann boundary nodes for concentration
  340 write (6, 1270)
          if (KNSDN.EQ.0) go to 345
      call BC (KLR, KNSDN, -4, NN, MAXNN, ISTOP)
      read (5, 1030) (NSK(K), CN(K), K = 1, KNSDN)
  345 continue

C
C     Seepage
          if (NSEEP.EQ.0) go to 380
          do 370 K - 1, NSEEP
      read (5, 910) MSP(K), MP2
      write (6, 1590) K
      MSPK = MSP(K)
      MQ2 = MSP(K) - MP2
          if (MP2.EQ.0) go to 365
      read (5, 1595) (NSP(J, K), J = 1, MP2)
      write (6, 1596) (NSP(J, K), J = 1, MP2)
          do 357 J = 1, MP2
      I = NSP(J, K)
  357 LR(I) = 2
          if (MQ2.EQ.0) go to 370
  365 JJ = MP2 + 1
      read (5, 1595) (NSP(J, K), J = JJ, MSPK)
      write (6, 1596) (NSP(J, K), J = JJ, MSPK)
          do 367 J = JJ, MSPK
      I = NSP(J, K)
  367 LR(I) = -2
  370 continue
  380 continue

C
C     Initialization
C
C     Constants
      IT = 0
      DELP = 1.0E+10
      DELT = DELT * 3600.
      SSEC = STIME * 3600.

C
C     OLD, COLD
      IC = 0
      IP = 0
          do 470 I = 1, NN
      PHI(I) = PHII(I)
      CONC(I) = CONCI(I)
          if (LR(I).EQ.1) go to 468
      IP = IP + 1
      LP(IP) = I
      U(IP) = PHII(I)
      OLD(IP) = PHII(I)
      EST(IP) = PHII(I)
  468     if (KLR(I).EQ.1) go to 470
      IC = IC + 1
      KLP(IC) = I
      COLD(IC) = CONCI(I)
  470 continue
          do 474 L = 1, NE
  474 KF(L) = 0
          if (NVS.EQ.0) go to 476
      read (5, 910) (L, KF(L), LL = 1, NVS)
  476 continue

C
C     Hysteresis constants
      read (5, 910) (ISPL(K), K = 1, NK)
          if (ISPL(1).EQ.0) go to 520
      K = 0
  480 K = K + 1
      write (6, 1570) K
      ISPK = ISPL(K)
      ISPM = ISPK - 1
      read (5, 1460) (XPSI(I, K), I = 1, ISPK)
      read (5, 1460) (XM(I, K), I = 1, ISPK)
      read (5, 1460) (XK(I, K), I = 1, ISPK)
      write (6, 1465) (XM(I, K), I = 1, ISPK)
      write (6, 1465) (XPSI(I, K), I = 1, ISPK)
      PSIO(K) = -10.**XPSI(ISPK, K)
          do 490 I = 1, ISPK
      WXK(I) = XK(I, K)
      WXM(I) = XM(I, K)
  490 WXPSI(I) = XPSI(I, K)
      IER = 0
      call ICS1CU (WXM, WXPSI, ISPK, CC, WK, IER, ISPK - 1,
     & 2 * ISPK - 2)
      if (IER.GT.128) go to 512
          do 500 I = 1, ISPM
          do 500 J = 1, 3
  500 CTT(J, I, K) = CC(J, I)
      write (6, 1465) (XK(I, K), I = 1, ISPK)
      IER = 0
      call ICS1CU (WXK, WXPSI, ISPK, CC, WK, IER, ISPK - 1,
     & 2 * ISPK - 2)
          do 510 I = 1, ISPM
          do 510 J = 1, 3
  510 CKT(J, I, K) = CC(J, I)
          if (IER.EQ.0) go to 515
  512 write (6, 2005) IER
              ISTOP = ISTOP + 1
          go to 520
  515     if (NK.LT.K) go to 480

C
  520 continue
      close (5)
      return
C
  840 format(20A4)
  850 format(1H1, 28X, 21H GROUNDWATER FLOW AND,
     & 15H MASS TRANSPORT//31X,
     & 31HWITH ISOPARAMETRIC 2-D ELEMENTS//)
  860 format (11X, 70(1H*)//11X, 20A4//11X, 70(1H*)///)
  910 format (1615)
  911 format (//11X, 4HKOD1, I5/11X, 4HKOD2, I5/11X, 4HKOD3,
     & I5/11X, 4HKOD4, I5/11X, 4HKOD7, I5/11X, 4HKOD8, I5/11X,
     & 4HKOD9, I5/11X, 5HKOD10, I4/11X, 5HKOD11, I4/11X,
     & 5HKOD12, I4)
  915 format (3I5, 3E10.3, I5)
  920 format (///11X, 19HFINITE ELEMENT DATA/11X, 19(1H-)/)
  930 format (1H, 10X, 11HNUMBER OF -, 2X, 5HNODES, I24/21X, 1H-,
     & 2X, 8HELEMENTS, I21/21X, 1H-, 2X,
     & 24HCONSTANT FLOW BOUNDARIES, I5/21X, 1H-, 2X,
     & 25HCONSTANT CONC. BOUNDARIES, I4/21X, 1H-, 2X,
     & 20HSOURCE OR SINK NODES, I9/11X,
     & 37HESTIMATED HALF-BANDWIDTH FOR PRESSURE, I10/36X,
     & 17HFOR CONCENTRATION, I5/11X,
     & 31HMAXIMUM NUMBER OF ELEMENT NODES, I16)
  935 format (////11X,
     & 44HNUMBER OF BOUNDARY NODES WITH SPECIFIED FLUX, I5/11X,
     & 13HINITIAL VALUE, I36/15X, 24HMINIMUM ALLOWED PRESSURE,
     & E21.3/15X, 12HMAXIMUM FLUX, E33.3/11X,
     & 58HNUMBER OF BOUNDARY NODES WITH SPECIFIED CONCENTRATION FLUX,
     & I5/11X, 23HNUMBER OF SEEPAGE FACES, I26)
  940 format (2E10.3, 2I10, 2E10.3, A4/5E10.3, 2I10)
  943 format (/11X, 26HSTEADY-STATE FLOW EQUATION)
  944 format (/11X, 36HSTEADY-STATE MASS TRANSPORT EQUATION)
  950 format (/////11X, 15HTIME PARAMETERS/11X, I5(1H-)//11X,
     & 26HINITIAL TIME STEP IN HOURS, F30.6/11X,
     & 35HMULTIPLIER FOR INCREASING TIME STEP, F21.3/11X,
     & 38HMAXIMUM PERMITTED NUMBER OF TIME STEPS, I18/11X,
     & 44HNUMBER OF TIME STEPS BETWEEN CHANGES IN DELT, I12/11X,
     & 25HPRESSURE CHANGE CRITERION, E31.3/11X,
     & 29HFLUID COMPRESSIBILITY (BETAP), E27.3/11X,
     & 28HMOLECULAR DIFFUSION CONSTANT, E28.3/11X,
     & 38HFACTOR FOR TIME DERIVATIVE OF POROSITY, F18.1/11X,
     & 28HNUMBER OF ITERATIONS IN FLOW, I28/11X,
     & 17HCLOSURE CRITERION, E39.5/11X,
     & 44HNUMBER OF CONCENTRATION STEPS PER PRES. STEP, I12)
  960 format (///11X, 24HIMPLICIT TIME DERIVATIVE///)
  970 format (///11X, 24HCENTERED TIME DERIVATIVE///)
  980 format (I5, 2E10.3)
  990 format (////11X, 16HNODE COORDINATES/11X, 16(1H-)/)
 1000 format (2(1H, 11X, 4HNODE, 15X, 1HX, 15X, 1HY)/)
 1010 format (2(1H , 11X, I4, 2F16.4))
 1020 format (/////11X, 25HSOURCE AND SINK DISCHARGE/11X,
     & 25(1H-)/11X, 6(4HNODES, 5X, 5HVALUE, 5X))
 1030 format (5(I5, E10.4))
 1040 format (/////11X, 30HSOURCE AND SINK CONCENTRATIONS/11X,
     & 30(1H-)/11X, 6(4HNODE, 5X, 5HVALUE, 5X))
 1051 format (/11X, 'RUN IDENTIFICATION: ', A10, 4X, A10/)
 1052 format (17X, G15.7)
 1053 format (22X, G15.7)
 1056 format (4(I5, E15, 8))
 1058 format (////11X, 28HSPECIFIED CONCENTRATION FLUX/11X,
     & 28(1H-)//11X, 6(4HNODE, 5X, 5HVALUE, 5X))
 1059 format (////11X, 33HSPECIFIED FRACTION OF NORMAL FLUX/11X,
     & 33(1H-)/11X, 6(4HNODE, 5X, 5HVALUE, 5X))
 1070 format (/////11X, 12HINITIAL HEAD/11X, 12(1H-))
 1080 format (/11X, 6(4HNODE, 5X, 5HVALUE, 5X))
 1090 format (/(11X, 6(I4, 2X, 1PE10.3, 3X)))
 1100 format (////11X, 21HINITIAL CONCENTRATION/11X, 21(1H-)//11X,
     & 6(4HNODE, 5X, 5HVALUE, 5X))
 1140 format (////11X, 18HELEMENT INCIDENCES/11X, 18(1H-)/)
 1150 format (11X, 7HELEMENT, 2X, 13HMAXIMUM NODAL, 2X, 1H/ ,
     & 14(1H-), 10HINCIDENCES, 14(1H-), 1H/ , /20X, 10HDIFFERENCE)
 1160 format (11X, I7, 2X, I13, 2X, 12I5)
 1170 format (/////11X, 18HELEMENT PARAMETERS/11X, 18(1H-))
 1180 format (8E10.5)
 1190 format (11X, E11.3, E12.3, E14.3, E18.3, E12.3, E13.3, E20.3)
 1191 format (//58X, 12HDISPERSIVITY/10X, 11H X-MOBILITY, 1X,
     & 11H Y-MOBILITY, 5X, 9H POROSITY, 5X, 13H LONGITUDINAL, 1X,
     & 11H TRANSVERSE, 5X, 8HMOISTURE, 5X, 15HCOMPRESSIBILITY)
 1192 format (/11X, 14HDISTRIBUTIVITY, 5X, 14HDECAY CONSTANT, 5X,
     & 7HDENSITY/)
 1193 format (11X, E14.3, E19.3, E12.3)
 1194 format (/11X, 19HVALID FOR ELEMENTS:)
 1195 format (11X, 22I5)
 1215 format (////11X, 27HHALF-BANDWIDTH FOR PRESSURE, I15/11X,
     & 32HHALF-BANDWIDTH FOR CONCENTRATION, I10)
 1230 format (/////11X, 33HDIRICHLET BOUNDARY NODES FOR FLOW/11X,
     & 33(1H-))
 1250 format (/////11X, 31HNEUMANN BOUNDARY NODES FOR FLOW/11X,
     & 31(1H-))
 1270 format (/////11X,
     & 40HNEUMANN BOUNDARY NODES FOR CONCENTRATION/11X, 40(1H-))
 1280 format (/////11X,
     & 42HDIRICHLET BOUNDARY NODES FOR CONCENTRATION/11X, 42(1H-))
 1290 format (////11X, 28HNUMBER OF EQUATIONS FOR FLOW, I5/11X,
     & 38HNUMBER OF EQUATIONS FOR MASS TRANSPORT, I5)
 1460 format (8E10.3)
 1465 format (11X, 8D12.4)
 1440 format (8F10.4)
 1450 format (1H0, ///11X, 21HPARAMETER MULTIPLIERS /11X,
     & 21(1H-)/11X, 6HAFMOBX, E15.5/11X, 6HAFMOBY, E15.5/11X,
     & 4HAPOR, E17.5/11X, 6HAELONG, E15.5/11X, 6HAETRAN,
     & E15.5/11X, 5HAPHII, E16.5/11X, 6HACONCI, E15.5/11X,
     & 5HXFACT, E16.5/11X, 5HYFACT, E16.5/11X, 5HATETA, E16.5/11X,
     & 3HAAL, E18.5/11X, 3HAKD, E18.5/11X, 4HALAM, E17.5/11X,
     & 4HARHO, E17.5)
 1570 format (//11X,
     & 46HVARIATION OF MATERIAL PROPERTIES WITH PRESSURE,
     & 2h (,I2, 1H)/11X, 46(1H-))
 1580 format (/11X, 25HDEPENDENT BOUNDARY LENGTH/11X, 25(1H-))
 1590 format (//11X, 21HNODES ON SEEPAGE FACE, I3/11X, 21(1H-))
 1595 format (20I4)
 1596 format (11X, 20I5)
 2000 format (//29H ERROR, TOO MANY NODAL POINTS/ 6H NN = ,
     & I5, 11H MAXIMUM = , I5)
 2001 format (//25H ERROR, TOO MANY ELEMENTS/ 6H NE = , I5,
     & 11H MAXIMUM = , I5)
 2002 format (////11X, 29HERROR: MAXIMUM HALF-BANDWIDTH,
     & 23H EXCEEDS SPACE PROVIDED/11X, 5HNB = , I3, 10X, 6HKNB = ,
     & I3, 9H MAXBW = , I3)
 2003 format (////11X, 31HWARNING: MAXIMUM HALF-BANDWIDTH,
     & 22H GREATER THAN ESTIMATE/11X, 5HND = , I3, 10X, 6HKNB = ,
     & I3)
 2004 format (////11X, 31HWARNING: MAXIMUM HALF-BANDWIDTH,
     & 22H GREATER THAN ESTIMATE/11X, 5HND = , I3, 10X, 6HKNB = ,
     & I3)
 2005 format (//11X, 38HERROR IN HYSTERESIS INPUT DATA. IER = , I3)
 2006 format (//11X, 34HWRONG NUMBER OF EQUATIONS FOR FLOW)
 2007 format (//11X,
     & 44HWRONG NUMBER OF EQUATIONS FOR MASS TRANSPORT)
 2008 format (/(11X, 9(I4, 1X, I4, 4X)))
      end


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
     1    7HELEMENT, I5, 2X, 8HREQUIRES, I5, 2X, 10HINSTEAD OF, I5)
 2000 format (////11X, 22HINSUFFICIENT BANDWIDTH/11X,
     1    7HELEMENT, I5, 2X, 8HREQUIRES, I5, 2X, 1OHINSTEAD OF, I5)
  999 return                        
      end
      

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


      subroutine sos(A, NEQ, IBAND, ISTART, NDIM, MDIM)
      
      level 2, A
      dimension A(NDIM, MDIM)

      NB2 = (IBAND+9) / 10 * 10 - 9
          do 20 K1=1, NB2, 10
      K2 = K1 + 9
          if (K1.EQ.NB2) K2=IBAND
          do 10 I=ISTART, NEQ
            
   10 write (6, 90) I, (A(I, J), J=K1, K2)
   20 write (6, 50)        

   90 format (I5, 10E12.4)
   50 format (//)
      return
      end
      

      SUBROUTINE DBAND(S, N, NB, NDIM, MDIM, IEX)
C
C To triangularize a symmetric coefficent matrix
C**
      LEVEL 2, S
      DIMENSION S(NDIM, MDIM)

      IEX = 0
            DO 50 I=1, N
      IP = N - I + 1
            IF (NB.LT.IP) IP=NB
            DO 50 J=1,IP
      IQ = NB - J
            IF ((I-J).LT.IQ) IQ=I-1
      SUM = S(I,J)
            IF (IQ.LT.1) GO TO 20
            DO 10 K=1,IQ
      II = I - K
      SIIK = S(II,K+1)
      JZ = J + K
      SIIJZ = S(II,JZ)
            IF (SIIJZ.EQ.0.) GO TO 10
      SUM = SUM - SIIK*SIIJZ
10    CONTINUE
20          IF (J.NE.1) GO TO 40
            IF (SUM.LE.0) GO TO 30
      TEMP = 1./SQRT(SUM)
      S(I,J) = TEMP
            GO TO 50
30    WRITE (6,60) I
      WRITE (6,70) N,NB,IP,IQ,I,J,SUM
      IEX = 1
      RETURN
40    S(I,J) = SUM*TEMP
50    CONTINUE

60    FORMAT (1H1,OX,18HDBAND FAILS AT ROW, I4)
70    FORMAT (1H0,6I5,E20.8)
      RETURN
      END
      

       subroutine BC(LX, LN, KBC, NEQ, NDIM, ISTOP)
C
C       Purpose: To Identify the boundary nodes
C
       level 2, LX
       dimension LX(NDIM), LRT(20)
C      
       NST = 0
   80      if (NST.GE.LN) go to 150
       read (5, 40) (LRT(ITT), ITT=1,20)
       IA = I
       J = LRT(I)
       NST = NST + 1
           if (J.LE.NEQ) go to 90
       write (6, 50) J
           go to 10
   90  LX(J) = KBC
  100  write (6, 60) (LRT(ITT), ITT=1, IA)
           if (IA.EQ.20) go to 80
  150      if (NST.EQ.LN) go to 10                        
       write (6, 70) NST, LN
                 ISTOP = ISTOP + 1
   40  format (20I4)
   50  format (11X, 10(1H*), 14H BOUNDARY NODE, I4, 15H, DOES NOT EXIST)
   60  format (1H0, 5(1H*), 30H NUMBER OF BOUNDARY NODES READ, I10,
      1    34H DISAGREES WITH NUMBER ANTICIPATED, I10)
   10  return   
       end
       
       
       SUBROUTINE SBAND (S,P,U,NB,NDIM,MDIM,NU)
C
C	TO SOLVE FLOW EQUATION
C
C**
      LEVEL 2, S
      DIMENSION S(NDIM,MDIM),P(NU),U(NU)
		   
	    DO 30 I=1,N
      J = I - NB + 1
	    IF ((I+1).LE.NB) J=1
      SUM = P(I)
      K1 = I - 1
          IF (J.GT.K1) GO TO 20
	    DO 10 K=J,K1
      II = I - K + 1
      SKI = S(K,II)
10    IF(SKI.NE.0.) SUM=SUM-SKI*U(k)
20    U(I) = SUM*S(I, 1)
30    CONTINUE
	    DO 60 I1=1,N
      I = N - I1 + 1
      J = I + NB - 1
	    IF (J.GT.N) J=N
      SUM = U(I)
      K2 = I + 1
	    IF (K2.GT.J) GO TO 50
	    DO 40 K=K2,J
      KK = K - I + 1
      SKI = S(I,KK)
40        IF (SKI.NE.0.) SUM=SUM-SKI*U(K)
50    U(I) = SUM*S(I,1)
60    CONTINUE
      RETURN
      END
      
      
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


     SUBROUTINE PO (CONC,PHI,NN,IT,STIME,NDIM,RDATE,RTIME)
C
C    PURPOSE: TO GENERATE RESTART DATA FILES
C	          PRESSURES ARE WRITTEN TO TAPE7
C	          CONCENTRATIONS ARE WRITTEN TO TAPE8
C**
     LEVEL 2, CONC,PHI
     DIMENSION CONC(NDIM),PHI(NDIM)
     CHARACTER RDATE*10
     CHARACTER RTIME*10
C
     WRITE (8,1051) RDATE,RTIME
     WRITE (8,1053) IT
     WRITE (8,1054) STIME
     WRITE (8,1056) (I,CONC(I),I=1,NN)
     WRITE (7,1051) RDATE,RTIME
     WRITE (7,1052) IT
     WRITE (7,1055) STIME
     WRITE (7,1056) (I,PHI(I),I=1,NN)
1051 FORMAT ('1 RUN IDENTIFICATION: ',A10,1X,A10)
1052 FORMAT (' PRESSURE HEAD OUTPUT AT TIME STEP:',I4)
1053 FORMAT (' CONCENTRATION OUTPUT AT TIME STEP:',I4)
1054 FORMAT (' TIME (HOURS):	',G15.5)
1055 FORMAT (' TIME (HOURS):	',G15.5)
1056 FORMAT (4(I5,G15.8))
     RETURN
     END


      SUBROUTINE GREEN (X,Y,DETJ,AG,IN,KF,JD,IEQ,MS,MAXNN,MAXNE,MXT,
    1	ME,NP,L,ISTOP)
C
C     PURPOSE: TO TO EVALUATE 1-D SHAPE FUCTIONS AND DETERMINANT
C**
      LEVEL 2, X,Y,KF,IN
      DIMENSION X(MAXNN),Y(MAXNN),DETJ(MXT),IN(ME,MAXNE),KF(MAXNE),
    1	JD(12),IEQ(4),AG(6)
      DIMENSION G(4,4),DLX(4,4),ALF(2),JN(4),JS(4),KN(2)
C
C
      IC = 0
      KN(1) = 0
          GO TO 300
    1 KN(1) = KN(1) + 1
          IF (KF(L).NE.IC) GO TO 300
      KN(2) = KN(1) + 1
          IF (KN(1).EQ.4) KN(2)=1
    4 II = 0
      I = 0
          DO 10 K=1,2
    5	  IF (I.LT.4) GO TO 6
              ISTOP = ISTOP + 1
          GO TO 500
    6 I = I + 1
          IF (KN(K).NE.I) GO TO 5
      KNK = KN(K)
      II = II + 1
      JN(II) = JD(KNK)
      JS(II) = JD(KNK)
      IEQ(II) = I
      I = 0
   10 CONTINUE
      IP = 2
      IQ = 2
      ID = 2*KN(1) + 2
          DO 20 J=1,2
      IP = IP + 1
      ID = ID + 1
      JN(IP) = IN(ID,L)
          IF(IN(ID,L).EQ.0) GO TO 20
      II = II + 1
      JS(II) = IN(ID,L)
   20 CONTINUE
      MS = II
          IF (MS.EQ.2) GO TO 35
          DO 30 K=3,MS
   25 IQ = IQ + 1
          IF (JD(IQ).NE.JS(K)) GO TO 25
   30 IEQ(K) = IQ
   35 CONTINUE
          DO 80 K=1,NP
      XI = AG(K)
          IF (NP.GT.2) XI=AG(K+2)
      XI1 = 1.0 - XI
      XI2 = 1.0 + XI
C
C     CORNER NODE SHAPE FUNCTION, BASIC PART
      ALF(1) = 0.5*XI1
      ALF(2) = 0.5*XI2
C
C	  DETERMINE IF SIDE	IS LINEAR, QUADRATIC, OR CUBIC
          IF (MS.EQ.4) GO TO 45
          IF (MS.EQ.3) GO TO 40
C
C	  LINEAR SIDE
      G(1,K) = ALF(1)
      G(2,K) = ALF(2)
      DLX(1,K) = -0.5
      DLX(2,K) * 0.5
          GO TO 50
C
C	  QUADRATIC SIDE
   40 G(1,K) = -XI*ALF(1)
      G(2,K) = XI*ALF(2)
      G(3,K) = 1.0 - XI*XI
      DLX(1,K) = XI - 0.5
      DLX(2,K) = XI + 0.5
      DLX(3,K) = -2.0*XI
          GO TO 50
C
C	  CUBIC SIDE
   45 XI2 = XI*XI
      G(1,K) = ALF(1)*(1.125*XI2-0.125)
      G(2,K) = ALF(2)*(1.125*XI2-0.125)
      G(3,K) = 0.5625*(1.0-XI2)*(1.0-3.0*XI)
      G(4,K) = 0.5625*(1.0-XI2)*(1.0+3.0*XI)
      DLX(1,K) = 0.0625*(18.0*XI-27.0*XI2+1.0)
      DLX(2,K) = 0.0625*(18.0*XI+27.0*XI2-1.0)
      DLX(3,K) = 0.5625*(9.0*XI2-2.0*XI-3.0)
      DLX(4,K) = 0.5625*(-9.0*XI2-2.0*XI+3.0)
   50 CONTINUE
C
C	  JACOBIANS
      SUMX = 0.0
      SUMY = 0.0
          DO 60 LI=1,MS
      KS = JS(LI)
      SUMX = SUMX + DLX(LI,K)*X(KS)
   60 SUMY = SUMY + DLX(LI,K)*Y(KS)
      DETJ(K+NP) = SUMY
      DETJ(K) = SUMX
   80 CONTINUE
  300 IC = IC + 1
          IF (KN(1).LT.4) GO TO 1
  500 CONTINUE
      RETURN
      END


      SUBROUTINE SHAPE (X,Y,IN,L,M,XI,YI,F,DET,DGX,DGY,MAXNN,MAXNE,
    1	MXC,ME)
C
C     PURPOSE: TO EVALUATE 2-D SHAPE FUNCTIONS AND DETERMINANT
C**
      LEVEL 2, X,Y,IN
      DIMENSION X(MAXNN),Y(MAXNN),IN(ME,MAXNE)
      DIMENSION DGX(MXC),DGY(MXC),F(MXC)
      DIMENSION ALF(4),DAX(4),DAY(4),BTX(4),BTY(4),DBX(4),DBY(4),
    1	DFX(12),DFY(12)
C
      XI1 = 1. - XI
      XI2 = 1. + XI
      YI1 = 1. - YI
      YI2 = 1. + YI
C
C     CORNER NODE SHAPE FUNCTIONS, BASIC PART
      ALF(1) = .25*XI1*YI1
      ALF(2) = .25*XI2*YI1
      ALF(3) = .25*XI2*YI2
      ALF(4) = .25*XI1*YI2
      DAX(1) = -.25*YI1
      DAX(2) = .25*YI1
      DAX(3) = .25*YI2
      DAX(4) = -.25*YI2
      DAY(1) = -.25*XI1
      DAY(2) = -.25*XI2
      DAY(3) = .25*XI2
      DAY(4) = .25*XI1
C
C     CORNER NODE SHAPE FUNCTIONS, SIDE-DEPENDENT PART
      XQ1 = XI - .5
      XQ2 = -XI - .5
      YQ1 = YI - .5
      YQ2 = -YI - .5
      XC1 = 1.125*XI*XI - .625
      XC2 = 2.25*XI
      YC1 = 1.125*YI*YI - .625
      YC2 = 2.25*YI
      J1 = 1
      J2 = 2
      J3 = 5
          DO 50 J=1,2
          IF (IN(J3,L).EQ.0) GO TO 10
          IF (IN(J3+1,L).EQ.0) GO TO 20
          GO TO 30
   10 CONTINUE
      BTX(J1) = .5
      BTX(J2) = .5
      DBX(J1) = 0.
      DBX(J2) = 0.
          GO TO 40
   20 CONTINUE
      BTX(J1) = XQ2
      BTX(J2) = XQ1
      DBX(J1) = -1.
      DBX(J2) = 1.
          GO TO 40
   30 CONTINUE
      BTX(J1) = XC1
      BTX(J2) = XC1
      DBX(J1) = XC2
      DBX(J2) = XC2
   40 CONTINUE
      J1 = 4
      J2 = 3
      J3 = 9
   50 CONTINUE
      J1 = 2
      J2 = 3
      J3 = 7
          DO 100 J=1,2
          IF (IN(J3,L).EQ.0) GO TO 60
          IF (IN(J3+1,L).EQ.0) GO TO 70
          GO TO 80
   60 CONTINUE
      BTY(J1) = .5
      BTY(J2) = .5
      DBY(J1) = 0.
      DBY(J2) = 0.
          GO TO 90
   70 CONTINUE
      BTY(J1) = YQ2
      BTY(J2) = YQ1
      DBY(J1) = -1.
      DBY(J2) = 1.
          GO TO 90
   80 CONTINUE
      BTY(J1) = YC1
      BTY(J2) = YC1
      DBY(J1) = YC2
      DBY(J2) = YC2
   90 CONTINUE
      J1 = 1
      J2 = 4
      J3 = 11
  100 CONTINUE
C
C     SHAPE FUNCTION DERIVATIVE MATRIX - CORNER NODES
          DO 110 J=1,4
      DFX(J) = DAX(J)*(BTX(J)+BTY(J)) + DBX(J)*ALF(J)
      DFY(J) = DAY(J)*(BTX(J)+BTY(J)) + DBY(J)*ALF(J)
      F(J) = ALF(J)*(BTX(J)+BTY(J))
  110 CONTINUE
C
C     SHAPE FUNCTION DERIVATIVE MATRIX - EDGE NODES
          IF (M.EQ.4) GO TO 240
      J = 4
      XEQ = 1. -XI*XI
      YEQ = 1. -YI*YI
      XE1 = 1. -3.*XI
      XE2 = 1. +3.*XI
      YE1 = 1. -3.*YI
      YE2 = 1. +3.*YI
          IF (IN(5,L).EQ.0) GO TO 140
          IF (IN(6,L).EQ.0) GO TO 120
          GO TO 130
  120 J = J + 1
      DFX(J) = -XI*YI1
      DFY(J) = -.5*XEQ
      F(J) = .5*XEQ*YI1
          GO TO 140
  130 J = J + 1
      DFX(J) = -.28125*YI1*(3.*XEQ+2.*XI*XE1)
      DFY(J) = -.28125*XEQ*XE1
      F(J) = .28125*XEQ*XE1*YI1
      J = J + 1
      DFX(J) = .28125*YI1*(3.*XEQ-2.*XI*XE2)
      DFY(J) = -.28125*XEQ*XE2
      F(J) = .28125*XEQ*XE2*YI1
  140     IF (IN(7,L).EQ.0) GO TO 170
          IF (IN(8,L).EQ.0) GO TO 150
          GO TO 160
  150 J = J + 1
      DFX(J) = .5*YEQ
      DFY(J) = -YI*XI2
      F(J) = .5*XI2*YEQ
          GO TO 170
  160 J = J + 1
      DFX(J) = .28125*YEQ*YE1
      DFY(J) = -.28125*XI2*(3.*YEQ+2.*YI*YE1)
      F(J) = .28125*XI2*YEQ*YE1
      J = J + 1
      DFX(J) = .28125*YEQ*YE2
      DFY(J) = .28125*XI2*(3.*YEQ-2.*YI*YE2)
      F(J) = .28125*XI2*YEQ*YE2
  170     IF (IN(9,L).EQ.0) GO TO 200
          IF (IN(10,L).EQ.0) GO TO 180
          GO TO 190
  180 J = J + 1
      DFX(J) = -XI*YI2
      DFY(J) = .5*XEQ
      F(J) = .5*XEQ*YI2
          GO TO 200
  190 J = J + 1
      DFX(J) = .28125*YI2*(3.*XEQ-2.*XI*XE2)
      DFY(J) = .28125*XEQ*XE2
      F(J) = .28125*XEQ*XE2*YI2
      J = J + 1
      DFX(J) = -.28125*YI2*(3.*XEQ+2.*XI*XE1)
      DFY(J) = .28125*XEQ*XE1
      F(J) = .28125*XEQ*XE1*YI2
  200     IF (IN(11,L).EQ.0) GO TO 230
          IF (IN(12,L).EQ.0) GO TO 210
          GO TO 220
  210 J = J + 1
      DFX(J) = -.5*YEQ
      DFY(J) = -YI*XI1
      F(J) = .5*XI1*YEQ
          GO TO 230
  220 J = J + 1
      DFX(J) = -.28125*YEQ*YE2
      DFY(J) = .28125*XI1*(3.*YEQ-2.*YI*YE2)
      F(J) = .28125*XI1*YEQ*YE2
      J = J + 1
      DFX(J) = -.28125*YEQ*YE1
      DFY(J) = -.28125*XI1*(3.*YEQ+2.*YI*YE1)
      F(J) = .28125*YEQ*YE1*XI1
  230 CONTINUE
  240 CONTINUE
C
C     JACOBIAN
      SUM1 = 0.
      SUM2 = 0.
      SUM3 = 0.
      SUM4 = 0.
      K = 0
          DO 260 I=1,M
  250 K = K + 1
          IF (IN(K,L).EQ.0) GO TO 250
      KI = IN(K,L)
      SUM1 = SUM1 + DFX(I)*X(KI)
      SUM2 = SUM2 + DFX(I)*Y(KI)
      SUM3 = SUM3 + DFY(I)*X(KI)
      SUM4 = SUM4 + DFY(I)*Y(KI)
  260 CONTINUE
      DET = SUM1*SUM4 - SUM2*SUM3
      DET1 = 1./DET
      C11 = DET1*SUM4	^
      C12 = -DET1*SUM2
      C21 = -DET1*SUM3
      C22 = DET1*SUM1
C
C     SHAPE FUNCTION DERIVATIVES - GLOBAL
          DO 270 J=1,M
      DGX(J) = C11*DFX(J) + C12*DFY(J)
      DGY(J) = C21*DFX(J) + C22*DFY(J)
  270 CONTINUE
      RETURN
      END


      SUBROUTINE GELB (R,A,M,N,MUD,MLD,EPS,IER,MAXR,MAXA)
C
C     PURPOSE: TO SOLVE THE MASS-TRANSPORT EQUATION
C**
      LEVEL 2, A
      DIMENSION A(MAXA),R(MAXR)
C
C	TEST ON WRONG INPUT PARAMETERS
      IF(MLD) 47,1,1
    1 IF(MUD) 47,2,2
    2 MC=1+MLD+MUD
      IF(MC+1-M-M)3,3,47
C
C	PREPARE INTEGER PARAMETERS
C	    MC=NUMBER OF COLUMNS IN MATRIX A
C	    MU-NUMBER OF ZEROS TO BE INSERTED IN FIRST ROW OF MATRIX A
C	    ML=NUMBER OF MISSING ELEMENTS IN LAST ROW OF MATRIX A
C	    MR=INDEX OF LAST ROW IN MATRIX A WITH MC ELEMENTS
C	    MZ=TOTAL NUMBER OF ZEROS TO BE INSERTED IN MATRIX A
C	    MA=TOTAL NUMBER OF STORAGE LOCATIONS NECESSARY FOR MATRIX A
C	    NM=NUMBER OF ELEMENTS IN MATRIX R
    3 IF(MC-M)5,5,4
    4 MC=M
    5 MU=MC-MUD-1
      ML=MC-MLD-1
      MR=M-ML
      MZ=(MU*(MU+1))/2
      MA=M*MC-(ML*(ML+1))/2
      NM=N*M
C
C     MOVE ELEMENTS BACKWARD AND SEARCH FOR ABSOLUTELY GREATEST ELEMENT
      IER=0
      PIV=0.
      IF(MLD)14,14,6
    6 JJ=MA
      J=MA-MZ
      KST=J
      DO 9 K=1,KST
      TB=A(J)
      A(JJ)=TB
      TB=ABS(TB)
      IF(TB-PIV)8,8,7
    7 PIV=TB
    8 J=J-1
    9 JJ=JJ-1
C
C     INSERT ZEROS IN FIRST MU ROWS (NOT NECESSARY IN CASE MZ=0)
      IF(MZ)14,14,10
   10 JJ=1
      J=1+MZ
      IC=1+MUD
      DO 13 I=1,MU
      DO 12 K=1,MC
      A(JJ)=0.
      IF(K-IC)11,11,12
   11 A(JJ)=A(J)
      J=J+1
   12 JJ=JJ+1
   13 IC=IC+1
C
C	GENERATE TEST VALUE FOR SINGULARITY
   14 TOL=EPS*PIV
C
C	START DECOMPOSITION LOOP
      KST=1
      IDST=MC
      IC=MC-1
      DO 38 K=1,M
      IF(K-MR-1)16,16,15
   15 IDST=IDST-1
   16 ID=IDST
      ILR=K+MLD
      IF(ILR-M)18,18,17
   17 ILR=M
   18 II=KST
C
C     PIVOT SEARCH IN FIRST COLUMN (ROW INDEXES FROM I=K UP TO I=ILR)
      PIV=0.
      DO 22 I=K,ILR
      TB=ABS(A(II))
      IF(TB-PIV)20,20,19
   19 PIV=TB
      J=I
      JJ=II
   20 IF(I-MR)22,22,21
   21 ID=ID-1
   22 II=II+ID
C
C     TEST ON SINGULARITY
      IF(PIV)47,47,23
   23 IF(IER)26,24,26
   24 IF(PIV-TOL)25,25,26
   25 IER=K-1
   26 PIV=1./A(JJ)
C
C     PIVOT ROW REDUCTION AND ROW INTERCHANGE IN RIGHT HAND SIDE R
      ID=J-K
      DO 27 I=K,NM,M
      II=I+ID
      TB=PIV*R(II)
      R(II)=R(I)
   27 R(I)=TB
C
C     PIVOT ROW REDUCTION AND ROW INTERCHANGE IN COEFFICIENT MATRIX A
      II=KST
      J=JJ+IC
      DO 28 I=JJ,J
      TB=PIV*A(I)
      A(I)=A(II)
      A(II)=TB
   28 II=II+1
C
C     ELEMENT REDUCTION
      IF(K-ILR)29,34,34
   29 ID=KST
      II=K+1
      MU=KST+1
      MZ=KST+IC
      DO 33 I=II,ILR
C
C     IN MATRIX A
      ID=ID+MC
      JJ=I-MR-1
      IF(JJ)31,31,30
   30 ID=ID—JJ
   31 PIV=-A(ID)
      J=ID+1
      DO 32 JJ=MU,MZ
      A(J-1)=A(J)+PIV*A(JJ)
   32 J=J+1
      A(J-1)=0.
C
C     IN MATRIX R
      J=K
      DO 33 JJ=I,NM,M
      R(JJ)=R(JJ)+PIV*R(J)
      J=J+M
   33 CONTINUE
   34 KST=KST+MC
      IF(ILR-MR)36,35,35
   35 IC=IC-1
   36 ID=K-MR
      IF(ID)38,38,37
   37 KST=KST-ID
   38 CONTINUE
C     END OF DECOMPOSITION LOOP
C
C     BACK SUBSTITUTION
      IF(MC-1)46,46,39
   39 IC=2
      KST=MA+ML-MC+2
      II=M
      DO 45 I=2,M
      KST=KST-MC
      II=II-1
      J=II-MR
      IF(J)41,41,40
   40 KST=KST+J
   41 DO 43 J=II,NM,M
      TB=R(J)
      MZ=KST+IC-2
      ID=J
      DO 42 JJ=KST,MZ
      ID=ID+1
   42 TB=TB-A(JJ)*R(ID)
      R(J)=TB
   43 CONTINUE
      IF(IC-MC)44,45,45
   44 IC=IC+1
   45 CONTINUE
   46 RETURN
C
C     ERROR RETURN
   47 IER=—1
      RETURN
      END
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
C       END GROUP B
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
          PHII(I) = PHII(I) * APHII
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
      MND = MAX0(ND, MND)
      MAXDIF = MAX0(ND, MAXDIF)
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

C     Group M  
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
          do 370 K = 1, NSEEP
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

C Group P
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
  910 format (16I5)
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
     & 22H GREATER THAN ESTIMATE/11X, 5HND = , I3, 10X, 6HNB = ,
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

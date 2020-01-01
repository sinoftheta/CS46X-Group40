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

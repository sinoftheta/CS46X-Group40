#ifndef __CAP_GS2_H__
#define __CAP_GS2_H__

#include <stdio.h>

#include "../capstone/Array.h"
#include "../capstone/Matrix.h"

typedef struct gs2MemoryRequirements {
    int mxc;
    int mxt;
    int maxnn;
    int maxne;
    int ns1;
    int kns1;
    int maxm4;
    int maxm5;
    int maxeep;
    int maxbw;
    // 2 * maxbw - 1
    int maxbw2;
    // maxnn -  ns1
    int maxm1;
    // maxnn - nkns1
    int maxm2;
    // max(maxm1, maxm2)
    int maxs;
    // maxbw2 * maxs
    int mx;
} gs2MemoryRequirements;

typedef enum gs2Scheme {
    BACK,
    CENT
} gs2Scheme;

// this is the type that should be passed into each relevant 
// subroutine as a reference
typedef struct gs2State {
    gs2MemoryRequirements memoryRequirements;

    // program parameters
    // tape4 = input
    FILE* input;
    // tape6 = output
    FILE* output;
    
    FILE *tape7, *tape8, *tape2,
          *tape4, *tape11, *tape13;

    // Common BLK1
    // should be dimensioned (MAXBW2, MAXS)
    // where MAXBW2 = (2 * MAXBW-1)
    //       MAXS = max(MAXM1, MAXM2)
    Matrix s;

    // Common BLK1A
    // should be dimensioned (MAXS, MAXBW)
    // where MAXS = max(MAXM1, MAXM2)
    Matrix p;

    // Common BLK1B
    // should be dimensioned MX 
    // where MX = MAXBW2 * MAXS
    //       MAXBW2 = (2 * MAXBW-1)
    Array w;

    // Common BLK2
    // must be dimensoned to MAXNN
    // MAXNN must be greater than or equal to NN
    Array cfq, conc, conci, fq, klc, klr,
          lc, lr, phi, phii, x, y;

    // Common BLK3
    // must be dimensioned to MAXNE
    // MAXNE must be greater than or equal to NE
    Array alpha, elong, etrans, fmobx, fmoby,
          kd, kf, lambda, por, rho, tta;

    // Common BLK4
    // must be dimensioned (ME, MAXNE) and (2, MAXNE)
    Matrix in, ie;

    // Common ONE
    gs2Scheme type;

    double chng, tdr, stat, statp, oldt, delt, 
           dprdt, betap, clos1, delp, difusn,
           pchng, vmax, stime, ssec, h1, h2,
           pl, coefi, ei;

    int ne, np, nk, nseep, inc, me, igo, 
        kod1, kod2, kod3, kod4, kod7, 
        kod8, kod9, kod10, kod11, kod12,
        itmax, itchng, iter1;

    
    // Common TWO
    //    The maximum number of materials and unsaturated material
    //    property interpolation points is controlled by the size
    //    of the arrays in common block two which should be
    //    dimensioned such that:
    //      WK(12 * N-1), XK(N, M), XPSI(N, M), XM(N, M),
    //      CKT(3, N-1, M), CTT(3, N-1, M), PSIO(M), ISPL(M)
    //    Where,  M = maximum # of material types
    //            N = maximum # of interpolation points provided
    //                in the unsaturated material property tables.
    Array wk;
    Matrix xk;
    Matrix xm;
    Matrix xpsi;
    Matrix ckt[3];
    Matrix ctt[3];
    Array psio;
    Array ispl;

    // Common Three
    int nn, mm, km, it, nsdn, nb, knb, knb2, istop;
    double mb, mb2, 
           kmb, kmb2;

   // Common Four
   // Dimensions of variables in common block four should be
   // equal to MXC and MXT. These values are currently set to
   // MXC = 12 and MXT = 16. To save space, only in the case that
   // all quadrilateral elements are specified (input value of
   // INC = 4), both MXC and MXT can be set to 4 and all variables
   // in common block four changed to dimensions of 4.
   Matrix pe, se, f, dx, dy;
   Array q, detj, cphi, vkx, vky, dpordt,
         d0, dk, dh, srcrt, ff, dgx, dgy,
         srcr;

} gs2State;


// This function exists as a convience for creating a memReqs
// struct in python
gs2MemoryRequirements gs2CreateMemoryRequirements(
    int mxc,
    int mxt,
    int maxnn,
    int maxne,
    int ns1,
    int kns1,
    int maxm4,
    int maxm5,
    int maxeep,
    int maxbw
);

#endif /* __CAP_GS2_H__ */
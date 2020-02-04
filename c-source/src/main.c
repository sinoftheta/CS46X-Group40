#include <stdio.h>
#include <stdlib.h>

#include "capstone/Matrix.h"
#include "capstone/Array.h"
#include "capstone/FileUtil.h"
#include "capstone/CSVFile.h"
#include "capstone/MathUtil.h"

#include "gs2/datain.h"
#include "gs2/gs2.h"

#include <gsl/gsl_sf_bessel.h>

int main(int argc, char** argv) {


    double x = 5.0;
    double y = gsl_sf_bessel_J0 (x);
    printf ("J0(%g) = %.18e\n", x, y);

    gs2MemoryRequirements memReqs;
    memReqs.mxc = 12;
    memReqs.mxt = 16;
    memReqs.maxnn = 52;
    memReqs.maxne = 25;
    memReqs.ns1 = 2;
    memReqs.kns1 = 0;
    memReqs.maxm4 = 1;
    memReqs.maxm5 = 1;
    memReqs.maxeep = 1;
    memReqs.maxbw = 4;
    memReqs.maxbw2 = 7;
    memReqs.maxm1 = memReqs.maxnn - memReqs.ns1;
    memReqs.maxm2 = memReqs.maxnn - memReqs.kns1;
    memReqs.maxs = max(memReqs.maxm1, memReqs.maxm2);
    memReqs.mx = memReqs.maxbw2 * memReqs.maxs;

    gs2State state;
    state.memoryRequirements = memReqs;
    state.istop = 0;

    double maxdif = 0.0;

    Array old, cold, cn, vn, coef, u, est, lp, klp, nsf, nsk, msp;
    Matrix nsp;

    arrayDimension(&old, memReqs.maxm1);
    arrayDimension(&cold, memReqs.maxm2);
    arrayDimension(&cn, memReqs.maxm4);
    arrayDimension(&vn, memReqs.maxm4);
    arrayDimension(&coef, memReqs.maxm4);
    arrayDimension(&u, memReqs.maxm1);
    arrayDimension(&est, memReqs.maxm1);
    arrayDimension(&lp, memReqs.maxm1);
    arrayDimension(&klp, memReqs.maxm2);
    arrayDimension(&nsf, memReqs.maxm4);
    arrayDimension(&nsk, memReqs.maxm4);
    arrayDimension(&msp, memReqs.maxeep);
    matrixDimension(&nsp, memReqs.maxm5, memReqs.maxeep);
    
    gs2Datain(
        &state,
        "res/example1.csv",
        &old,
        &cold,
        &cn,
        &vn,
        &coef,
        &u,
        &est,
        &lp,
        &klp,
        &nsf,
        &nsk,
        &nsp,
        &msp, 
        &maxdif
    );

    // don't care about freeing right now

    return 0;
}
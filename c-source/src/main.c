#include <stdio.h>
#include <stdlib.h>

#include "capstone/Matrix.h"
#include "capstone/Array.h"
#include "capstone/FileUtil.h"
#include "capstone/CSVFile.h"
#include "capstone/MathUtil.h"

#include "gs2/datain.h"
#include "gs2/gs2.h"


int main(int argc, char** argv) {
    gs2MemoryRequirements memReqs = gs2CreateMemoryRequirements(
        12,
        16,
        52,
        25,
        2,
        0,
        2,
        1,
        1,
        4
    );

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
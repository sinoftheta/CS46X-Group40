#include <stdio.h>
#include <stdlib.h>

#include "capstone/Matrix.h"
#include "capstone/Array.h"
#include "capstone/FileUtil.h"
#include "capstone/CSVFile.h"
#include "capstone/MathUtil.h"

#include "gs2/datain.h"
#include "gs2/gs2.h"
#include "gs2/ts.h"

int main(int argc, char** argv) {


    gs2DefaultIO();
    gs2OutputFile("res/out.txt");

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

    Array fm, rt, cfm, crt, fx;
    

    arrayDimension(&fm, memReqs.maxm1);
    arrayDimension(&rt, memReqs.maxm1);
    arrayDimension(&cfm, memReqs.maxm2);
    arrayDimension(&crt, memReqs.maxm2);
    arrayDimension(&fx, memReqs.maxm1);

    state.h1 = 0.347854;
    state.h2 = 0.652145;

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

    gs2Ts(
        &state,
        &(state.s),
        &(state.p),
        &(state.w),
        &fm,
        &rt,
        &(state.phi),
        &(state.phii),
        &old,
        &cfm,
        &crt,
        &(state.conc),
        &(state.conci),
        &cold,
        &fx,
        &cn,
        &vn,
        &coef,
        &est,
        &u,
        &(state.fq),
        &(state.cfq),
        &(state.x),
        &(state.y),
        &(state.fmobx),
        &(state.fmoby),
        &(state.elong),
        &(state.etrans),
        &(state.por),
        &(state.alpha),
        &(state.tta),
        &(state.kd),
        &(state.lambda),
        &(state.rho),
        &(state.in),
        &(state.kf),
        &(state.lr),
        &(state.klr),
        &(state.lc),
        &(state.klc),
        &lp,
        &nsf,
        &(state.ie),
        &nsp,
        &msp,
        "text",
        "text2"
    );
    // don't care about freeing right now

    return 0;
}

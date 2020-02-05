#include "gs2.h"

#include "../capstone/MathUtil.h"

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
) {

    gs2MemoryRequirements memReqs;
    memReqs.mxc = mxc;
    memReqs.mxt = mxt;
    memReqs.maxnn = maxnn;
    memReqs.maxne = maxne;
    memReqs.ns1 = ns1;
    memReqs.kns1 = kns1;
    memReqs.maxm4 = maxm4;
    memReqs.maxm5 = maxm5;
    memReqs.maxeep = maxeep;
    memReqs.maxbw = maxbw;

    memReqs.maxbw2 = 2 * maxbw - 1;
    memReqs.maxm1 = memReqs.maxnn - memReqs.ns1;
    memReqs.maxm2 = memReqs.maxnn - memReqs.kns1;
    memReqs.maxs = max(memReqs.maxm1, memReqs.maxm2);
    memReqs.mx = memReqs.maxbw2 * memReqs.maxs;



    return memReqs;
}

// make sure gs2 is compiled

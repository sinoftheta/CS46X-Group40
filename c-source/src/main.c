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

    gs2Datain(
        &state,
        "res/example1.csv",
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL
    );

    return 0;
}
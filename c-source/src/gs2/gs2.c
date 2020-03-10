#include "gs2.h"

#include "../capstone/MathUtil.h"

#include <stdio.h>
#include <stdlib.h>

FILE* gs2stdin = NULL;
FILE* gs2stdout = NULL;
FILE* gs2stderr = NULL;

gs2CallbackType gs2Callback = &gs2DefaultCallback;

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

void gs2DefaultIO() {
    gs2stdout = stdout;
    gs2stdin = stdin;
    gs2stderr = stderr;
}

void gs2InputFile(const char* filepath) {
    gs2stdin = fopen(filepath, "r");

    if (!gs2stdin)
        exit(2);
}

void gs2OutputFile(const char* filepath) {
    gs2stdout = fopen(filepath, "w");

    if (!gs2stdout)
        exit(2);
}

void gs2ErrorFile(const char* filepath) {
    gs2stderr = fopen(filepath, "w");
    
    if (!gs2stderr)
        exit(2);
}

void gs2CloseFiles() {

    if (gs2stdin != stdin) 
        fclose(gs2stdin);

    if (gs2stderr != stderr)
        fclose(gs2stderr);

    if (gs2stdout != stdout)
        fclose(gs2stdout);

    gs2DefaultIO();
}

void gs2RegisterCallback(gs2CallbackType callback) {
    gs2Callback = callback;
}

int gs2DefaultCallback(gs2State state) {
    fprintf(stderr, "Default gs2Callback has been called!\n");
    return 0;
}
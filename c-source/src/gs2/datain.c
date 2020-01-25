#include "datain.h"

#include "../capstone/CSVFile.h"
#include "../capstone/MathUtil.h"

void gs2Datain(
    gs2State* state, 
    const char* csvPath,
    Array* cold, 
    Array* cn,
    Array* vn,
    Array* coef,
    Array* u,
    Array* est,
    Array* lp,
    Array* klp,
    Array* nsf,
    Array* nsk,
    Matrix* nsp,
    Array* msp
) {
    
    
    // need to have a discussion about how we want to set "memory" requirements
    // im thinking we just add another row to the file
    // this should probably be in main
    // gs2MemoryRequirements memReqs;
    // memReqs.mxc = 12;
    // memReqs.mxt = 16;
    // memReqs.maxnn = 52;
    // memReqs.maxne = 25;
    // memReqs.ns1 = 2;
    // memReqs.kns1 = 0;
    // memReqs.maxm4 = 1;
    // memReqs.maxm5 = 1;
    // memReqs.maxeep = 1;
    // memReqs.maxbw = 4;
    // memReqs.maxbw2 = 7;
    // memReqs.maxm1 = memReqs.maxnn - memReqs.ns1;
    // memReqs.maxm2 = memReqs.maxnn - memReqs.kns1;
    // memReqs.maxs = max(memReqs.maxm1, memReqs.maxm2);
    // memReqs.mx = memReqs.maxbw2 * memReqs.maxs;
    // state->istop = 0;
    
    CSVFile csvFile = csvLoadFile(csvPath);


    arrayDimension(&(state->phi), state->memoryRequirements.maxnn);
    arrayDimension(&(state->phii), state->memoryRequirements.maxnn);
    // old should already be dimensioned
    // est should already be dimensioned
    // u should already be dimensioned
    arrayDimension(&(state->conc), state->memoryRequirements.maxnn);
    arrayDimension(&(state->conci), state->memoryRequirements.maxnn);
    // cold should already be dimensioned
    // cn should already be dimensioned
    // vn should already be dimensioned
    arrayDimension(&(state->fq), state->memoryRequirements.maxnn);
    arrayDimension(&(state->cfq), state->memoryRequirements.maxnn);
    arrayDimension(&(state->x), state->memoryRequirements.maxnn);
    arrayDimension(&(state->y), state->memoryRequirements.maxnn);
    // coef should already be dimensioned
    arrayDimension(&(state->fmobx), state->memoryRequirements.maxne);
    arrayDimension(&(state->fmoby), state->memoryRequirements.maxne);
    arrayDimension(&(state->por), state->memoryRequirements.maxne);
    arrayDimension(&(state->elong), state->memoryRequirements.maxne);
    arrayDimension(&(state->etrans), state->memoryRequirements.maxne);
    arrayDimension(&(state->alpha), state->memoryRequirements.maxne);
    arrayDimension(&(state->tta), state->memoryRequirements.maxne);
    arrayDimension(&(state->kd), state->memoryRequirements.maxne);
    arrayDimension(&(state->lambda), state->memoryRequirements.maxne);
    arrayDimension(&(state->rho), state->memoryRequirements.maxne);
    // state->me should be 13
    matrixDimension(&(state->in), state->me, state->memoryRequirements.maxne);
    matrixDimension(&(state->ie), 2, state->memoryRequirements.maxne);
    arrayDimension(&(state->kf), state->memoryRequirements.maxne);
    arrayDimension(&(state->lr), state->memoryRequirements.maxnn);
    arrayDimension(&(state->klr), state->memoryRequirements.maxnn);
    arrayDimension(&(state->lc), state->memoryRequirements.maxnn);
    arrayDimension(&(state->klc), state->memoryRequirements.maxnn);
    // klp should already be dimensioned
    // title is local
    // nsf should already be dimensioned
    // nsk should already be dimensioned
    // msp should already be dimensioned

    Array wxpsi, wxm, wxk;
    Matrix cc;

    arrayDimension(&wxpsi, 20);
    arrayDimension(&wxm, 20);
    arrayDimension(&wxk, 20);
    matrixDimension(&cc, 3, 19);



    


    arrayFree(&wxpsi);
    arrayFree(&wxm);
    arrayFree(&wxk);
    matrixFree(&cc);
    csvFreeFile(&csvFile);
}
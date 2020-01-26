#include "datain.h"

#include "../capstone/CSVFile.h"
#include "../capstone/MathUtil.h"
#include "../capstone/Debug.h"

#include <string.h>


#define DEFAULT(cond, data, value) do { if (!cond) { data = value; } } while(0) 

const char* gs2DataGroupNames[NUM_DATA_GROUP] = {
    "A",
    "B",
    "C",
    "D",
    "E",
    "F-1",
    "F-2",
    "G-1",
    "G-2",
    "H-1",
    "H-2",
    "H-3",
    "I",
    "J-1",
    "J-2",
    "K",
    "L",
    "M-1",
    "M-2",
    "M-3",
    "M-4",
    "N-1",
    "N-2",
    "O-1",
    "O-2",
    "O-3",
    "P",
    "Q-1",
    "Q-2",
    "Q-3",
    "Q-4",
    "R"
};

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

    int ns, kns, nf, mq4, knsdn, nvs;

    CSVFile csvFile = csvLoadFile(csvPath);
    gs2DataGroup dataGroup = NUM_DATA_GROUP;

    arrayDimension(&wxpsi, 20);
    arrayDimension(&wxm, 20);
    arrayDimension(&wxk, 20);
    matrixDimension(&cc, 3, 19);

    CSVRow* row = csvFile.currentRow;
    do {
        dataGroup = gs2GetGroup(row, dataGroup);
        

        switch (dataGroup) { 
            case GROUP_A:
                gs2ReadGroupA(&row, state);
                break;
            case GROUP_B:
                // moves row
                gs2ReadGroupB(&row, state, &ns, &kns, &nf, &mq4, &knsdn, &nvs);
                // exit(1);
                break;
            default:
                //fprintf(stderr, "Reached default case in gs2Datain!\n");
                break;
        };
        row = row->next;
    } while (row != CSV_NULL_ROW_PTR);

    arrayFree(&wxpsi);
    arrayFree(&wxm);
    arrayFree(&wxk);
    matrixFree(&cc);
    csvFreeFile(&csvFile);
}

gs2DataGroup gs2GetGroup(CSVRow* csvRow, gs2DataGroup defaultTo) {
    if (csvRow->entryCount <= 0)
        return defaultTo;

    char* firstCell = csvRow->entries[0];

    for (int i = 0; i < NUM_DATA_GROUP; i++) {
        if (!strcmp(firstCell, gs2DataGroupNames[i])) {
            return (gs2DataGroup)(i);
        }
    }

    return defaultTo;
}

void gs2ReadGroupA(CSVRow** csvRow, gs2State* state) {
    // title should be in the second cell in csvRow
    // state is needed for when we actually write to files
    fprintf(stdout, "%s\n", (*csvRow)->entries[1]);
}

void gs2ReadGroupB(
    CSVRow** csvRow, 
    gs2State* state, 
    int* ns, 
    int* kns, 
    int* nf, 
    int* mq4,
    int* knsdn,
    int* nvs
) {
    // group b permits empty cells as zeros

    // first row: group, nn, ne, ns, kns, nb, knb, nf, inc, nk, nseep
    if ((*csvRow)->entryCount < 11)
        croak("Group B Card 1, too few entries");

    DEFAULT(sscanf((*csvRow)->entries[1], "%d", &(state->nn)), state->nn, 0);
    DEFAULT(sscanf((*csvRow)->entries[2], "%d", &(state->ne)), state->ne, 0);
    DEFAULT(sscanf((*csvRow)->entries[3], "%d", ns), *ns, 0);
    DEFAULT(sscanf((*csvRow)->entries[4], "%d", kns), *kns, 0);
    DEFAULT(sscanf((*csvRow)->entries[5], "%d", &(state->nb)), state->nb, 0);
    DEFAULT(sscanf((*csvRow)->entries[6], "%d", &(state->knb)), state->knb, 0);
    DEFAULT(sscanf((*csvRow)->entries[7], "%d", nf), *nf, 0);
    DEFAULT(sscanf((*csvRow)->entries[8], "%d", &(state->inc)), state->inc, 0);
    DEFAULT(sscanf((*csvRow)->entries[9], "%d", &(state->nk)), state->nk, 0);
    DEFAULT(sscanf((*csvRow)->entries[10], "%d", &(state->nseep)), state->nseep, 0);

    // second row: group, nsdn, mq4, knsdn, pl, coefi, ei, nvs
    *csvRow = (*csvRow)->next;
    if ((*csvRow)->entryCount < 8) 
        croak("Group B Card 2, too few entries");

    DEFAULT(sscanf((*csvRow)->entries[1], "%d", &(state->nsdn)), state->nsdn, 0);
    DEFAULT(sscanf((*csvRow)->entries[2], "%d", mq4), *mq4, 0);
    DEFAULT(sscanf((*csvRow)->entries[3], "%d", knsdn), *knsdn, 0);
    DEFAULT(sscanf((*csvRow)->entries[4], "%lf", &(state->pl)), state->pl, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[5], "%lf", &(state->coefi)), state->coefi, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[6], "%lf", &(state->ei)), state->ei, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[7], "%d", nvs), *nvs, 0);

    if (state->nn > state->memoryRequirements.maxnn) {
        state->istop++;
        croakf("Too many nodal points: nn = %d, maxnn = %d", state->nn, state->memoryRequirements.maxnn);
    }

    if (state->ne > state->memoryRequirements.maxne) {
        state->istop++;
        croakf("Too many nodal points: ne = %d, maxne = %d", state->ne, state->memoryRequirements.maxne);
    }

    fprintf(stdout, "Number of: \n");
    fprintf(stdout, "\tNodes: %d\n", state->nn);
    fprintf(stdout, "\tElements: %d\n", state->ne);
    fprintf(stdout, "\tConstant flow boundaries: %d\n", *ns);
    fprintf(stdout, "\tConstant conc. boundaries: %d\n", *kns);
    fprintf(stdout, "\tSource or Sink nodes: %d\n", *nf);
    fprintf(stdout, "\tEstimated half-bandwidth for pressure: %d\n", state->nb);
    fprintf(stdout, "\tEstimated half-bandwidth for concentration: %d\n", state->knb);
    fprintf(stdout, "\tMaximum number of element nodes %d\n", state->inc);

    fprintf(stdout, "\tboundary nodes with specified flux: %d\n", state->nsdn);
    fprintf(stdout, "\tInitial value: %d\n", *mq4);
    fprintf(stdout, "\tMinimum allowed pressure: %lf\n", state->pl);
    fprintf(stdout, "\tMaximum flux: %lf\n", state->ei);    
    fprintf(stdout, "\tboundary nodes with specified concentration flux: %d\n", *knsdn);
    fprintf(stdout, "\tSeepage nodes: %d\n", state->nseep);

    // third row: group, delt, chng, itmax, itchng, pchng, betap, type
    *csvRow = (*csvRow)->next;
    if ((*csvRow)->entryCount < 8)
        croak("Group B Card 3, too few entries");
    
    DEFAULT(sscanf((*csvRow)->entries[1], "%lf", &(state->delt)), state->delt, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[2], "%lf", &(state->chng)), state->chng, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[3], "%d", &(state->itmax)), state->itmax, 0);
    DEFAULT(sscanf((*csvRow)->entries[4], "%d", &(state->itchng)), state->itchng, 0);
    DEFAULT(sscanf((*csvRow)->entries[5], "%lf", &(state->pchng)), state->pchng, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[6], "%lf", &(state->betap)), state->betap, 0.0);

    if (!strcmp((*csvRow)->entries[7], "CENT")) 
        state->type = CENT;
    else
        state->type = BACK;
    
    // fourth row: group, difusn, dprdt, stat, statp, clos1, iter1, igo
    *csvRow = (*csvRow)->next;
    if ((*csvRow)->entryCount < 7)
        croak("Group B Card 4, too few entries");

    DEFAULT(sscanf((*csvRow)->entries[1], "%lf", &(state->difusn)), state->difusn, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[2], "%lf", &(state->dprdt)), state->dprdt, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[3], "%lf", &(state->stat)), state->stat, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[4], "%lf", &(state->statp)), state->statp, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[5], "%lf", &(state->clos1)), state->clos1, 0.0);
    DEFAULT(sscanf((*csvRow)->entries[6], "%d", &(state->iter1)), state->iter1, 0);
    DEFAULT(sscanf((*csvRow)->entries[7], "%d", &(state->igo)), state->igo, 0);

    fprintf(stdout, "Time parameters:\n");
    fprintf(stdout, "\tInitial time step in hours: %lf\n", state->delt);
    fprintf(stdout, "\tMultipler for increasing time step: %lf", state->chng);
    fprintf(stdout, "\tMaximum permitted number of time steps: %d\n", state->itmax);
    fprintf(stdout, "\tNumber of time steps between changes in delt: %d\n", state->itchng);
    fprintf(stdout, "\tPressure change criterion: %lf\n", state->pchng);
    fprintf(stdout, "\tFluid compressibility (betap): %lf\n", state->betap);
    fprintf(stdout, "\tMolecular diffusino constant: %lf\n", state->difusn);
    fprintf(stdout, "\tFactor for time derivative of porosity: %lf\n", state->dprdt);
    fprintf(stdout, "\tNumber of iteration in flow: %d\n", state->iter1);
    fprintf(stdout, "\tClosure criterion: %lf\n", state->clos1);
    fprintf(stdout, "\tNumber of concentration steps per pres. step: %d\n", state->igo);

    switch (state->type) {
        case BACK:
            state->tdr = 1.0;
            fprintf(stdout, "Implicit time derivative in use.\n");
            break;
        case CENT:
            state->tdr = 2.0;
            fprintf(stdout, "Centered time derivative in use.\n");
            break;
        default:
            break;
    };

    if (state->stat == 0.0)
        fprintf(stdout, "Steady-state mass transport equation\n");
    
    if (state->statp == 0.0)
        fprintf(stdout, "Steady-state flow quation\n");

}
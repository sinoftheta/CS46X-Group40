#include "datain.h"

#include "../capstone/CSVFile.h"
#include "../capstone/MathUtil.h"
#include "../capstone/Debug.h"

#include "bc.h"

#include <string.h>
#include <stdlib.h>


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
    Array* msp,
    double* maxdif
) {

    // init state constants
    // state->me should be 13, needs extra value to store more info
    // elements are defined as having at most 12 incident nodes.
    state->me = 13;

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

    // group B extra parameters
    int ns, kns, nf, mq4, knsdn, nvs;

    // group C extra parameters
    double afmobx, afmoby, apor, aelong, aetran, aphii, 
           aconci, xfact, yfact, ateta, aal, akd, alam, arho;

    // group H extra parameters
    double hone;

    // subgroup J1 extra parameters
    int i1, i2, itype;

    CSVFile csvFile = csvLoadFile(csvPath);
    printf("csvfile: %s\n", csvFile.currentRow->entries[0]);
    gs2DataGroup dataGroup = NUM_DATA_GROUP;

    arrayDimension(&wxpsi, 20);
    arrayDimension(&wxm, 20);
    arrayDimension(&wxk, 20);
    matrixDimension(&cc, 3, 19);

    CSVRow* row = csvFile.currentRow;
    printf("row: %s\n", row->entries[0]);
    do {
        dataGroup = gs2GetGroup(row, dataGroup);
        

        switch (dataGroup) { 
            case GROUP_A:
                gs2ReadGroupA(&row, state);
                break;
            case GROUP_B:
                // moves row
                gs2ReadGroupB(&row, state, &ns, &kns, &nf, &mq4, &knsdn, &nvs);
                break;
            case GROUP_C:
                // moves row
                gs2ReadGroupC(
                    &row, state, &afmobx, &afmoby, &apor, &aelong, &aetran, &aphii,
                    &aconci, &xfact, &yfact, &ateta, &aal, &akd, &alam, &arho
                );
                break;
            case GROUP_D:
                gs2ReadGroupD(&row, state);
                break;
            case GROUP_E:
                gs2ReadGroupE(&row, state, xfact, yfact);
                break;
            case GROUP_F_1:
                if (nf == 0) croak("Group F is not supported when nf == 0");
                gs2ReadSubGroupF1(&row, state);
                break;
            case GROUP_F_2:
                if (nf == 0) croak("Group F is not supported when nf == 0");
                gs2ReadSubGroupF2(&row, state);
                break;
            case GROUP_G_1:
                gs2ReadSubGroupG1(&row, state);
                break;
            case GROUP_G_2:
                gs2ReadSubGroupG2(&row, state, aconci);
                break;
            case GROUP_H_1:
                gs2ReadSubGroupH1(&row, state);
                break;
            case GROUP_H_2:
                gs2ReadSubGroupH2(&row, state, &hone);
                break;
            case GROUP_H_3:
                gs2ReadSubGroupH3(&row, state, hone, ns, aphii);
                break;
            case GROUP_I:
                gs2ReadGroupI(&row, state, maxdif);
                break;
            case GROUP_J_1:
                gs2ReadSubGroupJ1(&row, state, &i1, &i2, &itype);
                break;
            case GROUP_J_2:
                gs2ReadSubGroupJ2(
                    &row, state, i1, i2, itype, afmobx, afmoby,
                    aelong, aetran, apor, ateta, aal, akd, alam, arho
                );
                if (row->next != CSV_NULL_ROW_PTR && gs2GetGroup(row->next, GROUP_J_2) != GROUP_J_1 && gs2GetGroup(row->next, GROUP_J_2) != GROUP_J_2)
                    gs2FinalizeGroupJ(&row, state);
                break;
            case GROUP_K:
                gs2ReadGroupK(&row, state, ns);
                break;
            case GROUP_L:
                gs2ReadGroupL(&row, state, kns);
                break;
            default:
                break;
        };
        row = row->next;
    } while (row != CSV_NULL_ROW_PTR);



    // fprintf(stdout, "Dirichlet boundary nodes for flow\n");
    // for (int i = 0; i < state->nn; i++) {
    //     state->lr.elements[i] = 0;
    //     state->klr.elements[i] = 0;
    // }

    // if (ns != 0)
    //     gs2BoundaryCondition(&(state->lr), &ns, 1, state->nn, state->memoryRequirements.maxnn, &(state->istop));

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
    fprintf(stdout, "\tMaximum number of nodes per element %d:\n", state->inc);

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
    fprintf(stdout, "\tMultipler for increasing time step: %lf\n", state->chng);
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

void gs2ReadGroupC(
    CSVRow** csvRow,
    gs2State* state,
    double* afmobx,
    double* afmoby,
    double* apor,
    double* aelong,
    double* aetrans,
    double* aphii,
    double* aconci,
    double* xfact,
    double* yfact,
    double* ateta,
    double* aal,
    double* akd,
    double* alam,
    double* arho
) {
    // card 1: group, afmobx, afmoby, apor, aelong, aetrans, aphii, aconci, xfact
    if ((*csvRow)->entryCount < 9)
        croak("Group C, Card 1 too few entries");
    
    sscanf((*csvRow)->entries[1], "%lf", afmobx);
    sscanf((*csvRow)->entries[2], "%lf", afmoby);
    sscanf((*csvRow)->entries[3], "%lf", apor);
    sscanf((*csvRow)->entries[4], "%lf", aelong);
    sscanf((*csvRow)->entries[5], "%lf", aetrans);
    sscanf((*csvRow)->entries[6], "%lf", aphii);
    sscanf((*csvRow)->entries[7], "%lf", aconci);
    sscanf((*csvRow)->entries[8], "%lf", xfact);

    // card 2: group, yfact, ateta, aal, akd, alam, arho
    *csvRow = (*csvRow)->next;
    if ((*csvRow)->entryCount < 7)
        croak("Group C, Card 2 too few entries");

    sscanf((*csvRow)->entries[1], "%lf", yfact);
    sscanf((*csvRow)->entries[2], "%lf", ateta);
    sscanf((*csvRow)->entries[3], "%lf", aal);
    sscanf((*csvRow)->entries[4], "%lf", akd);
    sscanf((*csvRow)->entries[5], "%lf", alam);
    sscanf((*csvRow)->entries[6], "%lf", arho);

    fprintf(stdout, "Parameter Multipliers:\n");
    fprintf(stdout, "\tafombx: %lf\n", *afmobx);
    fprintf(stdout, "\tafomby: %lf\n", *afmoby);
    fprintf(stdout, "\tapor: %lf\n", *apor);
    fprintf(stdout, "\taelong: %lf\n", *aelong);
    fprintf(stdout, "\taetrans: %lf\n", *aetrans);

    fprintf(stdout, "\taphii: %lf\n", *aphii);
    fprintf(stdout, "\taconci: %lf\n", *aconci);
    fprintf(stdout, "\txfact: %lf\n", *xfact);
    fprintf(stdout, "\tyfact: %lf\n", *yfact);
    fprintf(stdout, "\tateta: %lf\n", *ateta);

    fprintf(stdout, "\taal: %lf\n", *aal);
    fprintf(stdout, "\takd: %lf\n", *akd);
    fprintf(stdout, "\talam: %lf\n", *alam);
    fprintf(stdout, "\tarho: %lf\n", *arho);

}

void gs2ReadGroupD(CSVRow** csvRow, gs2State* state) {

    // card 1: group, kod1, kod2, kod3, kod4, kody, kod8, kod9, kod9, kod10, kod11, kod12
    if ((*csvRow)->entryCount < 11)
        croak("Group D, Card 1 too few entries");

    sscanf((*csvRow)->entries[1], "%d", &(state->kod1));
    sscanf((*csvRow)->entries[2], "%d", &(state->kod2));
    sscanf((*csvRow)->entries[3], "%d", &(state->kod3));
    sscanf((*csvRow)->entries[4], "%d", &(state->kod4));
    sscanf((*csvRow)->entries[5], "%d", &(state->kod7));
    sscanf((*csvRow)->entries[6], "%d", &(state->kod8));
    sscanf((*csvRow)->entries[7], "%d", &(state->kod9));
    sscanf((*csvRow)->entries[8], "%d", &(state->kod10));
    sscanf((*csvRow)->entries[9], "%d", &(state->kod11));
    sscanf((*csvRow)->entries[10], "%d", &(state->kod12));

    fprintf(stdout, "Print options: \n");
    fprintf(stdout, "\tkod1: %d\n", state->kod1);
    fprintf(stdout, "\tkod3: %d\n", state->kod3);
    fprintf(stdout, "\tkod4: %d\n", state->kod4);
    fprintf(stdout, "\tkod7: %d\n", state->kod7);
    fprintf(stdout, "\tkod8: %d\n", state->kod8);
    fprintf(stdout, "\tkod9: %d\n", state->kod9);
    fprintf(stdout, "\tkod10: %d\n", state->kod10);
    fprintf(stdout, "\tkod11: %d\n", state->kod11);
    fprintf(stdout, "\tkod12: %d\n", state->kod12);
}

void gs2ReadGroupE(CSVRow** csvRow, gs2State* state, double xfact, double yfact) {
    if ((*csvRow)->entryCount < 4) 
        croak("Group E, too few entries");
    
    // card: group, index, X[index], Y[index]
    
    int index;
    sscanf((*csvRow)->entries[1], "%d", &index);

    sscanf((*csvRow)->entries[2], "%lf", arrayAt(&(state->x), index));
    sscanf((*csvRow)->entries[3], "%lf", arrayAt(&(state->y), index));

    *arrayAt(&(state->x), index) *= xfact;
    *arrayAt(&(state->y), index) *= yfact;

    fprintf(
        stdout, 
        "Node at: %16.4lf, %16.4lf\n", 
        *arrayAt(&(state->x), index), 
        *arrayAt(&(state->y), index)
    );
}

void gs2ReadSubGroupF1(CSVRow** csvRow, gs2State* state) {
    if ((*csvRow)->entryCount < 9)
        croak("SubGroup F1, too few entries");

    int index[4];   
    sscanf((*csvRow)->entries[1], "%d", &index[0]);
    sscanf((*csvRow)->entries[2], "%lf", arrayAt(&(state->fq), index[0]));

    sscanf((*csvRow)->entries[3], "%d", &index[1]);
    sscanf((*csvRow)->entries[4], "%lf", arrayAt(&(state->fq), index[1]));

    sscanf((*csvRow)->entries[5], "%d", &index[2]);
    sscanf((*csvRow)->entries[6], "%lf", arrayAt(&(state->fq), index[2]));

    sscanf((*csvRow)->entries[7], "%d", &index[3]);
    sscanf((*csvRow)->entries[8], "%lf", arrayAt(&(state->fq), index[3]));

    for (int i = 0; i < 4; i++)
        fprintf(stdout, "Node %d source and sink discharge: %lf\n", index[i], *arrayAt(&(state->fq), index[i]));
}

void gs2ReadSubGroupF2(CSVRow** csvRow, gs2State* state) {
    if ((*csvRow)->entryCount < 9)
        croak("SubGroup F2, too few entries");

    int index[4];   
    sscanf((*csvRow)->entries[1], "%d", &index[0]);
    sscanf((*csvRow)->entries[2], "%lf", arrayAt(&(state->cfq), index[0]));

    sscanf((*csvRow)->entries[3], "%d", &index[1]);
    sscanf((*csvRow)->entries[4], "%lf", arrayAt(&(state->cfq), index[1]));

    sscanf((*csvRow)->entries[5], "%d", &index[2]);
    sscanf((*csvRow)->entries[6], "%lf", arrayAt(&(state->cfq), index[2]));

    sscanf((*csvRow)->entries[7], "%d", &index[3]);
    sscanf((*csvRow)->entries[8], "%lf", arrayAt(&(state->cfq), index[3]));

    for (int i = 0; i < 4; i++)
        fprintf(stdout, "Node %d source and sink concentration: %lf\n", index[i], *arrayAt(&(state->cfq), index[i]-1));
}

void gs2ReadSubGroupG1(CSVRow** csvRow, gs2State* state) {
    if ((*csvRow)->entryCount < 2)
        croak("Sub Group G1, too few entries!");   

    sscanf((*csvRow)->entries[1], "%lf", &(state->stime));
}

void gs2ReadSubGroupG2(CSVRow** csvRow, gs2State* state, double aconci) {
    if ((*csvRow)->entryCount < 9)
        croak("Sub Group G2, too few entries!");
    
    int index[4];   
    
    sscanf((*csvRow)->entries[1], "%d", &index[0]);
    sscanf((*csvRow)->entries[2], "%lf", arrayAt(&(state->conci), index[0]));

    sscanf((*csvRow)->entries[3], "%d", &index[1]);
    sscanf((*csvRow)->entries[4], "%lf", arrayAt(&(state->conci), index[1]));

    sscanf((*csvRow)->entries[5], "%d", &index[2]);
    sscanf((*csvRow)->entries[6], "%lf", arrayAt(&(state->conci), index[2]));

    sscanf((*csvRow)->entries[7], "%d", &index[3]);
    sscanf((*csvRow)->entries[8], "%lf", arrayAt(&(state->conci), index[3]));

    if (state->stime > 0.0) {
        for (int i = 0; i < 4; i++) {
            state->conci.elements[index[i]] *= aconci;
        }
    } 
}

void gs2ReadSubGroupH1(CSVRow** csvRow, gs2State* state) {
    if ((*csvRow)->entryCount < 2)
        croak("Sub Group H1, too few entries");
    
    sscanf((*csvRow)->entries[1], "%lf", &(state->stime));
}

void gs2ReadSubGroupH2(CSVRow** csvRow, gs2State* state, double* hone) {
    if ((*csvRow)->entryCount < 2)
        croak("Sub Group H2, too few entries");

    sscanf((*csvRow)->entries[1], "%lf", hone);
    fprintf(stdout, "Hone: %lf\n", *hone);

    if (*hone != 9999.0) {
        for (int i = 0; i < state->nn; i++) {
            state->phii.elements[i] = *hone - state->y.elements[i];
        }
    }
}

// the hone != 9999 is handled elsewhere
void gs2ReadSubGroupH3(CSVRow** csvRow, gs2State* state, double hone, int ns, double aphii) {
    if ((*csvRow)->entryCount < 9)
        croak("Sub Group H3 too few entries");

    int index[4];
    sscanf((*csvRow)->entries[1], "%d", &index[0]);
    sscanf((*csvRow)->entries[2], "%lf", arrayAt(&(state->phii), index[0]));

    sscanf((*csvRow)->entries[3], "%d", &index[1]);
    sscanf((*csvRow)->entries[4], "%lf", arrayAt(&(state->phii), index[1]));

    sscanf((*csvRow)->entries[5], "%d", &index[2]);
    sscanf((*csvRow)->entries[6], "%lf", arrayAt(&(state->phii), index[2]));

    sscanf((*csvRow)->entries[7], "%d", &index[3]);
    sscanf((*csvRow)->entries[8], "%lf", arrayAt(&(state->phii), index[3]));

    if (state->stime > 0.0) {
        for (int i = 0; i < 4; i++) {
            state->phii.elements[index[i]] *= aphii;
        }
    } 
}

void gs2ReadGroupI(CSVRow** csvRow, gs2State* state, double* maxdif) {
    do {
        // group I: group, element index, n1, n2, n3, n4, n5, n6, n7, n8, 9n, n10, n11, n12
      
        int elementIndex;
        sscanf((*csvRow)->entries[1], "%d", &elementIndex);
        

        int activeNodesForElement = 0;

        for (int i = 0; i < state->inc; i++) {
            // incidences start in the 3rd row
            int incident;
            sscanf((*csvRow)->entries[i + 2], "%d", &incident);
            *matrixAt(&(state->in), i+1, elementIndex) = (double)incident;

            if (incident != 0)
                activeNodesForElement++;
        }

        *matrixAt(&(state->in), state->me, elementIndex) = activeNodesForElement;

        *csvRow = (*csvRow)->next;
    } while (gs2GetGroup(*csvRow, GROUP_I) == GROUP_I);
    // the main datain func will move csvRow
    *csvRow = (*csvRow)->prev;

    fprintf(stdout, "Element Incidences:\n");
    fprintf(stdout, "\tElement\t\tMaximum Nodal Difference\tIncidences\n");

    Matrix* stateInRef = &(state->in);
    double nd, mnd = 0.0;

    for (int l = 1; l <= state->ne; l++) {
        int m = (int)(*matrixAt(stateInRef, state->me, l));
        int m1 = m - 1;
        mnd = 0.0;
        
        for (int i = 1; i <= m1; i++) {
            if (*matrixAt(stateInRef, i, l) == 0.0)
                break;
            
           
            for (int j = i + 1; j <= m; j++) {
                if (*matrixAt(stateInRef, j, l) == 0.0)
                    break;
                
                nd = abs(*matrixAt(stateInRef, i, l) - *matrixAt(stateInRef, j, l));
                // printf("l = %d, i = %d, j = %d, nd = %lf\n", l, i, j, nd);
                mnd = maxd(nd, mnd);
                *maxdif = maxd(nd, *maxdif);
                
            } // 202
        } // 205

        fprintf(stdout, "\t%d\t\t%lf\t\t\t", l, mnd);
        for (int i = 1; i <= state->inc; i++) 
            fprintf(stdout, "%d  ", (int)(*matrixAt(stateInRef, i, l)));
        fprintf(stdout, "\n");
    }  // 210

    nd = (*maxdif) + 1;
    if (nd - state->nb < 0.0) {
        state->nb = nd;
    } else if (nd - state->nb > 0.0) {
        warnf("Warning: maxium half-bandwidth exceeds space provided. nd = %lf, nb = %lf\n", nd, state->nb);
    }

    if (nd - state->knb < 0.0) {
        state->knb = nd;
    } else if (nd - state->knb > 0.0) {
        warnf("Warning: maximum half-bandwidth greater than estimate. nd = %lf, knb = %lf\n", nd, state->knb);
    }

    if (state->nb > state->memoryRequirements.maxbw || state->knb > state->memoryRequirements.maxbw) {
        croakf(
            "Error: maximum half-bandwidth greater than estimate. nd = %lf, knb = %lf, maxbw = %lf", 
             nd, 
             state->knb, 
             state->memoryRequirements.maxbw
        );
        state->istop++;
    }

    state->mb = 0;
    state->mb2 = 0;
    state->kmb = 0;
    state->kmb2 = 0;

    fprintf(stdout, "Half-bandwidth for pressure %d\n", state->nb);
    fprintf(stdout, "Half-bandwidth for concentration %d\n", state->knb);
}

void gs2ReadSubGroupJ1(CSVRow** csvRow, gs2State* state, int* i1, int* i2, int* itype) {
    // card 1: group, i1, i2, itype
    if ((*csvRow)->entryCount < 4)
        croak("Sub Group J1, too few entries");
    
    sscanf((*csvRow)->entries[1], "%d", i1);
    sscanf((*csvRow)->entries[1], "%d", i2);
    sscanf((*csvRow)->entries[1], "%d", itype);
}

void gs2ReadSubGroupJ2(
    CSVRow** csvRow, 
    gs2State* state, 
    int i1, 
    int i2, 
    int itype,
    double afmobx,
    double afmoby,
    double aelong,
    double aetran,
    double apor,
    double ateta,
    double aal,
    double akd,
    double alam,
    double arho
) {

    double tx, ty, dsl, dst, heta, teta, al, dist, decay, dens;
    // card 1: group, fmobx(l), fmoby(l), elong(l), etrans(l), por(l), tta(l), alpha(l), kd(l)
    if ((*csvRow)->entryCount < 9)
        croak("Sub Group J2, too few entries");

    sscanf((*csvRow)->entries[1], "%lf", &tx);
    sscanf((*csvRow)->entries[2], "%lf", &ty);
    sscanf((*csvRow)->entries[3], "%lf", &dsl);
    sscanf((*csvRow)->entries[4], "%lf", &dst);
    sscanf((*csvRow)->entries[5], "%lf", &heta);
    sscanf((*csvRow)->entries[6], "%lf", &teta);
    sscanf((*csvRow)->entries[7], "%lf", &al);
    sscanf((*csvRow)->entries[8], "%lf", &dist);

    // card 2: group, lambda(l), rho(l)
    *csvRow = (*csvRow)->next;
    if ((*csvRow)->entryCount < 3)
        croak("Sub Group J2, too few entries");
   
    sscanf((*csvRow)->entries[1], "%lf", &decay);
    sscanf((*csvRow)->entries[2], "%lf", &dens);

    for (int i = i1; i <= i2; i++) {
        *arrayAt(&(state->fmobx), i) = tx * afmobx;
        *arrayAt(&(state->fmoby), i) = ty * afmoby;
        *arrayAt(&(state->elong), i) = dsl * aelong;
        *arrayAt(&(state->etrans), i) = dst * aetran;
        *arrayAt(&(state->por), i) = heta * apor;
        *arrayAt(&(state->tta), i) = teta * ateta;
        *arrayAt(&(state->alpha), i) = al * aal;
        *arrayAt(&(state->kd), i) = akd * dist;
        *arrayAt(&(state->lambda), i) = alam * dist;
        *arrayAt(&(state->rho), i) = arho * dens;

        *matrixAt(&(state->ie), 2, i) = itype;

    }
}

void gs2FinalizeGroupJ(CSVRow** csvRow, gs2State* state) {
    for (int n = 0; n < state->nk; n++) {
        int k = 0;
        int ll = 0;
        for (int l = 1; l <= state->ne; l++) {
            if ((int)(*matrixAt(&(state->ie), 1, l)) != n)
                continue;
            
            
            *arrayAt(&(state->lr), k) = l;
            k++;
            ll = l;
        }

        fprintf(stdout, "Dispersivity\n");
        fprintf(stdout, "\tX-Mobility: %lf\n", *arrayAt(&(state->fmobx), ll));
        fprintf(stdout, "\tY-Mobility: %lf\n", *arrayAt(&(state->fmoby), ll));
        fprintf(stdout, "\tPorosity: %lf\n", *arrayAt(&(state->por), ll));
        fprintf(stdout, "\tLongitudinal Transverse: %lf, %lf\n", *arrayAt(&(state->elong), ll), *arrayAt(&(state->etrans), ll));
        fprintf(stdout, "\tMoisture: %lf\n", *arrayAt(&(state->tta), ll));
        fprintf(stdout, "\tCompressibliity: %lf\n", *arrayAt(&(state->alpha), ll));
        fprintf(stdout, "\tDistributivity: %lf\n", *arrayAt(&(state->kd), ll));
        fprintf(stdout, "\tDecay Constant: %lf\n", *arrayAt(&(state->lambda), ll));
        fprintf(stdout, "\tDensity: %lf\n", *arrayAt(&(state->rho), ll));

        fprintf(stdout, "\tValid for Elements: \n\t");
        for (int i = 0; i < k; i++) {
            fprintf(stdout, "%d\t", (int)(*arrayAt(&(state->lr), i)));
            if ((i+1) % 5 == 0)
                fprintf(stdout, "\n\t");
        } 
        fprintf(stdout, "\n");
    }
}

void gs2ReadGroupK(CSVRow** csvRow, gs2State* state, int ns) {
    if (ns == 0)
        return;

    fprintf(stdout, "Dirichlet Boundary Nodes for Flow:\n");
    Array lrt;
    arrayDimension(&lrt, 20);

    do {
        for (int i = 1; i < (*csvRow)->entryCount; i++) {
            int node = 0;
            sscanf((*csvRow)->entries[i], "%d", &node);
            *arrayAt(&lrt, i) = (double)node;
        }

        gs2BoundaryCondition(
            &(state->lr), 
            &lrt, 
            ns, 
            1, 
            state->nn, 
            &(state->istop)
        );
        
        *csvRow = (*csvRow)->next;
    } while (gs2GetGroup(*csvRow, GROUP_K) == GROUP_K);

    *csvRow = (*csvRow)->prev;
    arrayFree(&lrt);
}

void gs2ReadGroupL(CSVRow** csvRow, gs2State* state, int kns) {
    if (kns == 0)
        return;

    fprintf(stdout, "Dirichlet Boundary Nodes for Concentration:\n");
    Array lrt;
    arrayDimension(&lrt, 20);

    do {
        for (int i = 1; i < (*csvRow)->entryCount; i++) {
            int node = 0;
            sscanf((*csvRow)->entries[i], "%d", &node);
            *arrayAt(&lrt, i) = (double)node;
        }

        gs2BoundaryCondition(
            &(state->klr), 
            &lrt, 
            kns, 
            1, 
            state->nn, 
            &(state->istop)
        );
        
        *csvRow = (*csvRow)->next;
    } while (gs2GetGroup(*csvRow, GROUP_L) == GROUP_L);

    *csvRow = (*csvRow)->prev;
    arrayFree(&lrt);
}

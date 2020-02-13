#ifndef __CAP_DATAIN_H__
#define __CAP_DATAIN_H__

#include "gs2.h"
#include "../capstone/CSVFile.h"

typedef enum gs2DataGroup {
    GROUP_A,
    GROUP_B,
    GROUP_C,
    GROUP_D,
    GROUP_E,
    GROUP_F_1,
    GROUP_F_2,
    GROUP_G_1,
    GROUP_G_2,
    GROUP_H_1,
    GROUP_H_2,
    GROUP_H_3,
    GROUP_I,
    GROUP_J_1,
    GROUP_J_2,
    GROUP_K,
    GROUP_L,
    GROUP_M_1,
    GROUP_M_2,
    GROUP_M_3,
    GROUP_M_4,
    GROUP_N_1,
    GROUP_N_2,
    GROUP_O_1,
    GROUP_O_2,
    GROUP_O_3,
    GROUP_P,
    GROUP_Q_1,
    GROUP_Q_2,
    GROUP_Q_3,
    GROUP_Q_4,
    GROUP_R,
    NUM_DATA_GROUP
} gs2DataGroup;

extern const char* gs2DataGroupNames[NUM_DATA_GROUP];

void gs2Datain(
    gs2State* state, 
    const char* csvPath,
    Array* old,
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
);

gs2DataGroup gs2GetGroup(CSVRow* csvRow, gs2DataGroup defaultTo);

/**
 * Group A - Problem Title
 * Reads and prints the problem title
 */
void gs2ReadGroupA(CSVRow** csvRow, gs2State* state);

/**
 * Group B - Basic Parameters
 * 
 * reads four rows from the csv
 *
 */
void gs2ReadGroupB( 
    CSVRow** csvRow, 
    gs2State* state, 
    int* ns, 
    int* kns, 
    int* nf, 
    int* mq4,
    int* knsdn,
    int* nvs
);

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
);

void gs2ReadGroupD(
    CSVRow** csvRow,
    gs2State* state
);

void gs2ReadGroupE(
    CSVRow** csvRow,
    gs2State* state,
    double xfact,
    double yfact
);


// this function differs from the fortran source
// however, i believe it does the same thing
void gs2ReadSubGroupF1(
    CSVRow** csvRow,
    gs2State* state
);

// this function differs from the fortran source
// however, i believe it does the same thing
void gs2ReadSubGroupF2(
    CSVRow** csvRow,
    gs2State* state
);

void gs2ReadSubGroupG1(
    CSVRow** csvRow,
    gs2State* state
);

void gs2ReadSubGroupG2(
    CSVRow** csvRow,
    gs2State* state,
    double aconci
);

void gs2ReadSubGroupH1(
    CSVRow** csvRow,
    gs2State* state
);

void gs2ReadSubGroupH2(
    CSVRow** csvRow,
    gs2State* state,
    double* hone
);

void gs2ReadSubGroupH3(
    CSVRow** csvRow,
    gs2State* state,
    double hone,
    int ns,
    double aphii
);

void gs2ReadGroupI(
    CSVRow** csvRow,
    gs2State* state,
    double* maxdif
);

void gs2ReadSubGroupJ1(
    CSVRow** csvRow,
    gs2State* state,
    int* i1, 
    int* i2,
    int* itype
);

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
);

// called when subgroup j-2 is the sentinal value described in CH5
void gs2FinalizeGroupJ(
    CSVRow** csvRow, 
    gs2State* state
);

void gs2ReadGroupK(
    CSVRow** csvRow,
    gs2State* state,
    int ns
);

void gs2ReadGroupL(
    CSVRow** csvRow, 
    gs2State* state, 
    int kns
);

void gs2FinalizeGroupsK_L(
    CSVRow** csvRow, 
    gs2State* state,
    int ns,
    int kns
);

void gs2ReadSubGroupM1(
    CSVRow** csvRow,
    gs2State* state,
    int mq4
);


void gs2ReadSubGroupM2(
    CSVRow** csvRow,
    gs2State* state,
    int mq4
);

void gs2ReadSubGroupM3(
    CSVRow** csvRow,
    gs2State* state,
    Array* nsf, 
    Array* coef
);

void gs2ReadSubGroupM4(
    CSVRow** csvRow,
    gs2State* state,
    Array* nsf, 
    Array* vn
);

void gs2FinalizeGroupM(
    gs2State* state,
    Array* nsf, 
    Array* coef,
    Array* vn
);

void gs2ReadSubGroupN1(
    CSVRow** csvRow,
    gs2State* state,
    int knsdn
);

void gs2ReadSubGroupN2(
    CSVRow** csvRow,
    gs2State* state,
    Array* nsk,
    Array* cn,
    int knsdn
);

void gs2ReadGroupO(
    CSVRow** csvRow,
    gs2State* state,
    Array* msp,
    Matrix* nsp
);

void gs2ReadGroupP(
    CSVRow** csvRow,
    gs2State* state
);

void gs2ReadGroupQ(
    CSVRow** csvRow,
    gs2State* state,
    Array* wxpsi,
    Array* wxm,
    Array* wxk,
    Matrix* cc
);

void gs2ReadSubGroupQ2(
    CSVRow** csvRow,
    gs2State* state,
    int material,
    int count
);

void gs2ReadSubGroupQ3(
    CSVRow** csvRow,
    gs2State* state,
    int material,
    int count
);

void gs2ReadSubGroupQ4(
    CSVRow** csvRow,
    gs2State* state,
    int material,
    int count
);

void gs2ReadGroupR(
    CSVRow** csvRow,
    gs2State* state
);

#endif /* __CAP_DATAIN_H__ */

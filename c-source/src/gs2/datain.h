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

#endif /* __CAP_DATAIN_H__ */
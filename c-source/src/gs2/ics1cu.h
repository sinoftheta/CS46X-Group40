#ifndef __CAP_ICS1CU_H__
#define __CAP_ICS1CU_H__

#include "../capstone/Array.h"
#include "../capstone/Matrix.h"


void gs2ICS1CU(
    Array* gs2xs, 
    Array* gs2ys, 
    int dataPointCount, 
    int coefficentCount,
    Matrix* coefficentMatrix,
    int* ier
);

#endif /* __CAP_ICS1CU_H__ */ 

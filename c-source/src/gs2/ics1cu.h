#ifndef __CAP_ICS1CU_H__
#define __CAP_ICS1CU_H__

#include "../capstone/Array.h"
#include "../capstone/Matrix.h"


void gs2ICS1CU(
    Array* xs, 
    Array* ys, 
    int ispk, 
    Matrix* cc,
    int material,
    int* ier
);

#endif /* __CAP_ICS1CU_H__ */ 
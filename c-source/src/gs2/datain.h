#ifndef __CAP_DATAIN_H__
#define __CAP_DATAIN_H__

#include "gs2.h"

typedef enum gs2DataGroup {


} gs2DataGroup;

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

#endif /* __CAP_DATAIN_H__ */
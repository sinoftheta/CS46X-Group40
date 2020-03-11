#ifndef __GS2_PO_H__
#define __GS2_PO_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "gs2.h"

void gs2Po(gs2State* state, Array* conc, Array* phi, int nn, int it, double stime, char* rdate, char* rtime);

#endif /* __GS2_PO_H__ */

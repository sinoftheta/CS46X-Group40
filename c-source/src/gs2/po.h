#ifndef __GS2_PO_H__
#define __GS2_PO_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "gs2.h"

/*
 * Purpose:
    To generate restart data files
    Pressures are written to TAPE7
    Concentrations are written to TAPE8

    Called from TS
 */

void gs2Po(gs2State* state, Array* conc, Array* phi, int nn, int it, double stime, char* rdate, char* rtime);

#endif /* __GS2_PO_H__ */

#ifndef __GS2_LRHS_H__
#define __GS2_LRHS_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "../capstone/MathUtil.h"

/*
  Purpose:
    Used to form the right-hand side of the flow and mass transport
    equations.

    Called from TS
*/

void gs2Lrhs(Matrix* a, Matrix* b, Array* r, Array* rold,
          Array* u, Array* lq, int m, int ib, int jb, double a3,
          double a2, int kk);


#endif /* __GS2_LRHS_H__ */

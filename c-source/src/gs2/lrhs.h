#ifndef __GS2_LRHS_H__
#define __GS2_LRHS_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "../capstone/MathUtil.h"

void gs2Lrhs(Matrix* a, Matrix* b, Array* r, Array* T, Array* rold,
          Array* u, Array* lq, int m, int ib, int jb, double a3,
          double a3, int MAXNA, int MAXMA, int MAXNB, int MAXMB,
          int IR, int ILQ, int KK);


#endif /* __GS2_LRHS_H__ */

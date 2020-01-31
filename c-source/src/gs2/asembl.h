#ifndef __GS2_ASEMBL_H__
#define __GS2_ASEMBL_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "../capstone/MathUtil.h"

void gs2Asembl(Matrix* a, Matrix* b, Matrix* ea, Matrix* eb, Array* r,
              Array* u, Array* re, Array* F, Array* k0d, Array* lq,
              Array* jd, int m, int l, int ib, int ib2, int jb,
              int jb2, int* ISTOP);

#endif /* __GS2_ASEMBL_H__ */

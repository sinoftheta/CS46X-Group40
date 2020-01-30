#ifndef __GS2_MATGEN_H__
#define __GS2_MATGEN_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"

void gs2Matgen1(Matrix* pe, Matrix* se, Array* q, Array* qp, int i, int j);
void gs2Matgen2(Array* srcr, Array* srcrt, int i);
void gs2Matgen3(Matrix* pe, Matrix* se);

#endif /* __GS2_MATGEN_H__ */
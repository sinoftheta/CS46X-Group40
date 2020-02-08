#ifndef __GS2_MATGEN_H__
#define __GS2_MATGEN_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "gs2.h"

void gs2Matgen1(gs2State* state, Matrix* pe, Matrix* se, Array* q, Array* qp, int i, int j);
void gs2Matgen2(gs2State* state, Array* srcr, Array* srcrt, int i);
void gs2Matgen3(Matrix* pe, Matrix* se, int m, int l);

#endif /* __GS2_MATGEN_H__ */
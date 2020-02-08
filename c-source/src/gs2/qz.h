#ifndef __GS2_QZ_H__
#define __GS2_QZ_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "../capstone/MathUtil.h"
#include "shape.h"
#include "green.h"
#include "gs2.h"

void gs2Qz(gs2State* state, Array* u, Array* old, Array* phii, Array* x, Array* y, Array* fmobx, Array* fmoby, Matrix* f,
       Matrix* dx, Matrix* dy, Array* detj, Array* cphi, Array* vkx, Array* vky, Array* dgx, Array* dgy,
       Array* ff, Matrix* in, Array* kf, Array* jd, Array* ieq, Array* lc, Array* lr, int* ms, int* kase,
       int l, int m, int ik, int* ispk, int* ispm, double* psik, int* istop);

#endif /* __GS2_QZ_H__ */
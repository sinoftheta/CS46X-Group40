#ifndef __GS2_COGEN_H__
#define __GS2_COGEN_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "../capstone/MathUtil.h"
#include "gs2.h"
#include "shape.h"
#include "matgen.h"
#include "asembl.h"
#include "qz.h"
#include "sos.h"

void gs2Cogen(gs2State* state, Matrix* s, Matrix* p, Array* fm, Array* rt, Array* phi, Array* phii,
             Array* u, Array* old, Array* cfm, Array* crt, Array* conc, Array* conci, Array* fx,
             Array* cn, Array* est, Array* fq, Array* cfq, Array* x, Array* y, Array* fmobx,
             Array* fmoby, Array* por, Array* elong, Array* etrans, Array* alpha, Array* tta,
             Array* kd, Array* lambda, Array* rho, Matrix* in, Array* kf, Array* lr, Array* klr,
             Array* lc, Array* klc, Matrix* ie, int jtest);

#endif /* __GS2_COGEN_H__ */
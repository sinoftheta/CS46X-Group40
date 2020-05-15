#ifndef __GS2_TS_H__
#define __GS2_TS_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"
#include "../capstone/MathUtil.h"
#include "../capstone/CSVFile.h"
#include "../capstone/FileUtil.h"
#include "../capstone/Debug.h"
#include "gs2.h"
#include "upd.h"
#include "cogen.h"
#include "lrhs.h"
#include "zero.h"
#include "sos.h"
#include "dband.h"
#include "sband.h"
#include "array.h"
#include "gelb.h"
#include "po.h"

/*
 * Purpose:
    To formulate and solve flow and mass-transport equations
*/

void gs2Ts(gs2State* state, Matrix* s, Matrix* p, Array* w, Array* fm, Array* rt, Array* phi, Array* phii, Array* old,
           Array* cfm, Array* crt, Array* conc, Array* conci, Array* cold, Array* fx, Array* cn, Array* vn,
           Array* coef, Array* est, Array* u, Array* fq, Array* cfq, Array* x, Array* y, Array* fmobx,
           Array* fmoby, Array* elong, Array* etrans, Array* por, Array* alpha, Array* tta, Array* kd,
           Array* lambda, Array* rho, Matrix* in, Array* kf, Array* lr, Array* klr, Array* lc, Array* klc,
           Array* lp, Array* nsf, Matrix* ie, Matrix* nsp, Array* msp, char* rdate, char* rtime);

#endif /* __GS2_TS_H__ */

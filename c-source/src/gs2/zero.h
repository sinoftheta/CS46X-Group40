#ifndef __CAP_ZERO_H__
#define __CAP_ZERO_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"

/**
* Purpose:
    Applies Dirichlet boundary conditions during assembly of global
    coefficient matrix for flow.

    Called from TS.
 * @param ui - temporary storage index
 * @param neq - number of equations
 * @param ib - half boundary
 * @param n - equation index
 */
void gs2Zero(Matrix* a, Array* v, int ui, int neq, int ib, int n);


#endif /* __CAP_ZERO_H__ */

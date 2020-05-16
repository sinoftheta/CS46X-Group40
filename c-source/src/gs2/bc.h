#ifndef __CAP_BC_H__
#define __CAP_BC_H__

#include "../capstone/Array.h"

/**
    Purpose:
      Sets values for boundary condition nodes and boundary condition variables.

      Called from Datain

      Differs from fortran source. lrt will be initallized and filled prior
      to calling this function.
 */

void gs2BoundaryCondition(Array* lx, Array* lrt, int ln, double kbc, int neq, int* istop);


#endif /* __CAP_BC_H__ */

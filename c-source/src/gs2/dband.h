#ifndef __CAP_GS2_DBAND_H__
#define __CAP_GS2_DBAND_H__


#include "../capstone/Array.h"
#include "../capstone/Matrix.h"
#include "../capstone/MathUtil.h"

/*
    Purpose:
      Triangularize the flow equation prior to solution or sets ISTOP if
      the triangularization fails.

      Called from TS

*   @param s the matrix to traingularize
*   @param n iteration count
*   @param nb ???
*   @param iex iterator

*/
void gs2Dband(Matrix* s, int n, int nb, int* iex);

#endif /*__CAP_GS2_DBAND_H__*/

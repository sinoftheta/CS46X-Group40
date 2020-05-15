#ifndef __CAP_GS2_ARRAY_H__
#define __CAP_GS2_ARRAY_H__

#include "../capstone/Array.h"
#include "../capstone/Matrix.h"
#include "../capstone/MathUtil.h"

/*
    Purpose:
      Converts the 2-dimensional global coefficient matrix for mass transport, S,
      into its equivalent 1-dimensional matrix, W.

      Called from TS
*/

void gs2Array(Matrix*, Array*, int, int, int, int, int, int, int, int*);

#endif /*__CAP_GS2_ARRAY_H__*/

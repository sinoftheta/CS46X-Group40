#include "array.h"

#include "../capstone/Debug.h"

/*
    Purpose:
      Converts the 2-dimensional global coefficient matrix for mass transport, S,
      into its equivalent 1-dimensional matrix, W.

      Called from TS
*/

void gs2Array(
    Matrix* a, Array* z, int neq, int ib, int jb,
    int jb2, int mdim, int ndim, int mx, int* jx ){

    DEBUG_LOG("gs2Array");

    int j = 0, k, lim, k0, k1;

    for(int n = 1; n < neq; n++){
        k = ib - n + 1;
        lim = ib - n + neq;
        k0 = max(ib - jb + 1, k);
        k1 = min(jb2,lim);
        for(int m = k0; m < k1; m++){
            z->elements[j] = *matrixAt(a, m, n);
            j++;
        }
    }
    *jx = j;
}

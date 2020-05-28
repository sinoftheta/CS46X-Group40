#include "array.h"

#include "../capstone/Debug.h"


void gs2Array(
    Matrix* a, Array* z, int neq, int ib, int jb,
    int jb2, int mdim, int ndim, int mx, int* jx ){

    /*
     * This subroutine simply converts the two-dimensional global coefficient
     * matrix for mass transport, S, into its equivalent one-dimensional matrix, W.
     * It is called after the matrix has been assembled to set up for solution of
     * the mass transport equation
     * 
     * CALLED FROM: TS
     * SUBROUTINES CALLED: None
     */

    DEBUG_LOG("gs2Array");

    int j = 0, k, lim, k0, k1;

    for (int n = 1; n <= neq; n++){
        k = ib - n + 1;
        lim = ib - n + neq;
        k0 = max(ib - jb + 1, k);
        k1 = min(jb2, lim);
        for (int m = k0; m <= k1; m++) {
            j++;
            *arrayAt(z, j) = *matrixAt(a, m, n);
        }
    }
    *jx = j;
}

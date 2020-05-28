#include "zero.h"


void gs2Zero(Matrix* a, Array* v, int ui, int neq, int ib, int n) {
    matrixAssertNotNull(a, "matrix null in gs2Zero!");
    arrayAssertNotNull(v, "array null in gs2Zero!");

    for (int m = 2; m < ib; m++) {
        int k = n - m;
        if (k > 0) {
            *arrayAt(v, k) -= *matrixAt(a, k, m) * ui;
            *matrixAt(a, k, m) = 0.0;
        }

        k = n + m - 2;
        if (k <= neq) {
            *arrayAt(v, k) -= *matrixAt(a, n, m) * (double)ui;
            *matrixAt(a, n, m) = 0.0;
        }
    }

     *matrixAt(a, n, 0) = 1.0;
     *arrayAt(v, n) = (double)ui;
}

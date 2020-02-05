#include "matgen.h"
#include <stdio.h>

/*
 * Purpose: To perform surface integrations
 */

void gs2Matgen1(gs2State* state, Matrix* pe, Matrix* se, Array* q, Array* qp, int i, int j) {

    arrayAssertNotNull(q, "Array 'q' NULL in gs2Matgen!");
    arrayAssertNotNull(qp, "Array 'qp' NULL in gs2Matgen!");
    matrixAssertNotNull(pe, "Matrix 'pe' NULL in gs2Shape!");
    matrixAssertNotNull(se, "Matrix 'se' NULL in gs2Shape!");
    
    double h11, h12, h22;

    h11 = state->h1 * state->h1;
    h12 = state->h1 * state->h2;
    h22 = state->h2 * state->h2;

    if (state->np == 4) {
        *matrixAt(se, i, j) = h11 * (*arrayAt(q, 1) + *arrayAt(q, 4) + *arrayAt(q, 13) + *arrayAt(q, 16)) \
                            + h12 * (*arrayAt(q, 2) + *arrayAt(q, 3) + *arrayAt(q, 5) + *arrayAt(q, 8) + *arrayAt(q, 9) + *arrayAt(q, 12) + *arrayAt(q, 14) + *arrayAt(q, 15)) \
                            + h22 * (*arrayAt(q, 6) + *arrayAt(q, 7) + *arrayAt(q, 10) + *arrayAt(q, 11));
        *matrixAt(pe, i, j) = h11 * (*arrayAt(qp, 1) + *arrayAt(qp, 4) + *arrayAt(qp, 13) + *arrayAt(qp, 16)) \
                            + h12 * (*arrayAt(qp, 2) + *arrayAt(qp, 3) + *arrayAt(qp, 5) + *arrayAt(qp, 8) + *arrayAt(qp, 9) + *arrayAt(qp, 12) + *arrayAt(qp, 14) + *arrayAt(qp, 15)) \
                            + h22 * (*arrayAt(qp, 6) + *arrayAt(qp, 7) + *arrayAt(qp, 10) + *arrayAt(qp, 11));
    } else {
        *matrixAt(se, i, j) = *arrayAt(q, 1) + *arrayAt(q, 2) + *arrayAt(q, 3) + *arrayAt(q, 4);
        *matrixAt(pe, i, j) = *arrayAt(qp, 1) + *arrayAt(qp, 2) + *arrayAt(qp, 3) + *arrayAt(qp, 4);
    }

}

void gs2Matgen2(gs2State* state, Array* srcr, Array* srcrt, int i) {

    arrayAssertNotNull(srcr, "Array 'srcr' NULL in gs2Matgen!");
    arrayAssertNotNull(srcrt, "Array 'srcrt' NULL in gs2Matgen!");

    double h11, h12, h22;

    h11 = state->h1 * state->h1;
    h12 = state->h1 * state->h2;
    h22 = state->h2 * state->h2;

    if (state->np == 4) {
        *arrayAt(srcr, i) = h11 * (*arrayAt(srcrt, 1) + *arrayAt(srcrt, 4) + *arrayAt(srcrt, 13) + *arrayAt(srcrt, 16)) \
                          + h12 * (*arrayAt(srcrt, 2) + *arrayAt(srcrt, 3) + *arrayAt(srcrt, 5) + *arrayAt(srcrt, 8) + *arrayAt(srcrt, 9) + *arrayAt(srcrt, 12) + *arrayAt(srcrt, 14) + *arrayAt(srcrt, 15)) \
                          + h22 * (*arrayAt(srcrt, 6) + *arrayAt(srcrt, 7) + *arrayAt(srcrt, 10) + *arrayAt(srcrt, 11));
    } else {
        *arrayAt(srcr, i) = *arrayAt(srcrt, 1) + *arrayAt(srcrt, 2) + *arrayAt(srcrt, 3) + *arrayAt(srcrt, 4);
    }
}

void gs2Matgen3(Matrix* pe, Matrix* se, int m, int l) {

    matrixAssertNotNull(pe, "Matrix 'pe' NULL in gs2Shape!");
    matrixAssertNotNull(se, "Matrix 'se' NULL in gs2Shape!");

    int mm2, k1, k2, im, jm;

    printf("\n\n\n\n          ELEMENT%4d     STIFFNESS MATRIX\n\n", l);

    mm2 = (m+7)/8*8 - 7;

    for (k1 = 1; k1 <= mm2; k1 += 8) {
        k2 = k1 + 7;
        if (k1 == mm2) {
            k2 = m;
        }
        for (im = 1; im <= m; im++) {
            printf("%5d", im);
            for (jm = k1; jm <= k2; jm++) {
                printf("%15.6f", *matrixAt(se, im, jm));
            }
            printf("\n");
        }
        printf("\n\n\n");
    }

    printf("\n\n\n\n          ELEMENT%4d     STORAGE MATRIX\n\n", l);

    for (k1 = 1; k1 <= mm2; k1 += 8) {
        k2 = k1 + 7;
        if (k1 == mm2) {
            k2 = m;
        }
        for (im = 1; im <= m; im++) {
            printf("%5d", im);
            for (jm = k1; jm <= k2; jm++) {
                printf("%15.6f", *matrixAt(pe, im, jm));
            }
            printf("\n");
        }
        printf("\n\n\n");
    }
}
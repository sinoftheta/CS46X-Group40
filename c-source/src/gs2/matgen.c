#include "matgen.h"

/*
 * Purpose: To perform surface integrations
 */

void matgen1(Matrix* pe, Matrix* se, Array* q, Array* qp, int i, int j) {
    
    /* 
     * Global variables:
     * - h1
     * - h2
     * - np
     */
    
    double h11, h12, h22;

    h11 = h1 * h1;
    h12 = h1 * h2;
    h22 = h2 * h2;

    if (np == 4) {
        *matrixAt(se, i, j) = h11 * (*arrayAt(q, 0) + *arrayAt(q, 3) + *arrayAt(q, 12) + *arrayAt(q, 15)) \
                            + h12 * (*arrayAt(q, 1) + *arrayAt(q, 2) + *arrayAt(q, 4) + *arrayAt(q, 7) + *arrayAt(q, 8) + *arrayAt(q, 11) + *arrayAt(q, 13) + *arrayAt(q, 14)) \
                            + h22 * (*arrayAt(q, 5) + *arrayAt(q, 6) + *arrayAt(q, 9) + *arrayAt(q, 10));
        *matrixAt(pe, i, j) = h11 * (*arrayAt(qp, 0) + *arrayAt(qp, 3) + *arrayAt(qp, 12) + *arrayAt(qp, 15)) \
                            + h12 * (*arrayAt(qp, 1) + *arrayAt(qp, 2) + *arrayAt(qp, 4) + *arrayAt(qp, 7) + *arrayAt(qp, 8) + *arrayAt(qp, 11) + *arrayAt(qp, 13) + *arrayAt(qp, 14)) \
                            + h22 * (*arrayAt(qp, 5) + *arrayAt(qp, 6) + *arrayAt(qp, 9) + *arrayAt(qp, 10));
    } else {
        *matrixAt(se, i, j) = *arrayAt(q, 0) + *arrayAt(q, 1) + *arrayAt(q, 2) + *arrayAt(q, 3);
        *matrixAt(pe, i, j) = *arrayAt(qp, 0) + *arrayAt(qp, 1) + *arrayAt(qp, 2) + *arrayAt(qp, 3);
    }

}

void matgen2(Array* srcr, Array* srcrt, int i) {

    /* 
     * Global variables:
     * - h1
     * - h2
     * - np
     */

    double h11, h12, h22;

    h11 = h1 * h1;
    h12 = h1 * h2;
    h22 = h2 * h2;

    if (np == 4) {
        *arrayAt(srcr, i) = h11 * (*arrayAt(srcrt, 0) + *arrayAt(srcrt, 3) + *arrayAt(srcrt, 12) + *arrayAt(srcrt, 15)) \
                          + h12 * (*arrayAt(srcrt, 1) + *arrayAt(srcrt, 2) + *arrayAt(srcrt, 4) + *arrayAt(srcrt, 7) + *arrayAt(srcrt, 8) + *arrayAt(srcrt, 11) + *arrayAt(srcrt, 13) + *arrayAt(srcrt, 14)) \
                          + h22 * (*arrayAt(srcrt, 5) + *arrayAt(srcrt, 6) + *arrayAt(srcrt, 9) + *arrayAt(srcrt, 10));
    } else {
        *arrayAt(srcr, i) = *arrayAt(srcrt, 0) + *arrayAt(srcrt, 1) + *arrayAt(srcrt, 2) + *arrayAt(srcrt, 3);
    }
}

void matgen3(Matrix* pe, Matrix* se) {

    /* 
     * Global variables:
     * - m
     */

    int mm2, k1, k2, im, jm;

    printf("\n\n\n\n          ELEMENT%4d     STIFFNESS MATRIX\n", l);

    mm2 = (m+7)/8*8 - 7;

    for (k1 = 0; k1 < mm2; k1 += 8) {
        k2 = k1 + 7;
        if (k1 == mm2) {
            k2 = m;
        }
        for (im = 0; im < m; im++) {
            printf("%d", im);
            for (jm = k1; jm <= k2; jm++) {
                printf("%15.6f", *matrixAt(se, im, jm));
            }
            printf("\n\n");
        }
    }

    printf("\n\n\n\n          ELEMENT%4d     STORAGE MATRIX\n", l);

    for (k1 = 0; k1 < mm2; k1 += 8) {
        k2 = k1 + 7;
        if (k1 == mm2) {
            k2 = m;
        }
        for (im = 0; im < m; im++) {
            printf("%d", im);
            for (jm = k1; jm <= k2; jm++) {
                printf("%15.6f", *matrixAt(pe, im, jm));
            }
            printf("\n\n");
        }
    }


}
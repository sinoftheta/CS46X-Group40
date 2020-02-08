#include "sos.h"
#include <stdio.h>

void gs2Sos(Matrix* a, int neq, int iband, int istart) {

    matrixAssertNotNull(a, "Matrix 'a' NULL in gs2Sos!");

    int nb2, k1, k2, i, j;
    nb2 = (iband + 9) / 10 * 10 - 9;

    for (k1 = 1; k1 <= nb2; k1++) {
        k2 = k1 + 9;
        if (k1 == nb2) {
            k2 = iband;
        }
        for (i = istart; i <= neq; i++) {
            printf("%5d", i);
            for (j = k1; j <= k2; j++) {
                printf("%12.4f", *matrixAt(a, i, j)));
            }
            printf("\n");
        }
        printf("\n\n\n");
    }
}
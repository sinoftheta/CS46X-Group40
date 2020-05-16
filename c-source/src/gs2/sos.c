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
            fprintf(gs2stdout, "%5d", i);
            for (j = k1; j <= k2; j++) {
                fprintf(gs2stdout, "%12.4E", *matrixAt(a, i, j));
            }
            fprintf(gs2stdout, "\n");
        }
        fprintf(gs2stdout, "\n\n\n");
    }
}

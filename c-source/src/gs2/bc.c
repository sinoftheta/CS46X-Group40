#include "bc.h"
#include "gs2.h"


#include <stdio.h>

void gs2BoundaryCondition(Array* lx, Array* lrt, int ln, double kbc, int neq, int* istop) {
    arrayAssertNotNull(lx, "lx NULL in gs2BoundaryCondition");
    arrayAssertNotNull(lrt, "lrt NULL in gs2BoundaryCondition");

    int nst = 0;
    int ia = 0;
    int j = 0;

    while (nst < ln) {
        ia = 0;
        for (int i = 1; i <= 20; i++) {
            if (*arrayAt(lrt, i) == 0.0)
                break;
            
            ia = i;
            j = (int)(*arrayAt(lrt, i));
            nst++;

            if (j <= neq) {
                *arrayAt(lx, j) = kbc;
            } else {
                fprintf(gs2stderr, "Boundary node %d does not exist\n", j);
                return;
            }
        }

        for (int i = 0; i < lrt->size; i++) {
            if (lrt->elements[i] == 0.0)
                break;
            fprintf(gs2stdout, "%4d ", (int)(lrt->elements[i]));
        }
            
        fprintf(gs2stdout, "\n");

        if (ia != 20) 
            break;
    }


    if (nst == ln)
        return;

    fprintf(
        gs2stderr,
        "Number of boundary nodes read %d, disagrees with number anitipated %d.\n",
        nst, 
        ln
    );

    *istop += 1;
}
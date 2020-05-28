#include "sband.h"


void gs2Sband(Matrix* s, Array* p, Array* u, int n, int nb) {

    /*
     * The flow equation is solved, after triangularization by DBAND, in SBAND.
     * Back substitution is used to obtain a solution.
     * 
     * CALLED FROM: TS
     * SUBROUTINES CALLED: None
     */

    matrixAssertNotNull(s, "matrix s in gs2Sband is null");
    arrayAssertNotNull(p, "array p in gs2Sband is null");
    arrayAssertNotNull(u, "array u in gs2Sband is null");

    for (int i = 1; i <= n; i++){

        int j = i - nb + 1;
        if((i  + 1) <= nb)
            j = 1;

        double sum = *arrayAt(p, i);
        int k1 = i - 1;

        if (j <= k1) {

            for (int k = j; k <= k1; k++){

                int ii = i - k + 1;
                double ski = *matrixAt(s, k, ii);

                //10
                if(ski != 0.0)
                    sum = sum - ski * *arrayAt(u, k);
            }
        }
        //20
        *arrayAt(u, i) = sum* *matrixAt(s, i, 1);
    }//30

    for (int i1 = 1; i1 <= n; i1++){

        int i = n - i1 + 1;
        int j = i + nb - 1;

        if(j > n)
            j = n;

        double sum = *arrayAt(u, i);
        int k2 = i + 1;
        
        if (k2 <= j){ 

            for (int k = k2; k <= j; k++){
                
                int kk = k - i + 1;
                double ski = *matrixAt(s, i, kk);
                if(ski != 0.0)
                    sum = sum - ski * *arrayAt(u, k);

            }
        }
        //50
        *arrayAt(u, i) = sum * *matrixAt(s, i, 1);
    }//60
}

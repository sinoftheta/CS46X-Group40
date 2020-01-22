#include "sband.h"

void gs2Sband(Matrix* s, Array* p, Array* u, int n, int nb, int ndim, int mdim, int nu){

    int j, k1, ii;
    double sum, ski;

    for(int i = 1; i <= n; i++){

        j = i - nb + 1;
        if((i  + 1) <= nb) j = 1;
        sum = *arrayAt(p, i);
        k1 = i - 1;
            if(!(j > k1)){ // if(j > k1) go to 20

                for(int k = j; k <= k1; k++){

                    ii = i - k + 1;
                    ski = *matrixAt(s, k, ii);

                    //10
                    if(ski != 0.0) sum = sum - ski * *arrayAt(u, k);
                }
            }
            //20
            
            *arrayAt(u, i) = sum* *matrixAt(s, i, 1);

    }

}
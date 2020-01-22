#include "sband.h"

void gs2Sband(Matrix* s, Array* p, Array* u, int n, int nb, int ndim, int mdim, int nu){

    int j, k1, k2, ii, kk;
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

            //30
            for(int i1 = 1; i1 <= n; i1++){

            i = n - i1 + 1;
            j = i + nb - 1;

            if(j > n) j = n;

            sum = *arrayAt(u, i);
            k2 = i + 1;
            
            if(!(k2 > j)){ // if(k2 > j) go to 50

                for(int k = k2; k <= j; k++){

                    kk = k - i + 1;
                    ski = *matrixAt(s, i, kk);
                    if(ski != 0.0) sum = sum - ski * *arrayAt(u, k);
                    *arrayAt(u, i) = sum * *matrixAt(s, i, 1);
                }
            }
            //50

            //60
        }
    }
}
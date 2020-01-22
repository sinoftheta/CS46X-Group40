#include "array.h"

void gs2Array(
    Matrix* a, Array* z, int neq, int ib, int jb, 
    int jb2, int mdim, int ndim, int mx, int jx ){

    int j = 0, k, lim, k0, k1;

    for(int n = 1; n < neq; n++){
        k = ib - n + 1;
        lim = ib - n + neq;
        k0 = max(ib - jb + 1, k);
        k1 = min(jb2,lim);
            for(int m = k0; m < k1; m++){
                z->elements[j] = *matrixAt(a, m, n);
            }
    }
    jx = j;
}

int min(int a, int b){
    if(a > b) return b;
    else return a;
}

int max(int a, int b){
    if(a < b) return b;
    else return a;
}
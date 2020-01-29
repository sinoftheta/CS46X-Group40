#include "gelb.h"

//label 47, error return 
#define ERR {*ier = -1; return;}

void gs2Gelb(
    Array* r, Array* a, int m, int n, 
    int mud, int mld, int eps, int* ier
){
    double piv, tb, tol;
    int j, jj, kst, ic, idst, id, ilr, ii;

    arrayAssertNotNull(r, "array 'r' NULL in gs2Gleb");
    arrayAssertNotNull(a, "array 'a' NULL in gs2Gleb");

    //test wrong input params
    int mc, mu, ml, mr, mz, ma, nm;

    if(mld < 0 || mud < 0) ERR

    mc = 1 + mld + mud;

    if(mc + 1 - 2 * m > 0) ERR

    //prep integer params
    /*
    * mc: number of columns in matrix a
    * mu: number of zeros to be inserted in first row of matrix a
    * ml: number of missing elements in last row of matrix a
    * mr: index of last row in matrix a with mc elements
    * mz: total number of zeros to be inserted in matrix a
    * ma: total number of storage locations necessary for matrix a
    * nm: number of elements in matrix r
    */
    if(mc - m > 0) mc = m;
    mu = mc - mud - 1;
    ml = mc - mld - 1;
    mr = m - ml;
    mz = (mu * (mu + 1)) / 2;
    ma = m * mc - (ml * (ml + 1)) / 2;
    nm = n * m;

    //move elements backwars and search for absolutely greatest element

    ier = 0;
    piv = 0.0;
    if(mld <= 0){
        //6
        jj = ma;
        j - ma - mz;
        kst = j;
        for(int iter = 1; iter <= kst; ++iter){

            tb = *arrayAt(a, j);
            *arrayAt(a, jj) = tb;
            tb = abs(tb);
            if(tb - piv > 0) piv = tb;
            --jj;
            --j;
        }

        // insert zeros in first mu rows (not necessary in case mz = 0)
        if(mz <= 0){
            jj = 1;
            j = 1 + mz;
            ic = 1 + mud;

            for(int i = 1; i <= mu; ++i){
                for(int k = 1; k <= mc; ++k){
                    *arrayAt(a, jj) = 0.0;
                    if(k - ic <= 0 ){
                        *arrayAt(a, jj) = *arrayAt(a, j);
                        ++j;
                    }
                    ++jj;
                    ++ic;
                }
            }
        }
    }
    //14

    // generate test value for singularity
    tol = eps * piv;

    // start decomposition loop
    kst = 1;
    idst = mc;
    ic = mc - 1;

    for(int k = 0; k <= m; ++k){
        if(k - mr - 1 > 0 ) --idst;
        id = idst;
        ilr = k + mld;
        if(ilr - m > 0) ilr = m;
        ii = kst;

        // pivot search in first column (row indexes from i = k up to i = ilr)
        piv = 0.0;

        for(int i = k; i <= ilr; ++i){
            
        }
        // 22

    }
    // 38










}
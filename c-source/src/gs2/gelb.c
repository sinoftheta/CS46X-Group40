#include "gleb.h"

//label 47, error return 
#define ERR {*ier = -1; return;}

void gs2Gelb(
    Array* r, Array* a, int m, int n, 
    int mud, int mld, int eps, int* ier
){

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

    double piv, tb;
    int j, jj, kst, ic;

    ier = 0;
    piv = 0.0;
    if(mld <= 0){
        //6
        jj = ma;
        j - ma - mz;
        kst = j;
        for(int k = 1; k <= kst; ++k){

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
            j = 1+ mz;
            ic = 1 + mud;

        }
    }
    //14









}
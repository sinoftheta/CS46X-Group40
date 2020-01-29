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
    int mc;

    if(mld < 0 || mud < 0) ERR

    mc = 1 + mld + mud;

    if((mc + 1 - 2 * m) > 0) ERR

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






}
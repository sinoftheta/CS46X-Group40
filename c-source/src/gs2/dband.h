#ifndef __CAP_GS2_DBAND_H__
#define __CAP_GS2_DBAND_H__


#include "../capstone/Array.h"
#include "../capstone/Matrix.h"
#include "../capstone/MathUtil.h"

/**
* @param s the matrix to traingularize 
* @param n iteration count
* @param nb ???
* @param ndim s dimention, unused
* @param mdim s dimention, unused
* @param iex iterator

*/
void gs2Dband(Matrix* s, int n, int nb, int ndim, int mdim, int* iex); 

#endif /*__CAP_GS2_DBAND_H__*/
#ifndef __CAP_GS2_SBAND_H__
#define __CAP_GS2_SBAND_H__


#include "../capstone/Array.h"
#include "../capstone/Matrix.h"
#include "../capstone/MathUtil.h"

/**
* @param s matrix 
* @param p array
* @param u array
* @param n iteration count
* @param nb ???
* @param ndim s dimention, unused
* @param mdim s dimention, unused
* @param nu p and u dimension, unused
*/

void gs2Sband(Matrix* s, Array* p, Array* u, int n, int nb); 

#endif /*__CAP_GS2_SBAND_H__*/
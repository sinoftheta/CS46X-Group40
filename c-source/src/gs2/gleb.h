#ifndef __CAP_GLEB_H__
#define __CAP_GLEB_H__

#include "../capstone/MathUtil.h"
#include "../capstone/Matrix.h"
#include "../capstone/Array.h"

/**
 * @param r array r 
 * @param a array a
 * @param m 
 * @param n
 * @param mud
 * @param mld
 * @param eps
 * @param ier
 * @param maxr max size of r
 * @param maxa max size of a
*/
void gs2Gleb(
    Array* r, Array* a, int m, int n, 
    int mud, int mld, int eps, int ier, 
    int maxr, int maxa
);

#endif /*__CAP_GLEB_H__*/
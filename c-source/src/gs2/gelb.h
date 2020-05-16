#ifndef __CAP_GLEB_H__
#define __CAP_GLEB_H__

#include "../capstone/MathUtil.h"
#include "../capstone/Array.h"

/*
 * Purpose:
    Solves the mass transport equation by triangularizing the matrix and then
    solve the equations by backsubstitution.

    Called from TS

 * @param r array r
 * @param a array a
 * @param m
 * @param n
 * @param mud
 * @param mld
 * @param eps
 * @param ier error signal
 * maxa and maxr are unused
*/
void gs2Gelb(
    Array* r, Array* a, int m, int n,
    int mud, int mld, double eps, int *ier
);

#endif /*__CAP_GLEB_H__*/

#include "qz.h"
#include <stdlib.h>

void gs2Qz(gs2State* state, Array* u, Array* old, Array* phii, Array* x, Array* y, Array* fmobx, Array* fmoby, Matrix* f,
       Matrix* dx, Matrix* dy, Array* detj, Array* cphi, Array* vkx, Array* vky, Array* dgx, Array* dgy,
       Array* ff, Matrix* in, Array* kf, Array* jd, Array* ieq, Array* lc, Array* lr, int* ms, int* kase,
       int l, int m, int ik, int* ispk, int* ispm, int* psik, int* istop) {

    // Purpose: To evaluate line integral terms

    arrayAssertNotNull(u, "Array 'u' NULL in gs2Qz!");
    arrayAssertNotNull(old, "Array 'old' NULL in gs2Qz!");
    arrayAssertNotNull(phii, "Array 'phii' NULL in gs2Qz!");
    arrayAssertNotNull(x, "Array 'x' NULL in gs2Qz!");
    arrayAssertNotNull(y, "Array 'y' NULL in gs2Qz!");
    arrayAssertNotNull(fmobx, "Array 'fmobx' NULL in gs2Qz!");
    arrayAssertNotNull(fmoby, "Array 'fmoby' NULL in gs2Qz!");
    arrayAssertNotNull(detj, "Array 'detj' NULL in gs2Qz!");
    arrayAssertNotNull(cphi, "Array 'cphi' NULL in gs2Qz!");
    arrayAssertNotNull(vkx, "Array 'vkx' NULL in gs2Qz!");
    arrayAssertNotNull(vky, "Array 'vky' NULL in gs2Qz!");
    arrayAssertNotNull(dgx, "Array 'dgx' NULL in gs2Qz!");
    arrayAssertNotNull(dgy, "Array 'dgy' NULL in gs2Qz!");
    arrayAssertNotNull(ff, "Array 'ff' NULL in gs2Qz!");
    arrayAssertNotNull(kf, "Array 'kf' NULL in gs2Qz!");
    arrayAssertNotNull(jd, "Array 'jd' NULL in gs2Qz!");
    arrayAssertNotNull(ieq, "Array 'ieq' NULL in gs2Qz!");
    arrayAssertNotNull(lc, "Array 'lc' NULL in gs2Qz!");
    arrayAssertNotNull(lr, "Array 'lr' NULL in gs2Qz!");

    matrixAssertNotNull(f, "Matrix 'f' NULL in gs2Qz!");
    matrixAssertNotNull(dx, "Matrix 'dx' NULL in gs2Qz!");
    matrixAssertNotNull(dy, "Matrix 'dy' NULL in gs2Qz!");
    matrixAssertNotNull(in, "Matrix 'in' NULL in gs2Qz!");

    Array ag, bg;
    arrayDimension(&ag, 6);
    arrayDimension(&bg, 4);

    *arrayAt(&ag, 0) = -0.577350;
    *arrayAt(&ag, 1) = 0.577350;
    *arrayAt(&ag, 2) = -0.861136;
    *arrayAt(&ag, 3) = -0.339981;
    *arrayAt(&ag, 4) = 0.339981;
    *arrayAt(&ag, 5) = 0.861136;

    *arrayAt(&bg, 0) = -1.0;
    *arrayAt(&bg, 1) = 1.0;
    *arrayAt(&bg, 2) = 1.0;
    *arrayAt(&bg, 3) = -1.0;

    int is1, is, i, j, k, j1, jj, jdi;
    double xi, yi, phi, ppk, hmz, x1, x2, x3, pp, xcond, ycond, det;

    *ispk = *arrayAt(&(state->ispl), ik);
    *psik = *arrayAt(&(state->psio), ik);
    *ispm = *ispk - 1;
    is1 = 0;

    for (is = 0; is < 4; is++) {

        if (*arrayAt(kf, l) != is) {
            continue;
        }

        is1++;
        if (*kase != 0) {

            state->np = 2;
            if (m > 4) {
                state->np = 4;
            }

            k = -1;
            for (i = 0; i < m; i++) {

                do {
                    k++;
                } while (*matrixAt(in, k, l) == 0);

                *arrayAt(jd, i) = *matrixAt(in, k, l);
            }
        }

        gs2Green(x, y, detj, &ag, in, kf, jd, ieq, ms, state->np, l, istop);

        for (k = 0; k < state->np; k++) {
            
            j1 = k;
            if (state->np == 4) {
                j1 = k + 2;
            }

            if (is != 0 && is != 2) {
                xi = *arrayAt(&bg, is);
                yi = *arrayAt(&ag, j1);
            } else {
                xi = *arrayAt(&ag, j1);
                yi = *arrayAt(&bg, is);
            }

            gs2Shape(x, y, in, l, m, xi, yi, ff, &det, dgx, dgy);

            for (jj = 0; jj < m; jj++) {
                *matrixAt(f, jj, k) = *arrayAt(ff, jj);
                *matrixAt(dx, jj, k) = *arrayAt(dgx, jj);
                *matrixAt(dy, jj, k) = *arrayAt(dgy, jj);
            }

            // Unsaturated properties
            *arrayAt(vkx, k) = 0.0;
            *arrayAt(vky, k) = 0.0;
            *arrayAt(cphi, k) = 0.0;

            for (i = 0; i < m; i++) {
                
                jdi = *arrayAt(jd, i) - 1;
                
                if (*arrayAt(lr, jdi) != 1) {
                    j = jdi - *arrayAt(lc, jdi);
                    phi = state->tdr * *arrayAt(u, j) + (1.0 - state->tdr) * *arrayAt(old, j);
                } else {
                    phi = *arrayAt(phii, jdi);
                }

                *arrayAt(cphi, k) += *matrixAt(f, i, k) * phi;
            }

            if (*arrayAt(cphi, k) >= *psik || *ispk <= 0) {

                ppk = 1.0;

            } else {

                hmz = abs(*arrayAt(cphi, k));
                hmz = log10(hmz);
                if (hmz > *matrixAt(&(state->xpsi), 0, ik)) {
                    hmz = *matrixAt(&(state->xpsi), 0, ik);
                }

                j = 0;
                while (hmz < *matrixAt(&(state->xpsi), j+1, ik) && j < ispm-1) {
                    j++;
                }

                x1 = hmz - *matrixAt(&(state->xpsi), j, ik);
                x2 = x1 * x1;
                x3 = x2 * x1;
                pp = *matrixAt(&(state->ckt[2]), j, ik) * x3 + *matrixAt(&(state->ckt[1]), j, ik) * x2 + *matrixAt(&(state->ckt[0]), j, ik) * x1 + *matrixAt(&(state->xk), j, ik);
                ppk = pow(10.0, pp - *matrixAt(&(state->xk), ik, ik));
                if (ppk > 1.0) {
                    ppk = 1.0;
                }
            }

            xcond = -ppk * *arrayAt(fmobx, l);
            ycond = -ppk * *arrayAt(fmoby, l);

            for (i = 0; i < m; i++) {
                
                jdi = *arrayAt(jd, i) - 1;

                if (*arrayAt(lr, jdi) != 1) {
                    j = jdi - *arrayAt(lc, jdi);
                    phi = state->tdr * *arrayAt(u, j) + (1.0 - state->tdr) * *arrayAt(old, j);
                } else {
                    phi = *arrayAt(phii, jdi);
                }

                *arrayAt(vkx, k) += *matrixAt(dx, i, k) * phi;
                *arrayAt(vky, k) += *matrixAt(dy, i, k) * phi;
            }

            *arrayAt(vkx, k) = xcond * *arrayAt(vkx, k);
            *arrayAt(vky, k) = ycond * *arrayAt(vky, k) + ycond;
        }
    }

    *kase = is1;

    arrayFree(&ag);
    arrayFree(&bg);

    return;
}
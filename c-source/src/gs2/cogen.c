#include "cogen.h"
#include <stdio.h>
#include <stdlib.h>

#include "../capstone/Debug.h"

void gs2Cogen(gs2State* state, Matrix* s, Matrix* p, Array* fm, Array* rt, Array* phi, Array* phii,
             Array* u, Array* old, Array* cfm, Array* crt, Array* conc, Array* conci, Array* fx,
             Array* cn, Array* est, Array* fq, Array* cfq, Array* x, Array* y, Array* fmobx,
             Array* fmoby, Array* por, Array* elong, Array* etrans, Array* alpha, Array* tta,
             Array* kd, Array* lambda, Array* rho, Matrix* in, Array* kf, Array* lr, Array* klr,
             Array* lc, Array* klc, Matrix* ie, int jtest) {

    arrayAssertNotNull(fm, "Array 'fm' NULL in gs2Cogen!");
    arrayAssertNotNull(rt, "Array 'rt' NULL in gs2Cogen!");
    arrayAssertNotNull(phi, "Array 'phi' NULL in gs2Cogen!");
    arrayAssertNotNull(phii, "Array 'phii' NULL in gs2Cogen!");
    arrayAssertNotNull(u, "Array 'u' NULL in gs2Cogen!");
    arrayAssertNotNull(old, "Array 'old' NULL in gs2Cogen!");
    arrayAssertNotNull(cfm, "Array 'cfm' NULL in gs2Cogen!");
    arrayAssertNotNull(crt, "Array 'crt' NULL in gs2Cogen!");
    arrayAssertNotNull(conc, "Array 'conc' NULL in gs2Cogen!");
    arrayAssertNotNull(conci, "Array 'conci' NULL in gs2Cogen!");
    arrayAssertNotNull(fx, "Array 'fx' NULL in gs2Cogen!");
    arrayAssertNotNull(cn, "Array 'cn' NULL in gs2Cogen!");
    arrayAssertNotNull(est, "Array 'est' NULL in gs2Cogen!");
    arrayAssertNotNull(fq, "Array 'fq' NULL in gs2Cogen!");
    arrayAssertNotNull(cfq, "Array 'cfq' NULL in gs2Cogen!");
    arrayAssertNotNull(x, "Array 'x' NULL in gs2Cogen!");
    arrayAssertNotNull(y, "Array 'y' NULL in gs2Cogen!");
    arrayAssertNotNull(fmobx, "Array 'fmobx' NULL in gs2Cogen!");
    arrayAssertNotNull(fmoby, "Array 'fmoby' NULL in gs2Cogen!");
    arrayAssertNotNull(por, "Array 'por' NULL in gs2Cogen!");
    arrayAssertNotNull(elong, "Array 'elong' NULL in gs2Cogen!");
    arrayAssertNotNull(etrans, "Array 'etrans' NULL in gs2Cogen!");
    arrayAssertNotNull(alpha, "Array 'alpha' NULL in gs2Cogen!");
    arrayAssertNotNull(tta, "Array 'tta' NULL in gs2Cogen!");
    arrayAssertNotNull(kd, "Array 'kd' NULL in gs2Cogen!");
    arrayAssertNotNull(lambda, "Array 'lambda' NULL in gs2Cogen!");
    arrayAssertNotNull(rho, "Array 'rho' NULL in gs2Cogen!");
    arrayAssertNotNull(kf, "Array 'kf' NULL in gs2Cogen!");
    arrayAssertNotNull(lr, "Array 'lr' NULL in gs2Cogen!");
    arrayAssertNotNull(klr, "Array 'klr' NULL in gs2Cogen!");
    arrayAssertNotNull(lc, "Array 'lc' NULL in gs2Cogen!");
    arrayAssertNotNull(klc, "Array 'klc' NULL in gs2Cogen!");

    matrixAssertNotNull(s, "Matrix 's' NULL in gs2Cogen!");
    matrixAssertNotNull(p, "Matrix 'p' NULL in gs2Cogen!");
    matrixAssertNotNull(in, "Matrix 'in' NULL in gs2Cogen!");
    matrixAssertNotNull(ie, "Matrix 'ie' NULL in gs2Cogen!");

    Array jd, ag, ieq;
    arrayDimension(&jd, 12);
    arrayDimension(&ag, 6);
    arrayDimension(&ieq, 4);

    *arrayAt(&ag, 1) = -0.577350;
    *arrayAt(&ag, 2) = 0.577350;
    *arrayAt(&ag, 3) = -0.861136;
    *arrayAt(&ag, 4) = -0.339981;
    *arrayAt(&ag, 5) = 0.339981;
    *arrayAt(&ag, 6) = 0.861136;

    int i, j, k, l, m, np2, ik, ispk, ispm, jdi, jdj, ic, jc,  stop, i1;
    int kase, ms, ii, jj, jdii, jdjj, kc, kb1;
    double vmax, xi, yi, det, st, psik, hmz, x1, x2, x3, teta, ce, pp, ppk;
    double vlx, vly, rd, xcond, ycond, dispx, dispy, dispxy;
    double vxsqrd, vysqrd, vsqrd, uu, dl, dt, dvx, dvy, ap;

    if (jtest == 0) {

        for (i = 1; i <= state->km; i++) {
            for (j = 1; j <= state->knb; j++) {
                *matrixAt(p, i, j) = 0.0;
            }
            for (j = 1; j <= state->knb2; j++) {
                *matrixAt(s, j, i) = 0.0;
            }
        }

        for (i = 1; i <= state->nn; i++) {

            if (*arrayAt(klr, i) != 1) {

                j = i - *arrayAt(klc, i);

                *arrayAt(cfm, j) = 0.0;
                *arrayAt(crt, j) = 0.0;

                if (*arrayAt(lr, i) == 0) {
                    if (*arrayAt(cfq, i) < 0) {
                        *arrayAt(crt, j) = -*arrayAt(fq, i) * *arrayAt(conc, i);
                    } else {
                        *arrayAt(crt, j) = -*arrayAt(fq, i) * *arrayAt(cfq, i);
                    }
                }
            }
        }

        vmax = 0.0;
    
    } else {

        for (i = 1; i <= state->mm; i++) {
            for (j = 1; j <= state->nb; j++) {
                *matrixAt(p, i, j) = 0.0;
                *matrixAt(s, j, i) = 0.0;
            }
        }

        for (i = 1; i <= state->nn; i++) {

            if (*arrayAt(lr, i) != 1) {

                j = i - *arrayAt(lc, i);

                *arrayAt(fm, j) = 0.0;
                *arrayAt(rt, j) = 0.0;

                if (*arrayAt(lr, i) < 2) {
                    *arrayAt(u, j) = 0.0;
                }

                *arrayAt(fx, j) = *arrayAt(u, j);

                if (*arrayAt(lr, i) == 0) {
                    *arrayAt(rt, j) = -*arrayAt(fq, i);
                }
            }
        }

    }

    l = 0;
    do {
        
        l++;
        m = *matrixAt(in, state->me, l);

        state->np = 2;
        if (m > 4) {
            state->np = 4;
        }

        np2 = state->np * state->np;

        k = 0;
        for (i = 1; i <= m; i++) {
            
            do {
                k++;
            } while (*matrixAt(in, k, l) == 0);

            *arrayAt(&jd, i) = *matrixAt(in, k, l);
        }

        /*
         * The array jd now contains the incidences
         * of the active nodes in element L.
         * 
         * Shape functions for integration points
         * Integration by Gaussian quadrature
         * 2x2 rule for fully linear elements
         * 4x4 rule for all other elements
         */

        for (i = 1; i <= state->np; i++) {
            for (j = 1; j <= state->np; j++) {

                k = (i-1) * state->np + j;

                if (state->np == 4) {
                    xi = *arrayAt(&ag, j+2);
                    yi = *arrayAt(&ag, i+2);
                } else {
                    xi = *arrayAt(&ag, j);
                    yi = *arrayAt(&ag, i);
                }

                gs2Shape(x, y, in, l, m, xi, yi, &(state->ff), &det, &(state->dgx), &(state->dgy));

                for (jj = 1; jj <= m; jj++) {
                    *matrixAt(&(state->f), jj, k) = *arrayAt(&(state->ff), jj);
                    *matrixAt(&(state->dx), jj, k) = *arrayAt(&(state->dgx), jj);
                    *matrixAt(&(state->dy), jj, k) = *arrayAt(&(state->dgy), jj);
                }

                *arrayAt(&(state->detj), k) = det;
            }
        }

        // Compute element matrices for flow
        if (jtest != 0) {

            st = *arrayAt(tta, l) / *arrayAt(por, l) * (*arrayAt(alpha, l) + *arrayAt(por, l) * state->betap);
            *matrixAt(ie, 1, l) = 0;

            if (*arrayAt(&(state->ispl), 1) != 0) {

                ik = *matrixAt(ie, 2, l);
                ispk = *arrayAt(&(state->ispl), ik);
                psik = *arrayAt(&(state->psio), ik);
                ispm = ispk - 1;

                for (i = 1; i <= m; i++) {
                    jdi = *arrayAt(&jd, i);
                    if (*arrayAt(lr, jdi) != 1) {
                        ic = jdi - *arrayAt(lc, jdi);
                        if (*arrayAt(lr, jdi) >= 2) {
                            *arrayAt(&(state->srcr), i) = *arrayAt(u, ic);
                        } else {
                            *arrayAt(&(state->srcr), i) = *arrayAt(est, ic);
                        }
                    } else {
                        *arrayAt(&(state->srcr), i) = *arrayAt(phii, jdi);
                    }

                    if (*arrayAt(&(state->srcr), i) < psik) {
                        *matrixAt(ie, 1, l) = l;
                    }
                }
            } else {
                for (i = 1; i <= m; i++) {
                    *arrayAt(&(state->srcr), i) = 0.0;
                }
            }

            for (k = 1; k <= np2; k++) {

                *arrayAt(&(state->cphi), k) = 0.0;

                for (j = 1; j <= m; j++) {
                    *arrayAt(&(state->cphi), k) += *matrixAt(&(state->f), j, k) * *arrayAt(&(state->srcr), j);
                }
            }

            // Compute volume integrals
            if (*matrixAt(ie, 1, l) == 0) {

                for (i = 1; i <= m; i++) {
                    for (j = 1; j <= m; j++) {

                        for (k = 1; k <= np2; k++) {
                            *arrayAt(&(state->qp), k) = st * *matrixAt(&(state->f), i, k) * *matrixAt(&(state->f), j, k) * *arrayAt(&(state->detj), k);
                            *arrayAt(&(state->q), k) = (*arrayAt(fmobx, l) * *matrixAt(&(state->dx), i, k) * *matrixAt(&(state->dx), j, k) \
                                                      + *arrayAt(fmoby, l) * *matrixAt(&(state->dy), i, k) * *matrixAt(&(state->dy), j, k)) \
                                                      * *arrayAt(&(state->detj), k);
                        }

                        gs2Matgen1(state, &(state->pe), &(state->se), &(state->q), &(state->qp), i, j);
                    }
                }

                for (i = 1; i <= m; i++) {

                    for (k = 1; k <= np2; k++) {
                        *arrayAt(&(state->srcrt), k) = -*arrayAt(fmoby, l) * *matrixAt(&(state->dy), i, k) * *arrayAt(&(state->detj), k);
                    }

                    gs2Matgen2(state, &(state->srcr), &(state->srcrt), i);
                }

            } else {

                for (k = 1; k <= np2; k++) {
                    if (*arrayAt(&(state->cphi), k) >= psik) {

                        *arrayAt(&(state->vkx), k) = *arrayAt(fmobx, l);
                        *arrayAt(&(state->vky), k) = *arrayAt(fmoby, l);
                        *arrayAt(&(state->cphi), k) = st;

                    } else {

                        hmz = fabs(*arrayAt(&(state->cphi), k));
                        hmz = log10(hmz);
                        hmz = fmin(hmz, *matrixAt(&(state->xpsi), 1, ik));

                        stop = 0;
                        for (j = 1; j <= ispm && !stop; j++) {
                            if (hmz >= *matrixAt(&(state->xpsi), j+1, ik)) {
                                stop = 1;
                                x1 = hmz - *matrixAt(&(state->xpsi), j, ik);
                                x2 = x1 * x1;
                                x3 = x2 * x1;

                                teta = (*matrixAt(&(state->ctt[2]), j, ik) * x3 + *matrixAt(&(state->ctt[1]), j, ik) * x2 + *matrixAt(&(state->ctt[0]), j, ik) * x1 + \
                                        *matrixAt(&(state->xm), j, ik)) / *matrixAt(&(state->xm), ispk, ik);
                                ce = (3.0 * *matrixAt(&(state->ctt[2]), j, ik) * x2 + 2.0 * *matrixAt(&(state->ctt[1]), j, ik) * x1 + *matrixAt(&(state->ctt[0]), j, ik)) \
                                    * 0.43429 / *arrayAt(&(state->cphi), k);
                                pp = *matrixAt(&(state->ckt[2]), j, ik) * x3 + *matrixAt(&(state->ckt[1]), j, ik) * x2 + *matrixAt(&(state->ckt[0]), j, ik) * x1 + *matrixAt(&(state->xk), j, ik);
                                ppk = pow(10.0, pp - *matrixAt(&(state->xk), ispk, ik));

                                teta = fmin(teta, 1.0);
                                ppk = fmin(ppk, 1.0);
                                ce = fabs(ce);
                            }
                        }
                        if (!stop) {
                            teta = 1.0;
                            ppk = 1.0;
                            ce = 0.0;
                        }

                        *arrayAt(&(state->vkx), k) = ppk * *arrayAt(fmobx, l);
                        *arrayAt(&(state->vky), k) = ppk * *arrayAt(fmoby, l);
                        *arrayAt(&(state->cphi), k) = teta * *arrayAt(tta, l) * state->betap + ce;
                    }
                }

                for (i = 1; i <= m; i++) {
                    for (j = 1; j <= m; j++) {
                        for (k = 1; k <= np2; k++) {
                            *arrayAt(&(state->qp), k) = *arrayAt(&(state->cphi), k) * *matrixAt(&(state->f), i, k) * *matrixAt(&(state->f), j, k) * *arrayAt(&(state->detj), k);
                            *arrayAt(&(state->q), k) = (*arrayAt(&(state->vkx), k) * *matrixAt(&(state->dx), i, k) * *matrixAt(&(state->dx), j, k) \
                                                      + *arrayAt(&(state->vky), k) * *matrixAt(&(state->dy), i, k) * *matrixAt(&(state->dy), j, k)) \
                                                      * *arrayAt(&(state->detj), k);
                        }
                        gs2Matgen1(state, &(state->pe), &(state->se), &(state->q), &(state->qp), i, j);
                    }
                }

                for (i = 1; i <= m; i++) {
                    for (k = 1; k <= np2; k++) {
                        *arrayAt(&(state->srcrt), k) = -*arrayAt(&(state->vky), k) * *matrixAt(&(state->dy), i, k) * *arrayAt(&(state->detj), k);
                    }
                    gs2Matgen2(state, &(state->srcr), &(state->srcrt), i);
                }
            }

            for (i = 2; i <= m; i++) {
                
                i1 = i - 1;

                for (j = 1; j <= i1; j++) {
                    *matrixAt(&(state->pe), i, j) = *matrixAt(&(state->pe), j, i);
                    *matrixAt(&(state->se), i, j) = *matrixAt(&(state->se), j, i);
                }
            }

            // Print element matrices for flow
            if (state->kod1 == 1) {
                fprintf(gs2stdout, "1\n\n\n\n           ELEMENT MATRICES FOR FLOW\n           -------------------------\n");
                gs2Matgen3(&(state->pe), &(state->se), m, l);
            }

            // Assembly
            gs2Asembl(s, p, &(state->se), &(state->pe), rt, fx, &(state->srcr), phii, lr, lc,
                      &jd, m, l, state->nb, state->nb, &(state->mb), &(state->mb2), &(state->istop));
        
        
        // Compute concentration coefficient matrices
        } else {

            vlx = 0.0;
            vly = 0.0;
            rd = 1.0 + *arrayAt(kd, l) * *arrayAt(rho, l) / *arrayAt(por, l);

            for (k = 1; k <= np2; k++) {

                *arrayAt(&(state->vkx), k) = 0.0;
                *arrayAt(&(state->vky), k) = 0.0;
                *arrayAt(&(state->cphi), k) = 0.0;
                *arrayAt(&(state->dpordt), k) = 0.0;
                *arrayAt(&(state->dk), k) = 0.0;
                *arrayAt(&(state->dh), k) = 0.0;

                for (j = 1; j <= m; j++) {

                    jdj = *arrayAt(&jd, j);
                    jc = jdj - *arrayAt(lc, jdj);

                    if (*arrayAt(lr, jdj) != 1) {
                        *arrayAt(&(state->dpordt), k) += *matrixAt(&(state->f), j, k) * (*arrayAt(phi, jdj) - *arrayAt(old, jc));
                    }

                    *arrayAt(&(state->cphi), k) += *matrixAt(&(state->f), j, k) * *arrayAt(phi, jdj);
                }

                if (*matrixAt(ie, 1, l) == 0) {

                    *arrayAt(&(state->cphi), k) = *arrayAt(por, l);
                    *arrayAt(&(state->d0), k) = exp(10.0 * *arrayAt(por, l));
                    *arrayAt(&(state->dh), k) = rd * *arrayAt(alpha, l) * *arrayAt(&(state->cphi), k) * *arrayAt(&(state->dpordt), k) / state->delt;
                    *arrayAt(&(state->dpordt), k) = 0.0;
                    xcond = -*arrayAt(fmobx, l) / *arrayAt(por, l);
                    ycond = -*arrayAt(fmoby, l) / *arrayAt(por, l);

                } else {

                    ik = *matrixAt(ie, 2, l);
                    ispk = *arrayAt(&(state->ispl), ik);
                    psik = *arrayAt(&(state->psio), ik);
                    ispm = ispk - 1;

                    if (*arrayAt(&(state->cphi), k) >= psik) {

                        *arrayAt(&(state->cphi), k) = *arrayAt(por, l);
                        *arrayAt(&(state->d0), k) = exp(10.0 * *arrayAt(por, l));
                        *arrayAt(&(state->dh), k) = rd * *arrayAt(alpha, l) * *arrayAt(&(state->cphi), k) * *arrayAt(&(state->dpordt), k) / state->delt;
                        *arrayAt(&(state->dpordt), k) = 0.0;
                        xcond = -*arrayAt(fmobx, l) / *arrayAt(por, l);
                        ycond = -*arrayAt(fmoby, l) / *arrayAt(por, l);

                    } else {

                        hmz = fabs(*arrayAt(&(state->cphi), k));
                        hmz = log10(hmz);
                        hmz = fmin(hmz, *matrixAt(&(state->xpsi), 1, ik));

                        stop = 0;
                        for (j = 1; j <= ispm && !stop; j++) {
                            if (hmz >= *matrixAt(&(state->xpsi), j+1, ik)) {
                                stop = 1;
                            }
                        }
                        j--;

                        x1 = hmz - *matrixAt(&(state->xpsi), j, ik);
                        x2 = x1 * x1;
                        x3 = x2 * x1;
                        teta = (*matrixAt(&(state->ctt[2]), j, ik) * x3 + *matrixAt(&(state->ctt[1]), j, ik) * x2 + *matrixAt(&(state->ctt[0]), j, ik) * x1 + \
                                *matrixAt(&(state->xm), j, ik)) / *matrixAt(&(state->xm), ispk, ik);
                        ce = (3.0 * *matrixAt(&(state->ctt[2]), j, ik) * x2 + 2.0 * *matrixAt(&(state->ctt[1]), j, ik) * x1 + *matrixAt(&(state->ctt[0]), j, ik)) \
                            * 0.43429 / *arrayAt(&(state->cphi), k);
                        pp = *matrixAt(&(state->ckt[2]), j, ik) * x3 + *matrixAt(&(state->ckt[1]), j, ik) * x2 + *matrixAt(&(state->ckt[0]), j, ik) * x1 + *matrixAt(&(state->xk), j, ik);
                        ppk = pow(10.0, pp - *matrixAt(&(state->xk), ispk, ik));
                        teta = fmin(teta, 1.0);
                        ppk = fmin(ppk, 1.0);

                        xcond = -ppk * *arrayAt(fmobx, l) / (teta * *arrayAt(tta, l));
                        ycond = -ppk * *arrayAt(fmoby, l) / (teta * *arrayAt(tta, l));
                        *arrayAt(&(state->cphi), k) = teta * *arrayAt(tta, l);
                        *arrayAt(&(state->d0), k) = exp(10.0 * *arrayAt(&(state->cphi), k));
                        *arrayAt(&(state->dh), k) = rd * *arrayAt(alpha, l) * *arrayAt(&(state->cphi), k) * *arrayAt(&(state->dpordt), k) / state->delt;
                        *arrayAt(&(state->dpordt), k) = *arrayAt(&(state->dpordt), k) / state->delt * ce;
                        *arrayAt(&(state->dpordt), k) = *arrayAt(&(state->dpordt), k) * state->dprdt * rd;

                    }
                }

                *arrayAt(&(state->dk), k) = rd * *arrayAt(lambda, l) * *arrayAt(&(state->cphi), k);

                for (j = 1; j <= m; j++) {
                    jdj = *arrayAt(&jd, j);
                    *arrayAt(&(state->vkx), k) += *matrixAt(&(state->dx), j, k) * *arrayAt(phi, jdj);
                    *arrayAt(&(state->vky), k) += *matrixAt(&(state->dy), j, k) * *arrayAt(phi, jdj);
                }

                *arrayAt(&(state->vky), k) = ycond * (*arrayAt(&(state->vky), k) + 1.0);
                *arrayAt(&(state->vkx), k) = xcond * *arrayAt(&(state->vkx), k);
                vlx += *arrayAt(&(state->vkx), k);
                vly += *arrayAt(&(state->vky), k);

            }

            vlx /= np2;
            vly /= np2;
            vmax = fmax(vmax, fmax(fabs(vlx), fabs(vly)));

            for (i = 1; i <= m; i++) {
                *arrayAt(&(state->srcr), i) = 0.0;
                for (j = 1; j <= m; j++) {
                    for (k = 1; k <= np2; k++) {
                        if (state->difusn < 0) {
                            dispx = *arrayAt(elong, l);
                            dispy = *arrayAt(etrans, l);
                            dispxy = 0.0;
                        } else {
                            vxsqrd = *arrayAt(&(state->vkx), k) * *arrayAt(&(state->vkx), k);
                            vysqrd = *arrayAt(&(state->vky), k) * *arrayAt(&(state->vky), k);
                            vsqrd = vxsqrd + vysqrd;
                            
                            if (vsqrd == 0) {
                                dispx = state->difusn;
                                dispy = state->difusn;
                                dispxy = 0.0;
                            } else {
                                uu = sqrt(vsqrd);
                                dl = *arrayAt(elong, l) * uu;
                                dt = *arrayAt(etrans, l) * uu;
                                dvx = vxsqrd / vsqrd;
                                dvy = vysqrd / vsqrd;

                                dispx = dl * dvx + dt * dvy + state->difusn * *arrayAt(&(state->d0), k);
                                dispy = dt * dvx + dl * dvy + state->difusn * *arrayAt(&(state->d0), k);
                                dispxy = (dl - dt) * *arrayAt(&(state->vkx), k) * *arrayAt(&(state->vky), k) / vsqrd + state->difusn * *arrayAt(&(state->d0), k);
                            }
                        }

                        *arrayAt(&(state->qp), k) = *arrayAt(&(state->cphi), k) * *matrixAt(&(state->f), i, k) * *matrixAt(&(state->f), j, k) * *arrayAt(&(state->detj), k) * state->stat * rd;
                        *arrayAt(&(state->q), k) = ((dispx * *matrixAt(&(state->dx), i, k) * *matrixAt(&(state->dx), j, k) + dispy * *matrixAt(&(state->dy), i, k) * *matrixAt(&(state->dy), j, k) \
                                                 + dispxy * (*matrixAt(&(state->dx), i, k) * *matrixAt(&(state->dy), j, k) + *matrixAt(&(state->dy), i, k) * *matrixAt(&(state->dx), j, k)) \
                                                 + (*arrayAt(&(state->vkx), k) * *matrixAt(&(state->dx), j, k) + *arrayAt(&(state->vky), k) * *matrixAt(&(state->dy), j, k)) * *matrixAt(&(state->f), i, k)) * *arrayAt(&(state->cphi), k) \
                                                 + (*arrayAt(&(state->dpordt), k) + *arrayAt(&(state->dk), k) + *arrayAt(&(state->dh), k)) * *matrixAt(&(state->f), i, k) * *matrixAt(&(state->f), j, k)) * *arrayAt(&(state->detj), k);
                    }

                    gs2Matgen1(state, &(state->pe), &(state->se), &(state->q), &(state->qp), i, j);
                }
            }

            kase = 0;
            gs2Qz(state, u, old, phii, x, y, fmobx, fmoby, &(state->f), &(state->dx), &(state->dy), &(state->detj),
                  &(state->cphi), &(state->vkx), &(state->vky), &(state->dgx), &(state->dgy), &(state->ff),
                  in, kf, &jd, &ieq, lc, lr, &ms, &kase, l, m, ik, &ispk, &ispm, &psik, &(state->istop));

            if (kase != 0) {
                for (i = 1; i <= ms; i++) {
                    ii = *arrayAt(&ieq, i);
                    jdii = *arrayAt(&jd, ii);
                    if (*arrayAt(klr, jdii) == -4) {
                        for (j = 1; j <= ms; j++) {
                            jj = *arrayAt(&ieq, j);
                            jdjj = *arrayAt(&jd, jj);
                            if (*arrayAt(klr, jdjj) == -4) {
                                for (k = 1; k <= state->np; k++) {
                                    *arrayAt(&(state->q), k) = (*arrayAt(&(state->vkx), k) * *arrayAt(&(state->detj), k + state->np) - *arrayAt(&(state->vky), k) \
                                                              * *arrayAt(&(state->detj), k)) * *matrixAt(&(state->f), ii, k) * *matrixAt(&(state->f), jj, k);
                                }
                                if (state->np != 4) {
                                    ap = *arrayAt(&(state->q), 1) + *arrayAt(&(state->q), 2);
                                } else {
                                    ap = state->h1 * (*arrayAt(&(state->q), 1) + *arrayAt(&(state->q), 4)) \
                                       + state->h2 * (*arrayAt(&(state->q), 2) + *arrayAt(&(state->q), 3));
                                }
                                *matrixAt(&(state->se), ii, jj) -= ap;
                            }
                        }
                    }
                }

                kc = 0;
                for (i = 1; i <= ms; i++) {
                    ii = *arrayAt(&ieq, i);
                    jdii = *arrayAt(&jd, ii);
                    if (*arrayAt(klr, jdii) == -4) {
                        kc++;
                        for (k = 1; k <= state->np; k++) {
                            *arrayAt(&(state->srcrt), k) = (*arrayAt(&(state->vkx), k) * *arrayAt(&(state->detj), k + state->np) - *arrayAt(&(state->vky), k) \
                                                          * *arrayAt(&(state->detj), k)) * *matrixAt(&(state->f), ii, k);
                        }
                        if (state->np != 4) {
                            *arrayAt(&(state->srcr), ii) = *arrayAt(&(state->srcrt), 1) + *arrayAt(&(state->srcrt), 2);
                        } else {
                            *arrayAt(&(state->srcr), ii) = state->h1 * (*arrayAt(&(state->srcrt), 1) + *arrayAt(&(state->srcrt), 4)) \
                                                         + state->h2 * (*arrayAt(&(state->srcrt), 2) + *arrayAt(&(state->srcrt), 3));
                        }
                        *arrayAt(&(state->srcr), ii) = -*arrayAt(&(state->srcr), ii) * *arrayAt(cn, kc);
                    }
                }
            }

            // Print element matrices for concentration
            if (state->kod3 == 1) {
                fprintf(gs2stdout, "1\n\n\n\n           ELEMENT MATRICES FOR CONCENTRATION\n           ----------------------------------\n");
                gs2Matgen3(&(state->pe), &(state->se), m, l);
            }

            // Assembly of global coefficient matrix
            gs2Asembl(s, p, &(state->se), &(state->pe), crt, crt, &(state->srcr), conci, klr, klc,
                      &jd, m, l, state->knb, state->knb2, &(state->kmb), &(state->kmb2), &(state->istop));
        }
    } while (l != state->ne);

    // Write block on tape
    if (jtest != 0) {

        if (*arrayAt(&(state->ispl), 1) == 0) {
            if (state->statp != 0) {
                for (i = 1; i <= state->mm; i++) {
                    for (j = 1; j <= state->mb; j++) {
                        *matrixAt(&(state->tape11), i, j) = *matrixAt(p, i, j);
                    }
                }

                if (((state->it + 1) % state->itchng) == 0 || state->nsdn != 0) {
                    for (i = 1; i <= state->mb; i++) {
                        for (j = 1; j <= state->mm; j++) {
                            *matrixAt(&(state->tape13), i, j) = *matrixAt(s, i, j);
                        }
                    }
                }
            }
        } else if (state->nsdn != 0) {
            for (i = 1; i <= state->mb; i++) {
                for (j = 1; j <= state->mm; j++) {
                    *matrixAt(&(state->tape13), i, j) = *matrixAt(s, i, j);
                }
            }
        }

        if (state->kod2 - 1 > 0) {
            fprintf(gs2stdout, "1          GLOBAL COEFFICIENT MATRICES FOR FLOW\n           ------------------------------------\n");
            fprintf(gs2stdout, "0          S COEFFICIENT MATRIX - UPPER HALFBAND\n           -------------------------------------\n");
            gs2Sos(s, state->mb, state->mm, 1);
            fprintf(gs2stdout, "0          P COEFFICIENT MATRIX - UPPER HALFBAND\n           -------------------------------------\n");
            gs2Sos(p, state->mm, state->mb, 1);
        }

        if (state->kod2 - 1 >= 0) {
            fprintf(gs2stdout, "\n\n\n\n\n           F COEFFICIENT MATRIX\n           --------------------\n\n\n");
            for (i = 1; i <= state->mm; i++) {
                if (i % 10 == 1) {
                    fprintf(gs2stdout, "\n     ");
                }
                fprintf(gs2stdout, "%12.4E", *arrayAt(rt, i));
            }
            fprintf(gs2stdout, "\n");
        }

    } else {
        if (((state->it + 1) % state->itchng) == 0) {
            kb1 = state->knb - state->kmb + 1;
            for (i = kb1; i <= state->kmb2; i++) {
                for (j = 1; j <= state->km; j++) {
                    *matrixAt(&(state->tape2), i - kb1 + 1, j) = *matrixAt(s, i, j);
                }
            }
        }

        if (state->kod4 > 0) {
            fprintf(gs2stdout, "1          GLOBAL COEFFICIENT MATRICES FOR CONC.\n           -------------------------------------\n");
            fprintf(gs2stdout, "0          S COEFFICIENT MATRIX\n           --------------------\n\n\n");
            gs2Sos(s, state->kmb2, state->km, state->knb - state->kmb + 1);
            fprintf(gs2stdout, "0          P COEFFICIENT MATRIX\n           --------------------\n\n\n");
            gs2Sos(p, state->km, state->kmb, 1);
        }

        if (state->kod4 >= 0) {
            fprintf(gs2stdout, "\n\n\n\n\n           F COEFFICIENT MATRIX\n           --------------------\n\n\n");
            for (i = 1; i <= state->km; i++) {
                if (i % 10 == 1) {
                    fprintf(gs2stdout, "\n     ");
                }
                fprintf(gs2stdout, "%12.4E", *arrayAt(crt, i));
            }
            fprintf(gs2stdout, "\n");
        }

        fprintf(gs2stdout, "\n           VMAX%15.5E\n", state->vmax);
    }

    arrayFree(&jd);
    arrayFree(&ag);
    arrayFree(&ieq);
}

#include "ts.h"
#include <stdio.h>
#include <stdlib.h>

#define yes 1
#define no 0

void gs2Ts(gs2State* state, Matrix* s, Matrix* p, Array* w, Array* fm, Array* rt, Array* phi, Array* phii, Array* old,
           Array* cfm, Array* crt, Array* conc, Array* conci, Array* cold, Array* fx, Array* cn, Array* vn,
           Array* coef, Array* est, Array* u, Array* fq, Array* cfq, Array* x, Array* y, Array* fmobx,
           Array* fmoby, Array* elong, Array* etrans, Array* por, Array* alpha, Array* tta, Array* kd,
           Array* lambda, Array* rho, Matrix* in, Array* kf, Array* lr, Array* klr, Array* lc, Array* klc,
           Array* lp, Array* nsf, Matrix* ie, Matrix* nsp, Array* msp, char* rdate, char* rtime) {

    // Purpose: To formulate and solve flow and mass-transport equations

    arrayAssertNotNull(w, "Array 'w' NULL in gs2Ts!");
    arrayAssertNotNull(fm, "Array 'fm' NULL in gs2Ts!");
    arrayAssertNotNull(rt, "Array 'rt' NULL in gs2Ts!");
    arrayAssertNotNull(phi, "Array 'phi' NULL in gs2Ts!");
    arrayAssertNotNull(phii, "Array 'phii' NULL in gs2Ts!");
    arrayAssertNotNull(old, "Array 'old' NULL in gs2Ts!");
    arrayAssertNotNull(cfm, "Array 'cfm' NULL in gs2Ts!");
    arrayAssertNotNull(crt, "Array 'crt' NULL in gs2Ts!");
    arrayAssertNotNull(conc, "Array 'conc' NULL in gs2Ts!");
    arrayAssertNotNull(conci, "Array 'conci' NULL in gs2Ts!");
    arrayAssertNotNull(cold, "Array 'cold' NULL in gs2Ts!");
    arrayAssertNotNull(fx, "Array 'fx' NULL in gs2Ts!");
    arrayAssertNotNull(cn, "Array 'cn' NULL in gs2Ts!");
    arrayAssertNotNull(vn, "Array 'vn' NULL in gs2Ts!");
    arrayAssertNotNull(coef, "Array 'coef' NULL in gs2Ts!");
    arrayAssertNotNull(est, "Array 'est' NULL in gs2Ts!");
    arrayAssertNotNull(u, "Array 'u' NULL in gs2Ts!");
    arrayAssertNotNull(fq, "Array 'fq' NULL in gs2Ts!");
    arrayAssertNotNull(cfq, "Array 'cfq' NULL in gs2Ts!");
    arrayAssertNotNull(x, "Array 'x' NULL in gs2Ts!");
    arrayAssertNotNull(y, "Array 'y' NULL in gs2Ts!");
    arrayAssertNotNull(fmobx, "Array 'fmobx' NULL in gs2Ts!");
    arrayAssertNotNull(fmoby, "Array 'fmoby' NULL in gs2Ts!");
    arrayAssertNotNull(elong, "Array 'elong' NULL in gs2Ts!");
    arrayAssertNotNull(etrans, "Array 'etrans' NULL in gs2Ts!");
    arrayAssertNotNull(por, "Array 'por' NULL in gs2Ts!");
    arrayAssertNotNull(alpha, "Array 'alpha' NULL in gs2Ts!");
    arrayAssertNotNull(tta, "Array 'tta' NULL in gs2Ts!");
    arrayAssertNotNull(kd, "Array 'kd' NULL in gs2Ts!");
    arrayAssertNotNull(lambda, "Array 'lambda' NULL in gs2Ts!");
    arrayAssertNotNull(rho, "Array 'rho' NULL in gs2Ts!");
    arrayAssertNotNull(kf, "Array 'kf' NULL in gs2Ts!");
    arrayAssertNotNull(lr, "Array 'lr' NULL in gs2Ts!");
    arrayAssertNotNull(klr, "Array 'klr' NULL in gs2Ts!");
    arrayAssertNotNull(lc, "Array 'lc' NULL in gs2Ts!");
    arrayAssertNotNull(klc, "Array 'klc' NULL in gs2Ts!");
    arrayAssertNotNull(lp, "Array 'lp' NULL in gs2Ts!");
    arrayAssertNotNull(nsf, "Array 'nsf' NULL in gs2Ts!");
    arrayAssertNotNull(msp, "Array 'msp' NULL in gs2Ts!");

    matrixAssertNotNull(s, "Matrix 's' NULL in gs2Ts!");
    matrixAssertNotNull(p, "Matrix 'p' NULL in gs2Ts!");
    matrixAssertNotNull(in, "Matrix 'in' NULL in gs2Ts!");
    matrixAssertNotNull(ie, "Matrix 'ie' NULL in gs2Ts!");
    matrixAssertNotNull(nsp, "Matrix 'nsp' NULL in gs2Ts!");
    
    int ktcal = 0;
    int advanc, i, j, k, l, nit, jtest, ui, kkk, icheck, nt, jj, isk, stop, kb1, iex, ier, jx;
    double deltgo, delt1, a3, pn, un, smin;

    do {
        advanc = no;
        if (state->it % state->igo == 0) {
            advanc = yes;
        }

        if (advanc) {

            // Compute maximum value of delta p
            if (ktcal != 0) {
                state->delp = 0.0;
                for (i = 1; i <= state->nn; i++) {
                    if (*arrayAt(lr, i) != 1) {
                        j = i - *arrayAt(lc, i);
                        state->delp = max(state->delp, abs(*arrayAt(phi, i) - *arrayAt(old, j)));
                    }
                }
            }

            if (ktcal != 0) {
                // Update old
                for (i = 1; i <= state->mm; i++) {
                    *arrayAt(est, i) = *arrayAt(old, i);
                }

                gs2UPD(phi, old, lr, lc, state->nn);

                if (state->it % state->itchng == 0) {

                    // Time step modification
                    state->delt *= state->chng;
                    
                    if (state->delt <= 0.0) {
                        // read (5, 1980) delt
                        state->delt *= 3600;
                    }

                    fprintf(state->gs2stdout, "\n           TIME STEP MODIFICATION: DELT = %15.5E\n", state->delt);

                }

                for (i = 1; i <= state->mm; i++) {
                    *arrayAt(est, i) = *arrayAt(old, i) + state->delt / state->oldt / 2.0 * (*arrayAt(old, i) - *arrayAt(est, i));
                }

                if (state->statp == 0) {
                    if (*arrayAt(&(state->ispl), 1) == 0) {

                        fprintf(state->gs2stdout, "\n           PRESSURE WAS NOT RE-EVALUATED\n");
                        advanc = no;

                    } else if (state->stat >= 0) {

                        fprintf(state->gs2stdout, "\n           PRESSURE WAS NOT RE-EVALUTED: DELP = %15.8E\n", state->delp);
                        advanc = no;

                    }
                }
            }

            if (advanc) {
                if ((state->it + 1) % state->kod9 == 0) {

                    fprintf(state->gs2stdout, "\n\n           ESTIMATED HEAD\n           --------------\n");
                    fprintf(state->gs2stdout, "\n           NODE     VALUE     NODE     VALUE     NODE     VALUE     NODE     VALUE     NODE     VALUE     NODE     VALUE\n");
                    
                    for (i = 1; i <= state->mm; i++) {
                        if (i % 6 == 1) {
                            fprintf(state->gs2stdout, "\n           ");
                        }
                        fprintf(state->gs2stdout, "%4d  %10.3E   ", *arrayAt(lp, i), *arrayAt(est, i));
                    }
                    fprintf(state->gs2stdout, "\n");

                }

                // Select approximation for time
                deltgo = state->delt * state->igo;
                a3 = state->tdr / deltgo;

                nit = 0;
                stop = 0;

                do {

                    nit++;
                    if (ktcal == 0 || *arrayAt(&(state->ispl), 1) != 0) {

                        // Generate coef. matrices for flow
                        jtest = 1;
                        gs2Cogen(state, s, p, fm, rt, phi, phii, u, old, cfm, crt, conc, conci, fx, cn, est, fq, cfq,
                                 x, y, fmobx, fmoby, por, elong, etrans, alpha, tta, kd, lambda, rho, in, kf, lr, klr,
                                 lc, klc, ie, jtest);

                        if (state->istop > 0) {
                            return;
                        }

                        jtest = -1;

                    } else {

                        // Add time-dependent parts
                        // rewind 11
                        // read (11) ((P(I, J), J = 1, MB), I = 1, MM)

                        if (nit <= 1) {
                            if (state->it % state->itchng == 0) {
                                // rewind 13
                                // read (13) ((S(I, J), J = 1, MM), I = 1, MB)
                            } else {
                                jtest++;
                            }
                        }
                    }

                    gs2Lrhs(s, p, fm, old, u, lc, state->nn, state->nb, state->mb, a3, 0.0, 1);

                    if (state->kod8 >= 1) {

                        fprintf(state->gs2stdout, "\n           TIME-DEPENDENT PART OF RHS VECTOR\n           ---------------------------------\n");

                        for (i = 1; i <= state->mm; i++) {
                            if (i % 10 == 1) {
                                fprintf(state->gs2stdout, "\n     ");
                            }
                            fprintf(state->gs2stdout, "%12.4E", *arrayAt(fm, i));
                        }
                        fprintf(state->gs2stdout, "\n");
                    }

                    if (jtest <= 1) {

                        gs2Lrhs(s, p, fm, old, u, lc, state->nn, state->nb, state->mb, a3, 0.0, 3);

                        // Apply boundary conditions
                        for (i = 1; i <= state->nn; i++) {
                            if (*arrayAt(lr, i) >= 2) {
                                j = i - *arrayAt(lc, i);
                                ui = *arrayAt(u, j);
                                gs2Zero(p, fx, ui, state->mm, state->mb, j);
                            }
                        }

                        if (state->kod8 - 1 > 0) {
                            fprintf(state->gs2stdout, "\n\n1 GLOBAL COEFFICIENT MATRIX FOR FLOW\n  INPUT TO SOLVER\n\n\n");
                            gs2Sos(s, state->mb, state->mm, 1);
                            fprintf(state->gs2stdout, "0          P COEFFICIENT MATRIX\n           --------------------\n\n\n");
                            gs2Sos(p, state->mm, state->mb, 1);
                        }

                        if (state->kod8 - 1 >= 0) {
                            fprintf(state->gs2stdout, "\n           VECTOR OF BOUNDARY CONDITIONS\n           -----------------------------\n");
                            for (i = 1; i <= state->mm; i++) {
                                if (i % 10 == 1) {
                                    fprintf(state->gs2stdout, "\n     ");
                                }
                                fprintf(state->gs2stdout, "%12.4E", *arrayAt(fx, i));
                            }
                            fprintf(state->gs2stdout, "\n");
                        }

                        kkk = 1;
                    }

                    if (jtest < 0) {
                        jtest = 1;

                        if (ktcal == 0) {
                            fprintf(state->gs2stdout, "\n\n\n\n           FINAL HALF-BANDWIDTH FOR FLOW%5d%5d\n", state->mb, state->mb2);
                        }

                        if ((state->it + 1) % state->kod9 == 0) {
                            fprintf(state->gs2stdout, "\n\n           UNSATURATED ELEMENTS\n\n");

                            fprintf(state->gs2stdout, "           ");
                            for (l = 1; l <= state->ne; l++) {
                                if (l % 20 == 1 && l != 1) {
                                    fprintf(state->gs2stdout, "\n           ");
                                }
                                fprintf(state->gs2stdout, "%5d", *matrixAt(ie, 1, l));
                            }
                            fprintf(state->gs2stdout, "\n");

                        }
                    }

                    for (i = 1; i <= state->nn; i++) {
                        if (*arrayAt(lr, i) != 1) {
                            j = i - *arrayAt(lc, i);

                            if (*arrayAt(lr, i) < 2) {
                                *arrayAt(fm, j) += *arrayAt(rt, j) + *arrayAt(fx, j);
                                
                                if (*arrayAt(lr, i) < 0) {
                                    *arrayAt(fm, j) += *arrayAt(fq, i);
                                }
                            } else {
                                *arrayAt(fm, j) = *arrayAt(fx, j);
                            }
                        }
                    }

                    if (state->kod8 >= 1) {
                        fprintf(state->gs2stdout, "\n           RHS VECTOR (INPUT TO SOLVER)\n           ----------------------------\n");

                        for (i = 1; i <= state->mm; i++) {
                            if (i % 10 == 1) {
                                fprintf(state->gs2stdout, "\n     ");
                            }
                            fprintf(state->gs2stdout, "%12.4E", *arrayAt(fm, i));
                        }
                        fprintf(state->gs2stdout, "\n");
                    }

                    // Solve for pressure
                    if (kkk <= 1) {
                        
                        gs2Dband(p, state->mm, state->mb, &iex);

                        if (iex != 0) {
                            fprintf(state->gs2stdout, "\n IEX = %5d      STOP\n\n", iex);
                            state->istop++;
                            return;
                        } 
                    }

                    kkk++;

                    gs2Sband(p, fm, u, state->mm, state->mb);

                    // Determine boundary flux
                    if ((state->nsdn > 0 && state->coefi == 1) || state->nseep != 0) {
                        
                        // rewind 13
                        // read (13) ((P(J, I), J = 1, MM), I = 1, NB)

                        gs2Lrhs(s, p, fm, old, u, lc, state->nn, state->nb, state->mb, 1.0 - state->tdr, state->tdr, 1);

                        // Modify surface flux boundaries
                        if (state->nsdn != 0 && state->coefi == 1) {
                            
                            if ((state->it + 1) % state->kod9 == 0) {
                                fprintf(state->gs2stdout, "\n\n           CURRENT BOUNDARY CONDITIONS AT NEUMANN NODES\n           --------------------------------------------\n");
                                fprintf(state->gs2stdout, "           NODE   TYPE OF B.C.        HEAD IN       HEAD OUT        FLUX IN       FLUX OUT       FRACTION\n");
                            }

                            for (k = 1; k <= state->nsdn; k++) {

                                i = *arrayAt(nsf, k);
                                j = i - *arrayAt(lc, i);
                                *arrayAt(fm, j) -= *arrayAt(rt, j);

                                if (*arrayAt(lr, i) == 4) {

                                    if ((state->it + 1) % state->kod9 == 0) {
                                        fprintf(state->gs2stdout, "           %4d%15d%15.5E                              %15.5E\n", i, *arrayAt(lr, i), *arrayAt(u, j), *arrayAt(fm, j));
                                    }

                                    if (!((state->ei < 0 && *arrayAt(fm, j) >= state->ei * *arrayAt(vn, k)) ||
                                        (state->ei >= 0 && *arrayAt(fm, j) <= state->ei * *arrayAt(vn, k)))) {
                                        
                                        *arrayAt(lr, i) = -4;
                                        *arrayAt(fq, i) = state->ei * *arrayAt(vn, k);
                                    }

                                } else if (*arrayAt(lr, i) == -4) {

                                    pn = state->tdr * *arrayAt(u, j) + (1.0 - state->tdr) * *arrayAt(old, j);

                                    if ((state->it + 1) % state->kod9 == 0) {
                                        fprintf(state->gs2stdout, "           %4d%15d               %15.5E%15.5E               %15.2F\n", i, *arrayAt(lr, i), pn, *arrayAt(fq, i), *arrayAt(coef, k));
                                    }

                                    if (state->ei <= 0) {
                                        if (pn <= state->pl) {

                                            *arrayAt(lr, i) = 4;
                                            *arrayAt(u, j) = state->pl;

                                        } else {

                                            if (abs(pn) > -0.001 * state->pl) {
                                                *arrayAt(coef, k) *= abs(state->pl / pn);
                                            } else {
                                                *arrayAt(coef, k) = 1.0;
                                            }

                                            *arrayAt(coef, k) = max(*arrayAt(coef, k), 1.0);
                                            *arrayAt(fq, i) = *arrayAt(coef, k) * state->ei * *arrayAt(vn, k);
                                        }

                                    } else if (pn >= 0) {

                                        *arrayAt(lr, i) = 4;
                                        *arrayAt(u, j) = 0;

                                    } else {

                                        if (abs(pn - state->pl) > -0.001 * state->pl) {
                                            *arrayAt(coef, k) *= abs(state->pl / (pn - state->pl));
                                        } else {
                                            *arrayAt(coef, k) = 1.0;
                                        }

                                        if (pn < state->pl) {
                                            *arrayAt(coef, k) = 1.0;
                                        }

                                        *arrayAt(coef, k) = max(*arrayAt(coef, k), 1.0);
                                        *arrayAt(fq, i) = *arrayAt(coef, k) * state->ei * *arrayAt(vn, k);
                                    }
                                }
                            }

                            if ((state->it + 1) % state->kod9 == 0) {
                                fprintf(state->gs2stdout, "\n\n           MODIFIED BOUNDARY CONDITIONS AT NEUMANN NODES\n           ---------------------------------------------\n");
                                fprintf(state->gs2stdout, "           NODE   TYPE OF B.C.        HEAD IN                       FLUX IN                      FRACTION");

                                for (k = 1; k <= state->nsdn; k++) {
                                    i = *arrayAt(nsf, k);
                                    j = i - *arrayAt(lc, i);

                                    if (*arrayAt(lr, i) == 4) {
                                        fprintf(state->gs2stdout, "           %4d%15d%15.5E\n", i, *arrayAt(lr, i), *arrayAt(u, j));
                                    } else if (*arrayAt(lr, i) == -4) {
                                        fprintf(state->gs2stdout, "           %4d%15d                              %15.5E               %15.2F\n")
                                    }
                                }
                            }
                        }

                        // Modify conditions on seepage faces
                        if ((state->nsdn == 0 || state->coefi != 1) || state->nseep != 0) {
                            fprintf(state->gs2stdout, "\n\n           CURRENT BOUNDARY CONDITIONS ON SEEPAGE FACES\n           --------------------------------------------\n");
                            fprintf(state->gs2stdout, "           NODE   TYPE OF B.C.        HEAD IN       HEAD OUT        FLUX IN       FLUX OUT     SEEPAGE FACE\n");

                            for (k = 1; k <= state->nseep; k++) {
                                icheck = 0;
                                nt = *arrayAt(msp, k);

                                for (jj = 1; jj <= nt; jj++) {

                                    i = *matrixAt(nsp, jj, k);
                                    j = i - *arrayAt(lc, i);

                                    if (*arrayAt(lr, i) == -2) {

                                        *arrayAt(fm, j) -= *arrayAt(rt, j);
                                        pn = state->tdr * *arrayAt(u, j) + (1.0 - state->tdr) * *arrayAt(old, j);

                                        fprintf(state->gs2stdout, "           %4d%15d               %15.5E%15.5E%32d\n", i, *arrayAt(lr, i), pn, *arrayAt(fq, i), k);

                                        if (pn < 0.0) {
                                            icheck = 1;
                                        }

                                        if (icheck <= 0) {
                                            *arrayAt(lr, i) = 2;
                                            *arrayAt(u, j) = 0.0;
                                        }
                                    } else if (*arrayAt(lr, i) == 2) {

                                        fprintf(state->gs2stdout, "           %4d%15d%15.5E                              %15.5E%17d", i, *arrayAt(lr, i), *arrayAt(u, j), *arrayAt(fm, j), k);

                                        if (icheck > 0 || *arrayAt(fm, j) >= 0.0) {
                                            *arrayAt(lr, i) = -2;
                                            *arrayAt(fq, i) = 0.0;
                                            icheck = 1;
                                        }
                                    }
                                }
                            }

                            fprintf(state->gs2stdout, "\n\n           MODIFIED BOUNDARY CONDITIONS ON SEEPAGE FACES\n           ---------------------------------------------\n");
                            fprintf(state->gs2stdout, "           NODE   TYPE OF B.C.        HEAD IN                       FLUX IN                    SEEPAGE FACE\n");

                            for (k = 1; k <= state->nseep; k++) {
                                nt = *arrayAt(msp, k);

                                for (jj = 1; jj <= nt; jj++) {
                                    i = *matrixAt(nsp, jj, k);
                                    j = i - *arrayAt(lc, i);

                                    if (*arrayAt(lr, i) == 2) {
                                        fprintf(state->gs2stdout, "           %4d%15d%15.5E%62D\n", i, *arrayAt(lr, i), *arrayAt(u, j), k);
                                    } else if (*arrayAt(lr, i) == -2) {
                                        fprintf(state->gs2stdout, "           %4d%15d                              %15.5E               %17d\n", i, *arrayAt(lr, i), *arrayAt(fq, i), k);
                                    }
                                }
                            }
                        }
                    }

                    isk = 0;

                    if (state->iter1 == 1) {
                        stop = 1;
                    } else {
                        for (i = 1; i <= state->mm; i++) {
                            if (*arrayAt(est, i) != 0) {
                                un = 0.5 * (state->tdr * *arrayAt(u, i) + (1.0 - state->tdr) * *arrayAt(old, i) + *arrayAt(old, i));

                                if (abs((un - *arrayAt(est, i)) / *arrayAt(est, i)) > state->clos1) {
                                    isk++;
                                }
                            }
                            *arrayAt(est, i) = *arrayAt(u, i);
                        }

                        if (state->kod12 != 0) {
                            fprintf(state->gs2stdout, "\n\n\n\n           SOLUTION OF FLOW EQUATION AT ITERATION%5d     (ISK = %5d)\n           -------------------------------------------\n", nit, isk);
                            fprintf(state->gs2stdout, "\n           NODE     VALUE     NODE     VALUE     NODE     VALUE     NODE     VALUE     NODE     VALUE     NODE     VALUE\n");
                            
                            for (i = 1; i <= state->mm; i++) {
                                if (i % 6 == 1) {
                                    fprintf(state->gs2stdout, "\n           ");
                                }
                                fprintf(state->gs2stdout, "%4d  %10.3E   ", *arrayAt(lp, i), *arrayAt(u, i));
                            }
                            fprintf(state->gs2stdout, "\n");
                        }

                        if ((isk == 0 && ktcal > 0) || nit >= state->iter1) {
                            stop = 1;
                        } else {
                            for (i = 1; i <= state->mm; i++) {
                                *arrayAt(est, i) = 0.5 * (state->tdr * *arrayAt(u, i) + (1.0 - state->tdr) * *arrayAt(old, i) + *arrayAt(old, i));
                            }
                        }
                    }

                } while (!stop);

                // Calculate new values
                for (i = 1; i <= state->nn; i++) {
                    if (*arrayAt(lr, i) != 1) {
                        j = i - *arrayAt(lc, i);
                        *arrayAt(phi, i) = state->tdr * *arrayAt(u, j) + (1.0 - state->tdr) * *arrayAt(old, j);
                    } else {
                        *arrayAt(phi, i) = *arrayAt(phii, i);
                    }
                }

                if (state->stat < 0) {
                    delt1 = deltgo / 3600;
                    state->oldt = state->delt;
                    ktcal++;
                    state->it = ktcal;
                    state->ssec += deltgo;
                    smin = state->ssec / 60;
                    state->stime = smin / 60;
                }
            }
        }

        if (!advanc || state->stat >= 0) {
                
            // Select approximation for time derivative
            a3 = state->tdr / state->delt;

            // rewind 4

            if (advanc) {

                // Generate matrices for concentration
                jtest = 0;

                gs2Cogen(state, s, p, fm, rt, phi, phii, u, old, cfm, crt, conc, conci, fx, cn, est, fq, cfq,
                         x, y, fmobx, fmoby, por, elong, etrans, alpha, tta, kd, lambda, rho, in, kf, lr, klr,
                         lc, klc, ie, jtest);

                jtest = -1;
            }

            // Update cold
            if (ktcal != 0) {
                gs2UPD(conc, cold, klr, klc, state->nn);
            }

            // Add time-dependent part
            if (advanc || state->it % state->itchng == 0) {
                
                if (state->stat != 0) {
                    if (jtest >= 0) {
                        kb1 = state->knb - state->kmb + 1;
                        
                        // rewind 2
                        // read (2) ((S(I, J), J = 1, KM), I = KB1, KMB2)
                    }

                    gs2Lrhs(s, p, cfm, cold, crt, klc, state->nn, state->knb, state->kmb, a3, 0.0, 2);
                }

                if (state->kod7 - 1 > 0) {
                    fprintf(state->gs2stdout, "\n\n1 GLOBAL COEFFICIENT MATRIX FOR CONCENTRATION\n  INPUT TO SOLVER\n\n\n");
                    gs2Sos(s, state->kmb2, state->km, state->knb - state->kmb + 1);
                    fprintf(state->gs2stdout, "0          P COEFFICIENT MATRIX\n           --------------------\n\n\n");
                    gs2Sos(p, state->km, state->kmb, 1);
                }

                gs2Array(s, w, state->km, state->knb, state->kmb, state->kmb2, (state->memoryRequirements).maxbw2, 
                         (state->memoryRequirements).maxs, (state->memoryRequirements).mx, &jx);

                // write (4) (W(I), I = 1, JX)

            } else {

                // read (4) (W(I), I = 1, JX)
            
            }

            if (state->stat != 0) {
                
                gs2Lrhs(s, p, cfm, cold, crt, klc, state->nn, state->knb, state->kmb, a3, 0.0, 1);
                
                if (state->kod7 >= 1) {
                    // write (6, 1325)
                    // write (6, 1398) (CFM(I), I = 1, KM)
                }
            }

            if (jtest < 0) {
                jtest = 1;
                if (ktcal == 0) {
                    // write (6, 1630) KMB, KMB2
                }
            }

            for (i = 1; i <= state->nn; i++) {
                if (*arrayAt(klr, i) != 1) {
                    j = i - *arrayAt(klc, i);
                    *arrayAt(cfm, j) += *arrayAt(crt, j);
                }
            }

            if (state->kod7 >= 1) {
                // write (6, 1320)
                // write (6, 1398) (CFM(I), I = 1, KM)
            }

            // Solve concentration equation
            gs2Gelb(cfm, w, state->km, 1, state->kmb - 1, state->kmb - 1, 1.e-20, &ier);

            if (ier != 0) {
                // write (6, 1800) IER
                state->istop++;
                return;
            }

            ktcal++;
            state->oldt = state->delt;
            state->ssec += state->delt;
            smin = state->ssec / 60;
            state->stime = smin / 60;
            state->it++;

            // Calculate new values
            for (i = 1; i <= state->nn; i++) {
                if (*arrayAt(klr, i) <= 0) {
                    j = i - *arrayAt(klc, i);
                    *arrayAt(&(state->conc), i) = state->tdr * *arrayAt(cfm, j) + (1.0 - state->tdr) * *arrayAt(cold, j);
                } else {
                    *arrayAt(&(state->conc), i) = *arrayAt(&(state->conci), i);
                }
            }

            delt1 = state->delt / 3600;
        }

        // Write computed values
        if (state->it % state->kod9 == 0 || state->it % state->kod10 == 0) {
            // write (6, 1051) RDATE, RTIME
            // write (6, 1360) IT, DELT1, STIME, SMIN, SSEC
        }

        if (state->it % state->kod9 == 0) {
            // write (6, 1380)
            // write (6, 1080)
            // write (6, 1090) (I, PHI(I), I = 1, NN)
        }

        if (state->stat >= 0) {
            if (state->it % state->kod10 == 0) {
                // write (6, 1390)
                // write (6, 1080)
                // write (6, 1090) (I, CONC(I), I = 1, NN)
            }

            // write (6, 1400)
            // write (6, 1410)
        }

        if (state->kod11 != 0) {
            if (state->it % state->kod11 == 0) {
                gs2Po(state, conc, phi, state->nn, state->it, state->stime, rdate, rtime);
            }
        }

    } while (state->it < state->itmax);

    // write (6, 1430) IT

    if (state->kod11 != 0) {
        if (state->it % state->kod11 != 0) {
            gs2Po(state, conc, phi, state->nn, state->it, state->stime, rdate, rtime);
        }
    }
}

#include "green.h"

#include "../capstone/Debug.h"

/*
    Purpose:
        Evaluates the one-dimensional shape functions and determinants for each element.
        Determines if the side is linear, quadratic, or cubic.

    Called from QZ
*/

void gs2Green(Array* x, Array* y, Array* detj, Array* ag, Matrix* in,
              Array* kf, Array* jd, Array* ieq, int* ms, int me, int np,
              int l, int* istop){

            DEBUG_LOG("green called");

      int ic = 0, ii = 0, i = 0;
      // int jn[4]; written to but never read
      // all writes to jn have been commented out
      Array kn, js;
      Matrix dlx;
      // double g[4][4], alf[2]; written to but never read in this function
      //  all writes to g have been commented out
      //  all read/writes to alf commented out
      //  alf only read into g

      arrayDimension(&kn, 2);
      arrayDimension(&js, 4);
      matrixDimension(&dlx, 4, 4);

      *arrayAt(&kn, 1) = 1;

      while(*arrayAt(&kn, 1) < 4){
            (*arrayAt(&kn, 1))++;

            if (*arrayAt(kf, l) == ic){ // else increment ic and try again (go to 300)
                *arrayAt(&kn, 2) = *arrayAt(&kn, 1) + 1;

                if (*arrayAt(&kn, 1) == 4)
                    *arrayAt(&kn, 2) = 1;

                ii = 0;
                i = 0;

                for (int k = 1; k <= 2; k++){
                    while(*arrayAt(&kn, k) != i){
                        if (i >= 4) {
                            (*istop)++;
                            return;
                        }
                        i++;
                    }

                    int knk = (int) *arrayAt(&kn, k);
                    //jn[ii] = *arrayAt(jd, knk);
                    *arrayAt(&js, ii) = *arrayAt(jd, knk);
                    *arrayAt(ieq, ii) = i;
                    ii++;
                    i = 0;
                } // 10

                int ip = 2;
                int iq = 2;
                int id = (2 * (*arrayAt(&kn, 0)) + 2);

                for(int j = 1; j <= 2; j++){
                    ip++;
                    id++;
                    // Never used by the function
                    // jn[ip] = *matrixAt(in, id, l);
                    if (*matrixAt(in, id, l) != 0){
                        ii++;
                        *arrayAt(&js, ii) = *matrixAt(in, id, l);
                    }
                } // 20

                (*ms) = ii;

                if ((*ms) != 2){
                    for (int k = 3; k <= (*ms); k++){
                        while( (*arrayAt(jd, iq)) != *arrayAt(&js, k) )
                            iq++;

                        (*arrayAt(ieq, k)) = iq; // 30
                    } // 35
                }

                // np is either 2 or 4
                for (int k = 1; k <= np; k++) {
                    double xi = *arrayAt(ag, k);

                    if (np > 2){
                        xi = *arrayAt(ag, k+2);
                    }

                    // xi1 commented out because unused
                    // double xi1 = 1.0 - xi;
                    double xi2 = 1.0 + xi;

                    // Corner node shape function, basic part
                    // 		Commented to out because only used by g[][]
                    //alf[0] = 0.5 * xi1;
                    //alf[1] = 0.5 * xi2;

                    // Determine if side is linear, quadratic, or cubic
                    if ((*ms) == 3){
                        // quadratic side
                        //g[0][k] = (-xi * alf[0]);
                        //g[1][k] = (xi * alf[1]);
                        //g[2][k] = (1.0 - (xi * xi));
                        *matrixAt(&dlx, 1, k) = xi - 0.5;
                        *matrixAt(&dlx, 2, k) = xi + 0.5;
                        *matrixAt(&dlx, 3, k) = -2.0 * xi;
                    }
                    else if ((*ms) == 4){
                        // Cubic side
                        //g[0][k] = alf[0] * (1.125 * xi2 - 0.125);
                        //g[1][k] = alf[1] * (1.125 * xi2 - 0.125);
                        //g[2][k] = 0.5625 * (1.0 - xi2) * (1.0 - 3.0 * xi);
                        //g[3][k] = 0.5625 * (1.0 - xi2) * (1.0 + 3.0 * xi);
                        *matrixAt(&dlx, 1, k) = 0.0625 * (18.0 * xi - 27.0 * xi2 + 1.0);
                        *matrixAt(&dlx, 2, k) = 0.0625 * (18.0 * xi + 27.0 * xi2 - 1.0);
                        *matrixAt(&dlx, 3, k) = 0.5625 * (9.0 * xi2 - 2.0 * xi - 3.0);
                        *matrixAt(&dlx, 4, k) = 0.5625 * (-9.0 * xi2 - 2.0 * xi + 3.0);
                    }
                    else{
                        // Linear side
                        //g[0][k] = alf[0];
                        //g[1][k] = alf[1];
                        *matrixAt(&dlx, 1, k) = -0.5;
                        *matrixAt(&dlx, 2, k) = 0.5;
                    }

                    // Jacobians
                    double sumx = 0.0;
                    double sumy = 0.0;

                    for (int li = 1; li <= (*ms); li++){
                        int ks = (int) *arrayAt(&js, li);
                        sumx += (*matrixAt(&dlx, li, k) * (*arrayAt(x, ks)));
                        sumy += (*matrixAt(&dlx, li, k) * (*arrayAt(y, ks)));
                    } // 60

                    *arrayAt(detj, (k + np)) = sumy;
                    *arrayAt(detj, k) = sumx;
                } // 80
            } // end if (kf[l] == ic), label 300

            ic++;
      } // end outer while loop

}

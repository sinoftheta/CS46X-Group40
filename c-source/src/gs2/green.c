#include "green.h"

void gs2Green(Array* x, Array* y, Array* detj, Array* ag, Matrix* in,
              Array* kf, Array* jd, Array* ieq, int* ms, int me, int np,
              int l, int* istop){

      int kn[2], js[4];
      // int jn[4]; written to but never read
      // all writes to jn have been commented out
      double dlx[4][4];
      // double g[4][4], alf[2]; written to but never read in this function
      //	all writes to g have been commented out
      //	all read/writes to alf commented out
      //		alf only read into g
      int ic = 0, ii = 0, i = 0;

      kn[0] = 0;

      while(kn[0] < 4){

        if (*arrayAt(kf, l) == ic){ // else increment ic and try again (go to 300)
          kn[1] = kn[0] + 1;

          if (kn[0] == 3)
            kn[1] = 1;

          ii = 0;
          i = 0;

          for (int k = 0; k < 2; k++){ // changed loop from k = 1; k <= 2
            while(kn[k] != i){
              if (i > 3){
                (*istop)++;
                return;
              }
              i++;
            }

            int knk = kn[k];
            //jn[ii] = *arrayAt(jd, knk);
            js[ii] = *arrayAt(jd, knk);
            *arrayAt(ieq, ii) = i;
            ii++;
            i = 0;
          } // 10

          // ???
          /* ii == 2 here */
          //ii--; // -1 because accesses js[] next (line 55), js.length == 4

          int ip = 2;   // Might need to change to 1
          int iq = 2;   // Might need to change to 1
          int id = (2 * kn[0] + 2); // Might need to change to kn[0] + 1

          for(int j = 0; j < 2; j++){ // changed loop from j = 1; j <= 2
            ip++;
            id++;
	    // Never used by the function
            // jn[ip] = *matrixAt(in, id, l);
            if (*matrixAt(in, id, l) != 0){
              ii++;
              js[ii] = *matrixAt(in, id, l);  // will probably seg fault or access random mem not in js[]
            }
          } // 20

          (*ms) = ii;

          if ((*ms) != 2){
            for (int k = 3; k <= (*ms); k++){
              while( (*arrayAt(jd, iq)) != js[k] )
                iq++;

              (*arrayAt(ieq, k)) = iq; // 30
            } // 35
          }

          // np is either 2 or 4
          for (int k = 0; k < np; k++){ // changed from k = 1; k <= np
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
              dlx[0][k] = xi - 0.5;
              dlx[1][k] = xi + 0.5;
              dlx[2][k] = -2.0 * xi;
            }
            else if ((*ms) == 4){
              // Cubic side
              //g[0][k] = alf[0] * (1.125 * xi2 - 0.125);
              //g[1][k] = alf[1] * (1.125 * xi2 - 0.125);
              //g[2][k] = 0.5625 * (1.0 - xi2) * (1.0 - 3.0 * xi);
              //g[3][k] = 0.5625 * (1.0 - xi2) * (1.0 + 3.0 * xi);
              dlx[0][k] = 0.0625 * (18.0 * xi - 27.0 * xi2 + 1.0);
              dlx[1][k] = 0.0625 * (18.0 * xi + 27.0 * xi2 - 1.0);
              dlx[2][k] = 0.5625 * (9.0 * xi2 - 2.0 * xi - 3.0);
              dlx[3][k] = 0.5625 * (-9.0 * xi2 - 2.0 * xi + 3.0);
            }
            else{
              // Linear side
              //g[0][k] = alf[0];
              //g[1][k] = alf[1];
              dlx[0][k] = -0.5;
              dlx[1][k] = 0.5;
            }

            // Jacobians
            double sumx = 0.0;
            double sumy = 0.0;

            for (int li = 0; li < (*ms); li++){  // changed from li = 1; li <= ms
              int ks = js[li];
              sumx += (dlx[li][k] * (*arrayAt(x, ks)));
              sumy += (dlx[li][k] * (*arrayAt(y, ks)));
            } // 60

            *arrayAt(detj, (k + np)) = sumy;
            *arrayAt(detj, k) = sumx;
          } // 80
        } // end if (kf[l] == ic), label 300

        ic++;
        kn[0]++;  // moved from top because we need to start at 0, not 1
      } // end outer while(kn[0] < 4)

}

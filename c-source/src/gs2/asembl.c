#include "asembl.h"
#include <stdio.h>
#include <stdlib.h>

#include "../capstone/Debug.h"

/*
    Purpose
      To assemble element matrices

      Called from cogen
*/

void gs2Asembl(Matrix* a, Matrix* b, Matrix* ea, Matrix* eb, Array* r,
              Array* u, Array* re, Array* F, Array* k0d, Array* lq,
              Array* jd, int m, int l, int ib, int ib2, int* jb,
              int* jb2, int* ISTOP){
      matrixAssertNotNull(a, "Matrix 'a' NULL in gs2Asembl!");
      matrixAssertNotNull(b, "Matrix 'b' NULL in gs2Asembl!");
      matrixAssertNotNull(ea, "Matrix 'ea' NULL in gs2Asembl!");
      matrixAssertNotNull(eb, "Matrix 'eb' NULL in gs2Asembl!");
      arrayAssertNotNull(r, "Array 'r' NULL in gs2Asembl!");
      arrayAssertNotNull(u, "Array 'u' NULL in gs2Asembl!");
      arrayAssertNotNull(F, "Array 'F' NULL in gs2Asembl!");
      arrayAssertNotNull(k0d, "Array 'k0d' NULL in gs2Asembl!");
      arrayAssertNotNull(lq, "Array 'lq' NULL in gs2Asembl!");
      arrayAssertNotNull(jd, "Array 'jd' NULL in gs2Asembl!");

      DEBUG_LOG("asembl called");

      int ih, jdi, ir, jdj, jc;

      ih =  (ib2 - ib + 1);


      for (int iter = 1; iter <= m; iter++){ //loop through all materials

        // current node
        jdi = *arrayAt(jd, iter);

        // index of equation for current node once deleted from flow system
        int temp = *arrayAt(lq, jdi);

        // index for value in flow equation
        ir = (jdi - temp);

        if (*arrayAt(k0d, jdi) != 1){
          *arrayAt(r, ir) += *arrayAt(re, iter);

          for (int iter2 = 1; iter2 <= m; iter2++){ // inner loop if k0d(jdi) != 1
            jdj = *arrayAt(jd, iter2);

            if (*arrayAt(k0d, jdj) != 1){
              jc = (jdj - ir + 1 - *arrayAt(lq, jdj));

              if (jc > ib){
                fprintf(stderr,
                  "Insufficient half-bandwidth\n\t\tElement %5d requires %5d instead of %5d", l, jc, ib);
                (*ISTOP)++;
                return;
              }
              else { // jc <= ib

                if ( jc >= 1){
                  *matrixAt(b, ir, jc) += *matrixAt(eb, iter, iter2);
                  if (jc > *jb) *jb = jc;
                }

                int nc = (ih + jc - 1); // 30

                if (nc > ib2){
                  fprintf(stderr,
                    "Insufficient bandwidth\n\t\tElement %5d requires %5d instead of %5d", l, nc, ib2);
                    (*ISTOP)++;
                    return;
                }

                if (nc >= 1){
                  *matrixAt(a, nc, ir) += *matrixAt(ea, iter, iter2);
                  if ( nc > *jb2) *jb2 = nc;
                }
              }
            } // end if (k0d[jdj] != 1)
          } // 50
        } // end if (k0d[jdi] != 1)
        else if(*arrayAt(k0d, jdi) == 1){

          for(int iter2 = 1; iter2 <= m; iter2++){
            // get current node
            jdj = *arrayAt(jd, iter2);
            if(*arrayAt(k0d, jdj) <= 0){
              jc = jdj - *arrayAt(lq, jdj);
              *arrayAt(u, jc) -= (*matrixAt(ea, iter2, iter) * *arrayAt(F, jdi));
            }
          } // 70

        }

      } // end outer loop through materials
}

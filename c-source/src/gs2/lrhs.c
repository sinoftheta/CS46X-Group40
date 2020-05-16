#include <stdlib.h>
#include <stdio.h>

#include "lrhs.h"

#include "../capstone/Debug.h"


void gs2Lrhs(Matrix* a, Matrix* b, Array* r, Array* rold,
          Array* u, Array* lq, int m, int ib, int jb, double a3,
          double a2, int kk){

   matrixAssertNotNull(a, "Matrix 'a' NULL in gs2Lrhs");
   matrixAssertNotNull(b, "Matrix 'b' NULL in gs2Lrhs");
   arrayAssertNotNull(r, "Array 'r' NULL in gs2Lrhs");
   arrayAssertNotNull(rold, "Array 'rold' NULL in gs2Lrhs");
   arrayAssertNotNull(u, "Array 'u' NULL in gs2Lrhs");
   arrayAssertNotNull(lq, "Array 'lq' NULL in gs2Lrhs");

  DEBUG_LOG("lrhs called");

  // matrixPrint("b", b);
  // matrixPrint("a", a);

   int l1, ll, jm, jn;
   int mq = m - *arrayAt(lq, m);
   int m2 = mq - 1;


    if (kk - 2 < 0){
      DEBUG_LOG("case 1");
     // kk = 1 : Form right-hand side of equations
      for(int iter = 1; iter <= mq; iter++){
       *arrayAt(r, iter) = 0.0;
      }

      *arrayAt(r, mq) += (*matrixAt(b, mq, 1)) * (*arrayAt(rold, mq) * a3 +
                        *arrayAt(u, mq) * a2);

      if (m2 > 0){
        for(int iter = 1; iter <= m2; iter++){

         l1 = min(jb, mq + 1 - iter);

         *arrayAt(r, iter) += (*matrixAt(b, iter, 1) * (*arrayAt(rold, iter) *
            a3 + *arrayAt(u, iter) * a2));

            for(int iter2 = 2; iter2 <= l1; iter2++){

              ll = iter + iter2 - 1;

              if(ll <= mq){
                *arrayAt(r, iter) += (*matrixAt(b, iter, iter2) *
                  (*arrayAt(rold, ll) * a3 + *arrayAt(u, ll) * a2));

                *arrayAt(r, ll) += (*matrixAt(b, iter, iter2) *
                  (*arrayAt(rold, iter) * a3 + *arrayAt(u, iter) * a2));
              }
            }
        } // 50
      }
    }
    else if (kk - 2 == 0){ // 90
      DEBUG_LOG("case 2");
      // kk = 2 : Form left-hand side of equations for mass transport
      *matrixAt(a, ib, mq) += (*matrixAt(b, mq, 1) * a3);
      if(m2 > 0){
        for(int iter = 1; iter <= m2; iter++){

          l1 = min(jb, (mq + 1 - iter));

          *matrixAt(a, ib, iter) += (*matrixAt(b, iter, 1) * a3);

          for(int iter2 = 2; iter2 <= l1; iter2++){

            ll = (iter + iter2 - 1);
            jm = ib - iter2 + 1;
            jn = ib + iter2 - 1;

            *matrixAt(a, jn, iter) += (*matrixAt(b, iter, iter2) * a3);

            if(ll <= mq)
              *matrixAt(a, jm, ll) += (*matrixAt(b, iter, iter2) * a3);
          }
        }
      } // 100
    }
    else { // 170
      DEBUG_LOG("case 3");
      // kk = 3 : Form left-hand side of equations for flow
      *matrixAt(b, mq, 1) = (*matrixAt(a, 1, mq) + (*matrixAt(b, mq, 1) * a3));

      if(m2 > 0){

        for(int iter = 1; iter <= m2; iter++){
          l1 = min(jb, mq + 1 - iter);

          *matrixAt(b, iter, 1) = *matrixAt(a, 1, iter) +
            (*matrixAt(b, iter, 1) * a3);

          for(int iter2 = 2; iter2 <= l1; iter2++){
            *matrixAt(b, iter, iter2) = (*matrixAt(a, iter2, iter) +
                (*matrixAt(b, iter, iter2) * a3));
          }
        }
      }
    }

}

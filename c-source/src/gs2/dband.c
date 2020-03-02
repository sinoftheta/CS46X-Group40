#include "dband.h"
#include "gs2.h"
#include <stdio.h>
#include <math.h>

void gs2Dband(Matrix* s, int n, int nb, int* iex){

    matrixPrint("s", s);

    *iex = 0;
    for (int i = 1; i <= n; i++) {
        int ip = n - i + 1;
        if (nb < ip)
            ip = nb;

        for (int j = 1; j <= ip; j++) {
            int iq = nb - j;
            if (i - 1 < iq)
                iq = i - 1;

            double sum = *matrixAt(s, i, j);

            for (int k = 1; k <= iq; k++) {
                int jz = j + k;
                int ii = i - k;
                double siik = *matrixAt(s, ii, k + 1);
                double siijz = *matrixAt(s, ii, jz);

                if (siijz == 0.0)
                    continue;
                
                sum -= siik*siijz;
            } // end for k

            if (j == 1) {
                if (sum <= 0) {
                    fprintf(gs2stderr, "Dband fails at row %d\n", i); 
                    fprintf(gs2stderr, "N: %d, NB: %d, IP: %d, IQ: %d, I: %d, J: %d, SUM: %f\n",
                        n, nb, ip, iq, i, j, sum
                    );
                    
                    iex = (int*)0;
                    *iex = 1;
                    return;
                }

                double temp = 1.0 / sqrt(sum);
                *matrixAt(s, i, j) = sum * temp;
                continue;
            }
        } // end for j
    } // end for i
}

#include "dband.h"
#include "gs2.h"
#include <stdio.h>
#include <math.h>

void gs2Dband(Matrix* s, int n, int nb, int* iex){

    printf("n = %d, nb = %d\n", n, nb);

    *iex = 0;
    int ip, iq, ii, jz;

    double siik, siijz, sum, temp;

    sum = 0.0f;
    temp = 1.0f;

    for(int i = 1; i <= n; i++){ 

        ip = n - i + 1;
        if(nb < ip) 
            ip = nb;

        for(int j = 1; j <= ip; j++){ 

            iq = nb - j;
            
            if ((i - j) < iq) 
                iq = i - 1;

            sum = *matrixAt(s, i, j);

            if(iq >= 1){ // if(iq < 1) go to 20

                for(int k = 1; k <= iq; k++){

                    ii = i - k;
                    siik = *matrixAt(s, ii, k+1);

                    jz = j + k;
                    siijz = *matrixAt(s, ii, jz);
                    // matches if(siijz == 0.0) go to 10 in the fortran source
                    if (siijz != 0.0) { 

                        sum = sum - siik * siijz;
                    }
                }
                //10
            }
            // 20
        
            // captures behavior of, if(j != 1) go to 40, in the fortran source
            if (j == 1) {  
                        
                // captures behavior of, if(sum <= 0.0) go to 30, in the fortran source
                if(sum > 0.0){ 
                    temp = 1.0 / sqrt(sum);
                    *matrixAt(s, i, j) = temp;
                } else {
                    //30

                    //write error messages
                    fprintf(gs2stderr, "Dband fails at row %d\n", i); 
                    fprintf(gs2stderr, "N: %d, NB: %d, IP: %d, IQ: %d, I: %d, J: %d, SUM: %f\n",
                        n, nb, ip, iq, i, j, sum
                    );
                    
                    *iex = 1;
                    return;
                }
            }
            //40
            *matrixAt(s, i, j) = sum * temp;
        }
    }
    // 50
}

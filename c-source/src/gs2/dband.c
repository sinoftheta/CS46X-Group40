#include "dband.h"

void gs2Dband(Matrix* s, int n, int nb, int ndim, int mdim, int* iex){

    *iex = 0;
    int ip, iq, ii, jz;

    double siik, siijz, sum, temp;

    for(int i = 1; i <= n; i++){ // unsure if logic should be: i <= n, or i < n 

        ip = n - i + 1;
        if(nb < ip) ip = nb;

        for(int j = 1; j <= ip; j++){ // again, unsure

            iq = nb - j;
            if((i - 1) < iq) iq = i - 1;

            sum = *matrixAt(s, i, j);

            if(!(iq < 1)){ // if(iq < 1) go to 20

                for(int k = 1; k <= iq; k++){ // again, unsure

                    ii = i - k;
                    siik = *matrixAt(s, ii, k+1);

                    jz = j + k;
                    siijz = *matrixAt(s, ii, jz);

                        if(!(siijz == 0.0)){ // if(siijz == 0.0) go to 10

                            sum = sum - siik * siijz;
                        }
                }
                //10
            }
            // 20
        
            if(!(j != 1)){ // if(j != 1) go to 40
                        
                if(!(sum <= 0.0 )){ // if(sum <= 0.0) go to 30
                    temp = 1 / sqrt(sum);
                }
                //30

                //write error messages
                //change to fprintf? write to stderr? need file pointer
                printf("Dband fails at row %d\n", i); 
                printf("N: %d, NB: %d, IP: %d, IQ: %d, I: %d, J: %d, SUM: %f\n",
                    n, nb, ip, iq, i, j, sum
                );
                
                *iex = 1;
                return;
            }
            //40
            *matrixAt(s, i, j) = temp;
        }
    }
    // 50
}
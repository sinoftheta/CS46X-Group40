#include "po.h"
#include <stdio.h>

void gs2Po(gs2State* state, Array* conc, Array* phi, int nn, int it, double stime, char* rdate, char* rtime) {

    /*
     * Purpose: To generate restart data files
     *          Pressures are written to TAPE7
     *          Concentrations are written to TAPE8
     */

    int i;

    fprintf(state->tape8, "1 RUN IDENTIFICATION: %.10s %.10s\n", rdate, rtime);
    fprintf(state->tape8, " CONCENTRATION OUTPUT AT TIME STEP:%4d\n", it);
    fprintf(state->tape8, " TIME (HOURS):        %15.5G\n", stime);
    
    for (i = 1; i <= nn; i++) {
        if (i % 4 == 1 && i != 1) {
            fprintf(state->tape7, "\n")
        }
        fprintf(state->tape8, "%5d,%15.8G", i, *arrayAt(conc, i));
    }
    fprintf(state->tape8, "\n");

    fprintf(state->tape7, "1 RUN IDENTIFICATION: %.10s %.10s\n", rdate, rtime);
    fprintf(state->tape7, " PRESSURE HEAD OUTPUT AT TIME STEP:%4d\n", it);
    fprintf(state->tape7, " TIME (HOURS):   %15.5G\n", stime);

    for (i = 1; i <= nn; i++) {
        if (i % 4 == 1 && i != 1) {
            fprintf(state->tape7, "\n")
        }
        fprintf(state->tape7, "%5d,%15.8G", i, *arrayAt(phi, i));
    }
    fprintf(state->tape7, "\n");

}
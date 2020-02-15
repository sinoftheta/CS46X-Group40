#include "po.h"
#include <stdio.h>

void gs2Po(gs2State* state, Array* conc, Array* phi, int nn, int it, double stime, char* rdate, char* rtime) {

    /*
     * Purpose: To generate restart data files
     *          Pressures are written to TAPE7
     *          Concentrations are written to TAPE8
     */

    int i;

    fprintf(state->tape8, "1 RUN IDENTIFICATION: %s %s\n", rdate, rtime);
    fprintf(state->tape8, "CONCENTRATION OUTPUT AT TIME STEP: %d\n", it);
    fprintf(state->tape8, "TIME (HOURS): %15.5f\n", stime);
    
    for (i = 1; i <= nn; i++) {
        fprintf(state->tape8, "%5d,%15.8f", i, *arrayAt(conc, i));
    }
    fprintf(state->tape8, "\n");

    fprintf(state->tape7, "1 RUN IDENTIFICATION: %s %s\n", rdate, rtime);
    fprintf(state->tape7, "PRESSURE HEAD OUTPUT AT TIME STEP: %d\n", it);
    fprintf(state->tape7, "TIME (HOURS): %15.5f\n", stime);

    for (i = 1; i <= nn; i++) {
        fprintf(state->tape7, "%5d,%15.8f", i, *arrayAt(phi, i));
    }
    fprintf(state->tape7, "\n");

}
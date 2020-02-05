#include "ics1cu.h"

#include <gsl/gsl_bspline.h>
#include <gsl/gsl_multifit.h>

#include <stdio.h>

void gs2ICS1CU(
    Array* gs2xs, 
    Array* gs2ys, 
    int dataPointCount, 
    int coefficentCount,
    Matrix* coefficentMatrix,
    int* ier
) {

    // 4 represents a cubic spline
    const size_t k = 4;
    const size_t nbreak = coefficentCount + 2 - k;

    gsl_bspline_workspace* bw;
    gsl_vector* B;
    gsl_vector* xs;
    gsl_vector* ys;
    gsl_matrix* X;

    bw = gsl_bspline_alloc(k, nbreak);

    B = gsl_vector_alloc(coefficentCount);
    xs = gsl_vector_alloc(dataPointCount);
    ys = gsl_vector_alloc(dataPointCount);

    X = gsl_matrix_alloc(dataPointCount, coefficentCount);

    double maxXI = 0.0;
    double minXI = 0.0;
    for (int i = 0; i < dataPointCount; i++) {
        double xi = *arrayAt(gs2xs, i + 1);
        double yi = *arrayAt(gs2ys, i + 1);

        gsl_vector_set(xs, i, xi);
        gsl_vector_set(ys, i, yi);

        maxXI = xi > maxXI ? xi : maxXI; 
        minXI = xi < minXI ? xi : minXI;
    }

    gsl_bspline_knots_uniform(minXI-0.02, maxXI+0.02, bw);

    for (int i = 0; i < dataPointCount; i++) {
        double xi = gsl_vector_get(xs, i);
        
        gsl_bspline_eval(xi, B, bw);

        for (int j = 0; j < coefficentCount; j++){
          double Bj = gsl_vector_get(B, j);
          gsl_matrix_set(X, i, j, Bj);
        }
    }



    for (int i = 0; i < dataPointCount; i++) {
        for (int j = 0; j < coefficentCount; j++) {
            *matrixAt(coefficentMatrix, j+1, i+1) = gsl_matrix_get(X, i, j);
        }
    }


    gsl_bspline_free(bw);
    gsl_vector_free(xs);
    gsl_vector_free(ys);
    gsl_vector_free(B);
    gsl_matrix_free(X);
}
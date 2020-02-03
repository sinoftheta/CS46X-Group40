#include "ics1cu.h"

#include <gsl/gsl_interp.h>

#include <stdio.h>

void gs2ICS1CU(
    Array* xs, 
    Array* ys, 
    int ispk, 
    Matrix* cc,
    int material,
    int* ier
) {

    gsl_interp* interp = gsl_interp_alloc(gsl_interp_cspline, ispk);

    *ier = gsl_interp_init(interp, xs->elements, ys->elements, xs->size);

    gsl_interp_accel *acc = gsl_interp_accel_alloc();

    // constant
    double d = gsl_interp_eval(interp, xs->elements, ys->elements, 0.0, acc);
    // linear
    double c = gsl_interp_eval_deriv(interp, xs->elements, ys->elements, 0.0, acc);
    // quadratic
    double b = 0.5 * gsl_interp_eval_deriv2(interp, xs->elements, ys->elements, 0.0, acc);
    // cubic
    double a = gsl_interp_eval(interp, xs->elements, ys->elements, 1.0, acc) - b - c - d;

   
    *matrixAt(cc, 3, material) = a;
    *matrixAt(cc, 2, material) = b;
    *matrixAt(cc, 1, material) = c;

    gsl_interp_accel_free(acc);
    gsl_interp_free(interp);
}
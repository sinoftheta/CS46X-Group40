#include "shape.h"

void gs2Shape(Array* x, Array*  y, Matrix* in, int l, int m, double xi, double yi, Array* f,
              double* det, Array* dgx, Array* dgy, int maxnn, int maxne, int mxc, int me) {

    // Purpose: To evaluate 2-D shape functions and determinant
    
    arrayAssertNotNull(x, "Array 'x' NULL in gs2Shape!");
    arrayAssertNotNull(y, "Array 'y' NULL in gs2Shape!");
    arrayAssertNotNull(f, "Array 'f' NULL in gs2Shape!");
    arrayAssertNotNull(dgx, "Array 'dgx' NULL in gs2Shape!");
    arrayAssertNotNull(dgy, "Array 'dgy' NULL in gs2Shape!");
    matrixAssertNotNull(in, "Matrix 'in' NULL in gs2Shape!");

    Array alf, dax, day, btx, bty, dbx, dby, dfx, dfy;
    arrayDimension(&alf, 4);
    arrayDimension(&dax, 4);
    arrayDimension(&day, 4);
    arrayDimension(&btx, 4);
    arrayDimension(&bty, 4);
    arrayDimension(&dbx, 4);
    arrayDimension(&dby, 4);
    arrayDimension(&dfx, 12);
    arrayDimension(&dfy, 12);

    double xi1, xi2, yi1, yi2;
    double xq1, xq2, yq1, yq2;
    double xc1, xc2, yc1, yc2;
    double xeq, yeq, xe1, xe2, ye1, ye2;
    double sum1, sum2, sum3, sum4;
    double det1, c11, c12, c21, c22;
    int i, j, k, ki;
    int j1, j2, j3;

    xi1 = 1.0 - xi;
    xi2 = 1.0 + xi;
    yi1 = 1.0 - yi;
    yi2 = 1.0 + yi;

    // Corner node shape functions, basic part
    *arrayAt(&alf, 0) = 0.25 * xi1 * yi1;
    *arrayAt(&alf, 1) = 0.25 * xi2 * yi1;
    *arrayAt(&alf, 2) = 0.25 * xi2 * yi2;
    *arrayAt(&alf, 3) = 0.25 * xi1 * yi2;
    *arrayAt(&dax, 0) = -0.25 * yi1;
    *arrayAt(&dax, 1) = 0.25 * yi1;
    *arrayAt(&dax, 2) = 0.25 * yi2;
    *arrayAt(&dax, 3) = -0.25 * yi2;
    *arrayAt(&day, 0) = -0.25 * xi1;
    *arrayAt(&day, 1) = -0.25 * xi2;
    *arrayAt(&day, 2) = 0.25 * xi2;
    *arrayAt(&day, 3) = 0.25 * xi1;

    // Corner node shape functions, side-dependent part
    xq1 = xi - 0.5;
    xq2 = -xi - 0.5;
    yq1 = yi - 0.5;
    yq2 = -yi - 0.5;
    xc1 = 1.125 * xi * xi - 0.625;
    xc2 = 2.25 * xi;
    yc1 = 1.125 * yi * yi - 0.625;
    yc2 = 2.25 * yi;

    j1 = 0;
    j2 = 1;
    j3 = 4;
    for (i = 0; i < 2; i++) {
        if (*matrixAt(in, j3, l) == 0) {

            *arrayAt(&btx, j1) = 0.5;
            *arrayAt(&btx, j2) = 0.5;
            *arrayAt(&dbx, j1) = 0.0;
            *arrayAt(&dbx, j2) = 0.0;

        } else if (*matrixAt(in, j3+1, l) == 0) {
            
            *arrayAt(&btx, j1) = xq2;
            *arrayAt(&btx, j2) = xq1;
            *arrayAt(&dbx, j1) = -1.0;
            *arrayAt(&dbx, j2) = 1.0;

        } else {
            
            *arrayAt(&btx, j1) = xc1;
            *arrayAt(&btx, j2) = xc1;
            *arrayAt(&dbx, j1) = xc2;
            *arrayAt(&dbx, j2) = xc2;

        }
        j1 = 3;
        j2 = 2;
        j3 = 6;
    }

    j1 = 1;
    j2 = 2;
    j3 = 6;
    for (i = 0; i < 2; i++) {
        if (*matrixAt(in, j3, l) == 0) {
            
            *arrayAt(&bty, j1) = 0.5;
            *arrayAt(&bty, j2) = 0.5;
            *arrayAt(&dby, j1) = 0.0;
            *arrayAt(&dby, j2) = 0.0;

        } else if (*matrixAt(in, j3+1, l) == 0) {
            
            *arrayAt(&bty, j1) = yq2;
            *arrayAt(&bty, j2) = yq1;
            *arrayAt(&dby, j1) = -1.0;
            *arrayAt(&dby, j2) = 1.0;

        } else {
            
            *arrayAt(&bty, j1) = yc1;
            *arrayAt(&bty, j2) = yc1;
            *arrayAt(&dby, j1) = yc2;
            *arrayAt(&dby, j2) = yc2;

        }
        j1 = 0;
        j2 = 3;
        j3 = 10;
    }

    // Shape function derivative matrix - corner nodes
    for (j = 0; j < 4; j++) {
        *arrayAt(&dfx, j) = *arrayAt(&dax, j) * (*arrayAt(&btx, j) + *arrayAt(&bty, j)) + *arrayAt(&dbx, j) * *arrayAt(&alf, j);
        *arrayAt(&dfy, j) = *arrayAt(&day, j) * (*arrayAt(&btx, j) + *arrayAt(&bty, j)) + *arrayAt(&dby, j) * *arrayAt(&alf, j);
        *arrayAt(f, j) = *arrayAt(&alf, j) * (*arrayAt(&btx, j) + *arrayAt(&bty, j));
    }

    // Shape function derivative matrix - edge nodes
    if (m != 4) {
        j = 3;
        xeq = 1.0 - xi * xi;
        yeq = 1.0 - yi * yi;
        xe1 = 1.0 - 3.0 * xi;
        xe2 = 1.0 + 3.0 * xi;
        ye1 = 1.0 - 3.0 * yi;
        ye2 = 1.0 + 3.0 * yi;

        if (*matrixAt(in, 4, l) != 0) {
            if (*matrixAt(in, 5, l) == 0) {
                j += 1;
                *arrayAt(&dfx, j) = -xi * yi1;
                *arrayAt(&dfy, j) = -0.5 * xeq;
                *arrayAt(f, j) = 0.5 * xeq * yi1;
            } else {
                j += 1;
                *arrayAt(&dfx, j) = -0.28125 * yi1 * (3.0 * xeq + 2.0 * xi * xe1);
                *arrayAt(&dfy, j) = -0.28125 * xeq * xe1;
                *arrayAt(f, j) = 0.28125 * yi1 * xeq * xe1;
                j += 1;
                *arrayAt(&dfx, j) = 0.28125 * yi1 * (3.0 * xeq - 2.0 * xi * xe2);
                *arrayAt(&dfy, j) = -0.28125 * xeq * xe2;
                *arrayAt(f, j) = 0.28125 * yi1 * xeq * xe2;
            }
        }

        if (*matrixAt(in, 6, l) != 0) {
            if (*matrixAt(in, 7, l) == 0) {
                j += 1;
                *arrayAt(&dfx, j) = 0.5 * yeq;
                *arrayAt(&dfy, j) = -yi * xi2;
                *arrayAt(f, j) = 0.5 * xi2 * yeq;
            } else {
                j += 1;
                *arrayAt(&dfx, j) = 0.28125 * yeq * ye1;
                *arrayAt(&dfy, j) = -0.28125 * xi2 * (3.0 * yeq + 2.0 * yi * ye1);
                *arrayAt(f, j) = 0.28125 * xi2 * yeq * ye1;
                j += 1;
                *arrayAt(&dfx, j) = 0.28125 * yeq * ye2;
                *arrayAt(&dfy, j) = 0.28125 * xi2 * (3.0 * yeq - 2.0 * yi * ye2);
                *arrayAt(f, j) = 0.28125 * xi2 * yeq * ye2;
            }
        }

        if (*matrixAt(in, 8, l) != 0) {
            if (*matrixAt(in, 9, l) == 0) {
                j += 1;
                *arrayAt(&dfx, j) = -xi * yi2;
                *arrayAt(&dfy, j) = 0.5 * xeq;
                *arrayAt(f, j) = 0.5 * xeq * yi2;
            } else {
                j += 1;
                *arrayAt(&dfx, j) = 0.28125 * yi2 * (3.0 * xeq - 2.0 * xi * xe2);
                *arrayAt(&dfy, j) = 0.28125 * xeq * xe2;
                *arrayAt(f, j) = 0.28125 * yi2 * xeq * xe2;
                j += 1;
                *arrayAt(&dfx, j) = -0.28125 * yi2 * (3.0 * xeq + 2.0 * xi * xe1);
                *arrayAt(&dfy, j) = 0.28125 * xeq * xe1;
                *arrayAt(f, j) = 0.28125 * yi2 * xeq * xe1;
            }
        }

        if (*matrixAt(in, 10, l) != 0) {
            if (*matrixAt(in, 11, l) == 0) {
                j += 1;
                *arrayAt(&dfx, j) = -0.5 * yeq;
                *arrayAt(&dfy, j) = -yi * xi1;
                *arrayAt(f, j) = 0.5 * xi1 * yeq;
            } else {
                j += 1;
                *arrayAt(&dfx, j) = -0.28125 * yeq * ye2;
                *arrayAt(&dfy, j) = 0.28125 * xi1 * (3.0 * yeq - 2.0 * yi * ye2);
                *arrayAt(f, j) = 0.28125 * xi1 * yeq * ye2;
                j += 1;
                *arrayAt(&dfx, j) = -0.28125 * yeq * ye1;
                *arrayAt(&dfy, j) = -0.28125 * xi1 * (3.0 * yeq + 2.0 * yi * ye1);
                *arrayAt(f, j) = 0.28125 * xi1 * yeq * ye1;
            }
        }
    }

    // Jacobian
    sum1 = 0.0;
    sum2 = 0.0;
    sum3 = 0.0;
    sum4 = 0.0;

    k = -1;
    for (i = 0; i < m; i++) {

        do {
            k += 1;
        } while (*matrixAt(in, k, l) == 0);

        ki = *matrixAt(in, k, l) - 1;
        sum1 += *arrayAt(&dfx, i) * *arrayAt(x, ki);
        sum2 += *arrayAt(&dfx, i) * *arrayAt(y, ki);
        sum3 += *arrayAt(&dfy, i) * *arrayAt(x, ki);
        sum4 += *arrayAt(&dfy, i) * *arrayAt(y, ki);
    }

    *det = sum1 * sum4 - sum2 * sum3;
    det1 = 1.0 / *det;
    c11 = det1 * sum4;
    c12 = -det1 * sum2;
    c21 = -det1 * sum3;
    c22 = det1 * sum1;

    for (j = 0; j < m; j++) {
        *arrayAt(dgx, j) = c11 * *arrayAt(&dfx, j) + c12 * *arrayAt(&dfy, j);
        *arrayAt(dgy, j) = c21 * *arrayAt(&dfx, j) + c22 * *arrayAt(&dfy, j);
    }

    arrayFree(&alf);
    arrayFree(&dax);
    arrayFree(&day);
    arrayFree(&btx);
    arrayFree(&bty);
    arrayFree(&dbx);
    arrayFree(&dby);
    arrayFree(&dfx);
    arrayFree(&dfy);

    return;
}
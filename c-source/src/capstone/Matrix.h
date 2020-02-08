#ifndef __CAP_MATRIX_H__
#define __CAP_MATRIX_H__

/**
 * Matricies are stored in column major form. 
 * That is, if A is an NxM matrix, the first N values of
 * elements is the first column, the next N the column
 */
typedef struct {
    double** elements;
    int rows;
    int columns;
} Matrix;


void matrixDimension(Matrix* oMatrix, int rows, int columns);
void matrixFree(Matrix* oMatrix);

double* matrixAt(Matrix* matrix, int row, int col);

void matrixAssertNotNull(Matrix* matrix, const char* message);

#endif /* __CAP_MATRIX_H__ */
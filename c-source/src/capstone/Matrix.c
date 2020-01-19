#include "Matrix.h"

#include <stdio.h>
#include <stdlib.h>

void matrixDimension(Matrix* oMatrix, int rows, int columns) {
    matrixAssertNotNull(oMatrix, "matrix null in matrixDimension!");

    oMatrix->elements = calloc(rows * columns, sizeof(double));
    oMatrix->columns = columns;
    oMatrix->rows = rows;
}

void matrixFree(Matrix* oMatrix) {
    matrixAssertNotNull(oMatrix, "matrix null in matrixFree!");

    free(oMatrix->elements);

    oMatrix->elements = NULL;
    oMatrix->columns = 0;
    oMatrix->rows = 0;
}

double* matrixAt(Matrix* matrix, int row, int col) {
    matrixAssertNotNull(matrix, "matrix null in matrixAt!");

    int index = col + matrix->columns * row;

    if (index >= matrix->columns * matrix->rows) {
        fprintf(stderr, "matrixAt index out of bounds: %d\n", index);
        exit(1);
    }

    return &matrix->elements[index];
}

void matrixAssertNotNull(Matrix* matrix, const char* message) {
    if (matrix == NULL) {
        fprintf(stderr, "%s\n", message);
        exit(1);
    }
}
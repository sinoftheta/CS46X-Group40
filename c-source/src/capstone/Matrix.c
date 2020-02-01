#include "Matrix.h"

#include <stdio.h>
#include <stdlib.h>

void matrixDimension(Matrix* oMatrix, int rows, int columns) {
    matrixAssertNotNull(oMatrix, "matrix null in matrixDimension!");

    oMatrix->elements = calloc(rows, sizeof(double*));
    if(oMatrix->elements == NULL){
        fprintf(stderr, "Matrix memory allocation failed in matrixDimension!");
        exit(1);
    }

    for(int i = 0; i < rows; ++i){
        oMatrix->elements[i] = calloc(columns, sizeof(double));
        if(oMatrix->elements[i] == NULL){
            fprintf(stderr, "Matrix allocation failed in matrixDimension!");
            exit(1);
        }
    }

    oMatrix->columns = columns;
    oMatrix->rows = rows;
}

void matrixFree(Matrix* oMatrix) {
    matrixAssertNotNull(oMatrix, "matrix null in matrixFree!");


    for(int i = 0; i < oMatrix->rows; ++i){
        free(oMatrix->elements[i]);
    }

    free(oMatrix->elements);

    oMatrix->elements = NULL;
    oMatrix->columns = 0;
    oMatrix->rows = 0;
}

double* matrixAt(Matrix* matrix, int row, int col) {
    matrixAssertNotNull(matrix, "matrix null in matrixAt!");

    


    if (col - 1 >= matrix->columns || row - 1 >= matrix->rows) {
        fprintf(stderr, "matrixAt index out of bounds at row: %d, column: %d\n", row, col);
        exit(1);
    }

    return &matrix->elements[row - 1][col -1];
}

void matrixAssertNotNull(Matrix* matrix, const char* message) {
    if (matrix == NULL) {
        fprintf(stderr, "%s\n", message);
        exit(1);
    }
    if (matrix->elements == NULL){
        fprintf(stderr, "%s\nMatrix Uninitialized!\n", message);
        exit(1);
    }
}
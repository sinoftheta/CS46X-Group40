#include "Matrix.h"

#include "../gs2/gs2.h"

#include <stdio.h>
#include <stdlib.h>

void matrixDimension(Matrix* oMatrix, int rows, int columns) {
    matrixAssertNotNull(oMatrix, "matrix null in matrixDimension!");

    oMatrix->elements = calloc(rows, sizeof(double*));
    if(oMatrix->elements == NULL){
        fprintf(gs2stderr, "Matrix memory allocation failed in matrixDimension!");
        exit(1);
    }

    for(int i = 0; i < rows; ++i){
        oMatrix->elements[i] = calloc(columns, sizeof(double));
        if(oMatrix->elements[i] == NULL){
            fprintf(gs2stderr, "Matrix allocation failed in matrixDimension!");
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

    


    if (col - 1 >= matrix->columns || row - 1 >= matrix->rows || row <= 0 || col <= 0) {
        fprintf(gs2stderr, "matrixAt index out of bounds at row: %d (%d), column: %d (%d)\n", row, matrix->rows, col, matrix->columns);
        ((Matrix*)(0))->elements[0][0] = 1.0;
        // exit(1);
    }

    return &matrix->elements[row - 1][col -1];
}

void matrixAssertNotNull(Matrix* matrix, const char* message) {
    if (matrix == NULL) {
        fprintf(gs2stderr, "%s\n", message);
        exit(1);
    }

    if (matrix->columns * matrix->rows == 0) {
        fprintf(gs2stderr, "[Warn] matrix has zero elements: %s\n", message);
    } 
}

#ifdef DO_DEBUG
void matrixPrint(const char* name, Matrix* matrix) {
    fprintf(gs2stderr, "Matrix: %s\n", name);
    for (int i = 0; i < matrix->rows; i++) {
        for (int j = 0; j < matrix->columns; j++) {
            fprintf(gs2stderr, "%15.3E   ", matrix->elements[i][j]);
        }
        fprintf(gs2stderr, "\n");
    }
}
#else
void matrixPrint(const char* name, Matrix* matrix) {
    
}
#endif

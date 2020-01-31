#include "Array.h"
#include <stdlib.h>
#include <stdio.h>

void arrayDimension(Array* array, int size) {
    arrayAssertNotNull(array, "array null in arrayDimension!");

    array->elements = calloc(size, sizeof(double));
    if(array->elements == NULL){
        fprintf(stderr, "Memory allocation failed in arrayDimension!\n");
    }
    array->size = size;
}

void arrayFree(Array* array) {
    arrayAssertNotNull(array, "array null in arrayFree!");

    free(array->elements);

    array->elements = NULL;
    array->size = 0;
}

double* arrayAt(Array* array, int index) {
    arrayAssertNotNull(array, "array null in arrayAt!");


    if (index - 1 >= array->size) {
        fprintf(stderr, "arrayAt index out of bounds: %d\n", index);
        exit(1);
    }

    return &array->elements[index - 1];
}

void arrayAssertNotNull(Array* array, const char* message) {
    if (array == NULL) {
        fprintf(stderr, "%s\n", message);
        exit(1);
    }
}
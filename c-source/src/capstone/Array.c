#include "Array.h"
#include "../gs2/gs2.h"

#include <stdlib.h>
#include <stdio.h>

void arrayDimension(Array* array, int size) {
    arrayAssertNotNull(array, "array null in arrayDimension!");
    printf("asdfadsf\n");
    array->elements = calloc(size, sizeof(double));
    if(array->elements == NULL){
        fprintf(gs2stderr, "Memory allocation failed in arrayDimension!\n");
        exit(-1);
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


    if (index - 1 >= array->size || index <= 0) {
        fprintf(gs2stderr, "arrayAt index out of bounds: %d\n", index);
        int a = ((Array*)(NULL))->elements[0];
        exit(a);
    }

    return &array->elements[index - 1];
}

void arrayAssertNotNull(Array* array, const char* message) {
    if (array == NULL) {
        fprintf(gs2stderr, "%s\n", message);
        exit(-1);
    }
}

#ifdef DO_DEBUG
void arrayPrint(const char* name, Array* array) {
    fprintf(gs2stderr, "Array: %s\n", name);

    for (int i = 0; i < array->size; i++) {
        if ((i+1) % 5 == 0)
            fprintf(gs2stderr, "\n");
        fprintf(gs2stderr, "%15.6E", array->elements[i]);
    }
    fprintf(gs2stderr, "\n");
}
#else
void arrayPrint(const char* name, Array* array) {
    
}
#endif

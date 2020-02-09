#ifndef __CAP_ARRAY_H__
#define __CAP_ARRAY_H__

typedef struct {
    double* elements;
    int size;
} Array;

void arrayDimension(Array* array, int size);
void arrayFree(Array* array);

double* arrayAt(Array* array, int index);

void arrayAssertNotNull(Array* array, const char* message);


#endif /* __CAP_ARRAY_H__ */


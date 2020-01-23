#include <stdio.h>
#include <stdlib.h>

#include "capstone/Matrix.h"
#include "capstone/Array.h"

#include "capstone/CSVFile.h"

int main(int argc, char** argv) {


    FILE* fp;
    fp = fopen("res/example.txt", "r");
    if (fp == NULL) {
        puts("failed to read");
    }
    
    char* line = NULL;
    int length = 0;
    while (csvGetline(&line, &length, fp) != -1) {
        printf("%s\n", line);
        free(line);
        line = NULL;
    }


    fclose(fp);
    return 0;
}
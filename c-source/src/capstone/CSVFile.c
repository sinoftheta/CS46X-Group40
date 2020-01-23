#include "CSVFile.h"

#include <string.h>

const CSVRow CSV_NULL_ROW = {
    NULL, -1
};

CSVRow const* CSV_NULL_ROW_PTR = &CSV_NULL_ROW;


int alloc_line(char** oLine, int* oCurrLength, int newLength) {
    void* n = realloc(*oLine, newLength);

    if (n == NULL)
        return -1;

    *oLine = n;
    *oCurrLength = newLength;

    return 0;
}

int csvGetline(char** oLine, int* oLength, FILE* fp) {

    if (*oLength < 0) {
        if (alloc_line(oLine, oLength, 2) == -1) {
            return -1;
        }
    }

    if (*oLine == NULL) 
        return -1;
    

    if (feof(fp)) 
        return -1;
    

    char* cur = *oLine;
    int currLength = *oLength;

    for (;;) {
        char* ret = fgets(cur, currLength, fp);
        
        if (ret == NULL && !feof(fp)) 
            return -1; //read error

        if (ret == NULL && cur == *oLine) 
            return -1; // read empty

        char* eod = memchr(cur, '\0', currLength);
        if (feof(fp) || eod[-1] == '\n') 
            return eod - *oLine;
      
        // line continued
        currLength = *oLength + 1; // last of *pline is '\0'
        if (alloc_line(oLine, oLength, *oLength * 2)) 
            return -1;

        cur = *oLine + *oLength - currLength;
    }
}

CSVFile csvLoadFile(const char* filepath) {
    FILE* fp;

    fp = fopen(filepath, "r");





    fclose(fp);
    CSVFile csv;
    csv.filepath = NULL;
    csv.currentRow = NULL;

    return csv;
}
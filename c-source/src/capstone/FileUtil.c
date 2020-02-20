#include "FileUtil.h"

#include <string.h>
#include <stdlib.h>

#define _XOPEN_SOURCE 500
#include <unistd.h>


int __alloc_line(char** oLine, int* oCurrLength, int newLength) {
    void* n = realloc(*oLine, newLength);

    if (n == NULL)
        return -1;

    *oLine = n;
    *oCurrLength = newLength;

    return 0;
}

int getFileName(FILE* fp, char* filename, int size) {
    if (fp == NULL)
        return -1;

    int fno, r;
    char proclnk[size];

    fno = fileno(fp);
    sprintf(proclnk, "/proc/self/fd/%d", fno);
    r = readlink(proclnk, filename, size);

    if (r < 0)
        return -1;

    filename[r] = '\0';
    return r;
}

int readline(char** oLine, int* oLength, FILE* stream) {

    if (*oLength <= 0) {
        if (__alloc_line(oLine, oLength, 2) == -1) {
            return -1;
        }
    }

    if (*oLine == NULL) 
        return -1;
    

    if (feof(stream)) 
        return -1;
    

    char* cur = *oLine;
    int currLength = *oLength;

    for (;;) {
        char* ret = fgets(cur, currLength, stream);
        
        if (ret == NULL && !feof(stream)) 
            return -1; //read error

        if (ret == NULL && cur == *oLine) 
            return -1; // read empty

        char* eod = memchr(cur, '\0', currLength);
        if (feof(stream) || eod[-1] == '\n') 
            return eod - *oLine;
      
        // line continued
        currLength = *oLength + 1; // last of *pline is '\0'
        if (__alloc_line(oLine, oLength, *oLength * 2)) 
            return -1;

        cur = *oLine + *oLength - currLength;
    }
}

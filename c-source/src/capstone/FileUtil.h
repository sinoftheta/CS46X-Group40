#ifndef __CAP_FILE_UTIL_H__
#define __CAP_FILE_UTIL_H__

#include <stdio.h>

// stdio.h getline is posix only
int readline(char** oLine, int* oLength, FILE* stream);

// get filename from a FILE pointer
int getFileName(FILE* fp, char* filename, int size);

#endif /* __CAP_FILE_UTIL_H__ */

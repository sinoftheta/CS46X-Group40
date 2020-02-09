#ifndef __CAP_FILE_UTIL_H__
#define __CAP_FILE_UTIL_H__

#include <stdio.h>

// stdio.h getline is posix only
int readline(char** oLine, int* oLength, FILE* stream);

#endif /* __CAP_FILE_UTIL_H__ */


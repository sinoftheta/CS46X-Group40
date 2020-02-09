#include "Debug.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void croak(const char* msg) {
    fprintf(stderr, "%s\n", msg);
    exit(1);
}


void croakf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    exit(1);
}

void warn(const char* msg) {
    fprintf(stderr, "%s\n", msg);
}

void warnf(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
}


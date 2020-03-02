#ifndef __CAP_DEBUG_H__
#define __CAP_DEBUG_H__

#include <stdio.h>

void croak(const char* msg);
void croakf(const char* format, ...);

void warn(const char* msg);
void warnf(const char* fmt, ...);

#ifdef DO_DEBUG
#define DEBUG_LOG(x) printf("[DEBUG:%s:%d]: %s\n", __FILE__, __LINE__, x)
#else
#define DEBUG_LOG(x) 
#endif

#endif /* __CAP_DEBUG_H__ */

#ifndef __CAP_DEBUG_H__
#define __CAP_DEBUG_H__

void croak(const char* msg);
void croakf(const char* format, ...);

void warn(const char* msg);
void warnf(const char* fmt, ...);

#endif /* __CAP_DEBUG_H__ */


#ifndef __GS2_MATGEN_H__
#define __GS2_MATGEN_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"

void matgen1(Matrix* pe, Matrix* se, Array* q, Array* qp, int i, int j);
void matgen2(Array* srcr, Array* srcrt, int i);
void matgen3(Matrix* pe, Matrix* se);

#endif /* __GS2_MATGEN_H__ */
#ifndef __GS2_SHAPE_H__
#define __GS2_SHAPE_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"

void gs2Shape(Array* x, Array*  y, Matrix* in, int l, int m, double xi, double yi, Array* f,
              double* det, Array* dgx, Array* dgy);

#endif /* __GS2_SHAPE_H__ */
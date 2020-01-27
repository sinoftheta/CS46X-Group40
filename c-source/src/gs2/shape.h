#ifndef __GS2_SHAPE_H__
#define __GS2_SHAPE_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"

void gs2Shape(Array* X, Array*  Y, Matrix* IN, int L, int M, double XI, double YI, Array* F,
              Array* DGX, Array* DGY, int MAXNN, int MAXNE, int MXC, int ME);

#endif /* __GS2_SHAPE_H__ */
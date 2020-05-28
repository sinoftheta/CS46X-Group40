#ifndef __GS2_SOS_H__
#define __GS2_SOS_H__

#include "../capstone/Matrix.h"
#include "gs2.h"

/*
    Purpose:
      Called to print the global coefficient matrices if print control
      variables are set to 1.

      Called from TS, Cogen
*/

void gs2Sos(Matrix* a, int neq, int iband, int istart);

#endif /* __GS2_SOS_H__ */

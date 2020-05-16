#ifndef __CAP_UPD_H__
#define __CAP_UPD_H__

#include "../capstone/Array.h"

/*
    Purpose:
      Updates the arrays which contain the pressure heads and concentrations
      from the previous time step (time T).

      Called from TS
*/

void gs2UPD(Array* a, Array* b, Array* kod, Array* lq, int nn);

#endif /* __CAP_UPD_H__ */

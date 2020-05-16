#include "upd.h"


void gs2UPD(Array* a, Array* b, Array* kod, Array* lq, int nn) {

    for (int i = 1; i < nn; i++) {
        if (*arrayAt(kod, i) == 1.0)
            continue;
        int k = i - (int)(*arrayAt(lq, i));
        *arrayAt(b, k) = *arrayAt(a, i);
    }
}

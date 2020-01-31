#ifndef __GS2_GREEN_H__
#define __GS2_GREEN_H__

#include "../capstone/Matrix.h"
#include "../capstone/Array.h"


/*
* Purpose: To evaluate 1-d Shape functions and determinants for element l.
*           Determines if the side is linear, quadratic or cubic.
*
* @param x[]    - Array of x-coordinates x[i] for node i
* @param y[]    - Array of y-coordinates y[i] for node i
* @param detj   - Determinant of Jacobian calculation
* @param ag[]   - Coordinates of integration point ag[i] for node i
* @param in[][] - Element incidences in[i][l] at node i and element l
*                     number of active nodes at element l is at in[me][l]
* @param kf[]   - kf[l] specifies the side of element l with
*                     mixed-boundary conditions.   kf[l] == 0-4;
* @param jd[]   - incidences of active nodes in an element
* @param ieq[]  - incidences for line integrations
* @param ms     - ?
* @param me     - max number of nodes per element, + 1
* @param np     - number of integration points,
*                     np == 2 fully linear, np == 4 otherwise
* @param l      - element number
* @param istop  - error indicator
*
*/

void gs2Green(Array* x, Array* y, Array* detj, Array* ag, Matrix* in,
              Array* kf, Array* jd, Array* ieq, int* ms, int me, int np, int l, int* istop);

#endif /* __GS2_GREEN_H__ */

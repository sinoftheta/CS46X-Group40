from ctypes import *

from gs2 import types

lib_path = "../c-source/build/libgs2.so"

gs2 = CDLL(lib_path)


gs2.gs2CreateMemoryRequirements.restype = types.MemoryRequirements
memReqs = gs2.gs2CreateMemoryRequirements(
        12,
        16,
        52,
        25,
        2,
        0,
        2,
        1,
        1,
        4
    )

old = types.Array()
cold = types.Array()
cn = types.Array()
vn = types.Array()
coef = types.Array()
u = types.Array()
est = types.Array()
lp = types.Array()
klp = types.Array()
nsf = types.Array()
nsk = types.Array()
msp = types.Array()
nsp = types.Matrix()


gs2.arrayDimension(byref(old), memReqs.maxm1)
gs2.arrayDimension(byref(cold), memReqs.maxm2)
gs2.arrayDimension(byref(cn), memReqs.maxm4)
gs2.arrayDimension(byref(vn), memReqs.maxm4)
gs2.arrayDimension(byref(coef), memReqs.maxm4)
gs2.arrayDimension(byref(u), memReqs.maxm1)
gs2.arrayDimension(byref(est), memReqs.maxm1)
gs2.arrayDimension(byref(lp), memReqs.maxm1)
gs2.arrayDimension(byref(klp), memReqs.maxm2)
gs2.arrayDimension(byref(nsf), memReqs.maxm4)
gs2.arrayDimension(byref(nsk), memReqs.maxm4)
gs2.arrayDimension(byref(msp), memReqs.maxeep)

gs2.matrixDimension(byref(nsp), memReqs.maxm5, memReqs.maxeep)

# arr = types.Array()

# gs2.arrayAt.restype = POINTER(c_double)
# gs2.arrayDimension(byref(arr), 10)

# for i in range(arr.size):
#     # elemPtr = gs2.arrayAt(byref(arr), i+1)
#     # elemValue = cast(elemPtr, c_double_p)
#     print(gs2.arrayAt(byref(arr), i+1).contents)
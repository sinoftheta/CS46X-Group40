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


state = types.State()
state.memoryRequirements = memReqs
state.istop = c_int(0)


maxdif = c_double(0.0)
inputPath = create_string_buffer(b"../c-source/res/example1.csv")

gs2.gs2Datain.argtypes = [
    POINTER(types.State),
    c_char_p,
    POINTER(types.Array),
    POINTER(types.Array), 
    POINTER(types.Array),
    POINTER(types.Array),
    POINTER(types.Array),
    POINTER(types.Array), 
    POINTER(types.Array),
    POINTER(types.Array),
    POINTER(types.Array),
    POINTER(types.Array),
    POINTER(types.Array),
    POINTER(types.Matrix),
    POINTER(types.Array),
    POINTER(c_double)
]

gs2.gs2Datain(
    byref(state),
    inputPath,
    byref(old),
    byref(cold),
    byref(cn),
    byref(vn),
    byref(coef),
    byref(u),
    byref(est),
    byref(lp),
    byref(klp),
    byref(nsf),
    byref(nsk),
    byref(nsp),
    byref(msp), 
    byref(maxdif)
)

gs2.arrayFree(byref(old))
gs2.arrayFree(byref(cold))
gs2.arrayFree(byref(cn))
gs2.arrayFree(byref(vn))
gs2.arrayFree(byref(coef))
gs2.arrayFree(byref(u))
gs2.arrayFree(byref(est))
gs2.arrayFree(byref(lp))
gs2.arrayFree(byref(klp))
gs2.arrayFree(byref(nsf))
gs2.arrayFree(byref(nsk))
gs2.arrayFree(byref(msp))

gs2.matrixFree(byref(nsp))
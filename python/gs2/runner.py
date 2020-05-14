from os import path
from gs2 import types

from ctypes import *

class Runner:
    def __init__(self, simulationModel, config):
        gs2_lib_path = path.abspath(path.join(config['paths']['bundle'], config['paths']['gs2Lib']))

        self.datain_path = simulationModel.dataInputFile

        self.stdout = simulationModel.stdout
        self.stdin = simulationModel.stdin
        self.stderr = simulationModel.stdout

        self.gs2 = CDLL(gs2_lib_path)

    def openFiles(self):
        self.gs2.gs2DefaultIO()
        
        if self.stdout:
            self.gs2.gs2OutputFile(create_string_buffer(self.stdout.encode('utf-8')))

        if self.stdin:
            self.gs2.gs2InputFile(create_string_buffer(self.stdin.encode('utf-8')))

        if self.stderr:
            self.gs2.gs2ErrorFile(create_string_buffer(self.stderr.encode('utf-8')))

    # it is required that the `pythonFunc` parameter is the result of a call to 
    # types.CallbackType(callback to be registered)
    def registerCallback(self, pythonFunc):
        self.gs2.gs2RegisterCallback(pythonFunc)
        

    def run(self):
        self.gs2.gs2CreateMemoryRequirements.restype = types.MemoryRequirements
        memReqs = self.gs2.gs2CreateMemoryRequirements(
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

        self.gs2.arrayDimension(byref(old), memReqs.maxm1)
        self.gs2.arrayDimension(byref(cold), memReqs.maxm2)
        self.gs2.arrayDimension(byref(cn), memReqs.maxm4)
        self.gs2.arrayDimension(byref(vn), memReqs.maxm4)
        self.gs2.arrayDimension(byref(coef), memReqs.maxm4)
        self.gs2.arrayDimension(byref(u), memReqs.maxm1)
        self.gs2.arrayDimension(byref(est), memReqs.maxm1)
        self.gs2.arrayDimension(byref(lp), memReqs.maxm1)
        self.gs2.arrayDimension(byref(klp), memReqs.maxm2)
        self.gs2.arrayDimension(byref(nsf), memReqs.maxm4)
        self.gs2.arrayDimension(byref(nsk), memReqs.maxm4)
        self.gs2.arrayDimension(byref(msp), memReqs.maxeep)

        self.gs2.matrixDimension(byref(nsp), memReqs.maxm5, memReqs.maxeep)

        fm = types.Array()
        rt = types.Array()
        cfm = types.Array()
        crt = types.Array()
        fx = types.Array()

        self.gs2.arrayDimension(byref(fm), memReqs.maxm1)
        self.gs2.arrayDimension(byref(rt), memReqs.maxm1)
        self.gs2.arrayDimension(byref(cfm), memReqs.maxm2)
        self.gs2.arrayDimension(byref(crt), memReqs.maxm2)
        self.gs2.arrayDimension(byref(fx), memReqs.maxm1)


        state = types.State()
        state.memoryRequirements = memReqs
        state.istop = c_int(0)


        maxdif = c_double(0.0)
        inputPath = create_string_buffer(self.datain_path.encode('utf-8'))

        self.gs2.gs2Datain.argtypes = [
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

        self.gs2.gs2Datain(
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

        self.gs2.gs2Ts(
            byref(state),
            byref(state.s),
            byref(state.p),
            byref(state.w),
            byref(fm),
            byref(rt),
            byref(state.phi),
            byref(state.phii),
            byref(old),
            byref(cfm),
            byref(crt),
            byref(state.conc),
            byref(state.conci),
            byref(cold),
            byref(fx),
            byref(cn),
            byref(vn),
            byref(coef),
            byref(est),
            byref(u),
            byref(state.fq),
            byref(state.cfq),
            byref(state.x),
            byref(state.y),
            byref(state.fmobx),
            byref(state.fmoby),
            byref(state.elong),
            byref(state.etrans),
            byref(state.por),
            byref(state.alpha),
            byref(state.tta),
            byref(state.kd),
            byref(state._lambda),
            byref(state.rho),
            byref(state._in),
            byref(state.kf),
            byref(state.lr),
            byref(state.klr),
            byref(state.lc),
            byref(state.klc),
            byref(lp),
            byref(nsf),
            byref(state.ie),
            byref(nsp),
            byref(msp),
            create_string_buffer("text".encode("utf-8")),
            create_string_buffer("text2".encode("utf-8"))
        )

        self.gs2.arrayFree(byref(old))
        self.gs2.arrayFree(byref(cold))
        self.gs2.arrayFree(byref(cn))
        self.gs2.arrayFree(byref(vn))
        self.gs2.arrayFree(byref(coef))
        self.gs2.arrayFree(byref(u))
        self.gs2.arrayFree(byref(est))
        self.gs2.arrayFree(byref(lp))
        self.gs2.arrayFree(byref(klp))
        self.gs2.arrayFree(byref(nsf))
        self.gs2.arrayFree(byref(nsk))
        self.gs2.arrayFree(byref(msp))

        self.gs2.matrixFree(byref(nsp))

        self.gs2.gs2CloseFiles()


        del self.gs2
    

class ParametersModel:
    def __init__(self):
        self.NN = 0
        self.NE = 0
        self.NK = 0
        self.NB = 0
        self.KNB = 0
        self.PL = 0.000
        self.EI = 0.000
        self.PCHNG = 0.000
        self.BETAP = 0.000
        self.DIFUSN = 0.000
        self.CLOS1 = 0.000
        self.DELT = 0.000
        self.CHNG = 0.000
        self.ITMAX = 0
        self.ITER1 = 0
        self.ITCHNG = 0
        self.IGO = 0

        # Can enumerate these options instead of storing strings
        self.TYPE = "Implicit"
        self.STAT = "Steady-state"
        self.STATP = "Steady-state"

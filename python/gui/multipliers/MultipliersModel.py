


class MultipliersModel:
    def __init__(self):
        self.AFMOBX = 0.0
        self.AFMOBY = 0.0
        self.APOR = 0.0
        self.AELONG = 0.0
        self.AETRANS = 0.0
        self.APHII = 0.0
        self.ACONCI = 0.0
        self.XFACT = 0.0
        self.YFACT = 0.0
        self.ATETA = 0.0
        self.AAL = 0.0
        self.AKD = 0.0
        self.ALAM = 0.0
        self.ARHO = 0.0
    
    #generic mutator
    def setVal(self, key, val):
        if hasattr(self, key):
            setattr(self, key, val)
            #print(str(key) + ": " + str(val))

    #other mutators would have to be written for live data
    def setLiveData():
        pass

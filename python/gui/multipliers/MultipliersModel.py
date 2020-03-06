class MultipliersModel:
    def __init__(self):
        self.AFMOBX = 1.0
        self.AFMOBY = 1.0
        self.APOR = 1.0
        self.AELONG = 1.0
        self.AETRANS = 1.0
        self.APHII = 1.0
        self.ACONCI = 1.0
        self.XFACT = 1.0
        self.YFACT = 1.0
        self.ATETA = 1.0
        self.AAL = 1.0
        self.AKD = 1.0
        self.ALAM = 1.0
        self.ARHO = 1.0
    
    #generic mutator
    def setVal(self, key, val):
        if hasattr(self, key):
            setattr(self, key, val)



class MultipliersModel:
    def __init__(self):

        init = 0.0
        keys = [
            "AFMOBX",
            "AFMOBY",
            "APOR",
            "AELONG",
            "AETRANS",
            "APHII",
            "ACONCI",
            "XFACT",
            "YFACT",
            "ATETA",
            "AAL",
            "AKD",
            "ALAM",
            "ARHO"
        ] 

        self.multipliers = {}
        for key in keys:
            self.multipliers[key] = init

    def getVals(self):
        return self.multipliers
    
    def setVal(self, key, val):
        if key in self.multipliers:
            self.multipliers[key] = val
        else:
            print("Error: multiplier " + key + " key does not exist")
    
        
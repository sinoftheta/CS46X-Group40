class ElementPropertiesModel:
    def __init__(self, materialGroupNum):
        self.materialGroupId = materialGroupNum

        self.FMOBX = 0.00000
        self.FMOBY = 0.00000
        self.ELONG = 0.00000
        self.ETRANS = 0.00000
        self.POR = 0.00000
        self.TTA = 0.00000
        self.ALPHA = 0.00000
        self.KD = 0.00000
        self.LAMBDA = 0.00000
        self.RHO = 0.00000

    def setValue(self, key, value):
        if (hasattr(self, key)):
            setattr(self, key, value)

    def getMaterialGroupId(self):
        return self.materialGroupId

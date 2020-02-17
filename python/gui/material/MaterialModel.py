class MaterialModel:
    def __init__(self, group):
        self.materialGroup = group
        
        # default to maximum supported number
        self._interpolationPointCount = 15

        self.pressureHead = [0.0 for x in range(self._interpolationPointCount)]
        self.moistureContent = [0.0 for x in range(self._interpolationPointCount)]
        self.hydraulicConductivity = [0.0 for x in range(self._interpolationPointCount)]


    def setInterpolationPointCount(self, newCount):
        diff = newCount - self._interpolationPointCount
        
        # remove the last points in the lists
        if diff < 0:
            self.pressureHead = self.pressureHead[:diff]
            self.moistureContent = self.moistureContent[:diff]
            self.hydraulicConductivity = self.hydraulicConductivity[:diff]
        else:
            self.pressureHead.extend([0.0 for x in range(diff)])
            self.moistureContent.extend([0.0 for x in range(diff)])
            self.hydraulicConductivity.extend([0.0 for x in range(diff)])

        self._interpolationPointCount = newCount

    def getInterpolationPointCount(self):
        return self._interpolationPointCount

    

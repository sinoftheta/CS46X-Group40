from ..LiveData import LiveData

class ElementModel:
    def __init__(self, elementNum):
        self.elementNumber = elementNum

        self.maxIncidenceCount = 12

        self.incidences = [0 for node in range(self.maxIncidenceCount)]

        self.materialGroup = LiveData(None)

    def getIncidences(self):
        # return list of incidences for element
        #   slice list before first occurence of 0
        return self.incidences[:self.incidences.index(0)]

    def getMaterialGroup(self):
        return self.materialGroup.getData()

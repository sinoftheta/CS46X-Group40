from ..LiveData import LiveData

class ElementModel:
    def __init__(self, elementNum):
        self.elementNumber = elementNum

        # default to max number nodes per element in GS2
        self.maxIncidenceCount = 12

        # default to max number of nodes for GS2
        self.nodeCount = 52

        self.incidences = [0 for node in range(self.maxIncidenceCount)]

        # default to first material group
        self.materialGroup = 1

    def getIncidences(self):
        # return list of incidences for element
        #   slice list before first occurence of 0
        return self.incidences[:self.incidences.index(0)]

    def getMaterialGroup(self):
        return self.materialGroup.getData()

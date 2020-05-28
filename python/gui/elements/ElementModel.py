class ElementModel:
    """
        Contains attribute information for a particular element indentified
        by elementNumber including incidence nodes and material group.
    """
    def __init__(self, elementNum):
        # Element ID
        self.elementNumber = elementNum

        # default to max number nodes per element in GS2
        self.maxIncidenceCount = 12

        # List of nodes that make up this element
        self.incidences = [0 for node in range(self.maxIncidenceCount)]

        # default to first material group
        self.materialGroup = 1

    def getIncidences(self):
        # return list of incidences for element
        #   slice list before first occurence of 0
        return self.incidences[:self.incidences.index(0)]

    def getMaterialGroup(self):
        return self.materialGroup

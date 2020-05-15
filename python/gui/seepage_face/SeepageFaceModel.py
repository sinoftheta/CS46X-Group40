

class SeepageFaceModel:
    """
        Contains lists of nodes belonging to seepage faces in the
        problem domain.
    """
    def __init__(self, seepageFaceID = 0):
        self.seepageFaceID = seepageFaceID

        # both diricheltNodes and nuemannNodes should contain the number
        # of some node

        # the elements in this list will need to be kept in sync
        # with the nodes page
        self.diricheltNodes = []

        # the elements in this list will need to be kept in sync
        # with the nodes page
        self.nuemannNodes = []


    def setDiricheltNode(self, index, value):
        if index < self.getNumberOfDiricheltNodes():
            self.diricheltNodes[index] = value

    def setNuemannNode(self, index, value):
        if index < self.getNumberOfNuemannNodes():
            self.nuemannNodes[index] = value

    def getNumberOfDiricheltNodes(self):
        return len(self.diricheltNodes)

    def setNumberOfDiricheltNodes(self, count):
        diff = count - self.getNumberOfDiricheltNodes()

        if diff < 0:
            self.diricheltNodes = self.diricheltNodes[:diff]
        else:
            self.diricheltNodes.extend([0 for x in range(diff)])

    def getNumberOfNuemannNodes(self):
        return len(self.nuemannNodes)

    def setNumberOfNuemannNodes(self, count):
        diff = count - self.getNumberOfNuemannNodes()

        if diff < 0:
            self.nuemannNodes = self.nuemannNodes[:diff]
        else:
            self.nuemannNodes.extend([0 for x in range(diff)])

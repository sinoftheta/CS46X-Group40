

class SeepageFace:
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


    def getNumberOfDiricheltNodes(self):
        return len(self.diricheltNodes)

    def getNumberOfNuemannNodes(self):
        return len(self.nuemannNodes)


    
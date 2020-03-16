class NodeModel():
    def __init__(self, nodeNum):
        self.I = nodeNum
        self.X = 0.0
        self.Y = 0.0
        self.CONCI = 0.0
        self.PHII = 0.0
        self.boundary = ""

    #generic mutator
    def setVal(self, key, val):
        if hasattr(self, key):
            setattr(self, key, val)
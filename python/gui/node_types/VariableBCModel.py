class VariableBCModel():
    def __init__(self, nodeID=None):
        self.nodeID = nodeID

        self.dirichlet = False
        self.neumann = False
        self.COEF = 0.0
        self.VN = 0.0

    def setVal(self, key, value):
        if (hasattr(self, key)):
            setattr(self, key, value)

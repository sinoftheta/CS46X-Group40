class MixedBCModel():
    def __init__(self, nodeID=None):
        self.nodeID = nodeID

        self.CN = 0.0

    def setVal(self, key, value):
        if (hasattr(self, key)):
            setattr(self, key, value)

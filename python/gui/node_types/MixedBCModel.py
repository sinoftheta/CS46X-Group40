class MixedBCModel():
    def __init__(self):
        self.nodeID = None

        self.CN = 0.0

    def setVal(self, key, value):
        if (hasattr(self, key)):
            setattr(self, key, value)

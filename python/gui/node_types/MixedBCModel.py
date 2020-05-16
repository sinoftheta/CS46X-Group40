class MixedBCModel():
    """
        Contains attribute values for nodes having mixed boundary conditions
        for mass transport.
    """
    def __init__(self, nodeID=None):
        # Node ID
        self.nodeID = nodeID
        # Influx concentration of water
        self.CN = 0.0

    def setVal(self, key, value):
        if (hasattr(self, key)):
            setattr(self, key, value)

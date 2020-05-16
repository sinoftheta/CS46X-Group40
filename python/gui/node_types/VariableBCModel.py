class VariableBCModel():
    """
        Contains attribute values for nodes where infiltration or evaporation
        takes place.
    """
    def __init__(self, nodeID=None):
        # Node ID
        self.nodeID = nodeID
        # Constant head node
        self.dirichlet = False
        # Constant flux node
        self.neumann = False
        # Fraction of maximum flux
        self.COEF = 0.0
        # Tributary length
        self.VN = 0.0

    def setVal(self, key, value):
        if (hasattr(self, key)):
            setattr(self, key, value)

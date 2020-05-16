class SourceSinkModel():
    """
        Contains attribute values of flux and concentration for source
        and sink nodes.
    """
    def __init__(self, nodeID=None):
        # Node ID
        self.nodeID = nodeID
        # Node recharge/discharge rate
        self.FQ = 0.0
        # Node Concentration
        self.CFQ = 0.0

from gui.LiveData import LiveData

class NodeModel():
    """
        Contains attributes for individual nodes in the problem scenario.
    """
    def __init__(self, nodeNum):
        # Node ID
        self.I = nodeNum
        # Node x-coordinate
        self.X = 0.0
        # Node y-coordinate
        self.Y = 0.0
        # Node initial concentration
        self.CONCI = 0.0
        # Node initial pressure head
        self.PHII = 0.0


        self.boundary = LiveData("-Select Boundary Type-")

    #generic mutator
    # TODO: STRONGLY reconsider this setVal pattern
    def setVal(self, key, val):
        if key == "boundary":
            self.boundary.setData(val)
        elif hasattr(self, key):
            setattr(self, key, val)

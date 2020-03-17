from gui.LiveData import LiveData

class NodeModel():
    def __init__(self, nodeNum):
        self.I = nodeNum
        self.X = 0.0
        self.Y = 0.0
        self.CONCI = 0.0
        self.PHII = 0.0


        self.boundary = LiveData("-Select Boundary Type-")

    #generic mutator
    # TODO: STRONGLY reconsider this setVal pattern
    def setVal(self, key, val):
        if key == "boundary":
            self.boundary.setData(val)
        elif hasattr(self, key):
            setattr(self, key, val)
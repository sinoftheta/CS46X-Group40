class NodeModel():
    def __init(self, NodeNum)__:
        self.I = NodeNum
        self.X = 0.0
        self.Y = 0.0
        self.CONCI = 0.0
        self.PHII = 0.0
        self.boundary = ""

class NodesModel():
    def __init(self, parametersModel)__:
        self.nodes = []
        
        for i in range(0, parametersModel.NN.getData):
            self.nodes.append(NodeModel(i + 1))

        parametersModel.NN.connectObserver(self._updateNodes)

    # updates the nodes array to have the same length as NN 
    def _updateNodes(self, newNumNodes):
        if(newNumNodes > len(self.nodes)):
            while(len(self.nodes) != newNumNodes):
                self.nodes.append(NodeModel(len(self.nodes) + 1))
        elif(newNumNodes < len(self.nodes)):
            while(len(self.nodes) != newNumNodes):
                self.nodes.remove(len(self.nodes) + 1)
        

from ..parameters.ParametersModel import parametersModel

class NodesModel():
    def __init(self)__:
        self.nodes = []
        for i in range(0, parametersModel.NN.getData):
            self.nodes.append(NodeModel(i + 1))

    # updates the nodes array to have the same length as NN 
    def _updateNodes(self):

class NodeModel():
    def __init(self, NodeNum)__:
        self.I = NodeNum
        self.X = 0.0
        self.Y = 0.0
        self.CONCI = 0.0
        self.PHII = 0.0
        self.boundary = ""


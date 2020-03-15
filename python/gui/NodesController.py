from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .nodes.NodeModel import NodeModel
from .nodes.nodeView import nodeView

nodeTableLabels = [
    "Node",
    "Boundary Type",
    "X-Coordinate",
    "Y-Coordinate",
    "Initial Pressure",
    "Initial Concentration" 
]

nodeTypeLabels = [
    "Constant Head (Dirichlet)",
    "Source/Sink",
    "Variable Boundary Condition (Flow)",
    "Seepage Face",
    "Mixed Boundary Condition (Mass Transport)"
]
class NodesController(QGroupBox):
    def __init__(self):
        super(Nodes, self).__init__('Nodes')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        self.createTable()

        parametersModel.NN.connectObserver(self._updateNodes)

        self.nodes = []
        for i in range(0, parametersModel.NN.getData()):
            self.nodes.append(NodeModel(i + 1))

    # updates the nodes array to have the same length as NN 
    def _updateNodes(self, newNumNodes):
        if(newNumNodes > len(self.nodes)):
            while(len(self.nodes) != newNumNodes):
                self.nodes.append(NodeModel(len(self.nodes) + 1))
        elif(newNumNodes < len(self.nodes)):
            while(len(self.nodes) != newNumNodes):
                self.nodes.remove(len(self.nodes) + 1)

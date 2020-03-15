from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .nodes.NodeModel import NodeModel
from .nodes.NodesView import NodesView

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
        super(NodesController, self).__init__('Nodes')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)

        #parametersModel.NN.connectObserver(self._updateNodes)

        self.nodes = []
        #for i in range(0, parametersModel.NN.getData()):
        #    self.nodes.append(NodeModel(i + 1))

        
        #creation and population of the table go in NodesView
        self.view = NodesView(self.nodes, self.setTableVal)

        #append table
        self.layout.addWidget(self.view)

    # updates the nodes array to have the same length as NN ...REMOVING IS BROKEN
    def onNodeCountChange(self, newNumNodes):
        if(newNumNodes > len(self.nodes)):
            while(len(self.nodes) != newNumNodes):
                self.nodes.append(NodeModel(len(self.nodes) + 1))
        elif(newNumNodes < len(self.nodes)):
            while(len(self.nodes) != newNumNodes):
                self.nodes.remove(len(self.nodes))
        print(self.nodes)
        
        self.view.updateTable()

    def setTableVal(self, row, key, val):
        print("setting row" + str(row) + " key: " + key + ", val: " + val)
        

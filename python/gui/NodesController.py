from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .nodes.NodeModel import NodeModel
from .nodes.NodesView import NodesView
from .BasicParametersController import BasicParameterChangeListener

from .BasicParametersController import BasicParameterChangeListener

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
    "Constant Concentration (Dirichlet)",
    "Source/Sink",
    "Variable Boundary Condition (Flow)",
    "Mixed Boundary Condition (Mass Transport)"
]

class NodesController(QGroupBox, BasicParameterChangeListener):
    def __init__(self):
        super(NodesController, self).__init__('Nodes')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        self.nodes = []
        self.nodeBoundaryChangeListeners = []
        self.view = NodesView(self.nodes, self.setTableVal, nodeTypeLabels, nodeTableLabels)
        self.layout.addWidget(self.view)

    def updateView(self, nodes):
        self.nodes.clear()
        self.nodes.extend(nodes)
        for node in self.nodes:
            node.boundary.connectObserver(lambda newData: self.notifyNodeBoundaryChanged(nodeModel.I, newData))
        self.view.populateTable()
        
    def onNodeCountChange(self, newNumNodes):
        if newNumNodes > len(self.nodes):
            while len(self.nodes) != newNumNodes:
                nodeModel = NodeModel(len(self.nodes) + 1)
                nodeModel.boundary.connectObserver(lambda newData: self.notifyNodeBoundaryChanged(nodeModel.I, newData))
                self.nodes.append(nodeModel)
        elif newNumNodes < len(self.nodes):
            while len(self.nodes) != newNumNodes:
                self.nodes.pop()
        self.view.populateTable()
    def getNodes(self):
        return self.nodes

    def setTableVal(self, i, key, val):
        #print("setting index " + str(i) + ", key: " + key + ", val: " + val)
        self.nodes[i].setVal(key, val)

    def addNodeBoundaryChangeListener(self, listener):
        if listener not in self.nodeBoundaryChangeListeners:
            self.nodeBoundaryChangeListeners.append(listener)

    def removeNodeBoundaryChangeListener(self, listener):
        self.nodeBoundaryChangeListeners.remove(listener)

    def notifyNodeBoundaryChanged(self, nodeNum, newData):
        for listener in self.nodeBoundaryChangeListeners:
            listener.onNodeBoundaryChange(nodeNum, newData)


class NodeBoundaryChangeListener:
    def onNodeBoundaryChange(self, nodeNum, newType):
        pass
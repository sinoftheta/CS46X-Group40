from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

#import enum

from .node_types.SourceSinkView import SourceSinkView
from .node_types.SourceSinkModel import SourceSinkModel
from .node_types.VariableBCView import VariableBCView
from .node_types.VariableBCModel import VariableBCModel
from .node_types.MixedBCView import MixedBCView
from .node_types.MixedBCModel import MixedBCModel


nodeTypeLabels = [
        "Source/Sink",
        "Variable Boundary Condition (Flow)",
        "Mixed Boundary Condition (Mass Transport)"
]

class NodeTypesController(QGroupBox):
    def __init__(self):
        super(NodeTypesController, self).__init__('Node Types')

        self.ssModels = []
        self.variableBCModels = []
        self.mixedBCModels = []

        self.currentNodeType = None

        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)

        self.nodeTypesLabel = QLabel("Node Boundary Type")
        self.nodeTypesLabel.setFont(QFont('Helvetica', 16))
        self.nodeTypesLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(self.nodeTypesLabel)

        self.nodeTypeSelector = TypeSelectorComboBox()
        self.nodeTypeSelector.currentIndexChanged.connect(self.nodeTypeSelectionChanged)
        self.layout.addWidget(self.nodeTypeSelector)

    def nodeTypeSelectionChanged(self, index):
        if self.currentNodeType != None and index != 0:
            # delete widgets from current table
            self.currentNodeType.typeTable.clearContents()
            self.layout.removeWidget(self.currentNodeType)
            self.currentNodeType.deleteLater()
            self.currentNodeType = None

        if index == 0:
            pass
        elif index == 1:
            self.currentNodeType = SourceSinkView(self.ssModels)
            self.layout.addWidget(self.currentNodeType)
        elif index == 2:
            self.currentNodeType = VariableBCView(self.variableBCModels)
            self.layout.addWidget(self.currentNodeType)
        elif index == 3:
            self.currentNodeType = MixedBCView(self.mixedBCModels)
            self.layout.addWidget(self.currentNodeType)

    def getNodeTypes(self):
        types = {}
        types['SSNodes'] = self.ssModels
        types['VariableBCNodes'] = self.variableBCModels
        types['MixedBCNodes'] = self.mixedBCModels
        return types

    #
    # Need listener from nodes screen to know when a boundary type is set on a node
    #

class TypeSelectorComboBox(QComboBox):
    def __init__(self):
        super(TypeSelectorComboBox, self).__init__()
        self.setFixedWidth(220)
        self.addItem('-Select Node Boundary Type-')
        for nodeType in nodeTypeLabels:
            self.addItem(nodeType)

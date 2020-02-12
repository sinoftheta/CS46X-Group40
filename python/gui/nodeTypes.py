from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

nodeTypeLabels = [
        "Constant Head (Dirichlet)",
        "Source/Sink",
        "Infiltration/Evaporation",
        "Seepage Face",
        "Mixed Boundary Condition"
]

class NodeTypes(QGroupBox):
    def __init__(self):
        super(NodeTypes, self).__init__('Node Types')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        self.nodeTypeSelector = TypeSelectorComboBox()

    def setPages(self):
        self.typeStack = QStackedLayout()
        # todo: create QGroupBox for each node type
        #   connect qcombobox change signal to set appropriate
        #   page from self.typeStack with setCurrentIndex

    def buildTables(self, nodeTypes):
        print(nodeTypes)
        # todo: build table for each type,
        # nodeTypes contains lists with node number of each type



class TypeSelectorComboBox(QComboBox):
    def __init__(self):
        super(BoundaryComboBox, self).__init__()
        for nodeType in nodeTypeLabels:
            self.addItem(nodeType)

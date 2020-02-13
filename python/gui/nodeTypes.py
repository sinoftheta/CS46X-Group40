from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .nodes import nodeTypeLabels


dirichletNodeLabels = [
        "Node",
        "Flow Equation",
        "Mass Transport"
]

neumannNodeLabels = [
        "Node",
        "Recharge/Discharge Rate",
        "Concentration"
]

variableBoundaryLabels = [
        "Node",
        "Constant Head\n(Dirichlet) Initially",
        "Constant Flux\n(Neumann) Initially",
        "Fraction of\nMaximum Flux",
        "Tributary Length"
]

seepageFaceLabels = [
        "Node",
        "Saturated\n(Dirichlet Boundary)",
        "Unsaturated\n(Neumann Boundary)"
]

mixedBoundaryLabels = [
        "Node",
        "Influx Concentration"
]

class NodeTypes(QGroupBox):
    def __init__(self):
        super(NodeTypes, self).__init__('Node Types')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        self.setupPage()

    def setNodeTypes(self, nodeTypes):
        self.nodeTypes = nodeTypes

    def setupPage(self):
        self.nodeTypeSelector = TypeSelectorComboBox()

        # attach function to a change in the selected value from drop down menu
        self.nodeTypeSelector.currentIndexChanged.connect(self.typeSelectionChanged)
        self.layout.addWidget(self.nodeTypeSelector)

        self.dirichlet = QGroupBox(nodeTypeLabels[0])
        self.dirichlet.setLayout(QVBoxLayout())

        self.neumann = QGroupBox(nodeTypeLabels[1])
        self.neumann.setLayout(QVBoxLayout())

        self.variableBoundary = QGroupBox(nodeTypeLabels[2])
        self.variableBoundary.setLayout(QVBoxLayout())

        self.seepageFace = QGroupBox(nodeTypeLabels[3])
        self.seepageFace.setLayout(QVBoxLayout())

        self.mixedBoundary = QGroupBox(nodeTypeLabels[4])
        self.mixedBoundary.setLayout(QVBoxLayout())

        self.typeStack = QStackedLayout()
        self.layout.addLayout(self.typeStack)
        self.typeStack.addWidget(self.dirichlet)
        self.typeStack.addWidget(self.neumann)
        self.typeStack.addWidget(self.variableBoundary)
        self.typeStack.addWidget(self.seepageFace)
        self.typeStack.addWidget(self.mixedBoundary)


    def buildTable(self, type):
        if (not self.nodeTypes):
            return
        if (type == nodeTypeLabels[0]):
            if (not hasattr(self, 'dirichletTable') and
                len(self.nodeTypes[type]) > 0):
                # Create table for constant head nodes
                self.createDirichletTable(self.nodeTypes[nodeTypeLabels[0]])
            # todo: else to update table
        elif (type == nodeTypeLabels[1]):
            if (not hasattr(self, 'neumannTable') and
                len(self.nodeTypes[type]) > 0):
                # Create table for source/sink nodes
                self.createNeumannTable(self.nodeTypes[nodeTypeLabels[1]])
            # todo: else to update table
        elif (type == nodeTypeLabels[2]):
            if (not hasattr(self, 'variableBoundaryTable') and
                len(self.nodeTypes[type]) > 0):
                # Create table for infiltration/evaporation nodes
                self.createVariableBoundaryTable(self.nodeTypes[nodeTypeLabels[2]])
            # todo: else to update table
        elif (type == nodeTypeLabels[3]):
            if (not hasattr(self, 'seepageFaceTable') and
                len(self.nodeTypes[type]) > 0):
                # Create table for nodes belonging to a seepage face element
                self.createSeepageFaceTable(self.nodeTypes[nodeTypeLabels[3]])
            # todo: else to update table
        elif (type == nodeTypeLabels[4]):
            if (not hasattr(self, 'mixedBoundaryTable') and
                len(self.nodeTypes[type]) > 0):
                # Create table for mixed boundary condition nodes
                self.createMixedBoundaryTable(self.nodeTypes[nodeTypeLabels[4]])
            # todo: else to update table

    def createDirichletTable(self, nodes):
        numNodes = len(nodes)
        self.dirichletTable = QTableWidget()
        self.dirichletTable.setRowCount(numNodes)
        self.dirichletTable.setColumnCount(len(dirichletNodeLabels))
        self.dirichletTable.verticalHeader().hide()
        self.dirichletTable.setHorizontalHeaderLabels(dirichletNodeLabels)
        self.dirichletTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)

        for row in range(0, numNodes):
            nodeLabel = QLabel(str(nodes[row]))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.dirichletTable.setCellWidget(row, 0, nodeLabel)
            flowCB = QCheckBox()
            # center the checkbox within the table cell
            flowCB.setStyleSheet("margin-left: 50%; margin-right: 50%;")
            self.dirichletTable.setCellWidget(row, 1, flowCB)
            massTransCB = QCheckBox()
            massTransCB.setStyleSheet("margin-left: 50%; margin-right: 50%;")
            self.dirichletTable.setCellWidget(row, 2, massTransCB)

        self.dirichlet.layout().addWidget(self.dirichletTable)

    def createNeumannTable(self, nodes):
        numNodes = len(nodes)
        self.neumannTable = QTableWidget()
        self.neumannTable.setRowCount(numNodes)
        self.neumannTable.setColumnCount(len(neumannNodeLabels))
        self.neumannTable.verticalHeader().hide()
        self.neumannTable.setHorizontalHeaderLabels(neumannNodeLabels)
        self.neumannTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)

        for node in range(0, numNodes):
            nodeLabel = QLabel(str(nodes[node]))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.neumannTable.setCellWidget(node, 0, nodeLabel)

            FQ = QDoubleSpinBox()
            FQ.setAlignment(Qt.AlignRight)
            FQ.setRange(-99999.99999999, 99999.99999999)
            FQ.setDecimals(8)
            FQ.setSingleStep(0.00000001)
            self.neumannTable.setCellWidget(node, 1, FQ)

            CFQ = QDoubleSpinBox()
            CFQ.setAlignment(Qt.AlignRight)
            CFQ.setRange(-99999.99999999, 99999.99999999)
            CFQ.setDecimals(8)
            CFQ.setSingleStep(0.00000001)
            self.neumannTable.setCellWidget(node, 2, CFQ)

        self.neumann.layout().addWidget(self.neumannTable)

    def createVariableBoundaryTable(self, nodes):
        numNodes = len(nodes)
        self.variableBoundaryTable = QTableWidget()
        self.variableBoundaryTable.setRowCount(numNodes)
        self.variableBoundaryTable.setColumnCount(len(variableBoundaryLabels))
        self.variableBoundaryTable.verticalHeader().hide()
        self.variableBoundaryTable.setHorizontalHeaderLabels(variableBoundaryLabels)
        self.variableBoundaryTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)

        for node in range(0, numNodes):
            nodeLabel = QLabel(str(nodes[node]))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.variableBoundaryTable.setCellWidget(node, 0, nodeLabel)

            dirichletCB = QCheckBox()
            # center the checkbox within the table cell
            dirichletCB.setStyleSheet("margin-left: 50%; margin-right: 50%;")
            self.variableBoundaryTable.setCellWidget(node, 1, dirichletCB)
            neumannCB = QCheckBox()
            neumannCB.setStyleSheet("margin-left: 50%; margin-right: 50%;")
            self.variableBoundaryTable.setCellWidget(node, 2, neumannCB)

            COEF = QDoubleSpinBox()
            COEF.setAlignment(Qt.AlignRight)
            COEF.setRange(-9999.9999, 9999.9999)
            COEF.setDecimals(4)
            COEF.setSingleStep(0.0001)
            self.variableBoundaryTable.setCellWidget(node, 1, COEF)

            VN = QDoubleSpinBox()
            VN.setAlignment(Qt.AlignRight)
            VN.setRange(-9999.9999, 9999.9999)
            VN.setDecimals(4)
            VN.setSingleStep(0.0001)
            self.variableBoundaryTable.setCellWidget(node, 2, VN)

        self.variableBoundary.layout().addWidget(self.variableBoundaryTable)


    def createSeepageFaceTable(self, nodes):
        numNodes = len(nodes)
        self.seepageFaceTable = QTableWidget()
        self.seepageFaceTable.setRowCount(numNodes)
        self.seepageFaceTable.setColumnCount(len(seepageFaceLabels))
        self.seepageFaceTable.verticalHeader().hide()
        self.seepageFaceTable.setHorizontalHeaderLabels(seepageFaceLabels)
        self.seepageFaceTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)

        for node in range(0, numNodes):
            nodeLabel = QLabel(str(nodes[node]))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.seepageFaceTable.setCellWidget(node, 0, nodeLabel)

            dirichletCB = QCheckBox()
            # center the checkbox within the table cell
            dirichletCB.setStyleSheet("margin-left: 50%; margin-right: 50%;")
            self.seepageFaceTable.setCellWidget(node, 1, dirichletCB)
            neumannCB = QCheckBox()
            neumannCB.setStyleSheet("margin-left: 50%; margin-right: 50%;")
            self.seepageFaceTable.setCellWidget(node, 2, neumannCB)

        self.seepageFace.layout().addWidget(self.seepageFaceTable)




    def createMixedBoundaryTable(self, nodes):
        numNodes = len(nodes)
        self.mixedBoundaryTable = QTableWidget()
        self.mixedBoundaryTable.setRowCount(numNodes)
        self.mixedBoundaryTable.setColumnCount(len(mixedBoundaryLabels))
        self.mixedBoundaryTable.verticalHeader().hide()
        self.mixedBoundaryTable.setHorizontalHeaderLabels(mixedBoundaryLabels)
        self.mixedBoundaryTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)

        for node in range(0, numNodes):
            nodeLabel = QLabel(str(nodes[node]))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.mixedBoundaryTable.setCellWidget(node, 0, nodeLabel)

            CN = QDoubleSpinBox()
            CN.setAlignment(Qt.AlignRight)
            CN.setRange(-9999.9999, 9999.9999)
            CN.setDecimals(4)
            CN.setSingleStep(0.0001)
            self.mixedBoundaryTable.setCellWidget(node, 1, CN)

        self.mixedBoundary.layout().addWidget(self.mixedBoundaryTable)


    def typeSelectionChanged(self, index):
        self.buildTable(nodeTypeLabels[index])
        self.typeStack.setCurrentIndex(index)

class TypeSelectorComboBox(QComboBox):
    def __init__(self):
        super(TypeSelectorComboBox, self).__init__()
        self.setFixedWidth(220)
        for nodeType in nodeTypeLabels:
            self.addItem(nodeType)

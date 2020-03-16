from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

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

class NodesView(QTableWidget):
    def __init__(self, nodes, setTableVal):
        super(NodesView, self).__init__()
        self.nodes = nodes
        self.setTableVal = setTableVal

        self.createTable()
        self.populateTable()
        

        #self.clearContents()

    def populateTable(self):
        numNodes = len(self.nodes)
        self.setRowCount(numNodes)
        self.clearContents()

        for row in range(0, numNodes):
            
            nodeLabel = QLabel(str(self.nodes[row].I))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.setCellWidget(row, 0, nodeLabel)

            bComboBox = BoundaryComboBox()
            bComboBox.setCurrentIndex(2)
            self.setCellWidget(row, 1, bComboBox)
            

            nodeX = QDoubleSpinBox()
            nodeX.setAlignment(Qt.AlignRight)
            nodeX.setRange(-9999.999, 9999.999)
            nodeX.setDecimals(3)
            nodeX.setSingleStep(0.001)
            nodeX.setValue(self.nodes[row].X)
            nodeX.textChanged.connect(lambda val: self.setTableVal(row, "X", val))
            self.setCellWidget(row, 2, nodeX)
            

            nodeY = QDoubleSpinBox()
            nodeY.setAlignment(Qt.AlignRight)
            nodeY.setRange(-9999.999, 9999.999)
            nodeY.setDecimals(3)
            nodeY.setSingleStep(0.001)
            nodeY.setValue(self.nodes[row].Y)
            self.setCellWidget(row, 3, nodeY)

            iPressure = QDoubleSpinBox()
            iPressure.setAlignment(Qt.AlignRight)
            iPressure.setRange(-9999.999, 9999.999)
            iPressure.setDecimals(3)
            iPressure.setSingleStep(0.001)
            iPressure.setValue(self.nodes[row].PHII)
            self.setCellWidget(row, 4, iPressure)


            iConcentration = QDoubleSpinBox()
            iConcentration.setAlignment(Qt.AlignRight)
            iConcentration.setRange(-9999.999, 9999.999)
            iConcentration.setDecimals(3)
            iConcentration.setSingleStep(0.001)
            iConcentration.setValue(self.nodes[row].CONCI)
            self.setCellWidget(row, 5, iConcentration)

        self.setRowCount(numNodes)

    def createTable(self):
        self.setColumnCount(len(nodeTableLabels))
        self.setMaximumWidth(730)
        # Set labels
        self.setHorizontalHeaderLabels(nodeTableLabels)
        self.verticalHeader().hide()
        # Set table column widths to match label size
        self.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)


    def nodeTypeCounts(self):# unused?
        # get node type from QComboBox widget in nodeTable
        if (hasattr(self, 'nodeTable')):
            nodeTypes = { type: [] for type in nodeTypeLabels }
            numNodes = self.rowCount()
            for row in range(0, numNodes):
                if (self.cellWidget(row, 1).currentText() != '-Select Boundary Type-'):
                    nodeTypes[self.cellWidget(row, 1).currentText()].append(row+1)
            return nodeTypes


class BoundaryComboBox(QComboBox):
    def __init__(self):
        super(BoundaryComboBox, self).__init__()
        self.addItem('-Select Boundary Type-')
        for nodeType in nodeTypeLabels:
            self.addItem(nodeType)

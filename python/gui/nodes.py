from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

nodeTableLabels = [
            "Node",
            "Boundary Type",
            "X-Coordinate",
            "Y-Coordinate",
            "Initial Pressure",
            "Initial Concentration" ]

nodeTypeLabels = [
        "Constant Head (Dirichlet)",
        "Source/Sink",
        "Variable Boundary Condition (Flow)",
        "Seepage Face",
        "Mixed Boundary Condition (Mass Transport)"
]

class Nodes(QGroupBox):
    def __init__(self):
        super(Nodes, self).__init__('Nodes')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        self.createTable()

    def buildTable(self, numNodes):
        if (numNodes == 0):
            return
        elif (hasattr(self, 'nodeTable') and (numNodes != self.nodeTable.rowCount())):
            self.nodeTable.clearContents()
            self.nodeTable.setRowCount(numNodes)
        elif (hasattr(self, 'nodeTable') and (numNodes == self.nodeTable.rowCount())):
            return

        for row in range(0, numNodes):
            nodeLabel = QLabel(str(row+1))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.nodeTable.setCellWidget(row, 0, nodeLabel)

            bComboBox = BoundaryComboBox()
            self.nodeTable.setCellWidget(row, 1, bComboBox)

            nodeX = QDoubleSpinBox()
            nodeX.setAlignment(Qt.AlignRight)
            nodeX.setRange(-9999.999, 9999.999)
            nodeX.setDecimals(3)
            nodeX.setSingleStep(0.001)
            self.nodeTable.setCellWidget(row, 2, nodeX)

            nodeY = QDoubleSpinBox()
            nodeY.setAlignment(Qt.AlignRight)
            nodeY.setRange(-9999.999, 9999.999)
            nodeY.setDecimals(3)
            nodeY.setSingleStep(0.001)
            self.nodeTable.setCellWidget(row, 3, nodeY)

            iPressure = QDoubleSpinBox()
            iPressure.setAlignment(Qt.AlignRight)
            iPressure.setRange(-9999.999, 9999.999)
            iPressure.setDecimals(3)
            iPressure.setSingleStep(0.001)
            self.nodeTable.setCellWidget(row, 4, iPressure)

            iConcentration = QDoubleSpinBox()
            iConcentration.setAlignment(Qt.AlignRight)
            iConcentration.setRange(-9999.999, 9999.999)
            iConcentration.setDecimals(3)
            iConcentration.setSingleStep(0.001)
            self.nodeTable.setCellWidget(row, 5, iConcentration)

    def createTable(self):
        self.nodeTable = QTableWidget()
        self.nodeTable.setColumnCount(len(nodeTableLabels))
        self.nodeTable.setMaximumWidth(730)
        # Set labels
        self.nodeTable.setHorizontalHeaderLabels(nodeTableLabels)
        self.nodeTable.verticalHeader().hide()
        # Set table column widths to match label size
        self.nodeTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.layout.addWidget(self.nodeTable)

    def nodeTypeCounts(self):
        # get node type from QComboBox widget in nodeTable
        if (hasattr(self, 'nodeTable')):
            nodeTypes = { type: [] for type in nodeTypeLabels }
            numNodes = self.nodeTable.rowCount()
            for row in range(0, numNodes):
                if (self.nodeTable.cellWidget(row, 1).currentText() != '-Select Boundary Type-'):
                    nodeTypes[self.nodeTable.cellWidget(row, 1).currentText()].append(row+1)
            return nodeTypes

    def getCONCI(self):
        CONCI = []
        if hasattr(self, 'nodeTable'):
            for row in range(0, self.nodeTable.rowCount()):
                # The hardcode 5 = column number, specified in buildTable() method above

                nodeValue = self.nodeTable.cellWidget(row, 5).value()
                CONCI.append(nodeValue)

        return CONCI

    def getNumRows():
        if hasattr(self, 'nodeTable'):
            return self.nodeTable.rowCount()
        return 0

    def getRow(self, row):
        rowData = {}
        if hasattr(self, 'nodeTable') and (row <= self.nodeTable.rowCount()):
            #Node
            rowData['NodeNum'] = self.nodeTable.cellWidget(row, 0).text()
            #Boundary Type (q combo box)
            rowData['Boundary'] = self.nodeTable.cellWidget(row, 1).currentText()
            #x - coord
            rowData['XCoord'] = self.nodeTable.cellWidget(row, 2).value()
            #y - coord
            rowData['YCoord'] = self.nodeTable.cellWidget(row, 3).value()
            #init pressure
            rowData['Pressure'] = self.nodeTable.cellWidget(row, 4).value()
            #init concentration
            rowData['Conce'] = self.nodeTable.cellWidget(row, 5).value()
        return rowData


    def getRowsWhere(self, rowFilter):
        rows = []
        for rowIndex in range(self.nodeTable.rowCount()):
            row = self.getRow(rowIndex)
            if rowFilter(row):
                rows.append(row)

        return rows




class BoundaryComboBox(QComboBox):
    def __init__(self):
        super(BoundaryComboBox, self).__init__()
        self.addItem('-Select Boundary Type-')
        for nodeType in nodeTypeLabels:
            self.addItem(nodeType)

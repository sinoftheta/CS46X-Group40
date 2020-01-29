from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

tableLabels = ["Node", "Boundary Type", "X-Coordinate",
            "Y-Coordinate", "Initial Pressure", "Initial Concentration"]

class Nodes(QGroupBox):
    def __init__(self):
        super(Nodes, self).__init__('Nodes')
        self.layout = QGridLayout()
        self.setLayout(self.layout)
        self.createTable(6)
        self.layout.addWidget(self.nodeTable)

    def createTable(self, numNodes):
        self.nodeTable = QTableWidget()
        self.nodeTable.setRowCount(numNodes+1)
        self.nodeTable.setColumnCount(6)
        # Set labels
        self.nodeTable.setHorizontalHeaderLabels(tableLabels)
        # Set table column widths to match label size
        self.nodeTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)

        for row in range(0, numNodes):
                self.nodeTable.setCellWidget(row, 0, QLabel(str(row)))

                nodeX = QSpinBox()
                nodeX.setRange(-9999, 9999)
                self.nodeTable.setCellWidget(row, 1, nodeX)

class MultiSelectMenu(QComboBox):
    def __init__(self):
        super(MultiSelectMenu, self).__init__()

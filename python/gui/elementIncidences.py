from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

class ElementIncidences(QGroupBox):
    def __init__(self):
        super(ElementIncidences, self).__init__('Element Incidences')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)

    def buildTable(self, numElements, maxNodes):
        if (numElements == 0):
            return
        elif (hasattr(self, 'incidenceTable') and (numElements != self.incidenceTable.rowCount())):
            self.incidenceTable.setRowCount(numElements)
        elif (hasattr(self, 'incidenceTable') and (numElements == self.incidenceTable.rowCount())):
            return
        else:
            self.createTable(numElements, maxNodes)

        for row in range(0, numElements):
            elementLabel = QLabel(str(row+1))
            elementLabel.setAlignment(Qt.AlignCenter)
            self.incidenceTable.setCellWidget(row, 0, elementLabel)
            for col in range(1, maxNodes+1):
                cell = QSpinBox()
                cell.setAlignment(Qt.AlignRight)
                cell.setRange(0, 999)
                self.incidenceTable.setCellWidget(row, col, cell)


    def createTable(self, numElements, maxNodes):
        incidenceTableLabels = ["Element", ]
        for i in range(0, numElements):
            incidenceTableLabels.append(str(i+1))
        self.incidenceTable = QTableWidget()
        self.incidenceTable.setRowCount(numElements)
        self.incidenceTable.setColumnCount(maxNodes+1)
        # Set labels
        self.incidenceTable.setHorizontalHeaderLabels(incidenceTableLabels)
        self.incidenceTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.incidenceTable.verticalHeader().hide()
        self.layout.addWidget(self.incidenceTable)

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

class ElementIncidences(QGroupBox):
    def __init__(self):
        super(ElementIncidences, self).__init__('Element Incidences')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        self.createTable()


    def buildTable(self, numElements, maxNodes):
        if (numElements == 0):
            return
        elif (hasattr(self, 'incidenceTable') and (numElements != self.incidenceTable.rowCount())):
            self.incidenceTable.clear()
        elif (hasattr(self, 'incidenceTable') and (numElements == self.incidenceTable.rowCount())):
            return

        self.incidenceTable.setRowCount(numElements)
        self.incidenceTable.setColumnCount(maxNodes+1)

        # Set labels
        incidenceTableLabels = ["Element"]
        for i in range(maxNodes):
            incidenceTableLabels.append(str(i+1))
        self.incidenceTable.setHorizontalHeaderLabels(incidenceTableLabels)

        for row in range(numElements):
            elementLabel = QLabel(str(row+1))
            elementLabel.setAlignment(Qt.AlignCenter)
            self.incidenceTable.setCellWidget(row, 0, elementLabel)
            for col in range(maxNodes):
                cell = QSpinBox()
                cell.setAlignment(Qt.AlignRight)
                cell.setRange(0, 999)
                self.incidenceTable.setCellWidget(row, col+1, cell)


    def createTable(self):
        self.incidenceTable = QTableWidget()
        self.incidenceTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.incidenceTable.verticalHeader().hide()
        self.layout.addWidget(self.incidenceTable)


    def getRow(self, row):
        rowData = {}
        if hasattr(self, 'incidenceTable') and row < self.incidenceTable.rowCount():
            rowData['ElementNum'] = self.incidenceTable.cellWidget(row, 0).text()
            # as chapter 5, figure 1 shows
            # incidenceOrder = [1, 5, 6, 2, 7, 8, 3, 9, 10, 4, 11, 12]
            incidenceOrder = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
            rowData['IncidentNodes'] = filter(lambda elem: elem != "0", [self.incidenceTable.cellWidget(row, x).text() for x in incidenceOrder])
            rowData['IncidentNodes'] = list(filter(lambda elem: elem != "0", rowData['IncidentNodes']))

        return rowData

    def getRowsWhere(self, predicate):
        rows = []
        for rowIndex in range(self.incidenceTable.rowCount()):
            row = self.getRow(rowIndex)
            if predicate(row):
                rows.append(row)

        return rows

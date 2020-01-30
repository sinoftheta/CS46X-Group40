from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

elementTableLabels = [
    "Element", "Material \n Group",
    "Horizontal Saturated \n Hydraulic Conductivity",
    "Vertical Saturated \n Hydraulic Conductivity",
    "Longitudinal \n Dispersivity", "Transverse \n Dispersivity",
    "Porosity", "Moisture \n Content", "Compressibility",
    "Distribution \n Coefficient", "Decay \n Constant", "Dry Bulk \n Density"
]

class Elements(QGroupBox):
    def __init__(self):
        super(Elements, self).__init__('Elements')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        #self.layout.setHorizontalSpacing(30)
        #self.createTable(24)

    def buildTable(self, numElements, numMaterials):
        if (hasattr(self, 'elementTable') and (numElements != self.elementTable.rowCount())):
            self.elementTable.setRowCount(numElements)
        elif (hasattr(self, 'elementTable') and (numElements == self.elementTable.rowCount())):
            return
        else:
            self.createTable(numElements)

        for row in range(0, numElements):
            elementLabel = QLabel(str(row+1))
            elementLabel.setAlignment(Qt.AlignCenter)
            self.elementTable.setCellWidget(row, 0, elementLabel)
            mComboBox = MaterialsComboBox(numMaterials)
            self.elementTable.setCellWidget(row, 1, mComboBox)

    def createTable(self, numElements):
        self.elementTable = QTableWidget()
        self.elementTable.setRowCount(numElements)
        self.elementTable.setColumnCount(len(elementTableLabels))
        # Set labels
        self.elementTable.setHorizontalHeaderLabels(elementTableLabels)
        self.elementTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.elementTable.verticalHeader().hide()
        self.layout.addWidget(self.elementTable)


class MaterialsComboBox(QComboBox):
    def __init__(self, numMaterials):
        super(MaterialsComboBox, self).__init__()
        for i in range(numMaterials):
            self.addItem(str(i+1))

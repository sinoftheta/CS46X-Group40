from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

elementTableLabels = [
    "Element", "Material\nGroup",
    "Horiz. Sat.\nH. Conductivity",
    "Vert. Sat.\nH. Conductivity",
    "Longitudinal\nDispersivity",
    "Transverse\nDispersivity",
    "Porosity",
    "Moisture\nContent",
    "Compressibility",
    "Distribution\nCoefficient",
    "Decay\nConstant",
    "Dry Bulk\nDensity"
]

class Elements(QGroupBox):
    def __init__(self):
        super(Elements, self).__init__('Elements')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        self.createTable()

    def buildTable(self, numElements, numMaterials):
        if (numElements == 0):
            return
        elif (hasattr(self, 'elementTable') and (numElements != self.elementTable.rowCount())):
            self.elementTable.clearContents()
            self.elementTable.setRowCount(numElements)
        elif (hasattr(self, 'elementTable') and (numElements == self.elementTable.rowCount())):
            return

        for row in range(0, numElements):
            elementLabel = QLabel(str(row+1))
            elementLabel.setAlignment(Qt.AlignCenter)
            self.elementTable.setCellWidget(row, 0, elementLabel)

            mComboBox = MaterialsComboBox(numMaterials)
            self.elementTable.setCellWidget(row, 1, mComboBox)

            hSatHConduct = QDoubleSpinBox()
            hSatHConduct.setAlignment(Qt.AlignRight)
            hSatHConduct.setRange(-9999.99999, 9999.99999)
            hSatHConduct.setDecimals(5)
            hSatHConduct.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 2, hSatHConduct)

            vSatHConduct = QDoubleSpinBox()
            vSatHConduct.setAlignment(Qt.AlignRight)
            vSatHConduct.setRange(-9999.99999, 9999.99999)
            vSatHConduct.setDecimals(5)
            vSatHConduct.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 3, vSatHConduct)

            lDispersivity = QDoubleSpinBox()
            lDispersivity.setAlignment(Qt.AlignRight)
            lDispersivity.setRange(-9999.99999, 9999.99999)
            lDispersivity.setDecimals(5)
            lDispersivity.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 4, lDispersivity)

            hDispersivity = QDoubleSpinBox()
            hDispersivity.setAlignment(Qt.AlignRight)
            hDispersivity.setRange(-9999.99999, 9999.99999)
            hDispersivity.setDecimals(5)
            hDispersivity.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 5, hDispersivity)

            porosity = QDoubleSpinBox()
            porosity.setAlignment(Qt.AlignRight)
            porosity.setRange(-9999.99999, 9999.99999)
            porosity.setDecimals(5)
            porosity.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 6, porosity)

            moistContent = QDoubleSpinBox()
            moistContent.setAlignment(Qt.AlignRight)
            moistContent.setRange(-9999.99999, 9999.99999)
            moistContent.setDecimals(5)
            moistContent.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 7, moistContent)

            compressibility = QDoubleSpinBox()
            compressibility.setAlignment(Qt.AlignRight)
            compressibility.setRange(-9999.99999, 9999.99999)
            compressibility.setDecimals(5)
            compressibility.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 8, compressibility)

            distribCoef = QDoubleSpinBox()
            distribCoef.setAlignment(Qt.AlignRight)
            distribCoef.setRange(-9999.99999, 9999.99999)
            distribCoef.setDecimals(5)
            distribCoef.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 9, distribCoef)

            radDecay = QDoubleSpinBox()
            radDecay.setAlignment(Qt.AlignRight)
            radDecay.setRange(-9999.99999, 9999.99999)
            radDecay.setDecimals(5)
            radDecay.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 10, radDecay)

            dryBulk = QDoubleSpinBox()
            dryBulk.setAlignment(Qt.AlignRight)
            dryBulk.setRange(-9999.99999, 9999.99999)
            dryBulk.setDecimals(5)
            dryBulk.setSingleStep(0.00001)
            self.elementTable.setCellWidget(row, 11, dryBulk)

    def createTable(self):
        self.elementTable = QTableWidget()
        self.elementTable.setColumnCount(len(elementTableLabels))
        # Set labels
        self.elementTable.setHorizontalHeaderLabels(elementTableLabels)
        self.elementTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.elementTable.verticalHeader().hide()
        self.layout.addWidget(self.elementTable)

    def getRHO(self):
        RHO = []

        for row in range(0, self.elementTable.rowCount()):
            elementValue = self.elementTable.cellWidget(row, 11).value()
            RHO.append(elementValue)

        return RHO

    def getRow(self, row):
        rowData = {}
        if hasattr(self, 'elementTable') and row < self.elementTable.rowCount():
            rowData['ElementNum'] = self.elementTable.cellWidget(row, 0).text()
            rowData['Material'] = self.elementTable.cellWidget(row, 1).text()
            rowData['HorizontalConductivity'] = self.elementTable.cellWidget(row, 2).text()
            rowData['VerticalConductivity'] = self.elementTable.cellWidget(row, 3).text()
            rowData['LongitudinalDispersivity'] = self.elementTable.cellWidget(row, 4).text()
            rowData['TransverseDispersivity'] = self.elementTable.cellWidget(row, 5).text()
            rowData['Porosity'] = self.elementTable.cellWidget(row, 6).text()
            rowData['MoistureContent'] = self.elementTable.cellWidget(row, 7).text()
            rowData['Compressibility'] = self.elementTable.cellWidget(row, 8).text()
            rowData['DistributionCoefficient'] = self.elementTable.cellWidget(row, 9).text()
            rowData['DecayConstant'] = self.elementTable.cellWidget(row, 10).text()
            rowData['DryDensity'] = self.elementTable.cellWidget(row, 11).text()
        return rowData


    def getElementsWhere(self, predicate):
        rows = []
        for rowIndex in range(self.elementTable.rowCount()):
            row = self.getRow(rowIndex)
            if predicate(row):
                rows.append(row)

        return rows

class MaterialsComboBox(QComboBox):
    def __init__(self, numMaterials):
        super(MaterialsComboBox, self).__init__()
        for i in range(numMaterials):
            self.addItem(str(i+1))

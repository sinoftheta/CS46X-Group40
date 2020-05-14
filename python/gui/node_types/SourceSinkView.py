from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

ssNodeLabels = [
        "Node",
        "Charge Rate",
        "Concentration"
]

class SourceSinkView(QGroupBox):
    def __init__(self, models):
        super(SourceSinkView, self).__init__('Source or Sink Nodes')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)

        self.viewModels = models

        self.typeTable = QTableWidget()
        self.typeTable.setColumnCount(len(ssNodeLabels))
        self.typeTable.verticalHeader().hide()
        self.typeTable.setHorizontalHeaderLabels(ssNodeLabels)
        self.typeTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.layout.addWidget(self.typeTable)

        self.updateView(models)

    def updateView(self, models):
        self.typeTable.clearContents()
        self.viewModels = models
        self.typeTable.setRowCount(len(models))

        row = 0
        for node in models:
            nodeLabel = QLabel(str(node.nodeID))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.typeTable.setCellWidget(row, 0, nodeLabel)

            FQ = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
            FQ.setAlignment(Qt.AlignRight)
            FQ.setRange(-99999.99999999, 99999.99999999)
            FQ.setDecimals(8)
            FQ.setSingleStep(0.00000001)
            FQ.setValue(node.FQ)
            FQ.valueChanged.connect(self.updateModel(row, "FQ"))
            self.typeTable.setCellWidget(row, 1, FQ)

            CFQ = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
            CFQ.setAlignment(Qt.AlignRight)
            CFQ.setRange(-99999.99999999, 99999.99999999)
            CFQ.setDecimals(8)
            CFQ.setSingleStep(0.00000001)
            CFQ.setValue(node.CFQ)
            CFQ.valueChanged.connect(self.updateModel(row, "CFQ"))
            self.typeTable.setCellWidget(row, 2, CFQ)

            row += 1

    def updateModel(self, index, var):
        def inner(newValue):
            if var == "FQ":
                self.viewModels[index] = newValue
            elif var == "CFQ":
                self.viewModels[index] = newValue

        return inner

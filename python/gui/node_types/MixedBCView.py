from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

mixedBoundaryLabels = [
        "Node",
        "Influx Concentration"
]

class MixedBCView(QGroupBox):
    def __init__(self, models):
        super(MixedBCView, self).__init__('Mixed Boundary Nodes')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)

        self.viewModels = models

        self.typeTable = QTableWidget()
        self.typeTable.setColumnCount(len(mixedBoundaryLabels))
        self.typeTable.verticalHeader().hide()
        self.typeTable.setHorizontalHeaderLabels(mixedBoundaryLabels)
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

            CN = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
            CN.setAlignment(Qt.AlignRight)
            CN.setRange(-9999.9999, 9999.9999)
            CN.setDecimals(4)
            CN.setSingleStep(0.0001)
            CN.valueChanged.connect(lambda newValue: self.viewModels[row].setVal("CN", newValue))
            self.typeTable.setCellWidget(row, 1, CN)

            row += 1

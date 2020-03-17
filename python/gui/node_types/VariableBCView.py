from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

variableBoundaryLabels = [
        "Node",
        "Constant Head\n(Dirichlet) Initially",
        "Constant Flux\n(Neumann) Initially",
        "Fraction of\nMaximum Flux",
        "Tributary Length"
]

class VariableBCView(QGroupBox):
    def __init__(self, models):
        super(VariableBCView, self).__init__('Variable Boundary Nodes')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)

        self.viewModels = models

        self.typeTable = QTableWidget()
        self.typeTable.verticalHeader().hide()
        self.typeTable.setColumnCount(len(variableBoundaryLabels))
        self.typeTable.setHorizontalHeaderLabels(variableBoundaryLabels)
        self.typeTable.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeToContents)
        self.layout.addWidget(self.typeTable)

        self.updateView(models)

    def updateView(self, models):
        self.typeTable.clearContents()
        self.viewModels = models
        self.typeTable.setRowCount(len(self.viewModels))

        row = 0
        for node in models:
            nodeLabel = QLabel(str(node.nodeID))
            nodeLabel.setAlignment(Qt.AlignCenter)
            self.typeTable.setCellWidget(row, 0, nodeLabel)
            dirichletCB = QCheckBox()
            dirichletCB.setStyleSheet("margin-left: 50%; margin-right: 50%;")
            dirichletCB.toggled.connect(lambda: self.updateNodeCondition(row, "HEAD"))
            self.typeTable.setCellWidget(row, 1, dirichletCB)
            neumannCB = QCheckBox()
            neumannCB.setStyleSheet("margin-left: 50%; margin-right: 50%;")
            neumannCB.toggled.connect(lambda: self.updateNodeCondition(row, "FLUX"))
            self.typeTable.setCellWidget(row, 2, neumannCB)

            COEF = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
            COEF.setAlignment(Qt.AlignRight)
            COEF.setRange(-9999.9999, 9999.9999)
            COEF.setDecimals(4)
            COEF.setSingleStep(0.0001)
            COEF.valueChanged.connect(lambda newVal: self.viewModels[row].setVal("COEF", newVal))
            self.typeTable.setCellWidget(row, 3, COEF)

            VN = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
            VN.setAlignment(Qt.AlignRight)
            VN.setRange(-9999.9999, 9999.9999)
            VN.setDecimals(4)
            VN.setSingleStep(0.0001)
            VN.valueChanged.connect(lambda newVal: self.viewModels[row].setVal("VN", newVal))
            self.typeTable.setCellWidget(row, 4, VN)

            row += 1


    def updateNodeCondition(self, index, option):
        if option == "HEAD":
            self.viewModels[index].dirichlet = not self.viewModels[index].dirichlet
        elif option == "FLUX":
            self.viewModels[index].neumann = not self.viewModels[index].neumann

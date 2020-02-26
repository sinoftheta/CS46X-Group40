from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

class ElementView(QGroupBox):
    def __init__(self, elementModel, maxMaterialGroup):
        super(ElementView, self).__init__('Element ' + str(elementModel.elementNumber))
        self.viewModel = elementModel

        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)
        self.layout.setSpacing(20)
        self.layout.setContentsMargins(20, 10, 0, 50)

        materialGroupLabel = QLabel("Material Group")
        materialGroupLabel.setFont(QFont('Arial', 16))
        materialGroupLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(materialGroupLabel)
        self.materialCB = MaterialsComboBox(maxMaterialGroup)
        self.materialCB.currentIndexChanged.connect(self.updateModelMaterialGroup)
        self.materialCB.setCurrentIndex(self.viewModel.materialGroup - 1)

        self.layout.addWidget(self.materialCB)


        elementIncidenceLabel = QLabel("Element Incidences")
        elementIncidenceLabel.setFont(QFont('Arial', 16))
        elementIncidenceLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(elementIncidenceLabel)

        self.setIncidenceInputs()

    def setIncidenceInputs(self):
        self.incidenceLayout = QGridLayout()
        self.incidenceLayout.setHorizontalSpacing(20)
        self.incidenceLayout.setVerticalSpacing(10)

        col = row = 0
        for node in range(self.viewModel.maxIncidenceCount):
            if (not node % 6):
                row += 2
                col = 0

            incidenceLabel = QLabel("Incidence " + str(node+1))
            incidenceLabel.setFont(QFont('Arial', 13))
            incidenceLabel.setAlignment(Qt.AlignLeft)
            self.incidenceLayout.addWidget(incidenceLabel, row, col)

            incidenceInput = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
            incidenceInput.setAlignment(Qt.AlignRight)
            incidenceInput.setRange(0, self.viewModel.nodeCount)
            incidenceInput.setFixedWidth(60)
            incidenceInput.setValue(self.viewModel.incidences[node])
            incidenceInput.valueChanged.connect(self.updateModelIncidence(node))
            self.incidenceLayout.addWidget(incidenceInput, (row + 1), col)

            col += 1

        self.layout.addLayout(self.incidenceLayout)

    def updateModelMaterialGroup(self):
        index = self.materialCB.currentIndex()
        self.viewModel.materialGroup = int(self.materialCB.itemText(index))

    def updateModelIncidence(self, index):
        def listener(node):
            self.viewModel.incidences[index] = node

        return listener

class MaterialsComboBox(QComboBox):
    def __init__(self, numMaterials):
        super(MaterialsComboBox, self).__init__()
        for i in range(numMaterials):
            self.addItem(str(i+1))
        self.setFixedWidth(50)

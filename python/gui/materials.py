from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

materialsTableLabels = [ "Pressure Head",
                "Hydraulic Conductivity",
                "Moisture Content" ]

class Materials(QGroupBox):
    def __init__(self):
        super(Materials, self).__init__('Materials')
        #self.layout = QGridLayout()
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        #self.layout.setHorizontalSpacing(30)
        self.setLayout(self.layout)

    def MaterialsLayout(self, numMaterials):
        if (hasattr(self, 'materialGroup') and (numMaterials != self.materialGroup.count())):
            self.updateMaterialGroups(numMaterials)
            return
        elif (hasattr(self, 'materialGroup') and (numMaterials == self.materialGroup.count())):
            return
        else:
            if (numMaterials > 0):
                self.setMaterialGroupStack(numMaterials)
                self.setGroupMenu(numMaterials)

                materialGroupLabel = QLabel("Material Group")
                materialGroupLabel.setAlignment(Qt.AlignLeft)
                self.layout.addWidget(materialGroupLabel)
                self.layout.addWidget(self.materialGroup)
                self.layout.addWidget(self.materialsStack)


    def setGroupMenu(self, numMaterials):
        self.materialGroup = QComboBox()
        self.materialGroup.setFixedWidth(120)

        # Set Listener for changes to selection in material group menu
        self.materialGroup.currentIndexChanged.connect(self.materialGroupChange)

        for i in range(0, numMaterials):
            self.materialGroup.addItem(str(i+1))

    def updateMaterialGroups(self, numMaterials):
        currentNumMats = self.materialGroup.count()
        if (currentNumMats < numMaterials):
            for i in range (currentNumMats, numMaterials):
                self.materialGroup.addItem(str(i+1))
                self.materialsStack.addWidget(MaterialOptions(i+1))
        elif (currentNumMats > numMaterials):
            for i in range (currentNumMats, numMaterials-1, -1):
                print(i)
                self.materialGroup.removeItem(i)
                self.materialsStack.removeWidget(self.materialsStack.widget(i))

    def setMaterialGroupStack(self, numMaterials):
        self.materialsStack = QStackedWidget()

        # Create section for each material group
        for i in range(0, numMaterials):
            self.materialsStack.addWidget(MaterialOptions(i+1))


    def materialGroupChange(self, materialGroup):
        #print("Group " + str(materialGroup) + " selected")
        self.materialsStack.setCurrentIndex(materialGroup)

class MaterialOptions(QGroupBox):
    def __init__(self, material):
        super(MaterialOptions, self).__init__("Material " + str(material))
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignTop | Qt.AlignLeft)
        self.layout.setSpacing(20)
        self.setLayout(self.layout)
        pointsLabel = QLabel("Number of Interpolation Points")
        pointsLabel.setAlignment(Qt.AlignLeft)
        self.ISPL = QLineEdit()
        self.ISPL.setFixedWidth(60)
        self.ISPL.editingFinished.connect(self.updateInputs)
        self.ISPL.setAlignment(Qt.AlignRight)
        self.layout.addWidget(pointsLabel)
        self.layout.addWidget(self.ISPL)

    def updateInputs(self):
        numPoints = int(self.ISPL.text())
        if numPoints > 15:
            numPoints = 15
        if (hasattr(self, "XPSI")):
            self.clearLayouts()
            self.setInputs(numPoints)
        else:
            InterpolationPointsLabel = QLabel("Interpolation Points")
            InterpolationPointsLabel.setFont(QFont('Arial', 16))
            InterpolationPointsLabel.setAlignment(Qt.AlignLeft)
            self.layout.addWidget(InterpolationPointsLabel)
            self.inputLayout = QHBoxLayout()
            self.inputLayout.setAlignment(Qt.AlignCenter)
            self.layout.addLayout(self.inputLayout)

            self.XPSI = QVBoxLayout()
            self.XPSI.setContentsMargins(0, 0, 20, 2)
            self.XPSI.setSpacing(0)
            self.XPSI.setAlignment(Qt.AlignCenter)
            self.XM = QVBoxLayout()
            self.XM.setContentsMargins(0, 0, 20, 2)
            self.XM.setSpacing(0)
            self.XM.setAlignment(Qt.AlignCenter)
            self.XK = QVBoxLayout()
            self.XK.setContentsMargins(0, 0, 20, 2)
            self.XK.setSpacing(0)
            self.XK.setAlignment(Qt.AlignCenter)
            self.inputLayout.addLayout(self.XPSI)
            self.inputLayout.addLayout(self.XM)
            self.inputLayout.addLayout(self.XK)
            pressureHeadLabel = QLabel("Pressure Head")
            pressureHeadLabel.setFont(QFont('Arial', 13))
            pressureHeadLabel.setAlignment(Qt.AlignLeft)
            hConductivityLabel = QLabel("Hydraulic Conductivity")
            hConductivityLabel.setFont(QFont('Arial', 13))
            hConductivityLabel.setAlignment(Qt.AlignLeft)
            moistureContentLabel = QLabel("Moisture Content")
            moistureContentLabel.setFont(QFont('Arial', 13))
            moistureContentLabel.setAlignment(Qt.AlignLeft)

            self.XM.addWidget(moistureContentLabel)
            self.XPSI.addWidget(pressureHeadLabel)
            self.XK.addWidget(hConductivityLabel)


            self.setInputs(numPoints)

    def setInputs(self, numPoints):
            for i in range(0, numPoints):
                entryInput = QDoubleSpinBox()
                entryInput.setDecimals(3)
                entryInput.setSingleStep(0.001)
                entryInput.setRange(-999.999, 9999.999)
                entryInput.setAlignment(Qt.AlignRight)
                entryInput.setFixedWidth(85)
                self.XPSI.addWidget(entryInput)

            for i in range(0, numPoints):
                entryInput = QDoubleSpinBox()
                entryInput.setDecimals(3)
                entryInput.setSingleStep(0.001)
                entryInput.setRange(-999.999, 9999.999)
                entryInput.setAlignment(Qt.AlignRight)
                entryInput.setFixedWidth(85)
                self.XM.addWidget(entryInput)

            for i in range(0, numPoints):
                entryInput = QDoubleSpinBox()
                entryInput.setDecimals(3)
                entryInput.setSingleStep(0.001)
                entryInput.setRange(-999.999, 9999.999)
                entryInput.setAlignment(Qt.AlignRight)
                entryInput.setFixedWidth(85)
                self.XK.addWidget(entryInput)

    def clearLayouts(self):
        for inputWidget in range(0, self.inputLayout.count()):
            for i in reversed(range(1, self.inputLayout.itemAt(inputWidget).count())):
                if (i != 0):
                    widget = self.inputLayout.itemAt(inputWidget).takeAt(i).widget()
                    self.inputLayout.itemAt(inputWidget).removeWidget(widget)
                    widget.deleteLater()

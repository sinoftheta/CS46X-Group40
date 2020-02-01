from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

materialsTableLabels = []

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
            print("In currentNumMats > numMats")
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
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.layout.setSpacing(20)
        self.setLayout(self.layout)
        pointsLabel = QLabel("Number of Interpolation Points")
        pointsLabel.setAlignment(Qt.AlignLeft)
        self.ISPL = QLineEdit()
        self.ISPL.setFixedWidth(120)
        self.ISPL.editingFinished.connect(self.updateInputs)
        self.ISPL.setAlignment(Qt.AlignRight)
        self.layout.addWidget(pointsLabel)
        self.layout.addWidget(self.ISPL)

    def updateInputs(self):
        numPoints = int(self.ISPL.text())
        if (hasattr(self, "XPSI")):
            self.clearLayouts()
            self.setInputs(numPoints)
        else:
            InterpolationPointsLabel = QLabel("Interpolation Points")
            InterpolationPointsLabel.setFont(QFont('Helvetica', 14))
            InterpolationPointsLabel.setAlignment(Qt.AlignLeft)
            self.layout.addWidget(InterpolationPointsLabel)
            # spacer = QSpacerItem(5, 10, QSizePolicy.Minimum, QSizePolicy.Expanding)
            # self.layout.addItem(spacer)
            pressureHeadLabel = QLabel("Pressure Head")
            pressureHeadLabel.setFont(QFont('Helvetica', 12))
            pressureHeadLabel.setAlignment(Qt.AlignLeft)
            self.layout.addWidget(pressureHeadLabel)
            self.XPSI = QHBoxLayout()
            self.layout.addLayout(self.XPSI)
            moistureContentLabel = QLabel("Moisture Content")
            moistureContentLabel.setFont(QFont('Helvetica', 12))
            moistureContentLabel.setAlignment(Qt.AlignLeft)
            self.layout.addWidget(moistureContentLabel)
            self.XM = QHBoxLayout()
            self.layout.addLayout(self.XM)
            hConductivityLabel = QLabel("Hydraulic Conductivity")
            hConductivityLabel.setFont(QFont('Helvetica', 12))
            hConductivityLabel.setAlignment(Qt.AlignLeft)
            self.layout.addWidget(hConductivityLabel)
            self.XK = QHBoxLayout()
            self.layout.addLayout(self.XK)
            self.setInputs(numPoints)

    def setInputs(self, numPoints):
            for i in range(0, numPoints):
                entryInput = QDoubleSpinBox()
                entryInput.setDecimals(3)
                entryInput.setSingleStep(0.001)
                entryInput.setAlignment(Qt.AlignRight)
                self.XPSI.addWidget(entryInput)

            for i in range(0, numPoints):
                entryInput = QDoubleSpinBox()
                entryInput.setDecimals(3)
                entryInput.setSingleStep(0.001)
                entryInput.setAlignment(Qt.AlignRight)
                self.XM.addWidget(entryInput)

            for i in range(0, numPoints):
                entryInput = QDoubleSpinBox()
                entryInput.setDecimals(3)
                entryInput.setSingleStep(0.001)
                entryInput.setAlignment(Qt.AlignRight)
                self.XK.addWidget(entryInput)

    def clearLayouts(self):
        for i in reversed(range(self.XPSI.count())):
            widget = self.XPSI.itemAt(i).widget()
            self.XPSI.removeWidget(widget)
            widget.deleteLater()
            widget = self.XM.itemAt(i).widget()
            self.XM.removeWidget(widget)
            widget.deleteLater()
            widget = self.XK.itemAt(i).widget()
            self.XK.removeWidget(widget)
            widget.deleteLater()

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .element_properties.elementPropertiesModel import ElementPropertiesModel
from .element_properties.elementPropertiesView import ElementPropertiesView

from .BasicParametersController import BasicParameterChangeListener

class ElementPropertiesController(QGroupBox, BasicParameterChangeListener):
    def __init__(self):
        super(ElementPropertiesController, self).__init__('Material Properties')

        # List of element property view models
        self.elementPropertiesModels = []

        # Current Number of material groups in Simulation
        self.materialGroupCount = 1

        # Currently shown material group
        self.currentMaterialGroup = None

        self.layout = QHBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft)
        #self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.setSpacing(20)
        self.setLayout(self.layout)

        self.materialGroupListLayout = QVBoxLayout()
        self.materialGroupListLayout.setAlignment(Qt.AlignTop)
        self.materialGroupListLayout.setContentsMargins(0, 5, 0, 10)

        self.materialGroupListLabel = QLabel("Select Material Group")
        self.materialGroupListLabel.setFont(QFont('Helvetica', 16))
        self.materialGroupListLabel.setAlignment(Qt.AlignLeft)
        self.materialGroupListLayout.addWidget(self.materialGroupListLabel)

        self.materialGroupList = QListWidget()
        self.materialGroupList.itemClicked.connect(self.materialGroupSelectionChange)
        self.materialGroupList.setFixedWidth(120)
        self.materialGroupListLayout.addWidget(self.materialGroupList)

        self.layout.addLayout(self.materialGroupListLayout)

    def getElementProperties(self):
        return self.elementPropertiesModels

    def updateView(self, elementPropertiesModels):
        self.onMaterialCountChange(0)

        for epm in elementPropertiesModels:
            newMaterialGroup = QListWidgetItem(str(epm.materialGroupId))
            newMaterialGroup.setTextAlignment(Qt.AlignCenter)
            self.elementPropertiesModels.append(epm)
            self.materialGroupList.addItem(newMaterialGroup)

    def pushMaterialGroup(self):
        materialGroupId = str(len(self.elementPropertiesModels) + 1)
        viewModel = ElementPropertiesModel(materialGroupId)
        newMaterialGroup = QListWidgetItem(materialGroupId)
        newMaterialGroup.setTextAlignment(Qt.AlignCenter)
        self.elementPropertiesModels.append(viewModel)
        self.materialGroupList.addItem(newMaterialGroup)

    def popMaterialGroup(self):
        lastMaterialGroup = self.elementPropertiesModels.pop()
        materialGroup = self.materialGroupList.takeItem(self.materialGroupList.count() - 1)

    def materialGroupSelectionChange(self, materialGroup):
        if not len(self.elementPropertiesModels):
            return

        if self.currentMaterialGroup != None:
            # TODO: delete widgets from self.currentMaterialGroup
            self.layout.removeWidget(self.currentMaterialGroup)
            self.currentMaterialGroup.deleteLater()
            self.currentMaterialGroup = None

        materialGroupIndex = int(materialGroup.text()) - 1
        viewModel = self.elementPropertiesModels[materialGroupIndex]
        self.currentMaterialGroup = ElementPropertiesView(viewModel)
        self.layout.addWidget(self.currentMaterialGroup)

    # BasicParameterChangeListener "interface" method
    def onMaterialCountChange(self, count):
        if self.currentMaterialGroup != None:
            # TODO: delete widgets from self.currentMaterialGroup
            self.layout.removeWidget(self.currentMaterialGroup)
            self.currentMaterialGroup.deleteLater()
            self.currentMaterialGroup = None
            self.materialGroupList.clearSelection()

        if count > len(self.elementPropertiesModels):
            while count > len(self.elementPropertiesModels):
                self.pushMaterialGroup()
        else:
            while len(self.elementPropertiesModels) > count:
                self.popMaterialGroup()

        self.materialGroupCount = count

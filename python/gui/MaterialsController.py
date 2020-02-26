from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


from .material.MaterialModel import MaterialModel
from .material.MaterialView import MaterialView

from .BasicParametersController import BasicParameterChangeListener

class MaterialsController(QGroupBox, BasicParameterChangeListener):
    def __init__(self):
        super(MaterialsController, self).__init__('Materials')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)

        # list of material models that this controller is reponsible for
        self.materialModels = []

        # list of objects that care about how many materials exist
        self.materialChangeListeners = []

        self.materialGroupLabel = QLabel("Material Group")
        self.materialGroupLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(self.materialGroupLabel)

        self.materialGroup = QComboBox()
        self.materialGroup.setFixedWidth(50)
        self.materialGroup.currentIndexChanged.connect(self.materialGroupChanged)
        self.layout.addWidget(self.materialGroup)

        # this is the current material view
        self.currentMaterialGroup = None

        self.setLayout(self.layout)

    def getMaterials(self):
        return self.materialModels

    def pushMaterialGroup(self):
        group = str(len(self.materialModels) + 1)
        materialModel = MaterialModel(group)
        self.materialModels.append(materialModel)
        self.materialGroup.addItem(group)

        self.notifyMaterialAdded(materialModel)

    def popMaterialGroup(self):
        lastMaterial = self.materialModels.pop()
        self.materialGroup.removeItem(self.materialGroup.count() - 1)

        self.notifyMaterialRemoved(lastMaterial)

    def materialGroupChanged(self):
        if self.currentMaterialGroup != None:
            self.layout.removeWidget(self.currentMaterialGroup)
            self.currentMaterialGroup.deleteLater()
            self.currentMaterialGroup = None

        groupIndex = self.materialGroup.currentIndex()

        model = self.materialModels[groupIndex]
        self.currentMaterialGroup = MaterialView(model)
        self.layout.addWidget(self.currentMaterialGroup)

    def onMaterialCountChange(self, count):
        if count > len(self.materialModels):
            while count > len(self.materialModels):
                self.pushMaterialGroup()
        else:
            while len(self.materialModels) > count:
                self.popMaterialGroup()

    #
    # Listener functions
    #
    def addMaterialChangeListener(self, listener):
        # don't allow duplicate listeners
        if listener not in self.materialChangeListeners:
            self.materialChangeListeners.append(listener)

    def removeMaterialChangeListener(self, listener):
        self.materialChangeListeners.remove(listener)

    def notifyMaterialAdded(self, materialModel):
        for listener in self.materialChangeListeners:
            listener.onMaterialAdded(materialModel)

    def notifyMaterialRemoved(self, materialModel):
        for listener in self.materialChangeListeners:
            listener.onMaterialRemoved(materialModel)

class MaterialsChangeListener:
    def onMaterialAdded(self, materialModel):
        pass
    def onMaterialRemoved(self, materialModel):
        pass

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


from .material.MaterialModel import MaterialModel 
from .material.MaterialView import MaterialView

class Materials(QGroupBox):
    def __init__(self):
        super(Materials, self).__init__('Materials')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)

        self.materialModels = []

        self.materialGroupLabel = QLabel("Material Group")
        self.materialGroupLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(self.materialGroupLabel)

        self.materialGroup = QComboBox()
        self.materialGroup.setFixedWidth(50)
        self.materialGroup.currentIndexChanged.connect(self.materialGroupChanged)
        self.layout.addWidget(self.materialGroup)

        self.currentMaterialGroup = None

        self.setLayout(self.layout)

    def getMaterials(self):
        return self.materialModels

    def pushMaterialGroup(self):
        group = str(len(self.materialModels) + 1)
        materialModel = MaterialModel(group)
        self.materialModels.append(materialModel)
        self.materialGroup.addItem(group)
        
    def popMaterialGroup(self):
        self.materialModels.pop()
        self.materialGroup.removeItem(self.materialGroup.count() - 1)

    def materialGroupChanged(self, newGroupIndex):
        if self.currentMaterialGroup != None:
            self.layout.removeWidget(self.currentMaterialGroup)
            self.currentMaterialGroup.deleteLater()
            self.currentMaterialGroup = None
        
        groupIndex = self.materialGroup.currentIndex()

        model = self.materialModels[groupIndex]
        self.currentMaterialGroup = MaterialView(model)
        self.layout.addWidget(self.currentMaterialGroup)

    def modifyMaterialGroupCount(self, count):
        if count > len(self.materialModels):
            while count > len(self.materialModels):
                self.pushMaterialGroup()
        else:
            while len(self.materialModels) > count:
                self.popMaterialGroup()

    
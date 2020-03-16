from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .multipliers.MultipliersModel import MultipliersModel
from .multipliers.MultipliersView import MultipliersView

class MultipliersController(QGroupBox):
    def __init__(self):
        super(MultipliersController, self).__init__('Multipliers')
        self.layout = QGridLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.layout.setHorizontalSpacing(30)
        self.setLayout(self.layout)

        self.model = MultipliersModel()
        self.view = MultipliersView(self.model, self.layout)

    def updateView(self, model):
        self.view.setInputs(model)

    def getElements(self):
        return self.model

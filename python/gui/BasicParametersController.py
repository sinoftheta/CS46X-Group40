from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .parameters.ParametersView import ParametersView
from .parameters.ParametersModel import ParametersModel

class BasicParametersController(QGroupBox):
    def __init__(self):
        super(BasicParametersController, self).__init__('Basic Parameters')
        self.parametersModel = ParametersModel()
        self.layout = ParametersView(self.parametersModel)

        self.setLayout(self.layout)

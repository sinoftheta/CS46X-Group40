from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

class ElementView(QGroupBox):
    def __init__(self, elementModel):
        super(ElementView, self).__init__('Element ' + str(elementModel.elementNumber))
        self.viewModel = elementModel
        self.layout = QGridLayout()

        self.setLayout(self.layout)

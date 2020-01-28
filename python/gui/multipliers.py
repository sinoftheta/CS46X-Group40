from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


class Multipliers(QGroupBox):
    def __init__(self, *args, **kwargs):
        super(Multipliers, self).__init__('Multipliers')
        self.layout = QGridLayout()
        self.setLayout(self.layout)

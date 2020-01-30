from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

materialsTableLabels = []

class Materials(QGroupBox):
    def __init__(self):
        super(Materials, self).__init__('Materials')
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.setLayout(self.layout)

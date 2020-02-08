from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


class FileOptions(QGroupBox):
    def __init__(self):
        super(FileOptions, self).__init__('File Options')


        with QVBoxLayout as layout:
            layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
            self.setLayout(layout)
        
        
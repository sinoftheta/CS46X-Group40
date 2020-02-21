from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .seepage_face.SeepageFaceModel import SeepageFaceModel
from .seepage_face.SeepageFaceView import SeepageFaceView

class SeepageFaceController(QGroupBox):
    def __init__(self):
        super(SeepageFaceController, self).__init__('Seepage Faces')

        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)

        self.seepageFaces = []

        self.seepageFaceLabel = QLabel('Seepage Face')
        self.seepageFaceLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(self.seepageFaceLabel)

        self.seepageFaceGroup = QComboBox()
        self.seepageFaceGroup.setFixedWidth(50)
        self.seepageFaceGroup.currentIndexChanged.connect(self.seepageFaceGroupChanged)

        self.currentSeepageFace = None

        self.setLayout(self.layout)


    def getSeepageFaces(self):
        return self.seepageFaces

    def seepageFaceGroupChanged(self):
        if self.currentSeepageFace != None:
            self.layout.removeWidget(self.currentSeepageFace)
            self.currentSeepageFace.deleteLater()
            self.currentSeepageFace = None

        seepageFaceIndex = self.seepageFaceGroup.currentIndex()

        model = self.seepageFaceModels[seepageFaceIndex]
        self.currentSeepageFace = SeepageFaceView(model)
        self.layout.addWidget(self.currentSeepageFace)

    
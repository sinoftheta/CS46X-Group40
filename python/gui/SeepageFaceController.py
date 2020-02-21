from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .seepage_face.SeepageFaceModel import SeepageFaceModel
from .seepage_face.SeepageFaceView import SeepageFaceView



# I foresee the nodes page having a class NodeTableListener 
# with a method onNodeAdded(node)
# this class should inheirt from NodeTableListener
# and override onNodeAdded(node)
#
class SeepageFaceController(QGroupBox):
    def __init__(self):
        super(SeepageFaceController, self).__init__('Seepage Faces')

        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)

        self.seepageFaces = []

        self.seepageFaceLabel = QLabel('Seepage Face ID')
        self.seepageFaceLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(self.seepageFaceLabel)

        self.seepageFaceGroup = QComboBox()
        self.seepageFaceGroup.setFixedWidth(50)
        self.seepageFaceGroup.currentIndexChanged.connect(self.seepageFaceGroupChanged)
        self.layout.addWidget(self.seepageFaceGroup)

        self.currentSeepageFace = None

        self.setLayout(self.layout)


    def getSeepageFaces(self):
        return self.seepageFaces

    def popSeepageFace(self):
        lastSeepageFace = self.seepageFaces.pop()
        self.seepageFaceGroup.removeItem(self.seepageFacesGroup.count() - 1)


    def pushSeepageFace(self):
        group = str(len(self.seepageFaces) + 1)
        seepageFaceModel = SeepageFaceModel(group)

        self.seepageFaces.append(seepageFaceModel)
        self.seepageFaceGroup.addItem(group)

    def modifySeepageFaceCount(self, count):
        if count > len(self.seepageFaces):
            while count > len(self.seepageFaces):
                self.pushSeepageFace()
        else:
            while len(self.seepageFaces) > count:
                self.popSeepageFace()

    def seepageFaceGroupChanged(self):
        if self.currentSeepageFace != None:
            self.layout.removeWidget(self.currentSeepageFace)
            self.currentSeepageFace.deleteLater()
            self.currentSeepageFace = None

        seepageFaceIndex = self.seepageFaceGroup.currentIndex()

        model = self.seepageFaces[seepageFaceIndex]
        self.currentSeepageFace = SeepageFaceView(model)
        self.layout.addWidget(self.currentSeepageFace)

    
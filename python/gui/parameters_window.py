from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

import csv

from os import path

from .BasicParametersController import BasicParametersController
from .BasicParametersController import BasicParameterChangeListener
from .MultipliersController import MultipliersController
from .nodes import Nodes
from .nodeTypes import NodeTypes
from .materialProperties import Elements
from .elementIncidences import ElementIncidences
from .MaterialsController import MaterialsController
from .MaterialsController import MaterialsChangeListener
from .ElementsController import ElementsController

from .SeepageFaceController import SeepageFaceController

class ParametersPage(QGroupBox):
    def __init__(self, config):
        # Call QGroupBox constructor
        super(ParametersPage, self).__init__('Parameters')

        # reference to config file
        self.config = config

        # objects that care when the export button is clicked
        self.IOListeners = []

        # Create layout class and apply to Parameters section
        self.parametersPageLayout = QHBoxLayout()
        self.setLayout(self.parametersPageLayout)

        # Create layout classes for navigation buttons and
        #       parameters sub-sections
        self.parametersPageNav = QVBoxLayout()
        self.parametersPageNav.setAlignment(Qt.AlignCenter | Qt.AlignTop)
        self.parametersPageStack = QStackedLayout()

        # Attach navigation buttons layout and layout for contents
        self.parametersPageLayout.addLayout(self.parametersPageNav)
        self.parametersPageLayout.addLayout(self.parametersPageStack)

        # Creates Classes for each sub-section of the Parameters section
        self.parametersPageHome = QWidget()
        self.basicParametersController = BasicParametersController()
        self.multipliersController = MultipliersController()
        self.parametersPageNodes = Nodes()
        self.parametersPageNodeTypes = NodeTypes()
        self.parametersPageElem = Elements()
        self.elementsController = ElementsController()
        self.materialsController = MaterialsController()
        self.seepageFaceController = SeepageFaceController()

        # set up controller listeners
        self.basicParametersController.addBasicParameterListener(self.seepageFaceController)
        self.basicParametersController.addBasicParameterListener(self.materialsController)
        self.basicParametersController.addBasicParameterListener(self.elementsController)

        # Adds each class to stack layout
        self.parametersPageStack.addWidget(self.parametersPageHome)
        self.parametersPageStack.addWidget(self.basicParametersController)
        self.parametersPageStack.addWidget(self.multipliersController)
        self.parametersPageStack.addWidget(self.parametersPageNodes)
        self.parametersPageStack.addWidget(self.parametersPageNodeTypes)
        self.parametersPageStack.addWidget(self.parametersPageElem)
        self.parametersPageStack.addWidget(self.elementsController)
        self.parametersPageStack.addWidget(self.materialsController)
        self.parametersPageStack.addWidget(self.seepageFaceController)


        #   Add navigation buttons (widgets) to
        #       button container 'self.parametersPageStack'
        self.importNavBtn = QPushButton("Import")
        self.importNavBtn.setGeometry(0, 0, 150, 100)
        self.importNavBtn.clicked.connect(self.importNavClick)
        self.parametersPageNav.addWidget(self.importNavBtn)

        self.exportNavBtn = QPushButton("Export")
        self.exportNavBtn.setGeometry(0, 0, 150, 100)
        self.exportNavBtn.clicked.connect(self.notifyExport)
        self.parametersPageNav.addWidget(self.exportNavBtn)

        self.basicPNavBtn = QPushButton("Basic Parameters")
        self.basicPNavBtn.setGeometry(0, 0, 150, 100)
        self.basicPNavBtn.pressed.connect(self.basicParamClick)
        self.parametersPageNav.addWidget(self.basicPNavBtn)

        self.multipliersNavBtn = QPushButton("Multipliers")
        self.multipliersNavBtn.setGeometry(0, 0, 150, 100)
        self.multipliersNavBtn.pressed.connect(self.multipliersClick)
        self.parametersPageNav.addWidget(self.multipliersNavBtn)

        self.nodesNavBtn = QPushButton("Nodes")
        self.nodesNavBtn.setGeometry(0, 0, 150, 100)
        self.nodesNavBtn.pressed.connect(self.nodesClick)
        self.parametersPageNav.addWidget(self.nodesNavBtn)

        self.nodeTypesBtn = QPushButton("Node Types")
        self.nodeTypesBtn.setGeometry(0, 0, 150, 100)
        self.nodeTypesBtn.pressed.connect(self.nodeTypesClick)
        self.parametersPageNav.addWidget(self.nodeTypesBtn)

        self.elemNavBtn = QPushButton("Material Properties")
        self.elemNavBtn.setGeometry(0, 0, 150, 100)
        self.elemNavBtn.pressed.connect(self.materialPropClick)
        self.parametersPageNav.addWidget(self.elemNavBtn)

        self.elemIncNavBtn = QPushButton("Elements")
        self.elemIncNavBtn.setGeometry(0, 0, 150, 100)
        self.elemIncNavBtn.pressed.connect(self.elementsClick)
        self.parametersPageNav.addWidget(self.elemIncNavBtn)

        self.matsNavBtn = QPushButton("Materials")
        self.matsNavBtn.setGeometry(0, 0, 150, 100)
        self.matsNavBtn.pressed.connect(self.materialsClick)
        self.parametersPageNav.addWidget(self.matsNavBtn)

        seepageFaceNavBtn = QPushButton("Seepage Faces")
        seepageFaceNavBtn.setGeometry(0, 0, 150, 150)
        seepageFaceNavBtn.pressed.connect(self.seepageFaceClick)
        self.parametersPageNav.addWidget(seepageFaceNavBtn)

        self.parametersPageNav.setContentsMargins(0, 0, 0, 0)
        self.parametersPageNav.setSpacing(20)

        # open on the basic parameters page
        self.parametersPageStack.setCurrentIndex(1)



    def importNavClick(self):
        filename = QFileDialog.getOpenFileName(self, 'Open file',
            '/home', "CSV Files (*.csv)")
        if (len(filename[0]) > 0):
            self.notifyImport(filename[0])

    def basicParamClick(self):
        self.parametersPageStack.setCurrentIndex(1)

    def multipliersClick(self):
        self.parametersPageStack.setCurrentIndex(2)

    def nodesClick(self):
        numNodes = self.basicParametersController.parametersModel.NN.getData()
        self.parametersPageNodes.buildTable(numNodes)
        self.parametersPageStack.setCurrentIndex(3)

    def nodeTypesClick(self):
        nodeTypes = self.parametersPageNodes.nodeTypeCounts()
        self.parametersPageNodeTypes.setNodeTypes(nodeTypes)
        self.parametersPageStack.setCurrentIndex(4)

    def materialPropClick(self):
        numElements = self.basicParametersController.parametersModel.NE.getData()
        numMaterials = self.basicParametersController.parametersModel.NK.getData()
        self.parametersPageElem.buildTable(numElements, numMaterials)
        self.parametersPageStack.setCurrentIndex(5)

    def elementsClick(self):
        self.parametersPageStack.setCurrentIndex(6)

    def materialsClick(self):
        self.parametersPageStack.setCurrentIndex(7)

    def seepageFaceClick(self):
        self.parametersPageStack.setCurrentIndex(8)
        # TODO: When appropriate have the seepageFaceController listen for a change
        # in the number of seepage faces.

    def notifyExport(self):
        for listener in self.IOListeners:
            listener.onExport()

    def notifyImport(self, filename):
        for listener in self.IOListeners:
            listener.onImport(filename)

    def addIOListener(self, listener):
        if listener not in self.IOListeners:
            self.IOListeners.append(listener)

    def removeIOListener(self, listener):
        self.IOListeners.remove(listener)


class IOListener:
    def onExport(self):
        pass

    def onImport(self, filepath):
        pass

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
from .ElementsController import ElementsController
from .ElementPropertiesController import ElementPropertiesController
from .MaterialsController import MaterialsController
from .MaterialsController import MaterialsChangeListener
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
        self.elementsController = ElementsController()
        self.elementPropertiesController = ElementPropertiesController()
        self.materialsController = MaterialsController()
        self.seepageFaceController = SeepageFaceController()

        # set up controller listeners
        self.basicParametersController.addBasicParameterListener(self.seepageFaceController)
        self.basicParametersController.addBasicParameterListener(self.materialsController)
        self.basicParametersController.addBasicParameterListener(self.elementsController)
        self.basicParametersController.addBasicParameterListener(self.elementPropertiesController)

        # Adds each class to stack layout
        self.parametersPageStack.addWidget(self.parametersPageHome)
        self.parametersPageStack.addWidget(self.basicParametersController)
        self.parametersPageStack.addWidget(self.multipliersController)
        self.parametersPageStack.addWidget(self.parametersPageNodes)
        self.parametersPageStack.addWidget(self.parametersPageNodeTypes)
        self.parametersPageStack.addWidget(self.elementsController)
        self.parametersPageStack.addWidget(self.elementPropertiesController)
        self.parametersPageStack.addWidget(self.materialsController)
        self.parametersPageStack.addWidget(self.seepageFaceController)

        # Create navigation buttons
        importNavBtn = QPushButton("Import")
        importNavBtn.setGeometry(0, 0, 150, 100)
        importNavBtn.clicked.connect(self.importNavClick)

        exportNavBtn = QPushButton("Export")
        exportNavBtn.setGeometry(0, 0, 150, 100)
        exportNavBtn.clicked.connect(self.notifyExport)

        basicPNavBtn = QPushButton("Basic Parameters")
        basicPNavBtn.setGeometry(0, 0, 150, 100)
        basicPNavBtn.pressed.connect(self.basicParamClick)

        multipliersNavBtn = QPushButton("Multipliers")
        multipliersNavBtn.setGeometry(0, 0, 150, 100)
        multipliersNavBtn.pressed.connect(self.multipliersClick)

        nodesNavBtn = QPushButton("Nodes")
        nodesNavBtn.setGeometry(0, 0, 150, 100)
        nodesNavBtn.pressed.connect(self.nodesClick)

        nodeTypesBtn = QPushButton("Node Types")
        nodeTypesBtn.setGeometry(0, 0, 150, 100)
        nodeTypesBtn.pressed.connect(self.nodeTypesClick)

        elemNavBtn = QPushButton("Elements")
        elemNavBtn.setGeometry(0, 0, 150, 100)
        elemNavBtn.pressed.connect(self.elementsClick)

        elemPropsNavBtn = QPushButton("Element Properties")
        elemPropsNavBtn.setGeometry(0, 0, 150, 100)
        elemPropsNavBtn.pressed.connect(self.elementPropClick)

        matDataPointsNavBtn = QPushButton("Material Data Points")
        matDataPointsNavBtn.setGeometry(0, 0, 150, 100)
        matDataPointsNavBtn.pressed.connect(self.materialsClick)

        seepageFaceNavBtn = QPushButton("Seepage Faces")
        seepageFaceNavBtn.setGeometry(0, 0, 150, 150)
        seepageFaceNavBtn.pressed.connect(self.seepageFaceClick)

        #   Add navigation buttons (widgets) to
        #       button container 'self.parametersPageStack'
        self.parametersPageNav.addWidget(importNavBtn)
        self.parametersPageNav.addWidget(exportNavBtn)
        self.parametersPageNav.addWidget(basicPNavBtn)
        self.parametersPageNav.addWidget(multipliersNavBtn)
        self.parametersPageNav.addWidget(nodesNavBtn)
        self.parametersPageNav.addWidget(nodeTypesBtn)
        self.parametersPageNav.addWidget(elemNavBtn)
        self.parametersPageNav.addWidget(elemPropsNavBtn)
        self.parametersPageNav.addWidget(matDataPointsNavBtn)
        self.parametersPageNav.addWidget(seepageFaceNavBtn)

        self.parametersPageNav.setContentsMargins(0, 0, 0, 0)
        self.parametersPageNav.setSpacing(20)

        # open on the basic parameters page
        self.parametersPageStack.setCurrentIndex(1)



    def importNavClick(self):
        filename = QFileDialog.getOpenFileName(self, 'Open file',
            '/home', "CSV Files (*.csv)")

        if filename[0] != '':
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

    def elementsClick(self):
        self.parametersPageStack.setCurrentIndex(5)

    def elementPropClick(self):
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

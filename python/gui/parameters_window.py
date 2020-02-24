from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

import csv
import gs2

from os import path

from .BasicParametersController import BasicParametersController
from .BasicParametersController import BasicParameterChangeListener
from .multipliers import Multipliers
from .nodes import Nodes
from .nodeTypes import NodeTypes
from .elements import Elements
from .elementIncidences import ElementIncidences
from .MaterialsController import MaterialsController
from .MaterialsController import MaterialsChangeListener

from .SeepageFaceController import SeepageFaceController

parameters = [
        'Import',
        'Basic Parameters',
        'Multipliers',
        'Nodes',
        'Node Types',
        'Elements',
        'Element Incidences',
        'Materials' ]

class ParametersPage(QGroupBox):
    def __init__(self, config):
        # Call QGroupBox constructor
        super(ParametersPage, self).__init__('Parameters')

        # reference to config file
        self.config = config

        # objects that care when the export button is clicked
        self.exportListeners = []

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
        self.parametersPageBasic = BasicParametersController()
        self.parametersPageMult = Multipliers()
        self.parametersPageNodes = Nodes()
        self.parametersPageNodeTypes = NodeTypes()
        self.parametersPageElem = Elements()
        self.parametersPageElemIncid = ElementIncidences()
        self.materialsController = MaterialsController()
        self.seepageFaceController = SeepageFaceController()

        # set up controller listeners
        self.parametersPageBasic.addBasicParameterListener(self.seepageFaceController)
        self.parametersPageBasic.addBasicParameterListener(self.materialsController)


        # Adds each class to stack layout
        self.parametersPageStack.addWidget(self.parametersPageHome)
        self.parametersPageStack.addWidget(self.parametersPageBasic)
        self.parametersPageStack.addWidget(self.parametersPageMult)
        self.parametersPageStack.addWidget(self.parametersPageNodes)
        self.parametersPageStack.addWidget(self.parametersPageNodeTypes)
        self.parametersPageStack.addWidget(self.parametersPageElem)
        self.parametersPageStack.addWidget(self.parametersPageElemIncid)
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

        self.elemNavBtn = QPushButton("Elements")
        self.elemNavBtn.setGeometry(0, 0, 150, 100)
        self.elemNavBtn.pressed.connect(self.elementsClick)
        self.parametersPageNav.addWidget(self.elemNavBtn)

        self.elemIncNavBtn = QPushButton("Element Incidences")
        self.elemIncNavBtn.setGeometry(0, 0, 150, 100)
        self.elemIncNavBtn.pressed.connect(self.elementIncClick)
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
        #self.parametersPageStack.setCurrentIndex(0)
        filename = QFileDialog.getOpenFileName(self, 'Open file',

            '/home', "CSV Files (*.csv);;Text files (*.txt)")


    def basicParamClick(self):
        self.parametersPageStack.setCurrentIndex(1)

    def multipliersClick(self):
        self.parametersPageStack.setCurrentIndex(2)

    def nodesClick(self):
        numNodes = self.parametersPageBasic.parametersModel.NN.getData()
        self.parametersPageNodes.buildTable(numNodes)
        self.parametersPageStack.setCurrentIndex(3)
        #Test accessor
        #print(self.parametersPageNodes.getCONCI())

    def nodeTypesClick(self):
        nodeTypes = self.parametersPageNodes.nodeTypeCounts()
        self.parametersPageNodeTypes.setNodeTypes(nodeTypes)
        self.parametersPageStack.setCurrentIndex(4)

    def elementsClick(self):
        numElements = self.parametersPageBasic.parametersModel.NE.getData()
        numMaterials = self.parametersPageBasic.parametersModel.NK.getData()
        self.parametersPageElem.buildTable(numElements, numMaterials)
        self.parametersPageStack.setCurrentIndex(5)

    def elementIncClick(self):
        numElements = self.parametersPageBasic.parametersModel.NE
        # maxNodes = self.parametersPageBasic.getMaxNodesPerElem()
        self.parametersPageStack.setCurrentIndex(6)
        self.parametersPageElemIncid.buildTable(numElements, 12)

    def materialsClick(self):
        self.parametersPageStack.setCurrentIndex(7)

    def seepageFaceClick(self):
        self.parametersPageStack.setCurrentIndex(8)
        # TODO: When appropriate have the seepageFaceController listen for a change
        # in the number of seepage faces.

    def notifyExport(self):
        for listener in self.exportListeners:
            listener.onExport()

    def addExportListener(self, listener):
        if listener not in self.exportListeners:
            self.exportListeners.append(listener)

    def removeExportListener(self, listener):
        self.exportListeners.remove(listener)

    # Handling  file exporting is being moved.
    # this is being done as this class should not know about the simulation page
    # so in the export button will notify those who care that the button has been pressed.
    # def exportNavClick(self):
    #     fileWriter = gs2.FileWriter(
    #         self.materialsController.getMaterials(),
    #         self.
    #     )

    #     filePath = path.join(self.config['paths']['bundle'], self.config['paths']['data-out'])
    #     fileWriter.write(filePath)



class ExportListener:
    def onExport(self):
        pass

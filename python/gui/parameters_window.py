from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .parameters_basic import BasicParameters
from .multipliers import Multipliers
from .nodes import Nodes
from .nodeTypes import NodeTypes
from .elements import Elements
from .elementIncidences import ElementIncidences
from .materials import Materials

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
    def __init__(self):
        # Call QGroupBox constructor
        super(ParametersPage, self).__init__('Parameters')

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
        self.parametersPageBasic = BasicParameters()
        self.parametersPageMult = Multipliers()
        self.parametersPageNodes = Nodes()
        self.parametersPageNodeTypes = NodeTypes()
        self.parametersPageElem = Elements()
        self.parametersPageElemIncid = ElementIncidences()
        self.parametersPageMat = Materials()

        # Adds each class to stack layout
        self.parametersPageStack.addWidget(self.parametersPageHome)
        self.parametersPageStack.addWidget(self.parametersPageBasic)
        self.parametersPageStack.addWidget(self.parametersPageMult)
        self.parametersPageStack.addWidget(self.parametersPageNodes)
        self.parametersPageStack.addWidget(self.parametersPageNodeTypes)
        self.parametersPageStack.addWidget(self.parametersPageElem)
        self.parametersPageStack.addWidget(self.parametersPageElemIncid)
        self.parametersPageStack.addWidget(self.parametersPageMat)

        #   Add navigation buttons (widgets) to
        #       button container 'self.parametersPageStack'
        importNavBtn = QPushButton("Import")
        importNavBtn.setGeometry(0, 0, 150, 100)
        importNavBtn.clicked.connect(self.importNavClick)
        self.parametersPageNav.addWidget(importNavBtn)

        basicPNavBtn = QPushButton("Basic Parameters")
        basicPNavBtn.setGeometry(0, 0, 150, 100)
        basicPNavBtn.pressed.connect(self.basicParamClick)
        self.parametersPageNav.addWidget(basicPNavBtn)

        multipliersNavBtn = QPushButton("Multipliers")
        multipliersNavBtn.setGeometry(0, 0, 150, 100)
        multipliersNavBtn.pressed.connect(self.multipliersClick)
        self.parametersPageNav.addWidget(multipliersNavBtn)

        nodesNavBtn = QPushButton("Nodes")
        nodesNavBtn.setGeometry(0, 0, 150, 100)
        nodesNavBtn.pressed.connect(self.nodesClick)
        self.parametersPageNav.addWidget(nodesNavBtn)

        nodeTypesClick = QPushButton("Node Types")
        nodeTypesClick.setGeometry(0, 0, 150, 100)
        nodeTypesClick.pressed.connect(self.nodeTypesClick)
        self.parametersPageNav.addWidget(nodeTypesClick)

        elemNavBtn = QPushButton("Elements")
        elemNavBtn.setGeometry(0, 0, 150, 100)
        elemNavBtn.pressed.connect(self.elementsClick)
        self.parametersPageNav.addWidget(elemNavBtn)

        elemIncNavBtn = QPushButton("Element Incidences")
        elemIncNavBtn.setGeometry(0, 0, 150, 100)
        elemIncNavBtn.pressed.connect(self.elementIncClick)
        self.parametersPageNav.addWidget(elemIncNavBtn)

        matsNavBtn = QPushButton("Materials")
        matsNavBtn.setGeometry(0, 0, 150, 100)
        matsNavBtn.pressed.connect(self.materialsClick)
        self.parametersPageNav.addWidget(matsNavBtn)

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
        numNodes = self.parametersPageBasic.getNumNodes()
        self.parametersPageNodes.buildTable(numNodes)
        self.parametersPageStack.setCurrentIndex(3)

    def nodeTypesClick(self):
        nodeTypes = self.parametersPageNodes.nodeTypeCounts()
        self.parametersPageNodeTypes.setNodeTypes(nodeTypes)
        self.parametersPageStack.setCurrentIndex(4)

    def elementsClick(self):
        numElements = self.parametersPageBasic.getNumElements()
        numMaterials = self.parametersPageBasic.getNumMaterials()
        self.parametersPageElem.buildTable(numElements, numMaterials)
        self.parametersPageStack.setCurrentIndex(5)

    def elementIncClick(self):
        numElements = self.parametersPageBasic.getNumElements()
        maxNodes = self.parametersPageBasic.getMaxNodesPerElem()
        self.parametersPageStack.setCurrentIndex(6)
        self.parametersPageElemIncid.buildTable(numElements, maxNodes)

    def materialsClick(self):
        self.parametersPageStack.setCurrentIndex(7)
        self.parametersPageMat.MaterialsLayout(self.parametersPageBasic.getNumMaterials())

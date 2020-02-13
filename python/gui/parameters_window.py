from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *
import csv

from .parameters_basic import BasicParameters
from .multipliers import Multipliers
from .nodes import Nodes
from .nodeTypes import NodeTypes
from .elements import Elements
from .elementIncidences import ElementIncidences
from .materials import Materials

parameters = ['Import', 'Basic Parameters', 'Multipliers',
            'Nodes', 'Node Types', 'Elements', 'Materials']

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
        self.parametersPageNodesT = NodeTypes()
        self.parametersPageElem = Elements()
        self.parametersPageElemIncid = ElementIncidences()
        self.parametersPageMat = Materials()

        # Adds each class to stack layout
        self.parametersPageStack.addWidget(self.parametersPageHome)
        self.parametersPageStack.addWidget(self.parametersPageBasic)
        self.parametersPageStack.addWidget(self.parametersPageMult)
        self.parametersPageStack.addWidget(self.parametersPageNodes)
        self.parametersPageStack.addWidget(self.parametersPageNodesT)
        self.parametersPageStack.addWidget(self.parametersPageElem)
        self.parametersPageStack.addWidget(self.parametersPageElemIncid)
        self.parametersPageStack.addWidget(self.parametersPageMat)

        #   Add navigation buttons (widgets) to
        #       button container 'self.parametersPageStack'
        self.importNavBtn = QPushButton("Import")
        self.importNavBtn.setGeometry(0, 0, 150, 100)
        self.importNavBtn.clicked.connect(self.importNavClick)
        self.parametersPageNav.addWidget(self.importNavBtn)

        self.exportNavBtn = QPushButton("Export")
        self.exportNavBtn.setGeometry(0, 0, 150, 100)
        self.exportNavBtn.clicked.connect(self.exportNavClick)
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

        self.parametersPageNav.setContentsMargins(0, 0, 0, 0)
        self.parametersPageNav.setSpacing(20)

        if (self.parametersPageMat.MaterialsLayout(self.parametersPageBasic.getNumMaterials()) == 0):
            self.matsNavBtn.setDisabled(true)

    def importNavClick(self):
        #self.parametersPageStack.setCurrentIndex(0)
        filename = QFileDialog.getOpenFileName(self, 'Open file',
            '/home', "Text files (*.txt);;CSV Files (*.csv)")

    def basicParamClick(self):
        self.parametersPageStack.setCurrentIndex(1)

    def multipliersClick(self):
        self.parametersPageStack.setCurrentIndex(2)

    def nodesClick(self):
        numNodes = self.parametersPageBasic.getNumNodes()
        self.parametersPageNodes.buildTable(numNodes)
        self.parametersPageStack.setCurrentIndex(3)
        #Test accessor
        #print(self.parametersPageNodes.getCONCI())

    def nodeTypesClick(self):
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


    def exportNavClick(self):

        #check that node table has been opened, should probably grey out export button
        if(not hasattr(self.parametersPageNodes, 'nodeTable')):
            print('You have not set node properties yet')
            return
        print('exporting')
        with open('parameters.csv', 'w', newline='') as csvfile:
            writer = csv.writer(
                csvfile, 
                delimiter=',',
                quotechar='|', #unused, I think
                quoting=csv.QUOTE_MINIMAL) #also unused
            
            # TODO: values that are hard-coded must be derived 
            
            #write group A, problem title
            group = 'A'

            #write group B, basic parameters
            group = 'B'
            vals = self.parametersPageBasic.getVals()
            numNodes = vals['NN']

            writer.writerow([group, vals['NN'], vals['NE'], 'NS', 'KNS', vals['NB'], vals['KNB'], 'NF', vals['INC'], vals['NK'], 'NSEEP']) # page 3
            writer.writerow([group, 'NSDN', 'MQ4', 'KNSDN', vals['PL'], 'COEFI', vals['EI'], 'NVS', '', '', '']) # page 4
            writer.writerow([group, vals['DELT'], vals['CHNG'], vals['ITMAX'], vals['ITCHNG'], vals['PCHNG'], vals['BETAP'], 'TYPE', '', '', '']) # page 5, TODO: implement TYPE
            writer.writerow([group, vals['DIFUSN'], 'DPRDT', vals['STAT'], vals['STATP'], vals['CLOS1'], vals['ITER1'], vals['IGO']]) # page 6
            

            #write group C, multipliers
            group = 'C'
            vals = self.parametersPageMult.getVals()
            writer.writerow([group, vals['AFMOBX'], vals['AFMOBY'], vals['APOR'], vals['AELONG'], vals['AETRANS'], vals['APHII'], vals['ACONCI'], vals['XFACT'], '', '']) # page 8 
            writer.writerow([group, vals['YFACT'], vals['ATETA'], vals['AAL'], vals['AKD'], vals['ALAM'], vals['ARHO'], '', '', '', '']) # page 8 
            #write group D, output control
            
            #write group E, node coordinates
            group = 'E'
            for i in range(numNodes):
                print(self.parametersPageNodes.getRow(i))
            
            #write group F
            
            #write group G
            
            #write group H
            
            #write group I
            
            #write group J
            
            #write group K
            
            #write group L
            
            #write group M
            
            #write group N
            
            #write group O
            
            #write group P
            
            #write group Q

        
        
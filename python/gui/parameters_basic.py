from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


class BasicParameters(QGroupBox):
    def __init__(self):
        super(BasicParameters, self).__init__('Basic Parameters')
        self.layout = QGridLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.layout.setHorizontalSpacing(30)
        self.setLayout(self.layout)
        self.setInputs()

    def setInputs(self):
        nodesLabel = QLabel("Number of Nodes")
        nodesLabel.setAlignment(Qt.AlignLeft)
        self.NN = QSpinBox()
        self.NN.setAlignment(Qt.AlignRight)
        self.layout.addWidget(nodesLabel, 0, 0)
        self.layout.addWidget(self.NN, 1, 0)

        elementsLabel = QLabel("Number of Elements")
        elementsLabel.setAlignment(Qt.AlignBottom | Qt.AlignLeft)
        self.NE = QSpinBox()
        self.NE.setAlignment(Qt.AlignRight)
        self.layout.addWidget(elementsLabel, 2, 0)
        self.layout.addWidget(self.NE, 3, 0)

        elementIncLabel = QLabel("Max Number Nodes \nPer Element")
        elementIncLabel.setAlignment(Qt.AlignLeft)
        self.INC = QSpinBox()
        self.INC.setAlignment(Qt.AlignRight)
        self.layout.addWidget(elementIncLabel, 2, 1)
        self.layout.addWidget(self.INC, 3, 1)

        NKLabel = QLabel("Number of Material Groups")
        NKLabel.setAlignment(Qt.AlignLeft)
        self.NK = QSpinBox()
        self.NK.setAlignment(Qt.AlignRight)
        self.layout.addWidget(NKLabel, 4, 0)
        self.layout.addWidget(self.NK, 5, 0)

        # I recognise that this class will be changed,
        # for the moment this is here for testing
        nseepLabel = QLabel("Number of Seepage Faces")
        nseepLabel.setAlignment(Qt.AlignLeft)
        self.nseepLineEdit = QLineEdit()
        self.nseepLineEdit.setAlignment(Qt.AlignRight)
        self.layout.addWidget(nseepLabel, 4, 1)
        self.layout.addWidget(self.nseepLineEdit, 5, 1)

        self.layout.setRowMinimumHeight(6, 30)

        NBLabel = QLabel("Half-Bandwidth (Flow)")
        NBLabel.setAlignment(Qt.AlignLeft)
        self.NB = QSpinBox()
        self.NB.setAlignment(Qt.AlignRight)
        self.layout.addWidget(NBLabel, 7, 0)
        self.layout.addWidget(self.NB, 8, 0)

        KNBLabel = QLabel("Half-Bandwidth (Mass Transport)")
        KNBLabel.setAlignment(Qt.AlignLeft)
        self.KNB = QSpinBox()
        self.KNB.setAlignment(Qt.AlignRight)
        self.layout.addWidget(KNBLabel, 7, 1)
        self.layout.addWidget(self.KNB, 8, 1)

        PLLabel = QLabel("Minimum Pressure Head")
        PLLabel.setAlignment(Qt.AlignLeft)
        self.PL = QDoubleSpinBox()
        self.PL.setDecimals(3)
        self.PL.setSingleStep(0.001)
        self.PL.setRange(-9999.999, 9999.999)
        self.PL.setAlignment(Qt.AlignRight)
        self.layout.addWidget(PLLabel, 7, 2)
        self.layout.addWidget(self.PL, 8, 2)

        EILabel = QLabel("Max Infiltration Rate")
        EILabel.setAlignment(Qt.AlignLeft)
        self.EI = QDoubleSpinBox()
        self.EI.setDecimals(3)
        self.EI.setSingleStep(0.001)
        self.EI.setRange(-9999.999, 9999.999)
        self.EI.setAlignment(Qt.AlignRight)
        self.layout.addWidget(EILabel, 7, 3)
        self.layout.addWidget(self.EI, 8, 3)

        PCHNGLabel = QLabel("Pressure change criterion")
        PCHNGLabel.setAlignment(Qt.AlignLeft)
        self.PCHNG = QDoubleSpinBox()
        self.PCHNG.setDecimals(3)
        self.PCHNG.setSingleStep(0.001)
        self.PCHNG.setRange(-9999.999, 9999.999)
        self.PCHNG.setAlignment(Qt.AlignRight)
        self.layout.addWidget(PCHNGLabel, 10, 0)
        self.layout.addWidget(self.PCHNG, 11, 0)

        BETAPLabel = QLabel("Modified coefficient of fluid compressibility")
        BETAPLabel.setAlignment(Qt.AlignLeft)
        self.BETAP = QDoubleSpinBox()
        self.BETAP.setDecimals(3)
        self.BETAP.setSingleStep(0.001)
        self.BETAP.setRange(-9999.999, 9999.999)
        self.BETAP.setAlignment(Qt.AlignRight)
        self.layout.addWidget(BETAPLabel, 10, 1)
        self.layout.addWidget(self.BETAP, 11, 1)

        DIFUSNLabel = QLabel("Molecular diffusion constant")
        DIFUSNLabel.setAlignment(Qt.AlignLeft)
        self.DIFUSN = QDoubleSpinBox()
        self.DIFUSN.setDecimals(3)
        self.DIFUSN.setSingleStep(0.001)
        self.DIFUSN.setRange(-9999.999, 9999.999)
        self.DIFUSN.setAlignment(Qt.AlignRight)
        self.layout.addWidget(DIFUSNLabel, 10, 2)
        self.layout.addWidget(self.DIFUSN, 11, 2)

        CLOS1Label = QLabel("Convergence criteria for iteration")
        CLOS1Label.setAlignment(Qt.AlignLeft)
        self.CLOS1 = QDoubleSpinBox()
        self.CLOS1.setDecimals(3)
        self.CLOS1.setSingleStep(0.001)
        self.CLOS1.setRange(-9999.999, 9999.999)
        self.CLOS1.setAlignment(Qt.AlignRight)
        self.layout.addWidget(CLOS1Label, 10, 3)
        self.layout.addWidget(self.CLOS1, 11, 3)

        self.layout.setRowMinimumHeight(12, 30)

        timeSLabel = QLabel("Initial Time Step (hours)")
        timeSLabel.setAlignment(Qt.AlignLeft)
        self.DELT = QDoubleSpinBox()
        self.DELT.setDecimals(3)
        self.DELT.setSingleStep(0.001)
        self.DELT.setAlignment(Qt.AlignRight)
        self.layout.addWidget(timeSLabel, 13, 0)
        self.layout.addWidget(self.DELT, 14, 0)

        timeSMLabel = QLabel("Time Step Multiplier")
        timeSMLabel.setAlignment(Qt.AlignLeft)
        self.CHNG = QDoubleSpinBox()
        self.CHNG.setDecimals(3)
        self.CHNG.setSingleStep(0.001)
        self.CHNG.setAlignment(Qt.AlignRight)
        self.layout.addWidget(timeSMLabel, 13, 1)
        self.layout.addWidget(self.CHNG, 14, 1)

        maxTimeSLabel = QLabel("Max Time Steps")
        maxTimeSLabel.setAlignment(Qt.AlignLeft)
        self.ITMAX = QSpinBox()
        self.ITMAX.setAlignment(Qt.AlignRight)
        self.layout.addWidget(maxTimeSLabel, 13, 2)
        self.layout.addWidget(self.ITMAX, 14, 2)

        iterTimeSLabel = QLabel("Max Iterations Per Time Step")
        iterTimeSLabel.setAlignment(Qt.AlignLeft)
        self.ITER1 = QSpinBox()
        self.ITER1.setAlignment(Qt.AlignRight)
        self.layout.addWidget(iterTimeSLabel, 13, 3)
        self.layout.addWidget(self.ITER1, 14, 3)

        iterTimeSLabel = QLabel("Time steps between changes in DELT")
        iterTimeSLabel.setAlignment(Qt.AlignLeft)
        self.ITCHNG = QSpinBox()
        self.ITCHNG.setAlignment(Qt.AlignRight)
        self.layout.addWidget(iterTimeSLabel, 13, 4)
        self.layout.addWidget(self.ITCHNG, 14, 4)

        self.layout.setRowMinimumHeight(15, 30)

        TYPELabel = QLabel("Finite difference scheme")
        TYPELabel.setAlignment(Qt.AlignLeft)
        self.BACK = QRadioButton("Implicit")
        self.BACK.setChecked(True)
        #self.BACK.toggled.connect(function)
        self.CENT = QRadioButton("Centered in Time")
        self.CENT.setChecked(False)
        #self.CENT.toggled.connect(function)
        self.layout.addWidget(TYPELabel, 16, 0)
        self.layout.addWidget(self.BACK, 17, 0)
        self.layout.addWidget(self.CENT, 17, 1)

        STATLabel = QLabel("Mass Transport Solutions")
        STATLabel.setAlignment(Qt.AlignLeft)
        self.STAT = QComboBox()
        self.STAT.addItem("Flow equation only")
        self.STAT.addItem("Steady-state")
        self.STAT.addItem("Transient")
        #self.STAT.currentIndexChanged.connect(function)
        self.layout.addWidget(STATLabel, 16, 2)
        self.layout.addWidget(self.STAT, 17, 2)

        STATPLabel = QLabel("Flow Solutions")
        STATPLabel.setAlignment(Qt.AlignLeft)
        self.STATP = QComboBox()
        self.STATP.addItem("Steady-state")
        self.STATP.addItem("Transient")
        #self.STATP.currentIndexChanged.connect(function)
        self.layout.addWidget(STATPLabel, 16, 3)
        self.layout.addWidget(self.STATP, 17, 3)

        IGOLabel = QLabel("Number of Mass Transport Solutions")
        IGOLabel.setAlignment(Qt.AlignLeft)
        self.IGO = QSpinBox()
        self.IGO.setAlignment(Qt.AlignRight)
        self.layout.addWidget(IGOLabel, 16, 4)
        self.layout.addWidget(self.IGO, 17, 4)

    def getNumNodes(self):
        return self.NN.value()

    def getNumElements(self):
        return self.NE.value()

    def getMaxNodesPerElem(self):
        return self.INC.value()

    def getNumMaterials(self):
        return self.NK.value()

    # again part of testing
    def getNumSeepageFaces(self):
        return int(self.nseepLineEdit.text() or 0)

    def getVals(self):
        values = {}
        for var in vars(self):
            if var == 'layout':
                continue
            # return as unordered array
            #values.append(
            #    getattr(self, var).value()
            #)

            # return as key value pairs
            if isinstance(getattr(self, var), QComboBox): #will deal with these cases later
                values[var] = getattr(self, var).currentText()
            elif isinstance(getattr(self, var), QRadioButton):
                continue
            else:
                values[var] = getattr(self, var).value()
            
        return values



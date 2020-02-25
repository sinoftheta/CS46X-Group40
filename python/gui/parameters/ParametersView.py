from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


SchemeTypes = [
    "Centered in Time",
    "Implicit"
]

TransportTypes = [
    "Flow equation only",
    "Steady-state",
    "Transient"
]

FlowTypes = [
    "Steady-state",
    "Transient"
]


class ParametersView(QGridLayout):
    def __init__(self, parametersModel):
        super(ParametersView, self).__init__()
        self.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.viewModel = parametersModel
        self.setHorizontalSpacing(30)
        self.setInputs()
        self.updateView()

    def setInputs(self):
        nodesLabel = QLabel("Number of Nodes")
        nodesLabel.setAlignment(Qt.AlignLeft)
        self.NN = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.NN.setAlignment(Qt.AlignRight)
        self.NN.setRange(0, 52)
        self.addWidget(nodesLabel, 0, 0)
        self.addWidget(self.NN, 1, 0)
        self.NN.valueChanged.connect(self.numNodesChange)

        elementsLabel = QLabel("Number of Elements")
        elementsLabel.setAlignment(Qt.AlignBottom | Qt.AlignLeft)
        self.NE = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.NE.setAlignment(Qt.AlignRight)
        self.NE.setRange(0, 25)
        self.addWidget(elementsLabel, 0, 1)
        self.addWidget(self.NE, 1, 1)
        self.NE.valueChanged.connect(self.numElementsChange)

        NKLabel = QLabel("Number of Material Groups")
        NKLabel.setAlignment(Qt.AlignLeft)
        self.NK = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.NK.setAlignment(Qt.AlignRight)
        self.addWidget(NKLabel, 2, 0)
        self.addWidget(self.NK, 3, 0)
        self.NK.valueChanged.connect(self.numMaterialsChange)

        NSEEPLabel = QLabel("Number of Seepage Faces")
        NSEEPLabel.setAlignment(Qt.AlignLeft)
        self.NSEEP = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.NSEEP.setAlignment(Qt.AlignRight)
        self.NSEEP.setRange(0, 100)
        self.addWidget(NSEEPLabel, 2, 1)
        self.addWidget(self.NSEEP, 3, 1)
        self.NSEEP.valueChanged.connect(self.numSeepageFaceChange)

        self.setRowMinimumHeight(6, 30)

        NBLabel = QLabel("Half-Bandwidth (Flow)")
        NBLabel.setAlignment(Qt.AlignLeft)
        self.NB = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.NB.setAlignment(Qt.AlignRight)
        self.addWidget(NBLabel, 7, 0)
        self.addWidget(self.NB, 8, 0)
        self.NB.valueChanged.connect(
            lambda: setattr(self.viewModel, 'NB', self.NB.value())
        )

        KNBLabel = QLabel("Half-Bandwidth (Mass Transport)")
        KNBLabel.setAlignment(Qt.AlignLeft)
        self.KNB = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.KNB.setAlignment(Qt.AlignRight)
        self.addWidget(KNBLabel, 7, 1)
        self.addWidget(self.KNB, 8, 1)
        self.KNB.valueChanged.connect(
            lambda: setattr(self.viewModel, 'KNB', self.KNB.value())
        )

        PLLabel = QLabel("Minimum Pressure Head")
        PLLabel.setAlignment(Qt.AlignLeft)
        self.PL = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
        self.PL.setDecimals(3)
        self.PL.setSingleStep(0.001)
        self.PL.setRange(-9999.999, 9999.999)
        self.PL.setAlignment(Qt.AlignRight)
        self.addWidget(PLLabel, 7, 2)
        self.addWidget(self.PL, 8, 2)
        self.PL.valueChanged.connect(
            lambda: setattr(self.viewModel, 'PL', self.PL.value())
        )

        EILabel = QLabel("Max Infiltration Rate")
        EILabel.setAlignment(Qt.AlignLeft)
        self.EI = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
        self.EI.setDecimals(3)
        self.EI.setSingleStep(0.001)
        self.EI.setRange(-9999.999, 9999.999)
        self.EI.setAlignment(Qt.AlignRight)
        self.addWidget(EILabel, 7, 3)
        self.addWidget(self.EI, 8, 3)
        self.EI.valueChanged.connect(
            lambda: setattr(self.viewModel, 'EI', self.EI.value())
        )

        PCHNGLabel = QLabel("Pressure change criterion")
        PCHNGLabel.setAlignment(Qt.AlignLeft)
        self.PCHNG = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
        self.PCHNG.setDecimals(3)
        self.PCHNG.setSingleStep(0.001)
        self.PCHNG.setRange(-9999.999, 9999.999)
        self.PCHNG.setAlignment(Qt.AlignRight)
        self.addWidget(PCHNGLabel, 10, 0)
        self.addWidget(self.PCHNG, 11, 0)
        self.PCHNG.valueChanged.connect(
            lambda: setattr(self.viewModel, 'PCHNG', self.PCHNG.value())
        )

        BETAPLabel = QLabel("Modified coefficient of fluid compressibility")
        BETAPLabel.setAlignment(Qt.AlignLeft)
        self.BETAP = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
        self.BETAP.setDecimals(3)
        self.BETAP.setSingleStep(0.001)
        self.BETAP.setRange(-9999.999, 9999.999)
        self.BETAP.setAlignment(Qt.AlignRight)
        self.addWidget(BETAPLabel, 10, 1)
        self.addWidget(self.BETAP, 11, 1)
        self.BETAP.valueChanged.connect(
            lambda: setattr(self.viewModel, 'BETAP', self.BETAP.value())
        )

        DIFUSNLabel = QLabel("Molecular diffusion constant")
        DIFUSNLabel.setAlignment(Qt.AlignLeft)
        self.DIFUSN = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
        self.DIFUSN.setDecimals(3)
        self.DIFUSN.setSingleStep(0.001)
        self.DIFUSN.setRange(-9999.999, 9999.999)
        self.DIFUSN.setAlignment(Qt.AlignRight)
        self.addWidget(DIFUSNLabel, 10, 2)
        self.addWidget(self.DIFUSN, 11, 2)
        self.DIFUSN.valueChanged.connect(
            lambda: setattr(self.viewModel, 'DIFUSN', self.DIFUSN.value())
        )

        CLOS1Label = QLabel("Convergence criteria for iteration")
        CLOS1Label.setAlignment(Qt.AlignLeft)
        self.CLOS1 = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
        self.CLOS1.setDecimals(3)
        self.CLOS1.setSingleStep(0.001)
        self.CLOS1.setRange(-9999.999, 9999.999)
        self.CLOS1.setAlignment(Qt.AlignRight)
        self.addWidget(CLOS1Label, 10, 3)
        self.addWidget(self.CLOS1, 11, 3)
        self.CLOS1.valueChanged.connect(
            lambda: setattr(self.viewModel, 'CLOS1', self.CLOS1.value())
        )

        self.setRowMinimumHeight(12, 30)

        timeSLabel = QLabel("Initial Time Step (hours)")
        timeSLabel.setAlignment(Qt.AlignLeft)
        self.DELT = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
        self.DELT.setDecimals(3)
        self.DELT.setSingleStep(0.001)
        self.DELT.setAlignment(Qt.AlignRight)
        self.addWidget(timeSLabel, 13, 0)
        self.addWidget(self.DELT, 14, 0)
        self.DELT.valueChanged.connect(
            lambda: setattr(self.viewModel, 'DELT', self.DELT.value())
        )

        timeSMLabel = QLabel("Time Step Multiplier")
        timeSMLabel.setAlignment(Qt.AlignLeft)
        self.CHNG = QDoubleSpinBox(buttonSymbols = QDoubleSpinBox.NoButtons)
        self.CHNG.setDecimals(3)
        self.CHNG.setSingleStep(0.001)
        self.CHNG.setRange(1.0, 2.0)
        self.CHNG.setAlignment(Qt.AlignRight)
        self.addWidget(timeSMLabel, 13, 1)
        self.addWidget(self.CHNG, 14, 1)
        self.CHNG.valueChanged.connect(
            lambda: setattr(self.viewModel, 'CHNG', self.CHNG.value())
        )

        maxTimeSLabel = QLabel("Max Time Steps")
        maxTimeSLabel.setAlignment(Qt.AlignLeft)
        self.ITMAX = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.ITMAX.setAlignment(Qt.AlignRight)
        self.ITMAX.setRange(1, 100)
        self.addWidget(maxTimeSLabel, 13, 2)
        self.addWidget(self.ITMAX, 14, 2)
        self.ITMAX.valueChanged.connect(
            lambda: setattr(self.viewModel, 'ITMAX', self.ITMAX.value())
        )

        iterTimeSLabel = QLabel("Max Iterations Per Time Step")
        iterTimeSLabel.setAlignment(Qt.AlignLeft)
        self.ITER1 = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.ITER1.setAlignment(Qt.AlignRight)
        self.ITER1.setRange(1, 100)
        self.addWidget(iterTimeSLabel, 13, 3)
        self.addWidget(self.ITER1, 14, 3)
        self.ITER1.valueChanged.connect(
            lambda: setattr(self.viewModel, 'ITER1', self.ITER1.value())
        )

        iterTimeSLabel = QLabel("Time steps between changes in DELT")
        iterTimeSLabel.setAlignment(Qt.AlignLeft)
        self.ITCHNG = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.ITCHNG.setAlignment(Qt.AlignRight)
        self.ITCHNG.setRange(1, 100)
        self.addWidget(iterTimeSLabel, 13, 4)
        self.addWidget(self.ITCHNG, 14, 4)
        self.ITCHNG.valueChanged.connect(
            lambda: setattr(self.viewModel, 'ITCHNG', self.ITCHNG.value())
        )

        self.setRowMinimumHeight(15, 30)

        TYPELabel = QLabel("Finite difference scheme")
        TYPELabel.setAlignment(Qt.AlignLeft)
        self.TYPE = QComboBox()
        self.TYPE.addItem("Centered in Time")
        self.TYPE.addItem("Implicit")
        self.addWidget(TYPELabel, 16, 0)
        self.addWidget(self.TYPE, 17, 0)
        self.TYPE.currentIndexChanged.connect(
            lambda: setattr(self.viewModel, 'TYPE', self.TYPE.currentText())
        )

        STATLabel = QLabel("Mass Transport Solutions")
        STATLabel.setAlignment(Qt.AlignLeft)
        self.STAT = QComboBox()
        self.STAT.addItem("Flow equation only")
        self.STAT.addItem("Steady-state")
        self.STAT.addItem("Transient")
        self.addWidget(STATLabel, 16, 1)
        self.addWidget(self.STAT, 17, 1)
        self.STAT.currentIndexChanged.connect(
            lambda: setattr(self.viewModel, 'STAT', self.STAT.currentText())
        )

        STATPLabel = QLabel("Flow Solutions")
        STATPLabel.setAlignment(Qt.AlignLeft)
        self.STATP = QComboBox()
        self.STATP.addItem("Steady-state")
        self.STATP.addItem("Transient")
        self.addWidget(STATPLabel, 16, 2)
        self.addWidget(self.STATP, 17, 2)
        self.STATP.currentIndexChanged.connect(
            lambda: setattr(self.viewModel, 'STATP', self.STATP.currentText())
        )

        IGOLabel = QLabel("Number of Mass Transport Solutions")
        IGOLabel.setAlignment(Qt.AlignLeft)
        self.IGO = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
        self.IGO.setAlignment(Qt.AlignRight)
        self.addWidget(IGOLabel, 16, 3)
        self.addWidget(self.IGO, 17, 3)
        self.IGO.valueChanged.connect(
            lambda: setattr(self.viewModel, 'IGO', self.IGO.value())
        )

    def numNodesChange(self):
        self.viewModel.NN.setData(self.NN.value())

    def numElementsChange(self):
        self.viewModel.NE.setData(self.NE.value())

    def numMaterialsChange(self):
        self.viewModel.NK.setData(self.NK.value())

    def numSeepageFaceChange(self):
        self.viewModel.NSEEP.setData(self.NSEEP.value())

    def updateView(self):
        self.NN.setValue(self.viewModel.NN.getData())
        self.NE.setValue(self.viewModel.NE.getData())
        self.NK.setValue(self.viewModel.NK.getData())
        self.NB.setValue(self.viewModel.NB)
        self.KNB.setValue(self.viewModel.KNB)
        self.PL.setValue(self.viewModel.PL)
        self.EI.setValue(self.viewModel.EI)
        self.PCHNG.setValue(self.viewModel.PCHNG)
        self.BETAP.setValue(self.viewModel.BETAP)
        self.DIFUSN.setValue(self.viewModel.DIFUSN)
        self.CLOS1.setValue(self.viewModel.CLOS1)
        self.DELT.setValue(self.viewModel.DELT)
        self.CHNG.setValue(self.viewModel.CHNG)
        self.ITMAX.setValue(self.viewModel.ITMAX)
        self.ITER1.setValue(self.viewModel.ITER1)
        self.ITCHNG.setValue(self.viewModel.ITCHNG)
        self.IGO.setValue(self.viewModel.IGO)
        self.TYPE.setCurrentIndex(SchemeTypes.index(self.viewModel.TYPE))
        self.STAT.setCurrentIndex(TransportTypes.index(self.viewModel.STAT))
        self.STATP.setCurrentIndex(FlowTypes.index(self.viewModel.STATP))

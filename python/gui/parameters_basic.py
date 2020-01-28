from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


class BasicParameters(QGroupBox):
    def __init__(self, *args, **kwargs):
        super(BasicParameters, self).__init__('Basic Parameters')
        self.layout = QGridLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignCenter)
        self.layout.setHorizontalSpacing(20)
        self.setLayout(self.layout)
        self.setInputs()

    def setInputs(self):
        nodesLabel = QLabel("Number of Nodes")
        nodesLabel.setAlignment(Qt.AlignLeft)
        self.nodes = QSpinBox()
        self.nodes.setAlignment(Qt.AlignRight)
        self.layout.addWidget(nodesLabel, 1, 0)
        self.layout.addWidget(self.nodes, 2, 0)

        elementsLabel = QLabel("Number of Elements")
        elementsLabel.setAlignment(Qt.AlignLeft)
        self.elements = QSpinBox()
        self.elements.setAlignment(Qt.AlignRight)
        self.layout.addWidget(elementsLabel, 1, 1)
        self.layout.addWidget(self.elements, 2, 1)

        materialsLabel = QLabel("Number of Material Groups")
        materialsLabel.setAlignment(Qt.AlignLeft)
        self.materials = QSpinBox()
        self.materials.setAlignment(Qt.AlignRight)
        self.layout.addWidget(materialsLabel, 1, 2)
        self.layout.addWidget(self.materials, 2, 2)

        hbFlowLabel = QLabel("Half-Bandwidth (Flow)")
        hbFlowLabel.setAlignment(Qt.AlignLeft)
        self.hbFlow = QSpinBox()
        self.hbFlow.setAlignment(Qt.AlignRight)
        self.layout.addWidget(hbFlowLabel, 3, 0)
        self.layout.addWidget(self.hbFlow, 4, 0)

        hbMTLabel = QLabel("Half-Bandwidth (Mass Transport)")
        hbMTLabel.setAlignment(Qt.AlignLeft)
        self.hbMT = QSpinBox()
        self.hbMT.setAlignment(Qt.AlignRight)
        self.layout.addWidget(hbMTLabel, 3, 1)
        self.layout.addWidget(self.hbMT, 4, 1)

        minPhLabel = QLabel("Minimum Pressure Head")
        minPhLabel.setAlignment(Qt.AlignLeft)
        self.minPH = QDoubleSpinBox()
        self.minPH.setDecimals(3)
        self.minPH.setAlignment(Qt.AlignRight)
        self.layout.addWidget(minPhLabel, 5, 0)
        self.layout.addWidget(self.minPH, 6, 0)

        maxIrLabel = QLabel("Maximum Infiltration Rate")
        maxIrLabel.setAlignment(Qt.AlignLeft)
        self.maxIR = QDoubleSpinBox()
        self.maxIR.setDecimals(3)
        self.maxIR.setAlignment(Qt.AlignRight)
        self.layout.addWidget(maxIrLabel, 5, 1)
        self.layout.addWidget(self.maxIR, 6, 1)

        timeSLabel = QLabel("Initial Time Step")
        timeSLabel.setAlignment(Qt.AlignLeft)
        self.timeStep = QDoubleSpinBox()
        self.timeStep.setDecimals(3)
        self.timeStep.setAlignment(Qt.AlignRight)
        self.layout.addWidget(timeSLabel, 7, 0)
        self.layout.addWidget(self.timeStep, 8, 0)

        timeSMLabel = QLabel("Time Step Multiplier")
        timeSMLabel.setAlignment(Qt.AlignLeft)
        self.timeStepM = QDoubleSpinBox()
        self.timeStepM.setDecimals(3)
        self.timeStepM.setAlignment(Qt.AlignRight)
        self.layout.addWidget(timeSMLabel, 7, 1)
        self.layout.addWidget(self.timeStepM, 8, 1)

        maxTimeSLabel = QLabel("Maximum Time Steps")
        maxTimeSLabel.setAlignment(Qt.AlignLeft)
        self.maxTimeStep = QSpinBox()
        self.maxTimeStep.setAlignment(Qt.AlignRight)
        self.layout.addWidget(maxTimeSLabel, 9, 0)
        self.layout.addWidget(self.maxTimeStep, 10, 0)

        iterTimeSLabel = QLabel("Iterations Per Time Step")
        iterTimeSLabel.setAlignment(Qt.AlignLeft)
        self.timeStepIter = QSpinBox()
        self.timeStepIter.setAlignment(Qt.AlignRight)
        self.layout.addWidget(iterTimeSLabel, 9, 1)
        self.layout.addWidget(self.timeStepIter, 10, 1)

    def getNumNodes(self):
        return self.nodes.value()

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

import ctypes
from gs2 import types
from gs2.runner import Runner

class SimulationPage(QGroupBox):
    def __init__(self, config):
        super(SimulationPage, self).__init__('Simulation')

        self.config = config

        pageLayout = QHBoxLayout()
        self.setLayout(pageLayout)

        sideNav = QVBoxLayout()
        sideNav.setAlignment(Qt.AlignCenter | Qt.AlignTop)
        pageLayout.addLayout(sideNav)

        contentStack = QStackedLayout()
        pageLayout.addLayout(contentStack)

        #editFileOptions

        runSimulationBttn = QPushButton('Run Simulation')
        runSimulationBttn.setGeometry(0, 0, 150, 100)
        runSimulationBttn.pressed.connect(self.onClickRun)
        sideNav.addWidget(runSimulationBttn)



    def onClickRun(self):
        simulationRunner = Runner(self.config)
        simulationRunner.run()


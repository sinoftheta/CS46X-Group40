from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .simulation import SimulationModel
from .simulation import SimulationView
from gs2.runner import Runner

class SimulationController(QGroupBox):
    def __init__(self, config):
        super(SimulationController, self).__init__('Simulation')

        self.config = config

        self.simulationModel = SimulationModel(config)
        self.simulationView = SimulationView(self.simulationModel)

        pageLayout = QHBoxLayout()
        pageLayout.setAlignment(Qt.AlignLeft)
        self.setLayout(pageLayout)

        sideNav = QVBoxLayout()
        sideNav.setAlignment(Qt.AlignCenter | Qt.AlignTop)
        sideNav.setGeometry(QRect(0, 0, 150, 100))
        sideNav.setSizeConstraint(QLayout.SetFixedSize)
        pageLayout.addLayout(sideNav)

        sideNav.addWidget(self.simulationView)

        runSimulationBttn = QPushButton('Run Simulation')
        runSimulationBttn.setGeometry(0, 0, 150, 100)
        runSimulationBttn.pressed.connect(self.onClickRun)
        sideNav.addWidget(runSimulationBttn)

        self.simOutput = QTextEdit()
        self.simOutput.setReadOnly(True)
        pageLayout.addWidget(self.simOutput)

         # monitor file for changes
        def fileWatchCallback(file):
            with open(file) as x:
                f = x.read()
                self.simOutput.setText(f)

        self.fileWatcher = QFileSystemWatcher()
        self.fileWatcher.fileChanged.connect(fileWatchCallback)

    def onClickRun(self):
        simulationRunner = Runner(self.simulationModel, self.config)
        simulationRunner.openFiles()

        stdout = simulationRunner.stdout

        if stdout in self.fileWatcher.files():
            self.fileWatcher.removePaths(self.fileWatcher.files())
        self.fileWatcher.addPath(stdout)

        simulationRunner.run()
    



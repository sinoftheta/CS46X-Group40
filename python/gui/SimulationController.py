from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .simulation import SimulationModel
from .simulation import SimulationView
from gs2.runner import Runner
from gs2 import types

class SimulationController(QGroupBox):
    def __init__(self, config):
        super(SimulationController, self).__init__('Simulation')

        self.config = config

        self.simulationModel = SimulationModel(config)
        self.simulationView = SimulationView(self.simulationModel)

        self.gs2CallbackListeners = []

        pageLayout = QHBoxLayout()
        pageLayout.setAlignment(Qt.AlignLeft)
        self.setLayout(pageLayout)

        self.sideNav = QVBoxLayout()
        self.sideNav.setAlignment(Qt.AlignCenter | Qt.AlignTop)
        self.sideNav.setGeometry(QRect(0, 0, 150, 100))
        self.sideNav.setSizeConstraint(QLayout.SetFixedSize)
        pageLayout.addLayout(self.sideNav)

        self.sideNav.addWidget(self.simulationView)

        runSimulationBttn = QPushButton('Run Simulation')
        runSimulationBttn.setGeometry(0, 0, 150, 100)
        runSimulationBttn.pressed.connect(self.onClickRun)
        self.sideNav.addWidget(runSimulationBttn)

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

        callback = types.CallbackType(self.notifyOnCallback)
        simulationRunner.registerCallback(callback)

        stdout = simulationRunner.stdout

        if stdout in self.fileWatcher.files():
            self.fileWatcher.removePaths(self.fileWatcher.files())
        self.fileWatcher.addPath(stdout)

        simulationRunner.run()
    
    def updateView(self, model):
        self.simulationModel = model
        self.sideNav.removeWidget(self.simulationView)
        self.simulationView.deleteLater()
        self.simulationView = SimulationView(self.simulationModel)
        self.sideNav.addWidget(self.simulationView)

    def getSimulationModel(self):
        return self.simulationModel

    def addGS2CallbackListener(self, listener):
        if listener not in self.gs2CallbackListeners:
            self.gs2CallbackListeners.append(listener)
    
    def removeGS2CallbackListener(self, listener):
        self.gs2CallbackListeners.remove(listener)

    # return 0 required as to match the type in c
    def notifyOnCallback(self, state):
        for listener in self.gs2CallbackListeners:
            listener.onCallback(state)
        return 0


class GS2CallbackListener:
    def onCallback(self, state):
        pass



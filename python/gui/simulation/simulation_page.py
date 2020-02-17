from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

import ctypes
from gs2 import types
from gs2.runner import Runner

import sys
import pathlib

from .file_options import FileOptions

class SimulationPage(QGroupBox):
    def __init__(self, config):
        super(SimulationPage, self).__init__('Simulation')

        self.config = config

        pageLayout = QHBoxLayout()
        pageLayout.setAlignment(Qt.AlignLeft)
        self.setLayout(pageLayout)

        sideNav = QVBoxLayout()
        sideNav.setAlignment(Qt.AlignCenter | Qt.AlignTop)
        sideNav.setGeometry(QRect(0, 0, 150, 100))
        sideNav.setSizeConstraint(QLayout.SetFixedSize)
        pageLayout.addLayout(sideNav)

        #editFileOptions
        fileOptions = FileOptions(self.config)
        sideNav.addWidget(fileOptions)

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
        simulationRunner = Runner(self.config)
        simulationRunner.openFiles()

        stdout = simulationRunner.stdout

        if stdout not in self.fileWatcher.files():
            self.fileWatcher.removePaths(self.fileWatcher.files())
            self.fileWatcher.addPath(stdout)

        simulationRunner.run()

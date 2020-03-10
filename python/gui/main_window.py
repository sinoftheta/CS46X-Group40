import sys
from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

import gs2

from os import path

from .parameters_window import ParametersPage
from .parameters_window import IOListener

from .SimulationController import SimulationController
from .BasicParametersController import BasicParametersController
from .ElementsController import ElementsController
from .MeshViewController import MeshViewController

class MainWindow(QMainWindow, IOListener):
    def __init__(self, config, *args, **kwargs):
        super(MainWindow, self).__init__(*args, **kwargs)

        # reference to config.ini
        self.config = config

        #   Set window title
        self.setWindowTitle("GS2")
        self.setGeometry(0, 0, 1280, 720)
        #   Vertically oriented container
        #   (to be used as parent container)
        self.homePageLayout = QVBoxLayout()
        #   Horizontally oriented container
        self.homePageButtons = QHBoxLayout()
        #   Stack of containers, 1 visible at a time
        self.homePageStack = QStackedLayout()

        #   Add two containers, in order to
        #       parent container 'homePageLayout'
        self.homePageLayout.addLayout(self.homePageButtons)
        self.homePageLayout.addLayout(self.homePageStack)

        #   Create 'QWidget' object for each screen
        #   Then add to array of pages 'self.homePageStack'
        #   Parameters
        self.parametersPage = ParametersPage(self.config)
        self.parametersPage.addIOListener(self)
        self.homePageStack.addWidget(self.parametersPage)
        #   Mesh
        self.meshPage = MeshViewController()
        self.homePageStack.addWidget(self.meshPage)
        #   Simulation
        self.simulationPage = SimulationController(self.config)
        self.simulationPage.addGS2CallbackListener(self.meshPage)
        self.homePageStack.addWidget(self.simulationPage)

        self.meshPageLayout = QVBoxLayout()
        self.simulationPageLayout = QVBoxLayout()

        self.setHomePageButtons()

        self.homePageButtons.setAlignment(Qt.AlignLeft)
        self.homePageButtons.setContentsMargins(0, 0, 0, 0)
        self.homePageButtons.setSpacing(20)

        #   Create home page widget
        self.homePageWidget = QWidget()
        self.homePageWidget.setLayout(self.homePageLayout)
        self.setCentralWidget(self.homePageWidget)


    def setHomePageButtons(self):
        #   Add navigation buttons (widgets) to
        #       button container 'self.homePageButtons'
        self.parametersButton = QPushButton('Parameters')
        self.parametersButton.pressed.connect(lambda index=0:
                            self.homePageStack.setCurrentIndex(index))
        self.homePageButtons.addWidget(self.parametersButton)

        self.meshButton = QPushButton('Mesh')
        self.meshButton.pressed.connect(lambda index=1:
                            self.homePageStack.setCurrentIndex(index))
        self.homePageButtons.addWidget(self.meshButton)

        self.simulationButton = QPushButton('Simulation')
        self.simulationButton.pressed.connect(lambda index=2:
                            self.homePageStack.setCurrentIndex(index))
        self.homePageButtons.addWidget(self.simulationButton)



    def onExport(self):
        fileWriter = gs2.FileWriter(
            self.parametersPage.materialsController.getMaterials(),
            self.simulationPage.getSimulationModel(),
            self.parametersPage.seepageFaceController.getSeepageFaces(),
            self.parametersPage.basicParametersController.getBasicParametersModel(),
            self.parametersPage.elementsController.getElements(),
            self.parametersPage.multipliersController.getElements()
        )

        filePath = path.join(self.config['paths']['bundle'], self.config['paths']['data-out'])
        fileWriter.write(filePath)

    def onImport(self, filepath):
        fileReader = gs2.FileReader()
        fileReader.read(filepath)

        # file read now has data foreach model
        # the controllers should have updateModel methods
        # so the views can be updated.
        
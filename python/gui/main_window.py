import sys
from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from parameters_window import ParametersPage

class MainWindow(QMainWindow):
    def __init__(self, *args, **kwargs):
        super(MainWindow, self).__init__(*args, **kwargs)

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
        self.parametersPage = ParametersPage()
        self.homePageStack.addWidget(self.parametersPage)
        #   Mesh
        self.meshPage = QGroupBox('Mesh')
        self.homePageStack.addWidget(self.meshPage)
        #   Simulation
        self.simulationPage = QGroupBox('Simulation')
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

def main():
        app = QApplication(sys.argv)
        mainWindow = MainWindow()
        mainWindow.show()
        app.exec_()

if __name__ == "__main__":
    main()

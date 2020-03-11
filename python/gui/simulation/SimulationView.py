from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from enum import Enum

from .SimulationModel import GS2KOD

class SimulationEnum(Enum):
    DATA_INPUT = 1
    SIM_OUT = 2
    SIM_ERR = 3

class SimulationView(QGroupBox):
    def __init__(self, simulationModel):
        super(SimulationView, self).__init__('Simulation Config')
        self.simulationModel = simulationModel
        self.updateView()


    def updateView(self):
        layout = QVBoxLayout()
        layout.setAlignment(Qt.AlignLeft)

        ioLayout = QGridLayout()
        ioLayout.setAlignment(Qt.AlignCenter | Qt.AlignTop)
        ioLayout.setHorizontalSpacing(15)

        # UI component for title
        simulationTitle = QLabel('Simulation Title')
        simulationTitle.setAlignment(Qt.AlignLeft)
        ioLayout.addWidget(simulationTitle, 0, 0)

        simulationTitleInput = QLineEdit(self.simulationModel.simulationTitle)
        simulationTitleInput.editingFinished.connect(lambda: self.simulationModel.setSimulationTitle(simulationTitleInput.text()))
        ioLayout.addWidget(simulationTitleInput, 0, 1)


        # UI componenents pertaining to input file
        dataInput = QLabel('Simulation Input')
        dataInput.setAlignment(Qt.AlignLeft)
        ioLayout.addWidget(dataInput, 1, 0)

        self.dataInputPath = QLineEdit(self.simulationModel.dataInputFile)
        self.dataInputPath.setReadOnly(True)
        ioLayout.addWidget(self.dataInputPath, 1, 1)

        dataInputPathEdit = QPushButton('Edit')
        dataInputPathEdit.pressed.connect(self._onEditClick(SimulationEnum.DATA_INPUT))
        ioLayout.addWidget(dataInputPathEdit, 1, 2)

        # UI Components pertaining to stdout
        simulationOutput = QLabel('Simulation Output')
        simulationOutput.setAlignment(Qt.AlignLeft)
        ioLayout.addWidget(simulationOutput, 2, 0)

        self.simulationOutputPath = QLineEdit(self.simulationModel.stdout)
        self.simulationOutputPath.setReadOnly(True)
        ioLayout.addWidget(self.simulationOutputPath, 2, 1)

        simulationOutputPathEdit = QPushButton('Edit')
        simulationOutputPathEdit.pressed.connect(self._onEditClick(SimulationEnum.SIM_OUT))
        ioLayout.addWidget(simulationOutputPathEdit, 2, 2)

        # UI Components pertaining to program errors
        errorOutput = QLabel('Simulation Errors')
        errorOutput.setAlignment(Qt.AlignLeft)
        ioLayout.addWidget(errorOutput, 3, 0)

        self.errorOutputPath = QLineEdit(self.simulationModel.stdout)
        self.errorOutputPath.setReadOnly(True)
        ioLayout.addWidget(self.errorOutputPath, 3, 1)

        errorOutputPathEdit = QPushButton('Edit')
        errorOutputPathEdit.pressed.connect(self._onEditClick(SimulationEnum.SIM_ERR))
        ioLayout.addWidget(errorOutputPathEdit, 3, 2)

        # UI Components pertaining to what gets printed (KOD)
        kodLayout = QGridLayout()
        kodLayout.setAlignment(Qt.AlignCenter | Qt.AlignTop)
        kodLayout.setHorizontalSpacing(15)
        # layout.addLayout(kodLayout)

        kodLayoutIndexLeft = 0
        kodLayoutIndexRight = 0
        for kod in GS2KOD:
            kodLabel = QLabel(kod.name)

            if kod.value > 0:
                kodComboBox = QComboBox()
                kodComboBox.setFixedWidth(50)

                for x in range(kod.value + 1):
                    kodComboBox.addItem(str(x))

                kodComboBox.setCurrentIndex(self.simulationModel.getOutputModifier(kod))
                kodComboBox.currentIndexChanged.connect(lambda index: self.simulationModel.setOutputModifier(kod, kodComboBox.currentText()))

                kodLayout.addWidget(kodLabel, kodLayoutIndexLeft, 0)
                kodLayout.addWidget(kodComboBox, kodLayoutIndexLeft, 1)

                kodLayoutIndexLeft += 1

            else:
                kodSB = QSpinBox(buttonSymbols = QSpinBox.NoButtons)
                kodSB.setFixedWidth(50)
                kodSB.setValue(self.simulationModel.getOutputModifier(kod))
                # Prevents inputs from outside of this range (QValidator does not)
                kodSB.setRange(1, 100)
                kodSB.valueChanged.connect(lambda: self.simulationModel.setOutputModifier(kod, kodSB.value()))


                kodLayout.addWidget(kodLabel, kodLayoutIndexRight, 2)
                kodLayout.addWidget(kodSB, kodLayoutIndexRight, 3)

                kodLayoutIndexRight += 1

        layout.addLayout(ioLayout)
        layout.addLayout(kodLayout)

        self.setLayout(layout)

    def _onEditClick(self, simulationEnum):
        def inner():
            dialog = QFileDialog(None)
            dialog.setFileMode(QFileDialog.AnyFile)
            dialog.setViewMode(QFileDialog.Detail)

            if dialog.exec_():
                filename = dialog.selectedFiles()[0]

                if simulationEnum == SimulationEnum.DATA_INPUT:
                    self.simulationModel.dataInputFile = filename
                    self.dataInputPath.setText(filename)
                elif simulationEnum == SimulationEnum.SIM_OUT:
                    self.simulationModel.stdout = filename
                    self.simulationOutputPath.setText(filename)
                elif simulationEnum == SimulationEnum.SIM_ERR:
                    self.simulationModel.stderr = filename
                    self.errorOutputPath.setText(filename)
        return inner

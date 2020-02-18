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

        layout = QHBoxLayout()
        layout.setAlignment(Qt.AlignLeft)

        ioLayout = QGridLayout()
        ioLayout.setAlignment(Qt.AlignCenter | Qt.AlignTop)
        ioLayout.setHorizontalSpacing(15)
        layout.addLayout(ioLayout)

        # UI componenents pertaining to input file
        dataInput = QLabel('Simulation Input')
        dataInput.setAlignment(Qt.AlignLeft)
        ioLayout.addWidget(dataInput, 0, 0)

        self.dataInputPath = QLineEdit(self.simulationModel.dataInputFile)
        self.dataInputPath.setReadOnly(True)
        ioLayout.addWidget(self.dataInputPath, 0, 1)

        dataInputPathEdit = QPushButton('Edit')
        dataInputPathEdit.pressed.connect(self._onEditClick(SimulationEnum.DATA_INPUT))
        ioLayout.addWidget(dataInputPathEdit, 0, 2)

        # UI Components pertaining to stdout
        simulationOutput = QLabel('Simulation Output')
        simulationOutput.setAlignment(Qt.AlignLeft)
        ioLayout.addWidget(simulationOutput, 1, 0)

        self.simulationOutputPath = QLineEdit(self.simulationModel.stdout)
        self.simulationOutputPath.setReadOnly(True)
        ioLayout.addWidget(self.simulationOutputPath, 1, 1)

        simulationOutputPathEdit = QPushButton('Edit')
        simulationOutputPathEdit.pressed.connect(self._onEditClick(SimulationEnum.SIM_OUT))
        ioLayout.addWidget(simulationOutputPathEdit, 1, 2)

        # UI Components pertaining to program errors
        errorOutput = QLabel('Simulation Errors')
        errorOutput.setAlignment(Qt.AlignLeft)
        ioLayout.addWidget(errorOutput, 2, 0)

        self.errorOutputPath = QLineEdit(self.simulationModel.stdout)
        self.errorOutputPath.setReadOnly(True)
        ioLayout.addWidget(self.errorOutputPath, 2, 1)

        errorOutputPathEdit = QPushButton('Edit')
        errorOutputPathEdit.pressed.connect(self._onEditClick(SimulationEnum.SIM_ERR))
        ioLayout.addWidget(errorOutputPathEdit, 2, 2)

        # UI Components pertaining to what gets printed (KOD)
        ioLayoutIndexLeft = 3
        ioLayoutIndexRight = 3
        for kod in GS2KOD:
            kodLabel = QLabel(kod.name)

            if kod.value > 0:
                kodComboBox = QComboBox()
                kodComboBox.setFixedWidth(50)

                for x in range(kod.value + 1):
                    kodComboBox.addItem(str(x))

                kodComboBox.setCurrentIndex(self.simulationModel.getOutputModifier(kod))
                kodComboBox.currentIndexChanged.connect(lambda index: self.simulationModel.setOutputModifier(kod, kodComboBox.currentText()))

                ioLayout.addWidget(kodLabel, ioLayoutIndexLeft, 0)
                ioLayout.addWidget(kodComboBox, ioLayoutIndexLeft, 1)

                ioLayoutIndexLeft += 1

            else:
                kodLineEdit = QLineEdit()
                kodLineEdit.setText(str(self.simulationModel.getOutputModifier(kod)))
                kodLineEdit.setValidator(QIntValidator(1, 100))
                kodLineEdit.editingFinished.connect(lambda: self.simulationModel.setOutputModifier(kod, int(kodLineEdit.text())))


                ioLayout.addWidget(kodLabel, ioLayoutIndexRight, 2)
                ioLayout.addWidget(kodLineEdit, ioLayoutIndexRight, 3)

                ioLayoutIndexRight += 1
        


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

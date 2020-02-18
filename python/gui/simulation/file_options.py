from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


from os import path

class FileOptions(QGroupBox):
    def __init__(self, config):
        super(FileOptions, self).__init__('I/O Config')

        self.config = config

        layout = QGridLayout()
        layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        layout.setHorizontalSpacing(15)
        
        dataInput = QLabel('Simulation Input')
        dataInput.setAlignment(Qt.AlignLeft)
        layout.addWidget(dataInput, 0, 0)

        # TODO: call export here, or keep track of the most recent exported file
        dataInputPath = QLineEdit(path.abspath(self.config['paths']['exampleCsv']))
        layout.addWidget(dataInputPath, 0, 1)

        dataInputPathEdit = QPushButton("Edit")
        dataInputPathEdit.pressed.connect(self.onEditClick('paths', 'data-in', dataInputPath))
        layout.addWidget(dataInputPathEdit, 0, 2)

        mainOutput = QLabel('Simulation Output')
        mainOutput.setAlignment(Qt.AlignLeft)
        layout.addWidget(mainOutput, 1, 0)

        mainOutputPath = QLineEdit(path.abspath(self.config['io']['default-out']))
        layout.addWidget(mainOutputPath, 1, 1)

        mainOutputPathEdit = QPushButton("Edit")
        mainOutputPathEdit.pressed.connect(self.onEditClick('io', 'default-out', mainOutputPath))
        layout.addWidget(mainOutputPathEdit, 1, 2)

        errorOutput = QLabel('Simulation Error Log')
        errorOutput.setAlignment(Qt.AlignLeft)
        layout.addWidget(errorOutput, 2, 0)
     
        errorOutputPath = QLineEdit(path.abspath(self.config['io']['default-err']))
        layout.addWidget(errorOutputPath, 2, 1)

        errorOutputPathEdit = QPushButton("Edit")
        errorOutputPathEdit.pressed.connect(self.onEditClick('io', 'default-err', errorOutputPath))
        layout.addWidget(errorOutputPathEdit, 2, 2)

        self.setLayout(layout)


    def onEditClick(self, configGroup, configKey, lineEdit):
        def inner():
            dialog = QFileDialog(None)
            dialog.setFileMode(QFileDialog.AnyFile)
            dialog.setViewMode(QFileDialog.Detail)

            if dialog.exec_():
                filename = dialog.selectedFiles()[0]
                lineEdit.setText(filename)
                self.config[configGroup][configKey] = filename
        return inner 
from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


class FileOptions(QGroupBox):
    def __init__(self, config):
        super(FileOptions, self).__init__('I/O Config')

        self.config = config

        layout = QGridLayout()
        layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        layout.setHorizontalSpacing(15)
        
        mainOutput = QLabel('Simulation Output')
        mainOutput.setAlignment(Qt.AlignLeft)
        layout.addWidget(mainOutput, 0, 0)

        mainOutputPath = QLineEdit(self.config['io']['default-out'])
        layout.addWidget(mainOutputPath, 0, 1)

        mainOutputPathEdit = QPushButton("Edit")
        mainOutputPathEdit.pressed.connect(self.onEditClick('default-out', mainOutputPath))
        layout.addWidget(mainOutputPathEdit, 0, 2)

        errorOutput = QLabel('Simulation Error Log')
        errorOutput.setAlignment(Qt.AlignLeft)
        layout.addWidget(errorOutput, 1, 0)
     
        errorOutputPath = QLineEdit(self.config['io']['default-err'])
        layout.addWidget(errorOutputPath, 1, 1)

        errorOutputPathEdit = QPushButton("Edit")
        errorOutputPathEdit.pressed.connect(self.onEditClick('default-err', errorOutputPath))
        layout.addWidget(errorOutputPathEdit, 1, 2)

        self.setLayout(layout)


    def onEditClick(self, configKey, lineEdit):
        def inner():
            dialog = QFileDialog(None)
            dialog.setFileMode(QFileDialog.AnyFile)
            dialog.setNameFilter("Text (*.txt)")
            dialog.setViewMode(QFileDialog.Detail)

            if dialog.exec_():
                filename = dialog.selectedFiles()[0]
                lineEdit.setText(filename)
                self.config['io'][configKey] = filename
        return inner 
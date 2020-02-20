
import sys
import configparser
from os import path

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from gui.main_window import MainWindow

from gui.MaterialsController import MaterialsChangeListener


# in practice, do something like:
# class MaterialDependentController(QGroupBox, MaterialChangeListener):
# so that the MaterialDependentController knows when a material is added/removed
class ExampleListener(MaterialsChangeListener):
    def onMaterialAdded(self, material):
        print("Material " + str(material.materialGroup) + " Added!")

    def onMaterialRemoved(self, material):
        print("Material " + str(material.materialGroup) + " Removed!")


def getConfig():
    bundle_dir = path.abspath(path.dirname(__file__))

    if (getattr(sys, 'frozen', False)):
        # running in pyinstaller bundle
        bundle_dir = path.dirname(sys.executable)

    config_path = path.abspath(path.join(bundle_dir, 'config/config.ini'))

    config = configparser.ConfigParser()
    config.read(config_path)

    config['paths']['bundle'] = bundle_dir

    return config

def main():
    app = QApplication(sys.argv)
    mainWindow = MainWindow(getConfig())

    # TODO: remove before beta
    mainWindow.parametersPage.materialsController.addMaterialChangeListener(ExampleListener())

    mainWindow.show()
    app.exec_()

if __name__ == "__main__":
    main()

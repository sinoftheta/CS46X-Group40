from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .parameters.ParametersView import ParametersView
from .parameters.ParametersModel import ParametersModel

class BasicParametersController(QGroupBox):
    def __init__(self):
        super(BasicParametersController, self).__init__('Basic Parameters')
        self.parametersModel = ParametersModel()
        self.layout = ParametersView(self.parametersModel)
        self.setLayout(self.layout)

        self.parameterChangeListeners = []

        self.parametersModel.NN.connectObserver(lambda newData: self.notifyNodeCountChange(newData))
        self.parametersModel.NE.connectObserver(lambda newData: self.notifyElementCountChange(newData))
        self.parametersModel.NK.connectObserver(lambda newData: self.notifyMaterialCountChange(newData))

    """
        Listener Methods
    """
    def addBasicParameterListener(self, listener):
        if listener not in self.parameterChangeListeners:
            self.parameterChangeListeners.append(listener)

    def removeBasicParameterListener(self, listener):
        self.parameterChangeListeners.remove(listener)

    def notifyNodeCountChange(self, nodeCount):
        for listener in self.parameterChangeListeners:
            listener.onNodeCountChange(self, nodeCount)

    def notifyElementCountChange(self, elementCount):
        for listener in self.parameterChangeListeners:
            listener.onElementCountChange(self, elementCount)

    def notifyMaterialCountChange(self, materialCount):
        for listener in self.parameterChangeListeners:
            listener.onMaterialCountChange(self, materialCount)

class BasicParameterChangeListener:
    def onNodeCountChange(self, newCount):
        pass

    def onElementCountChange(self, newCount):
        pass

    def onMaterialCountChange(self, newCount):
        pass

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .elements.ElementModel import ElementModel
from .elements.ElementView import ElementView

from .BasicParametersController import BasicParameterChangeListener

class ElementsController(QGroupBox, BasicParameterChangeListener):
    def __init__(self):
        super(ElementsController, self).__init__('Elements')
        self.layout = QHBoxLayout()
        self.layout.setAlignment(Qt.AlignLeft | Qt.AlignTop)

        # list of element models that this controller 'controls'
        self.elementsModels = []

        # Objects that want to know material group of elements
        self.elementsChangeListeners = []

        self.elementsList = QListWidget()
        self.elementsList.setWindowTitle("Elements")
        self.elementsList.itemClicked.connect(self.elementSelectionChange)
        self.elementsList.setFixedSize(100, 200)
        self.layout.addWidget(self.elementsList)

        self.currentElement = None

        self.setLayout(self.layout)

    def getElements(self):
        return self.elementsModels

    def pushElement(self):
        elementNumber = str(len(self.elementsModels) + 1)
        elementModel = ElementModel(elementNumber)
        newElement = QListWidgetItem(elementNumber)
        newElement.setTextAlignment(Qt.AlignCenter)
        self.elementsModels.append(elementModel)
        self.elementsList.addItem(newElement)

        self.notifyElementAdded(elementModel)

    def popElement(self):
        lastElement = self.elementsModels.pop()
        element = self.elementsList.takeItem(self.elementsList.count() - 1)
        self.notifyElementRemoved(lastElement)

    def elementSelectionChange(self, element):
        if self.currentElement != None:
            self.layout.removeWidget(self.currentElement)
            self.currentElement.deleteLater()
            self.currentElement = None

        elementIndex = int(element.text()) - 1
        model = self.elementsModels[elementIndex]
        self.currentElement = ElementView(model)

        # TODO: connect observer for currently displayed
        #       element's material group change ?

        self.layout.addWidget(self.currentElement)

    def onElementCountChange(self, count):
        if count > len(self.elementsModels):
            while count > len(self.elementsModels):
                self.pushElement()
        else:
            while len(self.elementsModels) > count:
                self.popElement()

        if (
                (self.currentElement != None) and
                (int(self.currentElement.viewModel.elementNumber) >=
                len(self.elementsModels))
           ):
            self.layout.removeWidget(self.currentElement)
            self.currentElement.deleteLater()
            self.currentElement = None
            self.elementsList.clearSelection()


    """
        Listeners
    """
    def addElementChangeListener(self, listener):
        if listener not in self.elementsChangeListeners:
            self.elementsChangeListeners.append(listener)

    def removeElementChangeListener(self, listener):
        self.elementsChangeListeners.remove(listener)

    def notifyElementAdded(self, elementModel):
        for listener in self.elementsChangeListeners:
            listener.onElementAdded(elementModel)

    def notifyElementRemoved(self, elementModel):
        for listener in self.elementsChangeListeners:
            list.onElementRemoved(elementModel)

class ElementsChangeListener:
    def onElementAdded(self, elementModel):
        pass
    def onElementRemoved(self, elementModel):
        pass

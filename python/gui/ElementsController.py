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

        # TODO: adjust this mechanism for material groups ?
        # Max material group number
        self.materialMax = 0

        self.elementLayout = QVBoxLayout()
        self.elementLayout.setAlignment(Qt.AlignTop)
        self.elementLayout.setContentsMargins(0, 0, 0, 0)
        self.elementLayout.setSpacing(0)

        self.elementListLabel = QLabel("Select Element")
        self.elementListLabel.setFont(QFont('Arial', 16))
        self.elementListLabel.setAlignment(Qt.AlignLeft)
        self.elementLayout.addWidget(self.elementListLabel)

        self.elementsList = QListWidget()
        #self.elementsList.setContentsMargins(0, 0, 0, 0)
        self.elementsList.itemClicked.connect(self.elementSelectionChange)
        self.elementsList.setFixedSize(100, 200)
        self.elementLayout.addWidget(self.elementsList)

        self.layout.addLayout(self.elementLayout)

        self.currentElement = None

        self.setLayout(self.layout)

    def getElements(self):
        return self.elementsModels

    def pushElement(self):
        elementNumber = str(len(self.elementsModels) + 1)
        elementModel = ElementModel(elementNumber, self.materialMax)
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
            # TODO: delete widgets from self.currentElement (ElementView)
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

        # clear currently selected element
        #   and remove currently displayed element
        if (self.currentElement != None):
            self.layout.removeWidget(self.currentElement)
            self.currentElement.deleteLater()
            self.currentElement = None
            self.elementsList.clearSelection()

    # TODO: adjust this to use MaterialsChangeListener onMaterialRemoved instead ?
    def onMaterialCountChange(self, count):
        self.materialMax = count
        # clear currently selected element
        #   and remove currently displayed element
        if (self.currentElement != None):
            self.layout.removeWidget(self.currentElement)
            self.currentElement.deleteLater()
            self.currentElement = None
            self.elementsList.clearSelection()

        for element in self.elementsModels:
            element.maxMaterialGroup = count
            # if new count of material groups is > an elements material group
            #   set to 0
            if (
                    (element.materialGroup.getData() != None)
                    and (element.materialGroup.getData() > count)
               ):
                element.materialGroup.setData(1)

    def onNodeCountChange(self, count):
        for element in self.elementsModels:
            element.nodeCount = count

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

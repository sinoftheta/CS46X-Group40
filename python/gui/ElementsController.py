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
        self.elementModels = []

        # TODO: adjust this mechanism for material groups ?
        # Max material group number
        self.materialGroupIdMax = 0
        self.nodeIdMax = 0

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
        return self.elementModels

    def updateView(self, elementModels):
        # clear all currenlty bound elements
        self.onElementCountChange(0)
        for element in elementModels:
            newElement = QListWidgetItem(element.elementNumber)
            newElement.setTextAlignment(Qt.AlignCenter)
            self.elementModels.append(element)
            self.elementsList.addItem(newElement)

    def pushElement(self):
        elementNumber = str(len(self.elementModels) + 1)
        elementModel = ElementModel(elementNumber)
        newElement = QListWidgetItem(elementNumber)
        newElement.setTextAlignment(Qt.AlignCenter)
        self.elementModels.append(elementModel)
        self.elementsList.addItem(newElement)

    def popElement(self):
        lastElement = self.elementModels.pop()
        element = self.elementsList.takeItem(self.elementsList.count() - 1)

    def elementSelectionChange(self, element):
        if self.currentElement != None:
            # TODO: delete widgets from self.currentElement (ElementView)
            self.layout.removeWidget(self.currentElement)
            self.currentElement.deleteLater()
            self.currentElement = None

        elementIndex = int(element.text()) - 1
        model = self.elementModels[elementIndex]
        self.currentElement = ElementView(model, self.materialGroupIdMax, self.nodeIdMax)

        # TODO: connect observer for currently displayed
        #       element's material group change ?

        self.layout.addWidget(self.currentElement)

    def onElementCountChange(self, count):
        if count > len(self.elementModels):
            while count > len(self.elementModels):
                self.pushElement()
        else:
            while len(self.elementModels) > count:
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
        self.materialGroupIdMax = count
        # clear currently selected element
        #   and remove currently displayed element
        if (self.currentElement != None):
            self.layout.removeWidget(self.currentElement)
            self.currentElement.deleteLater()
            self.currentElement = None
            self.elementsList.clearSelection()

        for element in self.elementModels:
            # if new count of material groups is > an elements material group
            #   set to 1
            if ( element.materialGroup > count ):
                element.materialGroup = 1

    def onNodeCountChange(self, count):
        self.nodeIdMax = count

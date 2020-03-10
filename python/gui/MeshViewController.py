from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .SimulationController import GS2CallbackListener

class MeshViewController(QGroupBox, GS2CallbackListener):
    def __init__(self):
        super(MeshViewController, self).__init__('Mesh')

    def onCallback(self, state):
        print("Number of Nodes: " + str(state.nn))

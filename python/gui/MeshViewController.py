import pyvista as pv
import numpy as np

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .SimulationController import GS2CallbackListener
from gs2 import types
from os import path
from ctypes import CDLL

class MeshViewController(QGroupBox, GS2CallbackListener):
    def __init__(self, config):
        super(MeshViewController, self).__init__('Mesh')
        
        self.layout = QVBoxLayout()
        self.frame = QFrame()
        self.frame.setFrameStyle(QFrame.StyledPanel | QFrame.Raised)
        self.vtk_widget = pv.QtInteractor(self.frame)
        self.mesh = None

        self.layout.addWidget(self.vtk_widget)
        self.setLayout(self.layout)

    def createMesh(self, state):
        
        vertices = np.array([[state.x[i], state.y[i]] for i in range(state.nn)])

        faces = []
        for i in range(state.ne):
            for j in range(state.memoryRequirements.maxne):
                if state._in[i][j] > 0:
                    faces.append(state._in[i][j] - 1)

        faces = [[len(row)] + row for row in faces]
        faces = np.hstack(faces)
        self.mesh = pv.PolyData(vertices, faces)
        self.vtk_widget.add_mesh(self.mesh)

    def onCallback(self, state):
        
        if self.mesh is None:
            self.createMesh(self.gs2, state)

        self.mesh.point_arrays['Pressure Head'] = np.array([state.phi[i] for i in range(state.nn)])
        self.mesh.point_arrays['Concentration'] = np.array([state.conc[i] for i in range(state.nn)])

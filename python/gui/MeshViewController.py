import pyvista as pv
import numpy as np

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .SimulationController import GS2CallbackListener
from gs2 import types

class MeshViewController(QGroupBox, GS2CallbackListener):
    def __init__(self, gs2):
        super(MeshViewController, self).__init__('Mesh')
        self.gs2 = gs2
        self.layout = QVBoxLayout()

        self.frame = QFrame()
        self.frame.setFrameStyle(QFrame.StyledPanel | QFrame.Raised)
        self.vtk_widget = pv.QtInteractor(self.frame)
        self.mesh = None

        self.layout.addWidget(self.vtk_widget)
        self.setLayout(self.layout)

    def createMesh(self, state):

        vertices = np.array([
            [
                cast(self.gs2.arrayAt(byref(state.x), i), POINTER(c_double)).contents.value,
                cast(self.gs2.arrayAt(byref(state.y), i), POINTER(c_double)).contents.value
            ]
            for i in range(1, state.nn + 1)
        ])

        faces = [
            [
                cast(self.gs2.matrixAt(byref(state.in), i, j, POINTER(c_double)).contents.value - 1
                for j in range(1, state.memoryRequirements.maxne + 1)
                if cast(self.gs2.matrixAt(byref(state.in), i, j, POINTER(c_double)).contents.value > 0
            ]
            for i in range(1, state.ne + 1)
        ]

        faces = [[len(row)] + row for row in faces]
        faces = np.hstack(faces)
        self.mesh = pv.PolyData(vertices, faces)
        self.vtk_widget.add_mesh(self.mesh)

    def onCallback(self, state):
        
        if self.mesh is None:
            self.createMesh(self.gs2, state)

        self.mesh.point_arrays['Pressure Head'] = np.array([
            cast(self.gs2.arrayAt(byref(state.phi), i), POINTER(c_double)).contents.value
            for i in range(1, state.nn + 1)
        ])

        self.mesh.point_arrays['Concentration'] = np.array([
            cast(self.gs2.arrayAt(byref(state.conc), i), POINTER(c_double)).contents.value
            for i in range(1, state.nn + 1)
        ])
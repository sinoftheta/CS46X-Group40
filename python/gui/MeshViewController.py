import pyvista as pv
import numpy as np

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .SimulationController import GS2CallbackListener

class MeshViewController(QGroupBox, GS2CallbackListener):
    def __init__(self):
        super(MeshViewController, self).__init__('Mesh')
        self.layout = QVBoxLayout()

        self.frame = QFrame()
        self.frame.setFrameStyle(QFrame.StyledPanel | QFrame.Raised)
        self.vtk_widget = pv.QtInteractor(self.frame)
        self.mesh = None

        self.layout.addWidget(self.vtk_widget)
        self.setLayout(self.layout)

    def createMesh(self, gs2, state):
        vertices = np.array([[gs2.arrayAt(byref(state.x, i)), gs2.arrayAt(byref(state.y, i))] for i in range(1, state.nn + 1)])
        faces = [[gs2.matrixAt(byref(state.in, i, j)) - 1 for j in range(1, state.memoryRequirements.maxne + 1) if gs2.matrixAt(byref(state.in, i, j)) > 0] for i in range(1, state.ne + 1)]
        faces = [[len(row)] + row for row in faces]
        faces = np.hstack(faces)
        self.mesh = pv.PolyData(vertices, faces)
        self.vtk_widget.add_mesh(self.mesh)

    def onCallback(self, gs2, state):
        if self.mesh is None:
            self.createMesh(gs2, state)
        self.mesh.point_arrays['Pressure Head'] = np.array([gs2.arrayAt(byref(state.phi), i) for i in range(1, state.nn + 1)])
        self.mesh.point_arrays['Concentration'] = np.array([gs2.arrayAt(byref(state.conc), i) for i in range(1, state.nn + 1)])
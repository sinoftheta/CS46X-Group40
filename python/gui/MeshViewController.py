import pyvista as pv
import numpy as np

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .SimulationController import GS2CallbackListener
from gs2 import types

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
        # vertices = np.array([[gs2.arrayAt(byref(state.x), i), gs2.arrayAt(byref(state.y), i)] for i in range(1, state.nn + 1)])

        vertices = []
        for i in range(1, state.nn + 1):
            x = cast(gs2.arrayAt(byref(state.x), i), POINTER(c_double)).contents.value
            y = cast(gs2.arrayAt(byref(state.y), i), POINTER(c_double)).contents.value
            vertices.append([x, y])
        vertices = np.array(vertices)
        

        # faces = [[gs2.matrixAt(byref(state.in), i, j) - 1 for j in range(1, state.memoryRequirements.maxne + 1)\
        #          if gs2.matrixAt(byref(state.in), i, j) > 0] for i in range(1, state.ne + 1)]

        faces = []
        for i in range(1, state.ne + 1):
            incidences = []
            for j in range(1, state.memoryRequirements.maxne + 1):
                inc = cast(gs2.matrixAt(byref(state.in), i, j, POINTER(c_double)).contents.value
                if inc > 0:
                    incidences.append(inc - 1)
            faces.append(incidences)


        faces = [[len(row)] + row for row in faces]
        faces = np.hstack(faces)
        self.mesh = pv.PolyData(vertices, faces)
        self.vtk_widget.add_mesh(self.mesh)

    def onCallback(self, gs2, state):
        
        if self.mesh is None:
            self.createMesh(gs2, state)

        # self.mesh.point_arrays['Pressure Head'] = np.array([gs2.arrayAt(byref(state.phi), i) for i in range(1, state.nn + 1)])

        head = []
        for i in range(1, state.nn + 1):
            h = cast(gs2.arrayAt(byref(state.phi), i), POINTER(c_double)).contents.value
            head.append(h)
        self.mesh.point_arrays['Pressure Head'] = np.array(head)

        # self.mesh.point_arrays['Concentration'] = np.array([gs2.arrayAt(byref(state.conc), i) for i in range(1, state.nn + 1)])

        conc = []
        for i in range(1, state.nn + 1):
            c = cast(gs2.arrayAt(byref(state.conc), i), POINTER(c_double)).contents.value
            conc.append(c)
        self.mesh.point_arrays['Concentration'] = np.array(conc)
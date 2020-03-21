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
        self.buttons = QHBoxLayout()
        self.stack = QStackedLayout()

        # Add child container to parent container
        self.layout.addLayout(self.buttons)
        self.layout.addLayout(self.stack)

        # Plotter for Pressure Head
        self.frame1 = QFrame()
        self.frame1.setFrameStyle(QFrame.StyledPanel | QFrame.Raised)
        self.plotter1 = pv.QtInteractor(self.frame1)
        self.plotter1.show_bounds(grid=True, location='back')
        self.stack.addWidget(self.plotter1.interactor)

        # Plotter for Concentration
        self.frame2 = QFrame()
        self.frame2.setFrameStyle(QFrame.StyledPanel | QFrame.Raised)
        self.plotter2 = pv.QtInteractor(self.frame2)
        self.plotter2.show_bounds(grid=True, location='back')
        self.stack.addWidget(self.plotter2.interactor)

        # Set up navigation buttons
        self.setMeshPageButtons()
        self.buttons.setAlignment(Qt.AlignLeft)
        self.buttons.setContentsMargins(0, 0, 0, 0)
        self.buttons.setSpacing(20)

        self.setLayout(self.layout)

    def setMeshPageButtons(self):
        # Navigation Button for Pressure Head mesh
        self.pressureButton = QPushButton('Pressure Head')
        self.pressureButton.pressed.connect(lambda: self.stack.setCurrentIndex(0))
        self.buttons.addWidget(self.pressureButton)

        # Navigation Button for Concentration mesh
        self.concentrationButton = QPushButton('Concentration')
        self.concentrationButton.pressed.connect(lambda: self.stack.setCurrentIndex(1))
        self.buttons.addWidget(self.concentrationButton)

    def createMesh(self, state):
        nodeLocations = []
        for i in range(state.nn):
            nodeLocations.append([state.x.elements[i], state.y.elements[i], 0.0])

        vertices = np.array(nodeLocations)
        # vertices = np.array([[x, y, 0.0] for x, y in zip(list(state.x), list(state.y))][:state.nn.value])

        # Numbering scheme
        scheme = [1, 4, 7, 10, 2, 3, 5, 6, 8, 9, 11, 12]

        faces = []
        for i in range(state.ne):
            face = []
            for j in range(int(state._in.elements[state.me-1][i])):
                face.append(int(state._in.elements[j][i]))
            faces.append(face)


        # incidences = np.array(state._in.elements[:state.inc]).T
        faces = [sorted(face, key=lambda x: scheme[face.index(x)]) for face in faces]

        faces = [[inc - 1 for inc in face if inc > 0] for face in faces][:state.ne]
        faces = [[len(row)] + row for row in faces]
        faces = np.hstack(faces)

        self.mesh1 = pv.PolyData(vertices, faces)
        self.mesh1.point_arrays['Pressure Head'] = np.array(state.phi.elements[:state.nn])

        # second mesh, can look more in depth, but this is showing both inital meshse
        self.mesh2 = pv.PolyData(vertices, faces)
        self.mesh2.point_arrays['Concentration'] = np.array(state.conc.elements[:state.nn])

        self.plotter1.clear()
        self.plotter2.clear()
        self.plotter1.add_mesh(self.mesh1, show_edges=True, scalars='Pressure Head')
        self.plotter2.add_mesh(self.mesh2, show_edges=True, scalars='Concentration')
        self.plotter1.reset_camera()
        self.plotter2.reset_camera()

    def updateMesh(self, state):
        self.mesh1.point_arrays['Pressure Head'] = np.array(state.phi[:state.nn])
        self.mesh2.point_arrays['Concentration'] = np.array(state.conc[:state.nn])

    def onCallback(self, state):
        if state.it == 0:
            self.createMesh(state)
        else:
            self.updateMesh(state)

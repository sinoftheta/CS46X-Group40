import pyvista as pv
import numpy as np

from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

from .SimulationController import GS2CallbackListener

class MeshViewController(QGroupBox, GS2CallbackListener):
    def __init__(self, config):
        super(MeshViewController, self).__init__('Mesh')
        self.mesh = None
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
        self.stack.addWidget(self.plotter1)

        # Plotter for Concentration
        self.frame2 = QFrame()
        self.frame2.setFrameStyle(QFrame.StyledPanel | QFrame.Raised)
        self.plotter2 = pv.QtInteractor(self.frame2)
        self.plotter2.show_bounds(grid=True, location='back')
        self.stack.addWidget(self.plotter2)

        # Set up navigation buttons
        self.setMeshPageButtons()
        self.buttons.setAlignment(Qt.AlignLeft)
        self.buttons.setContentsMargins(0, 0, 0, 0)
        self.buttons.setSpacing(20)

        self.setLayout(self.layout)

    def setMeshPageButtons(self):
        # Navigation Button for Pressure Head mesh
        self.pressureButton = QPushButton('Pressure Head')
        self.pressureButton.pressed.connect(lambda index=0: self.stack.setCurrentIndex(index))
        self.buttons.addWidget(self.pressureButton)

        # Navigation Button for Concentration mesh
        self.concentrationButton = QPushButton('Concentration')
        self.concentrationButton.pressed.connect(lambda index=0: self.stack.setCurrentIndex(index))
        self.buttons.addWidget(self.concentrationButton)

    def createMesh(self, state):
        vertices = np.array([[x, y, 0.0] for x, y in zip(state.x, state.y)][:state.nn.value])

        # Numbering scheme
        scheme = [1, 4, 7, 10, 2, 3, 5, 6, 8, 9, 11, 12]

        incidences = np.array(state._in[:state.inc.value]).T
        faces = [sorted(face, key=lambda x:scheme[face.index(x)]) for face in incidences]

        faces = [[inc - 1 for inc in face if inc > 0] for face in faces][:state.ne.value]
        faces = [[len(row)] + row for row in faces]
        faces = np.hstack(faces)

        self.mesh = pv.PolyData(vertices, faces)
        self.mesh.point_arrays['Pressure Head'] = np.array(state.phi[:state.nn.value])
        self.mesh.point_arrays['Concentration'] = np.array(state.conc[:state.nn.value])

        self.plotter1.add_mesh(self.mesh, show_edges=True, scalars='Pressure Head')
        self.plotter2.add_mesh(self.mesh, show_edges=True, scalars='Concentration')

    def updateMesh(self, state):
        self.mesh.point_arrays['Pressure Head'] = np.array(state.phi[:state.nn.value])
        self.mesh.point_arrays['Concentration'] = np.array(state.conc[:state.nn.value])

    def onCallback(self, state):
        if self.mesh is None:
            self.createMesh(state)
        else:
            self.updateMesh(state)

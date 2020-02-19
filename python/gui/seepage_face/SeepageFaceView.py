from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *


class SeepageFaceView(QGroupBox):
    def __init__(self, seepageFaceModel):
        super(SeepageFaceView, self).__init__('Seepage Face' + str(seepageFaceModel.seepageFaceID))

        self.seepageFaceModel = seepageFaceModel


        layout = QVBoxLayout()
        layout.setAlignment(Qt.AlignTop | Qt.AlignLeft)
        layout.setSpacing(20)

        nodesInputLayout = QHBoxLayout()
        nodesInputLayout.setAlignment(Qt.AlignCenter)

        # column for dirichelt nodes
        self.diricheltNodesLayout = QVBoxLayout()
        self.diricheltNodesLayout.setContentsMargins(0, 0, 20, 2)
        self.diricheltNodesLayout.setAlignment(Qt.Center)
        
        self.diricheltNodesHeader = QLabel('Dirichelt Nodes')
        self.diricheltNodesHeader.setFont(QFont('Arial', 13))
        self.diricheltNodesHeader.setAlignment(Qt.AlignLeft)
        self.diricheltNodesLayout.addWidget(self.diricheltNodesHeader)
            
        # column for neumann nodes
        self.nuemannNodesLayout = QVBoxLayout()
        self.nuemannNodesLayout.setContentsMargins(0, 0, 20, 2)
        self.nuemannNodesLayout.setAlignment(Qt.Center)
        
        self.nuemannNodesHeader = QLabel('Nuemann Nodes')
        self.nuemannNodesHeader.setFont(QFont('Arial', 13))
        self.nuemannNodesHeader.setAlignment(Qt.AlignLeft)
        self.nuemannNodesLayout.addWidget(self.nuemannNodesHeader)

        layout.addLayout(self.diricheltNodesLayout)
        layout.addLayout(self.nuemannNodesLayout)

        self.setLayout(layout)

        self.updateView()


    def updateView(self):
        pass
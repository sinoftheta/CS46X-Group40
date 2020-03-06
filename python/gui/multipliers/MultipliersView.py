from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *

#class MultipliersView(QGroupBox):
class MultipliersView():
    def __init__(self, multipliersModel, layout):
        self.layout = layout
        self.model = multipliersModel
        self.setInputs()

    def setInputs(self):

        XFACTLabel = QLabel("X-Coordinate")
        XFACTLabel.setAlignment(Qt.AlignLeft)
        self.XFACT = QDoubleSpinBox()
        self.XFACT.setDecimals(4)
        self.XFACT.setSingleStep(0.0001)
        self.XFACT.setRange(-9999.999, 9999.999)
        self.XFACT.setAlignment(Qt.AlignRight)
        
        self.layout.addWidget(XFACTLabel, 1, 0)
        self.layout.addWidget(self.XFACT, 2, 0)
        self.XFACT.textChanged.connect(lambda val: self.model.setVal("XFACT", val))
        self.XFACT.setValue(self.model.XFACT)

        YFACTLabel = QLabel("Y-Coordinate")
        YFACTLabel.setAlignment(Qt.AlignLeft)
        self.YFACT = QDoubleSpinBox()
        self.YFACT.setDecimals(4)
        self.YFACT.setSingleStep(0.0001)
        self.YFACT.setRange(-9999.999, 9999.999)
        self.YFACT.setAlignment(Qt.AlignRight)
        self.layout.addWidget(YFACTLabel, 1, 1)
        self.layout.addWidget(self.YFACT, 2, 1)
        self.YFACT.textChanged.connect(lambda val: self.model.setVal("YFACT", val))
        self.YFACT.setValue(self.model.YFACT)

        APHIILabel = QLabel("Initial Pressure Head")
        APHIILabel.setAlignment(Qt.AlignLeft)
        self.APHII = QDoubleSpinBox()
        self.APHII.setDecimals(4)
        self.APHII.setSingleStep(0.0001)
        self.APHII.setRange(-9999.999, 9999.999)
        self.APHII.setAlignment(Qt.AlignRight)
        self.layout.addWidget(APHIILabel, 1, 2)
        self.layout.addWidget(self.APHII, 2, 2)
        self.APHII.textChanged.connect(lambda val: self.model.setVal("APHII", val))
        self.APHII.setValue(self.model.APHII)

        ACONCILabel = QLabel("Initial Concentration")
        ACONCILabel.setAlignment(Qt.AlignLeft)
        self.ACONCI = QDoubleSpinBox()
        self.ACONCI.setDecimals(4)
        self.ACONCI.setSingleStep(0.0001)
        self.ACONCI.setRange(-9999.999, 9999.999)
        self.ACONCI.setAlignment(Qt.AlignRight)
        self.layout.addWidget(ACONCILabel, 1, 3)
        self.layout.addWidget(self.ACONCI, 2, 3)
        self.ACONCI.textChanged.connect(lambda val: self.model.setVal("ACONCI", val))
        self.ACONCI.setValue(self.model.ACONCI)

        ATETALabel = QLabel("Saturated Moisture Content")
        ATETALabel.setAlignment(Qt.AlignLeft)
        self.ATETA = QDoubleSpinBox()
        self.ATETA.setDecimals(4)
        self.ATETA.setSingleStep(0.0001)
        self.ATETA.setRange(-9999.999, 9999.999)
        self.ATETA.setAlignment(Qt.AlignRight)
        self.layout.addWidget(ATETALabel, 3, 0)
        self.layout.addWidget(self.ATETA, 4, 0)
        self.ATETA.textChanged.connect(lambda val: self.model.setVal("ATETA", val))
        self.ATETA.setValue(self.model.ATETA)

        AALLabel = QLabel("Medium Compressibility")
        AALLabel.setAlignment(Qt.AlignLeft)
        self.AAL = QDoubleSpinBox()
        self.AAL.setDecimals(4)
        self.AAL.setSingleStep(0.0001)
        self.AAL.setRange(-9999.999, 9999.999)
        self.AAL.setAlignment(Qt.AlignRight)
        self.layout.addWidget(AALLabel, 3, 1)
        self.layout.addWidget(self.AAL, 4, 1)
        self.AAL.textChanged.connect(lambda val: self.model.setVal("AAL", val))
        self.AAL.setValue(self.model.AAL)

        AKDLabel = QLabel("Distribution Coefficient")
        AKDLabel.setAlignment(Qt.AlignLeft)
        self.AKD = QDoubleSpinBox()
        self.AKD.setDecimals(4)
        self.AKD.setSingleStep(0.0001)
        self.AKD.setRange(-9999.999, 9999.999)
        self.AKD.setAlignment(Qt.AlignRight)
        self.layout.addWidget(AKDLabel, 3, 2)
        self.layout.addWidget(self.AKD, 4, 2)
        self.AKD.textChanged.connect(lambda val: self.model.setVal("AKD", val))
        self.AKD.setValue(self.model.AKD)

        ALAMLabel = QLabel("Radioactive Decay Constant")
        ALAMLabel.setAlignment(Qt.AlignLeft)
        self.ALAM = QDoubleSpinBox()
        self.ALAM.setDecimals(4)
        self.ALAM.setSingleStep(0.0001)
        self.ALAM.setRange(-9999.999, 9999.999)
        self.ALAM.setAlignment(Qt.AlignRight)
        self.layout.addWidget(ALAMLabel, 3, 3)
        self.layout.addWidget(self.ALAM, 4, 3)
        self.ALAM.textChanged.connect(lambda val: self.model.setVal("ALAM", val))
        self.ALAM.setValue(self.model.ALAM)

        APORLabel = QLabel("Porosity")
        APORLabel.setAlignment(Qt.AlignLeft)
        self.APOR = QDoubleSpinBox()
        self.APOR.setDecimals(4)
        self.APOR.setSingleStep(0.0001)
        self.APOR.setRange(-9999.999, 9999.999)
        self.APOR.setAlignment(Qt.AlignRight)
        self.layout.addWidget(APORLabel, 5, 0)
        self.layout.addWidget(self.APOR, 6, 0)
        self.APOR.textChanged.connect(lambda val: self.model.setVal("APOR", val))
        self.APOR.setValue(self.model.APOR)

        ARHOLabel = QLabel("Density")
        ARHOLabel.setAlignment(Qt.AlignLeft)
        self.ARHO = QDoubleSpinBox()
        self.ARHO.setDecimals(4)
        self.ARHO.setSingleStep(0.0001)
        self.ARHO.setRange(-9999.999, 9999.999)
        self.ARHO.setAlignment(Qt.AlignRight)
        self.layout.addWidget(ARHOLabel, 5, 1)
        self.layout.addWidget(self.ARHO, 6, 1)
        self.ARHO.textChanged.connect(lambda val: self.model.setVal("ARHO", val))
        self.ARHO.setValue(self.model.ARHO)

        AFMOBLabel = QLabel("Saturated Hydraulic Conductivity")
        AFMOBLabel.setFont(QFont('Helvetica', 14))
        AFMOBLabel.setAlignment(Qt.AlignCenter)
        self.layout.addWidget(AFMOBLabel, 8, 0)
        self.layout.setRowMinimumHeight(7, 30)
        AFMOBXLabel = QLabel("Horizontal")
        AFMOBXLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(AFMOBXLabel, 9, 0)
        AFMOBYLabel = QLabel("Vertical")
        AFMOBYLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(AFMOBYLabel, 9, 1)
        self.AFMOBX = QDoubleSpinBox()
        self.AFMOBX.setDecimals(4)
        self.AFMOBX.setSingleStep(0.0001)
        self.AFMOBX.setRange(-9999.999, 9999.999)
        self.AFMOBX.setAlignment(Qt.AlignRight)
        self.AFMOBY = QDoubleSpinBox()
        self.AFMOBY.setDecimals(4)
        self.AFMOBY.setSingleStep(0.0001)
        self.AFMOBY.setRange(-9999.999, 9999.999)
        self.AFMOBY.setAlignment(Qt.AlignRight)
        self.layout.addWidget(self.AFMOBX, 10, 0)
        self.layout.addWidget(self.AFMOBY, 10, 1)
        self.AFMOBY.textChanged.connect(lambda val: self.model.setVal("AFMOBY", val))
        self.AFMOBY.setValue(self.model.AFMOBY)
        self.AFMOBX.textChanged.connect(lambda val: self.model.setVal("AFMOBX", val))
        self.AFMOBX.setValue(self.model.AFMOBX)

        AELabel = QLabel("Dispersivity")
        AELabel.setFont(QFont('Helvetica', 14))
        AELabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(AELabel, 8, 2)
        AETRANSLabel = QLabel("Transverse")
        AETRANSLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(AETRANSLabel, 9, 2)
        AELONGLabel = QLabel("Longitudinal")
        AELONGLabel.setAlignment(Qt.AlignLeft)
        self.layout.addWidget(AELONGLabel, 9, 3)
        self.AETRANS = QDoubleSpinBox()
        self.AETRANS.setDecimals(4)
        self.AETRANS.setSingleStep(0.0001)
        self.AETRANS.setRange(-9999.999, 9999.999)
        self.AETRANS.setAlignment(Qt.AlignRight)
        self.AELONG = QDoubleSpinBox()
        self.AELONG.setDecimals(4)
        self.AELONG.setSingleStep(0.0001)
        self.AELONG.setRange(-9999.999, 9999.999)
        self.AELONG.setAlignment(Qt.AlignRight)
        self.layout.addWidget(self.AETRANS, 10, 2)
        self.layout.addWidget(self.AELONG, 10, 3)
        self.AETRANS.textChanged.connect(lambda val: self.model.setVal("AETRANS", val))
        self.AETRANS.setValue(self.model.AETRANS)
        self.AELONG.textChanged.connect(lambda val: self.model.setVal("AELONG", val))
        self.AELONG.setValue(self.model.AELONG)
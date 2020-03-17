import csv

from gui.simulation.SimulationModel import GS2KOD

from gui.simulation import SimulationModel
from gui.elements import ElementModel
from gui.parameters import ParametersModel
from gui.material import MaterialModel
from gui.multipliers import MultipliersModel
from gui.seepage_face import SeepageFaceModel
from gui.element_properties import ElementPropertiesModel
from gui.nodes import NodeModel
from gui.node_types import MixedBCModel, VariableBCModel, SourceSinkModel


class FileReader:
    def __init__(self):
        # this class should the same data members 
        # as FileWriter I.E. various models 
        
        self.simulationModel = SimulationModel()
        self.parametersModel = ParametersModel()
        self.multipliersModel = MultipliersModel()

        # dictionary of element models keyed off
        # of the element number
        self.elementModels = {}

        # dictionary keyed off of materialID
        self.materialModels = {}

        self.seepageFaces = []

        self.elementPropertiesModels = {}

        self.nodeModels = {}

        self.sourceSinkModels = {}
        self.mixedBCModels = {}
        self.variableBCModels = {}

        self.csvRows = []

    def read(self, filepath):
        with open(filepath, 'r', newline='') as csvfile:
            reader = csv.reader(
                csvfile,
                delimiter=',',
                quotechar='|'
            )

            # set data input file here 
            # so it shows up in the sim page
            self.simulationModel.dataInputFile = filepath
            
            self.csvRows = [row for row in reader]

            # for the readGroup functions it is important to modify csvRows
            # each readGroup should check the first entry of the first row to check
            # that it is operating on the correct row

            # TODO: probably create somesort of dispatch like in the c code

            self._readGroupA()
            self._readGroupB()
            self._readGroupC()
            self._readGroupD()
            self._readGroupE()
            self._readGroupF()
            self._readGroupG()
            self._readGroupH()
            self._readGroupI()
            self._readGroupJ()
            self._readGroupK()
            self._readGroupL()
            self._readGroupM()
            self._readGroupN()
            self._readGroupO()
            self._readGroupP()
            self._readGroupQ()
            self._readGroupR()
        
    def _readGroupA(self):
        if self.csvRows[0][0] != "A":
            return
        
        row = self.csvRows.pop(0)
        self.simulationModel.setSimulationTitle(row[1])


    def _readGroupB(self):
        if self.csvRows[0][0] != "B":
            return

        card1 = self.csvRows.pop(0)
        card2 = self.csvRows.pop(0)
        card3 = self.csvRows.pop(0)
        card4 = self.csvRows.pop(0)

        # remove group label
        card1.pop(0)
        card2.pop(0)
        card3.pop(0)
        card4.pop(0)
        
        # read card 1
       
        self.parametersModel.NN.setData(int(card1[0]))
        self.parametersModel.NE.setData(int(card1[1]))
        ns = card1[2]
        kns = card1[3]
        self.parametersModel.NB = int(card1[4])
        self.parametersModel.KNB = int(card1[5])
        nf = int(card1[6])
        inc = int(card1[7])
        self.parametersModel.NK.setData(int(card1[8]))
        self.parametersModel.NSEEP.setData(int(card1[9]))

        # read card 2
        nsdn = int(card2[0])
        mq4 = int(card2[1])
        knsdn = int(card2[2])
        self.parametersModel.PL = float(card2[3])
        coefi = float(card2[4])
        self.parametersModel.EI = float(card2[5])
        nvs = int(card2[6])

        # read card 3
        self.parametersModel.DELT = float(card3[0])
        self.parametersModel.CHNG = float(card3[1])
        self.parametersModel.ITMAX = int(card3[2])
        self.parametersModel.ITCHNG = int(card3[3])
        self.parametersModel.PCHNG = float(card3[4])
        self.parametersModel.BETAP = float(card3[5])
        if str(card3[6]) == "BACK":
            self.parametersModel.TYPE = "Implicit"
        else:
            self.parametersModel.TYPE = "Centered"

        # read card 4
        self.parametersModel.DIFUSN = float(card4[0])
        dprdt = float(card4[1])
        statNumeric = str(card4[2])
        if statNumeric == "-1.0":
            self.parametersModel.STAT = "Flow equation only"
        elif statNumeric == "0.0":
            self.parametersModel.STAT = "Steady-state"
        else:
            self.parametersModel.STAT = "Transient"
        
        statpNumeric = str(card4[3])
        if statpNumeric == "0.0":
            self.parametersModel.STATP = "Steady-state"
        else:
            self.parametersModel.STATP = "Transient"

        self.parametersModel.CLOS1 = float(card4[4])
        self.parametersModel.ITER1 = int(card4[5])
        self.parametersModel.IGO = int(card4[6])

    def _readGroupC(self):
        if self.csvRows[0][0] != "C":
            return
        
        card1 = self.csvRows.pop(0)
        card2 = self.csvRows.pop(0)

        # remove group labels
        card1.pop(0)
        card2.pop(0)

        # all values are expected to be floats
        card1 = list(map(lambda elem: float(elem), card1[0:8]))
        card2 = list(map(lambda elem: float(elem), card2[0:6]))

        self.multipliersModel.AFMOBX = card1[0]
        self.multipliersModel.AFMOBY = card1[1]
        self.multipliersModel.APOR = card1[2]
        self.multipliersModel.AELONG = card1[3]
        self.multipliersModel.AETRANS = card1[4]
        self.multipliersModel.APHII = card1[5]
        self.multipliersModel.ACONCI = card1[6]
        self.multipliersModel.XFACT = card1[7]

        self.multipliersModel.YFACT = card2[0]
        self.multipliersModel.ATETA = card2[1]
        self.multipliersModel.AAL = card2[2]
        self.multipliersModel.AKD = card2[3]
        self.multipliersModel.ALAM = card2[4]
        self.multipliersModel.ARHO = card2[5]

    def _readGroupD(self):
        if self.csvRows[0][0] != "D":
            return 

        card1 = self.csvRows.pop(0)
        
        # remove label
        card1.pop(0)

        # data in this group are ints
        card1 = list(map(lambda elem: int(elem), card1[0:11]))


        self.simulationModel.setOutputModifier(GS2KOD.KOD1, card1[0])
        self.simulationModel.setOutputModifier(GS2KOD.KOD2, card1[1])
        self.simulationModel.setOutputModifier(GS2KOD.KOD3, card1[2])
        self.simulationModel.setOutputModifier(GS2KOD.KOD4, card1[3])
        self.simulationModel.setOutputModifier(GS2KOD.KOD7, card1[4])
        self.simulationModel.setOutputModifier(GS2KOD.KOD8, card1[5])
        self.simulationModel.setOutputModifier(GS2KOD.KOD9, card1[6])
        self.simulationModel.setOutputModifier(GS2KOD.KOD10, card1[7])
        self.simulationModel.setOutputModifier(GS2KOD.KOD11, card1[8])
        self.simulationModel.setOutputModifier(GS2KOD.KOD12, card1[9])

            

    def _readGroupE(self):
        if self.csvRows[0][0] != "E":
            return

        while self.csvRows[0][0] == "E":
            card = self.csvRows.pop(0)
            card.pop(0)


            nodeModel = NodeModel(int(card[0]))
            nodeModel.X = float(card[1])
            nodeModel.Y = float(card[2])

            self.nodeModels[card[0]] = nodeModel

    

    def _readGroupF(self):
        if self.csvRows[0][0] != "F-1":
            return
        
        while self.csvRows[0][0] == "F-1":
            card = self.csvRows.pop(0)
            card.pop(0)

            while len(card) and card[0] != '':
                nodeNum = card.pop(0)
                fq = float(card.pop(0))

                self.sourceSinkModels[nodeNum] = SourceSinkModel(nodeNum)
                self.sourceSinkModels[nodeNum].FQ = fq

        while self.csvRows[0][0] == "F-2":
            card = self.csvRows.pop(0)
            card.pop(0)

            while len(card) and card[0] != '':
                nodeNum = card.pop(0)
                qfq = float(card.pop(0))

                self.sourceSinkModels[nodeNum].QFQ = qfq


    def _readGroupG(self):
        if self.csvRows[0][0] != "G-1":
            return

        # don't care about g1 for GUI
        g1Card = self.csvRows.pop(0)

        while self.csvRows[0][0] == "G-2":
            g2Card = self.csvRows.pop(0)
            g2Card.pop(0)

            while g2Card[0] != '':
                nodeNum = g2Card.pop(0)
                conci = float(g2Card.pop(0))

                self.nodeModels[nodeNum].CONCI = conci


    def _readGroupH(self):
        if self.csvRows[0][0] != "H-1":
            return 

        # don't card about h1 for GUI
        h1Card = self.csvRows.pop(0)

        h2Card = self.csvRows.pop(0)
        h2Card.pop(0)

        hone = float(h2Card[0])

        if hone != 9999.0:
            for key in self.nodeModels:
                self.nodeModels[key].PHII = hone - self.nodeModels[key].Y

        while self.csvRows[0][0] == "H-3":
            h3Card = self.csvRows.pop(0)
            h3Card.pop(0)

            while h3Card[0] != '':
                nodeNum = h3Card.pop(0)
                phii = float(h3Card.pop(0))

                self.nodeModels[nodeNum].PHII = phii


    def _readGroupI(self):
        if self.csvRows[0][0] != "I":
            return

        # Emulate Do While

        while True:
            
            card = self.csvRows.pop(0)
            card.pop(0)
            #card = list(map(lambda elem: int(elem), card[1:14]))

            element = ElementModel(card[0])
            
            for i in range(len(card) - 1):
                if card[i+1] == '':
                    break
                element.incidences[i] = int(card[i + 1])

            self.elementModels[element.elementNumber] = element

            if self.csvRows[0][0] != "I":
                break
         
    def _readGroupJ(self):
        if self.csvRows[0][0] != "J-1":
            return
            
        # formatted 
        # j-1 card
        # j-2 card
        # j-1 card
        # ...
        while self.csvRows[0][0][0] == "J":
            j1Card = self.csvRows.pop(0)
            j2Card1 = self.csvRows.pop(0)
            j2Card2 = self.csvRows.pop(0)

            j1Card.pop(0)

            j2Card1 = list(map(lambda elem: float(elem), j2Card1[1:9]))
            j2Card2 = list(map(lambda elem: float(elem), j2Card2[1:3]))

            lowerElementBound = int(j1Card[0])
            upperElementBound = int(j1Card[1])

            materialGroup = int(j1Card[2])

            elementPropertiesModel = ElementPropertiesModel(materialGroup)

            for (key, element) in self.elementModels.items():
                if element.elementNumber in range(lowerElementBound, upperElementBound+1):
                    self.elementModels[key].materialGroup = materialGroup

            elementPropertiesModel.FMOBX = j2Card1[0]
            elementPropertiesModel.FMOBY = j2Card1[1]
            elementPropertiesModel.ELONG = j2Card1[2]
            elementPropertiesModel.ETRANS = j2Card1[3]
            elementPropertiesModel.POR = j2Card1[4]
            elementPropertiesModel.TTA = j2Card1[5]
            elementPropertiesModel.ALPHA = j2Card1[6]
            elementPropertiesModel.KD = j2Card1[7]

            elementPropertiesModel.LAMBDA = j2Card2[0]
            elementPropertiesModel.RHO = j2Card2[1]

            self.elementPropertiesModels[materialGroup] = elementPropertiesModel

            

    def _readGroupK(self):
        if self.csvRows[0][0] != "K":
            return
        while self.csvRows[0][0] == "K":
            card = self.csvRows.pop(0)
            card.pop(0)

            while card[0] != '':
                dirichletNode = card.pop(0)

                self.nodeModels[dirichletNode].boundary.setData("Constant Head (Dirichlet)")


    def _readGroupL(self):
        if self.csvRows[0][0] != "L":
            return

        while self.csvRows[0][0] == "L":
            card = self.csvRows.pop(0)
            card.pop(0)

            while card[0] != '':
                dirichletNode = card.pop(0)

                self.nodeModels[dirichletNode].boundary.setData("Constant Concentration (Dirichlet)")

    def _readGroupM(self):
        if self.csvRows[0][0] != "M-1":
            return

        while self.csvRows[0][0] == "M-1":
            card = self.csvRows.pop(0)
            card.pop(0)

            while len(card) and card[0] != '':
                nodeNum = card.pop(0)
                self.variableBCModels[nodeNum] = VariableBCModel(nodeNum)
                self.variableBCModels[nodeNum].dirichlet = True

        while self.csvRows[0][0] == "M-2":
            card = self.csvRows.pop(0)
            card.pop(0)

            while len(card) and card[0] != '':
                nodeNum = card.pop(0)

                self.variableBCModels[nodeNum] = VariableBCModel(nodeNum)
                self.variableBCModels[nodeNum].nuemann = True

        while self.csvRows[0][0] == "M-3":
            card = self.csvRows.pop(0)
            card.pop(0)

            while len(card) and card[0] != '':
                nodeNum = card.pop(0)
                coef = float(card.pop(0))

                self.variableBCModels[nodeNum].COEF = coef

        while self.csvRows[0][0] == "M-4":
            card = self.csvRows.pop(0)
            card.pop(0)

            while len(card) and card[0] != '':
                nodeNum = card.pop(0)
                vn = float(card.pop(0))

                self.variableBCModels[nodeNum].VN = vn
            

    def _readGroupN(self):
        if self.csvRows[0][0] != "N-1":
            return


        # might be able to `skip` this section
        # that is pop until N2
        while self.csvRows[0][0] == "N-1":
            card = self.csvRows.pop(0)
            card.pop(0)

            while len(card) and card[0] != '':
                nodeNum = card.pop(0)
                self.mixedBCModels[nodeNum] = MixedBCModel(nodeNum)

        while self.csvRows[0][0] == "N-2":
            card = self.csvRows.pop(0)
            card.pop(0)

            while len(card) and card[0] != '':
                nodeNum = card.pop(0)
                cn = float(card.pop(0))

                self.mixedBCModels[nodeNum].CN = cn

    def _readGroupO(self):
        if self.csvRows[0][0] != "O-1":
            return

        seepageFaceNumber = 0

        # format for group O is
        # Cards for Group O-1
        # Cards for Group O-2
        # Cards for Group O-3
        # Cards for Group O-1
        # ...
        while self.csvRows[0][0][0] == "O":
            seepageFaceNumber += 1
            
            # handle sub group 1
            o1Card = self.csvRows.pop(0)
            o1Card.pop(0)

            numberOfNodesOnFace = int(o1Card[0])
            numberOfDiricheltNodes = int(o1Card[1])

            seepageFace = SeepageFaceModel(seepageFaceNumber)
            seepageFace.setNumberOfDiricheltNodes(numberOfDiricheltNodes)
            seepageFace.setNumberOfNuemannNodes(numberOfNodesOnFace - numberOfDiricheltNodes)

            # handle sub group 2
            while self.csvRows[0][0] == "O-2":
                o2Card = self.csvRows.pop(0)
                o2Card.pop(0)

                for i in range(len(o2Card)):
                    if o2Card[i] == '':
                        break
                    seepageFace.setDiricheltNode(i, int(o2Card[i]))

            # handle sub group 3
            while self.csvRows[0][0] == "O-2":
                o3Card = self.csvRows.pop(0)
                o3Card.pop(0)

                for i in range(len(o3Card)):
                    if o3Card[i] == '':
                        break
                    seepageFace.setNuemannNode(i, int(o3Card[i]))

            self.seepageFaces.append(seepageFace)
            
        

    def _readGroupP(self):
       # skip through P
       while self.csvRows[0][0] == "P":
           self.csvRows.pop(0)

    def _readGroupQ(self):
             
        if self.csvRows[0][0] != "Q-1":
            return


        # sub group Q1
        q1Card = self.csvRows.pop(0)
        q1Card.pop(0)

        for i in range(len(q1Card)):
            if q1Card[i] == '':
                break

            materialNumber = str(i+1)
            material = MaterialModel(materialNumber)
            material.setInterpolationPointCount(int(q1Card[0]))
            self.materialModels[materialNumber] = material


        for materialNumber in self.materialModels:
            
            pressureHeadIndex = 0
            while self.csvRows[0][0] == "Q-2":
                q2Card = self.csvRows.pop(0)
                q2Card.pop(0)
                
                while len(q2Card) and q2Card[0] != '':
                    self.materialModels[materialNumber].pressureHead[pressureHeadIndex] = float(q2Card.pop(0))
                    pressureHeadIndex += 1

            moistureContentIndex = 0
            while self.csvRows[0][0] == "Q-3":
                q3Card = self.csvRows.pop(0)
                q3Card.pop(0)
               
                while len(q3Card) and q3Card[0] != '':
                    self.materialModels[materialNumber].moistureContent[moistureContentIndex] = float(q3Card.pop(0))
                    moistureContentIndex += 1

            # include check for last csvrow, as Q4 will often be the final card
            conductivityIndex = 0
            while len(self.csvRows) and self.csvRows[0][0] == "Q-4":
                q4Card = self.csvRows.pop(0)
                q4Card.pop(0)

                while len(q4Card) and q4Card[0] != '':
                    self.materialModels[materialNumber].hydraulicConductivity[conductivityIndex] = float(q4Card.pop(0))
                    conductivityIndex += 1
            
        
    def _readGroupR(self):
        pass

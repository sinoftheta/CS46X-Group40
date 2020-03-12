import csv

from gui.simulation.SimulationModel import GS2KOD

from gui.simulation import SimulationModel
from gui.elements import ElementModel
from gui.parameters import ParametersModel

class FileReader:
    def __init__(self):
        # this class should the same data members 
        # as FileWriter I.E. various models 
        
        self.simulationModel = SimulationModel()
        self.parametersModel = ParametersModel()

        # dictionary of element models keyed off
        # of the element number
        self.elementModels = {}

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

            self._readGroupI()
        
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
      pass

    def _readGroupD(self):
        pass

    def _readGroupE(self):
        pass

    def _readGroupF(self):
        pass

    def _readGroupG(self):
        pass

    def _readGroupH(self):
        pass

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
        pass

    def _readGroupK(self):
        pass

    def _readGroupL(self):
        pass

    def _readGroupM(self):
        pass

    def _readGroupN(self):
        pass

    def _readGroupO(self):
        pass

    def _readGroupP(self):
        pass

    def _readGroupQ(self):
        pass

    def _readGroupR(self):
        pass

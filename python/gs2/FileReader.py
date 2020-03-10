import csv

from gui.simulation.SimulationModel import GS2KOD

from gui.simulation import SimulationModel
from gui.parameters import ParametersModel

class FileReader:
    def __init__(self):
        # this class should the same data members 
        # as FileWriter I.E. various models 
        
        self.simulationModel = SimulationModel()
        self.parametersModel = ParametersModel()

        self.csvRows = []

    def read(self, filepath):
        with open(filepath, 'r', newline='') as csvfile:
            reader = csv.reader(
                csvfile,
                delimiter=',',
                quotechar='|'
            )

            self.csvRows = [row for row in reader]

            # for the readGroup functions it is important to modify csvRows
            # each readGroup should check the first entry of the first row to check
            # that it is operating on the correct row
            self._readGroupA()
        
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
        card1.pop()
        card2.pop()
        card3.pop()
        card4.pop()

        # read card 1
        card1 = list(map(card1, lambda elem: int(elem)))

        self.parametersModel.NN.setData(card1[0])
        self.parametersModel.NE.setData(card1[1])

        ns = card1[2]
        kns = card1[3]
        

        # read card 2

        # read card 3

        # read card 4
        

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
        pass

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

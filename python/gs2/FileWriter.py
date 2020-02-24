import csv

from gui.simulation.SimulationModel import GS2KOD

class FileWriter:
    def __init__(self, materialModels, simulationModel, seepageFaceModels, basicParametersModel):
        self.materialModels = materialModels
        self.simulationModel = simulationModel
        self.seepageFaceModels = seepageFaceModels
        self.basicParametersModel = basicParametersModel

    def write(self, filepath):

        with open(filepath, 'w', newline='') as csvfile:
            writer = csv.writer(
                csvfile,
                delimiter=',',
                quotechar='|', #unused, I think
                quoting=csv.QUOTE_MINIMAL #also unused
            )

            # write groups
            self._writeGroupA(writer, self.simulationModel)
            self._writeGroupB(writer, self.basicParametersModel)
            self._writeGroupD(writer, self.simulationModel)
            self._writeGroupO(writer, self.seepageFaceModels)
            self._writeGroupQ(writer, self.materialModels)

    def _csvPad(self, cols):
        # 20 data points plus group
        maxCols = 21
        while len(cols) < maxCols:
            cols.append('')
        return cols

    def _writeGroupQ(self, csv, materials):

        # sub group Q-1
        group = "Q-1"
        csvRow = [group]
        for mat in materials:
            csvRow.append(mat.getInterpolationPointCount())

            # allows 20 values in this group
            if len(csvRow) == 21:
                csv.writerow(csvRow)
                csvRow = [group]

        # make sure the last row actually has meaningful data
        if len(csvRow) > 1:
            csv.writerow(self._csvPad(csvRow))


        groups = ["Q-2", "Q-3", "Q-4"]
        for mat in materials:
            matData = [mat.pressureHead, mat.moistureContent, mat.hydraulicConductivity]
            for i in range(3):
                csvRow = [groups[i]]
                for elem in matData[i]:
                    csvRow.append(elem)
                    # these groups contains at most 8 data points plus the group
                    if len(csvRow) == 9:
                        csv.writerow(self._csvPad(csvRow))
                        csvRow = [groups[i]]

                if len(csvRow) > 1:
                    csv.writerow(self._csvPad(csvRow))


    def _writeGroupA(self, csv, simulation):
        group = "A"
        csvRow = [group, simulation.simulationTitle]
        csv.writerow(self._csvPad(csvRow))

    def _writeGroupB(self, csv, basicParameters):
        group = "B"

        TYPE = 'BACK' if basicParameters.TYPE == "Implicit" else 'CENT'
        STATP = '1.0' if basicParameters.STATP == "Transient" else '0.0'

        if basicParameters.STAT == "Flow equation only":
            STAT = '-1.0'
        elif basicParameters.STAT == "Steady-state":
            STAT = '0.0'
        else:
            STAT = '1.0'

        # derived parameters
        NS = ''
        KNS = ''
        NF = ''
        INC = ''
        NSDN = ''
        MQ4 = ''
        KNSDN = ''
        COEFI = ''
        NVS = ''
        DPRDT = '0'

        csvRow = [
                group,
                basicParameters.NN.getData(),
                basicParameters.NE.getData(),
                NS,
                KNS,
                basicParameters.NB,
                basicParameters.KNB,
                NF,
                INC,
                basicParameters.NK.getData(),
                basicParameters.NSEEP.getData()
        ]

        csvRow2 = [
                group,
                NSDN,
                MQ4,
                KNSDN,
                basicParameters.PL,
                COEFI,
                basicParameters.EI,
                NVS
        ]

        csvRow3 = [
                group,
                basicParameters.DELT,
                basicParameters.CHNG,
                basicParameters.ITMAX,
                basicParameters.ITCHNG,
                basicParameters.PCHNG,
                basicParameters.BETAP,
                TYPE
        ]

        csvRow4 = [
                group,
                DPRDT,
                STAT,
                STATP,
                basicParameters.CLOS1,
                basicParameters.ITER1,
                basicParameters.IGO
        ]

        csv.writerow(self._csvPad(csvRow))
        csv.writerow(self._csvPad(csvRow2))
        csv.writerow(self._csvPad(csvRow3))
        csv.writerow(self._csvPad(csvRow4))


    def _writeGroupD(self, csv, simulation):
        group = "D"
        csvRow = [group]

        # order in simulation KOD array is the same as
        # whats expected by GS2

        for kod in GS2KOD:
            csvRow.append(simulation.getOutputModifier(kod))

        csv.writerow(self._csvPad(csvRow))

    def _writeGroupO(self, csv, seepageFaces):
        for seepageFace in seepageFaces:
            group = "O-1"
            totalNodes = seepageFace.getNumberOfDiricheltNodes() + seepageFace.getNumberOfNuemannNodes()
            csvRow = [group,  totalNodes, seepageFace.getNumberOfDiricheltNodes()]

            csv.writerow(self._csvPad(csvRow))

            groups = ["O-2", "O-3"]
            nodeLists = [seepageFace.diricheltNodes, seepageFace.nuemannNodes]
            
            for x in range(len(groups)):
                csvRow = [groups[x]]
                for node in nodeLists[x]:
                    csvRow.append(node)

                    # group + 20 nodes
                    if len(csvRow) == 21:
                        csv.writerow(csvRow)
                        csvRow = [groups[x]]

                if len(csvRow) > 1:
                    csv.writerow(self._csvPad(csvRow))
            


            



        
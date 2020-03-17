import csv

from gui.simulation.SimulationModel import GS2KOD

class FileWriter:
    def __init__(self,
            materialModels,
            simulationModel,
            seepageFaceModels,
            basicParametersModel,
            elementModels,
            multipliersModel,
            elementPropertiesModels,
            nodeTypesModels):
        self.materialModels = materialModels
        self.simulationModel = simulationModel
        self.seepageFaceModels = seepageFaceModels
        self.basicParametersModel = basicParametersModel
        self.elementModels = elementModels
        self.multipliersModel = multipliersModel
        self.elementPropertiesModels = elementPropertiesModels
        self.nodeTypesModels = nodeTypesModels

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
            self._writeGroupC(writer, self.multipliersModel)
            self._writeGroupD(writer, self.simulationModel)
            self._writeGroupI(writer, self.elementModels)
            self._writeGroupJ(writer, self.elementPropertiesModels)
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
        # assume zeros for the time being; makes testing import do able
        NS = '0'
        KNS = '0'
        NF = '0'
        INC = '0'
        NSDN = '0'
        MQ4 = '0'
        KNSDN = '0'
        COEFI = '0'
        NVS = '0'
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
                basicParameters.DIFUSN,
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

    def _writeGroupC(self, csv, model):
        group = "C"

        csvRow1 = [
            group,
            model.AFMOBX,
            model.AFMOBY,
            model.APOR,
            model.AELONG,
            model.AETRANS,
            model.APHII,
            model.ACONCI,
            model.XFACT
        ]

        csvRow2 = [
            group,
            model.YFACT,
            model.ATETA,
            model.AAL,
            model.AKD,
            model.ALAM,
            model.ARHO
        ]

        csv.writerow(self._csvPad(csvRow1))
        csv.writerow(self._csvPad(csvRow2))

    def _writeGroupD(self, csv, simulation):
        group = "D"
        csvRow = [group]

        # order in simulation KOD array is the same as
        # whats expected by GS2

        for kod in GS2KOD:
            csvRow.append(simulation.getOutputModifier(kod))

        csv.writerow(self._csvPad(csvRow))

    def _writeGroupI(self, csv, elementModels):
        group = "I"
        for element in elementModels:
            csvRow = [ group, element.elementNumber ]

            for node in element.getIncidences():
                csvRow.append(node)

            csv.writerow(self._csvPad(csvRow))

    def _writeGroupJ(self, csv, elementPropertiesModels):
        materialGroups = {}
        for group in range(1, (self.basicParametersModel.NK.getData()+1)):
            materialGroups[str(group)] = []
        for element in self.elementModels:
            materialGroups[str(element.materialGroup)].append(element.elementNumber)

        for materialGroup in elementPropertiesModels:
            group = "J-1"
            csvRow = [
                group, materialGroups[materialGroup.materialGroupId][0],
                materialGroups[materialGroup.materialGroupId][-1],
                materialGroup.materialGroupId
            ]
            csv.writerow(self._csvPad(csvRow))
            group = "J-2"
            csvRow = [
                group,
                materialGroup.FMOBX,
                materialGroup.FMOBY,
                materialGroup.ELONG,
                materialGroup.ETRANS,
                materialGroup.POR,
                materialGroup.TTA,
                materialGroup.ALPHA,
                materialGroup.KD,
            ]
            csv.writerow(self._csvPad(csvRow))
            csvRow = [
                group,
                materialGroup.LAMBDA,
                materialGroup.RHO,
            ]
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

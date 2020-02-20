import csv

class FileWriter:
    def __init__(self, materialModels):
        self.materialModels = materialModels


    def write(self, filepath):

        with open(filepath, 'w') as csvfile:
            writer = csv.writer(
                csvfile, 
                delimiter=',',
                quotechar='|', #unused, I think
                quoting=csv.QUOTE_MINIMAL #also unused 
            ) 

            # write groups
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


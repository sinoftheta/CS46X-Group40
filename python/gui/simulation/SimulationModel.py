
from os import path

from aenum import Enum, NoAlias

# value is highest value for specific kod
# -1 indicates > 1
class GS2KOD(Enum):

    _settings_ = NoAlias

    KOD1 = 1
    KOD2 = 2
    KOD3 = 1
    KOD4 = 2
    KOD7 = 2
    KOD8 = 2
    KOD9 = -1
    KOD10 = -1
    KOD11 = -1
    KOD12 = 1

class SimulationModel:
    def __init__(self, config=None):
        self.simulationTitle = ""

        # File descriptors for gs2
        # stdin likely isn't important to keep
        self.stdout = ""
        self.stderr = ""
        self.stdin = ""

        # which file to read into gs2
        self.dataInputFile = ""

        # this dict corresponds to the KODx variables in gs2
        self.outputModifiers = {}

        for kod in GS2KOD:
            self.outputModifiers[kod] = 0
            if kod.value < 0:
                self.outputModifiers[kod] = 1

        if config != None:
            if config['io']['default-in']:
                self.stdin = path.abspath(path.join(config['paths']['bundle'], config['io']['default-in']))

            if config['io']['default-out']:
                self.stdout = path.abspath(path.join(config['paths']['bundle'], config['io']['default-out']))

            if config['io']['default-err']:
                self.stderr = path.abspath(path.join(config['paths']['bundle'], config['io']['default-err']))

            if config['paths']['data-in']:
                self.dataInputFile = path.abspath(path.join(config['paths']['bundle'], config['paths']['data-in']))


    def setOutputModifier(self, kod, state):
        self.outputModifiers[kod] = state

    def getOutputModifier(self, kod):
        return self.outputModifiers[kod]

    def setSimulationTitle(self, title):
        self.simulationTitle = title

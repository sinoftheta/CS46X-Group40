class ParametersModel:
    def __init__(self):
        self.NN = LiveData(0)
        self.NE = LiveData(0)
        self.NK = LiveData(0)
        self.NB = 0
        self.KNB = 0
        self.PL = 0.000
        self.EI = 0.000
        self.PCHNG = 0.000
        self.BETAP = 0.000
        self.DIFUSN = 0.000
        self.CLOS1 = 0.000
        self.DELT = 0.000
        self.CHNG = 0.000
        self.ITMAX = 0
        self.ITER1 = 0
        self.ITCHNG = 0
        self.IGO = 0

        self.TYPE = "Implicit"
        self.STAT = "Steady-state"
        self.STATP = "Steady-state"


class LiveData:
    def __init__(self, initData):
        self._data = initData

        self._observers = []

    def setData(self, newData):
        self._data = newData
        self.notifyChange(self._data)

    def getData(self):
        return self._data

    def connectObserver(self, observer):
        if observer not in self._observers:
            self._observers.append(observer)

    def notifyChange(self, newData):
        for observer in self._observers:
            observer(newData)

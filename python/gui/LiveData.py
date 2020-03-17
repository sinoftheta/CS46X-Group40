class LiveData:
    def __init__(self, initData):
        self._data = initData

        self._observers = []

    def setData(self, newData):
        self._data = newData
        self._notifyChange(self._data)

    def getData(self):
        return self._data

    def connectObserver(self, observer):
        if observer not in self._observers:
            self._observers.append(observer)

    def _notifyChange(self, newData):
        for observer in self._observers:
            observer(newData)

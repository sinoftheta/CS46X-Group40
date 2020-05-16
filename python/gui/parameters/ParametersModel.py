from ..LiveData import LiveData

class ParametersModel:
    def __init__(self):
        # Number of nodal points
        self.NN = LiveData(0)
        # Number of elements
        self.NE = LiveData(0)
        # Number of material types (groups)
        self.NK = LiveData(0)
        # Number of seepage faces
        self.NSEEP = LiveData(0)
        # Estimated half bandwidth for flow matrices
        self.NB = 0
        # Estimated half bandwidth for mass transport matrices
        self.KNB = 0
        # Minimum pressure head allowed at soil surface
        self.PL = 0.000
        # Maximum infiltration rate along infiltration boundaries
        self.EI = 0.000
        # Pressure change criterion
        self.PCHNG = 0.000
        # Modified coefficient of fluid compressibility
        self.BETAP = 0.000
        # Molecular diffusion constant
        self.DIFUSN = 0.000
        # Convergence criteria for iteration
        self.CLOS1 = 0.000
        # Initial time step (hours)
        self.DELT = 0.000
        # Multiplier for changing the time step
        self.CHNG = 0.000
        # Maximum permitted number of time steps
        self.ITMAX = 0
        # Maximum number of iterations per time step
        self.ITER1 = 0
        # Number of time steps between changes in DELT
        self.ITCHNG = 0
        # Number of solutions of the mass transport equations between
        #   each solution of the flow equations
        self.IGO = 0

        # TODO: Refactor to enums
        self.TYPE = "Implicit"
        self.STAT = "Steady-state"
        self.STATP = "Steady-state"

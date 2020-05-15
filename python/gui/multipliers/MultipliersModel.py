class MultipliersModel:
    """
        Contains multipliers for varius input parameters
    """
    def __init__(self):
        # Horizontal component of saturated hydraulic conductivity
        self.AFMOBX = 1.0
        # Vertical component of saturated hydraulic conductivity
        self.AFMOBY = 1.0
        # Porosity
        self.APOR = 1.0
        # Longitudinal dispersivity
        self.AELONG = 1.0
        # Transverse dispersivity
        self.AETRANS = 1.0
        # Initial pressure head
        self.APHII = 1.0
        # Initial concentration
        self.ACONCI = 1.0
        # x-coordinate (horizontal direction)
        self.XFACT = 1.0
        # y-coordinate (vertical direction)
        self.YFACT = 1.0
        # Saturated moisture content
        self.ATETA = 1.0
        # Medium compressibility
        self.AAL = 1.0
        # Distribution coefficient
        self.AKD = 1.0
        # Radioactive decay constant
        self.ALAM = 1.0
        # Density
        self.ARHO = 1.0

    #generic mutator
    def setVal(self, key, val):
        if hasattr(self, key):
            setattr(self, key, val)

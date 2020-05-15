class ElementPropertiesModel:
    """
        Contains attribute values for a specific material group describing
        properties of each element belonging to that material group.
    """
    def __init__(self, materialGroupNum):
        # Material group ID number
        self.materialGroupId = materialGroupNum
        # Horizontal component of saturated hydraulic conductivity
        self.FMOBX = 0.00000
        # Vertical component of saturated hydraulic conductivity
        self.FMOBY = 0.00000
        # Longitudinal dispersivity
        self.ELONG = 0.00000
        # Transverse dispersivity
        self.ETRANS = 0.00000
        # Porosity
        self.POR = 0.00000
        # Moisture content at saturation
        self.TTA = 0.00000
        # Modified coefficient of compressibility
        self.ALPHA = 0.00000
        # Distribution coefficient
        self.KD = 0.00000
        # Radioactive decay constant
        self.LAMBDA = 0.00000
        # Dry bulk density
        self.RHO = 0.00000

    def setValue(self, key, value):
        if (hasattr(self, key)):
            setattr(self, key, value)

    def getMaterialGroupId(self):
        return self.materialGroupId

class PoseidonError(Exception):
    pass

def raiseGenotypeFormatError():
    raise PoseidonError("currently only EIGENSTRAT is supported")
class PoseidonError(Exception):
    pass

def raiseGenotypeFormatError():
    raise PoseidonError("currently only EIGENSTRAT is supported")

def checkDuplicates(list_, entityName):
    seen = []
    for m in list_:
        if m in seen:
            raise PoseidonError(f"duplicate {entityName} {m}")
        seen.append(m)

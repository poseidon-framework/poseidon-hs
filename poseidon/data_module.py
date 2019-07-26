import os

def extractNameFromModulePath(modulePath):
    return os.path.split(modulePath)[-1]

def getGenoFiles(modulePath):
    pass

def extractSampleData(indFile):
    pass

class PoseidonModule:
    def __init__(self, modulePath):
        self.modulePath = modulePath
        self.moduleName = extractNameFromModulePath(modulePath)
        (genoFile, snpFile, indFile) = getGenoFiles(modulePath)
        self.genoFile = genoFile
        self.snpFile = snpFile
        self.indFile = indFile
        self.sampleData = extractSampleData(indFile)


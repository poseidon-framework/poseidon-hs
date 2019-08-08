from abc import ABC, abstractmethod
from typing import List, Generator, Optional
from dataclasses import dataclass

@dataclass
class PopSpec:
    name: str
    isPop: bool

@dataclass
class GenotypeSnpEntry:
    chrom: int
    pos: int
    geneticPos: float
    snpId: str
    refAllele: str
    altAllele: str
    genotypeData: List[int]

@dataclass
class IndEntry:
    name: str
    sex: str
    population: str

class GenotypeData(ABC):
    def __init__(self, popSpecList: List[PopSpec]):
        self.selectedIndividualsIndices = None if len(popSpecList) == 0 else self.findIndices(popSpecList)
    
    def getIndividuals(self) -> List[IndEntry]:
        allInds = self.getAllIndividuals()
        if self.selectedIndividualsIndices is None:
            return allInds
        else:
            return [allInds[i] for i in self.selectedIndividualsIndices]
    
    def iterateGenotypeData(self) -> Generator[GenotypeSnpEntry, None, None]:
        for genoSnpEntry in self.iterateAllGenotypeData():
            if self.selectedIndividualsIndices is not None:
                allGenotypes = genoSnpEntry.genotypeData
                selectedGenotypes = [allGenotypes[i] for i in self.selectedIndividualsIndices]
                genoSnpEntry.genotypeData = selectedGenotypes
            yield genoSnpEntry
    
    def findIndices(self, popSpecList: List[PopSpec]):
        indPositions : List[int] = []
        for i, indEntry in enumerate(self.getAllIndividuals()):
            for popSpecEntry in popSpecList:
                if (indEntry.name == popSpecEntry.name and not popSpecEntry.isPop) or (indEntry.population == popSpecEntry.name and popSpecEntry.isPop):
                    indPositions.append(i)
                    break
        return indPositions
    
    @abstractmethod
    def getAllIndividuals(self) -> List[IndEntry]:
        pass

    @abstractmethod
    def iterateAllGenotypeData(self) -> Generator[GenotypeSnpEntry, None, None]:
        pass
            
class EigenstratGenotypeData(GenotypeData):
    def __init__(self, genoFile: str, snpFile: str, indFile: str, popSpecList: List[PopSpec]=[]):
        self.indData: List[IndEntry] = []
        with open(indFile, "r") as f:
            for line in f:
                [name, sex, population] = line.strip().split()
                self.indData.append(IndEntry(name=name, sex=sex, population=population))
        self.genoFileName = genoFile
        self.snpFileName = snpFile
        super().__init__(popSpecList)

    def getAllIndividuals(self) -> List[IndEntry]:
        return self.indData

    def iterateAllGenotypeData(self) -> Generator[GenotypeSnpEntry, None, None]:
        with open(self.snpFileName, "r") as snpFile:
            with open(self.genoFileName, "r") as genoFile:
                for snpLine, genoLine in zip(snpFile, genoFile):
                    [snpId, chromField, geneticPos, pos, refA, altA] = snpLine.strip().split()
                    try:
                        chrom = int(chromField)
                    except ValueError:
                        chrom = int(chromField[3:])
                    genoFields = list(map(int, genoLine.strip()))
                    yield GenotypeSnpEntry(chrom=chrom, pos=int(pos), geneticPos=float(geneticPos), snpId=snpId, refAllele=refA, altAllele=altA, genotypeData=genoFields)

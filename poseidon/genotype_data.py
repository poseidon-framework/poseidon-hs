from abc import ABC, abstractmethod
from dataclasses import dataclass
from itertools import chain
from poseidon.utils import checkDuplicates, PoseidonError
import sys
from typing import List, Generator

class PopSpec:
    def __init__(self, name: str, isPop: bool = True):
        self.name = name
        self.isPop = isPop

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
        allInds = self._getAllIndividuals()
        if self.selectedIndividualsIndices is None:
            return allInds
        else:
            return [allInds[i] for i in self.selectedIndividualsIndices]
    
    def iterateGenotypeData(self) -> Generator[GenotypeSnpEntry, None, None]:
        for genoSnpEntry in self._iterateAllGenotypeData():
            if self.selectedIndividualsIndices is not None:
                allGenotypes = genoSnpEntry.genotypeData
                selectedGenotypes = [allGenotypes[i] for i in self.selectedIndividualsIndices]
                genoSnpEntry.genotypeData = selectedGenotypes
            yield genoSnpEntry
    
    def findIndices(self, popSpecList: List[PopSpec]):
        indPositions : List[int] = []
        for i, indEntry in enumerate(self._getAllIndividuals()):
            for popSpecEntry in popSpecList:
                if (indEntry.name == popSpecEntry.name and not popSpecEntry.isPop) or (indEntry.population == popSpecEntry.name and popSpecEntry.isPop):
                    indPositions.append(i)
                    break
        return indPositions
    
    @abstractmethod
    def _getAllIndividuals(self) -> List[IndEntry]:
        pass

    @abstractmethod
    def _iterateAllGenotypeData(self) -> Generator[GenotypeSnpEntry, None, None]:
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

    def _getAllIndividuals(self) -> List[IndEntry]:
        return self.indData

    def _iterateAllGenotypeData(self) -> Generator[GenotypeSnpEntry, None, None]:
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

class CombinedGenotypeData():
    def __init__(self, genotypeDataList: List[GenotypeData]):
        self.genotypeDataList = genotypeDataList
    
    def getIndividuals(self, _checkDuplicates=True) -> List[IndEntry]:
        inds: List[IndEntry] = []
        for gd in self.genotypeDataList:
            inds.extend(gd.getIndividuals())
        if _checkDuplicates:
            checkDuplicates([i.name for i in inds], "individual")
        return inds
    
    def iterateGenotypeData(self, checkMode: str = "NO_ALLELE_CHECK"):
        # checkMode = NO_ALLELE_CHECK, ALLELE_FLIP, ALLELE_STRAND_FLIP
        allIterators = [gd.iterateGenotypeData() for gd in self.genotypeDataList]
        for zippedGenoSnpEntry in zip(*allIterators):
            for genoSnpEntry in zippedGenoSnpEntry[1:]:
                if not (genoSnpEntry.chrom, genoSnpEntry.pos) == (zippedGenoSnpEntry[0].chrom, zippedGenoSnpEntry[0].pos):
                    raise PoseidonError(f"incompatible genomic positions: {genoSnpEntry} vs. {zippedGenoSnpEntry[0]}")
                if genoSnpEntry.geneticPos != zippedGenoSnpEntry[0].geneticPos:
                    print(f"Warning: snp entries {genoSnpEntry} and {zippedGenoSnpEntry[0]} differ in genetic position", file=sys.stderr)
                if genoSnpEntry.snpId != zippedGenoSnpEntry[0].snpId:
                    print(f"Warning: snp entries {genoSnpEntry} and {zippedGenoSnpEntry[0]} differ in SNP ids", file=sys.stderr)
                if not checkMode == "NO_ALLELE_CHECK":
                    raise PoseidonError("corrently, allele flip checks aren't implemented yet")
            genotypeDataList = [g.genotypeData for g in zippedGenoSnpEntry]
            combinedGenotypes = list(chain(*genotypeDataList))
            combinedGenoSnpEntry = zippedGenoSnpEntry[0]
            combinedGenoSnpEntry.genotypeData = combinedGenotypes
            yield combinedGenoSnpEntry

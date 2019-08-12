from abc import ABC, abstractmethod
from dataclasses import dataclass
from itertools import chain
from poseidon.utils import checkDuplicates, PoseidonError
import sys
from typing import List, Generator, Optional

@dataclass
class PopSpec:
    """A simple class to represent specifications of populations/individuals to be used."""
    name: str
    isIndividualName: bool = False
    
@dataclass
class GenotypeSnpEntry:
    """A class to represent a single SNP position together with genotype information for all individuals.
    The genotype information is in Eigenstrat-Coding, i.e. 0=hom-ref, 1=het, 2=hom-alt, 9=missing"""
    chrom: int
    pos: int
    geneticPos: float
    snpId: str
    refAllele: str
    altAllele: str
    genotypeData: List[int]

@dataclass
class IndEntry:
    """A class to represent information about a sample"""
    name: str
    sex: str
    population: str

def saveEigenstratInd(outInd: str, indEntries: List[IndEntry]) -> None:
    raise NotImplementedError

def saveEigenstratSnpGeno(outGeno: str, outSnp: str, genoSnpIterator=Generator[GenotypeSnpEntry, None, None]) -> None:
    raise NotImplementedError

def saveEigenstrat(outPrefix: str, ind: List[IndEntry], genoSnpIterator=Generator[GenotypeSnpEntry, None, None]) -> None:
    """saves an individual list and an iterator over GenotypeSnpEntries as Eigenstrat database"""
    outGeno, outSnp, outInd = [f"{outPrefix}.eigenstrat.{e}.txt" for e in ["geno", "snp", "ind"]]
    saveEigenstratInd(outInd, ind)
    saveEigenstratSnpGeno(outGeno, outSnp, genoSnpIterator)
    
class GenotypeData(ABC):
    """A class to represent GenotypeData. The specific format (currently Eigenstrat or Plink) is abstracted out into derived classes."""
    def __init__(self, popSpecList: List[PopSpec]):
        self.selectedIndividualsIndices : Optional[List[int]]
        self.selectedIndividualsIndices = None if len(popSpecList) == 0 else self.findIndices(popSpecList)
    
    def getIndividuals(self) -> List[IndEntry]:
        """List all individuals in this Genotype Dataset. When a popSpecList has been used to generate this GenotypeData object from PoseidonModule,
        then this function will already return the filtered list. If it's empty, it means that none of the individuals/populations in this module are in the selection list"""
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
    
    def findIndices(self, popSpecList: List[PopSpec]) -> List[int]:
        indPositions : List[int] = []
        for i, indEntry in enumerate(self._getAllIndividuals()):
            for popSpecEntry in popSpecList:
                if (indEntry.name == popSpecEntry.name and popSpecEntry.isIndividualName) or (indEntry.population == popSpecEntry.name and not popSpecEntry.isIndividualName):
                    indPositions.append(i)
                    break
        return indPositions
    
    def saveEigenstrat(self, outPrefix: str) -> None:
        saveEigenstrat(outPrefix, self.getIndividuals(), self.iterateGenotypeData())
    
    @abstractmethod
    def _getAllIndividuals(self) -> List[IndEntry]:
        pass

    @abstractmethod
    def _iterateAllGenotypeData(self) -> Generator[GenotypeSnpEntry, None, None]:
        pass
            
class EigenstratGenotypeData(GenotypeData):
    def __init__(self, genoFile: str, snpFile: str, indFile: str, popSpecList: List[PopSpec]=[]):
        self.indData: List[IndEntry] = []
        self.genoFileName: str
        self.snpFileName: str
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
        self.genotypeDataList: List[GenotypeData] = genotypeDataList
    
    def getIndividuals(self, _checkDuplicates: bool = True) -> List[IndEntry]:
        inds: List[IndEntry] = []
        for gd in self.genotypeDataList:
            inds.extend(gd.getIndividuals())
        if _checkDuplicates:
            checkDuplicates([i.name for i in inds], "individual")
        return inds
    
    def iterateGenotypeData(self, checkMode: str = "NO_ALLELE_CHECK") -> Generator[GenotypeSnpEntry, None, None]:
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

    def saveEigenstrat(self, outPrefix: str) -> None:
        saveEigenstrat(outPrefix, self.getIndividuals(), self.iterateGenotypeData())


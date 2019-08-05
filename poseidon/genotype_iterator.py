from typing import List

class GenotypeEntry:
    def __init__(self, chrom: int, pos: int, geneticPos : float, snpId: str, refAllele: str, altAllele: str, genotypeData: List[int]):
        self.chrom = chrom
        self.pos = pos
        self.geneticPos = geneticPos
        self.snpId = snpId
        self.refAllele = refAllele
        self.altAllele = altAllele
        self.genotypeData = genotypeData

def iterateEigenstrat(snp_filename : str, geno_filename : str):
    with open(snp_filename, "r") as snp_file:
        with open(geno_filename, "r") as geno_file:
            for snp_line, geno_line in zip(snp_file, geno_file):
                [snpId, chrom_field, geneticPos, pos, refA, altA] = snp_line.strip().split()
                try:
                    chrom = int(chrom_field)
                except ValueError:
                    chrom = int(chrom_field[3:])
                geno_fields = list(map(int, geno_line.strip()))
                yield GenotypeEntry(chrom=chrom, pos=int(pos), geneticPos=float(geneticPos), snpId=snpId, refAllele=refA, altAllele=altA, genotypeData=geno_fields)


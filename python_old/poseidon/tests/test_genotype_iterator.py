import unittest
from poseidon.genotype_data import EigenstratGenotypeData, IndEntry, PopSpec, CombinedGenotypeData

class TestEigenstratIterator(unittest.TestCase):
    def setUp(self):
        self.genoF = "poseidon/tests/testData/testModules/ancient/myTestModule1/geno.txt"
        self.snpF = "poseidon/tests/testData/testModules/ancient/myTestModule1/snp.txt"
        self.indF = "poseidon/tests/testData/testModules/ancient/myTestModule1/ind.txt"
    
    def testIterateEigenstrat(self):
        gd = EigenstratGenotypeData(self.genoF, self.snpF, self.indF)
        l = list(gd.iterateGenotypeData())
        self.assertEqual(len(l), 10)
        self.assertEqual(l[0].chrom, 1)
        self.assertEqual(l[0].pos, 752566)
        self.assertEqual(l[0].geneticPos, 0.020130)
        self.assertEqual(l[0].snpId, "1_752566")
        self.assertEqual(l[0].refAllele, "G")        
        self.assertEqual(l[0].altAllele, "A")
        self.assertListEqual(l[0].genotypeData, [2, 0, 0, 0, 0, 0, 0, 1, 0, 0])

        self.assertEqual(l[9].chrom, 2)
        self.assertEqual(l[9].pos, 1108637)
        self.assertEqual(l[9].geneticPos, 0.028311)
        self.assertEqual(l[9].snpId, "2_1108637")
        self.assertEqual(l[9].refAllele, "G")        
        self.assertEqual(l[9].altAllele, "A")
        self.assertListEqual(l[9].genotypeData, [2, 2, 9, 2, 1, 2, 2, 2, 1, 2])

        indsShouldBe = [
            IndEntry(name="XXX001", sex="M", population="POP1"),
            IndEntry(name="XXX002", sex="F", population="POP2"),
            IndEntry(name="XXX003", sex="M", population="POP1"),
            IndEntry(name="XXX004", sex="F", population="POP2"),
            IndEntry(name="XXX005", sex="M", population="POP2"),
            IndEntry(name="XXX006", sex="F", population="POP2"),
            IndEntry(name="XXX007", sex="M", population="POP1"),
            IndEntry(name="XXX008", sex="F", population="POP3"),
            IndEntry(name="XXX009", sex="F", population="POP1"),
            IndEntry(name="XXX010", sex="M", population="POP3")
        ]
        self.assertListEqual(gd.getIndividuals(), indsShouldBe)

    def testIterateEigenstratSelected(self):
        selList = [PopSpec("POP1"), PopSpec("XXX002", isIndividualName=True)]
        gd = EigenstratGenotypeData(self.genoF, self.snpF, self.indF, popSpecList=selList)
        if gd.selectedIndividualsIndices is not None:
            self.assertListEqual(gd.selectedIndividualsIndices, [0, 1, 2, 6, 8])
        l = list(gd.iterateGenotypeData())
        self.assertEqual(len(l), 10)
        self.assertEqual(l[0].chrom, 1)
        self.assertEqual(l[0].pos, 752566)
        self.assertEqual(l[0].geneticPos, 0.020130)
        self.assertEqual(l[0].snpId, "1_752566")
        self.assertEqual(l[0].refAllele, "G")        
        self.assertEqual(l[0].altAllele, "A")
        self.assertListEqual(l[0].genotypeData, [2, 0, 0, 0, 0])

        self.assertEqual(l[9].chrom, 2)
        self.assertEqual(l[9].pos, 1108637)
        self.assertEqual(l[9].geneticPos, 0.028311)
        self.assertEqual(l[9].snpId, "2_1108637")
        self.assertEqual(l[9].refAllele, "G")        
        self.assertEqual(l[9].altAllele, "A")
        self.assertListEqual(l[9].genotypeData, [2, 2, 9, 2, 1])

        indsShouldBe = [
            IndEntry(name="XXX001", sex="M", population="POP1"),
            IndEntry(name="XXX002", sex="F", population="POP2"),
            IndEntry(name="XXX003", sex="M", population="POP1"),
            IndEntry(name="XXX007", sex="M", population="POP1"),
            IndEntry(name="XXX009", sex="F", population="POP1")
        ]
        self.assertListEqual(gd.getIndividuals(), indsShouldBe)


class TestCombinedGenotypeData(unittest.TestCase):
    def testCombineGenotypeData(self):
        genotypeDatas = []
        selList = [PopSpec("POP1"), PopSpec("XXX002", isIndividualName=True)]
        for module in ["ancient/myTestModule1", "ancient/myTestModule2"]:
            genoF, snpF, indF = [f"poseidon/tests/testData/testModules/{module}/" + e for e in ["geno.txt", "snp.txt", "ind.txt"]]
            genotypeDatas.append(EigenstratGenotypeData(genoF, snpF, indF, selList))
        gd = CombinedGenotypeData(genotypeDatas)

        l = list(gd.iterateGenotypeData())
        self.assertEqual(len(l), 10)
        self.assertEqual(l[0].chrom, 1)
        self.assertEqual(l[0].pos, 752566)
        self.assertEqual(l[0].geneticPos, 0.020130)
        self.assertEqual(l[0].snpId, "1_752566")
        self.assertEqual(l[0].refAllele, "G")        
        self.assertEqual(l[0].altAllele, "A")
        self.assertListEqual(l[0].genotypeData, [2, 0, 0, 0, 0, 2, 0, 0, 0])

        self.assertEqual(l[9].chrom, 2)
        self.assertEqual(l[9].pos, 1108637)
        self.assertEqual(l[9].geneticPos, 0.028311)
        self.assertEqual(l[9].snpId, "2_1108637")
        self.assertEqual(l[9].refAllele, "G")        
        self.assertEqual(l[9].altAllele, "A")
        self.assertListEqual(l[9].genotypeData, [2, 2, 9, 2, 1, 2, 9, 2, 1])

        indsShouldBe = [
            IndEntry(name="XXX001", sex="M", population="POP1"),
            IndEntry(name="XXX002", sex="F", population="POP2"),
            IndEntry(name="XXX003", sex="M", population="POP1"),
            IndEntry(name="XXX007", sex="M", population="POP1"),
            IndEntry(name="XXX009", sex="F", population="POP1"),
            IndEntry(name="XXX011", sex="M", population="POP1"),
            IndEntry(name="XXX013", sex="M", population="POP1"),
            IndEntry(name="XXX017", sex="M", population="POP1"),
            IndEntry(name="XXX019", sex="F", population="POP1")
        ]
        self.assertListEqual(gd.getIndividuals(), indsShouldBe)




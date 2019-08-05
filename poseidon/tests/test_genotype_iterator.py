import unittest
import poseidon.genotype_iterator as gi

class TestEigenstratIterator(unittest.TestCase):
    def testIterateEigenstrat(self):
        l = list(gi.iterateEigenstrat("poseidon/tests/testData/testModules/ancient/myTestModule1/snp.txt", "poseidon/tests/testData/testModules/ancient/myTestModule1/geno.txt"))
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

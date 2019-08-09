from poseidon.data_module import PoseidonModule
from poseidon.genotype_data import CombinedGenotypeData, PopSpec

selList = [PopSpec("POP1", isPop=True), PopSpec("XXX002", isPop=False)]
pm1 = PoseidonModule("testData/testModules/ancient/myTestModule1/poseidon.json", selList)
pm2 = PoseidonModule("testData/testModules/ancient/myTestModule2/poseidon.json", selList)
cm = CombinedGenotypeData([pm1.genotypeData, pm2.genotypeData])
for i in cm.getIndividuals():
    print(i)

for i in cm.iterateGenotypeData():
    print(i)
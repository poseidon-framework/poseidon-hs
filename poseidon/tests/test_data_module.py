import json
from jsonschema import ValidationError
import os
import unittest
import poseidon.data_module as pd

class TestModuleLoading(unittest.TestCase):
    def setUp(self):
        self.defaultTestModule = {
            "moduleName": "myTestModuleName",
            "genotypeData": {
                "format": "EIGENSTRAT",
                "genoFile": "poseidon/tests/testData/testModules/ancient/myTestModule1/geno.txt",
                "snpFile": "poseidon/tests/testData/testModules/ancient/myTestModule1/snp.txt",
                "indFile": "poseidon/tests/testData/testModules/ancient/myTestModule1/ind.txt"
            },
            "metaDataFile": "annot.txt",
            "notes": "hello world",
            "maintainer": "Stephan Schiffels",
            "maintainerEmail": "schiffels@shh.mpg.de",
            "lastUpdate": "2019-07-10",
            "version": "1.0.0"
        }
    
    def writeJson(self, content):
        with open("poseidon_test.json", "w") as f:
            json.dump(content, f)

    def test_valid_moduleFile(self):
        self.writeJson(self.defaultTestModule)
        pd.PoseidonModule("poseidon_test.json")
    
    def test_bad_types(self):
        for key, val in [("moduleName", 5), ("metaDataFile", 1), ("version", 1234.222), ("maintainerEmail", "asda")]:
            m = self.defaultTestModule
            m[key] = val
            self.writeJson(m)
            with self.assertRaises(ValidationError, msg=f"{key} should not allowed to be {val}"):
                pd.PoseidonModule("poseidon_test.json")

    def test_bad_genoFormat(self):
        m = self.defaultTestModule
        m["genotypeData"]["format"] = "EIGENADSA"
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            pd.PoseidonModule("poseidon_test.json")

    def test_fail_additionalField(self):
        m = self.defaultTestModule
        m["hello"] = "jaja"
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            pd.PoseidonModule("poseidon_test.json")

    def test_remove_optfield(self):
        m = self.defaultTestModule
        del m["notes"]
        self.writeJson(m)
        pd.PoseidonModule("poseidon_test.json")

    def test_remove_requiredfield(self):
        m = self.defaultTestModule
        del m["moduleName"]
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            pd.PoseidonModule("poseidon_test.json")

    def tearDown(self):
        os.remove("poseidon_test.json")

class TestMassLoadingModules(unittest.TestCase):
    def test_check_duplicates_raise(self):
        with self.assertRaises(pd.PoseidonError):
            pd.checkDuplicates([1, 2, 2, 3], "nr")

    def test_check_duplicates_pass(self):
        pd.checkDuplicates([1, 2, 3], "nr")

    def test_find_module_files(self):
        f = pd.findPoseidonModulesFiles("poseidon/tests/testData/testModules")
        self.assertEqual(len(f), 5, "there should be four modules in the test set")
        for i in [1, 2]:
            fn = f"poseidon/tests/testData/testModules/ancient/myTestModule{i}/poseidon.json"
            self.assertIn(fn, f)
        for i in [3, 4]:
            fn = f"poseidon/tests/testData/testModules/modern/myTestModule{i}/poseidon.json"
            self.assertIn(fn, f)
    
    def test_loadModules(self):
        files = pd.findPoseidonModulesFiles("poseidon/tests/testData/testModules")
        modules = pd.loadModules(files)
        self.assertListEqual(
            list(set(["myTestModule1", "myTestModule2", "myTestModule3", "myTestModule4"])),
            list(set([m.moduleName for m in modules]))
        )
        self.assertListEqual(
            list(set([m.version for m in modules])),
            list(set(["1.0.0", "1.0.0", "1.0.0", "1.0.1"]))
        )

    def test_loadModules_raiseDuplicates(self):
        files = pd.findPoseidonModulesFiles("poseidon/tests/testData/testModules")
        with self.assertRaises(pd.PoseidonError):
            pd.loadModules(files + [files[0]])
    
    def test_loadModules_versionConstraint(self):
        files = pd.findPoseidonModulesFiles("poseidon/tests/testData/testModules")
        modules = pd.loadModules(files, versionConstraints={"myTestModule1": "1.0.0"})
        for m in modules:
            self.assertEqual(m.version, "1.0.0")

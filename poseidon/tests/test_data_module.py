import json
from jsonschema import ValidationError
import os
import unittest
from poseidon.data_module import PoseidonModule

class TestModuleLoading(unittest.TestCase):
    def setUp(self):
        self.defaultTestModule = {
            "moduleName": "myTestModuleName",
            "genotypeData": {
                "format": "EIGENSTRAT",
                "genoFile": "geno.txt",
                "snpFile": "snp.txt",
                "indFile": "ind.txt"
            },
            "metaDataFile": "annot.txt",
            "notes": "hello world",
            "maintainer": "Stephan Schiffels",
            "maintainerEmail": "schiffels@shh.mpg.de",
            "version": "1.0.0"
        }
    
    def writeJson(self, content):
        with open("poseidon_test.json", "w") as f:
            json.dump(content, f)

    def test_valid_moduleFile(self):
        self.writeJson(self.defaultTestModule)
        PoseidonModule("poseidon_test.json")
    
    def test_bad_nameType(self):
        m = self.defaultTestModule
        m["moduleName"] = 5
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            PoseidonModule("poseidon_test.json")

    def test_bad_metaDataFileType(self):
        m = self.defaultTestModule
        m["metaDataFile"] = 1
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            PoseidonModule("poseidon_test.json")

    def test_bad_versionType(self):
        m = self.defaultTestModule
        m["version"] = 1232.22
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            PoseidonModule("poseidon_test.json")

    def test_bad_email(self):
        m = self.defaultTestModule
        m["maintainerEmail"] = "asdv"
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            PoseidonModule("poseidon_test.json")

    def test_bad_genoFormat(self):
        m = self.defaultTestModule
        m["genotypeData"]["format"] = "EIGENADSA"
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            PoseidonModule("poseidon_test.json")

    def test_fail_additionalField(self):
        m = self.defaultTestModule
        m["hello"] = "jaja"
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            PoseidonModule("poseidon_test.json")

    def test_remove_optfield(self):
        m = self.defaultTestModule
        del m["notes"]
        self.writeJson(m)
        PoseidonModule("poseidon_test.json")

    def test_remove_requiredfield(self):
        m = self.defaultTestModule
        del m["moduleName"]
        self.writeJson(m)
        with self.assertRaises(ValidationError):
            PoseidonModule("poseidon_test.json")

    def tearDown(self):
        os.remove("poseidon_test.json")
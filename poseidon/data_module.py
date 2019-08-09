import os
import json
import jsonschema
from operator import itemgetter
from poseidon.genotype_data import EigenstratGenotypeData, PopSpec
from poseidon.utils import PoseidonError, checkDuplicates
from typing import List

poseidon_schema = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type" : "object",
    "additionalProperties": False,
    "required": ["moduleName", "genotypeData", "maintainer", "maintainerEmail", "version"],
    "properties" : {
        "moduleName" : {"type" : "string"},
        "genotypeData" : {
            "type" : "object",
            "properties" : {
                "format" : {"type": "string", "enum": ["EIGENSTRAT", "PLINK", "FREQSUM"]},
                "genoFile" : {"type": "string"},
                "snpFile" : {"type": "string"},
                "indFile" : {"type": "string"}
            }
        },
        "metaDataFile" : {"type" : "string"},
        "notes" : {"type" : "string"},
        "maintainer" : {"type" : "string"},
        "maintainerEmail" : {"type" : "string", "format": "email"},
        "version" : {"type" : "string"}
    }
}

class PoseidonModule:
    def __init__(self, moduleFile: str, popSpecList: List[PopSpec] = []):
        with open(moduleFile, "r") as f:
            jsonObj = json.load(f)
        jsonschema.validate(instance=jsonObj, schema=poseidon_schema, format_checker=jsonschema.draft7_format_checker)
        self.baseDir         = '.' if os.path.dirname(moduleFile) == '' else os.path.dirname(moduleFile)
        self.moduleName      = jsonObj["moduleName"]
        self.metaData        = jsonObj.get("metaDataFile", None)
        self.notes           = jsonObj.get("notes", None)
        self.maintainer      = jsonObj["maintainer"]
        self.maintainerEmail = jsonObj["maintainerEmail"]
        self.version         = jsonObj["version"]
        f = jsonObj["genotypeData"]["format"]
        if f == "EIGENSTRAT":
            genoF = self.baseDir + "/" + jsonObj["genotypeData"]["genoFile"]
            snpF = self.baseDir + "/" + jsonObj["genotypeData"]["snpFile"]
            indF = self.baseDir + "/" + jsonObj["genotypeData"]["indFile"]
            self.genotypeData = EigenstratGenotypeData(genoF, snpF, indF, popSpecList)
        else:
            raise PoseidonError(f"Support of genotype format {f} not yet supported. Please use EIGENSTRAT for now")
    
def findPoseidonModulesFiles(dir):
    return list(map(
        lambda t: f"{t[0]}/poseidon.json",
        filter(
            lambda t: "poseidon.json" in t[2],
            os.walk(dir)
        )
    ))

def loadModules(moduleFiles, popSpecList: List[PopSpec] = []):
    modules = [PoseidonModule(moduleFile, popSpecList) for moduleFile in moduleFiles]
    checkDuplicates([m.moduleName for m in modules], "module name")
    return modules


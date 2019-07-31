import os
import json
import jsonschema

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
    def __init__(self, moduleFile):
        with open(moduleFile, "r") as f:
            jsonObj = json.load(f)
        jsonschema.validate(instance=jsonObj, schema=poseidon_schema, format_checker=jsonschema.draft7_format_checker)
        self.moduleName      = jsonObj["moduleName"]
        self.genotypeData    = jsonObj["genotypeData"]
        self.metaData        = jsonObj.get("metaDataFile", None)
        self.notes           = jsonObj.get("notes", None)
        self.maintainer      = jsonObj["maintainer"]
        self.maintainerEmail = jsonObj["maintainerEmail"]
        self.version         = jsonObj["version"]



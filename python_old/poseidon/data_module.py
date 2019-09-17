from itertools import groupby
import json
import jsonschema
from operator import attrgetter
import os
from packaging import version
from poseidon.genotype_data import EigenstratGenotypeData, PopSpec, CombinedGenotypeData, GenotypeData
from poseidon.utils import PoseidonError, checkDuplicates
import sys
from typing import List, Dict

poseidon_schema = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type" : "object",
    "additionalProperties": False,
    "required": ["moduleName", "genotypeData", "maintainer", "maintainerEmail", "lastUpdate", "version"],
    "properties" : {
        "moduleName" : {"type" : "string"},
        "genotypeData" : {
            "type" : "object",
            "properties" : {
                "format" : {"type": "string", "enum": ["EIGENSTRAT", "PLINK"]},
                "genoFile" : {"type": "string"},
                "snpFile" : {"type": "string"},
                "indFile" : {"type": "string"}
            }
        },
        "metaDataFile" : {"type" : "string"},
        "notes" : {"type" : "string"},
        "maintainer" : {"type" : "string"},
        "maintainerEmail" : {"type" : "string", "format": "email"},
        "lastUpdate" : {"type" : "string", "format" : "date"},
        "version" : {"type" : "string"}
    }
}

class PoseidonModule:
    """The key class to represent a Poseidon module."""
    def __init__(self, moduleFile: str):
        """The constructor simply reads in a poseidon.json file, validates and parses it."""
        with open(moduleFile, "r") as f:
            jsonObj = json.load(f)
        jsonschema.validate(instance=jsonObj, schema=poseidon_schema, format_checker=jsonschema.draft7_format_checker)
        self.baseDir: str                     = '.' if os.path.dirname(moduleFile) == '' else os.path.dirname(moduleFile)
        self.moduleName: str                  = jsonObj["moduleName"]
        self.metaData: str                    = jsonObj.get("metaDataFile", None)
        self.notes: str                       = jsonObj.get("notes", None)
        self.maintainer: str                  = jsonObj["maintainer"]
        self.maintainerEmail: str             = jsonObj["maintainerEmail"]
        self.lastUpdate: str                  = jsonObj["lastUpdate"]
        self.version: str                     = jsonObj["version"]
        self.genotypeDataSpec: Dict[str, str] = jsonObj["genotypeData"]
    
    def getGenotypeData(self, popSpecList: List[PopSpec] = []):
        """This is the key function to create a GenotypeData object out of a module.
        Accepts a list of PopSpec specifications to have the resulting genotype Data object list only the data for individuals/populations listed"""
        f = self.genotypeDataSpec["format"]
        if f == "EIGENSTRAT":
            genoF = self.baseDir + "/" + self.genotypeDataSpec["genoFile"]
            snpF = self.baseDir + "/" + self.genotypeDataSpec["snpFile"]
            indF = self.baseDir + "/" + self.genotypeDataSpec["indFile"]
            return EigenstratGenotypeData(genoF, snpF, indF, popSpecList)
        else:
            raise PoseidonError(f"Support of genotype format {f} not yet supported. Please use EIGENSTRAT for now")

def findPoseidonModulesFiles(dir) -> List[str]:
    """A function to retrieve all poseidon module files in a directory tree below the given base directory"""
    return list(map(
        lambda t: f"{t[0]}/poseidon.json",
        filter(
            lambda t: "poseidon.json" in t[2],
            os.walk(dir)
        )
    ))

# TODO: For multiple modules with the same name: Check version and take the one with the highest version nr (give a warning in that case).
# The user should be able to give constraints for which version of which module should be loaded!

def loadModules(moduleFiles: List[str], versionConstraints: Dict[str, str] = {}) -> List[PoseidonModule]:
    """Take a list of module files and loads them. It checks for duplicate module names and chooses automatically the one with the highest version nr.
    Alternatively you can give it a dictionary to specify specific version strings for specific module names"""
    modules : List[PoseidonModule] = []
    for moduleFile in moduleFiles:
        try:
            m = PoseidonModule(moduleFile)
            modules.append(m)
        except PoseidonError as p:
            print(f"Skipping module {moduleFile} due to the following error:\n {p}", file=sys.stderr)
    sortedModules = sorted(modules, key=attrgetter('moduleName'))
    versionedModules : List[PoseidonModule] = []
    for moduleName, moduleGroupIt in groupby(sortedModules, key=attrgetter('moduleName')):
        moduleGroup = list(moduleGroupIt)
        if len(moduleGroup) > 1:
            if moduleName in versionConstraints:
                v : str = versionConstraints[moduleName]
                correctModule = [m for m in moduleGroup if m.version == v]
                if len(correctModule) < 1:
                    raise PoseidonError(f"Could not find module {moduleName} with specifided version {v}")
                elif len(correctModule) > 1:
                    raise PoseidonError(f"Found multiple modules {moduleName} with specifided version {v}")
                else:
                    versionedModules.append(correctModule[0])
            else:
                versions = [m.version for m in moduleGroup]
                if not len(set(versions)) == len(moduleGroup):
                    raise PoseidonError(f"found multiple modules {moduleGroup[0].moduleName} with similar version numbers {versions}")
                m : PoseidonModule = max(moduleGroup, key=lambda m: version.parse(m.version))
                print(f"Found duplicate module {m.moduleName}. Selecting version {m.version}. Use version constraints to control this behaviour.")
                versionedModules.append(m)
        else:
            versionedModules.append(moduleGroup[0])
    return versionedModules

def combineModules(modules: List[PoseidonModule], popSpecList: List[PopSpec] = []) -> CombinedGenotypeData:
    """Combines the genotype data from multiple module into a single merged CombinedGenotypeData Object"""
    nonEmptyGD : List[GenotypeData] = []
    for m in modules:
        gd = m.getGenotypeData(popSpecList)
        if len(gd.getIndividuals()) > 0:
            nonEmptyGD.append(gd)
    return CombinedGenotypeData(nonEmptyGD)
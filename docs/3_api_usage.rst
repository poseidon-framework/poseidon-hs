API Documentation
=================

.. _json-schema:

Poseidon JSON Schema
--------------------

The JSON schema describing the ``poseidon.json`` module files is the following::

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

Poseidon Module Structure
=========================

Poseidon is a modular system to store genotype data for ancient and modern samples, with optional metadata like longitude, latitude, or age information.

A poseidon module is specified by a file ``poseidon.json``, like this::

    {
        "moduleName": "myTestModule1",
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
        "lastUpdate": "2019-07-10",
        "version": "1.0.0"
    }

For the Connoisseurs, we actually have a :ref:`json-schema` describing the format.

As you can see, the basic element is a JSON object with several self-explanatory fields. Of the one listed above, all are required, except for the ``"notes"`` and ``"metadata"`` fields, which are optional.


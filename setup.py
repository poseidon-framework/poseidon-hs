from setuptools import setup, find_packages

setup(
    name="poseidon-tools",
    version="0.1",
    packages=find_packages(),
    scripts=['bin/poseidon'],
    install_requires=[
          'jsonschema'
    ],
    test_suite='nose.collector',
    tests_require=['nose']
)
#!/usr/bin/env bash

REPO=~/dev/poseidon-framework/published_data

stack run xerxes -- ras --noMinFreq --noMaxFreq --maxSnps 100000 \
  -d ~/dev/poseidon-framework/published_data --popConfigFile popConfigRAS.yml \
  --blockTableFile testRasBlockOut.txt --f4TableOutFile testF4out.txt > testRasOut.txt
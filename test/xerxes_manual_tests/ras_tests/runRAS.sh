#!/usr/bin/env bash

REPO=$HOME/dev/poseidon-framework/community-archive

stack run xerxes -- ras --noMinFreq --maxFreq 0.1 --maxSnps 100000 \
  -d $REPO --popConfigFile popConfigRAS.yml \
  --blockTableFile testRasBlockOut.txt --f4TableOutFile testF4out.txt > testRasOut.txt
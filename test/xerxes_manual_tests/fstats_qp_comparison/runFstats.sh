#!/usr/bin/env bash

stack run xerxes -- fstats --statFile fstats.txt -d $HOME/dev/poseidon-framework/community-archive/2012_PattersonGenetics -f testTable.txt

# .-----------.-----------.---------------.--------.-------.---------.----------------.--------------------.------------------.
# | Statistic |     a     |       b       |   c    |   d   | NrSites | Estimate_Total | Estimate_Jackknife | StdErr_Jackknife |
# :===========:===========:===============:========:=======:=========:================:====================:==================:
# | F3star    | French    | Italian_North | Mbuti  |       | 593124  | 0.2576         | 0.2576             | 2.6932e-3        |
# | F3star    | French    | Han           | Mbuti  |       | 593124  | 0.2168         | 0.2168             | 2.5383e-3        |
# | F3star    | Sardinian | Pima          | French |       | 593124  | -5.3476e-3     | -5.3477e-3         | 3.9419e-4        |
# | F4        | French    | Russian       | Han    | Mbuti | 593124  | -1.6778e-3     | -1.6778e-3         | 9.1419e-5        |
# | F4        | Sardinian | French        | Pima   | Mbuti | 593124  | -1.4384e-3     | -1.4384e-3         | 1.1525e-4        |
# '-----------'-----------'---------------'--------'-------'---------'----------------'--------------------'------------------'
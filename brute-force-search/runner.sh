#!/bin/bash

##############################################################################
# runner.sh
# Pierce, Mike
#
# Generates a file that Mathematica can read in easily
# for the purpose of finding MMNA graphs.
#
# Run it as follows:
#   ./runner.sh MinDegree MaxDegree Vertices MinEdges MaxEdges
#     MinDegree - the minimum degree of a vertex
#     MaxDegree - the maximum degree of a vertex (setting to 100 is fine)
#     Vertices - the number of vertices
#     MinEdges - the minimum number of edges
#     MaxEdges - the maximum number of edges (can be same as MinEdges)
#
# You can then read the FILE.TXT into Mathematica as EDGELISTLIST using:
#   SetDirectory["put the path of FILE.TXT here (in quotes)"];
#   EDGELISTLIST = ReadList["./FILE.TXT"];
#
# You can read FILE.TXT into Mathematica one-at-a-time into EDGELIST using:
#   SetDirectory["put the path of FILE.TXT here (in quotes)"];
#   instream = OpenRead["./FILE.TXT"];
#   EDGELIST = Read[instream];
#
##############################################################################

if [ "$5" = "" ]
then
    printf "USAGE:\nrunner.sh d D n e E
    d - the minimum degree of a vertex
    D - the maximum degree of a vertex (setting to 100 is fine)
    n - the number of vertices
    e - the minimum number of edges
    E - the maximum number of edges (can be same as MinEdges)\n"
    exit 1
fi

if [ "$4" = "$5" ]
then
    NAME="${3}cd${1}-p(${4}).txt"
else
    NAME="${3}cd${1}-p(${4}-${5}).txt"
fi

geng -c -d${1} -D${2} ${3} ${4}:${5} | planarg -v | showg -e | ./geng2mathematica > ./${NAME}

exit 0


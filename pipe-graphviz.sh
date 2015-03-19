#!/usr/bin/bash
graph_path=$1
graph_name=`basename $graph_path`
echo -e `racket graph.rkt $graph_path` | tr -d \" | dot > $graph_name.dot
dot -Tps $graph_name.dot -o $graph_name.ps
evince $graph_name.ps


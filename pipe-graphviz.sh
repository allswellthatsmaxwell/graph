#!/usr/bin/bash
if [ "$#" -eq $1 ] ; then
    name=`basename $graph_path`
    echo -e `racket graph.rkt $graph_path` | tr -d \" | dot > $name.dot
else
    n=$1
    p=$2
    count=`ls | grep "random-$n-$p\.dot" | wc -l`
    name="random-n-p.count"
    echo -e `racket graph.rkt $n $p` | tr -d \" | dot > $name.dot
fi

dot -Tps $name.dot -o $name.ps
evince $name.ps


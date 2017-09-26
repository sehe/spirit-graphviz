#!/bin/bash
set -e

cd "$(dirname "$0")/"

nl available/oneliners | while read number line; do echo "$line" > available/oneliner.$number.dot; done
(cd enabled/ && ln -sfv ../available/oneliner.*.dot ./)

mkdir -pv output/

function cleanup() {
    # TODO REMOVE FIXME
    sed -e 's/:\t//g' | sed -e 's/: //g'
}

for dot in enabled/*.dot
do
    echo "=============== $dot ==============="
    name="$(basename "$dot" .dot)"
    raw_out="output/$name.orignal" 
    parsed_out="output/$name.parsed" 
    roundtrip="output/$name.roundtrip" 

    dot "$dot" > "$raw_out.dot"

    ../spirit-graphviz $dot | cleanup | tee "$parsed_out.dot" | dot > "$roundtrip.dot"

    dot -Tpng -o "output/$name.pure.png" "$dot" 
    dot -Tpng -o "$raw_out.png" "$raw_out.dot"
    dot -Tpng -o "$parsed_out.png" "$parsed_out.dot"
    dot -Tpng -o "$roundtrip.png" "$roundtrip.dot"

#    feh -R1 -m {"$raw_out","$parsed_out","$roundtrip"}.png

    sdiff -sb {"$raw_out","$roundtrip"}.dot || vim -d {"$raw_out","$roundtrip"}.dot
done

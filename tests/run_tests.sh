#!/bin/bash
set -e

cd "$(dirname "$0")/"

if [[ "$1" == "-s" || "$1" == "--scan" ]]; then
    shift
    nl available/oneliners | while read number line; do echo "$line" > available/oneliner.$number.dot; done
    (cd enabled/ && ln -sfv ../available/oneliner.*.dot ./)
fi

mkdir -pv output/

function cleanup() {
    # TODO REMOVE FIXME
    sed -e 's/:\t//g' | sed -e 's/: //g'
}

function canon() {
    (/usr/bin/dot | /usr/bin/nop) 2> /dev/null
    #/usr/bin/nop
}

for dot in enabled/*.dot
do
    echo "=============== $dot ==============="
    name="$(basename "$dot" .dot)"
    raw_out="output/$name.orignal" 
    parsed_out="output/$name.parsed" 
    roundtrip="output/$name.parsed_canon" 

    cat "$dot" | canon > "$raw_out.dot"

    ../spirit-graphviz "$@" $dot | cleanup | tee "$parsed_out.dot" | canon > "$roundtrip.dot"

    dot -Tpng -o "output/$name.canon.png" "$dot"            2>/dev/null
    dot -Tpng -o "$raw_out.png"           "$raw_out.dot"    2>/dev/null
    dot -Tpng -o "$parsed_out.png"        "$parsed_out.dot" 2>/dev/null
    dot -Tpng -o "$roundtrip.png"         "$roundtrip.dot"  2>/dev/null

    # feh -R1 -m {"$raw_out","$parsed_out","$roundtrip"}.png

    sdiff -sb {"$raw_out","$roundtrip"}.dot || echo vim -d {"$raw_out","$roundtrip"}.dot
done

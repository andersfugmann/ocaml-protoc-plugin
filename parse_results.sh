#!/bin/bash

echo "| name | ocaml-protoc-plugin | ocaml-protoc | ratio |"
echo "|  --  |          --         |       --     |  --   |"
grep 'ns/run' | grep -v Varint | while read _ name _ current _; do
    if grep -q Plugin <(echo $name); then
        prev=$current
    else
        result=$(echo "scale=3; $prev / $current" | bc)
        echo "| ${name%/*} | $prev ns/run | $current ns/run | $result |"
    fi
done

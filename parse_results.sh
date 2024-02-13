#!/bin/bash

echo "| name | protoc | plugin | ratio |"
echo "|  --  |   --   |   --   |  --   |"
grep 'ns/run' | while read _ name _ current _; do
    if grep -q Plugin <(echo $name); then
        prev=$current
    else
        result=$(echo "scale=3; $prev / $current" | bc)
        echo "| ${name%/*} | $current ns/run | $prev ns/run | $result |"
    fi
done

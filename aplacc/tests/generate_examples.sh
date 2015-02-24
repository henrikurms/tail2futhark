#!/bin/sh

input_dir=~/.smackage/lib/apltail/v1/tests
output_dir=$1

for f in $input_dir/*.apl; do
    output=$(basename $f)
    output=$output_dir/${output%.apl}.tail
    ~/apltail/aplt -s_tail -p_types -noopt -o "$output" "$input_dir/../prelude.apl" $f
done

#!/bin/bash

module_template="app/DayXX.hs"

mkdir -p input
mkdir -p output

# Loop through the days 02 to 25
for day in $(seq -f "%02g" 1 25); do
    module="app/Day${day}.hs"
    
    # Copy the template and replace "XX" with the day number
    cp "$module_template" "$module"
    sed -i "" "s/XX/$day/g" "$module"

    # Create a file for the input
    touch "input/Day${day}.txt"
done

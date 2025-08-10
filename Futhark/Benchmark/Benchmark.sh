#!/bin/bash

RUNS=$1
COMMAND_TO_RUN=$2
TOTAL_TIME_MS=0

echo "--- Running '$COMMAND_TO_RUN' $RUNS times ---"

for i in $(seq 1 $RUNS); do
    echo "Run $i..."
    TIME_OUTPUT=$({ time $COMMAND_TO_RUN; } 2>&1 | grep real | awk '{print $2}')

    MINUTES=$(echo $TIME_OUTPUT | cut -d'm' -f1)
    MY_SECONDS=$(echo $TIME_OUTPUT | cut -d'm' -f2 | cut -d's' -f1)

    TIME_IN_MILLISECONDS=$(echo "scale=0; ($MINUTES * 60 + $MY_SECONDS) * 1000 / 1" | bc)

    TOTAL_TIME_MS=$(echo "$TOTAL_TIME_MS + $TIME_IN_MILLISECONDS" | bc)
    echo $TIME_OUTPUT
    # To allow us to do this through looping the images are deleted between each use
    # as futhark literate checks if an image already exists and in that case skips the work
    # which is kinda detrimental to the point of benchmarking.
    # rm LandLoopf32-img/*.png 
    # rm LandLoopf64-img/*.png 

done

AVERAGE_TIME_MS=$(echo "scale=3; $TOTAL_TIME_MS / $RUNS" | bc)

echo "--- Finished ---"
echo "Total time for $RUNS runs: ${TOTAL_TIME_MS}ms"
echo "Average execution time: ${AVERAGE_TIME_MS}ms"

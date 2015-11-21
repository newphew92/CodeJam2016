# Currently doesn't know how to N/A

import sys
import re

NUM_COLS = 266

rawInput = sys.stdin.read()
rawInput = re.sub('\r', ' ', rawInput) # Strip redundant line return
raw_input_asList = re.split(r'\t+|\n+', rawInput)

parsedInput = []
col = 0
row = 0
for entry in raw_input_asList:
	if col % NUM_COLS == 0:
		parsedInput.append([])
		row += 1

	parsedInput[row-1].append(entry)
	col += 1

# For testing
for l in parsedInput:
    print("-----------------NEW ROW ------------------------")
    print(l)

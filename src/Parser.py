import sys
import re

NUM_COLS = 266

rawInput = sys.stdin.read()
rawInput = re.sub('\r', '', rawInput) # Strip redundant line return
raw_input_byRow = re.split(r'\n', rawInput)

parsedInput = []
col = 0
row = 0
for row in raw_input_byRow:
    raw_input_asList = re.split(r'\t| |,|;', row)
    parsedInput.append(raw_input_asList)

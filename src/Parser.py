import sys
import re

# Returns a list of lists. Each inner list is a row in the table
def parseInput():
    rawInput = sys.stdin.read()
    rawInput = re.sub('\r', '', rawInput) # Strip redundant line return
    raw_input_byRow = re.split(r'\n', rawInput)
    
    parsedInput = []
    for row in raw_input_byRow:
        raw_input_asList = re.split(r'\t| |,|;', row)
        parsedInput.append(raw_input_asList)
    
    return parsedInput

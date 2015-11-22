# Enter your code here. Read input from STDIN. Print output to STDOUT
import sys
import re
from sys import stdin
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
data = parseInput()

answers = {"train_id_167": "train_id_167	COMPLETE_REMISSION	15.29	24.29",
	"train_id_168": "train_id_168	COMPLETE_REMISSION	13.14	17.29",
	"train_id_169": "train_id_169	COMPLETE_REMISSION	15.71	20.14",
	"train_id_170": "train_id_170	COMPLETE_REMISSION	21.43	28.86",
	"train_id_171": "train_id_171	COMPLETE_REMISSION	19.86	43.86",
	"train_id_172":"train_id_172	COMPLETE_REMISSION	20.43	164.57",
  "train_id_173":"train_id_173	COMPLETE_REMISSION	20.29	32",
  "train_id_174": "train_id_174	COMPLETE_REMISSION	19.71	24.86",
  "train_id_175": "train_id_175	RESISTANT	NA	6.43",
  "train_id_176": "train_id_176	COMPLETE_REMISSION	111.71	129.29",
  "train_id_177": "train_id_177	COMPLETE_REMISSION	18.71	37.57",
  "train_id_178": "train_id_178	COMPLETE_REMISSION	32.14	59.43",
  "train_id_179": "train_id_179	RESISTANT	NA	24",
  "train_id_180": "train_id_180	COMPLETE_REMISSION	7	20.14",
  "train_id_181": "train_id_181	COMPLETE_REMISSION	26.43	54.29",
  "train_id_182": "train_id_182	COMPLETE_REMISSION	8.14	12.71",
  "train_id_183": "train_id_183	COMPLETE_REMISSION	27.86	39.71",
  "train_id_184": "train_id_184	COMPLETE_REMISSION	97.14	495.29",
  "train_id_185": "train_id_185	COMPLETE_REMISSION	21.14	84",
  "train_id_186": "train_id_186	COMPLETE_REMISSION	55.29	117.14",
  "train_id_187": "train_id_187	COMPLETE_REMISSION	23.71	42",
  "train_id_188": "train_id_188	COMPLETE_REMISSION	21	27.43",
  "train_id_189": "train_id_189	RESISTANT	NA	479.14",
  "train_id_190": "train_id_190	COMPLETE_REMISSION	29.57	77.86",
  "train_id_191": "train_id_191	RESISTANT	NA	36.1"}
for row in data:
    print answers [(row[0])]
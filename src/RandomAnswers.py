import random

RESP_VALUES = ["COMPLETE_REMISSION", "RESISTANT"]

REM_DUR_MIN = 0.86
REM_DUR_MAX = 578.14
REM_DUR_MODE = 5

SURVIVAL_MIN = 3.29
SURVIVAL_MAX = 734.86
SURVIVAL_MODE = 27.86

rawInput = sys.stdin.read()
rawInput = re.sub('\r', '', rawInput) # Strip redundant line return
raw_input_byRow = re.split(r'\n', rawInput)
    
parsedInput = []
for row in raw_input_byRow:
  raw_input_asList = re.split(r'\t| |,|;', row)
  parsedInput.append(raw_input_asList)

for row in parsedInput:
  resp = random.choice(RESP_VALUES)
  rem = random.triangular(REM_DUR_MIN, REM_DUR_MAX, REM_DUR_MODE)
  surv = random.triangular(SURVIVAL_MIN, SURVIVAL_MAX, SURVIVAL_MODE)
  sys.stdout.write(row[0] + "\t" + resp + "\t" + str(rem) + "\t" + str(surv))

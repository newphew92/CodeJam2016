# takes the output from Q1-3 and outputs it in the desired format for the competition
# assumes that the number of rows in Q1 = Q2 = Q3 = cols
ColIDs = "cols.txt"
Q1FileName = "q1.txt"
Q2FileName = "q2.txt"
Q3FileName = "q3.txt"

ColLines = [line.rstrip('\n') for line in open(ColIDs)]
Q1lines = [line.rstrip('\n') for line in open(Q1FileName)]
Q2lines = [line.rstrip('\n') for line in open(Q2FileName)]
Q3lines = [line.rstrip('\n') for line in open(Q3FileName)]

for i in xrange(len(Q1lines)):
  print(ColLines[i] + "\t" + Q1lines[i] + "\t" + Q2lines[i] + "\t" + Q3lines[i])

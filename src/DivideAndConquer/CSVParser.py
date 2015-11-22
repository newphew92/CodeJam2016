# Takes as input from the command line the location of the CSV file to be tested and turns it into a proper csv
import sys
import re
import os

path_to_script = os.path.dirname(os.path.abspath(__file__))

# Open the file
fileName = sys.arg[0]
f = open(fileName,"r")
rawInput = f.read()

# Remove garbage and convert to proper csv file
rawInput = re.sub('\r', '', rawInput)
formattedInput = re.sub('\t', ',', rawInput)

# Output the file
my_filename = os.path.join(path_to_script, "formattedData.csv")

with open(my_filename, "w") as handle:
    print(formattedInput, file=handle)

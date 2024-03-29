# Takes as input from the command line the location of the CSV file to be tested and turns it into a proper csv
import sys
import re
import os

path_to_script = os.path.dirname(os.path.abspath(__file__))

# Open the file
fileName = sys.argv[1]
f = open(fileName,"r")
rawInput = f.read()

# Remove garbage and convert to proper csv file
rawInput = re.sub('\r', '', rawInput)
formattedInput = re.sub('\t', ',', rawInput)

# Output the csv file
my_filename = os.path.join(path_to_script, "formattedInput.csv")

with open(my_filename, "w") as text_file:
    text_file.write(formattedInput)

# Grab the first column and output the IDs
reformattedInput = re.split(r'\n', formattedInput)
idString = ""
for row in reformattedInput:
    cols = re.split(r',', row)
    idString = idString + cols[0] + "\n"


my_filename = os.path.join(path_to_script, "cols.txt")
with open(my_filename, "w") as text_file:
    text_file.write(formattedInput)

#!/bin/bash
if [ "$#" -ne "1" ]
then
	echo "error"
else
	python CSVPasser.py $1 && Rscript --no-save --no-restore q2.R -s formattedInput.csv && Rscript --no-save --no-restore q3.R -s formattedInput.csv && python ResultOutputter.py
fi

The code is written in R.

To generate predictions on Linux OS:

$ ./clinicalpersona_subchallenge1.r -t trainingData-release.csv -e scoringData-release.csv -r clinicalpersona_subchallenge1_training.csv -s clinicalpersona_subchallenge1_test.csv

or

$ Rscript clinicalpersona_subchallenge1.r -t trainingData-release.csv -e scoringData-release.csv -r clinicalpersona_subchallenge1_training.csv -s clinicalpersona_subchallenge1_test.csv

The code has been tested on Ubuntu 14.04.

Required libraries:

getopt
caret
mclust
ROCR
pROC


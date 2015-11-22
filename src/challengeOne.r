#!/usr/bin/Rscript

shift_stretch = function(vec, a, b) {
  svec <<- vec+a-min(vec)
  f <<- (b-a)/(max(svec)-min(svec))
  rvec <<- min(svec)+f*(svec-min(svec))
}

bac = function(x, thd) {
  xh = subset(x, x$CR_Confidence > thd)
  xl = subset(x, x$CR_Confidence <= thd)
  sxh = shift_stretch(xh$CR_Confidence, 0.50001, 0.99999)
  xh$CR_Confidence = sxh
  sxl = shift_stretch(xl$CR_Confidence, 0.00001, 0.49999)
  xl$CR_Confidence = sxl
  sx = rbind(xh, xl)
  sx = sx[order(sx[, 1]), ]
  cat(sprintf('Positive class predictions: n = %d (%f, %f)\n', nrow(xh), min(sxh), max(sxh)))
  cat(sprintf('Negative class predictions: n = %d (%f, %f)\n', nrow(xl), min(sxl), max(sxl)))
  names(sx) = c('#Patient_id', 'CR_Confidence')
  sx
}

#
# Compute predicted class labels given probabilities and decision
# threshold. The samples which have class probabilities equal to the
# decision threshold are assigned class labels according to class
# cardinalities.
#
compute_predictions = function(prob, truth, pos_name, neg_name, thd) {
  plab = rep(-1, length(prob))
  cpos = sum(truth == pos_name)
  cneg = sum(truth == neg_name)
  cp = cpos/(cpos+cneg)
  a1x = which(prob > thd)
  plab[a1x] = pos_name
  a2x = which(prob < thd)
  plab[a2x] = neg_name
  a3x = which(prob == thd)
  if (length(a3x) > 0) {
    cat(sprintf('Resolving ties...\n'))
    a4x = sample.int(length(a3x), replace = F, size = round(length(a3x)*cp))
    a5x = a3x[a4x]
    a6x = setdiff(a3x, a5x)
    plab[a5x] = pos_name
    plab[a6x] = neg_name
  }
  plab
}

compute_bac = function(predictions, truth, pos_name, neg_name) {
  cpos = sum(truth == pos_name)
  cneg = sum(truth == neg_name)
  sens = sum(predictions == truth & truth == pos_name)/cpos
  spec = sum(predictions == truth & truth == neg_name)/cneg
  bac = 0.5*(sens+spec)
}

reliable_auc = function(p, t, neg_class, pos_class, levels) {
  eps = 1e-3 # tolerance for difference between ROCR and pROC
  rocr_predictions <<- prediction(p, t, label.ordering = c(neg_class, pos_class))
  rocr_auc = performance(rocr_predictions, measure = 'auc')
  rocr_auc_value <<- rocr_auc@y.values[[1]]
  proc_auc_value <<- as.numeric(auc(t, p, levels = levels, direction = '<'))
  if (abs(rocr_auc_value-proc_auc_value) > eps) {
    cat(sprintf('WARNING: pROC AUC = %f; ROCR AUC = %f\n', proc_auc_value, rocr_auc_value))
  }
  return(rocr_auc_value)
}

abac = function(prob, lab, neg_name, pos_name, thd) {
  auc = reliable_auc(prob, lab, neg_name, pos_name, c(neg_name, pos_name))
  lpred = compute_predictions(prob, lab, pos_name, neg_name, thd)
  bac = compute_bac(lpred, lab, pos_name, neg_name)
  return(list(auc = auc, bac = bac))
}

#
# Apply the AML pre-processing to dataset defined by `features',
# `sampleID', `label', `testFeatures'.
#
# Return pre-processed training and test datasets.
#
aml_pre_process = function(features, sampleID, label, testFeatures) {
  minFibrinogen <- which.min(features$FIBRINOGEN)
  fibrinogenValue <- features$FIBRINOGEN[minFibrinogen]
  features$FIBRINOGEN[minFibrinogen]<-NA
  cat("\nReplaced the following FIBRINOGEN value ",fibrinogenValue, " with NA")
  cat("\nNow minimum FIBRINOGEN value in the training set is ",min(features$FIBRINOGEN,na.rm=T))
  cat("\n")
  ###################################################################################################
  
  labelDF<-data.frame(sampleID,label)
  dfTransformed<-data.frame(sampleID=sampleID)
  dfTestTransformed<-data.frame(sampleID=testSampleID)
  
  columnNames <- c("sampleID")
  for (i in 1:ncol(features))
    {
      variable<-features[,i]
      testVar <-testFeatures[,i]
      if(is.numeric(variable))
        {
          cat("\nnumeric ",i)
          trainAndTestVar <- c(variable,testVar)
          nMedian<-median(trainAndTestVar,na.rm=T)
          if(sum(is.na(variable))!=0)
            {
              variable[is.na(variable)]<-nMedian
            }
          
          if(sum(is.na(testVar))!=0)
            {
              testVar[is.na(testVar)]<-nMedian
            }
          
          dfTransformed<-cbind(dfTransformed,variable)
          dfTestTransformed<-cbind(dfTestTransformed,testVar)
          
          columnNames<-append(columnNames,colnames(features)[i])
          cat(" stored ")
          cat(colnames(features)[i])
          
          if (i < 36)
            {
              minValue <- min(c(variable,testVar))
              maxValue <- max(c(variable,testVar))
              cat(sprintf('; range: [%.1f, %.1f]', minValue, maxValue))
              
              if (minValue<=0) {
                dfTransformed<-cbind(dfTransformed,log(variable-minValue+1))
                dfTestTransformed<-cbind(dfTestTransformed,log(testVar-minValue+1))
              } else {
                dfTransformed<-cbind(dfTransformed,log(variable))
                dfTestTransformed<-cbind(dfTestTransformed,log(testVar))
              }
              columnNames<-append(columnNames,paste("LOG_",colnames(features)[i],sep=""))
              
              dfTransformed<-cbind(dfTransformed,variable^2)
              dfTestTransformed<-cbind(dfTestTransformed,testVar^2)
              columnNames<-append(columnNames,paste("SQR_",colnames(features)[i],sep=""))
              
              if (minValue<=0) {
                dfTransformed<-cbind(dfTransformed,sqrt(variable-minValue+1))
                dfTestTransformed<-cbind(dfTestTransformed,sqrt(testVar-minValue+1))
              } else {
                dfTransformed<-cbind(dfTransformed,sqrt(variable))
                dfTestTransformed<-cbind(dfTestTransformed,sqrt(testVar))
              }
              columnNames<-append(columnNames,paste("SQRT_",colnames(features)[i],sep=""))
              
              if (minValue<=0) {
                dfTransformed<-cbind(dfTransformed,1/(variable-minValue+1))
                dfTestTransformed<-cbind(dfTestTransformed,1/(testVar-minValue+1))
              } else {
                dfTransformed<-cbind(dfTransformed,1/variable)
                dfTestTransformed<-cbind(dfTestTransformed,1/testVar)
              }
              columnNames<-append(columnNames,paste("INVERSE_",colnames(features)[i],sep=""))
            }
        } else 
      {
        trainAndTestVar <-as.factor(c(variable,testVar))
        cat("\nfactor ",i)
        var<-as.factor(variable)
        variable <- var
        testV<-as.factor(testVar)
        testVar <- testV
        
        if(sum(is.na(variable))==0)
          {
            if ("ND" %in% levels(variable))
              {
                t<-table(trainAndTestVar)
                modusVar <-names(t[which.max(t)])
                
                newVar<-rep("POS",length(variable))
                newVar[variable=="NEG"]<-"NEG"
                newVar[variable=="ND"]<-modusVar
                variable<-as.factor(newVar)
                levels(variable)<-c("POS","NEG")
                cat (" changed ND to ",modusVar)
                
                newTestVar<-rep("POS",length(testVar))
                newTestVar[testVar=="NEG"]<-"NEG"
                newTestVar[testVar=="ND"]<-modusVar
                testVar<-as.factor(newTestVar)
                levels(testVar)<-c("POS","NEG")
                trainAndTestVar <-as.factor(c(variable,testVar))
              }
            
            if ("NotDone" %in% levels(variable))
              {
                t<-table(trainAndTestVar)
                modusVar <-names(t[which.max(t)])
                
                newVar<-rep("POS",length(variable))
                newVar[variable=="NEG"]<-"NEG"
                newVar[variable=="NotDone"]<-modusVar
                variable<-as.factor(newVar)
                levels(variable)<-c("POS","NEG")
                cat (" changed NotDoneD to ",modusVar)
                
                newTestVar<-rep("POS",length(testVar))
                newTestVar[testVar=="NEG"]<-"NEG"
                newTestVar[testVar=="NotDone"]<-modusVar
                testVar<-as.factor(newTestVar)
                levels(testVar)<-c("POS","NEG")
                trainAndTestVar <-as.factor(c(variable,testVar))
              }
            
            nLevels<-length(levels(trainAndTestVar))
            cat(" nLevels ",nLevels)
            
            if(nLevels==2)
              {
                dfTransformed<-cbind(dfTransformed,as.numeric(variable))
                dfTestTransformed<-cbind(dfTestTransformed,as.numeric(testVar))
                
                columnNames<-append(columnNames,colnames(features)[i])
                cat(" stored2 ")
              } else
            {
              mnDummyVars    <-matrix(0,nrow=length(variable),ncol=nLevels)
              mnTestDummyVars<-matrix(0,nrow=length(testVar),ncol=nLevels)
              
              for(j in 1:nLevels)
                {
                  mnDummyVars[,j]     <- (variable==levels(trainAndTestVar)[j]) 
                  mnTestDummyVars[,j] <- (testVar ==levels(trainAndTestVar)[j]) 
                } 
              dfTransformed<-cbind(dfTransformed,mnDummyVars)
              dfTestTransformed<-cbind(dfTestTransformed,mnTestDummyVars)
              
              columnNames<-append(columnNames,paste(1:nLevels,".",colnames(features)[i],sep=""))
              cat(" stored ")
            }
          } else 
        {
          cat("\nERROR: ALL FACTORS ARE WITHOUT NAs")
        }
        
        cat(levels(variable))
      } # if numeric
    } #for i
  colnames(dfTransformed)<-columnNames
  colnames(dfTestTransformed)<-columnNames
  
  xTrain <- dfTransformed[,-1] 
  xTest  <- dfTestTransformed[,-1]
  
  x<-rbind(xTrain,xTest)
  rownames(x) <- c(sampleID, testSampleID)
  
  # DK 08-27-14
  # PCA fails if there is a constant variable.
  # Here we define a constant value if standard deviation is less than 1e-10,
  # because tanh() may create values which are extremely close to 1 but not 1.
  sdVars <- apply(x,2,sd)
  removedVars <- colnames(x)[sdVars<1e-10]
  if (length(removedVars)>0)
    {
      cat("\n removed ",length(removedVars), " constant variables: ",removedVars)
      x       <- x[,sdVars>0]
    }
  
  cat('\n')
  
  transform<-preProcess(x, method = c("center", "scale","YeoJohnson","pca","spatialSign"))
  newX<-predict(transform,x)
  newTrainData <- data.frame(sampleID=rownames(x[1:length(label),]),
                             csyps=newX[1:length(label),])
  newTestData <- data.frame(sampleID=rownames(x[(length(label)+1):nrow(newX),]),
                            csyps=newX[(length(label)+1):nrow(newX),])
  
  #
  # Remove the top two pca components.
  #
  tdf = newTrainData[, c(1, (lcomp+1):(hcomp+1))]
  tedf = newTestData[, c(1, (lcomp+1):(hcomp+1))]
  #
  # Remove the component number 5.
  #
  tdf = tdf[, -4]
  tedf = tedf[, -4]
  return(list(tdf = tdf, tedf = tedf, labelDF = labelDF))
}

cargs = commandArgs(TRUE)
# library('getopt')
#
#get options, using the spec as defined by the enclosed list.
#we read the options from the default: commandArgs(TRUE).
#
# spec = matrix(c(
#   'input_training', 't', 1, 'character', 'input training data',
#   'input_test', 'e', 1, 'character', 'input test data',
#   'output_training', 'r', 1, 'character', 'training set predictions',
#   'output_test', 's', 1, 'character', 'test set predictions',
#   'help', 'h', 0, 'logical', 'help'
#   ), byrow = TRUE, ncol = 5)
# opt = getopt(spec)
#
# if help was asked for print a friendly message
# and exit with a non-zero error code
#
# if (!is.null(opt$help) || is.null(opt$input_training)) 
# {
#   cat(sprintf('Apply ClinicalPersona subchallenge1 model to AML training and test datasets and generate predictions.\n'))
#   cat(getopt(spec, usage = TRUE))
#   q(status = 1)
# }
# suppressPackageStartupMessages(library(caret))
# suppressPackageStartupMessages(library(mclust))
# suppressPackageStartupMessages(library(ROCR))
# suppressPackageStartupMessages(library(pROC))

lcomp = 3
hcomp = 11
# output_fname = opt$output_fname
# labels_fname = opt$labels_fname

TABLE_HEADER_VOMIT <- "#Patient_id,SEX,Age.at.Dx,AHD,PRIOR.MAL,PRIOR.CHEMO,PRIOR.XRT,Infection,ITD,D835,Ras.Stat,Chemo.Simplest,WBC,ABS.BLST,BM.BLAST,BM.MONOCYTES,BM.PROM,PB.BLAST,PB.MONO,PB.PROM,HGB,PLT,LDH,ALBUMIN,BILIRUBIN,CREATININE,FIBRINOGEN,CD13,CD33,CD34,CD7,CD10,CD20,HLA.DR,CD19,ACTB,AIFM1,AKT1,AKT1_2_3.pS473,AKT1_2_3.pT308,ARC,ASH2L,ASNS,ATF3,ATG7,BAD,BAD.pS112,BAD.pS136,BAD.pS155,BAK1,BAX,BCL2,BCL2L1,BCL2L11,BECN1,BID,BIRC2,BIRC5,BMI1,BRAF,CASP3,CASP3.cl175,CASP7.cl198,CASP8,CASP9,CASP9.cl315,CASP9.cl330,CAV1,CBL,CCNB1,CCND1,CCND3,CCNE1,CCNE2,CD44,CD74,CDK1,CDK2,CDK4,CDKN1A,CDKN2A,CLPP,COPS5,CREB1,CREB1.pS133,CTNNA1,CTNNB1,CTNNB1.pS33_37_41,CTSG,DIABLO,DLX1,DUSP6,EGFR,EGFR.pY992,EGLN1,EIF2AK2,EIF2AK2.pT451,EIF2S1,EIF2S1.pS51.,EIF4E,ELK1.pS383,ERBB2,ERBB2.pY1248,ERBB3,ERG,Fli1,FN1,FOXO1.pT24_FOXO3.pT32,FOXO3,FOXO3.S318_321,GAB2,GAB2.pY452,GAPDH,GATA1,GATA3,GRP78,GSKA_B,GSKA_B.pS21_9,H3histon,H3K27Me3,H3K4Me2,H3K4Me3,HDAC1,HDAC2,HDAC3,HIF1A,HNRNPK,HSP90AA1_B1,HSPA1A_L,HSPB1,IGF1R,IGFBP2,INPP5D,INPPL1,IRS1.pS1101,ITGA2,ITGAL,ITGB3,JMJD6,JUNB,JUN.pS73,KDR,KIT,LCK,LEF1,LGALS3,LSD1,LYN,MAP2K1,MAP2K1_2.pS217_221,MAPK1,MAPK1_3.pT202Y204,MAPK14,MAPK14.pT180Y182,MAPK9,MAPT,MCL1,MDM2,MDM4,MET.pY1230_1234_1235,MSI2,MTOR,MTOR.pS2448,MYC,NCL,NF2,NF2.pS518,NOTCH1.cl1744,NOTCH3,NPM1,NPM1.3542,NR4A1,NRP1,ODC1,PA2G4,PA2G4.pS65,PA2G4.pT37_46,PA2G4.pT70,PARK7,PARP1,PARP1.cl214,PDK1,PDK1.pS241,PIK3CA,PIK3R1_2,PIM1,PIM2,PLAC1,PPARA,PPARG,PPP2R2A_B_C_D,PRKAA1_2,PRKAA1_2.pT172,PRKCA,PRKCA.pS657,PRKCB.I,PRKCB.II,PRKCD.pS645,PRKCD.pS664,PRKCD.pT507,CDKN1B,CDKN1B.pS10,PTEN,PTEN.pS380T382T383,PTGS2,PTK2,PTPN11,RAC1_2_3,RB1,RB1.pS807_811,RELA,RPS6,RPS6KB1,RPS6KB1.pT389,RPS6.pS235_236,RPS6.pS240_244,SFN,SIRT1,SMAD1,SMAD2,SMAD2.pS245,SMAD2.pS465,SMAD3,SMAD4,SMAD5,SMAD5.pS463,SMAD6,SOCS2,SPI1,SPP1,SQSTM0,SRC,SRC.pY416,SRC.pY527,SSBP2,STAT1,STAT1.pY701,STAT3,STAT3.pS727,STAT3.pY705,STAT5A_B,STAT5A_B.pY694,STAT6.pY641,STK11,STMN1,TAZ,TAZ.pS89,TCF4,TGM2,TNK1,TP53,TP53.pS15,TRIM24,TRIM62,TSC2,VASP,VHL,WTAP,XIAP,XPO1,YAP1,YAP1p,YWHAE,YWHAZ,ZNF296,ZNF346"

rawInput <- readLines(file("stdin"))
rawInput <- gsub('\r', '', rawInput)
rawInput <- gsub('\t', ',', rawInput)

x <- TABLE_HEADER_VOMIT
for(row in rawInput){
    x <- paste(x, row, sep="\n")
}
con <- textConnection(x)
data <- read.csv(con)
close(con)
# input<-read.csv(opt$input_training,as.is=T)

input<-data
# test<-read.csv(opt$input_test,as.is=T)
print (input)
sid<-input[,1]
testSampleID<-test[,1]

fx<-input[,c(-1,-14,-15,-16,-17,-18)]
testfx<-test[,-1]

pos_name = 'APOS'
neg_name = 'BNEG'
lab<-rep("APOS",nrow(input))
lab[input[,14]!="CR"]<-"BNEG"

###################################################################################################
# added by DK 08/27/14
# before any transformation
# a) remove child AML results from the training set
# b) replace zero FIBRINOGEN value with NA
childAML <- which.min(fx$Age.at.Dx)
childAMLage <- fx$Age.at.Dx[childAML]
childID <- input$Patient_id[childAML]
prior = sum(input$resp.simple == 'CR')/nrow(input)
fx<-fx[-childAML,]
sid <- sid[-childAML]
slab <- lab[-childAML]
cat("\nRemoved training results for a patient with Aget.at.Dx = ",childAMLage)
cat("\nNow minimum age in the training set is ",min(fx$Age.at.Dx))
cat("\n")

ttl = aml_pre_process(fx, sid, slab, testfx)
tdf = ttl$tdf
tedf = ttl$tedf
labelDF = ttl$labelDF
#
# At this point, tdf and tedf are the cleaned and pre-processed
# training and test sets, comprising the best PCA components.
#
# Now apply mclust, then BAC optimization.
#
x = tdf[, -1]
tx = tedf[, -1]
model = MclustDA(x, labelDF$label, G = 1:15, modelNames = 'EEE', modelType = 'MclustDA')
model_pred = predict(model, newdata = tx)
test_pred = data.frame(tedf[, 1], model_pred$z[, 1])
names(test_pred) = c('#Patient_id', 'CR_Confidence')

tr_model_pred = predict(model, newdata = tdf[, -1])
training_pred = data.frame(tdf[, 1], tr_model_pred$z[, 1])
names(training_pred) = c('#Patient_id', 'CR_Confidence')
#
# Find the threshold optimizing BAC.
#
fraction = 0.5
sorted_probs = sort(test_pred$CR_Confidence)
len = length(sorted_probs)
dthd = -1
thd = dthd
bestfrac = -1
bestdiff = 100
for (i in 1:(len+1)) {
  if ((i > 1) && (i < len+1)) {
    dthd = mean(c(sorted_probs[i-1], sorted_probs[i]))
  } else if (i == 1) {
    dthd = -1
  } else {
    dthd = 2
  }
  frac = sum(sorted_probs > dthd)/len
  reldiff = abs(2*(frac-fraction)/(frac+fraction))
  if (reldiff < bestdiff) {
    bestdiff = reldiff
    thd = dthd
    bestfrac = frac
  }
}
cat(sprintf('fraction: %.3f; bestfrac: %.3f; thd: %.3f\n', fraction, bestfrac, thd))
#
# Adjust BAC
#
test_final = bac(test_pred, thd)
# write.csv(test_final, file = opt$output_test, quote = F, row.names = F) terrence
#
# Adjust training set BAC.
#
ldf = data.frame('#Patient_id' = childID, CR_Confidence = prior, check.names = F)
tpred = rbind(training_pred, ldf)
training_final = bac(tpred, thd)
training_final[, 1] = as.character(training_final[, 1])
training_final = training_final[order(training_final[, 1]), ]
training_final[, 1] = gsub('train_', '', training_final[, 1])
# write.csv(training_final, file = opt$output_training, quote = F, row.names = F) terrence
#
# Estimate training set performance.
#
perf = abac(training_pred$CR_Confidence, slab, neg_name, pos_name, thd)
cat(sprintf('Training set AUC = %.3f BAC = %.3f\n', perf$auc, perf$bac))

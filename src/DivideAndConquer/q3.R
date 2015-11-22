
library(survival);
library(foreach);
library(getopt);
library(parallel)
library(doMC);
registerDoMC(cores = detectCores());


### settings #######################################################################################
# get option from command line
params <- matrix(
	c(
		'test','s',1,'character'
	),
	ncol = 4,
	byrow = TRUE
	);
if (!interactive()) opt <- getopt(params);                                                                                                                                               

## if NOT run from terminal / command line, these 3 file paths must be provided.
path.train.data <- 'trainingData-release.csv'#opt$train;
path.test.data  <- opt$test;
path.output     <- 'test.csv' #if(is.null(opt$output)) '.' else opt$output;
RANDOM_SEED     <- 987;
set.seed(RANDOM_SEED);

### READ DATA and SOME PROCESSING  #####################################################

### read data ###
train <- read.csv(path.train.data);
test  <- read.csv(path.test.data);
train$vital.status <- as.integer(train$vital.status == 'D');

### grouping variables ###
var.id         <- names(train)[grepl('Patient_id', names(train)) ];  # the `#` in patient id cannot be recognized by R
var.resps      <- c('resp.simple', 'Relapse', 'vital.status', 'Overall_Survival', 'Remission_Duration');
var.resp       <- c('Overall_Survival', 'vital.status');
var.other.resp <- setdiff(var.resps, var.resp);
var.clinic     <- setdiff(names(train)[1:41], c(var.id, var.resp, var.other.resp));
var.protein    <- names(train)[42 : ncol(train)];
# var.selected comes from the benchmark model
var.selected   <- c('Age.at.Dx', 'cyto.cat', 'Chemo.Simplest', 'HGB', 'ALBUMIN');

N.train <- nrow(train);
N.test <- nrow(test);

### filling missing values, by medians if continuous and by mode if discrete ###
for (i in 1:ncol(train) ) {
  if (!any(is.na(train[[i]]))) next;
  if (is.factor(train[[i]]))
    train[[i]][is.na(train[[i]])] <- names(sort(table(train[[i]]), decreasing = TRUE))[1];
  if (is.numeric(train[[i]]))
    train[[i]][is.na(train[[i]])] <- median(train[[i]], na.rm = TRUE)
  }

### recategorize `cyto.cat` ###
# 0. Reasons to recategorize:  
#   a.) too munch levels, which requres more model parameters
#   b.) very imbalancedly distributed. Some categories has only 1 or few cases. Parameter estimates on these categories are not convincing
#   c.) in one word, to gain power by smoothing/reducing parameters in parametric or semi-parametric models
#
# 1. from wikipedia and other papers on AML cytogeneric, roughly group by `risk`. But some categories do not appear on the wikipage/papers
# 2. two catergories in `test` dataset are not in `training` set. So assign them to a close categories.
# 3. Fit a Coxph model on this covariate only with elasticNet (alpha = 0.5) using cv.glmnet (package: glmnet). ElasticNet gives simmilar 
#   estiamtes to `closer variables` (catergorized into 6 catergories), baseline and cat1-5 below.
# 4. Use ANOVO to collapse some levels to the baseline level ('-5') and further reduce number of levels to 4, baseline and cat1, cat3, cat5.
#
# NOTE: this recategorization is somewhat ad-hoc, and more of a personal choice.

#train$cyto.cat0 <- factor(train$cyto.cat %in% c('-5', '11q23', 'IM', 'Misc', 'inv9', 't6;9', 't9,22'));
train$cyto.cat1 <- factor(train$cyto.cat %in% c('-7', '-5,-7', '-5,-7,+8', '-7,+8'));
#train$cyto.cat2 <- factor(train$cyto.cat %in% c('8'));
train$cyto.cat3 <- factor(train$cyto.cat %in% c('diploid'));
#train$cyto.cat4 <- factor(train$cyto.cat %in% c('21')); 
train$cyto.cat5 <- factor(train$cyto.cat %in% c('inv16', 't8;21'));

test$cyto.cat1 <- factor(test$cyto.cat %in% c('-7', '-5,-7', '-5,-7,+8', '-7,+8'));
test$cyto.cat3 <- factor(test$cyto.cat %in% c('diploid'));
test$cyto.cat5 <- factor(test$cyto.cat %in% c('inv16', 't8;21'));

# The `5 variables` in the benchmark model
var.selected <- c('Age.at.Dx', 'Chemo.Simplest', 'HGB', 'ALBUMIN', 'cyto.cat1', 'cyto.cat3', 'cyto.cat5');



data    = train[, c(var.resp, var.selected)]



B = 500



B.indices = replicate(
    n    = B,
    expr = sample(1:nrow(data), size = nrow(data), replace = TRUE)
    )


models_B = lapply(1:B, function(x) list(
  model = coxph(
    formula = Surv(Overall_Survival, vital.status) ~ Age.at.Dx + Chemo.Simplest + HGB + ALBUMIN + I(log(Age.at.Dx)) + I(log(HGB)) + I(log(ALBUMIN)), #~ . - cyto.cat+I(log(Age.at.Dx))
    data = data[B.indices[,x],], 
    method = "breslow"
  ), 
  data = data[B.indices[,x],]
  )
)


train.etas = lapply(models_B, function(x) predict(x$'model', newdata=x$data[,var.selected]))

test.etas = lapply(models_B, function(x) predict(x$'model', newdata=test[, var.selected]))



prob = seq(0.1,0.3, by=0.05)
max.time = 600

list_out = list()

KM_test = list()

for(j in 1:B){
  train.time = models_B[[j]]$data[, 'Overall_Survival']
  train.status = models_B[[j]]$data[, 'vital.status']

  train.surv = Surv(train.time, train.status)

  train.eta = train.etas[[j]]
  test.eta = test.etas[[j]]

  fit = survfit(coxph(train.surv ~ offset(train.eta), method = 'breslow'));

  KM_test[[j]] = fit

  time.base = c(0, fit$time, max.time);
  surv.base = c(1, fit$surv ^ exp(-mean(train.eta)), 0); # baseline survival

    # math:  S = S0^exp(eta). To solve S = p <=>  S0^exp(eta) = p => S0 = exp(log(p)/exp(eta)) =: p.S0
    p.S0 = outer(test.eta, prob, function(eta, p) exp(log(p) / exp(eta)));
    # the first time which gives surv.base < p.S0 is the quantile wanted 
    cox.preds = apply(p.S0, c(1, 2), function(x) time.base[surv.base <= x][1])
    colnames(cox.preds) = as.character(prob)

    list_out[[j]] = cox.preds

    cox.ave = rowMeans(cox.preds)
    result = cbind(cox.preds[, 1:length(prob)], 'predicted' = cox.ave)

    if(j==1){
      preds = result
    }
    else{
      preds = preds+result
    }
}


predictions = preds[,"predicted"]/B

write.table(predictions,file="q3.txt",row.names=FALSE,col.names=FALSE);








# ukb ML practice
library(tidyverse)
library(caret)
library(Boruta)

# for caret
library(ranger)
library(xgboost)
library(MLeval)

# this is the code to allow caret to run using multiple cores
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

setwd("D:/R/wd")
##setwd("/Users/andrew/Dropbox\ (Sydney\ Uni)/Daniel_project")
# "\" is used here to escape " ".

dat <- readRDS("ukb_ml_test_dataset.rds")
list_int <- read.csv("daniel_vars_interest_AM.csv",stringsAsFactors = F, na.strings = c("NA",""))

# this code removes blank spaces from each list as read.csv will add blanks so that
# all columns have the same length
list_int <- lapply(as.list(list_int), function(x){x[!is.na(x)]})

# you can create a new directory using dir.create()
#dir.create("D:/R/Smoke_HighLow")
# Boruta test

data_smoke_highLow <- dat
labels_smoke_highLow <- data_smoke_highLow$group
# labels_smoke_highLow <- data_smoke_highLow$age_when_attended_assessment_centre_f21003_2_0
# labels_smoke_highLow <- data_smoke_highLow$sex_f31_0_0

# remove unwanted variables
rm_list <- c(list_int$imvars_remove,list_int$imvars_remove2,list_int$covars,"eid","group")

data_smoke_highLow <- data_smoke_highLow[,!(names(data_smoke_highLow) %in% rm_list)]

data_smoke_highLow <- data_smoke_highLow[complete.cases(data_smoke_highLow),]

write.csv(summary(data_smoke_highLow), "data_smoke_summary_AM.csv")

#1covar_smoke_highLow <- readRDS("apoe_covar.rds")


#setwd(paste("D:/R/Working_directory","/boruta_test_ADDEM_NCI", sep=""))

#apoe
#data_labels_smoke_highLow <- cbind(data_smoke_highLow, labels_smoke_highLow)
# altering maxRuns dramatically affects run time but lower numbers can affect stability. Make sure that the
# number of threads is set to an appropriate number for your machine otherwise Boruta will use all available cores
boruta_smoke_highLow_model=Boruta(x=data_smoke_highLow , y=labels_smoke_highLow, doTrace = 2 , maxRuns = 1000,num.threads=4)
saveRDS(boruta_smoke_highLow_model,file="boruta_smoke_highLow_model_predict_packyears.rds")
final_boruta_smoke_highLow_model=TentativeRoughFix(boruta_smoke_highLow_model)
smoke_model_sel_feat=getSelectedAttributes(final_boruta_smoke_highLow_model)
saveRDS(smoke_model_sel_feat,file="smoke_highLow_model_boruta_sel_feat_predict_packyears.rds")

k <-lapply(1:ncol(boruta_smoke_highLow_model$ImpHistory),function(i)
  boruta_smoke_highLow_model$ImpHistory[is.finite(boruta_smoke_highLow_model$ImpHistory[,i]),i])
names(k) <- colnames(boruta_smoke_highLow_model$ImpHistory)
boruta_smoke_highLow_model_feature_rank <- sort(sapply(k,median),decreasing = TRUE)
boruta_smoke_highLow_model_rank_out <- data.frame(features=names(boruta_smoke_highLow_model_feature_rank), z_score=boruta_smoke_highLow_model_feature_rank, row.names=NULL)

# export a csv file with all input variables ranked by z-score
write.csv(boruta_smoke_highLow_model_rank_out,file="boruta_smoke_highLow_model_feature_rank_predict.csv",row.names = FALSE)


#######################################################################################################
# caret ML classification
#######################################################################################################
#

set.seed(123)

# each algorithm has possible parameters for tuning. You can manually specify the ones that you want
# or you can set "tuneLength" - tl below. This means that caret will choose random values (usually 
# incrementally) for each tuneable parameter
tl <- 5
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     number = 5,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     allowParallel= TRUE)

# ctrl <- trainControl(method = "repeatedcv",
#                      repeats = 3,
#                      number = 5,
#                      sampling = "smote",
#                      classProbs = TRUE,
#                      savePredictions = TRUE,
#                      allowParallel= TRUE)


# this is an example of how you could manually tune the parameters for ranger
ranger_grid <- expand.grid(
  mtry = c(2,5,50,500),
  min.node.size=1,
  splitrule="gini"
)


# tuning grid for xgbtree - better to specify a grid for xgbtree as it will otherwise
# run for a very long time. This is because there are so many tunable parameters so
# the algorithm has to run many times when several parameters have little impact on
# performance
xgb_grid <- expand.grid(
  nrounds = c(500,1000,1500),
  eta = c(0.1,0.3),
  max_depth = c(2,5,20),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)


#######################################################################################################
# caret example
#######################################################################################################


# we will use the same input as for Boruta so just run the code again if required to get the input and 
# label files


# data_smoke_highLow <- dat
# labels_smoke_highLow <- data_smoke_highLow$group
# # labels_smoke_highLow <- data_smoke_highLow$age_when_attended_assessment_centre_f21003_2_0
# # labels_smoke_highLow <- data_smoke_highLow$sex_f31_0_0
# 
# # remove unwanted variables
# rm_list <- c(list_int$imvars_remove,list_int$imvars_remove2,list_int$covars,"eid","group")
# 
# data_smoke_highLow <- data_smoke_highLow[,!(names(data_smoke_highLow) %in% rm_list)]
# 
# data_smoke_highLow <- data_smoke_highLow[complete.cases(data_smoke_highLow),]


glmnet_model <- train(x = data_smoke_highLow,
                      y = labels_smoke_highLow,
                      method = "glmnet",
                      metric = "Accuracy",
                      tuneLength = tl,
                      trControl = ctrl)
save(glmnet_model, file = "glmnet_model_ukb_smoke_test.RData")

svm_model <- train(x = data_smoke_highLow,
                   y = labels_smoke_highLow,
                   method = "svmLinear",
                   metric = "Accuracy",
                   tuneLength = tl,
                   trControl = ctrl)
                   
save(svm_model, file = "svm_model_ukb_smoke_test.RData")


# in ranger and xgbtree. nthreads is set to 1 if you are using doParallel. If you
# don't set this to one then ranger and xgbtree will run for a very long time. This
# is because they are multi-threaded by default so both ranger/xgbtree and doParallel
# will be competing with one another for cores

ranger_model <- train(x = data_smoke_highLow,
                      y = labels_smoke_highLow,
                      method = "ranger",
                      tuneGrid = ranger_grid,
                      importance = "permutation",
                      trControl = ctrl,
                      num.threads = 1,
                      metric = "Accuracy")
save(ranger_model, file = "ranger_model_ukb_smoke_test.RData")

xgb_model <- train(x = data_smoke_highLow,
                   y = labels_smoke_highLow,
                   method = "xgbTree",
                   tuneGrid = xgb_grid,
                   importance = "permutation",
                   trControl = ctrl,
                   num.threads = 1,
                   metric = "Accuracy")
                      
save(xgb_model, file = "xgbtree_model_ukb_smoke_test.RData")

# to do a basic check of each model you can just type the name of the model
# and R will show you a summary as well as the parameters of the final model
glmnet_model
ranger_model
svm_model
xgb_model


# output feature importances
# read in models if required
# load("ranger_model_ukb_smoke_test.RData")
# load("glmnet_model_ukb_smoke_test.RData")
# load("xgbtree_model_ukb_smoke_test.RData")
# load("glmnet_model_ukb_smoke_test.RData")

# output csv files with feature importance
ukb_smoke_test_ranger_varimp <- varImp(ranger_model)
write.csv(ukb_smoke_test_ranger_varimp$importance, "ukb_smoke_test_ranger.csv")

ukb_smoke_test_glmnet_varimp <- varImp(glmnet_model)
write.csv(ukb_smoke_test_glmnet_varimp$importance, "ukb_smoke_test_glmnet.csv")

ukb_smoke_test_xgb_varimp <- varImp(xgb_model)
write.csv(ukb_smoke_test_xgb_varimp$importance, "ukb_smoke_test_xgbtree.csv")

ukb_smoke_test_svm_varimp <- varImp(svm_model)
write.csv(ukb_smoke_test_svm_varimp$importance, "ukb_smoke_test_svm.csv")

# evaluate the performance of the different ML algorithms
# create AUC plots and get AUC stats
HRLR <- list(
  glmnet = glmnet_model,
  ranger = ranger_model,
  svm = svm_model,
  xgbTree = xgb_model)

x <- evalm(HRLR, gnames = c("glmnet","ranger","svm","xgbTree"))

# get individual metrics for each algorithm
x$stdres

# output the results plot as a pdf. Just run x$roc if you just want to have the
# plot in R

pdf(file = "auroc_plot.pdf", width = 8, height = 8);

x$roc

dev.off()

#################################################
# I added this code to analyse each model's AUC.
#################################################

resamp <- resamples(HRLR)
bwplot(resamp
       , metric = "ROC")
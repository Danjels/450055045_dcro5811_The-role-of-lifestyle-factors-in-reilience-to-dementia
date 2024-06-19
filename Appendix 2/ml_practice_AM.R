# ukb ML practice
library(caret)
library(Boruta)

# DC: Set the wd.
setwd("D:/R/wd")
# If Ubuntu:
setwd("media/danjels/ukb/R/wd")


dat <- readRDS("ukb_ml_test_dataset.rds")
list_int <- read.csv("daniel_vars_interest.csv",stringsAsFactors = F, na.strings = c("NA",""))
# this code removes blank spaces from each list as read.csv will add blanks so that
# all columns have the same length
list_int <- lapply(as.list(list_int), function(x){x[!is.na(x)]})


# Boruta test

data_smoke_highLow <- dat
labels_smoke_highLow <- data_smoke_highLow$group
# remove unwanted variables
rm_list <- c(list_int$imvars_remove,list_int$imvars_remove2,list_int$covars,"eid","group")

data_smoke_highLow <- data_smoke_highLow[,!(names(data_smoke_highLow) %in% rm_list)]

data_smoke_highLow <- data_smoke_highLow[complete.cases(data_smoke_highLow),]

#write.csv(summary(data_smoke_highLow), "data_smoke_summary.csv")

#covar_smoke_highLow <- readRDS("apoe_covar.rds")


#setwd(paste(path,"/boruta_test_ADDEM_NCI", sep=""))

#apoe
#data_labels_smoke_highLow <- cbind(data_smoke_highLow, labels_smoke_highLow)
# altering maxRuns dramatically affects run time but lower numbers can affect stability. Make sure that the
# number of threads is set to an appropriate number for your machine otherwise Boruta will use all available cores
boruta_smoke_highLow_model=Boruta(x=data_smoke_highLow , y=labels_smoke_highLow, doTrace = 2 , maxRuns = 1000,num.threads=4)
saveRDS(boruta_smoke_highLow_model,file="boruta_smoke_highLow_model_predict_packyears.rds")
final_boruta_smoke_highLow_model=TentativeRoughFix(boruta_smoke_highLow_model)
apoe_model_sel_feat=getSelectedAttributes(final_boruta_smoke_highLow_model)
saveRDS(apoe_model_sel_feat,file="smoke_highLow_model_boruta_sel_feat_predict_packyears.rds")

k <-lapply(1:ncol(boruta_smoke_highLow_model$ImpHistory),function(i)
  boruta_smoke_highLow_model$ImpHistory[is.finite(boruta_smoke_highLow_model$ImpHistory[,i]),i])
names(k) <- colnames(boruta_smoke_highLow_model$ImpHistory)
boruta_smoke_highLow_model_feature_rank <- sort(sapply(k,median),decreasing = TRUE)
boruta_smoke_highLow_model_rank_out <- data.frame(features=names(boruta_smoke_highLow_model_feature_rank), z_score=boruta_smoke_highLow_model_feature_rank, row.names=NULL)

# export a csv file with all input variables ranked by z-score
write.csv(boruta_smoke_highLow_model_rank_out,file="boruta_smoke_highLow_model_feature_rank_predict_packyears.csv",row.names = FALSE)


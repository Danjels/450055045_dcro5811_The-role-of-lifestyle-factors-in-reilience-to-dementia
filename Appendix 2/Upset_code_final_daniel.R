library(tidyverse)
library(UpSetR)

setwd("/Users/andrew/Dropbox\ (Sydney\ Uni)/Daniel_project/final_analysis")
test_dat <- readRDS("../input_file_for_riskfactor_overlap.rds")


#######################################################################################################
# code to read in all boruta lists to find overlaps
#######################################################################################################

# its a good idea to start this in a new R session as the list names are the same as before
# parental AD = 0, 1 or 2 parents with AD, binary = yes/no for parents having AD

boruta_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","atrial_fib_icd10","current_smoker",
                "alcohol_over21","parental_ad_binary","heart_failure_icd10","obese_1_0","low_education",
                "hearing_icd10","lonely","pack_years","pm10_2007","parental_ad","townsend_quintile",
                "high_pm10_2007","low_phys_activity","proxy_dementia","tbi_icd10",
                "livingston_3ormore","livingston_4ormore","livingston_number")


boruta_int <- c("livingston_3ormore","livingston_4ormore","livingston_number")

# # these are the results that are here at the moment
# boruta_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","atrial_fib_icd10",
#                 "alcohol_over21","heart_failure_icd10","obese_1_0","townsend_quintile",
#                 "pack_years","pm10_2007","parental_ad")


# set your working directory to where your results are
#setwd("../../prelim_results/")
# this loop outputs the list of participant IDs with each risk factor
# this code block is for the results from females
for (ctrast in boruta_int) {
  #cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_female_noE4.csv",sep = "_"),header = T)
  cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_female_all.csv",sep = "_"),header = T)
  #cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_female_E4only.csv",sep = "_"),header = T)
  # take all measures with a z-score of at least 2 (can change higher/lower)
  cont_int <- cont_int[cont_int$z_score>=2,]
  
  # or take all variables above the shadowmax value
  #cont_int <- cont_int[1:which(grepl("shadowMax", cont_int$features)),]
  
  gl <- as.character(cont_int$features)
  # remove shadowMax from results
  gl <- gl[!gl %in% c("shadowMax")]
  # alternatively you could take the top n features e.g., top 20 below
  #gl <- as.character(cont_int$ensembl_gene_id)[c(1:20)]
  assign(ctrast,gl)
  print(ctrast)
}

# edited to output a vector with the names of the contrast and how many variables came out

list_num <- c("male_all")
# same code as above except for male subjects instead
for (ctrast in boruta_int) {
  #cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_male_noE4.csv",sep = "_"),header = T)
  cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_male_all.csv",sep = "_"),header = T)
  #cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_male_E4only.csv",sep = "_"),header = T)
  # take all measures with a z-score of at least 2 (can change higher/lower)
  cont_int <- cont_int[cont_int$z_score>=2,]
  
  # or take all variables above the shadowmax value
  #cont_int <- cont_int[1:which(grepl("shadowMax", cont_int$features)),]
  
  gl <- as.character(cont_int$features)
  # remove shadowMax from results
  gl <- gl[!gl %in% c("shadowMax")]
  # alternatively you could take the top n features e.g., top 20 below
  #gl <- as.character(cont_int$ensembl_gene_id)[c(1:20)]
  assign(ctrast,gl)
  list_num <- c(list_num,paste(ctrast,length(gl), sep = "_"))
  print(ctrast)
}


# this short loop was used to make the list below
for (ctr in boruta_int) {
  cat(paste(ctr,'=',ctr,',',sep=""))
}
# full list
list_full <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                  depression_icd10=depression_icd10,atrial_fib_icd10=atrial_fib_icd10,
                  current_smoker=current_smoker,alcohol_over21=alcohol_over21,tbi_icd10=tbi_icd10,
                  parental_ad_binary=parental_ad_binary,heart_failure_icd10=heart_failure_icd10,
                  obese_1_0=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                  lonely=lonely,pack_years=pack_years,pm10_2007=pm10_2007,parental_ad=parental_ad,
                  townsend_quintile=townsend_quintile,high_pm10_2007=high_pm10_2007,
                  livingston_3ormore=livingston_3ormore,livingston_4ormore=livingston_4ormore,
                  livingston_number=livingston_number,low_phys_activity=low_phys_activity,
                  proxy_dementia=proxy_dementia)



# empty results removed
list_full <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                  depression_icd10=depression_icd10,atrial_fib_icd10=atrial_fib_icd10,
                  current_smoker=current_smoker,alcohol_over21=alcohol_over21,
                  parental_ad_binary=parental_ad_binary,heart_failure_icd10=heart_failure_icd10,
                  obese_1_0=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                  lonely=lonely,pack_years=pack_years,pm10_2007=pm10_2007,parental_ad=parental_ad,
                  townsend_quintile=townsend_quintile,high_pm10_2007=high_pm10_2007,
                  livingston_3ormore=livingston_3ormore,livingston_4ormore=livingston_4ormore,
                  livingston_number=livingston_number,low_phys_activity=low_phys_activity,
                  proxy_dementia=proxy_dementia)




# this makes a list object that stores the list of participant IDs with each risk factor
# in a single R object. It is the best way of storing vectors of different lengths



# this produces a spreadsheet that shows which subjects have each risk factor
upset_object = map(list_full, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0) 


# make sure matches the number of columns (excluding "target")
upset_object$Total <- rowSums(upset_object[,c(2:23)])

# sort based on total column
upset_object <- upset_object[order(upset_object$Total,decreasing = TRUE),]

# rename based on the group you are looking at (female/male, E4only, noE4, All)
write.csv(upset_object,"boruta_list_full_results_male_E4only.csv",row.names = FALSE)

#######################################################################################################
# livingston only
#######################################################################################################

# all results
list_livingston <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                        depression_icd10=depression_icd10, lonely=lonely,tbi_icd10=tbi_icd10,
                        current_smoker=current_smoker,alcohol_over21=alcohol_over21,
                        obese_1_0=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                        high_pm10_2007=high_pm10_2007,low_phys_activity=low_phys_activity)
# empty removed
list_livingston <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                        depression_icd10=depression_icd10, lonely=lonely,
                        current_smoker=current_smoker,alcohol_over21=alcohol_over21,
                        obese_1_0=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                        high_pm10_2007=high_pm10_2007,low_phys_activity=low_phys_activity)

# livingston
upset_object = map(list_livingston, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0)

# make sure matches the number of columns (excluding "target")
upset_object$Total <- rowSums(upset_object[,c(2:13)])

# sort based on total column
upset_object <- upset_object[order(upset_object$Total,decreasing = TRUE),]

# rename based on the group you are looking at (female/male, E4only, noE4, All)
write.csv(upset_object,"boruta_list_livingston_results_male_E4only.csv",row.names = FALSE)


pdf("upset_livingston_all_female.pdf",height = 20,width = 30)
upset(upset_object, 
      sets = names(list_livingston), 
      sets.bar.color = "#56B4E9",
      order.by = "freq",
      text.scale = 2,
      number.angles=20,
      nintersects = 50)

dev.off()

#######################################################################################################
# end of boruta results
#######################################################################################################



# var_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","low_phys_activity","tbi_icd10",
#              "alcohol_over21","obese_other","low_education","hearing_icd10","lonely","current_smoker",
#              "high_pm10_2007","heart_failure_icd10","parental_ad_binary","parental_ad_both",
#              "atrial_fib_icd10","dementia_all_icd10","apoe4d")

var_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","low_phys_activity","tbi_icd10",
             "alcohol_over21","obese_other","low_education","hearing_icd10","lonely","current_smoker",
             "high_pm10_2007","heart_failure_icd10","parental_ad_both",
             "atrial_fib_icd10","dementia_all_icd10","apoe4d")


# variable is normally distributed so more than one standard deviation (2.716597) away from
# the mean of 21.53 = 24.2466 - rounded to 24.25

# ignore these lines. These show how some variables were encoded
# test_dat$high_pm10_2007 <- rep(0)
# test_dat$high_pm10_2007[test_dat$pm10_2007>24.25] <- 1
# test_dat$high_pm10_2007[is.na(test_dat$pm10_2007)] <- NA
# test_dat$high_pm10_2007 <- as.factor(test_dat$high_pm10_2007)
# 
# test_dat$obese_other <- rep(0)
# test_dat$obese_other[test_dat$obese_binary=="Obese"] <- 1
# test_dat$obese_other[is.na(test_dat$obese_binary)] <- NA
# test_dat$obese_other <- as.factor(test_dat$obese_other)
# 
# test_dat$parental_ad_both <- rep(0)
# test_dat$parental_ad_both[test_dat$parental_ad==2] <- 1
# test_dat$parental_ad_both[is.na(test_dat$parental_ad)] <- NA
# test_dat$parental_ad_both <- as.factor(test_dat$parental_ad_both)


# if you want to run just in apoe4 carriers. you will need to remove "apoe4d"
# from the var_int vector above as well

# i often make a backup of the full size dataframe so that I don't have to read
# the file in again if I stuff something up
test_dat_bup <- test_dat

# now you can just restart from here using the backup object
test_dat <- test_dat_bup
# first remove subjects with missing APOE genotype
test_dat <- test_dat[complete.cases(test_dat$apoe4d),]
# to keep APOE4 carriers run this line where noncarriers are removed
test_dat <- test_dat[!test_dat$apoe4d==0,]
# to remove APOE4 carriers run this line where all APOE4 carriers are removed
test_dat <- test_dat[!test_dat$apoe4d==1,]


# this loop outputs the list of participant IDs with each risk factor
for (ctrast in var_int) {
  dat_input <- test_dat
  dat_input$labels <- dat_input[,names(dat_input) %in% ctrast]
  dat_input <- dat_input[!is.na(dat_input$labels),]
  dat_input <- dat_input[dat_input$labels==1,]
  gl <- dat_input$ukbid
  assign(ctrast,gl)
  print(ctrast)
}

# this short loop was used to make the list below
# for (ctr in var_int) {
#   cat(paste(ctr,'=',ctr,',',sep=""))
# }

# this makes a list object that stores the list of participant IDs with each risk factor
# in a single R object. It is the best way of storing vectors of different lengths
list_full <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                  depression_icd10=depression_icd10,low_phys_activity=low_phys_activity,
                  tbi_icd10=tbi_icd10,alcohol_over21=alcohol_over21,obese=obese_other,
                  low_education=low_education,hearing_icd10=hearing_icd10,lonely=lonely,
                  current_smoker=current_smoker,high_pm10_2007=high_pm10_2007,dementia=dementia_all_icd10,
                  heart_failure_icd10=heart_failure_icd10,parental_ad_binary=parental_ad_binary,
                  parental_ad_both=parental_ad_both,atrial_fib_icd10=atrial_fib_icd10,apoe4d=apoe4d)


# this produces a spreadsheet that shows which subjects have each risk factor
upset_object = map(list_full, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0) 

write.csv(upset_object,"ukbid_risk_factor_overlap_42806_cohort.csv")


#######################################################################################################
# generate upset plot for livingston risk factors
#######################################################################################################


list_livingston <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                        depression_icd10=depression_icd10,low_phys_activity=low_phys_activity,
                        tbi_icd10=tbi_icd10,alcohol_over21=alcohol_over21,obese=obese_other,
                        low_education=low_education,hearing_icd10=hearing_icd10,lonely=lonely,
                        current_smoker=current_smoker,high_pm10_2007=high_pm10_2007)



upset_object = map(list_livingston, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0) 

pdf("upset_livingston_all.pdf",height = 20,width = 30)
upset(upset_object, 
      sets = names(list_livingston), 
      sets.bar.color = "#56B4E9",
      order.by = "freq",
      text.scale = 2.4,
      number.angles=20,
      nintersects = 50)

dev.off()




#######################################################################################################
# code to make summary sheets for individual risk factors
#######################################################################################################

# join together results

# its a good idea to start this in a new R session as the list names are the same as before
# parental AD = 0, 1 or 2 parents with AD, binary = yes/no for parents having AD

boruta_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","atrial_fib_icd10","current_smoker",
                "alcohol_over21","parental_ad_binary","heart_failure_icd10","obese_1_0","low_education",
                "hearing_icd10","lonely","pack_years","pm10_2007","parental_ad","townsend_quintile",
                "high_pm10_2007","low_phys_activity","proxy_dementia","tbi_icd10",
                "livingston_3ormore","livingston_4ormore","livingston_number")


#boruta_int <- c("livingston_3ormore","livingston_4ormore","livingston_number")

# # these are the results that are here at the moment
# boruta_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","atrial_fib_icd10",
#                 "alcohol_over21","heart_failure_icd10","obese_1_0","townsend_quintile",
#                 "pack_years","pm10_2007","parental_ad")


# set your working directory to where your results are
#setwd("../../prelim_results/")
# this loop outputs the list of participant IDs with each risk factor
# this code block is for the results from females


for (ctrast in boruta_int) {
  cont_noe4F <- read.csv(paste(ctrast,"model_feature_rank_predict_female_noE4.csv",sep = "_"),header = T)
  cont_allF <- read.csv(paste(ctrast,"model_feature_rank_predict_female_all.csv",sep = "_"),header = T)
  cont_E4onlyF <- read.csv(paste(ctrast,"model_feature_rank_predict_female_E4only.csv",sep = "_"),header = T)
  cont_noe4M <- read.csv(paste(ctrast,"model_feature_rank_predict_male_noE4.csv",sep = "_"),header = T)
  cont_allM <- read.csv(paste(ctrast,"model_feature_rank_predict_male_all.csv",sep = "_"),header = T)
  cont_E4onlyM <- read.csv(paste(ctrast,"model_feature_rank_predict_male_E4only.csv",sep = "_"),header = T)
  
  # take all measures with a z-score of at least 2 (can change higher/lower)
  # remove shadowMax
  cont_noe4F <- cont_noe4F[cont_noe4F$z_score>=2,]
  cont_noe4F <- cont_noe4F[!cont_noe4F$features=="shadowMax",]
  
  cont_allF <- cont_allF[cont_allF$z_score>=2,]
  cont_allF <- cont_allF[!cont_allF$features=="shadowMax",]
  
  cont_E4onlyF <- cont_E4onlyF[cont_E4onlyF$z_score>=2,]
  cont_E4onlyF <- cont_E4onlyF[!cont_E4onlyF$features=="shadowMax",]
  
  cont_noe4M <- cont_noe4M[cont_noe4M$z_score>=2,]
  cont_noe4M <- cont_noe4M[!cont_noe4M$features=="shadowMax",]
  
  cont_allM <- cont_allM[cont_allM$z_score>=2,]
  cont_allM <- cont_allM[!cont_allM$features=="shadowMax",]
  
  cont_E4onlyM <- cont_E4onlyM[cont_E4onlyM$z_score>=2,]
  cont_E4onlyM <- cont_E4onlyM[!cont_E4onlyM$features=="shadowMax",]
  
  # or take all variables above the shadowmax value
  #cont_int <- cont_int[1:which(grepl("shadowMax", cont_int$features)),]
  
  # convert to character and remove shadowMax from results
  gl_noE4F <- as.character(cont_noe4F$features)
  gl_noE4F <- gl_noE4F[!gl_noE4F %in% c("shadowMax")]
  
  gl_allF <- as.character(cont_allF$features)
  gl_allF <- gl_allF[!gl_allF %in% c("shadowMax")]
  
  gl_E4onlyF <- as.character(cont_E4onlyF$features)
  gl_E4onlyF <- gl_E4onlyF[!gl_E4onlyF %in% c("shadowMax")]
  
  # convert to character and remove shadowMax from results
  gl_noE4M <- as.character(cont_noe4M$features)
  gl_noE4M <- gl_noE4M[!gl_noE4M %in% c("shadowMax")]
  
  gl_allM <- as.character(cont_allM$features)
  gl_allM <- gl_allM[!gl_allM %in% c("shadowMax")]
  
  gl_E4onlyM <- as.character(cont_E4onlyM$features)
  gl_E4onlyM <- gl_E4onlyM[!gl_E4onlyM %in% c("shadowMax")]
  
  list_out <- list(allF=gl_allF,noE4F=gl_noE4F,E4onlyF = gl_E4onlyF,
                   allM=gl_allM,noE4M=gl_noE4M,E4onlyM = gl_E4onlyM)
  
  list_out <- list_out[lengths(list_out) >= 1]
  
  upset_object = map(list_out, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
    bind_rows(.id = "source") %>%
    spread(source, present, 0) 
  
  
  # make sure matches the number of columns (excluding "target")
  upset_object$Total <- rowSums(upset_object[,c(2:ncol(upset_object))])
  
  # sort based on total column
  upset_object <- upset_object[order(upset_object$Total,decreasing = TRUE),]
  
  # rename based on the group you are looking at (female/male, E4only, noE4, All)
  write.csv(upset_object,paste(ctrast,"_only_boruta_summary_upset.csv",sep = ""),row.names = FALSE)
  
  #make a vector for each column that we want in the output (ie variables and z-scores)
  # round the z-scores
  noe4F <- cont_noe4F$features
  noe4F_zscore <- round(cont_noe4F$z_score,3)
  allF <- cont_allF$features
  allF_zscore <- round(cont_allF$z_score,3)
  E4onlyF <- cont_E4onlyF$features
  E4onlyF_zscore <- round(cont_E4onlyF$z_score,3)
  noe4M <- cont_noe4M$features
  noe4M_zscore <- round(cont_noe4M$z_score,3)
  allM <- cont_allM$features
  allM_zscore <- round(cont_allM$z_score,3)
  E4onlyM <- cont_E4onlyM$features
  E4onlyM_zscore <- round(cont_E4onlyM$z_score,3)

  # make the vectors all the same length
  n <- max(length(noe4F), length(allF),length(E4onlyF),
           length(noe4M), length(allM),length(E4onlyM))
  length(noe4F) <- n                      
  length(noe4F_zscore) <- n
  length(allF) <- n
  length(allF_zscore) <- n
  length(E4onlyF) <- n                      
  length(E4onlyF_zscore) <- n
  length(noe4M) <- n                      
  length(noe4M_zscore) <- n
  length(allM) <- n
  length(allM_zscore) <- n
  length(E4onlyM) <- n                      
  length(E4onlyM_zscore) <- n
  
  dat_zscore <- cbind(allF,allF_zscore,allM,allM_zscore,noe4F,noe4F_zscore,noe4M,
                      noe4M_zscore,E4onlyF,E4onlyF_zscore,E4onlyM,E4onlyM_zscore)
  dat_zscore[is.na(dat_zscore)] <- ""
  
  write.csv(dat_zscore, paste(ctrast,"z_score_summary.csv",sep = "_"))
  # alternatively you could take the top n features e.g., top 20 below
  #gl <- as.character(cont_int$ensembl_gene_id)[c(1:20)]
  assign(ctrast,list_out)

  print(ctrast)
  
}

#######################################################################################################
# code to make summary sheets for individual risk factors
#######################################################################################################

boruta_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","atrial_fib_icd10","current_smoker",
                "alcohol_over21","parental_ad_binary","heart_failure_icd10","obese_1_0","low_education",
                "hearing_icd10","lonely","pack_years","pm10_2007","parental_ad","townsend_quintile",
                "high_pm10_2007","low_phys_activity","proxy_dementia","tbi_icd10",
                "livingston_3ormore","livingston_4ormore","livingston_number")

setwd("summary_per_risk_factor/")
# same code as above except for male subjects instead
for (ctrast in boruta_int) {
  #cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_male_noE4.csv",sep = "_"),header = T)
  cont_int <- read.csv(paste(ctrast,"only_boruta_summary_upset.csv",sep = "_"),header = T)
  #cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_male_E4only.csv",sep = "_"),header = T)
  
  gl <- as.character(cont_int$target)
  # remove shadowMax from results
  # alternatively you could take the top n features e.g., top 20 below
  #gl <- as.character(cont_int$ensembl_gene_id)[c(1:20)]
  assign(ctrast,gl)
  print(ctrast)
}



# this short loop was used to make the list below
# for (ctr in boruta_int) {
#   cat(paste(ctr,'=',ctr,',',sep=""))
# }
# full list
list_full <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                  depression_icd10=depression_icd10,atrial_fib_icd10=atrial_fib_icd10,
                  current_smoker=current_smoker,alcohol_over21=alcohol_over21,tbi_icd10=tbi_icd10,
                  parental_ad_binary=parental_ad_binary,heart_failure_icd10=heart_failure_icd10,
                  obesity=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                  lonely=lonely,pack_years=pack_years,pm10_2007=pm10_2007,parental_ad=parental_ad,
                  townsend_quintile=townsend_quintile,high_pm10_2007=high_pm10_2007,
                  livingston_3ormore=livingston_3ormore,livingston_4ormore=livingston_4ormore,
                  livingston_number=livingston_number,low_phys_activity=low_phys_activity,
                  proxy_dementia=proxy_dementia)



# livingston only

list_livingston <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                        depression_icd10=depression_icd10, lonely=lonely,tbi_icd10=tbi_icd10,
                        current_smoker=current_smoker,alcohol_over21=alcohol_over21,
                        obesity=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                        high_pm10_2007=high_pm10_2007,low_phys_activity=low_phys_activity)

# livingston only proxydem

list_livingston_dem <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                        depression_icd10=depression_icd10, lonely=lonely,tbi_icd10=tbi_icd10,
                        current_smoker=current_smoker,alcohol_over21=alcohol_over21,
                        obesity=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                        high_pm10_2007=high_pm10_2007,low_phys_activity=low_phys_activity,
                        proxy_dementia=proxy_dementia)

list_dem <- list(proxy_dementia=proxy_dementia,parental_ad=parental_ad,
                            parental_ad_binary=parental_ad_binary)


# this makes a list object that stores the list of participant IDs with each risk factor
# in a single R object. It is the best way of storing vectors of different lengths


# this produces a spreadsheet that shows which subjects have each risk factor
upset_object = map(list_dem, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0) 


# make sure matches the number of columns (excluding "target")
upset_object$Total <- rowSums(upset_object[,c(2:4)])

# sort based on total column
upset_object <- upset_object[order(upset_object$Total,decreasing = TRUE),]

# rename based on the group you are looking at (female/male, E4only, noE4, All)
write.csv(upset_object,"boruta_DemOnly_total_per_RF_combined.csv",row.names = FALSE)

pdf("upset_DemOnly_combined.pdf",height = 20,width = 30)
upset(upset_object, 
      sets = names(list_dem), 
      sets.bar.color = "#56B4E9",
      order.by = "freq",
      text.scale = 2,
      number.angles=20,
      nintersects = 50)

dev.off()

#######################################################################################################
# livingston only
#######################################################################################################

# all results
list_livingston <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                        depression_icd10=depression_icd10, lonely=lonely,tbi_icd10=tbi_icd10,
                        current_smoker=current_smoker,alcohol_over21=alcohol_over21,
                        obese_1_0=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                        high_pm10_2007=high_pm10_2007,low_phys_activity=low_phys_activity)
# empty removed
list_livingston <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                        depression_icd10=depression_icd10, lonely=lonely,
                        current_smoker=current_smoker,alcohol_over21=alcohol_over21,
                        obese_1_0=obese_1_0,low_education=low_education,hearing_icd10=hearing_icd10,
                        high_pm10_2007=high_pm10_2007,low_phys_activity=low_phys_activity)

# livingston
upset_object = map(list_livingston, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0)

# make sure matches the number of columns (excluding "target")
upset_object$Total <- rowSums(upset_object[,c(2:13)])

# sort based on total column
upset_object <- upset_object[order(upset_object$Total,decreasing = TRUE),]

# rename based on the group you are looking at (female/male, E4only, noE4, All)
write.csv(upset_object,"boruta_list_livingston_results_male_E4only.csv",row.names = FALSE)


pdf("upset_livingston_all_female.pdf",height = 20,width = 30)
upset(upset_object, 
      sets = names(list_livingston), 
      sets.bar.color = "#56B4E9",
      order.by = "freq",
      text.scale = 2,
      number.angles=20,
      nintersects = 50)

dev.off()





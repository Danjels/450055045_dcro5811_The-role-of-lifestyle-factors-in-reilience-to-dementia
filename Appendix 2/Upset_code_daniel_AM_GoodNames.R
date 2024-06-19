library(tidyverse)
library(UpSetR)
setwd("D:/R/wd")
test_dat <- readRDS("input_file_for_riskfactor_overlap.rds")

var_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","low_phys_activity","tbi_icd10",
             "alcohol_over21","obese_other","low_education","hearing_icd10","lonely","current_smoker",
             "high_pm10_2007","heart_failure_icd10","parental_ad_binary","parental_ad_both",
             "atrial_fib_icd10","dementia_all_icd10","apoe4d")

# I added this myself to get good names.
# Error in pdf("upset_livingston_all.pdf", height = 20, width = 20) : 
# cannot open file 'upset_livingston_all.pdf
# This list might not be useful.
# GoodNames <- c("Diabetes", "Hypertension", "Depression", "Low physical activity", "TBI",
#                "Alcohol consumption", "Obesity", "Low education", "Hearing loss", "Lonely",
#                "Smoker", "Poor air quality", "Heart failure", "Either parents with AD",
#                "Both parents with AD", "Atrial fibrillation", "All-cause dementia",
#                "APOE4 positive genotype")

# variable is normally distributed so more than one standard deviation (2.716597) away from
# the mean of 21.53 = 24.2466 - rounded to 24.25

# ignore these lines. These show how some variables were encoded
# test_dat$high_pm10_2007 <- rep(0)
# test_dat$high_pm10_2007[test_dat$pm10_2007>24.25] <- 1
# test_dat$high_pm10_2007[is.na(test_dat$pm10_2007)] <- NA
# test_dat$high_pm10_2007 <- as.factor(test_dat$high_pm10_2007)
# 
# test_dat$obese_other <- rep(0)
# test_dat$obese_other[test_dat$obese_1_0=="Obese"] <- 1
# test_dat$obese_other[is.na(test_dat$obese_1_0)] <- NA
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
#test_dat <- test_dat[!test_dat$apoe4d==0,]
# to remove APOE4 carriers run this line where all APOE4 carriers are removed
#test_dat <- test_dat[!test_dat$apoe4d==1,]


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

#this short loop was used to make the list below
for (ctr in var_int) {
  cat(paste(ctr,'=',ctr,',',sep=""))
}

# this makes a list object that stores the list of participant IDs with each risk factor
# in a single R object. It is the best way of storing vectors of different lengths
list_full <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                  depression_icd10=depression_icd10,low_phys_activity=low_phys_activity,
                  tbi_icd10=tbi_icd10,alcohol_over21=alcohol_over21,obese_other=obese_other,
                  low_education=low_education,hearing_icd10=hearing_icd10,lonely=lonely,
                  current_smoker=current_smoker,high_pm10_2007=high_pm10_2007,
                  heart_failure_icd10=heart_failure_icd10,parental_ad_binary=parental_ad_binary,
                  parental_ad_both=parental_ad_both,atrial_fib_icd10=atrial_fib_icd10,
                  dementia_all_icd10=dementia_all_icd10,apoe4d=apoe4d)


# this produces a spreadsheet that shows which subjects have each risk factor
upset_object = map(list_full, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0) 

write.csv(upset_object,"ukbid_risk_factor_overlap_42806_cohort_DC.csv")


#######################################################################################################
# generate upset plot for livingston risk factors
#######################################################################################################


list_livingston <- list(Diabetes=diabetes_icd10,Hypertension=hypertension_icd10,
                        Depression=depression_icd10,"Low physical activity"=low_phys_activity,
                        TBI=tbi_icd10,Alcohol=alcohol_over21,Obese=obese_other,
                        "Low education"=low_education,"Hearing loss"=hearing_icd10,Lonely=lonely,
                        "Current smoker"=current_smoker,"Air pollution"=high_pm10_2007)



upset_object = map(list_livingston, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0) 

# I added this line to change the names without changing the preceding code.
#names(var_int) <- GoodNames
#names(list_full) <- goodNames


pdf("upset_livingston_all.pdf",height = 20,width = 20)
upset(upset_object, 
      sets = names(list_livingston), 
      sets.bar.color = "#56B4E9",
      order.by = "freq",
      text.scale = 2.4,
      number.angles=20)

dev.off()

# I have made functions to here.
#######################################################################################################
# code to read in all boruta lists to find overlaps
#######################################################################################################

# its a good idea to start this in a new R session as the list names are the same as before

# boruta_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","atrial_fib_icd10",
#                 "alcohol_over21","parental_ad_binary","heart_failure_icd10","obese_1_0","low_education",
#                 "hearing_icd10","lonely","pack_years","pm10_2007","parental_ad","townsend_quintile")



# these are the results that are here at the moment
boruta_int <- c("diabetes_icd10","hypertension_icd10","depression_icd10","atrial_fib_icd10",
                "alcohol_over21","heart_failure_icd10","obese_1_0","townsend_quintile",
                "pack_years","pm10_2007","parental_ad")


# set your working directory to where your results are
#setwd("../../prelim_results/")
# this loop outputs the list of participant IDs with each risk factor
# this code block is for the results from females
for (ctrast in boruta_int) {
  cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_female_noE4.csv",sep = "_"),header = T)
  
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

# same code as above except for male subjects instead
for (ctrast in boruta_int) {
  cont_int <- read.csv(paste(ctrast,"model_feature_rank_predict_male_noE4.csv",sep = "_"),header = T)
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


# this short loop was used to make the list below
for (ctr in boruta_int) {
  cat(paste(ctr,'=',ctr,',',sep=""))
}

# this makes a list object that stores the list of participant IDs with each risk factor
# in a single R object. It is the best way of storing vectors of different lengths
list_full <- list(diabetes_icd10=diabetes_icd10,hypertension_icd10=hypertension_icd10,
                  depression_icd10=depression_icd10,atrial_fib_icd10=atrial_fib_icd10,
                  alcohol_over21=alcohol_over21,heart_failure_icd10=heart_failure_icd10,
                  obese_1_0=obese_1_0,townsend_quintile=townsend_quintile,
                  pack_years=pack_years,pm10_2007=pm10_2007,parental_ad=parental_ad)


# this produces a spreadsheet that shows which subjects have each risk factor
upset_object = map(list_full, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
  bind_rows(.id = "source") %>%
  spread(source, present, 0) 

upset_object$Total <- rowSums(upset_object[,c(2:12)])
upset_object <- upset_object[order(upset_object$Total,decreasing = TRUE),]

write.csv(upset_object,"boruta_prelim_results_female_noE4.csv")




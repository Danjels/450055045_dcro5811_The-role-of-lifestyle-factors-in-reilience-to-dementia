# check for size/balance of groups for ML

# daniel project artemis

library(tidyverse)
library(Boruta)

# set input files
setwd("/Users/andrew/Dropbox\ (Sydney\ Uni)/Daniel_project")
#setwd("/scratch/RDS-FMH-UKB-RW/ukb_daniel_project/boruta_MRI_risk_factors")
in_dat <- readRDS("../../42806_ukb_with_dummyvars.rds")
# read in lists of interest
list_int <- read.csv("daniel_vars_interest.csv",stringsAsFactors = F, na.strings = c("NA",""))
list_int <- lapply(as.list(list_int), function(x){x[!is.na(x)]})

icd10_int <- read.csv("risk_factors_defined.csv", stringsAsFactors = F, na.strings = c("NA",""))
icd10_int <- lapply(as.list(icd10_int), function(x){x[!is.na(x)]})
list_int$covars <- c(list_int$covars,"no_livingston_no_parental_both","obese_1_0")

rm_list <- c(list_int$imvars_remove,list_int$imvars_remove2,list_int$covars,"eid","group")

#add no_livingston_no_parental_both to list_int$covars


vars_final <- c(list_int$imaging_vars2_0,list_int$covars,list_int$freesurfer_only)

# make a new column for controls
in_dat$no_livingston_no_parental_both <- rep(0)
in_dat$no_livingston_no_parental_both[in_dat$eid %in% list_int$no_livingston] <- 1
in_dat$no_livingston_no_parental_both <- as.factor(in_dat$no_livingston_no_parental_both)


# recode obesity variable
in_dat$obese_1_0 <- rep(0)
in_dat$obese_1_0[is.na(in_dat$obese_binary)] <- NA
in_dat$obese_1_0[in_dat$obese_binary=="Obese"] <- 1
in_dat$obese_1_0[in_dat$obese_binary=="Other"] <- 0
in_dat$obese_1_0 <- as.factor(in_dat$obese_1_0)


# heart failure variable as factor
in_dat$heart_failure_icd10 <- as.factor(in_dat$heart_failure_icd10)

dat <- in_dat[,names(in_dat) %in% vars_final]

# only keep subjects with particular variables
dat <- dat[complete.cases(dat$apoe_genotype),]
dat <- dat[!dat$dementia_all_icd10==1,]
dat <- dat[!dat$apoe_genotype=="e4e4",]
dat <- dat[!dat$apoe_genotype=="e2e4",]
dat <- dat[!dat$apoe_genotype=="e3e4",]

dat <- dat[dat$age_when_attended_assessment_centre_f21003_2_0 > 64.99,]
#dat <- dat[c(1:1000,10001:11000),]

# define controls
ctrl <- dat[dat$no_livingston_no_parental_both==1,]
label_ctrl <- ctrl[,names(ctrl) %in% list_int$covars]
ctrl <- ctrl[,!(names(ctrl) %in% rm_list)]

# define cases
cases <- dat[dat$no_livingston_no_parental_both==0,]
label_cases <- cases[,names(cases) %in% list_int$covars]
cases <- cases[,!(names(cases) %in% rm_list)]

var_binary <- c("diabetes_icd10","hypertension_icd10","depression_icd10",
             "alcohol_over21","heart_failure_icd10","obese_1_0","low_education",
             "hearing_icd10","lonely","parental_ad_binary","atrial_fib_icd10")

# var_int <- c("parental_ad_binary","heart_failure_icd10","obese_binary","low_education",
#              "hearing_icd10","lonely")

# var_int <- c("pack_years","pm10_2007","parental_ad","townsend_quintile")
# var_list <- c("diabetes_icd10","hypertension_icd10")
#######################################################################################################
# loop to get group sizes and ages - male no APOE4
#######################################################################################################

sink(file = "male_noE4_test.txt")
for (ctrast in var_binary) {
  # cases
  case_input <- cases
  case_input$labels <- label_cases[,names(label_cases) %in% ctrast]
  case_input$sex <- label_cases$sex_f31_0_0
  case_input$age <- label_cases$age_when_attended_assessment_centre_f21003_2_0
  
  
  # the line below is only for binary variables of interest
  case_input <- case_input[case_input$labels==1,]
  
  case_input <- case_input[!case_input$sex=="Female",]
  case_input <- case_input[complete.cases(case_input),]
  
  # controls
  ctrl_input <- ctrl
  ctrl_input$labels <- label_ctrl[,names(label_ctrl) %in% ctrast]
  ctrl_input$sex <- label_ctrl$sex_f31_0_0
  ctrl_input$age <- label_ctrl$age_when_attended_assessment_centre_f21003_2_0
  
  ctrl_input <- ctrl_input[!ctrl_input$sex=="Female",]
  ctrl_input <- ctrl_input[complete.cases(ctrl_input),]
  
  
  dat_input <- rbind(case_input,ctrl_input)
  
  dat_input <- dat_input[complete.cases(dat_input),]
  print(ctrast)
  print(t.test(dat_input$age~dat_input$labels))
  print(summary(dat_input$labels))

}
sink()


sink(file = "female_noE4_age.txt")
for (ctrast in var_binary) {
  
  # cases
  case_input <- cases
  case_input$labels <- label_cases[,names(label_cases) %in% ctrast]
  case_input$sex <- label_cases$sex_f31_0_0
  case_input$age <- label_cases$age_when_attended_assessment_centre_f21003_2_0
  
  
  # the line below is only for binary variables of interest
  case_input <- case_input[case_input$labels==1,]
  
  case_input <- case_input[!case_input$sex=="Male",]
  case_input <- case_input[complete.cases(case_input),]
  
  # controls
  ctrl_input <- ctrl
  ctrl_input$labels <- label_ctrl[,names(label_ctrl) %in% ctrast]
  ctrl_input$sex <- label_ctrl$sex_f31_0_0
  ctrl_input$age <- label_ctrl$age_when_attended_assessment_centre_f21003_2_0
  
  ctrl_input <- ctrl_input[!ctrl_input$sex=="Male",]
  ctrl_input <- ctrl_input[complete.cases(ctrl_input),]
  
  
  dat_input <- rbind(case_input,ctrl_input)
  
  dat_input <- dat_input[complete.cases(dat_input),]
  print(ctrast)
  print(t.test(dat_input$age~dat_input$labels))
  print(summary(dat_input$labels))
  
}
sink()


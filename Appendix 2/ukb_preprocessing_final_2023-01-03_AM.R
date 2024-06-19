#library(UpSetR)
library(tidyverse)
#library(ukbtools)
library(Boruta)
#library(dplyr)

# basic analyses script
#setwd("/Users/andrew/Dropbox\ (Sydney\ Uni)/Daniel_project")
# replace with where the file is for you
#load("../../Downloads/imaging_only_allfields_names.RData")
load("imaging_only_allfields_names.RData")
# loads a dataframe called dat that has all fields for the subjects with
# neuroimaging data - 
#dat <- dat[order(dat$eid),]

diag_cols <- readRDS("diagnosis_column_names.rds")
list_int <- read.csv("daniel_vars_interest.csv",stringsAsFactors = F, na.strings = c("NA",""))
list_int <- lapply(as.list(list_int), function(x){x[!is.na(x)]})

icd10_int <- read.csv("risk_factors_defined.csv", stringsAsFactors = F, na.strings = c("NA",""))
# an issue with storing columns of different lengths in a csv file is that read.csv will
# treat every column as if it is the same length. This means that blank cells will be added to
# make sure that every column has the same length. dataframes require columns to have the same
# length but lists do not. The following function converts each column of the dataframe to 
# a vector, removes the NAs and then adds them to a list
icd10_int <- lapply(as.list(icd10_int), function(x){x[!is.na(x)]})
# saveRDS(ukb_icd10,"ukb_icd10.rds")
# saveRDS(ukb_selfrep,"ukb_selfrep.rds")
# saveRDS(ukb_ill_father,"ukb_illfather.rds")
# saveRDS(ukb_ill_mother,"ukb_illmother.rds")
# 

# There are over 27,000 variables so you may want to remove some columns that aren't going to
# be used in your analyses. The code below matches a particular string in the column name.
# You can add/remove strings if you like. Removing 4000 variables reduced file size by ~70 MB so
# no real need to do this unless you remove more than that

#library(dplyr)
# dat_trim <- dat %>%
#   select(-matches('method_of_recording|workplace_had|worked_with|night_shift|triplet_played|number_of_times_clear_was_pressed|
#                   keystroke_history|triplet_entered|triplet_correct|time_to_press_first_digit|time_to_press_last_digit|
#                   time_to_press_next|refractometry_result_unreliable|weak_meridian|strong_meridian|asymmetry_index|keratometry_result|
#                   ecg_|operation_code|interpolated_year|types_of_spread|bicycle_speed|regularity_index'))



# make new binary variables based on ICD10 diagnoses
dat <- dat %>%
  mutate(diabetes_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_diabetes))) %>%
  mutate(hearing_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_hearing_loss))) %>%
  mutate(tbi_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_traumatic_brain_injury))) %>%
  mutate(hypertension_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_hypertension))) %>%
  mutate(obesity_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_obesity))) %>%
  mutate(depression_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_depression))) %>%
  mutate(heart_disease_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_chronic_ischaemic_heart_disease))) %>%
  mutate(dementia_all_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_dementia_fields))) %>%
  mutate(alzheimer_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_AD))) %>%
  mutate(vasc_dementia_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_vasc_dementia))) %>%
  mutate(ftd_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_FTD))) %>%
  mutate(atrial_fib_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_atrial_fib)))
# add new variables one at a time
dat <- dat %>%
  mutate(atrial_fib_icd10 = +(if_any(matches(match="^diagnoses.*icd10|contributory_secondary_ca|cause_of_death_icd10"),
                                   ~. %in% icd10_int$icd_atrial_fib)))



  # mutate(hypertension_icd10 = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(hearing_icd10 = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(tbi_icd10 = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(obesity_icd10 = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(depression_icd10 = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(heart_failure_icd10 = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(heart_disease_icd10 = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(hypertension = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(hypertension = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%
  # mutate(hypertension = +(if_any(matches(match="^diagnoses.*icd10"),  ~. %in% c("I10")))) %>%

# add new variables one at a time
dat <- dat %>%
  mutate(diabetes_selfrep = +(if_any(matches(match="^noncancer_illness_code_selfreported_f20002_*"),
                                     ~. %in% c("1223")))) %>%
  mutate(dementia_selfrep = +(if_any(matches(match="^noncancer_illness_code_selfreported_f20002_*"),
                                     ~. %in% c("1263")))) %>%
  mutate(atr_fib_selfrep = +(if_any(matches(match="^noncancer_illness_code_selfreported_f20002_*"),
                                     ~. %in% c("1471","1483"))))


# add in apoe genotype information - done by Hamish previously
# done for full cohort of 502,524 so need to join by eid
apoe_all <- read.csv("full_apoe_genotype.csv")

#glimpse(apoe_all)
# need to remove subjects that don't have neuroimaging. done using the %in% function
# only keep the eids that are in dat (the dat subjects are the ones with neuroimaging)

apoe_dat <- apoe_all[apoe_all$eid %in% dat$eid,]
# before adding the apoe columns to dat - make sure that the eid columns
# are in the same order - done using identical function - returns true or false
identical(apoe_dat$eid,dat$eid)

# assuming true we can add columns simply using the $
dat$apoe_genotype <- apoe_dat$apoe_txt
dat$apoe_genotype <- as.factor(dat$apoe_genotype)

# add a binary variable for apoe4 carrier or not
dat$apoe4d <- apoe_dat$apoe4

# check APOE genotype breakdown of 42806 cohort
summary(dat$apoe_genotype)
# 22 = 191, 23 = 4432, 24 = 839, 33 = 21227, 34 = 8287, 44 = 789
# NA = 7041 (16.4% missing APOE genotype)

# make a new variable for apoe4 dosage ie number of apoe4 alleles
df_ukb65$apoe4_copies <-NULL

df_ukb65$apoe4_copies[df_ukb65$apoe_genotype=="e2e2"] <- 0
df_ukb65$apoe4_copies[df_ukb65$apoe_genotype=="e2e3"] <- 0
df_ukb65$apoe4_copies[df_ukb65$apoe_genotype=="e2e4"] <- 1
df_ukb65$apoe4_copies[df_ukb65$apoe_genotype=="e3e3"] <- 0
df_ukb65$apoe4_copies[df_ukb65$apoe_genotype=="e3e4"] <- 1
df_ukb65$apoe4_copies[df_ukb65$apoe_genotype=="e4e4"] <- 2
df_ukb65$apoe4_copies <- as.factor(df_ukb65$apoe4_copies)

# next step is to make dummy variables that represent the risk factors of interest

# obesity is typically defined as a BMI of over 30
# make a bmi field 
dat$BMI <- dat$weight_f23098_2_0/((dat$height_f12144_2_0/100) ^2)

# make a categorical variable for bmi
dat$BMI_cat[dat$BMI<18.5] <- "Underweight"
dat$BMI_cat[dat$BMI>=18.5 & dat$BMI<25] <- "Normal"
dat$BMI_cat[dat$BMI>=25 & dat$BMI<30] <- "Overweight"
dat$BMI_cat[dat$BMI>=30] <- "Obese"
dat$BMI_cat <- as.factor(dat$BMI_cat)

# make a detailed categorical variable for bmi
dat$BMI_cat_sub[dat$BMI<16.5] <- "Underweight_Severe"
dat$BMI_cat_sub[dat$BMI>=16.5 & dat$BMI<18.5] <- "Underweight"
dat$BMI_cat_sub[dat$BMI>=18.5 & dat$BMI<25] <- "Normal"
dat$BMI_cat_sub[dat$BMI>=25 & dat$BMI<30] <- "Overweight"
dat$BMI_cat_sub[dat$BMI>=30 & dat$BMI<35] <- "Obese_Class1"
dat$BMI_cat_sub[dat$BMI>=35 & dat$BMI<40] <- "Obese_Class2"
dat$BMI_cat_sub[dat$BMI>=40] <- "Obese_Class3"
dat$BMI_cat_sub <- as.factor(dat$BMI_cat_sub)

# make binary obesity variable
dat$obese_binary[dat$BMI<30] <- "Other"
dat$obese_binary[dat$BMI>=30] <- "Obese"
dat$obese_binary <- as.factor(dat$obese_binary)

# previous studies have split the townsend deprivation index into quintiles
# while not a risk factor - it is commonly included in UKB analyses
# townsend quintiles - townsend_deprivation_index_at_recruitment_f189_0_0
dat$townsend_quintile <-cut(dat$townsend_deprivation_index_at_recruitment_f189_0_0,
                            quantile(dat$townsend_deprivation_index_at_recruitment_f189_0_0, prob=0:5 / 5, na.rm=T),
                            include.lowest=TRUE,labels=FALSE)


# original ukb prevalence function recoded to give total number for each diagnosis
# this function can be used as shown in line underneath
ukb_icd_total <- function(data, icd.code, icd.version = 10) {
  
  ukb_case <- data %>%
    dplyr::select(matches(paste("^diagnoses.*icd", icd.version, sep = ""))) %>%
    purrr::map_df(~ grepl(icd.code, ., perl = TRUE)) %>%
    rowSums() > 0
  
  sum(ukb_case, na.rm = TRUE)
}

# example usage ukb_icd_total(dat,"Q796")

dat <- dat %>%
  mutate(mother_AD = +(if_any(matches(match="^illnesses_of_mother_f20110*"),
                                     ~. %in% c("Alzheimer's disease/dementia"))))
dat$mother_AD[dat$illnesses_of_mother_f20110_0_0=="Prefer not to answer (group 1)"] <- NA
dat$mother_AD[dat$illnesses_of_mother_f20110_0_0=="Do not know (group 1)"] <- NA
dat$mother_AD[is.na(dat$illnesses_of_mother_f20110_0_0)] <- NA


dat <- dat %>%
  mutate(father_AD = +(if_any(matches(match="^illnesses_of_father_f20107*"),
                              ~. %in% c("Alzheimer's disease/dementia"))))
dat$father_AD[dat$illnesses_of_father_f20107_0_0=="Prefer not to answer (group 1)"] <- NA
dat$father_AD[dat$illnesses_of_father_f20107_0_0=="Do not know (group 1)"] <- NA
dat$father_AD[is.na(dat$illnesses_of_father_f20107_0_0)] <- NA


dat$mother_AD <- as.factor(dat$mother_AD)
summary(dat$mother_AD)

# make a column of zeroes to start with and then define levels
dat$parental_ad <- rep(0)
dat$parental_ad[is.na(dat$father_AD) & is.na(dat$mother_AD) ] <- NA
dat$parental_ad[dat$father_AD==1 & dat$mother_AD==1 ] <- 2
dat$parental_ad[dat$father_AD==1 & dat$mother_AD==0 ] <- 1
dat$parental_ad[dat$father_AD==0 & dat$mother_AD==1 ] <- 1
dat$parental_ad <- as.factor(dat$parental_ad)


dat$parental_ad_binary <- rep(0)
dat$parental_ad_binary[is.na(dat$parental_ad)] <- NA
dat$parental_ad_binary[dat$parental_ad==1] <- 1
dat$parental_ad_binary[dat$parental_ad==2] <- 1
dat$parental_ad_binary <- as.factor(dat$parental_ad_binary)



summary(dat$parental_ad)
# use levels(dat$illnesses_of_father_f20107_0_0) to check levels

# low physical activity
dat$low_phys_activity <- rep(0)
dat$low_phys_activity[is.na(dat$ipaq_activity_group_f22032_0_0)] <- NA
dat$low_phys_activity[dat$ipaq_activity_group_f22032_0_0=="low"] <- 1
dat$low_phys_activity[dat$ipaq_activity_group_f22032_0_0=="moderate"] <- 0
dat$low_phys_activity[dat$ipaq_activity_group_f22032_0_0=="high"] <- 0
dat$low_phys_activity <- as.factor(dat$low_phys_activity)


dat$previous_myo_infarct[!is.na(dat$date_of_myocardial_infarction_f42000_0_0)] <- 1
dat$previous_myo_infarct[is.na(dat$date_of_myocardial_infarction_f42000_0_0)] <- 0

dat$previous_stroke[!is.na(dat$date_of_stroke_f42006_0_0)] <- 1
dat$previous_stroke[is.na(dat$date_of_stroke_f42006_0_0)] <- 0

# listed as algorithmically defined in Tai paper with the following field IDs
#date_of_myocardial_infarction_f42000_0_0
#date_of_stroke_f42006_0_0


dat$sex <- as.factor(dat$sex_f31_0_0)
dat$age <- dat$age_when_attended_assessment_centre_f21003_2_0
dat$age_dec[dat$age>40 & dat$age<50] <- "40_49"
dat$age_dec[dat$age>49.999 & dat$age<60] <- "50_59"
dat$age_dec[dat$age>59.999 & dat$age<70] <- "60_69"
dat$age_dec[dat$age>69.999 & dat$age<80] <- "70_79"
dat$age_dec[dat$age>79.999 & dat$age<90] <- "80_89"
dat$age_dec <- as.factor(dat$age_dec)

dat$low_education <- rep(0)
dat$low_education[dat$qualifications_f6138_0_0=="None of the above"] <- 1
dat$low_education[is.na(dat$qualifications_f6138_0_0)] <- NA

dat$pack_years <- dat$pack_years_of_smoking_f20161_2_0
dat$pack_years[is.na(dat$pack_years_of_smoking_f20161_2_0)] <- 0
dat$pack_years[is.na(dat$ever_smoked_f20160_2_0)] <- NA

dat$alcohol_binge <- rep(0)
dat$alcohol_binge[dat$frequency_of_consuming_six_or_more_units_of_alcohol_f20416_0_0=="Monthly"] <- 1
dat$alcohol_binge[dat$frequency_of_consuming_six_or_more_units_of_alcohol_f20416_0_0=="Weekly"] <- 1
dat$alcohol_binge[dat$frequency_of_consuming_six_or_more_units_of_alcohol_f20416_0_0=="Daily or almost daily"] <- 1
dat$alcohol_binge[is.na(dat$ever_smoked_f20160_2_0)] <- NA

dat$alcohol_over21 <- rep(0)
dat$alcohol_over21[dat$alcohol_intake_frequency_f1558_2_0=="Daily or almost daily" & dat$amount_of_alcohol_drunk_on_a_typical_drinking_day_f20403_0_0=="10 or more" ] <- 1
dat$alcohol_over21[dat$alcohol_intake_frequency_f1558_2_0=="Daily or almost daily" & dat$amount_of_alcohol_drunk_on_a_typical_drinking_day_f20403_0_0=="7, 8 or 9" ] <- 1
dat$alcohol_over21[dat$alcohol_intake_frequency_f1558_2_0=="Daily or almost daily" & dat$amount_of_alcohol_drunk_on_a_typical_drinking_day_f20403_0_0=="5 or 6" ] <- 1
dat$alcohol_over21[dat$alcohol_intake_frequency_f1558_2_0=="Three or four times a week" & dat$amount_of_alcohol_drunk_on_a_typical_drinking_day_f20403_0_0=="5 or 6" ] <- 1
dat$alcohol_over21[dat$alcohol_intake_frequency_f1558_2_0=="Three or four times a week" & dat$amount_of_alcohol_drunk_on_a_typical_drinking_day_f20403_0_0=="7, 8 or 9" ] <- 1
dat$alcohol_over21[dat$alcohol_intake_frequency_f1558_2_0=="Three or four times a week" & dat$amount_of_alcohol_drunk_on_a_typical_drinking_day_f20403_0_0=="10 or more" ] <- 1
dat$alcohol_over21[is.na(dat$alcohol_intake_frequency_f1558_2_0)] <- NA

dat$lonely <- rep(0)
dat$lonely[dat$loneliness_isolation_f2020_2_0=="Yes"] <- 1
dat$lonely[dat$loneliness_isolation_f2020_2_0=="Prefer not to answer"] <- NA
dat$lonely[is.na(dat$loneliness_isolation_f2020_2_0)] <- NA

dat$current_smoker <- rep(0)
dat$current_smoker[dat$smoking_status_f20116_2_0=="Current"] <- 1
dat$current_smoker[dat$smoking_status_f20116_2_0=="Prefer not to answer"] <- NA
dat$current_smoker[is.na(dat$smoking_status_f20116_2_0)] <- NA


# set threshold of 1 standard deviation above the mean (24.25)
dat$high_pm10_2007 <- rep(0)
dat$high_pm10_2007[dat$pm10_2007>24.25] <- 1
dat$high_pm10_2007[is.na(dat$pm10_2007)] <- NA
dat$high_pm10_2007 <- as.factor(dat$high_pm10_2007)

# both parents having dementia. exclude those with only one
dat$parental_ad_both <- rep(0)
dat$parental_ad_both[dat$parental_ad==2] <- 1
dat$parental_ad_both[dat$parental_ad==1] <- NA
dat$parental_ad_both[is.na(dat$parental_ad)] <- NA
dat$parental_ad_both <- as.factor(dat$parental_ad_both)

# make a new column for controls
dat$no_livingston_no_parental_both <- rep(0)
dat$no_livingston_no_parental_both[dat$eid %in% list_int$no_livingston] <- 1
dat$no_livingston_no_parental_both <- as.factor(dat$no_livingston_no_parental_both)

dat$pm10_2007 <- dat$particulate_matter_air_pollution_pm10_2007_f24019_0_0
dat$pm25_2010 <- dat$particulate_matter_air_pollution_pm25_2010_f24006_0_0
dat$systolic_bp <- dat$systolic_brachial_blood_pressure_f12697_2_0

tail(names(dat))

#saveRDS(dat,"../../42806_ukb_with_dummyvars.rds")
saveRDS(dat,"42806_ukb_with_dummyvars.rds")


#######################################################################################################
# define controls - people with no livingston risk and no parental both
#######################################################################################################

dat <- in_dat[,names(in_dat) %in% vars_final]

dat <- dat[complete.cases(dat$apoe_genotype),]
dat <- dat[!dat$dementia_all_icd10==1,]
dat <- dat[!dat$diabetes_icd10==1,]
dat <- dat[!dat$hearing_icd10==1,]
dat <- dat[!dat$tbi_icd10==1,]
dat <- dat[!dat$obesity_icd10==1,]
dat <- dat[!dat$depression_icd10==1,]
dat <- dat[!dat$heart_disease_icd10==1,]

dat <- dat[complete.cases(dat$obese_binary),]
dat <- dat[complete.cases(dat$current_smoker),]
dat <- dat[complete.cases(dat$alcohol_over21),]
dat <- dat[complete.cases(dat$lonely),]
dat <- dat[complete.cases(dat$low_phys_activity),]
dat <- dat[complete.cases(dat$high_pm10_2007),]
dat <- dat[complete.cases(dat$low_education),]
dat <- dat[complete.cases(dat$parental_ad),]


dat <- dat[!(dat$obese_binary=="Obese"),]
dat <- dat[!dat$current_smoker==1,]
dat <- dat[!dat$alcohol_over21==1,]
dat <- dat[!dat$lonely==1,]
dat <- dat[!dat$prev_MI==1,]
dat <- dat[!dat$previous_stroke==1,]
dat <- dat[!dat$low_phys_activity==1,]
dat <- dat[!dat$high_pm10_2007==1,]
dat <- dat[!dat$parental_ad==2,]
dat <- dat[!dat$low_education==1,]

write.csv(dat$eid,"no_livingston_no_parental_both.csv")

write.csv(names(dat),"extra_vars.csv")

# define practice dataset

vars_final <- c(list_int$imaging_vars2_0,list_int$covars,list_int$freesurfer_only)

dat_prac <- dat[,names(dat) %in% vars_final]
# saveRDS(dat_prac, "42806_cohort_vars_int_mri_only.rds")

dat_prac <- dat_prac[complete.cases(dat_prac$apoe_genotype),]
dat_prac <- dat_prac[complete.cases(dat_prac$BMI),]
dat_prac <- dat_prac[complete.cases(dat_prac$pack_years),]
dat_prac <- dat_prac[complete.cases(dat_prac$alcohol_over21),]
dat_prac <- dat_prac[complete.cases(dat_prac$mother_AD),]
dat_prac <- dat_prac[complete.cases(dat_prac$father_AD),]

dat_prac <- dat_prac[dat_prac$age_when_attended_assessment_centre_f21003_2_0 > 65,]
# random freesurfer variable to make sure they have freesurfer data
dat_prac <- dat_prac[complete.cases(dat_prac$volume_of_sprecentralsuppart_right_hemisphere_f27767_2_0),]
# random FA variable to make sure that they have tractography data
dat_prac <- dat_prac[complete.cases(dat_prac$mean_isovf_in_body_of_corpus_callosum_on_fa_skeleton_f25443_2_0),]


dat_nosmoke <- dat_prac[dat_prac$pack_years==0,]
dat_ctrl <- dat_nosmoke[sample(nrow(dat_nosmoke), 50), ]
dat_smoke <- dat_prac[dat_prac$pack_years > 65,]

dat_test <- rbind(dat_ctrl, dat_smoke)
dat_test$group[dat_test$pack_years > 1] <- "high_smoker"
dat_test$group[dat_test$pack_years==0] <- "no_smoker"
dat_test$group <- as.factor(dat_test$group)

# Boruta test

data_smoke_highLow <- dat_test
labels_smoke_highLow <- data_smoke_highLow$group
# remove unwanted variables
rm_list <- c(list_int$imvars_remove,list_int$imvars_remove2,"eid","group")
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
boruta_smoke_highLow_model=Boruta(x=data_smoke_highLow , y=labels_smoke_highLow, doTrace = 2 , maxRuns = 1000,num.threads=5)
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



data_smoke_highLow <- dat_test


# remove unwanted variables
rm_list <- c(list_int$imvars_remove,list_int$imvars_remove2,"eid")
rm_list <- c(list_int$imvars_remove,list_int$imvars_remove2,list_int$covars,"eid")

data_smoke_highLow <- data_smoke_highLow[,!(names(data_smoke_highLow) %in% rm_list)]

data_smoke_highLow <- data_smoke_highLow[complete.cases(data_smoke_highLow),]



# t.test
t.test(dat$age_when_attended_assessment_centre_f21003_2_0 ~ as.factor(dat$sex_f31_0_0))
t.test(dat$volume_of_grey_matter_normalised_for_head_size_f25005_2_0 ~ as.factor(dat$sex_f31_0_0))
t.test(dat$volume_of_grey_matter_f25006_2_0 ~ as.factor(dat$sex_f31_0_0))

# histogram
hist(dat$age_when_attended_assessment_centre_f21003_2_0)
hist(dat$volume_of_grey_matter_f25006_2_0,breaks = 20)

# code to use complete.cases 
dat_t1 <- dat[complete.cases(dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0),]
dat_2 <- dat[complete.cases(dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_3_0),]

# list_selfrep <- grep("noncancer_illness_code_selfreported_f20002", colnames(dat) )
# dat_t <- dat[,c(1,list_selfrep)]
#length(which(dat$noncancer_illness_code_selfreported_f20002_0_0 == 1263))

# # replaced the number at the end with the code
# dat_n <- dat_t %>% filter_all(any_vars(. %in% c(1086))) 
# 
# self_rep_codes <- c(1263,1220,1222)
# 
# for (var in self_rep_codes) {
#   dat_n <- dat_t %>% filter_all(any_vars(. %in% c(var)))
#   # rename deg object according to the name of the contrast
#   gl <- dat_n$eid
#   assign(paste("list_test_",var,sep=""),gl)
#   print(var)
# }

#either name manually or use following code to make a list
# for (ctr in self_rep_codes) {
#   cat(paste("list_",ctr,'=',"list_",ctr,',',sep=""))
# }
# 
# 
# # nothing for 1244 - CNS infection - manual naming
# list_full_all <- list(dementia=list_1263,diabetes_all=list_1220,t1_diabetes=list_1222,t2_diabetes=list_1223,
#                       atrial_fib1=list_1471, atrial_fib2=list_1483,periph_vasc_dis1=list_1067,periph_vasc_dis2=list_1087,
#                       encephalitis=list_1246,meningitis=list_1247, ALS=list_1259,MS=list_1261, head_inj=list_1266,
#                       subdural_haem=list_1083,SA_haem=list_1086)
# 
# 
# 
# data_ukb_comb = map(list_full_all, ~ data.frame(target = .x, present = 1L,stringsAsFactors = F)) %>%
#   bind_rows(.id = "source") %>%
#   spread(source, present, 0) 
# 
# write.csv(data_ukb_comb,"any_self_report.csv")
# 
# # nothing for 1244 - CNS infection
# df_full <- data.frame(list(dementia=list_1263,diabetes_all=list_1220,t1_diabetes=list_1222,t2_diabetes=list_1223,
#                            atrial_fib1=list_1471, atrial_fib2=list_1483,periph_vasc_dis1=list_1067,periph_vasc_dis1=list_1087,
#                            encephalitis=list_1246,meningitis=list_1247, ALS=list_1259,MS=list_1261, head_inj=list_1266,
#                            subdural_haem=list_1083,SA_haem=list_1086))
# 
# #df %>% filter(across((c), any_vars(. %in% c('M017', 'M018')))                                                                                                     
# 
# cat(capture.output(print(list_full), file="test.txt"))
# 
# #define file name
# sink('sink_test.txt')
# 
# #print my_list to file
# print(list_full)
# 
# #close external connection to file 
# sink()

# normalising all volume fields - there is a tidier way of doing this but given how
# many variables have "volume" in their name it was easier and a lower chance of error
#

# for (ctr in c_list) {
#   cat(paste('dat$',ctr,' <- dat$',ctr,' * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 \n',sep=""))
# }



# normalise volume columns
dat$volume_of_peripheral_cortical_grey_matter_f25002_2_0 <- dat$volume_of_peripheral_cortical_grey_matter_f25002_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_ventricular_cerebrospinal_fluid_f25004_2_0 <- dat$volume_of_ventricular_cerebrospinal_fluid_f25004_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_f25006_2_0 <- dat$volume_of_grey_matter_f25006_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_white_matter_f25008_2_0 <- dat$volume_of_white_matter_f25008_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_brain_greywhite_matter_f25010_2_0 <- dat$volume_of_brain_greywhite_matter_f25010_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_thalamus_left_f25011_2_0 <- dat$volume_of_thalamus_left_f25011_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_thalamus_right_f25012_2_0 <- dat$volume_of_thalamus_right_f25012_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_caudate_left_f25013_2_0 <- dat$volume_of_caudate_left_f25013_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_caudate_right_f25014_2_0 <- dat$volume_of_caudate_right_f25014_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_putamen_left_f25015_2_0 <- dat$volume_of_putamen_left_f25015_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_putamen_right_f25016_2_0 <- dat$volume_of_putamen_right_f25016_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_pallidum_left_f25017_2_0 <- dat$volume_of_pallidum_left_f25017_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_pallidum_right_f25018_2_0 <- dat$volume_of_pallidum_right_f25018_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_hippocampus_left_f25019_2_0 <- dat$volume_of_hippocampus_left_f25019_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_hippocampus_right_f25020_2_0 <- dat$volume_of_hippocampus_right_f25020_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_amygdala_left_f25021_2_0 <- dat$volume_of_amygdala_left_f25021_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_amygdala_right_f25022_2_0 <- dat$volume_of_amygdala_right_f25022_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_accumbens_left_f25023_2_0 <- dat$volume_of_accumbens_left_f25023_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_accumbens_right_f25024_2_0 <- dat$volume_of_accumbens_right_f25024_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_brain_stem_4th_ventricle_f25025_2_0 <- dat$volume_of_brain_stem_4th_ventricle_f25025_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$total_volume_of_white_matter_hyperintensities_from_t1_and_t2_flair_images_f25781_2_0 <- dat$total_volume_of_white_matter_hyperintensities_from_t1_and_t2_flair_images_f25781_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_frontal_pole_left_f25782_2_0 <- dat$volume_of_grey_matter_in_frontal_pole_left_f25782_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_frontal_pole_right_f25783_2_0 <- dat$volume_of_grey_matter_in_frontal_pole_right_f25783_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_insular_cortex_left_f25784_2_0 <- dat$volume_of_grey_matter_in_insular_cortex_left_f25784_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_insular_cortex_right_f25785_2_0 <- dat$volume_of_grey_matter_in_insular_cortex_right_f25785_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_superior_frontal_gyrus_left_f25786_2_0 <- dat$volume_of_grey_matter_in_superior_frontal_gyrus_left_f25786_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_superior_frontal_gyrus_right_f25787_2_0 <- dat$volume_of_grey_matter_in_superior_frontal_gyrus_right_f25787_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_middle_frontal_gyrus_left_f25788_2_0 <- dat$volume_of_grey_matter_in_middle_frontal_gyrus_left_f25788_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_middle_frontal_gyrus_right_f25789_2_0 <- dat$volume_of_grey_matter_in_middle_frontal_gyrus_right_f25789_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_frontal_gyrus_pars_triangularis_left_f25790_2_0 <- dat$volume_of_grey_matter_in_inferior_frontal_gyrus_pars_triangularis_left_f25790_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_frontal_gyrus_pars_triangularis_right_f25791_2_0 <- dat$volume_of_grey_matter_in_inferior_frontal_gyrus_pars_triangularis_right_f25791_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_frontal_gyrus_pars_opercularis_left_f25792_2_0 <- dat$volume_of_grey_matter_in_inferior_frontal_gyrus_pars_opercularis_left_f25792_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_frontal_gyrus_pars_opercularis_right_f25793_2_0 <- dat$volume_of_grey_matter_in_inferior_frontal_gyrus_pars_opercularis_right_f25793_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_precentral_gyrus_left_f25794_2_0 <- dat$volume_of_grey_matter_in_precentral_gyrus_left_f25794_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_precentral_gyrus_right_f25795_2_0 <- dat$volume_of_grey_matter_in_precentral_gyrus_right_f25795_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_temporal_pole_left_f25796_2_0 <- dat$volume_of_grey_matter_in_temporal_pole_left_f25796_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_temporal_pole_right_f25797_2_0 <- dat$volume_of_grey_matter_in_temporal_pole_right_f25797_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_superior_temporal_gyrus_anterior_division_left_f25798_2_0 <- dat$volume_of_grey_matter_in_superior_temporal_gyrus_anterior_division_left_f25798_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_superior_temporal_gyrus_anterior_division_right_f25799_2_0 <- dat$volume_of_grey_matter_in_superior_temporal_gyrus_anterior_division_right_f25799_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_superior_temporal_gyrus_posterior_division_left_f25800_2_0 <- dat$volume_of_grey_matter_in_superior_temporal_gyrus_posterior_division_left_f25800_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_superior_temporal_gyrus_posterior_division_right_f25801_2_0 <- dat$volume_of_grey_matter_in_superior_temporal_gyrus_posterior_division_right_f25801_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_middle_temporal_gyrus_anterior_division_left_f25802_2_0 <- dat$volume_of_grey_matter_in_middle_temporal_gyrus_anterior_division_left_f25802_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_middle_temporal_gyrus_anterior_division_right_f25803_2_0 <- dat$volume_of_grey_matter_in_middle_temporal_gyrus_anterior_division_right_f25803_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_middle_temporal_gyrus_posterior_division_left_f25804_2_0 <- dat$volume_of_grey_matter_in_middle_temporal_gyrus_posterior_division_left_f25804_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_middle_temporal_gyrus_posterior_division_right_f25805_2_0 <- dat$volume_of_grey_matter_in_middle_temporal_gyrus_posterior_division_right_f25805_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_middle_temporal_gyrus_temporooccipital_part_left_f25806_2_0 <- dat$volume_of_grey_matter_in_middle_temporal_gyrus_temporooccipital_part_left_f25806_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_middle_temporal_gyrus_temporooccipital_part_right_f25807_2_0 <- dat$volume_of_grey_matter_in_middle_temporal_gyrus_temporooccipital_part_right_f25807_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_temporal_gyrus_anterior_division_left_f25808_2_0 <- dat$volume_of_grey_matter_in_inferior_temporal_gyrus_anterior_division_left_f25808_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_temporal_gyrus_anterior_division_right_f25809_2_0 <- dat$volume_of_grey_matter_in_inferior_temporal_gyrus_anterior_division_right_f25809_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_temporal_gyrus_posterior_division_left_f25810_2_0 <- dat$volume_of_grey_matter_in_inferior_temporal_gyrus_posterior_division_left_f25810_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_temporal_gyrus_posterior_division_right_f25811_2_0 <- dat$volume_of_grey_matter_in_inferior_temporal_gyrus_posterior_division_right_f25811_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_temporal_gyrus_temporooccipital_part_left_f25812_2_0 <- dat$volume_of_grey_matter_in_inferior_temporal_gyrus_temporooccipital_part_left_f25812_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_inferior_temporal_gyrus_temporooccipital_part_right_f25813_2_0 <- dat$volume_of_grey_matter_in_inferior_temporal_gyrus_temporooccipital_part_right_f25813_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_postcentral_gyrus_left_f25814_2_0 <- dat$volume_of_grey_matter_in_postcentral_gyrus_left_f25814_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_postcentral_gyrus_right_f25815_2_0 <- dat$volume_of_grey_matter_in_postcentral_gyrus_right_f25815_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_superior_parietal_lobule_left_f25816_2_0 <- dat$volume_of_grey_matter_in_superior_parietal_lobule_left_f25816_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_superior_parietal_lobule_right_f25817_2_0 <- dat$volume_of_grey_matter_in_superior_parietal_lobule_right_f25817_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_supramarginal_gyrus_anterior_division_left_f25818_2_0 <- dat$volume_of_grey_matter_in_supramarginal_gyrus_anterior_division_left_f25818_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_supramarginal_gyrus_anterior_division_right_f25819_2_0 <- dat$volume_of_grey_matter_in_supramarginal_gyrus_anterior_division_right_f25819_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_supramarginal_gyrus_posterior_division_left_f25820_2_0 <- dat$volume_of_grey_matter_in_supramarginal_gyrus_posterior_division_left_f25820_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_supramarginal_gyrus_posterior_division_right_f25821_2_0 <- dat$volume_of_grey_matter_in_supramarginal_gyrus_posterior_division_right_f25821_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_angular_gyrus_left_f25822_2_0 <- dat$volume_of_grey_matter_in_angular_gyrus_left_f25822_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_angular_gyrus_right_f25823_2_0 <- dat$volume_of_grey_matter_in_angular_gyrus_right_f25823_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_lateral_occipital_cortex_superior_division_left_f25824_2_0 <- dat$volume_of_grey_matter_in_lateral_occipital_cortex_superior_division_left_f25824_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_lateral_occipital_cortex_superior_division_right_f25825_2_0 <- dat$volume_of_grey_matter_in_lateral_occipital_cortex_superior_division_right_f25825_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_lateral_occipital_cortex_inferior_division_left_f25826_2_0 <- dat$volume_of_grey_matter_in_lateral_occipital_cortex_inferior_division_left_f25826_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_lateral_occipital_cortex_inferior_division_right_f25827_2_0 <- dat$volume_of_grey_matter_in_lateral_occipital_cortex_inferior_division_right_f25827_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_intracalcarine_cortex_left_f25828_2_0 <- dat$volume_of_grey_matter_in_intracalcarine_cortex_left_f25828_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_intracalcarine_cortex_right_f25829_2_0 <- dat$volume_of_grey_matter_in_intracalcarine_cortex_right_f25829_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_frontal_medial_cortex_left_f25830_2_0 <- dat$volume_of_grey_matter_in_frontal_medial_cortex_left_f25830_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_frontal_medial_cortex_right_f25831_2_0 <- dat$volume_of_grey_matter_in_frontal_medial_cortex_right_f25831_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_juxtapositional_lobule_cortex_formerly_supplementary_motor_cortex_left_f25832_2_0 <- dat$volume_of_grey_matter_in_juxtapositional_lobule_cortex_formerly_supplementary_motor_cortex_left_f25832_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_juxtapositional_lobule_cortex_formerly_supplementary_motor_cortex_right_f25833_2_0 <- dat$volume_of_grey_matter_in_juxtapositional_lobule_cortex_formerly_supplementary_motor_cortex_right_f25833_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_subcallosal_cortex_left_f25834_2_0 <- dat$volume_of_grey_matter_in_subcallosal_cortex_left_f25834_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_subcallosal_cortex_right_f25835_2_0 <- dat$volume_of_grey_matter_in_subcallosal_cortex_right_f25835_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_paracingulate_gyrus_left_f25836_2_0 <- dat$volume_of_grey_matter_in_paracingulate_gyrus_left_f25836_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_paracingulate_gyrus_right_f25837_2_0 <- dat$volume_of_grey_matter_in_paracingulate_gyrus_right_f25837_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_cingulate_gyrus_anterior_division_left_f25838_2_0 <- dat$volume_of_grey_matter_in_cingulate_gyrus_anterior_division_left_f25838_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_cingulate_gyrus_anterior_division_right_f25839_2_0 <- dat$volume_of_grey_matter_in_cingulate_gyrus_anterior_division_right_f25839_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_cingulate_gyrus_posterior_division_left_f25840_2_0 <- dat$volume_of_grey_matter_in_cingulate_gyrus_posterior_division_left_f25840_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_cingulate_gyrus_posterior_division_right_f25841_2_0 <- dat$volume_of_grey_matter_in_cingulate_gyrus_posterior_division_right_f25841_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_precuneous_cortex_left_f25842_2_0 <- dat$volume_of_grey_matter_in_precuneous_cortex_left_f25842_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_precuneous_cortex_right_f25843_2_0 <- dat$volume_of_grey_matter_in_precuneous_cortex_right_f25843_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_cuneal_cortex_left_f25844_2_0 <- dat$volume_of_grey_matter_in_cuneal_cortex_left_f25844_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_cuneal_cortex_right_f25845_2_0 <- dat$volume_of_grey_matter_in_cuneal_cortex_right_f25845_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_frontal_orbital_cortex_left_f25846_2_0 <- dat$volume_of_grey_matter_in_frontal_orbital_cortex_left_f25846_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_frontal_orbital_cortex_right_f25847_2_0 <- dat$volume_of_grey_matter_in_frontal_orbital_cortex_right_f25847_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_parahippocampal_gyrus_anterior_division_left_f25848_2_0 <- dat$volume_of_grey_matter_in_parahippocampal_gyrus_anterior_division_left_f25848_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_parahippocampal_gyrus_anterior_division_right_f25849_2_0 <- dat$volume_of_grey_matter_in_parahippocampal_gyrus_anterior_division_right_f25849_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_parahippocampal_gyrus_posterior_division_left_f25850_2_0 <- dat$volume_of_grey_matter_in_parahippocampal_gyrus_posterior_division_left_f25850_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_parahippocampal_gyrus_posterior_division_right_f25851_2_0 <- dat$volume_of_grey_matter_in_parahippocampal_gyrus_posterior_division_right_f25851_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_lingual_gyrus_left_f25852_2_0 <- dat$volume_of_grey_matter_in_lingual_gyrus_left_f25852_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_lingual_gyrus_right_f25853_2_0 <- dat$volume_of_grey_matter_in_lingual_gyrus_right_f25853_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_temporal_fusiform_cortex_anterior_division_left_f25854_2_0 <- dat$volume_of_grey_matter_in_temporal_fusiform_cortex_anterior_division_left_f25854_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_temporal_fusiform_cortex_anterior_division_right_f25855_2_0 <- dat$volume_of_grey_matter_in_temporal_fusiform_cortex_anterior_division_right_f25855_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_temporal_fusiform_cortex_posterior_division_left_f25856_2_0 <- dat$volume_of_grey_matter_in_temporal_fusiform_cortex_posterior_division_left_f25856_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_temporal_fusiform_cortex_posterior_division_right_f25857_2_0 <- dat$volume_of_grey_matter_in_temporal_fusiform_cortex_posterior_division_right_f25857_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_temporal_occipital_fusiform_cortex_left_f25858_2_0 <- dat$volume_of_grey_matter_in_temporal_occipital_fusiform_cortex_left_f25858_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_temporal_occipital_fusiform_cortex_right_f25859_2_0 <- dat$volume_of_grey_matter_in_temporal_occipital_fusiform_cortex_right_f25859_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_occipital_fusiform_gyrus_left_f25860_2_0 <- dat$volume_of_grey_matter_in_occipital_fusiform_gyrus_left_f25860_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_occipital_fusiform_gyrus_right_f25861_2_0 <- dat$volume_of_grey_matter_in_occipital_fusiform_gyrus_right_f25861_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_frontal_operculum_cortex_left_f25862_2_0 <- dat$volume_of_grey_matter_in_frontal_operculum_cortex_left_f25862_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_frontal_operculum_cortex_right_f25863_2_0 <- dat$volume_of_grey_matter_in_frontal_operculum_cortex_right_f25863_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_central_opercular_cortex_left_f25864_2_0 <- dat$volume_of_grey_matter_in_central_opercular_cortex_left_f25864_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_central_opercular_cortex_right_f25865_2_0 <- dat$volume_of_grey_matter_in_central_opercular_cortex_right_f25865_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_parietal_operculum_cortex_left_f25866_2_0 <- dat$volume_of_grey_matter_in_parietal_operculum_cortex_left_f25866_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_parietal_operculum_cortex_right_f25867_2_0 <- dat$volume_of_grey_matter_in_parietal_operculum_cortex_right_f25867_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_planum_polare_left_f25868_2_0 <- dat$volume_of_grey_matter_in_planum_polare_left_f25868_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_planum_polare_right_f25869_2_0 <- dat$volume_of_grey_matter_in_planum_polare_right_f25869_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_heschls_gyrus_includes_h1_and_h2_left_f25870_2_0 <- dat$volume_of_grey_matter_in_heschls_gyrus_includes_h1_and_h2_left_f25870_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_heschls_gyrus_includes_h1_and_h2_right_f25871_2_0 <- dat$volume_of_grey_matter_in_heschls_gyrus_includes_h1_and_h2_right_f25871_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_planum_temporale_left_f25872_2_0 <- dat$volume_of_grey_matter_in_planum_temporale_left_f25872_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_planum_temporale_right_f25873_2_0 <- dat$volume_of_grey_matter_in_planum_temporale_right_f25873_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_supracalcarine_cortex_left_f25874_2_0 <- dat$volume_of_grey_matter_in_supracalcarine_cortex_left_f25874_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_supracalcarine_cortex_right_f25875_2_0 <- dat$volume_of_grey_matter_in_supracalcarine_cortex_right_f25875_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_occipital_pole_left_f25876_2_0 <- dat$volume_of_grey_matter_in_occipital_pole_left_f25876_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_occipital_pole_right_f25877_2_0 <- dat$volume_of_grey_matter_in_occipital_pole_right_f25877_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_thalamus_left_f25878_2_0 <- dat$volume_of_grey_matter_in_thalamus_left_f25878_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_thalamus_right_f25879_2_0 <- dat$volume_of_grey_matter_in_thalamus_right_f25879_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_caudate_left_f25880_2_0 <- dat$volume_of_grey_matter_in_caudate_left_f25880_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_caudate_right_f25881_2_0 <- dat$volume_of_grey_matter_in_caudate_right_f25881_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_putamen_left_f25882_2_0 <- dat$volume_of_grey_matter_in_putamen_left_f25882_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_putamen_right_f25883_2_0 <- dat$volume_of_grey_matter_in_putamen_right_f25883_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_pallidum_left_f25884_2_0 <- dat$volume_of_grey_matter_in_pallidum_left_f25884_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_pallidum_right_f25885_2_0 <- dat$volume_of_grey_matter_in_pallidum_right_f25885_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_hippocampus_left_f25886_2_0 <- dat$volume_of_grey_matter_in_hippocampus_left_f25886_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_hippocampus_right_f25887_2_0 <- dat$volume_of_grey_matter_in_hippocampus_right_f25887_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_amygdala_left_f25888_2_0 <- dat$volume_of_grey_matter_in_amygdala_left_f25888_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_amygdala_right_f25889_2_0 <- dat$volume_of_grey_matter_in_amygdala_right_f25889_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_ventral_striatum_left_f25890_2_0 <- dat$volume_of_grey_matter_in_ventral_striatum_left_f25890_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_ventral_striatum_right_f25891_2_0 <- dat$volume_of_grey_matter_in_ventral_striatum_right_f25891_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_brainstem_f25892_2_0 <- dat$volume_of_grey_matter_in_brainstem_f25892_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_iiv_cerebellum_left_f25893_2_0 <- dat$volume_of_grey_matter_in_iiv_cerebellum_left_f25893_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_iiv_cerebellum_right_f25894_2_0 <- dat$volume_of_grey_matter_in_iiv_cerebellum_right_f25894_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_v_cerebellum_left_f25895_2_0 <- dat$volume_of_grey_matter_in_v_cerebellum_left_f25895_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_v_cerebellum_right_f25896_2_0 <- dat$volume_of_grey_matter_in_v_cerebellum_right_f25896_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_vi_cerebellum_left_f25897_2_0 <- dat$volume_of_grey_matter_in_vi_cerebellum_left_f25897_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_vi_cerebellum_vermis_f25898_2_0 <- dat$volume_of_grey_matter_in_vi_cerebellum_vermis_f25898_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_vi_cerebellum_right_f25899_2_0 <- dat$volume_of_grey_matter_in_vi_cerebellum_right_f25899_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_crus_i_cerebellum_left_f25900_2_0 <- dat$volume_of_grey_matter_in_crus_i_cerebellum_left_f25900_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_crus_i_cerebellum_vermis_f25901_2_0 <- dat$volume_of_grey_matter_in_crus_i_cerebellum_vermis_f25901_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_crus_i_cerebellum_right_f25902_2_0 <- dat$volume_of_grey_matter_in_crus_i_cerebellum_right_f25902_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_crus_ii_cerebellum_left_f25903_2_0 <- dat$volume_of_grey_matter_in_crus_ii_cerebellum_left_f25903_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_crus_ii_cerebellum_vermis_f25904_2_0 <- dat$volume_of_grey_matter_in_crus_ii_cerebellum_vermis_f25904_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_crus_ii_cerebellum_right_f25905_2_0 <- dat$volume_of_grey_matter_in_crus_ii_cerebellum_right_f25905_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viib_cerebellum_left_f25906_2_0 <- dat$volume_of_grey_matter_in_viib_cerebellum_left_f25906_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viib_cerebellum_vermis_f25907_2_0 <- dat$volume_of_grey_matter_in_viib_cerebellum_vermis_f25907_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viib_cerebellum_right_f25908_2_0 <- dat$volume_of_grey_matter_in_viib_cerebellum_right_f25908_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viiia_cerebellum_left_f25909_2_0 <- dat$volume_of_grey_matter_in_viiia_cerebellum_left_f25909_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viiia_cerebellum_vermis_f25910_2_0 <- dat$volume_of_grey_matter_in_viiia_cerebellum_vermis_f25910_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viiia_cerebellum_right_f25911_2_0 <- dat$volume_of_grey_matter_in_viiia_cerebellum_right_f25911_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viiib_cerebellum_left_f25912_2_0 <- dat$volume_of_grey_matter_in_viiib_cerebellum_left_f25912_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viiib_cerebellum_vermis_f25913_2_0 <- dat$volume_of_grey_matter_in_viiib_cerebellum_vermis_f25913_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_viiib_cerebellum_right_f25914_2_0 <- dat$volume_of_grey_matter_in_viiib_cerebellum_right_f25914_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_ix_cerebellum_left_f25915_2_0 <- dat$volume_of_grey_matter_in_ix_cerebellum_left_f25915_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_ix_cerebellum_vermis_f25916_2_0 <- dat$volume_of_grey_matter_in_ix_cerebellum_vermis_f25916_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_ix_cerebellum_right_f25917_2_0 <- dat$volume_of_grey_matter_in_ix_cerebellum_right_f25917_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_x_cerebellum_left_f25918_2_0 <- dat$volume_of_grey_matter_in_x_cerebellum_left_f25918_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_x_cerebellum_vermis_f25919_2_0 <- dat$volume_of_grey_matter_in_x_cerebellum_vermis_f25919_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 
dat$volume_of_grey_matter_in_x_cerebellum_right_f25920_2_0 <- dat$volume_of_grey_matter_in_x_cerebellum_right_f25920_2_0 * dat$volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0 


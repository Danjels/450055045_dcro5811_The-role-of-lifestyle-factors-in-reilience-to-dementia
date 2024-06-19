# making balanced control group

#dat_dem <- dat[dat$proxy_dementia==1,]
#dat_ctrl <- dat[dat$eid %in% list_int$no_livingston,]
setwd("D:/R/wd")

dat_dem <- readRDS("dat_dem.rds")
dat_ctrl <- readRDS("dat_ctrl.rds")

# dementia
summary(dat_dem$apoe_genotype)
summary(dat_dem$sex_f31_0_0)
summary(dat_dem$age_when_attended_assessment_centre_f21003_2_0)
summary(dat_dem$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0)

# get means by sex for age and ICV

mean((dat_dem$age_when_attended_assessment_centre_f21003_2_0[dat_dem$sex_f31_0_0 == "Male"]))
mean((dat_dem$age_when_attended_assessment_centre_f21003_2_0[dat_dem$sex_f31_0_0 == "Female"]))

# > mean((dat_dem$age_when_attended_assessment_centre_f21003_2_0[dat_dem$sex_f31_0_0 == "Male"]))
# [1] 63.7234
# > mean((dat_dem$age_when_attended_assessment_centre_f21003_2_0[dat_dem$sex_f31_0_0 == "Female"]))
# [1] 61.22255

mean((dat_dem$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0[dat_dem$sex_f31_0_0 == "Male"]), na.rm = T)
mean((dat_dem$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0[dat_dem$sex_f31_0_0 == "Female"]), na.rm = T)

#mean((dat_dem$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0[dat_dem$sex_f31_0_0 == "Male"]), na.rm = T)
#[1] 1629966
# mean((dat_dem$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0[dat_dem$sex_f31_0_0 == "Female"]), na.rm = T)
#[1] 1467994

# ctrl
summary(dat_ctrl$apoe_genotype)
summary(dat_ctrl$sex_f31_0_0)
summary(dat_ctrl$age_when_attended_assessment_centre_f21003_2_0)
summary(dat_ctrl$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0)

# make a female and male subset of controls
dat_ctrl_f <- dat_ctrl[dat_ctrl$sex_f31_0_0=="Female",]
dat_ctrl_m <- dat_ctrl[dat_ctrl$sex_f31_0_0=="Male",]

# remove some of the older males because the control male cohort were too old
dat_ctrl_m <- dat_ctrl_m[!dat_ctrl_m$age_when_attended_assessment_centre_f21003_2_0>78,]


# take random samples of male and female controls
set.seed(123)
m_sample <- dat_ctrl_m[sample(nrow(dat_ctrl_m), 376), ]
f_sample <- dat_ctrl_f[sample(nrow(dat_ctrl_f), 683), ]

# check output
summary(m_sample$apoe_genotype)
summary(m_sample$sex_f31_0_0)
summary(m_sample$age_when_attended_assessment_centre_f21003_2_0)
summary(m_sample$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0)


summary(f_sample$apoe_genotype)
summary(f_sample$sex_f31_0_0)
summary(f_sample$age_when_attended_assessment_centre_f21003_2_0)
summary(f_sample$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0)

# combine and then make a new case_control variable 
dat_comb <- rbind(dat_dem,f_sample,m_sample)
dat_comb$case_control <- rep(0)
dat_comb$case_control[dat_comb$no_livingston_no_parental_both==0] <- "case"
dat_comb$case_control[dat_comb$no_livingston_no_parental_both==1] <- "control"
dat_comb$case_control <- as.factor(dat_comb$case_control)

# check age and brain volume balance
mean((dat_comb$age_when_attended_assessment_centre_f21003_2_0[dat_comb$case_control == "case"]))
mean((dat_comb$age_when_attended_assessment_centre_f21003_2_0[dat_comb$case_control == "control"]))

mean((dat_comb$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0[dat_comb$case_control == "case"]),na.rm = T)
mean((dat_comb$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0[dat_comb$case_control == "control"]),na.rm = T)

# plot the means and ICV to show that they are balanced
library(lattice)
bwplot(dat_comb$volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0~dat_comb$case_control)
bwplot(dat_comb$age_when_attended_assessment_centre_f21003_2_0~dat_comb$case_control)

#

# 
saveRDS(dat_comb,"matched_case_control_for_ML.rds")


# str()
# typeof()

# tried using the matchControl function from e1071 package but it was shit
# code is below
#dat_comb <- rbind(dat_ctrl,dat_dem)

# dat_comb$case_control <- rep(0)
# dat_comb$case_control[dat_comb$no_livingston_no_parental_both==0] <- "case"
# dat_comb$case_control[dat_comb$no_livingston_no_parental_both==1] <- "control"
# dat_comb$case_control <- as.factor(dat_comb$case_control)
# 
# 
# match_test <- matchControls(case_control ~ age_when_attended_assessment_centre_f21003_2_0 +
#                           volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0,data=dat_comb,
#                           contlabel = "control", caselabel = "case")
# 
# keep <- c(match_test$cases,match_test$controls)
# 
# test_sample <- dat_comb[rownames(dat_comb) %in% keep,]
# 
# # before
# mean((dat_comb$age_when_attended_assessment_centre_f21003_2_0[dat_comb$case_control == "case"]))
# mean((dat_comb$age_when_attended_assessment_centre_f21003_2_0[dat_comb$case_control == "control"]))
# 
# # after
# mean((test_sample$age_when_attended_assessment_centre_f21003_2_0[test_sample$case_control == "case"]))
# mean((test_sample$age_when_attended_assessment_centre_f21003_2_0[test_sample$case_control == "control"]))
# 
# table(test_sample$sex_f31_0_0,test_sample$case_control)
`# 
# #######################################################################################################
# # making original control group
# #######################################################################################################
# 
# # # making the control group
 in_dat <- readRDS("../../42806_ukb_with_dummyvars.rds")

list_int <- read.csv("daniel_vars_interest.csv",stringsAsFactors = F, na.strings = c("NA",""))
list_int <- lapply(as.list(list_int), function(x){x[!is.na(x)]})
# #
icd10_int <- read.csv("risk_factors_defined.csv", stringsAsFactors = F, na.strings = c("NA",""))
icd10_int <- lapply(as.list(icd10_int), function(x){x[!is.na(x)]})
# #
 rm_list <- c(list_int$imvars_remove,list_int$imvars_remove2,list_int$covars,"eid","group")
# #
# #
vars_final <- c(list_int$imaging_vars2_0,list_int$covars,list_int$freesurfer_only,
                 "date_of_attending_assessment_centre_f53_2_0",
                "when_pain_questionnaire_completed_f120128_0_0",
                 "cognitive_symptoms_severity_over_the_past_week_f120042_0_0")

dat <- in_dat[,names(in_dat) %in% vars_final]
# 
# 
# # make a new column for controls
# # in_dat$no_livingston_no_parental_both <- rep(0)
# # in_dat$no_livingston_no_parental_both[in_dat$eid %in% list_int$no_livingston] <- 1
# # in_dat$no_livingston_no_parental_both <- as.factor(in_dat$no_livingston_no_parental_both)
# 
# 
# 
# 
# 
# # code used to make the control group (list_int$no_livingston) = not for matching
# dat <- dat[complete.cases(dat$apoe_genotype),]
# dat <- dat[!dat$dementia_all_icd10==1,]
# dat <- dat[!dat$diabetes_icd10==1,]
# dat <- dat[!dat$hearing_icd10==1,]
# dat <- dat[!dat$tbi_icd10==1,]
# dat <- dat[!dat$obesity_icd10==1,]
# dat <- dat[!dat$depression_icd10==1,]
# dat <- dat[!dat$heart_disease_icd10==1,]
# dat <- dat[!dat$hypertension_icd10==1,]
# 
# 
# dat <- dat[complete.cases(dat$obese_binary),]
# dat <- dat[complete.cases(dat$current_smoker),]
# dat <- dat[complete.cases(dat$alcohol_over21),]
# dat <- dat[complete.cases(dat$lonely),]
# dat <- dat[complete.cases(dat$low_phys_activity),]
# dat <- dat[complete.cases(dat$high_pm10_2007),]
# dat <- dat[complete.cases(dat$low_education),]
# dat <- dat[complete.cases(dat$parental_ad),]
# dat <- dat[complete.cases(dat$proxy_dementia),]
# 
# 
# dat <- dat[!(dat$obese_binary=="Obese"),]
# dat <- dat[!dat$current_smoker==1,]
# dat <- dat[!dat$alcohol_over21==1,]
# dat <- dat[!dat$lonely==1,]
# dat <- dat[!dat$prev_MI==1,]
# dat <- dat[!dat$previous_stroke==1,]
# dat <- dat[!dat$low_phys_activity==1,]
# dat <- dat[!dat$high_pm10_2007==1,]
# dat <- dat[!dat$parental_ad==2,]
# dat <- dat[!dat$low_education==1,]
# dat <- dat[!dat$proxy_dementia==1,]
# 
# 
# # resulted in 9261 subjects without any of the risk factors and with APOE genotype
# 
# 
# summary(dat$low_education)
# summary(dat$parental_ad)
# summary(dat$prev_MI)
# summary(dat$previous_stroke)
# summary(dat$apoe_genotype)
# summary(dat$lonely)
# 
# # get basic stats
# summary(dat$apoe_genotype)
# summary(dat$sex_f31_0_0)
# summary(dat$age_when_attended_assessment_centre_f21003_2_0)
# 
# write.csv(dat$eid,"no_livingston_no_parental_both.csv")
# 
# 
# age - age_when_attended_assessment_centre_f21003_2_0
# sex - sex_f31_0_0
# norm factor - volumetric_scaling_from_t1_head_image_to_standard_space_f25000_2_0
# etiv - volume_of_estimatedtotalintracranial_whole_brain_f26521_2_0
# x position - scanner_lateral_x_brain_position_f25756_2_0
# y position - scanner_transverse_y_brain_position_f25757_2_0
# z position - scanner_longitudinal_z_brain_position_f25758_2_0
# table position - scanner_table_position_f25759_2_0
# inverted SNR - inverted_signaltonoise_ratio_in_t1_f25734_2_0
# inverted CNR - inverted_contrasttonoise_ratio_in_t1_f25735_2_0
# discrep T1 T2 - discrepancy_between_t2_flair_brain_image_and_t1_brain_image_f25736_2_0
# outlier slices - number_of_dmri_outlier_slices_detected_and_corrected_f25746_2_0
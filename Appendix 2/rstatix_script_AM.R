library(tidyverse)
library(ggpubr)
library(ggsci)
library(rstatix)

dat <- readRDS("ukb_over65_nodem_no_outliers_data_long.rds")

# make total volume column
dat$total_hippocampal_vol <- dat$volume_of_hippocampus_left_f25019_2_0 + dat$volume_of_hippocampus_right_f25020_2_0

#######################################################################################################
# cohens d
#######################################################################################################

hip_left_eff <- cohens_d(data=dat,formula = volume_of_hippocampus_left_f25019_2_0~diagnosis,
                 comparisons = list(c("diabetes", "control"), c("hypertension", "control"),
                                    c("alcohol_over21", "control"), c("current_smoker", "control"),
                                    c("depression", "control"), c("hearing", "control"),
                                    c("high_pollution", "control"), c("low_phys_activity", "control"),
                                    c("lonely", "control"), c("low_education", "control"),
                                    c("obesity", "control"), c("TBI", "control")),
                 ref.group = "control",hedges.correction = TRUE)

hip_right_eff <- cohens_d(data=dat,formula = volume_of_hippocampus_right_f25020_2_0~diagnosis,
                     comparisons = list(c("diabetes", "control"), c("hypertension", "control"),
                                        c("alcohol_over21", "control"), c("current_smoker", "control"),
                                        c("depression", "control"), c("hearing", "control"),
                                        c("high_pollution", "control"), c("low_phys_activity", "control"),
                                        c("lonely", "control"), c("low_education", "control"),
                                        c("obesity", "control"), c("TBI", "control")),
                     ref.group = "control",hedges.correction = TRUE)

hip_total_eff <- cohens_d(data=dat,formula = total_hippocampal_vol~diagnosis,
                     comparisons = list(c("diabetes", "control"), c("hypertension", "control"),
                                        c("alcohol_over21", "control"), c("current_smoker", "control"),
                                        c("depression", "control"), c("hearing", "control"),
                                        c("high_pollution", "control"), c("low_phys_activity", "control"),
                                        c("lonely", "control"), c("low_education", "control"),
                                        c("obesity", "control"), c("TBI", "control")),
                     ref.group = "control",hedges.correction = TRUE)

#######################################################################################################
# t_test
#######################################################################################################

hip_left_t <- t_test(data=dat,formula = volume_of_hippocampus_left_f25019_2_0~diagnosis,
                     comparisons = list(c("diabetes", "control"), c("hypertension", "control"),
                                        c("alcohol_over21", "control"), c("current_smoker", "control"),
                                        c("depression", "control"), c("hearing", "control"),
                                        c("high_pollution", "control"), c("low_phys_activity", "control"),
                                        c("lonely", "control"), c("low_education", "control"),
                                        c("obesity", "control"), c("TBI", "control")),
                     ref.group = "control",p.adjust.method = "BH",detailed = TRUE)

hip_right_t <- t_test(data=dat,formula = volume_of_hippocampus_right_f25020_2_0~diagnosis,
                      comparisons = list(c("diabetes", "control"), c("hypertension", "control"),
                                         c("alcohol_over21", "control"), c("current_smoker", "control"),
                                         c("depression", "control"), c("hearing", "control"),
                                         c("high_pollution", "control"), c("low_phys_activity", "control"),
                                         c("lonely", "control"), c("low_education", "control"),
                                         c("obesity", "control"), c("TBI", "control")),
                      ref.group = "control",p.adjust.method = "BH",detailed = TRUE)

hip_total_t <- t_test(data=dat,formula = total_hippocampal_vol~diagnosis,
                      comparisons = list(c("diabetes", "control"), c("hypertension", "control"),
                                         c("alcohol_over21", "control"), c("current_smoker", "control"),
                                         c("depression", "control"), c("hearing", "control"),
                                         c("high_pollution", "control"), c("low_phys_activity", "control"),
                                         c("lonely", "control"), c("low_education", "control"),
                                         c("obesity", "control"), c("TBI", "control")),
                      ref.group = "control",p.adjust.method = "BH",detailed = TRUE)
# Getting Ready
library(caret)
library(ggplot2)
library(dplyr)

# 1. Preprocessing
# 1.1 Creating feature matrix and outcome vector
patient_feature_matrix <- read.csv("../hw3/data/patient_feature_matrix.csv", as.is = TRUE)
# require feature matrix to be numerical so convert oxy_drop and gender variable into 0 and 1
# oxy_drop: oxy_drop -> 1, stable -> 0; gender: M -> 1, F -> 0
patient_feature_matrix <-
  patient_feature_matrix %>% 
  mutate(gender=ifelse(gender=='M', 1, 0)) %>%
  mutate(oxy_drop=ifelse(oxy_drop=='oxy_drop', 1, 0))
# split the patient_feature_matrix into feature matrix and outcome vector 
feature_matrix <-
  patient_feature_matrix %>%
  select(-subject_id, -death_in_stay) %>%
  data.matrix()
outcome <- patient_feature_matrix$death_in_stay  


# 1.2 Removing uninformative features
# feature_matrix has dimension (3455, 2438)
# remove near-zero variance features that aren't useful
# constant and almost constant predictors across samples are called 
# zeor and near-zero variance predictors
# nearZeroVar: 1.removes predictors that have one unique value across samples,
# 2.removes predictors that have both few unique values relative to the number of
# samples and large ratio of the frequency of the most common value to the frequency
# of the second most common value. 
# By default, a predictor is classified as near-zero variance if the percentage of 
# unique values in the samples is less than 10% and when the frequency ratio mentioned
# above is greater than 19 (95/5). 

near_zero <- nearZeroVar(feature_matrix)
feature_matrix_filtered <-
  feature_matrix[, -near_zero]

# report how many of each different kind of feature are left after filtering out
# the near-zero variance features.
feature_descriptions <- read.csv("../hw3/data/feature_descriptions.csv", as.is = TRUE)
variable_remained <- colnames(feature_matrix_filtered)
feature_descriptions_remained <-
  feature_descriptions %>%
  filter(feature %in% variable_remained) %>%
  group_by(feature_type) %>%
  summarise(n=n())

# 1 chartindicator   335
# 2     chartvalue    48
# 3    demographic     2
# 4     engineered     1
# 5       note CUI   122

# This full set of features: 
# chartindicator     chartvalue    demographic     engineered           icd9       note CUI 
# 1345             51              2              1            489            550 

# All icd9 features are filtered out and this makes sense because the icd9 code 
# might be very specific so that for each code there is only a few patients have it
# and the majority of patients have 0. Most of the chartvalue are remained and
# this makes sense since chartvalues are numerical numbers and they are less likely
# to have near zero variances. 












  


# Getting Ready
library(caret)
library(ggplot2)
library(dplyr)

# 1. Preprocessing
# 1.1 Creating feature matrix and outcome vector
patient_feature_matrix <- read.csv("../hw3/data/patient_feature_matrix.csv", as.is = TRUE)
# create dummy variables for oxy_drop and gender
patient_feature_matrix2 <-
  patient_feature_matrix %>%
  mutate(gender_num = ifelse(gender=='F', 1, 0)) %>%  # Female is mapped to 1, male is mapped to 0
  mutate(oxy_drop_num = ifelse(oxy_drop=='oxy_drop', 1, 0)) %>%
  select(-oxy_drop, -gender) %>%
  rename(oxy_drop=oxy_drop_num, gender=gender_num)
feature_matrix <-
  patient_feature_matrix2 %>%
  select(-subject_id, -death_in_stay) %>%
  data.matrix()
outcome <- patient_feature_matrix2$death_in_stay  

# 1.2 removing uninformative features
feature_descriptions <- read.csv("../hw3/data/feature_descriptions.csv", as.is = TRUE)
nzv <- nearZeroVar(feature_matrix)
new_feature_matrix <- data.frame(feature_matrix[, -nzv])
feature_names_df <- data.frame(feature=colnames(new_feature_matrix), stringsAsFactors = FALSE)
feature_type <-
  feature_descriptions %>%
  select(feature, feature_type)

remained_feature_type <-
  feature_names_df %>%
  left_join(feature_type, by='feature')
  
table(remained_feature_type$feature_type)
table(feature_type$feature_type)
  
# 2. Associative Analyses
# 2.1 Hypothesis testing
# 2.1.1 Statistical tests of differences between two groups
qplot(new_feature_matrix$chartindicator_1622, geom="histogram", binwidth=1) # rank-sum
qplot(new_feature_matrix$chartindicator_31, geom="histogram", binwidth=1) # rank-sum
# use Mann-Whitney-Wilcoxon Test, we can decide whether the population distributions are identical without
# assuming them to follow the normal distribution
qplot(new_feature_matrix$chartvalue_618, geom="histogram", binwidth=1)  # t-test, normality of data distribution
qplot(new_feature_matrix$chartvalue_778, geom="histogram", binwidth=1)  # t-test, normality of data distribution
# Fisher's Exact test is a way to test the association between two categorical variables when you have small
# cell sizes (<5). Chi-square test is used when the cell sizes are expeted to be large.
qplot(new_feature_matrix$oxy_drop, geom="histogram", binwidth=1)  # Fisher, chi-square
qplot(new_feature_matrix$C2720507, geom="histogram", binwidth=1)  # Fisher, chi-squre



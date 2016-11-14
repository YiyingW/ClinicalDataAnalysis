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

# 2. Associative Analyses
# 2.1 Hypothesis testing
# 2.1.1 statistical tests of differences between two groups
# features: alarms, activity, respiratory rate, arterial PaCO2, oxy_drop, snomed ct concept 
# t-test, rank-sum test, Fisher exact test, or chi squred test 
# to determine if each of these features is associated with mortality. 

# Step 1: find these features' feature name in feature_matrix_filtered
# alarms -> chartindicator_1622
# activity -> chartindicator_31
# respiratory rate -> chartvalue_618
# arterial PaCO2 -> chartvalue_778
# oxy_drop -> oxy_drop
# snomed ct concept -> C2720507

# Step2: inspect these features and decide which tests to use
alarms_dat <- 
  data.frame(feature_matrix_filtered[,"chartindicator_1622"], outcome)
colnames(alarms_dat) <- c("variable", "outcome")

ggplot(alarms_dat)+
  geom_histogram(aes(x=variable,colour=outcome),binwidth=0.5)
# for alarm, it has continuous interger numerical values and its distribution is not normal. Rank-sum
# test should be used.

activity_dat <- 
  data.frame(feature_matrix_filtered[,"chartindicator_31"], outcome)
colnames(activity_dat) <- c("variable", "outcome")

ggplot(activity_dat)+
  geom_histogram(aes(x=variable,colour=outcome),binwidth=0.5)
# for activity, it has continuous interger numerical values and its distribution is not normal. Rank-sum
# test should be used.

resp_dat <- 
  data.frame(feature_matrix_filtered[,"chartvalue_618"], outcome)
colnames(resp_dat) <- c("variable", "outcome")

ggplot(resp_dat)+
  geom_histogram(aes(x=variable,colour=outcome),binwidth=0.5)
# for respiratory rate, it has continuous interger numerical values and its distribution is approximately normal. 
# T test can be used and rank-sum test can also be used. 

PaCO2_dat <- 
  data.frame(feature_matrix_filtered[,"chartvalue_778"], outcome)
colnames(PaCO2_dat) <- c("variable", "outcome")

ggplot(PaCO2_dat)+
  geom_histogram(aes(x=variable,colour=outcome),binwidth=0.5)
# for arterial PaCO2, it has continuous interger numerical values and its distribution is approximately normal. 
# T test can be used and rank-sum test can also be used.

# oxy_drop -> oxy_drop
oxydrop_dat <- 
  data.frame(feature_matrix_filtered[,"oxy_drop"], outcome)
colnames(oxydrop_dat) <- c("variable", "outcome")

ggplot(oxydrop_dat)+
  geom_histogram(aes(x=variable,colour=outcome),binwidth=1)
# Fisher's Exact test is a way to test the association between two categorical variables when there is a small
# cell sizes (<5). Chi-square test is used when the cell sizes are expeted to be large. Oxy_drop variable
# can use both of these tests. 

snomed_dat <- 
  data.frame(feature_matrix_filtered[,"C2720507"], outcome)
colnames(snomed_dat) <- c("variable", "outcome")

ggplot(snomed_dat)+
  geom_histogram(aes(x=variable,colour=outcome),binwidth=1)
# the smallest cell has 31 samples. It is larger than 5 but it is relatively small then other cells. Both
# Fisher's Exact test and chi-square test can be used. 

# Step3: apply tests
# alrams
wilcox.test(alarms_dat[which(outcome=="died"),"variable"], 
            alarms_dat[which(outcome=="survived"),"variable"], correct = F)
#  p-value = 0.07161

# activity
wilcox.test(activity_dat[which(outcome=="died"),"variable"], 
            activity_dat[which(outcome=="survived"),"variable"], correct = F)
# p-value = 0.1854

# respiratory rate
wilcox.test(resp_dat[which(outcome=="died"),"variable"], 
            resp_dat[which(outcome=="survived"),"variable"], correct = F)
# p-value = 1.669e-13
t.test(resp_dat[which(outcome=="died"),"variable"], 
       resp_dat[which(outcome=="survived"),"variable"])
# p-value = 3.83e-11

# arterial PaCO2
wilcox.test(PaCO2_dat[which(outcome=="died"),"variable"], 
            PaCO2_dat[which(outcome=="survived"),"variable"], correct = F)
# p-value < 2.2e-16
t.test(PaCO2_dat[which(outcome=="died"),"variable"], 
       PaCO2_dat[which(outcome=="survived"),"variable"])
# p-value = 2.314e-15

# oxy_drop
chisq.test(oxydrop_dat$variable, oxydrop_dat$outcome)
# p-value = 1.939e-09
fisher.test(oxydrop_dat$variable, oxydrop_dat$outcome)
# p-value = 3.571e-09

# snomed ct concept
chisq.test(snomed_dat$variable, snomed_dat$outcome)
# p-value = 0.8487
fisher.test(snomed_dat$variable, snomed_dat$outcome)
# p-value = 0.8422


# 2.1.2 Identify control associations
# It would be negative because snomed ct concept and mortality don't have statistically
# significant associations with p value greater than 0.8.

# 2.1.3 Hypothesis testing with Bonferroni correction
# need to load broom package
# select features from feature matrix that are chart value features

chartvalue_features <- # a dataframe has all chart value features and outcome
  feature_matrix_filtered %>%
  as.data.frame() %>%
  select(starts_with('chartvalue')) %>%
  cbind(outcome,.)
num_chartvalue_features <- ncol(chartvalue_features)-1
corrected_p <- 0.05/num_chartvalue_features
  
p_values <- sapply(chartvalue_features[-1], function(x) 
  unlist(t.test(x~chartvalue_features$outcome)["p.value"]))

# How many chart value features are significantly associated with death at corrected p value?  
sum(p_values <= corrected_p)  # 26
# How many chart value features are significantly associated with death at the standard cutoff of 0.05?
sum(p_values <= 0.05)   # 30

# 2.2 Adjusted Analyses
# 2.2.1 Regression Models for Association
# the outcome is binary so logistic regression is appropriate 

# first, find out chart value features that are significantly associated with death after Bonferroni correction
chart_values <- rownames(as.data.frame(p_values))

signif_chart_value <-
  p_values %>%
  as.data.frame() %>% 
  mutate(namep=chart_values) %>%
  mutate(name=substr(namep, 1, nchar(namep)-8)) %>%
  filter(.<=corrected_p) %>%
  select(name)

regression_model_features <-
  feature_matrix_filtered %>%
  as.data.frame() %>%
  select(age_in_days, oxy_drop, gender, one_of(signif_chart_value$name)) %>%
  cbind(outcome,.)
  

model1 <- glm(outcome ~ age_in_days + oxy_drop, family = "binomial", data=regression_model_features)
model2 <- glm(outcome ~ age_in_days + gender + oxy_drop, family = "binomial", data=regression_model_features)
model3 <- glm(outcome ~ ., family = "binomial", data=regression_model_features)



















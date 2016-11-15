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

# 2.2.2 Comparing regression models
# what is the coefficient for oxy_drop in each model and what is its confidence interval?
summary(model1) # -5.747e-01
confint(model1) # -7.581675e-01 -3.900444e-01 delta: 0.3681231

summary(model2) # -5.778e-01
confint(model2) # -7.615069e-01 -3.928747e-01  delta: 0.3686322

summary(model3) # -3.151e-01
confint(model3) # -5.226494e-01 -1.060480e-01  delta: 0.4166014

# Why does the point estimate change as more features are added? 
# The point estimate changes as more features are added because the newly added features
# are correlated with that parameter's corresponding variable and correlated with the response
# variable as well. 

# Assuming you had a model of Y regressed on X1 and you added the variable X2, under what
# conditions would the coefficient for X1 not change? 
# If adding X2 doesn't change X1 coefficient, then X2 is not correlated with X1 and it is not
# correlated with Y.

# If both are positively correlated with the outcome and with each other, what would happen 
# to the coefficient of X1 after adding X2? 
# The coefficient of X1 after adding X2 will decrease because some of the marginal effect of X1
# is being taken up by the addition of X2.

# 2.2.3 Legitimancy of Confidence Intervals
# The more correlated the X variables are with each other, the bigger the standard errors
# become, and the less likely it is that a coefficient will be statistically significant. This
# is known as the problem of multicollinearity. The more highly correlated independent variables
# are, the more difficult it is to determine how much variation in Y each X is responsible for. 
# For example, if X1 and X2 are highly correlated, it is difficult to determine whether X1 is 
# responsible for variation in Y, or whether X2 is. As a result, the standard errors for both 
# variables become very large. When multicollinearity occurs, the estimated standard
# errors of the coefficients tend to be inflated. 

# 2.2.4 Testing residuals
anova(model1, model2, model3)
# compared with model1, model2 explains 0.45 more variance in the outcome variable.
# compared with model2, model2 explains 447.32 more variance in the outcome variable.
# model3 explains the most variance in the outcome variable. 


# 2.3 Survival Analysis
# 2.3.1 Creating Survival Data
cohort <- read.csv("../hw3/data/cohort.csv", as.is = TRUE)
patients_survival <-
  cohort %>%
  select(index_time, censor_time, oxy_drop, death_in_stay) %>%
  mutate(survival_time=ceiling(as.numeric(difftime(as.POSIXct(censor_time), as.POSIXct(index_time), units='days')))) %>%
  mutate(censor=ifelse(death_in_stay=='died', 0, 1)) %>%
  mutate(event=ifelse(death_in_stay=='died', 1, 0)) %>%
  select(survival_time, oxy_drop, censor, event)

# 2.3.2 Kaplan-Meier Curves

# My code
createKMtable <- function(rawdata){ # rawdata is a dataframe has four columns: survival_time, oxy_drop, censor, event
  longest_time <- max(rawdata$survival_time)
  time_intervals <-
    rawdata %>%
    filter(censor==0) %>%
    arrange(survival_time) %>%
    select(survival_time) %>%
    unique()
  n_total <- nrow(rawdata)
  KM_table <-
    rawdata %>%
    arrange(survival_time) %>%
    mutate(day=ifelse(censor==1, survival_time+0.1, survival_time)) %>%
    mutate(cum_d=cumsum(event)) %>%
    mutate(cum_c=cumsum(censor)) %>%
    select(day, cum_d, cum_c) %>%
    group_by(day) %>%
    top_n(n=1, wt=cum_d) %>%
    ungroup() %>%
    rbind(c(0, 0, 0),.) %>%
    mutate(dj=cum_d-lag(cum_d)) %>%
    mutate(cj=lead(cum_c)-cum_c) %>%
    right_join(., time_intervals, by=c('day'='survival_time')) %>%
    mutate(loss=dj+cj) %>%
    mutate(acum_loss=cumsum(loss)) %>%
    rbind(c(0,0,0,0,0,0,0),.) %>%
    mutate(nj=n_total-lag(acum_loss)) %>%
    mutate(pi=(nj-dj)/nj) %>%
    right_join(., time_intervals, by=c('day'='survival_time')) %>%
    mutate(st=cumprod(pi)) %>%
    select(day, st) %>%
    rbind(c(0,1),.) 
  lowest_prob <- min(KM_table$st)
  KM_table2 <-
    KM_table %>%
    mutate(st2=lag(st)) %>%
    select(day, st2) %>%
    rename(st=st2) %>%
    rbind(.,KM_table) %>%
    rbind(., c(longest_time, lowest_prob)) %>%
    drop_na()
  return (KM_table2)
}

subset_oxydrop <- 
  patients_survival %>%
  filter(oxy_drop=="oxy_drop")
KM_oxydrop <- createKMtable(subset_oxydrop) 
KM_oxydrop2 <- 
  KM_oxydrop %>%
  cbind(oxy_drop=rep("oxy_drop", nrow(KM_oxydrop)))


subset_stable <- 
  patients_survival %>%
  filter(oxy_drop=="stable")
KM_stable <- createKMtable(subset_stable) 
KM_stable2 <- 
  KM_stable %>%
  cbind(oxy_drop=rep("stable", nrow(KM_stable)))

KM_to_plot <-
  KM_oxydrop2 %>%
  rbind(KM_stable2)


ggplot(KM_to_plot, aes(x=day, y=st, color=oxy_drop)) +
  geom_line() +
  labs(x="survival time (day)", y='proportion surviving')

# use packages
library(survival)
surv_curve <- survfit(Surv(survival_time, event)~strata(oxy_drop), 
                      patients_survival)
plot(surv_curve, lty=c(1,3), xlab="survival time (day)",
     ylab="survival probability")
legend(120, 1.0, c("Stable","Oxy_drop"), lty=c(1,3))


### new try ###

createKMtable <- function(subset){
  num_patients <- nrow(subset)
  df <-
    subset %>%
    arrange(survival_time) %>%
    group_by(survival_time) %>%
    mutate(cum_c = cumsum(censor)) %>% # in each day, how many are censored
    mutate(cum_d = cumsum(event)) %>% # in each day, how many are dead
    select(survival_time, cum_c, cum_d) %>% 
    filter(row_number()==n()) %>% 
    ungroup() %>%
    rbind(data.frame(survival_time=0, cum_c=0, cum_d=0),.) %>%
    mutate(dead_bythisday=cumsum(cum_d)) %>%
    mutate(censored_bythisday=cumsum(cum_c)) %>%
    mutate(alive=num_patients-dead_bythisday-censored_bythisday) %>%
    mutate(P=(alive+cum_c)/(alive+cum_c+cum_d)) %>%
    mutate(st=cumprod(P)) %>%
    select(survival_time, st)
  df2 <-    # one day and above one day, use survival time column from this
    df %>%
    filter(row_number()<n())
  df3 <-  # survival function from 1 
    df %>%
    filter(row_number()>1)
  to_append <-
    data.frame(survival_time=df2$survival_time, st=df3$st)
  df_final <-
    rbind(df, to_append)
    return (df_final)
}
KM_oxydrop <- createKMtable(subset_oxydrop) 
KM_oxydrop2 <- 
  KM_oxydrop %>%
  cbind(oxy_drop=rep("oxy_drop", nrow(KM_oxydrop)))

KM_stable <- createKMtable(subset_stable) 
KM_stable2 <- 
  KM_stable %>%
  cbind(oxy_drop=rep("stable", nrow(KM_stable)))
KM_to_plot <-
  KM_oxydrop2 %>%
  rbind(KM_stable2)
ggplot(KM_to_plot, aes(x=survival_time, y=st, color=oxy_drop)) +
  geom_line() +
  labs(x="survival time (day)", y='Survival Function')





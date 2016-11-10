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

# even when the assumption of normality holds, the Mann-Whitney test is nearly as powerful as the t-test.
qplot(new_feature_matrix$chartvalue_618, geom="histogram", binwidth=1)  # t-test, normality of data distribution, or rank sum
qplot(new_feature_matrix$chartvalue_778, geom="histogram", binwidth=1)  # t-test, normality of data distribution, or rank sum
# Fisher's Exact test is a way to test the association between two categorical variables when you have small
# cell sizes (<5). Chi-square test is used when the cell sizes are expeted to be large.
qplot(new_feature_matrix$oxy_drop, geom="histogram", binwidth=1)  # Fisher
qplot(new_feature_matrix$C2720507, geom="histogram", binwidth=1)  # Fisher, chi-squre

####### doing tests ############
# chartindicator_1622 as iv1
iv1_outcome <- data.frame(new_feature_matrix$chartindicator_1622, outcome)
colnames(iv1_outcome) <- c("iv1", 'outcome')
iv1_dead <-
  iv1_outcome %>%
  filter(outcome=='died') 
iv1_dead_vector <-as.vector(iv1_dead$iv1)

iv1_not_dead <-
  iv1_outcome %>%
  filter(outcome=='survived') 
iv1_not_dead_vector <-as.vector(iv1_not_dead$iv1)

wilcox.test(iv1_dead_vector, iv1_not_dead_vector, correct = F)

#	Wilcoxon rank sum test
#data:  iv1_dead_vector and iv1_not_dead_vector
#W = 849870, p-value = 0.07161
#alternative hypothesis: true location shift is not equal to 0

# chartindicator_31 as iv2
iv2_outcome <- data.frame(new_feature_matrix$chartindicator_31, outcome)
colnames(iv2_outcome) <- c("iv2", 'outcome')
iv2_dead <-
  iv2_outcome %>%
  filter(outcome=='died') 
iv2_dead_vector <-as.vector(iv2_dead$iv2)

iv2_not_dead <-
  iv2_outcome %>%
  filter(outcome=='survived') 
iv2_not_dead_vector <-as.vector(iv2_not_dead$iv2)

wilcox.test(iv2_dead_vector, iv2_not_dead_vector, correct = F)

#	Wilcoxon rank sum test
# data:  iv2_dead_vector and iv2_not_dead_vector
# W = 860330, p-value = 0.1854
# alternative hypothesis: true location shift is not equal to 0


# chartvalue_618 as iv3
iv3_outcome <- data.frame(new_feature_matrix$chartvalue_618, outcome)
colnames(iv3_outcome) <- c("iv3", 'outcome')
iv3_dead <-
  iv3_outcome %>%
  filter(outcome=='died') 
iv3_dead_vector <-as.vector(iv3_dead$iv3)

iv3_not_dead <-
  iv3_outcome %>%
  filter(outcome=='survived') 
iv3_not_dead_vector <-as.vector(iv3_not_dead$iv3)

wilcox.test(iv3_dead_vector, iv3_not_dead_vector, correct = F)

#	Wilcoxon rank sum test
# data:  iv3_dead_vector and iv3_not_dead_vector
# W = 1056300, p-value = 1.669e-13
# alternative hypothesis: true location shift is not equal to 0

t.test(iv3_dead_vector, iv3_not_dead_vector, var.equal = T)


# Two Sample t-test

# data:  iv3_dead_vector and iv3_not_dead_vector
# t = 6.6282, df = 3453, p-value = 3.926e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.169279 2.151604
# sample estimates:
#   mean of x mean of y 
# 18.37304  16.71260 

t.test(iv3_dead_vector, iv3_not_dead_vector, var.equal = F)

# Welch Two Sample t-test

# data:  iv3_dead_vector and iv3_not_dead_vector
# t = 6.6899, df = 939.96, p-value = 3.83e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.173349 2.147533
# sample estimates:
#   mean of x mean of y 
# 18.37304  16.71260 


# chartvalue_778 as iv4
iv4_outcome <- data.frame(new_feature_matrix$chartvalue_778, outcome)
colnames(iv4_outcome) <- c("iv4", 'outcome')
iv4_dead <-
  iv4_outcome %>%
  filter(outcome=='died') 
iv4_dead_vector <-as.vector(iv4_dead$iv4)

iv4_not_dead <-
  iv4_outcome %>%
  filter(outcome=='survived') 
iv4_not_dead_vector <-as.vector(iv4_not_dead$iv4)

wilcox.test(iv4_dead_vector, iv4_not_dead_vector, correct = F)

#	Wilcoxon rank sum test
# data:  iv4_dead_vector and iv4_not_dead_vector
# W = 648010, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

t.test(iv4_dead_vector, iv4_not_dead_vector, var.equal = T)

# Two Sample t-test

# data:  iv4_dead_vector and iv4_not_dead_vector
# t = -9.1609, df = 3453, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.604566 -2.333650
# sample estimates:
#   mean of x mean of y 
# 35.99930  38.96841 

t.test(iv4_dead_vector, iv4_not_dead_vector, var.equal = F)

# Welch Two Sample t-test

# data:  iv4_dead_vector and iv4_not_dead_vector
# t = -8.0785, df = 827.23, p-value = 2.314e-15
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.690516 -2.247700
# sample estimates:
#   mean of x mean of y 
# 35.99930  38.96841 


qplot(new_feature_matrix$C2720507, geom="histogram", binwidth=1)  # Fisher, chi-squre

# oxy_drop as iv5 
outcome_num <- as.numeric(as.factor(outcome))
iv5_outcome <- data.frame(new_feature_matrix$oxy_drop, outcome_num)
colnames(iv5_outcome) <- c("iv5", 'outcome')

chisq.test(x=iv5_outcome$iv5, iv5_outcome$outcome)

# Pearson's Chi-squared test with Yates' continuity correction

# data:  iv5_outcome$iv5 and iv5_outcome$outcome
# X-squared = 36.034, df = 1, p-value = 1.939e-09
fisher.test(x=iv5_outcome$iv5, iv5_outcome$outcome)
# Fisher's Exact Test for Count Data

# data:  iv5_outcome$iv5 and iv5_outcome$outcome
# p-value = 3.571e-09
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
# 0.4763356 0.6907991
# sample estimates:
# odds ratio 
# 0.5732915 



# C2720507 as iv6 
outcome_code <- ifelse(outcome=="died", 1, 0)
iv6_outcome <- data.frame(new_feature_matrix$C2720507, outcome_code)
colnames(iv6_outcome) <- c("iv6", 'outcome')

chisq.test(x=iv6_outcome$iv6, iv6_outcome$outcome)

# Pearson's Chi-squared test with Yates' continuity correction

# data:  iv6_outcome$iv6 and iv6_outcome$outcome
# X-squared = 0.036408, df = 1, p-value = 0.8487

fisher.test(x=iv6_outcome$iv6, iv6_outcome$outcome)
# Fisher's Exact Test for Count Data

# data:  iv6_outcome$iv6 and iv6_outcome$outcome
# p-value = 0.8422
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
# 0.6120581 1.4127256
# sample estimates:
# odds ratio 
# 0.9428064 

# 2.1.3
# how many chart varible features?
chart_variables <-
  new_feature_matrix %>%
  select(starts_with('chartvalue')) 

dim(chart_variables)
adjusted_p <- 0.05/ncol(chart_variables)

# use for loop to iterate over chartvalue varibles. Perform t-test on each,
# compare the p-value with corrected p value and with 0.05. Count the number
bonferroni_corrected_count <- 0
standard_count <- 0
include_chartvalue <- c()
for (i in 1:ncol(chart_variables)){
  iv_outcome <- data.frame(chart_variables[,i], outcome_code)
  colnames(iv_outcome) <- c("iv", 'outcome')
  iv_dead <-
    iv_outcome %>%
    filter(outcome==1) 
  iv_dead_vector <-as.vector(iv_dead$iv)
  
  iv_not_dead <-
    iv_outcome %>%
    filter(outcome==0) 
  iv_not_dead_vector <-as.vector(iv_not_dead$iv)
  test <- t.test(iv_dead_vector, iv_not_dead_vector, var.equal = T)
  p <- test$p.value
  if (p<= 0.05) {standard_count <-standard_count+1}
  if (p<=adjusted_p) {
    bonferroni_corrected_count <-bonferroni_corrected_count+1
    include_chartvalue <- c(include_chartvalue, colnames(chart_variables[i]))
  }
}

# 2.2.1 Regression models for association 
# Build a feature matrix contain age, gender, oxy_drop and selected chartvalue variables
feature_matrix_for_RegModel <-
  new_feature_matrix %>%
  select(age_in_days, gender, oxy_drop, one_of(include_chartvalue))

model1 <- glm(outcome_code ~ age_in_days + oxy_drop, family = "binomial", data=feature_matrix_for_RegModel)
model2 <- glm(outcome_code ~ age_in_days + gender + oxy_drop, family = "binomial", data=feature_matrix_for_RegModel)
model3 <- glm(outcome_code ~ ., family = "binomial", data=feature_matrix_for_RegModel)

# 2.2.2
confint.default(model1,parm='oxy_drop', level=0.95)
confint.default(model2,parm='oxy_drop', level=0.95)
confint.default(model3,parm='oxy_drop', level=0.95)

# 2.2.3
# model3 CI range > model2 CI range > model1 CI range

# 2.2.4
# pass

# 2.3.1
cohort <- read.csv("../hw3/data/cohort.csv", as.is = TRUE)
patients_survival <-
  cohort %>%
  select(index_time, censor_time, oxy_drop, death_in_stay) %>%
  mutate(survival_time=ceiling(as.numeric(difftime(as.POSIXct(censor_time), as.POSIXct(index_time), units='days')))) %>%
  mutate(censor=ifelse(death_in_stay=='died', 0, 1)) %>%
  mutate(event=ifelse(death_in_stay=='died', 1, 0)) %>%
  select(survival_time, oxy_drop, censor, event)

# 2.3.2 Kaplan-Meier Curves


toy_data <- 
  data.frame(week=c(9,9,13,13,18,23,28,31,34,45,48,161), censor=c(0,0,0,1,0,0,1,0,0,1,0,1), 
             event=c(1,1,1,0,1,1,0,1,1,0,1,0))
longest_time <-
  max(toy_data$week)
  
time_intervals <-
  toy_data %>%
  filter(censor==0) %>%
  arrange(week) %>%
  select(week) %>%
  unique()
n_total <- nrow(toy_data)
KM_table <-
  toy_data %>%
  arrange(week) %>%
  mutate(new_week=ifelse(censor==1, week+0.1, week)) %>%
  mutate(cum_d=cumsum(event)) %>%
  mutate(cum_c=cumsum(censor)) %>%
  select(new_week, cum_d, cum_c) %>%
  group_by(new_week) %>%
  top_n(n=1, wt=cum_d) %>%
  ungroup() %>%
  rbind(c(0, 0, 0),.) %>%
  mutate(dj=cum_d-lag(cum_d)) %>%
  mutate(cj=lead(cum_c)-cum_c) %>%
  right_join(., time_intervals, by=c('new_week'='week')) %>%
  mutate(loss=dj+cj) %>%
  mutate(acum_loss=cumsum(loss)) %>%
  rbind(c(0,0,0,0,0,0,0),.) %>%
  mutate(nj=n_total-lag(acum_loss)) %>%
  mutate(pi=(nj-dj)/nj) %>%
  right_join(., time_intervals, by=c('new_week'='week')) %>%
  mutate(st=cumprod(pi)) %>%
  select(new_week, st) %>%
  rbind(c(0,1),.) 
lowest_prob <- min(KM_table$st)
KM_table2 <-
  KM_table %>%
  mutate(st2=lag(st)) %>%
  select(new_week, st2) %>%
  rename(st=st2) %>%
  rbind(.,KM_table) %>%
  rbind(., c(longest_time, lowest_prob)) %>%
  drop_na()
ggplot(KM_table2, aes(x=new_week, y=st)) +
  geom_line()

# 3
# 3.1 Creating training and test sets

# randomly split the data into training and test sets
library(caret)

set.seed(1)
outcome_code <- as.factor(outcome_code)
PredictiveMatrix <- cbind(new_feature_matrix, outcome_code)
trainIndex <- createDataPartition(PredictiveMatrix$outcome_code, p=0.8, list=FALSE)
trainingSet <- PredictiveMatrix[trainIndex, ]
testSet <- PredictiveMatrix[-trainIndex, ]


# 3.2.1 Fit an elastic net model use lambda = 0.01 and alpha = 1 (LASSO)

net.grid <- expand.grid(.alpha=1,.lambda=0.01)
ElasticNet.train <- train( outcome_code~., data=trainingSet,
                          method="glmnet",
                          tuneGrid=net.grid)

# Accuracy   Kappa    
# 0.8301081  0.2351053
# 3.2.2 Performance Metrics
training_features <-
  trainingSet %>%
  select(-outcome_code)
predicted_outcome <- predict(ElasticNet.train, training_features, type='raw')
confusionMatrix(data=predicted_outcome, reference=trainingSet$outcome_code,
                positive = levels(trainingSet$outcome_code)[2])











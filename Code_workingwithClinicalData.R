##############################
# Yiying Wang
#############################
rm(list=ls())
# 0 load pacakges needed
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

# run this command for date time data 
Sys.setenv(TZ='UTC')



# Building a Cohort Based on Inclusion Criteria
# 1.1 and 1.2 do not need coding. 
# 1.3 Load pf.csv and vent.csv
pf <- read.csv("../hw2/data/pf.csv", as.is = TRUE)
vent <- read.csv("../hw2/data/vent.csv", as.is =TRUE)
head(pf)
head(vent)

# make a dataframe with a single column, icustay_id, they are the patients who had a stable start (they did not have any
# P/F ratios under 250 for the first 12 hours of their ventilation) 
# 1. The beginning of ventilation is listed in vent table, starttime column: start_time = starttime
# 2. If there are multiple vents for a patient, only use the first one
# the piplines: a) for each icustay_id, filter out the ones that has num == 1;
#               b) create the ventilation first 12 hour period start_time and end_time = starttime + 12 hour; 
#               c) select useful columns from pf table: icustay_id, p_charttime, pfratio and inner join the above two tables
#               b) filter out the rows that are NOT in first 12 hour range
#               c) group_by icustay_id, find the smallest pf ratio for each icustay, if the smallest is less than 250, remove
#               d) select the icustay_id column and it is the result
first_12_hour <- 
  vent %>% 
  mutate(start_time = starttime, end_time = starttime + 12*60*60)
stable_12 <- 
  pf %>%
  select(icustay_id, p_charttime, pfratio) %>%
  inner_join(first_12_hour, by = 'icustay_id') %>%
  filter(p_charttime >= start_time, p_charttime <= end_time) %>%
  group_by(icustay_id) %>%
  summarise(minimum = min(pfratio)) %>%
  filter(minimum_pf > 250) %>%
  select(icustay_id)
# Error identified. Error: non-numeric argument to binary operator. The time variables are not represented correctly. 

# 1.4 use as.POSIXct try the code again 
first_12_hour <-  # a dataframe contains the time points for first 12 hours
  vent %>% 
  mutate(start_time = as.POSIXct(starttime), start_plus_12 = as.POSIXct(starttime) + 12*60*60) %>%
  select(icustay_id, start_time, start_plus_12)

stable_12 <- 
  pf %>%
  select(icustay_id, p_charttime, pfratio) %>%
  inner_join(first_12_hour, by = 'icustay_id') %>%
  filter(as.POSIXct(p_charttime) >= start_time, as.POSIXct(p_charttime) < start_plus_12) %>%
  group_by(icustay_id) %>%
  summarise(minimum_pf = min(pfratio)) %>%
  filter(minimum_pf > 250) %>%
  select(icustay_id)

# 1.5 Based on above ICU stays, find the ICU stays have an additional longer-than 3 hour period during which their PF ratio
# remained under 300. 
# 0) subset vent table to only include the ICU stays from above and find their 12 hour after start time point and endtime point
# 1) find all the pf measurements that are under 300 and happened 12 hours or more after the beginning of ventilation,
# but ALSO before the end of ventilation. 
#     a) inner_join stable_12 with pf by = icustay_id, 
#     b) select the rows have pfratio < 300, p_charttime >= end_time, <= endtime in vent table
# Output is a dataframe having the columns icustay_id and p_charttime

time12_andabove <- # a dataframe contains the above seleted ICU 12 hour time point, and end of ventilation time
  stable_12 %>% 
  inner_join(vent, by = 'icustay_id') %>%  # icustay_id, starttime, endtime
  mutate(hour12_timepoint = as.POSIXct(starttime) + 12*60*60, ventend = as.POSIXct(endtime)) %>%
  select(icustay_id, hour12_timepoint, ventend)
table1.5 <-
  pf %>%
  select(icustay_id, p_charttime, pfratio) %>%
  inner_join(time12_andabove, by = 'icustay_id') %>%
  filter(pfratio < 300, p_charttime >= hour12_timepoint, p_charttime < ventend) %>%
  select(icustay_id, p_charttime) 

# 1.6 Using a self-join, build the shortest possible time windows that begin and end with two PF values under 300 and which
# are longer than 3 hours. 

# Convert p_charttime in table1.5 into datetime 
table1.5 <- transmute(table1.5, icustay_id, p_charttime = as.POSIXct(p_charttime))
# Self-join table1.5
bigtable <- table1.5 %>% inner_join(table1.5, by='icustay_id')
# Find all time intervals that are longer than 3 hours
bigtable_over3 <- bigtable %>% filter(p_charttime.y - p_charttime.x > 3*60*60)
# Of all the time periods that are longer than 3 hours, find the smallest possible ones
table1.6a <- 
  bigtable_over3 %>% 
  group_by(icustay_id, p_charttime.x) %>% 
  filter(min_rank(p_charttime.y)<=1) %>% # if start at the same time, keep the one that end time is smallest.
  group_by(icustay_id, p_charttime.y) %>% 
  filter(min_rank(desc(p_charttime.x))<=1) %>% # if end at the same time, keep the one that start time is largest
  rename(window_begin = p_charttime.x, window_end = p_charttime.y) # give the columns proper names


# Using a join with the original measurements we will see if any measurements in the windows
# go above 300, and remove those windows that do.
pf_after12hour <- # a dataframe that has data for selected ICU stays, from 12 hour time point to vent endtime
  pf %>%
  transmute(icustay_id, p_charttime = as.POSIXct(p_charttime), pfratio) %>%
  inner_join(time12_andabove, by = 'icustay_id') %>%
  filter(p_charttime >= hour12_timepoint, p_charttime < ventend) %>%
  select(icustay_id, p_charttime, pfratio)
table1.6b <-
  table1.6a %>%
  inner_join(pf_after12hour, by='icustay_id') %>%
  filter(p_charttime >= window_begin, p_charttime <= window_end) %>%
  group_by(icustay_id, window_begin, window_end) %>%
  summarise(max_pf = max(pfratio)) %>%
  filter(max_pf < 300) %>%  # only take the ones that maximum pf is less than 300
  select(icustay_id, window_begin, window_end)
  

# 1.7 Plot things every once in a while as a sanity check
# join tables to create the dataframe for plot, join by icustay_id
# 1. icustay_id, window_begin, window_end, p_charttime, pfratio, 
# 2. filter, only keep the rows that p_charttime is in inclusive range of window_begin and window_end
# 3. create a new column, isWindow, to indicate if the p_charttime in a row is window border (1) or not (0)
# 4. select columns, icustay_id, p_charttime, isWindow, pfratio
# 5. join with vent start+12 hour table so that the plot table has a column for the start of ventilation time plus 12 hours

df_to_plot <-
  table1.6b %>%
  ungroup() %>%
  inner_join(pf_after12hour, by='icustay_id') %>%
  filter(p_charttime >= window_begin, p_charttime <= window_end) %>%
  mutate(WindowBorder = ifelse(p_charttime == window_begin | p_charttime == window_end, 'yes', 'no')) %>%
  inner_join(time12_andabove, by='icustay_id') %>%
  select(icustay_id, p_charttime, pfratio, WindowBorder, hour12_timepoint, ventend)

# filter(package %in% pkgname)
icu_ids_9 <- c(200059, 200109, 200249, 200250, 200364, 200387, 200438, 200569, 200639)
df_to_plot_9 <- df_to_plot %>% filter(icustay_id %in% icu_ids_9)
df_to_plot_9$icustay_id <- as.factor(df_to_plot_9$icustay_id)
df_to_plot_9$WindowBorder <- as.factor(df_to_plot_9$WindowBorder)
dummy <- df_to_plot_9 %>% select(icustay_id, hour12_timepoint) %>% mutate(p_charttime=hour12_timepoint, pfratio=300) %>% group_by(icustay_id)

g <- ggplot(df_to_plot_9, aes(x=p_charttime, y=pfratio))
g+geom_point(aes(color=WindowBorder))+ 
  geom_vline(aes(xintercept=as.numeric(df_to_plot_9$hour12_timepoint)),df_to_plot_9, color='green') + 
  geom_hline(yintercept = 300) +
  facet_wrap(~icustay_id, scales = "free_x", shrink=FALSE) +
  geom_blank(data = dummy) +
  xlab("datetime") + ylab("PF ratio") +
  scale_x_datetime() + 
  theme(axis.text.x=element_text(angle = -30, hjust = 0))

# 1.8 Find the point in time at which the clinician would have concluded that the patient was experiencing the
# condition. The index time is the end of the first window for each patient. 
# In table1.6b table, group_by icustay_id, find the minimum window end for each icustay_id
# Integrate with icustays.csv, output dataframe having three columns, icustay_id, subject_id, index_time
# If a patient has more than one ICU stay, only use the first
icustays <- read.csv("../hw2/data/icustays.csv", as.is = TRUE)
icu_subject <- icustays %>% select(icustay_id, subject_id)
icu_sub_index <- 
  table1.6b %>%
  group_by(icustay_id) %>%
  mutate(index_time = min(window_end)) %>%  # create a new variable index_time, it equals to window_end
  select(icustay_id, index_time) %>%  
  distinct(.keep_all=TRUE) %>%   # remove duplicates 
  inner_join(icu_subject, by='icustay_id') %>%  
  group_by(subject_id) %>%  # for patients have more than one icu, only keep the first
  arrange(icustay_id) %>% 
  top_n(1, desc(icustay_id))

# 2 Building a Patient-Feature Matrix for this Cohort
cohort <- read.csv("../hw2/data/cohort.csv", as.is = TRUE)
# 2.1
diagnoses_icd <- read.csv("../hw2/data/diagnoses_icd.csv", as.is = TRUE)
# 2.2 
mystery <- read.csv("../hw2/data/mystery.csv", as.is = TRUE)
sub_adm_dischtime <- mystery %>% select(subject_id, hadm_id, dischtime)
sub_adm_diag_icu <- diagnoses_icd %>% select(subject_id, hadm_id, icd9_code, icustay_id)

table2.2 <- 
  cohort %>%
  inner_join(sub_adm_diag_icu, by = c("subject_id", "icustay_id")) %>%
  inner_join(sub_adm_dischtime, by = c('subject_id', 'hadm_id')) %>%
  filter(dischtime < index_time) %>% # diagnoses occur before the index time
  rename(diagnosis_time = dischtime, diagnosis = icd9_code) %>%
  select(subject_id, diagnosis_time, diagnosis, index_time)
  
# 2.3 what are the top 10 most common diagnosis codes? 
icd9_group <-
  table2.2 %>%
  group_by(diagnosis) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
  
icd9_top_10 <- icd9_group %>% head(10)

# 2.4 Make a plot of the number of codes that are present in N number of patients. 
# x-axis should be the number of patients, y-axis should be the number of codes that are present in that number
# of patients. 

####### NEED CHANGE #####
# group_by code, unique the subject, count the subject, to find how many patients this code occurs, then count number 

df_to_plot_2.4 <-
  icd9_group %>%
  rename(code_appear_times = n) %>% 
  group_by(code_appear_times) %>%
  summarise(n=n()) # n is number of patients

# the number of codes that appear in m patients is same as how many times the code appears
qplot(x=code_appear_times, y=n, data=df_to_plot_2.4, geom='point', xlab ='Number of patients',
      ylab = 'Number of codes')
# Version 2, bar plot
ggplot(data=icd9_group) +
  geom_bar(mapping=aes(x=n)) + 
  labs(x='Number of patients', y='Number of codes')
  
# 2.5 Use ICs we have calculated in combination with SNOMED CT's concept hierarchy to aggregate ICD9 codes into their parent
# categories within a specific IC range. 

# 1. Load the files: icd9_cui.csv, child_parent_cui.csv, cui_ic.csv. 
icd9_cui <- read.csv("../hw2/data/icd9_cui.csv", as.is = TRUE)
child_parent_cui <- read.csv("../hw2/data/child_parent_cui.csv", as.is = TRUE)
cui_ic <- read.csv("../hw2/data/cui_ic.csv", as.is = TRUE)
# 2. Join icd9_cui and child_parent_cui to get all the parent concepts for each ICD9 code. How many parent concepts does ICD9
# code 401.9 have?
icd9_parent <- 
  icd9_cui %>%
  inner_join(child_parent_cui, by = c('cui'= 'child')) %>%
  distinct()
icd9_401.9 <- filter(icd9_parent, icd9 == 401.9)
# 10 including itself


# 2.6 What is the range (min and max) of ICs observed in the data? What are the 10 most general CUIs?
# Assume the data means the cui_ic table
cui_ic %>% summarise(min_ic = min(ic))  # minimum ic is 0.4280127
cui_ic %>% summarise(max_ic = max(ic))  # maximum ic is 20.41637
most_general_cui_10 <-
  cui_ic %>%
  arrange(ic) %>%
  head(10)

# 2.7 For each icd9 code, find its parent CUIs with an IC between 4 and 8, and keep only the most specific parent CUI.
# output is a dataframe with three columns, icd9, cui, parent_cui, ic
# 1. based on icd9_parent table, join with cui_ic table on parent cui, so that each row is icd9, cui, parent_cui, ic
# select the rows that ic is in between 4 and 8 inclusive and if multiple exist, pick the highest ic

table2.7 <-
  icd9_parent %>%
  inner_join(cui_ic, by=c('parent'='cui')) %>%
  filter(ic>=4, ic<=8) %>%
  group_by(icd9) %>%  # one icd9 code can have multiple cui for it
  filter(ic==max(ic)) %>%
  top_n(1, cui) %>%
  top_n(1, parent) %>%
  ungroup() %>%
  transmute(icd9, cui, parent_cui = parent, ic)
# NOTE: for the case where icd9 has more than one parent cui, I keep the first one.

  

# 2.8 Use table2.7 to replace diagnoses in the dx_cohort with their parent CUI that is in the desired IC range.
# Noticed there are some regions in the table icd9-parent_cui table, take theese as special cases and replace
# parent_cui id for these special cases in table 2.2 first. 
############ Special Cases: 
# 327 -> C1561892
# 5401 -> C0577030
# numbers starts with 800 - 829 -> C3872870
# 042 -> C0042769

# this is a helper function to extract the last n character from a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
### Convert the icd9 code in table2.2 to the format of icd9 code in table2.7
replace_special <- table2.2
for (i in 1:nrow(replace_special)){
  if (replace_special[i, 'diagnosis'] == "042"){
    replace_special[i, 'diagnosis'] = "C0042769"
  }else if (replace_special[i, 'diagnosis'] == "5401"){
    replace_special[i, 'diagnosis'] = "C0577030"
  }else if (replace_special[i, 'diagnosis']=="327"){
    replace_special[i, 'diagnosis'] = "C1561892"
  }else if (substr(replace_special[i, 'diagnosis'], 1, 1) == '8'){
    first_three = as.numeric(substr(replace_special[i, 'diagnosis'], 1, 3))
    if (first_three >= 800 & first_three <= 829){
      replace_special[i, 'diagnosis'] = "C3872870"
    }
  }else if (substr(replace_special[i, 'diagnosis'], 1, 1) == 'E'){
    if (nchar(replace_special[i, 'diagnosis']) == 4){
      replace_special[i, 'diagnosis'] = replace_special[i, 'diagnosis']
    }else if (substrRight(replace_special[i, 'diagnosis'], 1)=="0"){
      replace_special[i, 'diagnosis'] = substr(replace_special[i, 'diagnosis'], 1, 4)
    }else{ # add a point between third and fourth digits
      replace_special[i, 'diagnosis'] = paste(substr(replace_special[i, 'diagnosis'], 1, 4), ".", substr(replace_special[i, 'diagnosis'], 5, 5), sep = "")
    }
  }else{ 
    if (nchar(replace_special[i, 'diagnosis']) > 3){
      n = nchar(replace_special[i, 'diagnosis'])
      if (n == 5 & substr(replace_special[i, 'diagnosis'], 4, n) == '00'){
        replace_special[i, 'diagnosis'] = substr(replace_special[i, 'diagnosis'], 1, 3)
      }else if(n == 5 & substr(replace_special[i, 'diagnosis'], 5, n) == '0'){
        replace_special[i, 'diagnosis'] = paste(substr(replace_special[i, 'diagnosis'], 1, 3), ".", substr(replace_special[i, 'diagnosis'], 4, 4), sep = "")
      }else if (n == 4 & substr(replace_special[i, 'diagnosis'], 4, n) == '0'){
        replace_special[i, 'diagnosis'] = substr(replace_special[i, 'diagnosis'], 1, 3)
      }else{
        replace_special[i, 'diagnosis'] = paste(substr(replace_special[i, 'diagnosis'], 1, 3), ".", substr(replace_special[i, 'diagnosis'], 4, n), sep = "")
      }
      if (substr(replace_special[i, 'diagnosis'], 1, 2) == '00'){
        n = n = nchar(replace_special[i, 'diagnosis'])
        replace_special[i, 'diagnosis'] = substr(replace_special[i, 'diagnosis'], 3, n)
      }else if(substr(replace_special[i, 'diagnosis'], 1, 1) == '0'){
        n = nchar(replace_special[i, 'diagnosis'])
        replace_special[i, 'diagnosis'] = substr(replace_special[i, 'diagnosis'], 2, n)
      }
    }
  }
}
# Convert icd9_code in table2.2 to the SNOMED version of icd9_code

icd9_to_cui <-
  replace_special %>%
  left_join(table2.7, by = c('diagnosis' = 'icd9'))

# replace diagnosis with parent_cui if parent_cui is present
table2.8 <- icd9_to_cui
for (i in 1:nrow(table2.8)){
  if (!is.na(table2.8[i, 'parent_cui'])){
    table2.8[i,'diagnosis'] <- table2.8[i,'parent_cui']
  }
}
table2.8_final <-
  table2.8 %>%
  select(subject_id, diagnosis_time, diagnosis, index_time)


# 2.11
notes <- read.csv("../hw2/data/notes.csv", as.is = TRUE)

snomed_ct_isaclosure <- read.csv("../hw2/data/snomed_ct_isaclosure.csv", as.is = TRUE)
snomed_ct_str_cui<- read.csv("../hw2/data/snomed_ct_str_cui.csv", as.is = TRUE)
    
des_ances <- select(snomed_ct_isaclosure, descendant, ancestor)
cui_str <-
  des_ances %>%
  inner_join(snomed_ct_str_cui, by=c('descendant'='CUI')) %>%
  select(ancestor, str) %>%
  rename(cui=ancestor) %>%
  distinct()

# 2.12 
# C0035327
RSD <- 
  cui_str %>%
  filter(cui=='C0035237') %>%
  filter(nchar(str)<=20)
dict <- RSD$str

# another way: 
des_ances <- select(snomed_ct_isaclosure, descendant, ancestor)
cui_str <-
  des_ances %>%
  inner_join(snomed_ct_str_cui, by=c('descendant'='CUI')) %>%
  select(ancestor, str) %>%
  rename(cui=ancestor) %>%
  distinct()

# 2.12 
# C0035327
RSD <- 
  cui_str %>%
  filter(cui=='C0035237') %>%
  filter(nchar(str)<=20)
dict <- RSD$str

##############################
# Yiying Wang
#############################

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
  filter(ventnum == 1) %>%
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
  filter(ventnum == 1) %>%
  mutate(start_time = as.POSIXct(starttime), end_time = as.POSIXct(starttime) + 12*60*60)
stable_12 <- 
  pf %>%
  select(icustay_id, p_charttime, pfratio) %>%
  inner_join(first_12_hour, by = 'icustay_id') %>%
  filter(as.POSIXct(p_charttime) >= start_time, as.POSIXct(p_charttime) <= end_time) %>%
  group_by(icustay_id) %>%
  summarise(minimum_pf = min(pfratio)) %>%
  filter(minimum_pf > 250) %>%
  select(icustay_id)
# 5308 rows


# 1.5 Based on above ICU stays, find the ICU stays have an additional longer-than 3 hour period during which their PF ratio
# remained under 300. 
# 0) subset vent table to only include the ICU stays from above and find their 12 hour after start time point and endtime point
# 1) find all the pf measurements that are under 300 and happened 12 hours or more after the beginning of ventilation,
# but ALSO before the end of ventilation. 
#     a) inner_join stable_12 with pf by = icustay_id, 
#     b) select the rows have pfratio < 300, p_charttime >= end_time, <= endtime in vent table
# Output is a dataframe having the columns icustay_id and p_charttime

time12_andabove <- # a dataframe contains the above seleted ICU and their 12 hour time point and vent endtime
  stable_12 %>% 
  inner_join(filter(vent, ventnum==1), by = 'icustay_id') %>%
  mutate(hour12_timepoint = as.POSIXct(starttime) + 12*60*60, ventend = as.POSIXct(endtime)) %>%
  select(icustay_id, hour12_timepoint, ventend)
pf_under300 <-
  pf %>%
  select(icustay_id, p_charttime, pfratio) %>%
  inner_join(time12_andabove, by = 'icustay_id') %>%
  filter(pfratio < 300, p_charttime >= hour12_timepoint, p_charttime < ventend) %>%
  select(icustay_id, p_charttime)
# 12004 rows

# 1.6 Using a self-join, build the shortest possible time windows that begin and end with two PF values under 300 and which
# are longer than 3 hours. 
pf_under300 <- transmute(pf_under300, icustay_id, p_charttime = as.POSIXct(p_charttime))
bigtable <- pf_under300 %>% inner_join(pf_under300, by='icustay_id')
bigtable_over3 <- bigtable %>% filter(p_charttime.y - p_charttime.x > 3*60*60)
time_windows <- 
  bigtable_over3 %>% 
  group_by(icustay_id, p_charttime.x) %>% 
  filter(min_rank(p_charttime.y)<=1) %>% 
  group_by(icustay_id, p_charttime.y) %>% 
  filter(min_rank(desc(p_charttime.x))<=1) %>%
  rename(window_begin = p_charttime.x, window_end = p_charttime.y)

# 8378 rows

# Using a join with the original measurements we will see if any measurements in the windows
# go above 300, and remove those windows that do.
pf_after12hour <- 
  pf %>%
  transmute(icustay_id, p_charttime = as.POSIXct(p_charttime), pfratio) %>%
  inner_join(time12_andabove, by = 'icustay_id') %>%
  filter(p_charttime >= hour12_timepoint, p_charttime < ventend) %>%
  select(icustay_id, p_charttime, pfratio)
time_window_noabove300 <-
  time_windows %>%
  inner_join(pf_after12hour, by='icustay_id') %>%
  filter(p_charttime >= window_begin, p_charttime <= window_end) %>%
  group_by(icustay_id, window_begin, window_end) %>%
  summarise(max_pf = max(pfratio)) %>%
  filter(max_pf < 300) %>%  # make sure the maximum of pf in this range is less than 300
  select(icustay_id, window_begin, window_end)
  

# 1.7 Plot things every once in a while as a sanity check
# join tables to create the dataframe for plot, join by icustay_id
# 1. icustay_id, window_begin, window_end, p_charttime, pfratio, 
# 2. filter, only keep the rows that p_charttime is in inclusive range of window_begin and window_end
# 3. create a new column, isWindow, to indicate if the p_charttime in a row is window border (1) or not (0)
# 4. select columns, icustay_id, p_charttime, isWindow, pfratio
# 5. join with vent start+12 hour table so that the plot table has a column for the start of ventilation time plus 12 hours

df_to_plot <-
  time_window_noabove300 %>%
  ungroup() %>%
  inner_join(pf_after12hour, by='icustay_id') %>%
  filter(p_charttime >= window_begin, p_charttime <= window_end) %>%
  mutate(WindowBorder = ifelse(p_charttime == window_begin | p_charttime == window_end, 'yes', 'no')) %>%
  inner_join(time12_andabove, by='icustay_id') %>%
  select(icustay_id, p_charttime, pfratio, WindowBorder, hour12_timepoint, ventend)

icu_ids_9 <- c(200059, 200109, 200249, 200250, 200364, 200387, 200438, 200569, 200639)
df_to_plot_9 <- df_to_plot %>% filter(icustay_id == 200059|icustay_id == 200109|icustay_id == 200249|
                                        icustay_id == 200250|icustay_id == 200364|icustay_id == 200387|
                                        icustay_id == 200438|icustay_id == 200569|icustay_id == 200639)
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














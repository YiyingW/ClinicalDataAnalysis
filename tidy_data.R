library(readr)
library(dplyr)
library(readxl)
library(lubridate)
daily_spec <- read_csv("daily_SPEC_2014.csv.bz2")
# What is average Arithmetic.Mean for “Bromine PM2.5 LC” in the state of Wisconsin in this dataset?
wisconsin_PM2.5_mean <-
  daily_spec %>%
  filter(`State Name` == 'Wisconsin' & `Parameter Name` == 'Bromine PM2.5 LC') %>% 
  summarise(mean = mean(`Arithmetic Mean`))

# Which constituent Parameter.Name has the highest average level?
chemical_constituent_sort <-
  daily_spec %>%
  group_by(`Parameter Name`) %>%
  summarise(mean = mean(`Arithmetic Mean`)) %>%
  arrange(desc(mean))
  
# Which monitoring site has the highest average level of “Sulfate PM2.5 LC” across all time?
highest_sulfate <-
  daily_spec %>%
  select(`State Code`, `County Code`, `Site Num`, `Parameter Name`, `Arithmetic Mean`) %>%
  filter(`Parameter Name` == 'Sulfate PM2.5 LC') %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarise(mean = mean(`Arithmetic Mean`)) %>%
  arrange(desc(mean)) 

# What is the absolute difference in the average levels of “EC PM2.5 LC TOR” between the states 
# California and Arizona, across all time and all monitoring sites?
EC_PM2.5_CA_AZ <-
  daily_spec %>%
  select(`State Name`, `Parameter Name`, `Arithmetic Mean`) %>%
  filter((`State Name` == 'California' | `State Name` == 'Arizona') & `Parameter Name` == 'EC PM2.5 LC TOR') %>%
  group_by(`State Name`) %>%
  summarise(mean = mean(`Arithmetic Mean`)) %>%
  summarise(abs(diff(mean)))

# What is the median level of “OC PM2.5 LC TOR” in the western United States, across all time? 
# Define western as any monitoring location that has a Longitude LESS THAN -100.
western_median_OC <-
  daily_spec %>%
  select(`Longitude`, `Parameter Name`, `Arithmetic Mean`) %>%
  filter(`Longitude` < -100 & `Parameter Name` == 'OC PM2.5 LC TOR') %>%
  summarise(median = median(`Arithmetic Mean`))
  
# Load in a second data file
metaData <- read_excel("aqs_sites.xlsx")
# How many monitoring sites are labelled as both RESIDENTIAL for "Land Use"
# and SUBURBAN for "Location Setting"?
problem6 <- 
  metaData %>%
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>%
  count()

# What is the median level of “EC PM2.5 LC TOR” amongst monitoring sites that are labelled as both “RESIDENTIAL” 
# and “SUBURBAN” in the eastern U.S., where eastern is defined as Longitude greater than or equal to -100?
problem7 <-
  metaData %>%
  select(`Longitude`,`Latitude`, `Land Use`, `Location Setting`) %>%
  left_join(daily_spec %>% select(`Longitude`,`Latitude`, `Parameter Name`, `Arithmetic Mean`)) %>%
  filter(`Land Use` == "RESIDENTIAL" & `Location Setting` == "SUBURBAN" & `Longitude` >= -100 
         & `Parameter Name` == "EC PM2.5 LC TOR") %>%
  summarise(median = median(`Arithmetic Mean`))
  
# Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", which month of the year has the 
# highest average levels of "Sulfate PM2.5 LC"?
problem8 <- 
  metaData %>%
  select(`Longitude`,`Latitude`, `Land Use`) %>%
  filter(`Land Use` == "COMMERCIAL") %>%
  left_join(daily_spec %>% select(`Longitude`, `Latitude`, `Parameter Name`, `Arithmetic Mean`, `Date Local`)) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  mutate(Month = month(as.POSIXlt(`Date Local`, format = "%Y-%m-%d"))) %>%
  group_by(Month) %>%
  summarise(mean = mean(`Arithmetic Mean`)) %>%
  arrange(desc(mean))

# for how many days is the sum of "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" greater than 10?
problem9 <- 
  daily_spec %>%
  select(`State Code`, `County Code`, `Site Num`, `Parameter Name`, `Date Local`, `Arithmetic Mean`) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC" | `Parameter Name` == "Total Nitrate PM2.5 LC") %>%
  filter(`State Code` == "06" & `County Code` == "065" & `Site Num` == "8001") %>%
  group_by(`Date Local`, `Parameter Name`) %>%
  summarise(mean = mean(`Arithmetic Mean`)) %>% 
  group_by(`Date Local`) %>%
  summarise(sum = sum(mean)) %>%
  filter(sum > 10) %>%
  count()

# Which monitoring site in the dataset has the highest correlation between "Sulfate PM2.5 LC" and 
# "Total Nitrate PM2.5 LC" across all dates?
problem10 <-
  daily_spec %>%
  select(`State Code`, `County Code`, `Site Num`, `Parameter Name`, `Date Local`, `Arithmetic Mean`) %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC" | `Parameter Name` == "Total Nitrate PM2.5 LC") %>% 
  group_by(`Parameter Name`, `Date Local`) %>%
  mutate(mean = mean(`Arithmetic Mean`)) %>%
  select(`State Code`, `County Code`, `Site Num`, `Parameter Name`, `Date Local`, mean) %>%
  ungroup(`Parameter Name`, `Date Local`) %>%
  unique() %>%
  group_by(`State Code`, `County Code`, `Site Num`,`Date Local`) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  ungroup(`State Code`, `County Code`, `Site Num`, `Date Local`) %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarise(cor = cor(mean[`Parameter Name` == "Sulfate PM2.5 LC"], 
                     mean[`Parameter Name` == "Total Nitrate PM2.5 LC"])) %>%
  arrange(desc(cor))
  

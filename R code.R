# Libraries
library(haven)
library(tidyverse)
library(janitor)
library(sjmisc)
library(sjstats)
library(psych)
library(summarytools)
library(skimr)

dataset <- read_sav("location")
View(dataset)

names(dataset)
glimpse(dataset)  

# drop columns that are definitely not needed
removed_cols <- c("series_add", "sal2_di", "sal2_di", "tol3_di", "mood6_di", "rel7_di", "with9_di", "conf12_di", "prob14_di", "series_cutoff", "series_cutoff_di")

dataset <- dataset %>% 
  select(-one_of(removed_cols))

# look at summaries to identify variables that need to be manipulated
summary(dataset)

# recode all 1-7 scaled variables to 1-7, outlier values are NAs
data_new <- dataset %>% 
  mutate_at(
    vars(surg1:online7),
    ~ ifelse(. < 1 | . > 7, NA, .))

glimpse(data_new)

data_new <- data_new %>% 
  select(-one_of(removed_cols))

# verify recoding
summary(data_new)
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
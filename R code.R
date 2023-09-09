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

# recode place of residence
freq(data_new$Jelenleghollaksz)

data_new <- data_new %>% 
  mutate(residence = recode(Jelenleghollaksz,
                            'Külföldön' = 1,
                            'Magyarországon' = 0) %>% 
           as.numeric()) %>% 
  select(-Jelenleghollaksz)

freq(data_new$residence)

# recode family status
freq(data_new$Családiállapot)

data_new <- data_new %>% 
  mutate(family_status = case_when(
    Családiállapot %in% c('alapvetően egyedülálló, de vannak alkalmi kapcsolataim', 'egyedülálló', 'elvált') ~ 0,
    Családiállapot %in% c('alapvetően párkapcsolatban, de emellett előfordul(nak) alkalmi kapcsolat(ok) is', 'eljegyzett', 'házas', 'párkapcsolatban és egy háztartásban élünk a partneremmel', 'párkapcsolatban, de külön háztartásban élünk a partneremmel') ~ 1,
    TRUE ~ NA_integer_
  )) %>% 
  select(-Családiállapot)

glimpse(data_new)

freq(data_new$Egyátlagoshétköznaponhányóraszabadidővelrendelkezel)
freq(data_new$Egyátlagoshétvéginaponhányóraszabadidővelrendelkezel)

data_new <- data_new %>%
  rename(time_weekday = Egyátlagoshétköznaponhányóraszabadidővelrendelkezel,
         time_weekend = Egyátlagoshétvéginaponhányóraszabadidővelrendelkezel) %>% 
  mutate(time_weekday = ifelse(time_weekday > 720, NA, time_weekday),
         time_weekend = ifelse(time_weekend > 720, NA, time_weekend))

glimpse(data_new)
freq(data_new$time_weekday)
freq(data_new$time_weekend)

summary(data_new)

data_new <- data_new %>% 
  select(-one_of(removed_cols))

glimpse(data_new)

# create item lists for alphas and descriptives
list_surg <- c("surg1", "surg2", "surg3", "surg4", "surg5", "surg6")
list_gyasz <- c("gyasz1", "gyasz2", "gyasz3", "gyasz4", "gyasz5", "gyasz6")
list_teli <- c("telitodes1", "telitodes2", "telitodes3", "telitodes4", "telitodes5", "telitodes6")
list_menny <- c("mennyiseg1", "mennyiseg2", "mennyiseg3", "mennyiseg4", "mennyiseg5", "mennyiseg6")
list_cont <- c("kontrollvesztes1", "kontrollvesztes2", "kontrollvesztes3", "kontrollvesztes4", "kontrollvesztes5", "kontrollvesztes6")
list_iden <- c("identifikacio1", "identifikacio2", "identifikacio3", "identifikacio4", "identifikacio5", "identifikacio6")
list_offl <- c("offline1", "offline2", "offline3", "offline4", "offline5", "offline6", "offline7")
list_onli <- c("online1", "online2", "online4", "online5", "online6", "online7")
list_onfe <- c("onfejlesztes1", "onfejlesztes2", "onfejlesztes3", "onfejlesztes4", "onfejlesztes5", "onfejlesztes6")
list_bunt <- c("buntudat1", "buntudat2", "buntudat3", "buntudat4", "buntudat5", "buntudat6")
list_prob <- c("sal1", "sal2", "tol3", "tol4", "erz5", "erz6", "rel7", "rel8", "meg9", "meg10", "kon11", "kon12", "pro13", "pro14")

# descriptives 
averages <- data_new %>%
  transmute(surg_m = mean_n(select(., all_of(list_surg)), n = .5),
            gyasz_m = mean_n(select(., all_of(list_gyasz)), n = .5),
            teli_m = mean_n(select(., all_of(list_teli)), n = .5),
            menny_m = mean_n(select(., all_of(list_menny)), n = .5),
            cont_m = mean_n(select(., all_of(list_cont)), n = .5),
            iden_m = mean_n(select(., all_of(list_iden)), n = .5),
            offl_m = mean_n(select(., all_of(list_offl)), n = .5),
            onli_m = mean_n(select(., all_of(list_onli)), n = .5),
            onfe_m = mean_n(select(., all_of(list_onfe)), n = .5),
            bunt_m = mean_n(select(., all_of(list_bunt)), n = .5),
            prob_m = mean_n(select(., all_of(list_prob)), n = .5))

descr(averages)
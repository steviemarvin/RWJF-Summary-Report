## ASC Data
library(ipumsr)
library(here)
library(tidyverse)
library(epiextractr)
library(stats)
library(epidatatools) # summarize_groups
library(data.table)
library(readxl)
library(haven)
library(janitor)
library(xlsx)
library(openxlsx)
library(realtalk) #cpi_u_rs_annual
library(fastDummies) # create indicator variables 


## load data here
ddi <-  read_ipums_ddi(here("data/usa_00017.xml"))
acs_raw <- read_ipums_micro(ddi, verbose = FALSE)
names(acs_raw) <- tolower(names(acs_raw))
acs_raw <- select(acs_raw, year, sample, serial, hhwt, expwth, ownershp, owncost, rentgrs, hhincome,
                  perwt, nchild, eldch, yngch, related, sex, age, marst, raced, hispan, bpl,
                  ftotinc, incwage, poverty)

source(here("code/acs_functions.R"), echo = TRUE)

acs_raw1 <- filter(acs_raw, year <= 2015)
acs_raw2 <- filter(acs_raw, year > 2015 & year <= 2022)

acs_clean1 <- cleaning_fun(data = acs_raw1)
acs_clean2 <- cleaning_fun(data = acs_raw2)

acs_li_families1 <- families_fun(data = acs_clean1)
acs_li_families2 <- families_fun(data = acs_clean2)

acs_housing1 <- housing_df_fun(data = acs_clean1, data2 = filter(acs_li_families1, hh_type2 == "hhYes"))
acs_housing2 <- housing_df_fun(data = acs_clean2, data2 = filter(acs_li_families2, hh_type2 == "hhYes"))

housing_stats1 <- renter_stats_fun(data = acs_housing1)
housing_stats2 <- renter_stats_fun(data = acs_housing2)

housing_stats <- bind_rows(housing_stats1, housing_stats2)

samplesize1 <- acs_housing1 %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_hispanic | wbhaa_AIAN | wbhaa_AAPI,
                   n = n()) %>% 
  filter(group_value == 1) %>% select(-group_value)

samplesize2 <- acs_housing2 %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_hispanic | wbhaa_AIAN | wbhaa_AAPI,
                   n = n()) %>% 
  filter(group_value == 1) %>% select(-group_value) %>% 
  rbind(samplesize1)
  
##** Benchmarking data comparisons  BLS race and ethnicity not mutually exclusive **##
# RWJF Summary Report Background Section:

library(tidyverse)
library(epiextractr)
library(stats)
library(haven)
library(epidatatools)
library(here)

# replicate bls data use basic
raw_data <- load_basic (2007:2022, year, month, hhid, age, agechild, female, married, ownchild, basicwgt, famtype,
                        wage, wageotc, hoursu1, hoursut, emp, unemp, faminc, citistat, nilf, famrel, 
                        raceorig, hispanic)
# cleaning 
basic_data <- raw_data %>% 
  filter(agechild > 0, !is.na(married), !is.na(emp), age >= 16, !is.na(nilf)) %>% 
  mutate(wgt = basicwgt / 12, across(female | raceorig | hispanic | married, ~as.character(as_factor(.x))),
         full_time = if_else(hoursu1 >= 35, 1, 0),
         part_time = if_else(full_time == 0, 1, 0),
         full_time = labelled(full_time, c("Works full-time" = 1, "Works part-time" = 0)),
         part_time = labelled(part_time, c("Works part-time" = 1, "Works full-time" = 0)))


# standard cleaning for table 5
basic_data <- raw_data %>% 
  filter(agechild > 0, !is.na(married), !is.na(emp), age >= 16, !is.na(nilf)) %>% 
  mutate(wgt = basicwgt / 12, across(female | wbhao | raceorig | hispanic | married, ~as.character(as_factor(.x))))


## table 1 = 
# BLS def: the count of families is for "primary" families only
totfam <- family_data %>%
  filter(married == "Married") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE))

## table 4 = families with own children: employment status of parents by age of youngest child and family type


## table 5a = employment characteristics for women who have own children under 18
## Raceorig + hispanic variables used for mutually excl. race and ethnicity variables

### overall 
t5 <- basic_data %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            urate = weighted.mean(unemp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            epop = emp / civpop) %>% 
  select(year, civpop, laborforce, lfpr, urate, emp, epop)

### women
t5a <- basic_data %>% 
  filter(female == "Female") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            urate = weighted.mean(unemp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            epop = emp / civpop) %>% 
  select(year, civpop, laborforce, lfpr, urate, emp, epop)

### men
t5b <- basic_data %>% 
  filter(female == "Male") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            urate = weighted.mean(unemp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            epop = emp / civpop) %>% 
  select(year, civpop, laborforce, lfpr, urate, emp, epop)

## marriage
t518_Married <- basic_data %>% 
  filter(married == "Married") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            urate = weighted.mean(unemp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            epop = emp / civpop) %>% 
  select(year, civpop, laborforce, lfpr, urate, emp, epop)

t5b18_Married <- basic_data %>% 
  filter(married == "Married", female == "Male") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            urate = weighted.mean(unemp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            epop = emp / civpop) %>% 
  select(year, civpop, laborforce, lfpr, urate, emp, epop)

t5a18_Married <- basic_data %>% 
  filter(married == "Married", female == "Female") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            urate = weighted.mean(unemp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            epop = emp / civpop) %>% 
  select(year, civpop, laborforce, lfpr, urate, emp, epop)

## race disaggregation, testing Asian Women stats
table5aRaceorig <- basic_data %>% 
  filter(female == "Female", raceorig == "Asian only") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            urate = weighted.mean(unemp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            epop = emp / civpop) %>% 
  select(year, civpop, laborforce, lfpr, urate, emp, epop)

## table 5a = employment characteristics for women who have own children under 18
## wbhao not race and ethnicity mutually exclusive
table5aWbhao <- basic_data %>% 
  filter(female == "Female", wbhao == "Asian") %>% 
  group_by(year, wbhao) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            urate = weighted.mean(unemp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
           epop = emp / civpop) %>% 
  arrange(year, match(wbhao, c("White", "Black", "Hispanic", "Asian", "Other"))) %>% 
  select(year, wbhao, civpop, lfpr, urate, emp, epop)



t5a6White_test <- basic_data %>% 
  filter(female == "Female", raceorig == "White", (agechild != 3 & agechild != 4 & agechild != 10)) %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            emp = sum(emp * wgt, na.rm = TRUE),
            epop = emp / civpop) %>% 
  select(year, civpop, lfpr, urate, epop)

# benchmarking total family numbers 2021 ~ 83,144,000 2022 ~ 83,179,000
fam_test <- raw_data %>% 
  filter(!is.na(married), !is.na(emp), age >= 16, !is.na(nilf), famrel > 0, famtype == 1) %>% 
  mutate(wgt = basicwgt / 12, across(female | raceorig | hispanic, ~as.character(as_factor(.x)))) %>% 
  group_by(year, month) %>% 
  # dup variable to confirm hypothesis that there are either 2 or 1 hhid per year/month, dependent on marital status of respondent (2 for married, 1 for not married)
  mutate(dup = duplicated(hhid)) %>% 
  filter(dup == FALSE) %>% 
  group_by(year) %>% 
  summarize(totfam = sum(wgt, na.rm = TRUE))

fam_test_wageAndchildrenfilter <- raw_data %>% 
  filter(!is.na(married), !is.na(emp), age >= 16, !is.na(nilf), famrel > 0, famtype == 1, agechild > 0, ownchild > 0, !is.na(wage)) %>% 
  mutate(wgt = basicwgt / 12, across(female | raceorig | hispanic, ~as.character(as_factor(.x)))) %>% 
  group_by(year, month) %>% 
  # dup variable to confirm hypothesis that there are either 2 or 1 hhid per year/month, dependent on marital status of respondent (2 for married, 1 for not married)
  mutate(dup = duplicated(hhid)) %>% 
  filter(dup == FALSE) %>% 
  group_by(year) %>% 
  summarize(totfam = sum(wgt, na.rm = TRUE))

fam_test_wageAndchildrenfilter <- raw_data %>% 
  filter(!is.na(married), !is.na(emp), age >= 16, !is.na(nilf), famrel > 0, famtype == 1, agechild > 0, ownchild > 0) %>% 
  mutate(wgt = basicwgt / 12, across(female | raceorig | hispanic, ~as.character(as_factor(.x)))) %>% 
  group_by(year, month) %>% 
  # dup variable to confirm hypothesis that there are either 2 or 1 hhid per year/month, dependent on marital status of respondent (2 for married, 1 for not married)
  mutate(dup = duplicated(hhid)) %>% 
  filter(dup == FALSE) %>% 
  group_by(year) %>% 
  summarize(totfam = sum(wgt, na.rm = TRUE))


table5wage <- basic_data %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop,
            pt = sum(part_time * wgt, na.rm = TRUE),
            pt_share = pt / emp,
            pt2 = sum(part_time2 * wgt, na.rm = TRUE),
            pt_share2 = pt / emp)%>% 
  select(year, civpop, lfpr, urate, epop, pt, pt2, pt_share, pt_share2)

test_data <- load_basic(2023, year, wage, hoursu1i, basicwgt, emp, nilf) %>% 
  mutate(ftpt = case_when(hoursu1i >= 35 ~ "Full-time",
                          emp == 0 | nilf == 1 ~ "Unemployed/NILF",
                          is.na(emp) ~ NA_character_,
                          TRUE ~ "Part-time")) %>% 
  crosstab(emp, ftpt)
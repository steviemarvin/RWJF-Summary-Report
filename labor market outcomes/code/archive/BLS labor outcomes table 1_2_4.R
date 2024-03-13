# RWJF Summary Report Background Section:
## Labor Market Outcomes and Poverty Statistics by Race and Ethnicity
## Using CPS microdata and framed by BLS Employment Characteristics for Families

# Definitions:
## Children: own children under age 18 that live in the household. Included are biological, step-, and adopted children of the husband, wife, or person maintaining the family. 
## Children cont.: Not incl. nieces, nephews, grandchildren, other related children, and unrelated children living in hh. 

# goals:
## identifying number of families by race and ethnicity 2007 - 2022


library(tidyverse)
library(epiextractr)
library(stats)
library(haven)
library(epidatatools)
library(here)
library(data.table)

raw_data <- load_basic(2007:2022, year, month, hhid, age, raceorig, hispanic, agechild, female, married, ownchild, basicwgt, famtype,
                        wage, wageotc, hoursu1i, emp, unemp, faminc, citistat, nilf, famrel)

# cleaning 
# BLS def: the count of families is for "primary" families only
family_data <- raw_data %>% 
  filter(agechild > 0, ownchild > 0, !is.na(married), !is.na(emp), age >= 16, !is.na(nilf), famrel > 0, famtype == 1) %>% 
  mutate(wgt = basicwgt / 12, across(female | raceorig | hispanic, ~as.character(as_factor(.x))),
         full_time = if_else(hoursu1i >= 35, 1, 0),
         part_time = if_else(full_time == 0, 1, 0),
         full_time = labelled(full_time, c("Works full-time" = 1, "Works part-time" = 0)),
         part_time = labelled(part_time, c("Works part-time" = 1, "Works full-time" = 0)),
         annualwage = wage * hoursu1i * 52,
         annualwage_otc = wageotc * hoursu1i * 52,
         # create variable famsize that determines size of household (married + ownchild + 1)
         famsize = married + ownchild + 1) %>% 
  group_by(year, month) %>% 
  # dup variable to confirm hypothesis that there are either 2 or 1 hhid per year/month, dependent on marital status of respondent (2 for married, 1 for not married)
  mutate(dup = duplicated(hhid))

family_data_filtered <- family_data %>% 
  group_by(year, month, hhid) %>% 
  summarize(combined_annualwage = sum(annualwage, na.rm = TRUE))

# poverty guidelines taken from: https://www.ssa.gov/policy/docs/statcomps/supplement/2022/3e.html#table3.e8
fpl_guide <- read_csv("/projects/smarvin/RWJF Summary/suppdata/FPL_guidelines.csv")
fpl_guide <- fpl_guide[-(17:73),]
fpl_guide <- fpl_guide %>% 
  # manually add thresholds for famsize 9 - 15 <- there's got to be a way to use code for this 
  mutate(across(n1 | i, ~ as.numeric(.x)),
         n9 = n8 + i,
         n10 = n9 + i,
         n11 = n10 + i,
         n12 = n11 + i,
         n13 = n12 + i,
         n14 = n13 + i,
         n15 = n14 + i) %>% 
  # renaming to prep for the reshaping
  setnames(old = c('n1','n2','n3', 'n4', 'n5', 'n6', 'n7', 'n8', 'n9', 'n10', 'n11', 'n12', 'n13', 'n14', 'n15'), 
           new = c('1','2','3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15'), skip_absent = TRUE) %>% 
  select(-('i')) %>% 
  pivot_longer(cols = -year, names_to = "famsize", values_to = "threshold") %>% 
  mutate(across(famsize | threshold, ~ as.numeric(.x)),
         # threshold200 = low-income which we define as 200% of FPL 
         threshold200 = threshold * 2) %>%
  select(year, famsize, threshold200)


# left_join: left -> family_data, right -> fpl_guide c("year", "famsize")
family_data <- family_data %>% 
  left_join(fpl_guide, by = c('year', 'famsize')) %>% 
  left_join(family_data_filtered, by = c('year', 'month', 'hhid')) %>% 
  group_by(year) %>% 
  #filter(combined_annualwage != 0 ) %>% 
  mutate(hhinc = case_when(combined_annualwage < 1 ~ NA,
                           combined_annualwage < 5000 ~ 1,
                           combined_annualwage >= 5000 & combined_annualwage < 7500 ~ 2,
                           combined_annualwage >= 7500 & combined_annualwage < 10000 ~ 3,
                           combined_annualwage >= 10000 & combined_annualwage < 12500 ~ 4,
                           combined_annualwage >= 12500 & combined_annualwage < 15000 ~ 5,
                           combined_annualwage >= 15000 & combined_annualwage < 20000 ~ 6, 
                           combined_annualwage >= 20000 & combined_annualwage < 25000 ~ 7,
                           combined_annualwage >= 25000 & combined_annualwage < 30000 ~ 8,
                           combined_annualwage >= 30000 & combined_annualwage < 35000 ~ 9,
                           combined_annualwage >= 35000 & combined_annualwage < 40000 ~ 10,
                           combined_annualwage >= 40000 & combined_annualwage < 50000 ~ 11,
                           combined_annualwage >= 50000 & combined_annualwage < 75000 ~ 12,
                           combined_annualwage >= 75000 & combined_annualwage < 100000 ~ 13,
                           combined_annualwage >= 100000 & combined_annualwage < 150000 ~ 14, 
                           combined_annualwage >= 150000 ~ 15),
         threshold_intervalled = case_when(threshold200 < 1 ~ NA,
                                           threshold200 < 5000 ~ 1,
                                           threshold200 >= 5000 & threshold200 < 7500 ~ 2,
                                           threshold200 >= 7500 & threshold200 < 10000 ~ 3,
                                           threshold200 >= 10000 & threshold200 < 12500 ~ 4,
                                           threshold200 >= 12500 & threshold200 < 15000 ~ 5,
                                           threshold200 >= 15000 & threshold200 < 20000 ~ 6, 
                                           threshold200 >= 20000 & threshold200 < 25000 ~ 7,
                                           threshold200 >= 25000 & threshold200 < 30000 ~ 8,
                                           threshold200 >= 30000 & threshold200 < 35000 ~ 9,
                                           threshold200 >= 35000 & threshold200 < 40000 ~ 10,
                                           threshold200 >= 40000 & threshold200 < 50000 ~ 11,
                                           threshold200 >= 50000 & threshold200 < 75000 ~ 12,
                                           threshold200 >= 75000 & threshold200 < 100000 ~ 13,
                                           threshold200 >= 100000 & threshold200 < 150000 ~ 14, 
                                           threshold200 >= 150000 ~ 15),
         low_income = case_when(combined_annualwage < threshold200 ~ 1,
                                ))
           
  
interval_inc <- family_data %>% 
  mutate(low_income2 = if_else(faminc < threshold_intervalled, 1, 0)) %>% 
  filter(low_income == 0 & low_income2 == 1, faminc > 0, threshold_intervalled > faminc & faminc != hhinc, dup == FALSE) %>%
  select(year, hhid, ownchild, low_income, low_income2, part_time, threshold200, combined_annualwage, faminc, hhinc, threshold_intervalled)


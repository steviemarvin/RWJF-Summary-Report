# RWJF Summary Report Background Section:
## Labor Market Outcomes and Poverty Statistics by Race and Ethnicity
## Using CPS microdata and framed by BLS Employment Characteristics for Families

# Definitions:
## Children: own children under age 18 that live in the household. Included are biological, step-, and adopted children of the husband, wife, or person maintaining the family. 
## Children cont.: Not incl. nieces, nephews, grandchildren, other related children, and unrelated children living in hh. 

library(tidyverse)
library(epiextractr)
library(stats)
library(haven)
library(epidatatools)
library(here)

source(here("functions5.R"), echo = TRUE)

raw_data <- load_basic(2007:2022, year, month, hhid, age, agechild, female, married, ownchild, basicwgt, famtype,
                       wage, wageotc, hoursu1i, emp, unemp, faminc, citistat, nilf, famrel, 
                       raceorig, hispanic)

# cleaning 
basic_data <- raw_data %>% 
  filter(agechild > 0, !is.na(married), !is.na(emp), age >= 16, !is.na(nilf)) %>% 
  mutate(wgt = basicwgt / 12, across(female | raceorig | hispanic | married, ~as.character(as_factor(.x))),
         full_time = if_else(hoursu1i >= 35, 1, 0),
         part_time = if_else(full_time == 0, 1, 0),
         full_time = labelled(full_time, c("Works full-time" = 1, "Works part-time" = 0)),
         part_time = labelled(part_time, c("Works part-time" = 1, "Works full-time" = 0)))


#### table 5a u18 = employment characteristics for women who have own children under 18 ####
t5a18Asian <- female18_stats_fun(r = "Asian only")
t5a18AIAN <- female18_stats_fun(r = "American Indian/Alaskan Native only")
t5a18Black <- female18_stats_fun(r = "Black")
t5a18White <- female18_stats_fun(r = "White")

t5a18Asian_Married <- female18_married_fun(r = "Asian only", m = "Married")
t5a18AIAN_Married <- female18_married_fun(r = "American Indian/Alaskan Native only", m = "Married")
t5a18Black_Married <- female18_married_fun(r = "Black", m = "Married")
t5a18White_Married <- female18_married_fun(r = "White", m = "Married")

t5a18Asian_NMarried <- female18_married_fun(r = "Asian only", m = "Not married")
t5a18AIAN_NMarried <- female18_married_fun(r = "American Indian/Alaskan Native only", m = "Not married")
t5a18Black_NMarried <- female18_married_fun(r = "Black", m = "Not married")
t5a18White_NMarried <- female18_married_fun(r = "White", m = "Not married")


#### table 5a u6 = employment characteristics for women who have own children under 6 ####

t5a6Asian <- female6_stats_fun(r = "Asian only")
t5a6AIAN <- female6_stats_fun(r = "American Indian/Alaskan Native only")
t5a6Black <- female6_stats_fun(r = "Black")
t5a6White <- female6_stats_fun(r = "White")

t5a6Asian_Married <- female6_married_fun(r = "Asian only", m = "Married")
t5a6AIAN_Married <- female6_married_fun(r = "American Indian/Alaskan Native only", m = "Married")
t5a6Black_Married <- female6_married_fun(r = "Black", m = "Married")
t5a6White_Married <- female6_married_fun(r = "White", m = "Married")

t5a6Asian_NMarried <- female6_married_fun(r = "Asian only", m = "Not married")
t5a6AIAN_NMarried <- female6_married_fun(r = "American Indian/Alaskan Native only", m = "Not married")
t5a6Black_NMarried <- female6_married_fun(r = "Black", m = "Not married")
t5a6White_NMarried <- female6_married_fun(r = "White", m = "Not married")


## table 5b u18 = employment characteristics for men who have own children under 18 ####

t5b18Asian <- male18_stats_fun(r = "Asian only")
t5b18AIAN <- male18_stats_fun(r = "American Indian/Alaskan Native only")
t5b18Black <- male18_stats_fun(r = "Black")
t5b18White <- male18_stats_fun(r = "White")

t5b18Asian_Married <- male18_married_fun(r = "Asian only", m = "Married")
t5b18AIAN_Married <- male18_married_fun(r = "American Indian/Alaskan Native only", m = "Married")
t5b18Black_Married <- male18_married_fun(r = "Black", m = "Married")
t5b18White_Married <- male18_married_fun(r = "White", m = "Married")

t5b18Asian_NMarried <- male18_married_fun(r = "Asian only", m = "Not married")
t5b18AIAN_NMarried <- male18_married_fun(r = "American Indian/Alaskan Native only", m = "Not married")
t5b18Black_NMarried <- male18_married_fun(r = "Black", m = "Not married")
t5b18White_NMarried <- male18_married_fun(r = "White", m = "Not married")


#### table 5b u6 = employment characteristics for men who have own children under 6 ####

t5b6Asian <- male6_stats_fun(r = "Asian only")
t5b6AIAN <- male6_stats_fun(r = "American Indian/Alaskan Native only")
t5b6Black <- male6_stats_fun(r = "Black")
t5b6White <- male6_stats_fun(r = "White")

 
t5b6Asian_Married <- male6_married_fun(r = "Asian only", m = "Married")
t5b6AIAN_Married <- male6_married_fun(r = "American Indian/Alaskan Native only", m = "Married")
t5b6Black_Married <- male6_married_fun(r = "Black", m = "Married")
t5b6White_Married <- male6_married_fun(r = "White", m = "Married")

t5b6Asian_NMarried <- male6_married_fun(r = "Asian only", m = "Not married")
t5b6AIAN_NMarried <- male6_married_fun(r = "American Indian/Alaskan Native only", m = "Not married")
t5b6Black_NMarried <- male6_married_fun(r = "Black", m = "Not married")
t5b6White_NMarried <- male6_married_fun(r = "White", m = "Not married")


#### Hispanic men and women stats ####
## Hispanic men and women with children under 18

t5a18Hispan <- hispan18_stats_fun(f = "Female")
t5b18Hispan <- hispan18_stats_fun(f = "Male")

## Hispanic men and women, married, with children under 18

t5a18Hispan_Married <- hispan18_married_fun(f = "Female", m = "Married")
t5b18Hispan_Married <- hispan18_married_fun(f = "Male", m = "Married")

t5a18Hispan_NMarried <- hispan18_married_fun(f = "Female", m = "Not married")
t5b18Hispan_NMarried <- hispan18_married_fun(f = "Male", m = "Not married")

## Hispanic men and women with children under 6

t5a6Hispan <- hispan6_stats_fun(f = "Female")
t5b6Hispan <- hispan6_stats_fun(f = "Male")

t5a6Hispan_Married <- hispan6_married_fun(f = "Female", m = "Married")
t5b6Hispan_Married <- hispan6_married_fun(f = "Male", m = "Married")

t5a6Hispan_NMarried <- hispan6_married_fun(f = "Female", m = "Not married")
t5b6Hispan_NMarried <- hispan6_married_fun(f = "Male", m = "Not married")


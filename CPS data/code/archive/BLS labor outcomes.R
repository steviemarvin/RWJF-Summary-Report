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

# replicate bls data use basic
raw_data <- load_basic (2007:2022, year, age, agechild, female, married, ownchild, basicwgt, famtype,
                        wage, emp, unemp, faminc, citistat, citizen, nilf, famrel, raceorig, hispanic)

# standard cleaning - omit selfemp == 0, selfinc == 0, age >= 16 for now
basic_data <- raw_data %>% 
  filter(agechild > 0, !is.na(married), !is.na(emp), age >= 16, !is.na(nilf)) %>% 
  mutate(wgt = basicwgt / 12, across(female | raceorig | hispanic, ~as.character(as_factor(.x))))


#### //// TABLE 5 WOMEN WITH OWN CHILDREN U18 ///// #####

## table 5a = employment characteristics for women who have own children under 18
table5a18White <- basic_data %>% 
  filter(female == "Female", raceorig == "White") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a18White <- select(table5a18White, year, civpop, lfpr, urate, epop)

table5a18Black <- basic_data %>% 
  filter(female == "Female", raceorig == "Black") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a18Black <- select(table5a18Black, year, civpop, lfpr, urate, epop)

table5a18AIAN <- basic_data %>% 
  filter(female == "Female", raceorig == "American Indian/Alaskan Native only") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a18AIAN <- select(table5a18AIAN, year, civpop, lfpr, urate, epop)

table5a18Asian <- basic_data %>% 
  filter(female == "Female", raceorig == "Asian only") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a18Asian <- select(table5a18Asian, year, civpop, lfpr, urate, epop)


table5a18AAPI <- basic_data %>% 
  filter(female == "Female", raceorig == "Asian/Pacific Islander") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a18AAPI <- select(table5a18AAPI, year, civpop, lfpr, urate, epop)

table5a18Hispan <- basic_data %>% 
  filter(female == "Female", hispanic == "Hispanic") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a18Hispan <- select(table5a18Hispan, year, civpop, lfpr, urate, epop)

### /// table 5 WOMEN MARRIED /// ###

table5a18WhiteM <- basic_data %>% 
  filter(female == "Female", raceorig == "White", married == "Married") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a18WhiteM <- select(table5a18WhiteM, year, civpop, lfpr, urate, epop)

### /// table 5 WOMEN NOT MARRIED /// ###
table5a18WhiteNM <- basic_data %>% 
  filter(female == "Female", raceorig == "White", married == "Married") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a18WhiteNM <- select(table5a18WhiteNM, year, civpop, lfpr, urate, epop)


#### //// TABLE 5 WOMEN WITH OWN CHILDREN U6 ///// #####
## table 5a (2) = employment characteristics for women who have own children under 6
table5a6White <- basic_data %>% 
  filter(female == "Female", agechild == 5, raceorig == "White") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 

female_stats_fun <- function(x) {
  basic_data %>% 
    filter(female == "Female", raceorig == x) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop) 
}

df <- female_stats_fun(x = "White")

table5a6White <- select(table5a6White, year, civpop, lfpr, urate, epop)

table5a6Asian <- basic_data %>% 
  filter(female == "Female", agechild == 5, raceorig == "Asian only") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5a6Asian <- select(table5a6Asian, year, civpop, lfpr, urate, epop)

#### //// TABLE 5 MEN WITH OWN CHILDREN U18 ///// #####

## table 5b = employment characteristics for MEN who have own children under 18
test <- basic_data %>% 
  filter(age > 20, female == "Male") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 

table5b18White <- basic_data %>% 
  filter(female == "Male", raceorig == "White") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5b18White <- select(table5b18White, year, civpop, lfpr, urate, epop)

table5b18Black <- basic_data %>% 
  filter(female == "Male", raceorig == "Black") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5b18Black <- select(table5b18Black, year, civpop, lfpr, urate, epop)

table5b18AIAN <- basic_data %>% 
  filter(female == "Male", raceorig == "American Indian/Alaskan Native only") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5b18AIAN <- select(table5b18AIAN, year, civpop, lfpr, urate, epop)

table5b18Asian <- basic_data %>% 
  filter(female == "Male", raceorig == "Asian only") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5b18Asian <- select(table5b18Asian, year, civpop, lfpr, urate, epop)


table5b18AAPI <- basic_data %>% 
  filter(female == "Male", raceorig == "Asian/Pacific Islander") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5b18AAPI <- select(table5b18AAPI, year, civpop, lfpr, urate, epop)

table5b18Hispan <- basic_data %>% 
  filter(female == "Male", hispanic == "Hispanic") %>% 
  group_by(year) %>% 
  summarize(civpop = sum(wgt, na.rm = TRUE),
            nilf_agg = sum(nilf * wgt),
            laborforce = civpop - nilf_agg,
            lfpr = laborforce / civpop,
            emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
            emp = sum(emp * wgt, na.rm = TRUE),
            unemp = sum(unemp * wgt, na.rm = TRUE),
            urate = unemp / laborforce,
            epop = emp / civpop) 
table5b18Hispan <- select(table5b18Hispan, year, civpop, lfpr, urate, epop)

#### //// TABLE 5 MEN WITH OWN CHILDREN U6 ///// #####



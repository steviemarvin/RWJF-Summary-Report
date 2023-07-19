library(ipumsr)
library(here)
library(tidyverse)
library(epiextractr)
library(stats)
library(epidatatools)
library(data.table)
library(readxl)
library(haven)


ddi <-  read_ipums_ddi(here("suppdata/cps_00005.xml"))
asec_raw_data <- read_ipums_micro(ddi, verbose = FALSE)
names(asec_raw_data) <- tolower(names(asec_raw_data))

source(here("functionsMarch.R"), echo = TRUE)

## CPI DATA for inflation adjustment for median/av income
cpi_data = read_excel("/projects/smarvin/data_library_annualwages/data_library_annualwages/r-cpi-u-rs-allitems.xlsx", 
                      skip = 5) %>% 
  rename(year = YEAR, cpiurs = AVG) %>% 
  select(year, cpiurs) %>% 
  filter(!is.na(cpiurs))

# grabbing base value year 2022
cpi_base_value =  cpi_data %>% 
  filter(year == 2021) %>% 
  pull(cpiurs)

# cleaning data from raw, only want primary families that have children u18
asec_clean_data <- asec_raw_data %>% 
  filter(year >= 2007 & year <= 2022, nchild != 0,
         relate != 1001 & relate != 1115 & relate != 1241 & relate != 1260, 
         age >= 16, labforce != 0) %>% 
  # replace placeholder numbers with NAs
  mutate(survey_year = year,
         year = year - 1,
         eldch = na_if(eldch, 99),
         yngch = na_if(yngch, 99),
         inctot = na_if(inctot, 999999999),
         incwage = na_if(incwage, 99999999),
         uhrsworkly = na_if(uhrsworkly, 999),
         wkstat = na_if(wkstat, 99),
         uhrsworkt = na_if(uhrsworkt, 997),
         uhrsworkt = na_if(uhrsworkt, 999),
         uhrswork1 = na_if(uhrswork1, 999),
         ahrsworkt = na_if(ahrsworkt, 999),
         hourwage = na_if(hourwage, 999.99)) %>% 
  #create binary variables for family with children u18 and u6
  mutate(u18 = case_when(eldch < 18 ~ 1,
                         yngch < 18 ~ 1,
                         yngch >= 18 ~ 0),
         u6 = if_else(yngch < 6 | eldch < 6, 1, 0),
         # new race variable to have no combo
         new_race = case_when(race == 100 ~ 1,
                              race == 200 ~ 2,
                              race == 300 ~ 3,
                              race >= 651 & race <= 652 ~ 4),
         new_race = labelled(new_race, c("White" = 1, "Black" = 2, "AIAN" = 3, "AAPI" = 4)),
         # race and ethnicity variable so that race and ethnicity are mutually exclusive
         new_race_hispan = case_when(race == 100 & hispan == 0 ~ 1,
                                     race == 200 & hispan == 0 ~ 2,
                                     race == 300 & hispan == 0 ~ 3,
                                     race >= 651 & race <= 652 & hispan == 0 ~ 4,
                                     hispan >= 1 & hispan <= 612 ~ 5),
         new_race_hispan = labelled(new_race_hispan, c("White not hispanic" = 1, "Black not hispanic" = 2, 
                                                       "AIAN not hispanic" = 3, "AAPI not hispanic" = 4,
                                                       "Hispanic" = 5)),
         wgt = asecwth / 12, across(poverty | sex | new_race | new_race_hispan, ~as.character(as_factor(.x)))) %>%
  # second filter for under 18, and primary family only
  filter(u18 == 1, !is.na(new_race) & !is.na(new_race_hispan), famrel != 0) %>%
  #low income and labor force metrics creation
  mutate(lowincome = ifelse(ftotval <= 2 * cutoff, 1, 0),
         unemp = ifelse(empstat >= 20 & empstat <= 22, 1, 0),
         nilf = ifelse(labforce == 1, 1, 0),
         emp = ifelse(empstat > 0 & empstat < 20, 1, 0),
         ft = ifelse(uhrsworkly >= 35, 1, 0),
         pt = ifelse(uhrsworkly < 35, 1, 0))

# collapsing multiple entries per family to single entry per family
asec_family_data <- asec_clean_data %>% 
  # creating indicator variables for race/ethnicity for collapsing + calculating shares
  mutate(is_white_nh = ifelse(new_race_hispan == "White not hispanic", 1, 0), 
         is_black_nh = ifelse(new_race_hispan == "Black not hispanic", 1, 0),
         is_AIAN_nh = ifelse(new_race_hispan == "AIAN not hispanic", 1, 0),
         is_AAPI_nh = ifelse(new_race_hispan == "AAPI not hispanic", 1, 0),
         is_hispanic = ifelse(new_race_hispan == "Hispanic", 1, 0),
         #is_citizen = ifelse(citizen <= 4, 1, 0),
         #not_citizen = ifelse(citizen == 5, 1, 0),
         foreign = ifelse(nativity == 5 & nativity != 0, 1, 0),
         mar_sp_pres = case_when(marst == 1 ~ 1),
         mar_sp_abs = case_when()) %>% 
  #collapsing 
  group_by(year, month, serial, famid) %>% 
  summarize(
    wgt = min(wgt),
    fam_lowincome = min(lowincome),
    fam_income = min(ftotval),
    fam_white = min(is_white_nh),
    fam_black = min(is_black_nh),
    fam_AIAN = min(is_AIAN_nh),
    fam_AAPI = min(is_AAPI_nh),
    fam_hispan = min(is_hispanic),
    fam_foreign = min(foreign),
    fam_all_foreign = sum(foreign, na.rm = TRUE),
    fam_unemp = min(unemp),
    fam_emp = min(emp),
    fam_all_emp = sum(emp, na.rm = TRUE),
    fam_nilf = min(nilf),
    fam_ft = min(ft),
    fam_pt = min(pt),
    fam_all_ft = sum(ft, na.rm = TRUE),
    fam_all_pt = sum(pt, na.rm = TRUE),
    u6 = min(u6),
    u18 = min(u18)) %>% 
  ungroup() %>% 
  inner_join(cpi_data, by = "year") %>%
  mutate(real_fam_income = fam_income * cpi_base_value / cpiurs)

# race/ethnicity and nativity breakdown of families with children u18 & u6
all_families_u18 <- all_families_fun(u18)
all_families_u6 <- all_families_fun(u6)


# race/ethnicity and nativity breakdown of low-income families with children u18 & u6
all_families_li_u18 <- all_families_li_fun(u18)
all_families_li_u6 <- all_families_li_fun(u6)

# labor force statistics for families by race/ethnicity
family_data_black_u18 <- family_stats_fun(fam_black, u18)
family_data_white_u18 <- family_stats_fun(fam_white, u18)
family_data_AIAN_u18 <- family_stats_fun(fam_AIAN, u18)
family_data_AAPI_u18 <- family_stats_fun(fam_AAPI, u18)
family_data_hispan_u18 <- family_stats_fun(fam_hispan, u18)

family_data_black_u6 <- family_stats_fun(fam_black, u6)
family_data_white_u6 <- family_stats_fun(fam_white, u6)
family_data_AIAN_u6 <- family_stats_fun(fam_AIAN, u6)
family_data_AAPI_u6 <- family_stats_fun(fam_AAPI, u6)
family_data_hispan_u6 <- family_stats_fun(fam_hispan, u6)

# labor force statistics for low income families by race/ethnicity
family_data_li_black_u18 <- family_li_stats_fun(fam_black, u18)
family_data_li_white_u18 <- family_li_stats_fun(fam_white, u18)
family_data_li_AIAN_u18 <- family_li_stats_fun(fam_AIAN, u18)
family_data_li_AAPI_u18 <- family_li_stats_fun(fam_AAPI, u18)
family_data_li_hispan_u18 <- family_li_stats_fun(fam_hispan, u18)

family_data_li_black_u6 <- family_li_stats_fun(fam_black, u6)
family_data_li_white_u6 <- family_li_stats_fun(fam_white, u6)
family_data_li_AIAN_u6 <- family_li_stats_fun(fam_AIAN, u6)
family_data_li_AAPI_u6 <- family_li_stats_fun(fam_AAPI, u6)
family_data_li_hispan_u6 <- family_li_stats_fun(fam_hispan, u6)


testAAPI <- asec_family_data %>% 
  filter(fam_AAPI == 1) %>% 
  group_by(year) %>% 
  arrange(fam_income, by_group = TRUE) %>% 
  summarize(median_famincome = median(fam_income),
            mean_famincome = weighted.mean(fam_income, w = wgt),
            mean_famincomeuw = mean(fam_income))

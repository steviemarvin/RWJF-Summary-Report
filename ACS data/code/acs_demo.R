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
ddi <-  read_ipums_ddi(here("data/usa_00016.xml"))
acs_raw <- read_ipums_micro(ddi, verbose = FALSE)
names(acs_raw) <- tolower(names(acs_raw))

source(here("code/01_acs_cleaning.R"), echo = TRUE)
source(here("code/02_acs_universe.R"), echo = TRUE)


# sample size stuff
acs_demo <- acs_clean %>% 
  filter(u18 == 1 & (poverty < 200 & poverty != 0)) %>% 
  epidatatools::crosstab(wbhaa, related)

asec_demo <- asec_clean %>% 
  filter(u18 == 1 & lowincome == 1, year == 2022) %>% 
  epidatatools::crosstab(wbhaa, relate)

# baseline dfs for acs demographics
acs_li_hh_type <- acs_li_families %>% 
  group_by(hh_type) %>% 
  summarize(n = n(),
            w = sum(hhwgt)) %>% 
  pivot_wider(values_from = c("n", "w"), names_from = "hh_type", names_sep = "_")

acs_demo_2022 <- acs_clean %>% 
  inner_join(acs_li_families, by = c("year", "serial")) %>% 
  filter(hh_type2 == "hhYes")

# df with wbhaa of household heads
acs_hh_head_wbhaa <- acs_demo_2022 %>% 
  filter(related == 101) %>% 
  select(serial, wbhaa) %>% 
  rename(wbhaa_hh = wbhaa)

# households by race and ethnicity of household head
acs_li_hh_wbhaa <- acs_demo_2022 %>% 
  filter(related == 101) %>% 
  group_by(year, wbhaa) %>% 
  summarize(n=n(),
            w_n = round(sum(hhwgt), 0)) %>% 
  pivot_wider(id_cols = year, names_from = "wbhaa", values_from = c("n", "w_n"))

# households by nativity status of household head
acs_demo_2022 <- mutate(acs_demo_2022, foreign_born = case_when(bpl >= 1 & bpl <= 99 ~ 0,
                                                                bpl > 99 & bpl <= 800 ~ 1,
                                                                TRUE ~ NA))

acs_li_foreignborn <- acs_demo_2022 %>% 
  filter(related == 101) %>% 
  summarize(foreign_born_s = sum(foreign_born * hhwgt, na.rm = TRUE),
            foreign_born_p = weighted.mean(foreign_born, w = hhwgt, na.rm = TRUE))

acs_li_foreignbornXwbhaa <- acs_demo_2022 %>% 
  filter(related == 101) %>% 
  group_by(wbhaa) %>% 
  summarize(foreign_born_s = sum(foreign_born * hhwgt, na.rm = TRUE),
            foreign_born_p = weighted.mean(foreign_born, w = hhwgt, na.rm = TRUE))

# households by number of children in the household
acs_demo_2022 <- mutate(acs_demo_2022, 
                        childu18 = if_else(age < 18, 1, 0),
                        childu6 = if_else(age < 6, 1, 0))

acs_li_hh_child <- acs_demo_2022 %>% 
  group_by(serial) %>% 
  summarize(across(childu18 | childu6, ~ sum(.x)),
            hhwgt = max(hhwt)) %>% ungroup() %>% 
  mutate(num_childu18 = case_when(childu18 == 1 ~ 1,
                                  childu18 == 2 ~ 2,
                                  childu18 == 3 ~ 3,
                                  childu18 > 3 ~ 4),
         num_childu6 = case_when(childu6 == 1 ~ 1,
                                 childu6 == 2 ~ 2,
                                 childu6 == 3 ~ 3,
                                 childu6 > 3 ~ 4,
                                 TRUE ~ 0)) %>% 
  dummy_cols(select_columns = c('num_childu18', 'num_childu6'), ignore_na = TRUE) %>% 
  summarize(across(contains("num_childu18_") | contains("num_childu6_"), ~round(sum(.x * hhwgt), 0)))

acs_li_hh_childXwbhaa <- acs_demo_2022 %>% 
  group_by(serial) %>% 
  summarize(across(childu18 | childu6, ~ sum(.x)),
            hhwgt = max(hhwt)) %>% ungroup() %>% 
  mutate(num_childu18 = case_when(childu18 == 1 ~ 1,
                                  childu18 == 2 ~ 2,
                                  childu18 == 3 ~ 3,
                                  childu18 > 3 ~ 4),
         num_childu6 = case_when(childu6 == 1 ~ 1,
                                 childu6 == 2 ~ 2,
                                 childu6 == 3 ~ 3,
                                 childu6 > 3 ~ 4,
                                 TRUE ~ 0)) %>% 
  dummy_cols(select_columns = c('num_childu18', 'num_childu6'), ignore_na = TRUE) %>% 
  left_join(acs_hh_head_wbhaa, by = "serial") %>% 
  group_by(wbhaa_hh) %>% 
  summarize(across(contains("num_childu18_") | contains("num_childu6_"), ~sum(.x * hhwgt)))

# household by marital status of household head
acs_marst_hhead <- acs_demo_2022 %>% 
  mutate(married_parent = if_else((marst == 1 | marst == 2) & u18 == 1, 1, 0)) %>% 
  filter(u18 == 1 & related == 101) %>% 
  group_by(wbhaa, married_parent) %>% 
  summarize(uw = n(),
            w = sum(hhwgt)) %>% 
  pivot_wider(id_cols = wbhaa, names_from = married_parent, values_from = c("uw", "w"))

# LGB status of household head <3333
acs_LGB_hh <- acs_demo_2022 %>% 
  filter(related == 101 | related == 201 | related == 1114) %>% 
  mutate(related = as.character(as_factor(related))) %>% 
  group_by(serial, related, sex) %>% 
  summarize(n = n()) %>% ungroup() %>% 
  pivot_wider(id_cols = serial, names_from = related, values_from = c("n", "sex")) %>% 
  rename_with(tolower) %>% 
  rename_with(~gsub("head/householder", "hh", .), contains("head/householder")) %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  mutate(LGB = case_when(!is.na(n_spouse) & sex_spouse == sex_hh ~ 1,
                         !is.na(n_unmarried_partner) & sex_unmarried_partner == sex_hh ~ 1, 
                         TRUE ~ 0)) %>% 
  filter(LGB == 1) %>% select(serial, LGB) %>% 
  left_join(acs_demo_2022, by = "serial", multiple = "all") %>% 
  filter(related == 101 & u18 == 1) %>% 
  group_by(wbhaa) %>% 
  summarize(n = n(),
            w = sum(hhwgt))

# age of household head
acs_parents_age <- acs_demo_2022 %>% 
  filter(u18 == 1 & related == 101) %>% 
  group_by(wbhaa, sex) %>% 
  summarize(mean = round(weighted.mean(age, w = perwt), 1), 
            median = median(age)) %>% 
  pivot_wider(id_cols = wbhaa, names_from = sex, values_from = c("mean", "median"))

acs_parents_genderXwbhaa <- acs_demo_2022 %>% 
  filter(u18 == 1 & related == 101) %>% 
  group_by(wbhaa, sex) %>%
  summarize(sum = sum(perwt)) %>% 
  pivot_wider(id_cols = wbhaa, names_from = sex, values_from = c("sum")) %>% 
  mutate(sum = rowSums(across(where(is.numeric)), na.rm=TRUE),
         across(Female | Male, ~round(.x/sum, 3))) %>% 
  filter(!is.na(wbhaa))

acs_parents_gender <- acs_demo_2022 %>% 
  filter(u18 == 1 & related == 101) %>% 
  group_by(sex) %>% 
  summarize(sum = sum(perwt)) 

# disability in household
acs_disability <- acs_demo_2022 %>% 
  mutate(across(contains("diff"), ~case_when(.x == 0 ~ NA,
                                             .x == 1 ~ 0,
                                             .x == 2 ~ 1)),
         diffany_adult = case_when((diffrem == 1 | diffphys == 1 | diffmob == 1 |
                                     diffcare == 1 | diffsens == 1 | diffeye == 1 |
                                     diffhear == 1) ~ 1,
                                   (diffrem == 0 & diffphys == 0 & diffmob == 0 &
                                     diffcare == 0 & diffsens == 0 & diffeye == 0 & 
                                     diffhear == 0) ~ 0,
                                   TRUE ~ NA),
         diffany_child = case_when((diffrem == 1 | diffphys == 1 | diffmob == 1 |
                                     diffcare == 1 | diffsens == 1 | diffeye == 1 |
                                     diffhear == 1) & age < 18 ~ 1,
                                   ((diffrem == 0 | is.na(diffrem)) & (diffphys == 0 | is.na(diffphys)) & (diffmob == 0 | is.na(diffmob)) &
                                     (diffcare == 0 | is.na(diffcare)) & diffsens == 0 & diffeye == 0 & 
                                     diffhear == 0) & age < 18 ~ 0,
                                   TRUE ~ NA),
         diff_hh = case_when(diffany_adult == 1 & related == 101 ~ 1,
                             diffany_adult == 0 & related == 101 ~ 0,
                             TRUE ~ NA),
         diff_parent = case_when(diffany_adult == 1 & (related == 101 | related == 201 | related == 1114) ~ 1,
                                 diffany_adult == 0 & (related == 101 | related == 201 | related == 1114) ~ 0,
                                 TRUE ~ NA),
         diff_child = case_when(diffany_child == 1 ~ 1,
                                diffany_child == 0 ~ 0,
                                TRUE ~ NA),
         diff_hhmember = case_when(diffany_adult == 1 & (related != 101 & related != 201 & related != 1114) & age >= 18 ~ 1,
                                   diffany_adult == 0 & (related != 101 & related != 201 & related != 1114) & age >= 18 ~ 0,
                                   TRUE ~ -1),
         diff_childorparent = case_when(diff_parent == 1 ~ 1,
                                        diff_child == 1 ~ 1,
                                        TRUE ~ 0)) %>% 
  group_by(serial) %>% 
  summarize(across(diff_hh | diff_parent | diff_child | diff_hhmember | diff_childorparent, ~max(.x, na.rm = TRUE)),
            hhwgt = max(hhwgt)) %>% 
  mutate(diff_hhmember = if_else(diff_hhmember == -1, NA, diff_hhmember)) %>% 
  left_join(acs_hh_head_wbhaa, by = "serial") %>% 
  group_by(wbhaa_hh) %>% 
  summarize(across(contains("diff"), ~weighted.mean(.x, w = hhwgt, na.rm = TRUE), .names = "share_{.col}"))


# intergenerational households
acs_intergen_hh <- acs_demo_2022 %>% 
  mutate(related = as.character(as_factor(related))) %>% 
  group_by(serial, related) %>% 
  summarize(n = n()) %>% 
  pivot_wider(id_cols = serial, names_from = related, values_from = "n") %>% ungroup() %>% 
  # reformatting column names
  rename_with(tolower) %>% 
  rename_with(~gsub("/", "_", .), everything()) %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  rename_with(~gsub("-", "_", .), everything()) %>% 
  # indicator variable for intergenerational households
  mutate(intergenerational = case_when((!is.na(parent) | !is.na(parent_in_law)) & !is.na(head_householder) & !is.na(child) ~ 1,
                                       !is.na(head_householder) & !is.na(child) & !is.na(grandchild) ~ 1,
                                       TRUE ~ 0))

# number of intergenerational households by race and ethnicity
acs_intergenXwbhaa_hh <- acs_intergen_hh %>% 
  filter(intergenerational == 1) %>% 
  left_join(acs_demo_2022, by = "serial", multiple = "all") %>% 
  filter(related == 101) %>% 
  group_by(wbhaa) %>% 
  summarize(n = n(),
            w = sum(hhwt))

acs_foster_hh <- acs_demo_2022 %>% 
  mutate(foster_child = if_else(related == 1242 & age < 18, 1, 0)) %>% 
  group_by(serial) %>% 
  summarize(foster_child = max(foster_child),
            hhwgt = max(hhwgt)) %>%
  left_join(acs_hh_head_wbhaa, by = "serial") %>% 
  group_by(wbhaa_hh) %>% 
  summarize(share_foster = weighted.mean(foster_child, w = hhwgt), 
            sum_foster = sum(foster_child))
  
  foster_hh <- demo_2022 %>% 
  filter(relate == 1242 & age < 18) %>% 
  group_by(year) %>% distinct(serial, .keep_all = FALSE) %>% 
  left_join(demo_2022, by = c("year", "serial"), multiple = "all") %>% 
  filter(relate == 101 & lowincome == 1) %>% select(year, serial) %>% 
  left_join(demo_2022, by = c("year", "serial"), multiple = "all")



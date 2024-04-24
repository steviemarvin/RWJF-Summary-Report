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
  inner_join(acs_li_families, by = c("year", "serial")) %>% 
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
  inner_join(acs_li_families, by = c("year", "serial")) %>% 
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

# disability in household
acs_disability <- acs_demo_2022 %>% 
  mutate(across(contains("diff"), ~ case_when(.x == 0 ~ NA,
                                              .x == 1 ~ 0,
                                              .x == 2 ~ 1))) %>% 
  select(year, serial, cbserial, age, contains("diff"))
         
    diffany = case_when(diffrem == 2 | diffphys == 2 | diffmob == 2 | 
                               diffcare == 2 | diffsens == 2 | diffeye == 2 |
                               diffhear == 2 ~ 1,
                             diffrem == 1 | diffphys == 1 | diffmob == 1 | 
                               diffcare == 1 | diffsens == 1 | diffeye == 1 |
                               diffhear == 1 ~ 0,
                             is.na(diffrem) | is.na(diffphys) | is.na(diffmob) |
                               is.na(diffcare) | is.na(diffsens) | is.na(diffeye) | 
                               is.na(diffhear) ~ NA),
         diff_parent = if_else(diffany == 1 & u18 == 1 & (relate == 101 | relate == 201 | relate == 1114), 1, 0),
         diff_other_adult = if_else(diffany == 1 & (relate != 101 & relate != 201 & relate != 1114), 1, 0),
         diff_child = if_else(diffany == 1 & age < 18, 1, 0)) 

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
  mutate(intergenerational = case_when(!is.na(parent) & !is.na(head_householder) & !is.na(child) ~ 1,
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


# demographics

#### SETTING UP BASELINE DF ####
demo_2022 <- filter(asec_clean, year == 2022) %>% 
  select(year, serial, famid, adult, age, asecwt, asecwth, citizen, diffany, famrel, famsize, famunit, foreign_born, ftype, hh_head_sp, LGB, lowincome, married_parent,
         marst, nchild, pernum, pov100, pov50, relate, related_adult, sex, sploc, contains("wbhaa"), yngch, u18, u6) %>% 
  mutate(childu18 = if_else(age < 18, 1, 0),
         childu6 = if_else(age < 6, 1, 0)) %>% 
  inner_join(li_families, by = c("year", "serial")) %>% 
  filter(hh_type == "hhOnly" | hh_type == "hhXnr" | hh_type == "hhXr" | hh_type == "hhXnrXr")


#### TYPE OF LOW INCOME FAMILIES BY 7 HOUSEHOLD TYPES #### 
li_hh_type <- li_families %>% 
  group_by(year, hh_type) %>% 
  summarize(n = n(),
            wsum = sum(hhwgt)) %>% 
  pivot_wider(id_cols = year, values_from = c("n", "wsum"), names_from = "hh_type", names_sep = "_")


#### SHARE OF LOWINCOME FAMILIES BY WBHAA ####
# record of race/ethnicity of household head to see how many cases there are where hh head and parent (or other parent) have a wbhaa mismatch
hh_head_wbhaa <- demo_2022 %>% 
  filter(relate == 101) %>% 
  select(serial, wbhaa) %>% 
  rename(wbhaa_hh = wbhaa)

# df that shows whether or not there are parents of different races in the household. Excludes the race of spouse/unmarried partner because household head takes precedent
# the function of this df is mainly for determining how to impute the household if someone other than household head or spouse has a child
mixed_hh <- demo_2022 %>% 
  filter(u18 == 1, hh_head_sp == 1 | relate == 1114, hh_type == "hhOnly") %>% 
  group_by(serial, wbhaa) %>% 
  summarize(n = n()) %>% 
  pivot_wider(id_cols = c("serial"), names_from = "wbhaa", values_from = "n") %>% ungroup() %>% 
  mutate(mixed = case_when(!is.na(white) & (!is.na(black) | !is.na(hispanic) | !is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(black) & (!is.na(hispanic) | !is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(hispanic) & (!is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(AIAN) & !is.na(AAPI) ~ 1, 
                           TRUE ~ 0)) %>% 
  filter(mixed == 1) %>% 
  left_join(hh_head_wbhaa, by = "serial")

hh_head_white <- filter(mixed_hh, wbhaa_hh == "white") %>% 
  left_join(demo_2022, by = "serial", multiple = "all") %>% select(serial, relate, wbhaa) %>% 
  filter(relate == 1114 | relate == 201)
  
# df that looks at cases where the household head and a related member who has a child have different wbhaa
mixed_hh2 <- demo_2022 %>% 
  filter(u18 == 1, hh_type == "rOnly" | hh_type == "hhXr") %>% 
  group_by(serial, wbhaa) %>% 
  summarize(n = n()) %>% 
  pivot_wider(id_cols = c("serial"), names_from = "wbhaa", values_from = "n") %>% ungroup() %>% 
  mutate(mixed = case_when(!is.na(white) & (!is.na(black) | !is.na(hispanic) | !is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(black) & (!is.na(hispanic) | !is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(hispanic) & (!is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(AIAN) & !is.na(AAPI) ~ 1, 
                           TRUE ~ 0)) %>% 
  filter(mixed == 1) %>% 
  left_join(hh_head_wbhaa, by = "serial") %>% 
  left_join(filter(families_hh_type, year == 2022), by = "serial") %>% select(-year, -hhwgt)
  
# cases where the household head and the parent of a child do not have the same wbhaa classification (different from mixed households)
mismatch_wbhaa <- mixed_hh %>% 
  select(-mixed) %>% 
  mutate(mismatch = case_when(wbhaa_hh == "white" & is.na(white) ~ 1,
                              wbhaa_hh == "black" & is.na(black) ~ 1,
                              wbhaa_hh == "hispanic" & is.na(hispanic) ~ 1,
                              wbhaa_hh == "AIAN" & is.na(AIAN) ~ 1,
                              wbhaa_hh == "AAPI" & is.na(AAPI) ~ 1,
                              TRUE ~ 0)) %>% 
  filter(mismatch == 1) %>% 
  left_join(filter(families_df, year == 2022), by = "serial") %>% 
  select(-year) 

# df that shows raw and weighted sum of low income households with children by race and ethnicity. This df only looks at wbhaa of household head
li_hh_wbhaa <- demo_2022 %>% 
  filter(relate == 101) %>% 
  group_by(year, wbhaa) %>% 
  summarize(n=n(),
            w_n = round(sum(hhwgt), 0)) %>% 
  pivot_wider(id_cols = year, names_from = "wbhaa", values_from = c("n", "w_n"))


#### SHARE OF LOW INCOME FAMILIES THAT CAN BE CLASSIFIED AS NATIVE- OR FOREIGN-BORN ####
li_hh_foreignborn <- demo_2022 %>% 
  filter(relate == 101) %>% 
  summarize(foreign_born_s = sum(foreign_born * hhwgt, na.rm = TRUE),
            foreign_born_p = weighted.mean(foreign_born, w = hhwgt, na.rm = TRUE))

li_foreignbornXwbhaa <- demo_2022 %>% 
  filter(relate == 101) %>% 
  group_by(wbhaa) %>% 
  summarize(foreign_born_s = sum(foreign_born * hhwgt, na.rm = TRUE),
            foreign_born_p = weighted.mean(foreign_born, w = hhwgt, na.rm = TRUE))

#### NUMBER OF CHILDREN UNDER 18 (AND UNDER 6) IN THE HOUSEHOLD ####
li_hh_child <- demo_2022 %>% 
  inner_join(li_families, by = c("year", "serial")) %>% 
  group_by(serial) %>% 
  summarize(across(childu18 | childu6, ~ sum(.x)),
            hhwgt = max(asecwth)) %>% ungroup() %>% 
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

li_hh_childXwbhaa <- demo_2022 %>% 
  inner_join(li_families, by = c("year", "serial")) %>% 
  group_by(serial) %>% 
  summarize(across(childu18 | childu6, ~ sum(.x)),
            hhwgt = max(asecwth)) %>% ungroup() %>% 
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
  left_join(hh_head_wbhaa, by = "serial") %>% 
  group_by(wbhaa_hh) %>% 
  summarize(across(contains("num_childu18_") | contains("num_childu6_"), ~sum(.x * hhwgt)))

#### MARITAL STATUS AND LGB STATUS OF PARENTS OF CHILDREN UNDER 18 ####
## Marital status of parents (married or not) i.e. married families vs non married
marst_hhhead <- demo_2022 %>% 
  filter(u18 == 1 & relate == 101) %>% 
  group_by(wbhaa, married_parent) %>% 
  summarize(uw = n(),
            w = sum(hhwgt)) %>% 
  pivot_wider(id_cols = wbhaa, names_from = married_parent, values_from = c("uw", "w"))

# LGB status of household head (that has own child) by wbhaa
LGB_parents <- marst_LGB_hh %>%
  filter(LGB == 1) %>% select(-wbhaa) %>% 
  left_join(hh_head_wbhaa, by = "serial") %>% 
  group_by(wbhaa_hh) %>% 
  summarize(
    LGB_uw = sum(LGB),
    LGB = round(sum(hhwgt), 0)) %>% 
  filter(!is.na(wbhaa_hh))



#### AGE OF PARENTS ####
parents_age <- demo_2022 %>% 
  filter(u18 == 1, relate == 101) %>% 
  group_by(wbhaa, sex) %>% 
  summarize(mean = round(weighted.mean(age, w = asecwt), 1), 
            median = median(age)) %>% 
  pivot_wider(id_cols = wbhaa, names_from = sex, values_from = c("mean", "median"))


#### DISABILITY STATUS OF PARENT OR HOUSEHOLD MEMBER ####
li_hh_disabilityXwbhaa <- demo_2022 %>% 
  mutate(diff_parent = if_else((hh_head_sp == 1 | relate == 1114) & diffany == 2, 1, 0),
         diff_member = if_else((hh_head_sp == 0 & relate != 1114) & diffany == 2, 1, 0)) %>% 
  group_by(serial) %>%
  summarize(diff_parent = max(diff_parent),
            diff_member = max(diff_member),
            hhwgt = max(asecwth)) %>% ungroup() %>% 
  mutate(parentXmember = if_else(diff_parent > 0 & diff_member > 0, 1, 0),
         parentormember = if_else(diff_parent > 0 | diff_member > 0, 1, 0)) %>% 
  left_join(hh_head_wbhaa, by = "serial") %>% 
  group_by(wbhaa_hh) %>% 
  summarize(across(diff_parent | diff_member | parentXmember | parentormember, ~weighted.mean(.x, w = hhwgt), .names = "%.{.col}"),
            across(diff_parent | diff_member | parentXmember | parentormember, ~sum(.x * hhwgt), .names = "w.{col}"),
            across(diff_parent | diff_member | parentXmember | parentormember, ~sum(.x)))


#### INTERGENERATIONAL HOUSEHOLDS ####
# baseline df that classifies households as intergenerational or not, based on related members present in the household
intergen_hh <- demo_2022 %>% 
  mutate(relate = as.character(as_factor(relate))) %>% 
  group_by(serial, relate) %>% 
  summarize(n = n()) %>% 
  pivot_wider(id_cols = serial, names_from = relate, values_from = "n") %>% ungroup() %>% 
  # reformatting column names
  rename_with(~gsub(", n.s.", "_ns", .), contains("n.s.")) %>% 
  rename_with(~gsub("/", "_", .), everything()) %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  rename_with(tolower) %>% 
  # indicator variable for intergenerational households
  mutate(intergenerational = case_when(!is.na(parent) & !is.na(head_householder) & !is.na(child) ~ 1,
                                       !is.na(head_householder) & !is.na(child) & !is.na(grandchild) ~ 1,
                                       TRUE ~ 0))
# number of intergenerational households by race and ethnicity
intergenXwbhaa_hh <- intergen_hh %>% 
  filter(intergenerational == 1) %>% 
  left_join(demo_2022, by = "serial", multiple = "all") %>% 
  filter(relate == 101) %>% 
  group_by(wbhaa) %>% 
  summarize(n = n(),
            w = sum(asecwth))


#### HOUSEHOLDS WITH FOSTER CHILDREN ####
foster_hh <- demo_2022 %>% 
  filter(relate == 1242 & age < 18) %>% 
  group_by(year) %>% distinct(serial, .keep_all = FALSE) %>% 
  left_join(demo_2022, by = c("year", "serial"), multiple = "all") %>% 
  filter(relate == 101 & lowincome == 1) %>% select(year, serial) %>% 
  left_join(demo_2022, by = c("year", "serial"), multiple = "all")


#### SPM vs FPL ####
poverty_hh <- asec_clean %>%
  inner_join(families_hh_type, by = c("year", "serial"), multiple = "all") %>%
  filter(relate == 101, hh_type == "hhOnly" | hh_type == "hhXr" | hh_type == "hhXnr" | hh_type == "hhXnrXr") %>% 
  group_by(year, wbhaa) %>% 
  summarize(spmpov = round(weighted.mean(spmpov, w = asecwth, na.rm = TRUE), 4),
            fpl = round(weighted.mean(pov100, w = asecwth, na.rm = TRUE), 4),
            lowincome = round(weighted.mean(lowincome, w = asecwth, na.rm = TRUE), 4)) %>% 
  filter(!is.na(wbhaa)) %>% 
  pivot_wider(id_cols = year, names_from = wbhaa, values_from = c("spmpov", "fpl", "lowincome"))

# demographics
li_families_s <- li_families %>% 
  group_by(year) %>% 
  distinct(serial, .keep_all = TRUE) %>% ungroup() 


demo_2022 <- filter(asec_clean, year == 2022) %>% 
  select(year, serial, famid, adult, age, asecwth, citizen, diffany, famrel, famsize, famunit, foreign_born, ftype, hh_head_sp, LGB, lowincome, married_parent,
         marst, pov100, pov50, relate, related_adult, sploc, contains("wbhaa"), yngch, u18, u6) %>% 
  mutate(childu18 = if_else(age < 18, 1, 0),
         childu6 = if_else(age < 6, 1, 0)) %>% 
  inner_join(li_families_s, by = c("year", "serial"))

## Type of lowincome families by household structure
li_hh_type <- li_families_s %>% 
  group_by(year, hh_type) %>% 
  summarize(n = n(),
            wsum = sum(hhwgt)) %>% 
  pivot_wider(id_cols = "year", values_from = c("n", "wsum"), names_from = "hh_type", names_sep = "_")


## Share of lowincome families by WBHAA
# record of race/ethnicity of household head to see how many cases there are where hh head and parent (or other parent) have a wbhaa mismatch
hh_head_wbhaa <- demo_2022 %>% 
  filter(relate == 101) %>% 
  select(serial, relate, wbhaa)

# df that shows whether or not there are parents of different races in the household. Excludes the race of spouse/unmarried partner because household head takes precedent
# the function of this df is mainly for determining how to impute the household if someone other than household head or spouse has a child
mixed_hh <- demo_2022 %>% 
  inner_join(li_families, by = c("year", "serial"), multiple = "all") %>%
  filter(u18 == 1, relate != 1114 & relate != 201) %>% 
  group_by(serial, wbhaa) %>% 
  summarize(n = n(),
            hhwgt = max(asecwth)) %>% 
  pivot_wider(id_cols = c("serial"), names_from = "wbhaa", values_from = "n") %>% 
  left_join(filter(families_hh_type, year == 2022), by = "serial") %>% select(-year, -hhwgt) %>% 
  ungroup() %>% 
  mutate(mixed = case_when(!is.na(white) & (!is.na(black) | !is.na(hispanic) | !is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(black) & (!is.na(hispanic) | !is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(hispanic) & (!is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(AIAN) & !is.na(AAPI) ~ 1, 
                           TRUE ~ 0))

# df to impute race for mixed households 
mixed_hh_id <- mixed_hh %>% 
  filter(mixed == 1) %>% 
  # when hh_type = rOnly, almost every case has child (a parent) to hh head same race and hh head (other race person is the spouse to the child of hh head typically)
  mutate(wbhaa_imputed = case_when())
  
# cases where the household head and the parent of a child do not have the same wbhaa classification (different from mixed households)
mismatch_wbhaa <- mixed_hh %>% 
  inner_join(hh_head_wbhaa, by = "serial") %>% 
  select(-mixed, -relate) %>% 
  mutate(mismatch = case_when(wbhaa == "white" & is.na(white) ~ 1,
                              wbhaa == "black" & is.na(black) ~ 1,
                              wbhaa == "hispanic" & is.na(hispanic) ~ 1,
                              wbhaa == "AIAN" & is.na(AIAN) ~ 1,
                              wbhaa == "AAPI" & is.na(AAPI) ~ 1,
                              TRUE ~ 0)) %>% 
  filter(mismatch == 1) %>% 
  left_join(filter(families_df, year == 2022), by = "serial") %>% 
  select(-year)
  mutate(wbhaa_imputed = case_when(hh_type == "rOnly" & !is.na(Child) ~ 1))

# df that shows raw and weighted sum of low income households with children by race and ethnicity. This df only looks at wbhaa of household head
li_hh_wbhaa <- demo_2022 %>% 
  filter(relate == 101) %>% 
  group_by(wbhaa) %>% 
  summarize(n=n(),
            w_n = sum(hhwgt)) %>% 
  pivot_wider(names_from = "wbhaa", values_from = c("n", "w_n"))

## Share of low income families that can be classified as native- or foreign-born
li_hh_foreignborn <- demo_2022 %>% 
  filter(relate == 101) %>% 
  group_by(foreign_born) %>% 
  summarize(n=n(),
            w_n = sum(hhwgt))

## number of children under 18 (and under 6) in the household
li_hh_child <- demo_2022 %>% 
  inner_join(li_families_s, by = c("year", "serial")) %>% 
  group_by(serial) %>% 
  summarize(across(childu18 | childu6, ~ sum(.x)),
            hhwgt = max(asecwth)) %>% ungroup() %>% 
  summarize(childu18 = weighted.mean(childu18, w = hhwgt),
            childu6 = weighted.mean(childu6, w = hhwgt))

li_hh_childXwbhaa <- demo_2022 %>% 
  inner_join(li_families_s, by = c("year", "serial")) %>% 
  group_by(serial) %>% 
  summarize(across(childu18 | childu6, ~ sum(.x)),
            hhwgt = max(asecwth)) %>% ungroup() %>% 
  left_join(hh_head_wbhaa, by = "serial") %>% 
  group_by(wbhaa) %>% 
  summarize(childu18 = weighted.mean(childu18, w = hhwgt),
            childu6 = weighted.mean(childu6, w = hhwgt))
  

## Marital status of parents (married or not) i.e. married families vs non married
rOnly <- demo_2022 %>% 
  filter(hh_type == "rOnly", u18 == 1, relate != 1001) %>% 
  mutate(marst = as.character(as_factor(marst))) %>% 
  group_by(marst) %>% 
  summarize(n = n()) 


## LGBTQ+ parents
LGB_sp <- demo_2022 %>% 
  filter(u18 == 1 & LGB == 1)

## Parent is disabled/member of household/family is disabled
disabled_members <- demo_2022 %>%
  filter(diffany == 2) %>% 
  mutate(relate = as.character(as_factor(relate))) %>% 
  group_by(serial, relate) %>% 
  summarize(n = n()) %>% 
  pivot_wider(id_cols = serial, names_from = relate, values_from = "n") %>% 
  inner_join(filter(families_hh_type, year == 2022), by = "serial") %>% select(-year)
  

## blended families 

## intergenerational households
intergen_hh <- demo_2022 %>% 
  mutate(relate = as.character(as_factor(relate))) %>% 
  group_by(serial, relate) %>% 
  summarize(n = n()) %>% 
  pivot_wider(id_cols = serial, names_from = relate, values_from = "n") %>% ungroup() %>% 
  rename_with(~gsub(", n.s.", "_ns", .), contains("n.s.")) %>% 
  rename_with(~gsub("/", "_", .), everything()) %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  rename_with(tolower) %>% 
  mutate(intergenerational = case_when(!is.na(parent) & !is.na(head_householder) & !is.na(child) ~ 1,
                                       !is.na(head_householder) & !is.na(child) & !is.na(grandchild) ~ 1,
                                       TRUE ~ 0))

intergenXwbhaa_hh <- intergen_hh %>% 
  filter(intergenerational == 1) %>% 
  left_join(demo_2022, by = "serial", multiple = "all") %>% 
  filter(relate == 101) %>% 
  group_by(wbhaa) %>% 
  summarize(n = n(),
            w = sum(asecwth))
  
## co-habiting


## households with foster children
foster_hh <- demo_2022 %>% 
  filter(relate == 1242 & age < 18) %>% 
  group_by(year) %>% distinct(serial, .keep_all = FALSE) %>% 
  left_join(demo_2022, by = c("year", "serial"), multiple = "all") %>% 
  filter(relate == 101 & lowincome == 1) %>% select(year, serial) %>% 
  left_join(demo_2022, by = c("year", "serial"), multiple = "all")


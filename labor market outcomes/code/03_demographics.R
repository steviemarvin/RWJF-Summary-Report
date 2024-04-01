# demographics
demo_2022 <- filter(asec_clean, year == 2022) %>% 
  select(year, serial, famid, adult, age, asecwth, citizen, famrel, famsize, famunit, foreign_born, ftype, hh_head_sp, LGB, lowincome, married_parent,
         marst, pov100, pov50, relate, related_adult, contains("wbhaa"), yngch, u18, u6)

li_familes_s <- li_families %>% 
  group_by(year) %>% 
  distinct(serial, .keep_all = TRUE) %>% ungroup()

# Type of lowincome families by household structure
li_hh_type <- li_families %>% 
  group_by(year) %>% 
  distinct(serial, .keep_all = TRUE) %>% ungroup() %>% 
  group_by(year, hh_type) %>% 
  summarize(n = n(),
            wsum = sum(hhwgt)) %>% 
  pivot_wider(id_cols = "year", values_from = c("n", "wsum"), names_from = hh_type, names_sep = "_")

# Share of lowincome families by WBHAA

hh_head_wbhaa <- demo_2022 %>% 
  filter(relate == 101) %>% 
  select(serial, relate, wbhaa)

mixed_hh <- demo_2022 %>% 
  inner_join(li_families, by = c("year", "serial"), multiple = "all") %>%
  filter(u18 == 1, relate != 1114 & relate != 201) %>% 
  group_by(serial, wbhaa) %>% 
  summarize(n = n(),
            hhwgt = max(hhwgt)) %>% 
  pivot_wider(id_cols = c("serial"), names_from = "wbhaa", values_from = "n") %>% 
  left_join(filter(families_hh_type, year == 2022), by = "serial") %>% select(-year, -hhwgt) %>% 
  ungroup() %>% 
  mutate(mixed = case_when(!is.na(white) & (!is.na(black) | !is.na(hispanic) | !is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(black) & (!is.na(hispanic) | !is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(hispanic) & (!is.na(AIAN) | !is.na(AAPI)) ~ 1,
                           !is.na(AIAN) & !is.na(AAPI) ~ 1, 
                           TRUE ~ 0))

mismatch_wbhaa <- mixed_hh %>% 
  filter(mixed == 0) %>% 
  inner_join(test, by = "serial") %>% 
  select(-mixed, -relate) %>% 
  mutate(mismatch = case_when(wbhaa == "white" & is.na(white)~1,
                              wbhaa == "black" & is.na(black)~1,
                              wbhaa == "hispanic" & is.na(hispanic) ~ 1,
                              wbhaa == "AIAN" & is.na(AIAN) ~ 1,
                              wbhaa == "AAPI" & is.na(AAPI) ~ 1,
                              TRUE ~ 0)) %>% 
  filter(mismatch == 1)


li_hh_wbhaa <- demo_2022 %>% 
  inner_join(li_familes_s, by = c("year", "serial")) %>%
  
  


# Share of low income families that can be classified as native- or foreign-born

# number of children under 18 (and under 6) in the household

# Marital status of parents (married or not) i.e. married families vs non married

# LGBTQ+ parents

# Parent is disabled/member of household/family is disabled

# blended families 

# intergenerational households

# co-habiting



relateXhh_type <- demo_2022 %>% 
  filter(u18 == 1, relate != 201 & relate != 1114, !is.na(wbhaa)) %>% 
  inner_join(li_families, by = c("year", "serial"), multiple = "all") %>% 
  mutate(relate = as.character(as_factor(relate))) %>% 
  group_by(hh_type, relate) %>% 
  summarize(n=n()) %>% 
  pivot_wider(id_cols = "relate", names_from = "hh_type", values_from = "n")

test <- demo_2022 %>% 
  filter(u18 == 1, relate != 201 & relate != 1114, !is.na(wbhaa)) %>% 
  mutate(relate = as.character(as_factor(relate))) %>% 
  group_by(wbhaa, relate) %>% 
  summarize(n=n()) %>% 
  pivot_wider(id_cols = "wbhaa", names_from = "relate", values_from = "n")

# households with foster children
foster_hh <- demo_2022 %>% 
  filter(relate == 1242 & age < 18) %>% 
  group_by(year) %>% distinct(serial, .keep_all = FALSE) %>% 
  left_join(demo_2022, by = c("year", "serial"), multiple = "all") %>% 
  filter(relate == 101 & lowincome == 1) %>% select(year, serial) %>% 
  left_join(demo_2022, by = c("year", "serial"), multiple = "all")

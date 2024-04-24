## family structure labor force stats emulating urban report: 
#link: https://www.urban.org/sites/default/files/publication/32976/411936-racial-and-ethnic-disparities-among-low-income-families.pdf

demog_lfpr <- createWorkbook()

# creates a df that shows who has a child u18 in each household
fam_structure <- asec_clean %>% 
  filter(u18 == 1, relate != 1113, relate != 1115, relate != 1241, relate != 1242, relate != 1260, relate != 9900) %>% 
  mutate(relate = as.character(as_factor(relate)),
         relate = case_when(relate == "Spouse" | relate == "Same sex spouse" | relate == "Opposite sex spouse" ~ "Spouse",
                            relate == "Unmarried partner" | relate == "Opposite sex unmarried partner" | 
                              relate == "Same sex unmarried partner" ~ "Unmarried partner",
                            TRUE ~ relate)) %>% 
  group_by(year, serial, relate) %>%
  summarize(n = n()) %>% 
  pivot_wider(id_cols = c("year", "serial"), names_from = "relate", values_from = "n")

# households where people who are not the householder (and spouse) have a child u18
children_hhX <- fam_structure %>% 
  filter(is.na(`Head/householder`)) %>% 
  group_by(year) %>% 
  distinct(serial, .keep_all = TRUE)

# households where only the householder (and spouse) have a child u18
children_hhOnly <- fam_structure %>% 
  filter(!is.na(`Head/householder`) & is.na(Child) & is.na(Parent) & 
           is.na(`Other relatives, n.s.`) & is.na(Sibling) & is.na(Grandchild)) %>% 
  group_by(year) %>% 
  distinct(serial, .keep_all = TRUE)

# df of each unique serial that is lowincome + household head  has children under 18 and only household head (& sp) has child
unique_serial <- asec_clean %>% 
  filter(!is.na(wbhaa) & lowincome == 1 & u18 == 1 & hh_head_sp == 1) %>% 
  group_by(year) %>% 
  inner_join(children_hhOnly, by = c("year", "serial")) %>% 
  distinct(serial, .keep_all = TRUE) %>% 
  select(year, serial) %>% 
  ungroup()
  
# creating family df for all other code moving forward
asec_family <- asec_clean %>% 
  # inner joining unique_famid to keep all respondents per famid intact
  inner_join(unique_serial, by = c("year", "serial"), multiple = "all") %>% 
  mutate(# adjusting parent variables to only be household head
         single_parent_hh = if_else(relate == 101, single_parent, 0), #marst = 6: never married/single
         married_parent_hh = if_else(relate == 101, married_parent, 0),  #marst = 1: married, spouse present
         # removing non related adults who are not unmarried partners
         across(unemp | emp | ft | pt | earner_selfemp | earner, 
                ~if_else(.x == 1 & (relate == 1113 | relate == 1115 | relate == 1241 | relate == 1260), NA, .x)))

## df of each unique serial that has related adults to the hh head for descriptive stats: categorizing families by if there are related adults in the hh or not
has_related_adult <- asec_family %>% 
  filter(related_adult == 1) %>% 
  group_by(year) %>% 
  # keeping all unique famid
  distinct(serial, .keep_all = TRUE) %>% 
  select(year, serial) %>% 
  ungroup()

## df that only includes household heads that have children
parents_hh <- asec_family %>% 
  filter(relate == 101) %>% 
  group_by(year, serial) %>% 
  summarize(across(ends_with("_parent_hh") | starts_with("wbhaa_") | hhwgt, ~max(.x))) %>% 
  ungroup() %>%
  #filtering out obs that are N/A
  filter(!is.na(single_parent_hh) | !is.na(married_parent_hh)) 

## splitting above df by parents that have related adults and do not
parents_w_ra <- parents_hh %>% 
  inner_join(has_related_adult, by = c("year", "serial"), multiple = "all")

parents_wo_ra <- parents_hh %>% 
  anti_join(has_related_adult, by = c("year", "serial"))

# calculating weighted sums of single and married parents with related adults
parents_ra_sum <- parents_w_ra %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_AIAN | wbhaa_AAPI | wbhaa_hispanic, 
                   n_sp_ra = sum(single_parent_hh * hhwgt, na.rm = TRUE),
                   n_mp_ra = sum(married_parent_hh * hhwgt, na.rm = TRUE)) %>% 
  filter(group_value == 1) %>% select(-group_value)

# calculating weighted sums of parents with no related adults, then joining parents with related adults to create distribution
fam_structure_sum <- parents_wo_ra %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_AIAN | wbhaa_AAPI | wbhaa_hispanic, 
                   n_sp_nra = sum(single_parent_hh * hhwgt, na.rm = TRUE),
                   n_mp_nra = sum(married_parent_hh * hhwgt, na.rm = TRUE)) %>% 
  filter(group_value == 1) %>% select(-group_value) %>% 
  full_join(parents_ra_sum, by = c("year", "group_name")) %>% 
  select(year, group_name, contains("_ra") | contains("_nra")) %>% 
  mutate(sum = n_sp_ra + n_mp_ra + n_sp_nra + n_mp_nra,
         across(contains("_ra") | contains("_nra"), ~ round(.x / sum, digits = 4), .names = "share.{.col}"),
         ) %>% 
  select(year, group_name, contains("share.")) %>% 
  rename("Race" = group_name, 
         "Single parent, has related adults" = share.n_sp_ra, 
         "Married parent, has related adults" = share.n_mp_ra, 
         "Single parent, no related adults" = share.n_sp_nra,
         "Married parent, no related adults" = share.n_mp_nra)

////////// continue below line 

# df for labor stats for families, requires all adults in the household
all_parents <- parents_hh %>% 
  group_by(year, serial) %>% 
  # keeping all famid from parents_hh
  distinct(serial, .keep_all = TRUE) %>% 
  select(year, serial) %>% 
  ungroup() %>% 
  left_join(asec_family, by = c("year", "serial"), multiple = "all") %>% 
  filter(adult == 1, relate != 1113, relate != 1115, relate != 1241, relate != 1242, relate != 1260, relate != 9900) %>% 
  mutate(
    # family income of household head
    ftotval_1 = case_when(relate == 101 ~ ftotval,
                          TRUE ~ 0),
    # employment stat of household head and spouse, variable needed to distinguish between household and family
    emp_p = case_when(emp == 1 & hh_head_sp == 1 ~ 1,
                           emp == 0 & hh_head_sp == 1 ~ 0,
                           TRUE ~ NA),
    unemp_p = case_when(unemp == 1 & hh_head_sp == 1 ~ 1,
                             unemp == 0 & hh_head_sp == 1 ~ 0,
                             TRUE ~ NA),
    nilf_p = case_when(nilf == 1 & hh_head_sp == 1 ~ 1,
                            nilf == 0 & hh_head_sp == 1 ~ 0,
                            TRUE ~ NA),
    single_parent_hhsp = if_else(single_parent == 1 & hh_head_sp == 1 ~ 1, 0),
    married_parent_hhsp = if_else(married_parent == 1 & hh_head_sp == 1, 1, 0),
    across(emp | unemp | nilf | earner | earner_selfemp | ft | pt, 
           ~if_else(related_adult == 1 | relate == 1114 | relate == 1116 | relate == 1117, .x, 0)) %>% 
  group_by(year, serial, famid) %>%
  summarize(across(single_parent_hhsp | married_parent_hhsp | contains("wbhaa_") | ftotval_1 | hhwgt, ~max(.x)),
            across(earner | earner_selfemp | ft | pt | u6 | emp_p | unemp_p | nilf_p | emp | unemp | nilf, ~sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  # at least one related adult/parent in household is ...
  mutate(across(emp | ft | pt | unemp | nilf | emp_p | unemp_p | nilf_p, ~if_else(.x >= 1, 1, 0), .names = "at1_{.col}")))

# getting unweighted sample pop for families by race and marital status
uw_pop <- all_parents %>% 
  group_by(year) %>% 
  summarize_groups(fam_white | fam_black | fam_AIAN | fam_hispan | fam_AAPI,
                   sp = sum(single_parent),
                   mp = sum(married_parent)) %>% 
  filter(group_value == 1)

family_income <- all_parents %>% 
  group_by(year) %>% 
  summarize_groups(fam_white | fam_black | fam_AIAN | fam_hispan | fam_AAPI,
                   mean_faminc = round(weighted.mean(ftotval, w = hhwgt, na.rm = TRUE), 0),
                   median_faminc = round(median(ftotval, na.rm = TRUE), 0)) %>% 
  filter(group_value == 1) %>% 
  select(-group_value)

family_lf_stats_fun <- function(data, parent_marital){
  data %>% 
    filter({{parent_marital}} == 1) %>% 
    group_by(year) %>% 
    arrange(ftotval) %>% 
    summarize_groups(fam_white | fam_black | fam_AIAN | fam_hispan | fam_AAPI,
                     mean_faminc = round(weighted.mean(ftotval, w = hhwgt, na.rm = TRUE), 0),
                     #median_faminc = round(median(ftotval, na.rm = TRUE), 0),
                     n_earners = weighted.mean(n_earners, w = hhwgt, na.rm = TRUE),
                     n_emp = weighted.mean(n_emp, w = hhwgt, na.rm = TRUE),
                     n_selfemp = weighted.mean(n_selfemp, w = hhwgt, na.rm = TRUE),
                     at1_emp = weighted.mean(at1_emp, w = hhwgt, na.rm = TRUE),
                     at1_ft = weighted.mean(at1_ft, w = hhwgt, na.rm = TRUE),
                     at1_pt = weighted.mean(at1_pt, w = hhwgt, na.rm = TRUE),
                     at1_unemp = weighted.mean(at1_unemp, w = hhwgt, na.rm = TRUE),
                     at1_nilf = weighted.mean(at1_nilf, w = hhwgt, na.rm = TRUE),
                     p_emp_all = weighted.mean(p_emp_all, w = hhwgt, na.rm = TRUE),
                     p_emp_at1 = weighted.mean(p_emp_at1, w = hhwgt, na.rm = TRUE),
                     p_unemp_at1 = weighted.mean(p_unemp_at1, w = hhwgt, na.rm = TRUE),
                     p_nilf_at1 = weighted.mean(p_nilf_at1, w = hhwgt, na.rm = TRUE)) %>% 
    left_join(cpi_data, by = "year") %>% 
    mutate(r_mean_faminc = round(mean_faminc * cpi_base_value / cpi_u_rs, 0),
           r_median_faminc = round(median_faminc * cpi_base_value / cpi_u_rs, 0),
           across(n_earners | n_emp | n_selfemp | at1_emp | at1_ft | at1_pt | at1_unemp | at1_nilf |
                    p_emp_all | p_emp_at1 | p_unemp_at1 | p_nilf_at1, ~ round(.x, 3))) %>% 
    filter(group_value == 1) %>% 
    rename(Race = group_name, "Mean family income" = mean_faminc, "Median family income" = median_faminc, 
           "Average number of earners in household" = n_earners, "Average number of employed adults in household" = n_emp,
           "Average number of self-employed adults in household" = n_selfemp, "At least one adult employed in household" = at1_emp,
           "At least one adult working full-time in household" = at1_ft, "At least one adult working part-time in household" = at1_pt,
           "At least one adult unemployed in household" = at1_unemp, "At least one adult not in labor force in househld" = at1_nilf,
           "All parents employed" = p_emp_all, "At least one parent employed" = p_emp_at1, "At least one parent unemployed" = p_unemp_at1,
           "At least one parent not in labor force" = p_nilf_at1, "Real mean family income" = r_mean_faminc, 
           "Real median family income" = r_median_faminc) %>% 
    select(-group_value, -cpi_u_rs)
}

varlist_famgroup = c("fam_white", "fam_black", "fam_AIAN", "fam_hispan", "fam_AAPI")

households_u18_sp <- family_lf_stats_fun(all_parents, single_parent)
households_u18_sp <- select(households_u18_sp, -"All parents employed")
xlsx_hh_u18_sp <- varlist_famgroup %>% 
  map(~ sheets_fun(data = (filter(households_u18_sp, Race == .x)), wb = demog_lfpr, s = paste0("u18_sp_", .x)))

households_u18_mp <- family_lf_stats_fun(all_parents, married_parent)
xlsx_hh_u18_mp <- varlist_famgroup %>% 
  map(~ sheets_fun(data = (filter(households_u18_mp, Race == .x)), wb = demog_lfpr, s = paste0("u18_mp_", .x)))

households_u6_sp <- family_lf_stats_fun(filter(all_parents, u6 == 1), single_parent)
households_u6_sp <- select(households_u6_sp, -"All parents employed")
xlsx_hh_u6_sp <- varlist_famgroup %>% 
  map(~ sheets_fun(data = (filter(households_u6_sp, Race == .x)), wb = demog_lfpr, s = paste0("u6_sp_", .x)))

households_u6_mp <- family_lf_stats_fun(filter(all_parents, u6 == 1), married_parent)
xlsx_hh_u6_mp <- varlist_famgroup %>% 
  map(~ sheets_fun(data = (filter(households_u6_mp, Race == .x)), wb = demog_lfpr, s = paste0("u6_mp_", .x)))

# export df to workbook 
saveWorkbook(demog_lfpr, here("output","RWJF Family stats hhhead.xlsx"), overwrite = TRUE)

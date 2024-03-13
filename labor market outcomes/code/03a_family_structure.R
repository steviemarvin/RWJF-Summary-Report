## family structure labor force stats emulating urban report: 
#link: https://www.urban.org/sites/default/files/publication/32976/411936-racial-and-ethnic-disparities-among-low-income-families.pdf

# df of each unique famid that is lowincome + has children under 18
unique_famid <- asec_clean_data %>% 
  # year filers, respondent must have child, relation to household head must be a relative and not partner/roommate
  filter(year >= 2007 & year <= 2022, nchild != 0 & (eldch < 18 | yngch < 18), relate == 101) %>% 
  # creating variables to restrict to families that  are low income = below 200% of FPL
  mutate(lowincome = if_else(ftotval < (2 * cutoff), 1, 0)) %>% 
  # creating a key for inner_join later
  filter(lowincome == 1) %>% 
  group_by(year) %>% 
  # keeping all unique famid
  distinct(serial, .keep_all = TRUE) %>% 
  select(year, serial) %>% 
  ungroup()


# creating family df for all other code moving forward
asec_family_data <- asec_clean_data %>% 
  # inner joining unique_famid to keep all respondents per famid intact
  inner_join(unique_famid, by = c("year", "serial"), multiple = "all") %>% 
  mutate(
    ## creating variables for family structure identification
    # identifying related adults in the family to replicate figure 2: https://www.urban.org/sites/default/files/publication/32976/411936-racial-and-ethnic-disparities-among-low-income-families.pdf
    # also used to pull unique list of households with related adults to match parents that are householder
    adult = if_else(age >= 18, 1, 0),
    u6 = if_else(u6 == 1 & (relate == 101 | relate == 201), u6, 0),
    related_adult = if_else(relate >= 301 & relate <= 1001 & age >= 18, 1, 0),
    not_related_adult = if_else(relate >= 1113 & age >= 18, 1, 0),
    single_parent = if_else(marst == 6 & nchild > 0 & yngch < 18 & relate == 101, 1, 0),
    married_parent = if_else(marst == 1 & nchild > 0  & yngch < 18 & relate == 101, 1, 0),  # divorce, widowed, separated
    #msc_parent = if_else((marst == 2 | marst == 3 | marst == 4 | marst == 5 & age > 18) & nchild > 0, 1, 0),
    #partner = if_else(relate >= 1114 & relate <= 1117 & age > 18, 1, 0),
    # labor force variable creation
    # uhrsworkt = hours worked at all jobs
    uhrsworkt = na_if(uhrsworkt, 999),
    # uhrswork1 = hours worked at main job
    uhrswork1 = na_if(uhrswork1, 999),
    unemp = if_else(empstat >= 20 & empstat <= 22, 1, 0),
    unemp_18 = if_else(unemp == 1 & age >= 18, 1, 0),
    # 1 = not in labor force, restricting to adults
    nilf = case_when(labforce == 1 & adult == 1 ~ 1,
                     labforce == 2 & adult == 1 ~ 0,
                     TRUE ~ NA),
    emp = if_else(empstat > 0 & empstat < 20, 1, 0),
    emp_18 = if_else(emp == 1 & age >= 18, 1, 0),
    ft_t = if_else(uhrsworkt >= 35 & uhrsworkt != 997, 1, 0),
    pt_t = if_else(uhrsworkt < 35, 1, 0),
    vary_t = if_else(uhrsworkt == 997, 1, 0),
    ft_1 = if_else(uhrswork1 >= 35 & uhrswork1 != 997 & age >= 18, 1, 0),
    pt_1 = if_else(uhrswork1 < 35 & age >= 18, 1, 0),
    vary_1 = if_else(uhrswork1 == 997, 1, 0),
    earner = case_when(uhrswork1 != 0 & incwage != 0 & age >= 18 & (classwkr != 00 & classwkr != 29 & classwkr != 99 & classwkr != 13 & classwkr != 14) ~ 1,
                       adult == 0 ~ NA,
                       TRUE ~ 0),
    earner_selfemp = if_else(uhrswork1 != 0 & inctot != 0 & age >= 18 & (classwkr == 13 | classwkr == 14), 1, 0),
    earner_u18 = if_else(uhrswork1 != 0 & incwage != 0 & age < 18 & (classwkr != 00 & classwkr != 29 & classwkr != 99 & classwkr != 13 & classwkr != 14), 1, 0),
    across(unemp_18 | emp_18 | ft_t | pt_t | vary_t | ft_1 | ft_1 | vary_1 | earner_selfemp | earner, 
           ~if_else(.x == 1 & (relate == 1115 | relate == 1241 | relate == 1260), NA, .x)))


## df of each unique serial that has related adults to the hh head for descriptive stats: categorizing families by if there are related adults in the hh or not
has_related_adult <- asec_family_data %>% 
  filter(related_adult == 1) %>% 
  group_by(year) %>% 
  # keeping all unique famid
  distinct(serial, .keep_all = TRUE) %>% 
  select(year, serial) %>% 
  ungroup()

## df that only includes household heads that have children
parents_householdhead <- asec_family_data %>% 
  filter(relate == 101) %>% 
  group_by(year, serial) %>% 
  summarize(
    single_parent = max(single_parent),
    married_parent = max(married_parent),
    fam_white = max(is_white_nh),
    fam_black = max(is_black_nh),
    fam_AIAN = max(is_AIAN_nh),
    fam_AAPI = max(is_AAPI_nh),
    fam_hispan = max(is_hispanic),
    hhwgt = max(asecwth)) %>% 
  ungroup() %>%
  #filtering out obs that are N/A
  filter(!is.na(single_parent) | !is.na(married_parent)) 

## splitting above df by parents that have related adults and do not
parents_wo_related_adults <- parents_householdhead %>% 
  anti_join(has_related_adult, by = c("year", "serial"))

parents_w_related_adults <- parents_householdhead %>% 
  inner_join(has_related_adult, by = c("year", "serial"), multiple = "all")
  
# calculating weighted sums of single and married parents with related adults
parents_ra_dist <- parents_w_related_adults %>% 
  group_by(year) %>% 
  summarize_groups(fam_white | fam_black | fam_AIAN | fam_AAPI | fam_hispan, 
                   n_single_parent_ra = sum(single_parent * hhwgt, na.rm = TRUE),
                   n_married_parent_ra = sum(married_parent * hhwgt, na.rm = TRUE)) %>% 
  filter(group_value == 1)

# calculating weighted sums of parents with no related adults, then joining parents with related adults to create distribution
fam_structure_dist <- parents_wo_related_adults %>% 
  group_by(year) %>% 
  summarize_groups(fam_white | fam_black | fam_AIAN | fam_AAPI | fam_hispan, 
                   n_single_parent_nra = sum(single_parent * hhwgt, na.rm = TRUE),
                   n_married_parent_nra = sum(married_parent * hhwgt, na.rm = TRUE)) %>% 
  filter(group_value == 1) %>% 
  full_join(parents_ra_dist, by = c("year", "group_name")) %>% 
  select(year, group_name, n_single_parent_ra, n_married_parent_ra, n_single_parent_nra, n_married_parent_nra) %>% 
  mutate(sum = n_single_parent_ra + n_married_parent_ra + n_single_parent_nra + n_married_parent_nra,
         sp_ra = n_single_parent_ra / sum,
         mp_ra = n_married_parent_ra / sum,
         sp_nra = n_single_parent_nra / sum,
         mp_nra = n_married_parent_nra / sum,
         across(sp_ra | mp_ra | sp_nra | mp_nra, ~round(.x, digits = 4))) %>% 
  select(year, group_name, sp_ra, mp_ra, sp_nra, mp_nra) %>% 
  rename("Race" = group_name, "Single parent, has related adults" = sp_ra, 
         "Married parent, has related adults" = mp_ra, "Single parent, no related adults" = sp_nra,
         "Married parent, no related adults" = mp_nra)

fam_structure_dist <- sheets_fun(fam_structure_dist, wb = demog_lfpr, s = "family structure dist.") 

# df for labor stats for families that require all adults in the household
all_parents <- parents_householdhead %>% 
  group_by(year, serial) %>% 
  # keeping all famid from parents_householdhead
  distinct(serial, .keep_all = TRUE) %>% 
  select(year, serial) %>% 
  ungroup() %>% 
  left_join(asec_family_data, by = c("year", "serial"), multiple = "all") %>% 
  filter(adult == 1, relate != 1113, relate != 1115, relate != 1241, relate != 1242, relate != 1260, relate != 9900) %>% 
  mutate(
    # family income of household head
    ftotval_1 = case_when(relate == 101 ~ ftotval,
                          TRUE ~ 0),
    # employment stat of household head and spouse, variable needed to distinguish between household and family
    emp_parent = case_when(emp_18 == 1 & (relate == 101 | relate == 201) ~ 1,
                           emp_18 == 0 & (relate == 101 | relate == 201) ~ 0,
                           TRUE ~ NA),
    unemp_parent = case_when(unemp_18 == 1 & (relate == 101 | relate == 201) ~ 1,
                             unemp_18 == 0 & (relate == 101 | relate == 201) ~ 0,
                             TRUE ~ NA),
    nilf_parent = case_when(nilf == 1 & (relate == 101 | relate == 201) ~ 1,
                            nilf == 0 & (relate == 101 | relate == 201) ~ 0,
                            TRUE ~ NA)) %>% 
  group_by(year, serial, famid) %>%
  summarize(
    single_parent = max(single_parent),
    married_parent = max(married_parent),
    fam_white = max(is_white_nh),
    fam_black = max(is_black_nh),
    fam_AIAN = max(is_AIAN_nh),
    fam_AAPI = max(is_AAPI_nh),
    fam_hispan = max(is_hispanic),
    faminc = max(ftotval_1),
    n_earners = sum(earner, na.rm = TRUE),
    n_selfemp = sum(earner_selfemp, na.rm = TRUE),
    hhwgt = max(wgt),
    ftotval = max(ftotval_1),
    n_ft = sum(ft_1, na.rm = TRUE),
    n_pt = sum(pt_1, na.rm = TRUE),
    u6 = max(u6),
    n_emp = sum(emp_18, na.rm = TRUE),
    n_unemp = sum(unemp_18, na.rm = TRUE),
    n_nilf = sum(nilf, na.rm = TRUE),
    n_emp_parent = sum(emp_parent, na.rm = TRUE),
    n_unemp_parent = sum(unemp_parent, na.rm = TRUE),
    n_nilf_parent = sum(nilf_parent, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # at least one adult in household is employed
    at1_emp = if_else(n_emp >= 1, 1, 0),
    # at least one adult in household is full time
    at1_ft = if_else(n_ft >= 1, 1, 0),
    # at least one adult in household is part time
    at1_pt = if_else(n_pt >= 1, 1, 0),
    # at least one adult in household is unemployed
    at1_unemp = if_else(n_unemp >= 1, 1, 0),
    # at least one adult in household is not in the labor force
    at1_nilf = if_else(n_nilf >= 1, 1, 0),
    # all parents are employed
    p_emp_all = case_when(single_parent == 1 & n_emp_parent == 1 ~ 1,
                          married_parent == 1 & n_emp_parent == 2 ~ 1,
                          TRUE ~ 0),
    p_emp_at1 = if_else(n_emp_parent >= 1, 1, 0),
    p_unemp_at1 = if_else(n_unemp_parent >= 1, 1, 0),
    p_nilf_at1 = if_else(n_nilf_parent >= 1, 1, 0))

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
                     median_faminc = round(median(ftotval, na.rm = TRUE), 0),
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
    mutate(r_mean_faminc = round(mean_faminc * cpi_base_value / cpiurs, 0),
           r_median_faminc = round(median_faminc * cpi_base_value / cpiurs, 0),
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
    select(-group_value, -cpiurs)
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

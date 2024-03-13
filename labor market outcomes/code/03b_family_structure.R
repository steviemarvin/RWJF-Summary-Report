# df of each unique famid that is lowincome + has children under 18
unique_famid <- asec_clean_data %>% 
  # year filers, respondent must have child, relation to household head must be a relative and not partner/roommate
  filter(year >= 2007 & year <= 2022, nchild != 0, relate <= 1117 & relate != 1113) %>% 
  # creating variables to restrict to families that  are low income = below 200% of FPL
  mutate(lowincome = if_else(ftotval < 2 * cutoff, 1, 0)) %>% 
  # creating a key for inner_join later
  filter(lowincome == 1 & (eldch < 18 | yngch < 18)) %>% 
  group_by(year, serial) %>% 
  # keeping all unique famid
  distinct(famid, .keep_all = TRUE) %>% 
  select(year, serial, famid) %>% 
  ungroup()


# creating family df for all other code moving forward
asec_family_data <- asec_clean_data %>% 
  # inner joining unique_famid to keep all respondents per famid intact
  inner_join(unique_famid, by = c("year", "serial", "famid"), multiple = "all") %>% 
  filter(relate != 1115) %>% 
  mutate(## creating variables for family structure identification
    # identifying related adults in the family to replicate figure 2: https://www.urban.org/sites/default/files/publication/32976/411936-racial-and-ethnic-disparities-among-low-income-families.pdf
    # also used to pull unique list of households with related adults to match parents that are householder
    adult = if_else(age >= 18, 1, 0),
    related_adult = if_else(relate >= 301 & relate <= 1001 & age >= 18, 1, 0),
    not_related_adult = if_else(relate >= 1113 & age >= 18, 1, 0),
    single_parent = case_when(marst == 6 & nchild > 0 & yngch < 18 ~ 1,
                              marst == 1 & nchild > 0  & yngch < 18 ~ 0,
                              TRUE ~ NA),
    married_parent = if_else(single_parent == 0, 1, 0),# check on this
    # divorce, widowed, separated
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
    earner_u18 = if_else(uhrswork1 != 0 & incwage != 0 & age < 18 & (classwkr != 00 & classwkr != 29 & classwkr != 99 & classwkr != 13 & classwkr != 14), 1, 0))

parent_weights <- 

# this df groups by year, serial, and famid. There can be more than one family per serial
family_config_fam <- asec_family_data %>% 
  group_by(year, serial, famid) %>% 
  summarize(
    n_adults = sum(adult, na.rm = TRUE),
    n_earners = sum(earner, na.rm = TRUE),
    n_earner13 = sum(earner_13),
    n_earner_u18 = sum(earner_u18, na.rm = TRUE),
    n_related_adults = max(related_adult),
    n_not_related = max(not_related_adult),
    n_emp_adult = sum(emp_18),
    n_unemp_adult = sum(emp_18),
    n_nilf = sum(nilf, na.rm = TRUE),
    single_parent = sum(single_parent, na.rm = TRUE),
    married_parent = sum(married_parent, na.rm = TRUE),
    msc_parent = sum(msc_parent, na.rm = TRUE),
    partner = sum(partner, na.rm = TRUE),
    related_adult = max(related_adult, na.rm = TRUE),
    fam_white = max(is_white_nh),
    fam_black = max(is_black_nh),
    fam_AIAN = max(is_AIAN_nh),
    fam_AAPI = max(is_AAPI_nh),
    fam_hispan = max(is_hispanic),
    fam_other = max(is_other),
    fam_foreign = max(foreign),
    u6 = max(u6),
    hhwgt = max(wgt),
    ftotval = max(ftotval),
    n_ft = sum(ft_t),
    n_pt = sum(pt_t)
  ) %>% 
  ungroup() %>% 
  filter(n_adults != 0)


n_adults_fam <- family_config_fam %>% 
  group_by(year) %>% 
  arrange(n_adults) %>% 
  summarize_groups(fam_white | fam_black | fam_AIAN | fam_AAPI | fam_hispan, 
                   mean_n_adults = weighted.mean(n_adults, w = hhwgt),
                   med_n_adults = median(n_adults),
                   ) %>% 
  filter(group_value == 1)

n_adults_hh <- family_config_hh %>% 
  group_by(year) %>% 
  arrange(n_adults) %>% 
  summarize_groups(fam_white | fam_black | fam_AIAN | fam_AAPI | fam_hispan, 
                   mean_n_adults = weighted.mean(n_adults, w = hhwgt, na.rm = TRUE),
                   med_n_adults = median(n_adults)) %>% 
  filter(group_value == 1)


# list of race and ethnicity groups to run through function
varlist1 <- c("fam_white", "fam_black", "fam_AIAN", "fam_AAPI", "fam_hispan")


# create wb object
grandparent_child_fam <- asec_clean_data %>% 
  mutate(
    householdhead = if_else(relate == 101, 1, 0),
    child_to_hh = if_else((relate == 301 | relate == 303) & age < 18, 1, 0),
    parent_to_hh = if_else(relate == 501, 1, 0),
    grandchild_to_hh = if_else(relate == 901, 1, 0)) %>% 
  group_by(year, serial, famid) %>% 
  summarize(
    householdhead = max(householdhead),
    child_to_hh = max(child_to_hh),
    parent_to_hh = max(parent_to_hh),
    grandchild_to_hh = max(grandchild_to_hh),
    wgt = min(wgt)) %>% 
  mutate(grandparent_child = case_when(householdhead == 1 & grandchild_to_hh == 1 & child_to_hh == 0 ~ 1,
                                       TRUE ~ 0)) %>% 
  ungroup() %>% group_by(year) %>% 
  summarize(grandparent_child_sum = sum(grandparent_child * wgt))


# export df to workbook 
saveWorkbook(demog_lfpr, here("output","RWJF Family stats.xlsx"), overwrite = TRUE)
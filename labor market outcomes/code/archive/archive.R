
test <- asec_clean_data %>% 
  group_by(year, serial, famid) %>% 
  summarize(
    fam_white = max(is_white_nh),
    fam_black = max(is_black_nh),
    fam_AIAN = max(is_AIAN_nh),
    fam_AAPI = max(is_AAPI_nh),
    fam_hispan = max(is_hispanic),
    fam_other = max(is_other),
    u6 = max(u6),
    wgt = max(wgt)) %>% 
  ungroup() %>% 
  mutate(
    mixed_hh = case_when(fam_white >= 1 & (fam_black >= 1 | fam_AIAN >= 1 |
                                             fam_AAPI >= 1 | fam_hispan >= 1) ~ 1,
                         fam_black >= 1 & (fam_AIAN >= 1 | fam_AAPI >= 1 | fam_hispan >= 1) ~ 1,
                         fam_AIAN >= 1 & (fam_AAPI >= 1 | fam_hispan >= 1) ~ 1,
                         TRUE ~ 0),
    mixed_black = case_when(fam_black == 1 & mixed_hh == 1 ~ 1,
                            TRUE ~ 0),
    mixed_white = case_when(fam_white == 1 & mixed_hh == 1 ~ 1,
                            TRUE ~ 0),
    mixed_aian = case_when(fam_AIAN == 1 & mixed_hh == 1 ~ 1,
                           TRUE ~ 0),
    mixed_aapi = case_when(fam_AAPI == 1 & mixed_hh == 1 ~ 1,
                           TRUE ~ 0)) %>% 
  group_by(year) %>% 
  summarize(
    white = sum(fam_white * wgt),
    black = sum(fam_black * wgt),
    aian = sum(fam_AIAN * wgt),
    aapi = sum(fam_AAPI * wgt),
    hispan = sum(fam_hispan * wgt),
    other = sum(fam_other * wgt),
    mixed = sum(mixed_hh * wgt),
    mixed_white = sum(mixed_white * wgt),
    mixed_black = sum(mixed_black * wgt),
    mixed_aian = sum(mixed_aian * wgt),
    mixed_aapi = sum(mixed_aapi * wgt)) %>% 
  mutate(mixed_white_share = mixed_white / white * 100,
         mixed_black_share = mixed_black / black * 100,
         mixed_aian_share = mixed_aian / aian * 100,
         mixed_aapi_share = mixed_aapi / aapi * 100)

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
         foreign = ifelse(nativity == 5 & nativity != 0, 1, 0)) %>% 
  #collapsing 
  group_by(year, month, serial, famid) %>% 
  summarize(
    wgt = min(wgt), 
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


# collapsing multiple entries per family to single entry per family
asec_family_data2 <- asec_clean_data %>% 
  # creating indicator variables for race/ethnicity for collapsing + calculating shares
  mutate(is_white_nh = ifelse(new_race_hispan == "White not hispanic", 1, 0), 
         is_black_nh = ifelse(new_race_hispan == "Black not hispanic", 1, 0),
         is_AIAN_nh = ifelse(new_race_hispan == "AIAN not hispanic", 1, 0),
         is_AAPI_nh = ifelse(new_race_hispan == "AAPI not hispanic", 1, 0),
         is_hispanic = ifelse(new_race_hispan == "Hispanic", 1, 0),
         #is_citizen = ifelse(citizen <= 4, 1, 0),
         #not_citizen = ifelse(citizen == 5, 1, 0),
         foreign = ifelse(nativity == 5 & nativity != 0, 1, 0)) %>% 
  #collapsing 
  group_by(year, month, serial, famid) %>% 
  summarize(
    wgt = min(wgt), 
    fam_lowincome = min(lowincome),
    fam_income = min(ftotval),
    fam_white = max(is_white_nh),
    fam_black = max(is_black_nh),
    fam_AIAN = max(is_AIAN_nh),
    fam_AAPI = max(is_AAPI_nh),
    fam_hispan = max(is_hispanic),
    fam_foreign = max(foreign),
    fam_all_foreign = sum(foreign, na.rm = TRUE),
    fam_unemp = max(unemp),
    fam_emp = max(emp),
    fam_all_emp = sum(emp, na.rm = TRUE),
    fam_nilf = max(nilf),
    fam_ft = max(ft),
    fam_pt = max(pt),
    fam_all_ft = sum(ft, na.rm = TRUE),
    fam_all_pt = sum(pt, na.rm = TRUE),
    u6 = max(u6),
    u18 = max(u18)) %>% 
  ungroup() %>% 
  mutate(
    mixed_hh = case_when(fam_white == 1 & (fam_black == 1 | fam_AIAN == 1 |
                                             fam_AAPI == 1 | fam_hispan == 1) ~ 1,
                         fam_black == 1 & (fam_AIAN == 1 | fam_AAPI == 1 | fam_hispan == 1) ~ 1,
                         fam_AIAN == 1 & (fam_AAPI == 1 | fam_hispan == 1) ~ 1,
                         fam_AAPI == 1 & fam_hispan == 1 ~ 1,
                         TRUE ~ 0),
    mixed_black = case_when(fam_black == 1 & mixed_hh == 1 ~ 1,
                            TRUE ~ 0),
    mixed_white = case_when(fam_white == 1 & mixed_hh == 1 ~ 1,
                            TRUE ~ 0),
    mixed_hispan = case_when(fam_hispan == 1 & mixed_hh == 1 ~ 1,
                             TRUE ~ 0),
    mixed_aian = case_when(fam_AIAN == 1 & mixed_hh == 1 ~ 1,
                           TRUE ~ 0),
    mixed_aapi = case_when(fam_AAPI == 1 & mixed_hh == 1 ~ 1,
                           TRUE ~ 0))

fam_sum1 <- asec_family_data %>% 
  group_by(year) %>% 
  summarize(
    share_black = weighted.mean(fam_black, w = wgt),
    share_white = weighted.mean(fam_white, w = wgt),
    share_AAPI = weighted.mean(fam_AAPI, w = wgt),
    share_AIAN = weighted.mean(fam_AIAN, w = wgt),
    share_hispan = weighted.mean(fam_hispan, w = wgt),
    share_foreign = weighted.mean(fam_foreign, w = wgt),
    median_famincome = median(fam_income),
    #median_real_famincome = median(real_fam_income),
    mean_famincome = weighted.mean(fam_income, w = wgt),
    emp_1 = weighted.mean(fam_emp, w = wgt),
    unemp_1 = weighted.mean(fam_unemp, w = wgt),
    ft_1 = weighted.mean(fam_ft, w = wgt, na.rm = TRUE),
    pt_1 = weighted.mean(fam_pt, w = wgt, na.rm = TRUE),
    nilf_1 = weighted.mean(fam_nilf, w = wgt)
  )


fam_sum2 <- asec_family_data2 %>% 
  group_by(year) %>% 
  summarize(
    share_black = weighted.mean(fam_black, w = wgt),
    share_white = weighted.mean(fam_white, w = wgt),
    share_AAPI = weighted.mean(fam_AAPI, w = wgt),
    share_AIAN = weighted.mean(fam_AIAN, w = wgt),
    share_hispan = weighted.mean(fam_hispan, w = wgt),
    share_foreign = weighted.mean(fam_foreign, w = wgt),
    median_famincome = median(fam_income),
    #median_real_famincome = median(real_fam_income),
    mean_famincome = weighted.mean(fam_income, w = wgt),
    emp_1 = weighted.mean(fam_emp, w = wgt),
    unemp_1 = weighted.mean(fam_unemp, w = wgt),
    ft_1 = weighted.mean(fam_ft, w = wgt, na.rm = TRUE),
    pt_1 = weighted.mean(fam_pt, w = wgt, na.rm = TRUE),
    nilf_1 = weighted.mean(fam_nilf, w = wgt),
    share_mixed = weighted.mean(mixed_hh, w = wgt)
  )



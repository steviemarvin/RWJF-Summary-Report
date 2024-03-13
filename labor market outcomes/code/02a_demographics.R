# demographics
demographics <- asec_clean_data %>% 
  mutate(
    old_white = is_white_nh, old_black = is_black_nh, old_aian = is_AIAN_nh, old_aapi = is_AAPI_nh,
    old_hispan = is_hispanic, old_foreign = foreign,
    is_white_nh = if_else(is_white_nh == 1 & (relate == 101 | relate == 201) & u18 == 1, 1, 0),
    is_black_nh = if_else(is_black_nh == 1 & (relate == 101 | relate == 201) & u18 == 1, 1, 0),
    is_AIAN_nh = if_else(is_AIAN_nh == 1 & (relate == 101 | relate == 201) & u18 == 1, 1, 0),
    is_AAPI_nh = if_else(is_AAPI_nh == 1 & (relate == 101 | relate == 201) & u18 == 1, 1, 0),
    is_hispanic = if_else(is_hispanic == 1 & (relate == 101 | relate == 201) & u18 == 1, 1, 0),
    foreign = if_else(foreign == 1 & (relate == 101 | relate == 201) & u18 == 1, 1, 0)) %>% 
  group_by(year, serial) %>% 
  summarize(
    fam_white = max(is_white_nh),
    fam_black = max(is_black_nh),
    fam_AIAN = max(is_AIAN_nh),
    fam_AAPI = max(is_AAPI_nh),
    fam_hispan = max(is_hispanic),
    fam_other = max(is_other),
    fam_foreign = max(foreign),
    u6 = max(u6),
    u18 = max(u18),
    wgt = max(wgt),
    lowincome = max(lowincome)) %>% 
  ungroup() %>% 
  mutate(
    mixed_hh = case_when(fam_white >= 1 & (fam_black >= 1 | fam_AIAN >= 1 |
                                             fam_AAPI >= 1) ~ 1,
                         fam_black >= 1 & (fam_AIAN >= 1 | fam_AAPI >= 1) ~ 1,
                         fam_AIAN >= 1 & fam_AAPI >= 1 ~ 1,
                         TRUE ~ 0),
    mixed_white = if_else(fam_white >= 1 & mixed_hh == 1, 1, 0),
    mixed_black = if_else(fam_black >= 1 & mixed_hh == 1, 1, 0),
    mixed_AIAN = if_else(fam_AIAN >= 1 & mixed_hh == 1, 1, 0),
    mixed_AAPI = if_else(fam_AAPI >= 1 & mixed_hh == 1, 1, 0))

  
# population distribution by race/ethnicity by u18 or u6
demographics_fun <- function(age){
  demographics %>% 
    filter({{age}} == 1) %>% 
    group_by(year) %>% 
    summarize(
      share_black = weighted.mean(fam_black, w = wgt),
      black = sum(fam_black * wgt),
      share_white = weighted.mean(fam_white, w = wgt),
      white = sum(fam_white * wgt),
      share_AAPI = weighted.mean(fam_AAPI, w = wgt),
      aapi = sum(fam_AAPI * wgt),
      share_AIAN = weighted.mean(fam_AIAN, w = wgt),
      aian = sum(fam_AIAN * wgt),
      share_hispan = weighted.mean(fam_hispan, w = wgt),
      share_foreign = weighted.mean(fam_foreign, w = wgt),
      share_mixed = weighted.mean(mixed_hh, w = wgt),
      mixed_white = sum(mixed_white * wgt),
      mixed_black = sum(mixed_black * wgt),
      mixed_aian = sum(mixed_AIAN * wgt),
      mixed_aapi = sum(mixed_AAPI * wgt),
      share_foreign = weighted.mean(fam_foreign, w = wgt)) %>% 
    # calculating share of mixed households as a percentage of respective population group
    mutate(mixed_White = mixed_white / white,
           mixed_Black = mixed_black / black,
           mixed_AIAN = mixed_aian / aian,
           mixed_AAPI = mixed_aapi / aapi) %>% 
    select(year, share_black, mixed_Black, share_white, mixed_White, share_AAPI, mixed_AAPI,
           share_AIAN, mixed_AIAN, share_hispan, share_foreign, share_mixed)}

# population distribution of low income famillies by race/ethnicity by u18 or u6
demographics_li_fun <- function(age){
  demographics %>% 
    filter({{age}} == 1, lowincome == 1) %>% 
    group_by(year) %>% 
    summarize(
      share_black = weighted.mean(fam_black, w = wgt),
      black = sum(fam_black * wgt),
      share_white = weighted.mean(fam_white, w = wgt),
      white = sum(fam_white * wgt),
      share_AAPI = weighted.mean(fam_AAPI, w = wgt),
      aapi = sum(fam_AAPI * wgt),
      share_AIAN = weighted.mean(fam_AIAN, w = wgt),
      aian = sum(fam_AIAN * wgt),
      share_hispan = weighted.mean(fam_hispan, w = wgt),
      share_foreign = weighted.mean(fam_foreign, w = wgt),
      share_mixed = weighted.mean(mixed_hh, w = wgt),
      mixed_white = sum(mixed_white * wgt),
      mixed_black = sum(mixed_black * wgt),
      mixed_aian = sum(mixed_AIAN * wgt),
      mixed_aapi = sum(mixed_AAPI * wgt),
      share_foreign = weighted.mean(fam_foreign, w = wgt)) %>% 
    # calculating share of mixed households as a percentage of respective population group
    mutate(mixed_White = mixed_white / white,
           mixed_Black = mixed_black / black,
           mixed_AIAN = mixed_aian / aian,
           mixed_AAPI = mixed_aapi / aapi) %>% 
    select(year, share_black, mixed_Black, share_white, mixed_White, share_AAPI, mixed_AAPI,
           share_AIAN, mixed_AIAN, share_hispan, share_foreign, share_mixed)}

# creating a wb for export
demog_lfpr <- createWorkbook() 

# race and nativity distribution of families with children under 18
demographics_18 <- demographics_fun(u18)
sheets_fun(demographics_18, wb = demog_lfpr, s = "demog_18", format = "PERCENTAGE") 

# race and nativity distribution of families with children under 6
demographics_6 <- demographics_fun(u6)
sheets_fun(demographics_6, wb = demog_lfpr, s = "demog_6", format = "PERCENTAGE") 

# race and nativity distribution of lowincome families with children under 18
demographics_li_18 <- demographics_li_fun(u18)
sheets_fun(demographics_li_18, wb = demog_lfpr, s = "demog_li_18", format = "PERCENTAGE") 

# race and nativity distribution of lowincome families with children under 6
demographics_li_6 <- demographics_li_fun(u6)
sheets_fun(demographics_li_6, wb = demog_lfpr, s = "demog_li_6", format = "PERCENTAGE") 

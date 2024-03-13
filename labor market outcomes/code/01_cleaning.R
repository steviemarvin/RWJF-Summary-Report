## cleaning asec data

asec_clean_data <- asec_raw_data %>% 
  filter(hflag == 1 | is.na(hflag)) %>% 
  mutate(lowincome = if_else(ftotval < (cutoff * 2), 1, 0),
         u18 = if_else(yngch < 18 | eldch < 18, 1, 0),
         u6 = if_else(yngch < 6 | eldch < 6, 1, 0),
         # race and ethnicity variable so that race and ethnicity are mutually exclusive
         new_race_ethn = case_when(race == 100 & hispan == 0 & (eldch < 18 | yngch < 18)  ~ 1,
                                   race == 200 & hispan == 0 & (eldch < 18 | yngch < 18) ~ 2,
                                   race == 300 & hispan == 0 & (eldch < 18 | yngch < 18) ~ 3,
                                   race >= 650 & race <= 652 & hispan == 0 & (eldch < 18 | yngch < 18) ~ 4,
                                   hispan >= 1 & hispan <= 612 & (eldch < 18 | yngch < 18) ~ 5, 
                                   TRUE ~ 6),
         new_race_ethn = labelled(new_race_ethn, c("White not hispanic" = 1, "Black not hispanic" = 2, 
                                                   "AIAN not hispanic" = 3, "AAPI not hispanic" = 4,
                                                   "Hispanic" = 5, "Other" = 6)),
         # creating indicator variables for each race/ethnicity for summarizing families as a whole down the line
         is_white_nh = ifelse(new_race_ethn == 1, 1, 0), 
         is_black_nh = ifelse(new_race_ethn == 2, 1, 0),
         is_AIAN_nh = ifelse(new_race_ethn == 3, 1, 0),
         is_AAPI_nh = ifelse(new_race_ethn == 4, 1, 0),
         is_hispanic = ifelse(new_race_ethn == 5, 1, 0),
         is_other = ifelse(new_race_ethn == 6, 1, 0),
         foreign = ifelse(nativity == 5 & nativity != 0, 1, 0),
         wgt = asecwth, pwgt = asecwt, across(poverty | sex | new_race_ethn, ~as.character(as_factor(.x))))
  


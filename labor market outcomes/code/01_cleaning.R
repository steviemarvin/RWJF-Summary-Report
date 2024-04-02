## cleaning asec data

asec_clean <- asec_raw_data %>% 
  # filtering out hflag == 0 to ensure 2014 ASEC is properly weighted
  filter(hflag == 1 | is.na(hflag)) %>% 
  mutate(survey_year = year,
         year = year - 1,
         offtotval = if_else(offtotval == 9999999999, NA, offtotval),
         offcutoff = if_else(offcutoff == 999999, NA, offcutoff),
         lowincome = if_else(offtotval < (offcutoff * 2), 1, 0),
         pov100 = if_else(offtotval < offcutoff, 1, 0),
         pov50 = if_else(offtotval < (offcutoff / 2), 1, 0),
         # variable that indicates that respondent is either household head or spouse
         hh_head_sp = if_else(relate == 101 | relate == 201 | relate == 202 | relate == 203, 1, 0),
         LGB = if_else(relate == 202 | relate == 1117, 1, 0),
         # indicator variable for whether respondent is an adult related to the household head (includes spouses)
         related_adult = if_else(relate >= 301 & relate <= 1001 & age >= 18, 1, 0),
         # indicator variable for whether respondent is an adult not related to the household head (includes unmarried partner)
         not_related_adult = if_else(relate >= 1113 & age >= 18, 1, 0),
         # indicator variables for whether or not repondent has a child under 18 or under 6
         u18 = if_else(yngch < 18 | eldch < 18, 1, 0),
         u6 = if_else(yngch < 6 | eldch < 6, 1, 0),
         # race and ethnicity variable so that race and ethnicity are mutually exclusive, modeling race variable from epi microdata extracts
         wbhaa = case_when(race == 100 & hispan == 0 ~ 1,
                           # black = 2, following EPI methodology all Black people are accounted for
                          (race == 200 | race == 801 | race == 805 | race == 806 | 
                             race == 807 | race == 810 | race == 811 | race == 814 | 
                             race == 816 | race == 818) & hispan == 0 ~ 2,
                          # hispanic = 3
                          hispan >= 1 & hispan <= 612 ~ 3,
                          # AAPI = 4
                          ((race >= 650 & race <= 652) | race == 809 | race == 803 | 
                             race == 804) ~ 4,    
                          # AIAN = 5
                          (race == 300 | race == 802 | race == 808 | race == 812 | 
                             race == 813 | race == 815 | race == 817 | race == 819) & 
                             hispan == 0 ~ 5, 
                          TRUE ~ NA),
         wbhaa = labelled(wbhaa, c("white" = 1, "black" = 2, "hispanic" = 3, "AAPI" = 4, "AIAN" = 5)),
         # foreign born indicator variable
         foreign_born = ifelse(nativity == 5 & nativity != 0, 1, 0),
         citistat = case_when(citizen <= 3 ~ "citizen",
                              citizen == 4 ~ "naturalized",
                              citizen == 5 ~ "not_citizen",
                              TRUE ~ NA),
         ## creating variables for family structure identification
         # identifying related adults in the family to replicate figure 2: https://www.urban.org/sites/default/files/publication/32976/411936-racial-and-ethnic-disparities-among-low-income-families.pdf
         adult = if_else(age >= 18, 1, 0),
         # indicator variables of type of parent
         single_parent = if_else(marst == 6 & yngch < 18, 1, 0), #marst = 6: never married/single
         married_parent = if_else(marst == 1 & yngch < 18, 1, 0),  #marst = 1: married, spouse present
         # labor force variable creation
         # uhrsworkly = usual hours worked per week last year
         uhrsworkly = na_if(uhrsworkly, 999),
         unemp = if_else((empstat >= 20 & empstat <= 22) & age >= 18, 1, 0),
         # 1 = not in labor force, restricting to adults
         nilf = case_when(labforce == 1 & age >= 18 ~ 1,
                          labforce == 2 & age >= 18 ~ 0,
                          TRUE ~ NA),
         emp = if_else((empstat > 0 & empstat < 20) & age >= 18, 1, 0),
         # designating full time and part time status
         ft = if_else(uhrsworkly >= 35, 1, 0),
         pt = if_else(uhrsworkly < 35 & uhrsworkly > 0, 1, 0),
         # earner indicator variables
         annhrs = uhrsworkly * wkswork1,
         earner = if_else(annhrs != 0 & incwage != 0 & age >= 18 & (classwkr != 0 & classwkr != 29 & classwkr != 99 & classwkr != 13), 1, 0),
         # renaming weights
         relate = case_when(relate == 202 | relate == 203 ~ 201,
                            relate == 1116 | relate == 1117 ~ 1114,
                            TRUE ~ relate),
         across(sex | wbhaa | race, ~as.character(as_factor(.x)))) %>% 
  # creating indicator variables for each race/ethnicity for summarizing families as a whole down the line
  dummy_cols(select_columns = 'wbhaa', ignore_na = TRUE)


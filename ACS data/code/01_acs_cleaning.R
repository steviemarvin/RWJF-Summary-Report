## master 
# cleaning up acs data
acs_clean <- acs_raw %>% 
  mutate(# variable that indicates that respondent is either household head or spouse
         hh_head_sp = if_else(related == 101 | related == 201, 1, 0),
         # indicator variables for whether or not repondent has a child under 18 or under 6
         u18 = if_else(yngch < 18 | eldch < 18, 1, 0),
         u6 = if_else(yngch < 6 | eldch < 6, 1, 0),
         # race and ethnicity variable so that race and ethnicity are mutually exclusive, modeling race variable from epi microdata extracts
         wbhaa = case_when(raced == 100 & hispan == 0 ~ 1,
                           (raced == 200 | raced == 801 | raced == 826 | raced == 830 | 
                              (raced >= 831 & raced <= 842) | raced == 845 | (raced >= 901 & raced <= 904) | 
                              raced == 917 | raced == 950 | raced == 973 | (raced >= 982 & raced <= 984) | raced == 985 | raced == 990) & hispan == 0 ~ 2,
                           hispan >= 1 & hispan <= 4 ~ 3,
                           # AIAN == 4
                           ((raced >= 300 & raced <= 399) | raced == 802 | (raced >= 850 & raced <= 856) | 
                              (raced >= 905 & raced <= 907) | raced == 916 | raced == 960 | raced == 974) & hispan == 0 ~ 4,
                           # AAPI == 5
                           ((raced >= 400 & raced <= 699) | (raced >= 810 & raced <= 825) | 
                              (raced >= 860 & raced <= 868) | (raced >= 883 & raced <= 893) | (raced >= 910 & raced <= 914) | 
                              (raced >= 920 & raced <= 925) | (raced >= 943 & raced <= 944) | (raced >= 963 & raced <= 964) | raced == 976) & hispan == 0 ~ 5),
         wbhaa = labelled(wbhaa, c("white" = 1, "black" = 2, "hispanic" = 3, "AIAN" = 4, "AAPI" = 5)),
         # indicator variables of type of parent
         single_parent = if_else(marst == 6 & u18 == 1, 1, 0), #marst = 6: never married/single
         married_parent = if_else((marst == 1 | marst == 2) & u18 == 1, 1, 0),  #marst = 1: married, spouse present; 2: married, spouse absent
         across(sex | wbhaa, ~as.character(as_factor(.x)))) 
  # creating indicator variables for each race/ethnicity for summarizing families as a whole down the line
  dummy_cols(select_columns = 'wbhaa', ignore_na = TRUE)


         
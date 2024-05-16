cleaning_fun <- function(data){
  data %>% 
    mutate(# variable that indicates that respondent is either household head or spouse
      hh_head_sp = if_else(related == 101 | related == 201, 1, 0),
      # indicator variables for whether or not respondent has a child under 18 or under 6
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
      across(sex | wbhaa, ~as.character(as_factor(.x)))) %>% 
  # creating indicator variables for each race/ethnicity for summarizing families as a whole down the line
  dummy_cols(select_columns = 'wbhaa', ignore_na = TRUE)
}

families_fun <- function(data){
  data %>% 
    filter(u18 == 1, poverty < 200 & poverty != 0 & related != 1114) %>% 
    mutate(related = as.character(as_factor(related))) %>% 
    group_by(year, serial, related) %>%
    summarize(n = n(), 
              hhwgt = max(hhwt)) %>% 
    pivot_wider(id_cols = c("year", "serial", "hhwgt"), names_from = c("related"), values_from = "n") %>% ungroup() %>% 
    rename_with(~gsub("/", "_", .), everything()) %>% 
    rename_with(~gsub(" ", "_", .), everything()) %>% 
    rename_with(~gsub("-", "_", .), everything()) %>% 
    rename(other_non_rel = "Other_non_relatives_(1990_includes_employees)") %>% 
    rename_with(tolower) %>% 
    select(year, serial, hhwgt, order(names(.))) %>% 
    # categorizing households by who in household has child
    mutate(hh_type2 = case_when(!is.na(head_householder) ~ "hhYes",
                                is.na(head_householder) ~ "hhNo")) %>% 
    select(year, serial, hhwgt, hh_type2)
}

housing_df_fun <- function(data, data2){
data %>% 
  mutate(hhincome = na_if(hhincome, 9999999),
         rentgrs = na_if(rentgrs, 0),
         owncost = na_if(owncost, 99999),
         owner = case_when(ownershp == 1 ~ 1,
                           ownershp == 2 ~ 0,
                           TRUE ~ NA),
         renter = case_when(ownershp == 2 ~ 1,
                            ownershp == 1 ~ 0,
                            TRUE ~ NA),
         rent_income_ratio = rentgrs / (hhincome/12),
         owncost_income_ratio = owncost / (hhincome/12)) %>% 
    filter(related == 101) %>% 
    inner_join(data2, by = c("year", "serial")) %>% 
    group_by(year, serial) %>% 
    summarize(across(u6 | hhwt | contains("ratio") | owner | renter | contains("wbhaa_"), ~max(.x))) %>% 
    mutate(
      rent_burden = case_when(rent_income_ratio >= 0.3 ~ 1,
                                 is.na(rent_income_ratio) ~ NA,
                                 TRUE ~ 0),
      rentburden_mod = case_when(rent_income_ratio >= 0.3 & rent_income_ratio <= 0.5 ~ 1,
                              is.na(rent_income_ratio) ~ NA,
                              TRUE ~ 0),
      rentburden_sev = case_when(rent_income_ratio > 0.5 ~ 1,
                              is.na(rent_income_ratio) ~ NA,
                              TRUE ~ 0),
      owncost_burden = case_when(owncost_income_ratio >= 0.3 ~ 1,
                               is.na(owncost_income_ratio) ~ NA,
                               TRUE ~ 0),
      owncostburden_mod = case_when(owncost_income_ratio >= 0.3 & owncost_income_ratio <= 0.5 ~ 1,
                                 is.na(owncost_income_ratio) ~ NA,
                                 TRUE ~ 0),
      owncostburden_sev = case_when(owncost_income_ratio > 0.5 ~ 1,
                                 is.na(owncost_income_ratio) ~ NA,
                                 TRUE ~ 0))
  }

renter_stats_fun <- function(data) {
  data %>% 
    group_by(year) %>% 
    summarize_groups(wbhaa_white | wbhaa_black | wbhaa_hispanic | wbhaa_AIAN | wbhaa_AAPI,
                     ownership_rate = weighted.mean(owner, w = hhwt, na.rm = TRUE),
                     renter_share = weighted.mean(renter, w = hhwt, na.rm = TRUE),
                     owncost_burden = weighted.mean(owncost_burden, w = hhwt, na.rm = TRUE),
                     owncostburden_mod = weighted.mean(owncostburden_mod, w = hhwt, na.rm = TRUE),
                     owncostburden_sev = weighted.mean(owncostburden_sev, w = hhwt, na.rm = TRUE),
                     rent_burden = weighted.mean(rent_burden, w = hhwt, na.rm = TRUE),
                     rentburden_mod = weighted.mean(rentburden_mod, w = hhwt, na.rm = TRUE),
                     rentburden_sev = weighted.mean(rentburden_sev, w = hhwt, na.rm = TRUE)) %>% 
  filter(group_value == 1) %>% select(-group_value)
}


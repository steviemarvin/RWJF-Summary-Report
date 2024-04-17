# labor force stats for households where only household head/spouse has children

### setting up baseline df ###

# df that has all household members
hhOnly_df <- asec_clean %>% 
  # keeping households where only the hh head has a child under 18
  right_join(filter(li_families, hh_type == "hhOnly"), by = c("year", "serial")) %>% 
  # joining indicator variable to indicate that an unmarried partner is present in the household
  left_join(partner_present, by = c("year", "serial")) %>% 
  # filtering out unrelated adults in the household and foster children (different legal status)
  filter(relate != 1115 & relate != 1241 & relate != 1242 & relate != 1260) %>% 
  mutate(# adjusting parent variables to only be household head
    not_married_parent_hh = if_else(relate == 101, not_married_parent_hh, 0),
    single_parent_hh = if_else(relate == 101, single_parent, 0), #marst = 6: never married/single
    married_parent_hh = if_else(relate == 101, married_parent, 0),  #marst = 1: married, spouse present; 2: married, spouse absent
    # adjusting race variable so that the household head's race dictates household race classification
    across(contains("wbhaa_"), ~if_else(relate == 101, .x, 0)))

### categorizing families by if there are related adults in the household or not
## df of each household # that has related adults to the hh head for descriptive stats
has_related_adult <- hhOnly_df %>% 
  filter(related_adult == 1) %>% 
  group_by(year) %>% 
  distinct(serial, .keep_all = FALSE) %>% ungroup()

## df that summarizes households by household head's marital status (only single or married, spouse present)
hh_marst <- hhOnly_df %>% 
  filter(relate == 101) %>% 
  group_by(year, serial) %>% 
  summarize(across(ends_with("_parent_hh") | starts_with("wbhaa_") | asecwth, ~max(.x))) %>% 
  ungroup() 

## splitting above df by household heads that have related adults in the household and those that do not 
parents_w_ra <- hh_marst %>% 
  inner_join(has_related_adult, by = c("year", "serial")) %>% 
  left_join(partner_present, by = c("year", "serial"))

parents_wo_ra <- hh_marst %>% 
  anti_join(has_related_adult, by = c("year", "serial")) %>% 
  left_join(partner_present, by = c("year", "serial"))


# calculating weighted sums of single and married parents with related adults (joining later to calculate shares across entire sample)
parents_ra_sum <- parents_w_ra %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_AIAN | wbhaa_AAPI | wbhaa_hispanic, 
                   n_sp_ra = sum(single_parent_hh * asecwth, na.rm = TRUE),
                   n_nmp_ra = sum(not_married_parent_hh * asecwth, na.rm = TRUE),
                   n_mp_ra = sum(married_parent_hh * asecwth, na.rm = TRUE),
                   n_p_pres_ra = sum(partner_present * asecwth, na.rm = TRUE)) %>% 
  filter(group_value == 1) %>% select(-group_value) 

# calculating weighted shares of parents with no related adults, then joining parents with related adults to create distribution
fam_structure_sum <- parents_wo_ra %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_AIAN | wbhaa_AAPI | wbhaa_hispanic, 
                   n_sp_nra = sum(single_parent_hh * asecwth, na.rm = TRUE),
                   n_nmp_nra = sum(not_married_parent_hh * asecwth, na.rm = TRUE),
                   n_mp_nra = sum(married_parent_hh * asecwth, na.rm = TRUE),
                   n_p_pres_nra = sum(partner_present * asecwth, na.rm = TRUE)) %>% 
  filter(group_value == 1) %>% select(-group_value) %>% 
  full_join(parents_ra_sum, by = c("year", "group_name")) %>% 
  select(year, group_name, contains("_ra") | contains("_nra")) %>% 
  mutate(sum = n_sp_ra + n_mp_ra + n_sp_nra + n_mp_nra + n_nmp_ra + n_nmp_nra,
         sum2 = n_nmp_ra + n_nmp_nra,
         # calculating shares of households by marital status
         across((contains("_ra") & !contains("pres"))| (contains("_nra") & !contains("pres")), ~ round(.x / sum, digits = 4), .names = "share.{.col}"),
         across(contains("pres"), ~ round(.x / sum2, digits = 4), .names = "share.{.col}")) %>% 
  select(year, group_name, contains("share.")) %>% 
  rename("Race" = group_name, 
         "Single parent, has related adults" = share.n_sp_ra, 
         "Married parent, has related adults" = share.n_mp_ra, 
         "Not married parent, has related adults" = share.n_nmp_ra,
         "Parter present, has related adults" = share.n_p_pres_ra,
         "Single parent, no related adults" = share.n_sp_nra,
         "Married parent, no related adults" = share.n_mp_nra,
         "Not married parent, no related adults" = share.n_nmp_nra,
         "Partner present, no related adults" = share.n_p_pres_nra) %>% 
  mutate(Race = sub("wbhaa_", "", Race))

# summarize 
all_adults <- hhOnly_df %>% 
  filter(age >= 18) %>% 
  mutate(
    adult = if_else(age >= 18 & (hh_head_sp == 1 | related_adult == 1 | relate == 1114), 1, 0),
    earner_adult = if_else(earner == 1 & adult == 1, 1, 0),
    # only keeping values assigned to household head since for primary and related these are the same, but for unmarried partner, values can differ
    across(offtotval | pov100 | pov50, ~if_else(relate == 101, .x, 0)),
    # earner stat of household head and spouse, variable needed to distinguish between household and family
    
    earner_p = case_when(earner == 1 & hh_head_sp == 1 ~ 1,
                         earner == 0 & hh_head_sp == 0 ~ 0,
                         TRUE ~ NA),
    nonearner_p = case_when(nonearner_adult == 1 & hh_head_sp == 1 ~ 1,
                            nonearner_adult == 0 & hh_head_sp == 1 ~ 0,
                            TRUE ~ NA),
    ft_p = if_else(hh_head_sp == 1, ft, NA),
    pt_p = if_else(hh_head_sp == 1, pt, NA)) %>% 
  group_by(year, serial) %>%
  summarize(across(contains("_hh") | contains("wbhaa_") | offtotval | asecwth | pov100 | pov50, ~max(.x)),
            across(contains("earner") | (contains("ft") & !contains("totval")) | contains("pt") | adult, ~sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  # at least one related adult or parent in household is an earner, working ft/pt
  mutate(across(contains("earner") | (contains("ft") & !contains("totval")) | contains("pt"), ~if_else(.x >= 1, 1, 0), .names = "at1_{.col}"),
         across(adult | earner, ~case_when(.x == 0 ~ 0,
                                           .x == 1 ~ 1,
                                           .x == 2 ~ 2,
                                           .x >= 3 ~ 3), .names = "num_{.col}"),
         all_earner_p = case_when(not_married_parent_hh == 1 & earner_p == 1 ~ 1,
                                  married_parent_hh == 1 & earner_p == 2 ~ 1,
                                  TRUE ~ 0),
         all_nonearner_p = case_when(not_married_parent_hh == 1 & nonearner_p == 1 ~ 1,
                                 married_parent_hh == 1 & nonearner_p == 2 ~ 1,
                                 TRUE ~ 0),
         at2_earner_adult = if_else(earner_adult >= 2, 1, 0)) %>% 
  dummy_cols(select_columns = c('num_earner', 'num_adult'), ignore_na = TRUE) %>% 
  left_join(partner_present, by = c("year", "serial")) 

### MAIN OUTPUT ###
# getting unweighted and weightedsample pop for families by race and marital status
uw_pop <- all_adults %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_AIAN | wbhaa_hispanic | wbhaa_AAPI,
                   mp = sum(married_parent_hh),
                   nmp = sum(not_married_parent_hh),
                   w_mp = round(sum(married_parent_hh * asecwth), 0),
                   w_nmp = round(sum(not_married_parent_hh * asecwth), 0)) %>% 
  filter(group_value == 1) %>% select(-group_value) %>% 
  mutate(group_name = sub("wbhaa_", "", group_name))%>% 
  pivot_wider(id_cols = "year", names_from = "group_name", values_from = c("mp", "nmp", "w_mp", "w_nmp"), names_glue = "{group_name}.{.value}")

share_sp <- all_adults %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_AIAN | wbhaa_hispanic | wbhaa_AAPI,
                   sp = sum(single_parent_hh),
                   nmp = sum(not_married_parent_hh),
                   w_sp = round(sum(single_parent_hh * asecwth), 0),
                   w_nmp = round(sum(not_married_parent_hh * asecwth), 0)) %>% 
  filter(group_value == 1) %>% select(-group_value) %>% 
  mutate(group_name = sub("wbhaa_", "", group_name))%>% 
  pivot_wider(id_cols = "year", names_from = "group_name", values_from = c("sp", "nmp", "w_sp", "w_nmp"), names_glue = "{group_name}.{.value}")


## labor force stats
# varlists for mapping
wbhaa_list <- c("wbhaa_white", "wbhaa_black", "wbhaa_hispanic", "wbhaa_AIAN", "wbhaa_AAPI")
  
vector_pairs <- crossing(x = c("married_parent_hh", "not_married_parent_hh"), y = wbhaa_list)
  
# labor force stats output function
lf_stats_fun <- function(data, hh_marst, wbhaa){
  data %>% 
    filter({{hh_marst}} == 1, {{wbhaa}} == 1) %>% 
    group_by(year) %>% 
    summarize(adult = weighted.mean(adult, w = asecwth, na.rm = TRUE),
              earner = weighted.mean(earner, w = asecwth, na.rm = TRUE),
              emp = weighted.mean(emp, w = asecwth, na.rm = TRUE),
              at2_workadults = weighted.mean(at2_workadults, w = asecwth, na.rm = TRUE),
              at1_emp = weighted.mean(at1_emp, w = asecwth, na.rm = TRUE),
              at1_ft = weighted.mean(at1_ft, w = asecwth, na.rm = TRUE),
              at1_pt = weighted.mean(at1_pt, w = asecwth, na.rm = TRUE),
              at1_unemp = weighted.mean(at1_unemp, w = asecwth, na.rm = TRUE),
              at1_nilf = weighted.mean(at1_nilf, w = asecwth, na.rm = TRUE),
              all_emp_p = weighted.mean(all_emp_p, w = asecwth, na.rm = TRUE),
              at1_emp_p = weighted.mean(at1_emp_p, w = asecwth, na.rm = TRUE),
              at1_unemp_p = weighted.mean(at1_unemp_p, w = asecwth, na.rm = TRUE),
              at1_nilf_p = weighted.mean(at1_nilf_p, w = asecwth, na.rm = TRUE)) %>% 
    mutate(across(adult | earner | emp | contains("at") | all_emp_p, ~ round(.x, 2))) %>% 
    rename(
           "Average number of adults in household" = adult,
           "Average number of earners in household" = earner, "Average number of employed adults in household" = emp,
           "At least one adult employed in household" = at1_emp, "At least two adults employed in household" = at2_workadults,
           "At least one adult working full-time in household" = at1_ft, "At least one adult working part-time in household" = at1_pt,
           "At least one adult unemployed in household" = at1_unemp, "At least one adult not in labor force in househld" = at1_nilf,
           "All parents employed" = all_emp_p, "At least one parent employed" = at1_emp_p, "At least one parent unemployed" = at1_unemp_p,
           "At least one parent not in labor force" = at1_nilf_p)
  
}

test <- map2(.x = vector_pairs$x, .y = vector_pairs$y, .f = lf_stats_fun, data = all_adults)

lf_stats_all <-  all_adults %>% 
  filter(married_parent_hh == 1) %>% 
  group_by(year) %>% 
  summarize_groups(wbhaa_white | wbhaa_black | wbhaa_hispanic | wbhaa_AIAN | wbhaa_AAPI,
                   num_adult_1 = weighted.mean(num_adult_1, w = asecwth, na.rm = TRUE),
                   num_adult_2 = weighted.mean(num_adult_2, w = asecwth, na.rm = TRUE),
                   num_adult_3 = weighted.mean(num_adult_3, w = asecwth, na.rm = TRUE),
                   num_earner_0 = weighted.mean(num_earner_0, w = asecwth, na.rm = TRUE),
                   num_earner_1 = weighted.mean(num_earner_1, w = asecwth, na.rm = TRUE),
                   num_earner_2 = weighted.mean(num_earner_2, w = asecwth, na.rm = TRUE),
                   num_earner_3 = weighted.mean(num_earner_3, w = asecwth, na.rm = TRUE),
                   at2_earner_adult = weighted.mean(at2_earner_adult, w = asecwth, na.rm = TRUE),
                   at1_pt = weighted.mean(at1_pt, w = asecwth, na.rm = TRUE),
                   
                   
                   
                   adult = weighted.mean(adult, w = asecwth, na.rm = TRUE),
                   earner = weighted.mean(earner, w = asecwth, na.rm = TRUE),
                   emp = weighted.mean(emp, w = asecwth, na.rm = TRUE),
                   at2_workadults = weighted.mean(at2_workadults, w = asecwth, na.rm = TRUE),
                   at1_emp = weighted.mean(at1_emp, w = asecwth, na.rm = TRUE),
                   at1_ft = weighted.mean(at1_ft, w = asecwth, na.rm = TRUE),
                   at1_pt = weighted.mean(at1_pt, w = asecwth, na.rm = TRUE),
                   at1_unemp = weighted.mean(at1_unemp, w = asecwth, na.rm = TRUE),
                   at1_nilf = weighted.mean(at1_nilf, w = asecwth, na.rm = TRUE),
                   all_emp_p = weighted.mean(all_emp_p, w = asecwth, na.rm = TRUE),
                   at1_emp_p = weighted.mean(at1_emp_p, w = asecwth, na.rm = TRUE),
                   at1_unemp_p = weighted.mean(at1_unemp_p, w = asecwth, na.rm = TRUE),
                   at1_nilf_p = weighted.mean(at1_nilf_p, w = asecwth, na.rm = TRUE)) %>% 
  mutate(across(adult | earner | emp | contains("at") | all_emp_p, ~ round(.x, 3)),
         group_name = sub("wbhaa_", "", group_name)) %>% 
  filter(group_value == 1) %>% 
  rename(Race = group_name,
         "Average number of adults in household" = adult,
         "Average number of earners in household" = earner, "Average number of employed adults in household" = emp,
         "At least one adult employed in household" = at1_emp, "At least two adults employed in household" = at2_workadults,
         "At least one adult working full-time in household" = at1_ft, "At least one adult working part-time in household" = at1_pt,
         "At least one adult unemployed in household" = at1_unemp, "At least one adult not in labor force in househld" = at1_nilf,
         "All parents employed" = all_emp_p, "At least one parent employed" = at1_emp_p, "At least one parent unemployed" = at1_unemp_p,
         "At least one parent not in labor force" = at1_nilf_p) %>% select(-group_value)


## family income and share of lowincome families that are 100% FPL and 50% FPL ##

incpov_sum_fun <- function(data, varname){
  data %>% 
    group_by(year) %>% 
    summarize_groups(wbhaa_white | wbhaa_black | wbhaa_hispanic| wbhaa_AIAN  | wbhaa_AAPI,
                     varname = round(weighted.mean({{varname}}, w = asecwth, na.rm = TRUE), 4)) %>% 
    filter(group_value == 1) %>% 
    mutate(group_name = sub("wbhaa_", "", group_name)) %>% 
    pivot_wider(id_cols = "year", names_from = "group_name", values_from = "varname") 
}

family_income_notmarried <- incpov_sum_fun(data = filter(all_adults, not_married_parent_hh == 1), varname = offtotval) %>% 
  left_join(cpi_data, by = "year") %>% 
  mutate(across(!matches("year"), ~ round(.x * cpi_u_rs / cpi_base, 0))) %>% 
  select(-cpi_u_rs)

family_income_married <- incpov_sum_fun(data = filter(all_adults, married_parent_hh == 1), varname = offtotval) %>% 
    left_join(cpi_data, by = "year") %>% 
    mutate(across(!matches("year"), ~ round(.x * cpi_u_rs / cpi_base, 0))) %>% 
    select(-cpi_u_rs)

pov100_stat <- incpov_sum_fun(data = all_adults, varname = pov100)

pov50_stat <- incpov_sum_fun(data = all_adults, varname = pov50)


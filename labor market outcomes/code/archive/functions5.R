# all functions used in RWJF Summary Report data table 5


# CPS March Data family statistics by race and ethnicity
family_stats_fun <- function(x) {
  asec_family_data %>% 
    filter({{ x }} == 1) %>% 
    mutate(fam_unemp_all = ifelse(fam_all_unemp == 2, 1, 0)) %>% 
    group_by(year) %>% 
    summarize(
      multi = weighted.mean(fam_multi, w = wgt),
      lowincome = weighted.mean(fam_lowincome, w = wgt),
      emp_1 = weighted.mean(fam_emp, w = wgt),
      unemp_1 = weighted.mean(fam_unemp, w = wgt),
      ft_1 = weighted.mean(fam_ft, w = wgt, na.rm = TRUE),
      pt_1 = weighted.mean(fam_ft, w = wgt, na.rm = TRUE),
      unemp_all = weighted.mean(fam_unemp_all, w = wgt, na.rm = TRUE),
      nilf_1 = weighted.mean(fam_nilf, w = wgt))
}


# CPS March Data
family_li_stats_fun <- function(x) {
  asec_family_data %>% 
    filter({{ x }} == 1, lowincome == 1) %>% 
    mutate(fam_unemp_all = ifelse(fam_all_unemp == 2, 1, 0)) %>% 
    group_by(year) %>% 
    summarize(
      multi = weighted.mean(fam_multi, w = wgt),
      lowincome = weighted.mean(fam_lowincome, w = wgt),
      emp_1 = weighted.mean(fam_emp, w = wgt),
      unemp_1 = weighted.mean(fam_unemp, w = wgt),
      ft_1 = weighted.mean(fam_ft, w = wgt, na.rm = TRUE),
      pt_1 = weighted.mean(fam_ft, w = wgt, na.rm = TRUE),
      unemp_all = weighted.mean(fam_unemp_all, w = wgt, na.rm = TRUE),
      nilf_1 = weighted.mean(fam_nilf, w = wgt))
}



#------------------------
# BLS Family Labor Outcomes Table 5

female18_stats_fun <- function(r) {
  basic_data %>% 
    filter(female == "Female", raceorig == r) %>% 
    group_by(year) %>%
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              pt = sum(part_time * wgt, na.rm = TRUE),
              ft = sum(full_time * wgt, na.rm = TRUE),
              pt_share = pt / emp,
              ft_share = ft / emp) %>% 
    select(year, civpop, lfpr, urate, epop)
}



female18_married_fun <- function(r, m){
  basic_data %>% 
    filter(female == "Female", married == m, raceorig == r) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}



female6_stats_fun <- function(r) {
  basic_data %>% 
    filter(female == "Female", raceorig == r, (agechild != 3 & agechild != 4 & agechild != 10)) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}



female6_married_fun <- function(r, m){
  basic_data %>% 
    filter(female == "Female", (agechild != 3 & agechild != 4 & agechild != 10), married == m, raceorig == r) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}


male18_stats_fun <- function(r) {
  basic_data %>% 
    filter(female == "Male", raceorig == r) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}



male18_married_fun <- function(r, m){
  basic_data %>% 
    filter(female == "Male", married == m, raceorig == r) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}



#### table 5b u6 = employment characteristics for men who have own children under 6 ####
male6_stats_fun <- function(r) {
  basic_data %>% 
    filter(female == "Male", (agechild != 3 & agechild != 4 & agechild != 10), raceorig == r) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}



male6_married_fun <- function(r, m){
  basic_data %>% 
    filter(female == "Male", (agechild != 3 & agechild != 4 & agechild != 10), married == m, raceorig == r) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}


#### Hispanic men and women stats ####
## Hispanic men and women with children under 18
hispan18_stats_fun <- function(f) {
  basic_data %>% 
    filter(female == f, hispanic == "Hispanic") %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}



## Hispanic men and women, married, with children under 18
hispan18_married_fun <- function(f, m) {
  basic_data %>% 
    filter(female == f, hispanic == "Hispanic", married == m) %>% 
    group_by(year) %>%
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,              
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}


## Hispanic men and women with children under 6
hispan6_stats_fun <- function(f) {
  basic_data %>% 
    filter(female == f, (agechild != 3 & agechild != 4 & agechild != 10), hispanic == "Hispanic") %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}


hispan6_married_fun <- function(f, m) {
  basic_data %>% 
    filter(female == f, hispanic == "Hispanic", married == m) %>% 
    group_by(year) %>% 
    summarize(civpop = sum(wgt, na.rm = TRUE),
              nilf_agg = sum(nilf * wgt),
              laborforce = civpop - nilf_agg,
              lfpr = laborforce / civpop,
              emprate = weighted.mean(emp, w = wgt, na.rm = TRUE),
              emp = sum(emp * wgt, na.rm = TRUE),
              unemp = sum(unemp * wgt, na.rm = TRUE),
              urate = unemp / laborforce,
              epop = emp / civpop,
              ptshare = sum(part_time * wgt, na.rm = TRUE)) %>% 
    select(year, civpop, lfpr, urate, epop, ptshare)
}

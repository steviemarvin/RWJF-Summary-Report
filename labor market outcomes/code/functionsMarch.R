# functions used in March CPS Labor outcomes.R

all_families_fun <- function(u){
  asec_family_data %>% 
    filter({{ u }} == 1) %>% 
    group_by(year) %>% 
    summarize(
      share_black = weighted.mean(fam_black, w = wgt),
      share_white = weighted.mean(fam_white, w = wgt),
      share_AAPI = weighted.mean(fam_AAPI, w = wgt),
      share_AIAN = weighted.mean(fam_AIAN, w = wgt),
      share_hispan = weighted.mean(fam_hispan, w = wgt),
      share_foreign = weighted.mean(fam_foreign, w = wgt))
}

all_families_li_fun <- function(u){
  asec_family_data %>% 
    filter({{ u }} == 1, fam_lowincome == 1) %>% 
    group_by(year) %>% 
    summarize(
      share_black = weighted.mean(fam_black, w = wgt),
      share_white = weighted.mean(fam_white, w = wgt),
      share_AAPI = weighted.mean(fam_AAPI, w = wgt),
      share_AIAN = weighted.mean(fam_AIAN, w = wgt),
      share_hispan = weighted.mean(fam_hispan, w = wgt),
      share_foreign = weighted.mean(fam_foreign, w = wgt))
}

family_stats_fun <- function(x, y) {
  asec_family_data %>% 
    filter({{ x }} == 1, {{ y }} == 1) %>% 
    group_by(year) %>% 
    arrange(fam_income, by_group = TRUE) %>% 
    summarize(
      lowincome = weighted.mean(fam_lowincome, w = wgt),
      median_famincome = median(fam_income),
      median_real_famincome = median(real_fam_income),
      mean_famincome = weighted.mean(fam_income, w = wgt),
      emp_1 = weighted.mean(fam_emp, w = wgt),
      unemp_1 = weighted.mean(fam_unemp, w = wgt),
      ft_1 = weighted.mean(fam_ft, w = wgt, na.rm = TRUE),
      pt_1 = weighted.mean(fam_pt, w = wgt, na.rm = TRUE),
      nilf_1 = weighted.mean(fam_nilf, w = wgt))
}

family_li_stats_fun <- function(x, y) {
  asec_family_data %>% 
    filter({{ x }} == 1, fam_lowincome == 1, {{ y }} == 1) %>% 
    group_by(year) %>% 
    arrange(fam_income, by_group = TRUE) %>% 
    summarize(
      median_famincome = median(fam_income),
      median_real_famincome = median(real_fam_income),
      mean_famincome = weighted.mean(fam_income, w = wgt),
      emp_1 = weighted.mean(fam_emp, w = wgt),
      unemp_1 = weighted.mean(fam_unemp, w = wgt),
      ft_1 = weighted.mean(fam_ft, w = wgt, na.rm = TRUE),
      pt_1 = weighted.mean(fam_pt, w = wgt, na.rm = TRUE),
      nilf_1 = weighted.mean(fam_nilf, w = wgt))
}
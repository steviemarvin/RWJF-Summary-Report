
renter_stats_fun <- function(race_ethnicity, child_group){
  acs_renter_data %>% 
    filter({{ race_ethnicity }} == 1, {{ child_group }} == 1) %>% 
    group_by(year, lowincome) %>% 
    summarize(
      rburden_mod = weighted.mean(rburden_mod, w = wgt, na.rm = TRUE),
      rburden_sev = weighted.mean(rburden_sev, w = wgt, na.rm = TRUE))
}

transit_stats_fun <- function(race_ethnicity, child_group){
  acs_transit_data %>% 
    filter({{ race_ethnicity }} == 1, {{ child_group }} == 1) %>% 
    group_by(year, lowincome) %>% 
    summarize(
      has_car = weighted.mean(vehicle_avail, w = wgt, na.rm = TRUE),
      av_commute = weighted.mean(trantime_av, w = wgt, na.rm = TRUE),
      public_transit = weighted.mean(public_transit, w = wgt, na.rm = TRUE)
    )
}
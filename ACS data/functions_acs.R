
renter_stats_fun <- function(race_ethnicity, child_group){
  acs_housing_data %>% 
    filter({{ race_ethnicity }} == 1, {{ child_group }} == 1) %>% 
    group_by(year) %>% 
    summarize(
      ownership_rate = weighted.mean(owner, w = wgt, na.rm = TRUE),
      housing_burden = weighted.mean(housing_burden, w = wgt, na.rm = TRUE),
      burden_mod = weighted.mean(burden_mod, w = wgt, na.rm = TRUE),
      burden_sev = weighted.mean(burden_sev, w = wgt, na.rm = TRUE),
      rent_burden = weighted.mean(rent_burden, w = wgt, na.rm = TRUE),
      rburden_mod = weighted.mean(rburden_mod, w = wgt, na.rm = TRUE),
      rburden_sev = weighted.mean(rburden_sev, w = wgt, na.rm = TRUE))
  
}

transit_stats_fun <- function(race_ethnicity, child_group){
  acs_transit_data %>% 
    filter({{ race_ethnicity }} == 1, {{ child_group }} == 1) %>% 
    group_by(year) %>% 
    summarize(
      has_car = weighted.mean(vehicle_avail, w = wgt, na.rm = TRUE),
      av_commute = weighted.mean(trantime_av, w = wgt, na.rm = TRUE),
      public_transit = weighted.mean(public_transit, w = wgt, na.rm = TRUE)
    )
}

sheets_fun <- function(data, wb, s, format = NULL) {
  
  
  # add worksheet and map data
  addWorksheet(wb, sheetName = paste0(s))
  writeData(wb, sheet = paste0(s), x = data)
  
  # assign column widths and data formatting
  setColWidths(wb, sheet = paste0(s), cols = 2:ncol(data), widths = 15)
  
  # grab col length which will help exclude shortcodes from format
  #note: make sure shortcode is always last column
  if (any(colnames(data) %in% c("shortcodes"))) {
    col_number <- ncol(data) - 1
  } else {
    col_number <- ncol(data)
  }
  
  
  # format based on the value of format
  if (!is.null(format)) {
    lapply(format, function(f) {
      switch(f,
             "PERCENTAGE" = addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '0.0%'), cols = 2:col_number, rows = 2:(nrow(data) + 1), gridExpand = TRUE),
             "NUMBER" = addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '#,#0.0'), cols = 2:length(data), rows = 2:(nrow(data) + 1), gridExpand = TRUE)
      )
    })
  }
  
  return(data)
  
}
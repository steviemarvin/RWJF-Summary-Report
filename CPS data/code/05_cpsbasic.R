# cleaning up cps_basic from hwething_gates

basic_clean <- select(cps_feather, year, month, finalwgt, basicwgt, age, wbho, lfstat, prfamrel,
                      emp, unemp, agechild, child, childsize, ownchild, pov200) %>% 
  filter(pov200 == 1, wbho != 4, age >= 18) %>% 
  mutate(primeage = if_else(age >= 25 & age <= 54, 1, 0),
         yes_child = if_else(ownchild >= 1 & age >= 18 & agechild > 0, 1, 0),
         no_child = if_else(ownchild == 0 | agechild == 0, 1, 0),
         wbho = as.character(as_factor(wbho)))
  
sheets_fun <- function(data, wb, s) {
  
  # add worksheet and map data
  addWorksheet(wb, sheetName = paste0(s))
  writeData(wb, sheet = paste0(s), x = data)
  
}

summary_fun <- function(data, varname){
  data %>% 
    group_by(year, month, wbho) %>% 
    summarize(varname2 = round(weighted.mean({{varname}}, w = basicwgt, na.rm = TRUE), 4)) %>% ungroup() %>% 
    pivot_wider(id_cols = c("year", "month"), names_from = wbho, values_from = varname2) %>% 
    mutate(across(Black | Hispanic, ~round(.x / White, 4), .names = "{.col}White.ratio"),
           month = as.character(as_factor(labelled(month, c("January" = 1, "February" = 2, "March" = 3, "April" = 4,
                                                            "May" = 5, "June" = 6, "July" = 7, "August" = 8,
                                                            "September" = 9, "October" = 10, "November" = 11, "December" = 12)))),
           year = as.character(year),
           date = paste(month, year, sep = "-")) %>% 
    select(-year, -month) %>% select(date, everything())
  
}

RWJF_wb <- createWorkbook()

pa_epop_all <- filter(basic_clean, primeage == 1) %>% 
  summary_fun(varname = emp) 
addWorksheet(RWJF_wb, sheetName = "pa_epop_all")
writeData(RWJF_wb, sheet = "pa_epop_all", x = pa_epop_all)

unemp_all <- filter(basic_clean, lfstat != 3 & !is.na(lfstat)) %>% 
  summary_fun(varname = unemp)
addWorksheet(RWJF_wb, sheetName = "unemp_all")
writeData(RWJF_wb, sheet = "unemp_all", x = unemp_all)

pa_epop_wchild <- filter(basic_clean, primeage == 1, yes_child == 1) %>% 
  summary_fun(varname = emp) 
addWorksheet(RWJF_wb, sheetName = "pa_epop_wchild")
writeData(RWJF_wb, sheet = "pa_epop_wchild", x = pa_epop_wchild)

unemp_wchild <- filter(basic_clean, lfstat != 3 & !is.na(lfstat), yes_child == 1) %>% 
  summary_fun(varname = unemp) 
addWorksheet(RWJF_wb, sheetName = "unemp_wchild")
writeData(RWJF_wb, sheet = "unemp_wchild", x = unemp_wchild)
  
pa_epop_wochild <- filter(basic_clean, primeage == 1, yes_child == 0) %>% 
  summary_fun(varname = emp) 
addWorksheet(RWJF_wb, sheetName = "pa_epop_wochild")
writeData(RWJF_wb, sheet = "pa_epop_wochild", x = pa_epop_wochild)

unemp_wochild <- filter(basic_clean, lfstat != 3 & !is.na(lfstat), yes_child == 0) %>% 
  summary_fun(varname = unemp)
addWorksheet(RWJF_wb, sheetName = "unemp_wochild")
writeData(RWJF_wb, sheet = "unemp_wochild", x = unemp_wochild)


saveWorkbook(RWJF_wb, here("output","cps_basic_output.xlsx"), overwrite = TRUE)




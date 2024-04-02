## master 
## CPS ASEC DATA 
library(ipumsr)
library(here)
library(tidyverse)
library(epiextractr)
library(stats)
library(epidatatools) # summarize_groups
library(data.table)
library(readxl)
library(haven)
library(janitor)
library(xlsx)
library(openxlsx)
library(realtalk) #cpi_u_rs_annual
library(fastDummies) # create indicator variables 


## load data here
ddi <-  read_ipums_ddi(here("data/cps_00013.xml"))
asec_raw_data <- read_ipums_micro(ddi, verbose = FALSE)
names(asec_raw_data) <- tolower(names(asec_raw_data))


##CPI DATA for inflation adjustment for median/av income
cpi_data <- cpi_u_rs_annual

# grabbing base value year 2021
cpi_base <- pull(filter(cpi_u_rs_annual, year == 2022))

sheets_fun <- function(data, wb, s, format = NULL) {
  
  
  # add worksheet and map data
  addWorksheet(wb, sheetName = paste0(s))
  writeData(wb, sheet = paste0(s), x = data)
  
  # assign column widths and data formatting
  setColWidths(wb, sheet = paste0(s), cols = 2:ncol(data), widths = 15)
  
  # check if date is a column and format the first column as date
  if (any(colnames(data) %in% c("date"))) {
    # format date
    options("openxlsx.dateFormat" = "mmm-yyyy")
    addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = "DATE"), rows = 2:nrow(data)+1, cols = 1)
  }
  
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
             "NUMBER" = addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '#,#0'), cols = 2:length(data), rows = 2:(nrow(data) + 1), gridExpand = TRUE),
             "DOLLAR" = addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '$,$0'), cols = 2:length(data), rows = 2:(nrow(data) + 1), gridExpand = TRUE))
    })
  }
  
  return(data)
  
}

sheets_wbhaa_fun <- function(data, wb, s, wbhaa, format = NULL) {
  
  if (identical(wbhaa, "wbhaa_white")){
    start_row <- 2
  } else if (identical(wbhaa, "wbhaa_black")){
    start_row <- nrow(data) + 5
  } else if (identical(wbhaa, "wbhaa_hispanic")){
    start_row <- 2(nrow(data)) + 4
  } else if (identical(wbhaa, "wbhaa_AIAN")){
    start_row <- 3(nrow(data)) + 5
  } else {
    start_row <- 4(nrow(data)) + 6
  }
  
  
  # add worksheet and map data
  addWorksheet(wb, sheetName = paste0(s))
  writeData(wb, sheet = paste0(s), x = data, startCol = 1, startRow = start_row)
  
  # assign column widths and data formatting
  setColWidths(wb, sheet = paste0(s), cols = 2:ncol(data), widths = 15)
  
  # check if date is a column and format the first column as date
  if (any(colnames(data) %in% c("date"))) {
    # format date
    options("openxlsx.dateFormat" = "mmm-yyyy")
    addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = "DATE"), rows = 2:nrow(data)+1, cols = 1)
  }
  
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
             "PERCENTAGE" = addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '0.0%'), cols = 2:length(data), rows = (start_row + 1):(start_row + nrow(data) + 1), gridExpand = TRUE),
             "NUMBER" = addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '#,#0'), cols = 2:length(data), rows = (start_row + 1):(start_row + nrow(data) + 1), gridExpand = TRUE),
             "DOLLAR" = addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '$,$0'), cols = 2:length(data), rows = (start_row + 1):(start_row + nrow(data) + 1), gridExpand = TRUE))
    })
  }
  
  return(data)
  
}

## sourcing code
# cleaning the asec data, making new race and family variables, cleaning labor force variables
source(here("code/01_cleaning.R"), echo = TRUE)
# family configurations and low income families df creations
source(here("code/02_universe.R"), echo = TRUE)
#descriptive demographic stats for parents, children and families
source(here("03_demographics.R"), echo = TRUE)
# labor market outcomes for families by race and ethnicity and income and poverty 
source(here("code/04_hhOnly_lfstat.R"), echo = TRUE)


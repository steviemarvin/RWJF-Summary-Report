## master 

## CPS ASEC DATA 
library(ipumsr)
library(here)
library(tidyverse)
library(epiextractr)
library(stats)
library(epidatatools)
library(data.table)
library(readxl)
library(haven)
library(janitor)
library(xlsx)
library(openxlsx)
library(realtalk) #cpi_u_rs_annual

## load data here
ddi <-  read_ipums_ddi(here("suppdata/cps_00005.xml"))
asec_raw_data <- read_ipums_micro(ddi, verbose = FALSE)
names(asec_raw_data) <- tolower(names(asec_raw_data))


##CPI DATA for inflation adjustment for median/av income
cpi_data <- cpi_u_rs_annual

# grabbing base value year 2021
cpi_base <- pull(filter(cpi_u_rs_annual, year == 2021))

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
             "NUMBER" = addStyle(wb, sheet = paste0(s), style = createStyle(numFmt = '#,#0.0'), cols = 2:length(data), rows = 2:(nrow(data) + 1), gridExpand = TRUE)
      )
    })
  }
  
  return(data)
  
}

# sourcing code -- versions a and b distinguished by which parents are recorded, in b, 
  # child/racial status of only household head (and spouse) are the determining factors of family configuration
  # whereas in a, child/racial status of all adults in the household are the determining factors
## cleaning the asec data to prep for family data 
source(here("code/01_cleaning.R"), echo = TRUE)
source(here("code/02a_demographics.R"), echo = TRUE)
source(here("code/03a_family_structure.R"), echo = TRUE)


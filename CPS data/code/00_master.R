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
ddi <-  read_ipums_ddi(here("data/cps_00016.xml"))
asec_raw_data <- read_ipums_micro(ddi, verbose = FALSE)
names(asec_raw_data) <- tolower(names(asec_raw_data))


##CPI DATA for inflation adjustment for median/av income
cpi_data <- cpi_u_rs_annual

# grabbing base value year 2021
cpi_base <- pull(filter(cpi_u_rs_annual, year == 2022))

## sourcing code
# cleaning the asec data, making new race and family variables, cleaning labor force variables
source(here("code/01_cleaning.R"), echo = TRUE)
# family configurations and low income families df creations
source(here("code/02_universe.R"), echo = TRUE)
#descriptive demographic stats for parents, children and families
source(here("code/03_demographics.R"), echo = TRUE)
# labor market outcomes for families by race and ethnicity and income and poverty 
source(here("code/04_hhOnly_lfstat.R"), echo = TRUE)


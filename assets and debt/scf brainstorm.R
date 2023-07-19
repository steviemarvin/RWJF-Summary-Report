library(tidyverse)
library(stats)
library(haven)
library(readxl)
library(here)


## CPI DATA for inflation adjustment
# uploading/cleaning cpi data
cpi_data = read_excel("/projects/smarvin/data_library_annualwages/data_library_annualwages/r-cpi-u-rs-allitems.xlsx", 
                      skip = 5) %>% 
  rename(year = YEAR, cpiurs = AVG) %>% 
  select(year, cpiurs) %>% 
  filter(!is.na(cpiurs))

# grabbing base value year 2016 and 1989
cpi_base_value = cpi_data %>% 
  filter(year == 2016) %>% 
  pull(cpiurs)


scf_raw <- read_csv(here("data/scf_data.csv"))
names(scf_raw) <- tolower(names(scf_raw))


scf_clean <- scf_raw %>% 
  filter(year >= 2007, famstruct == 1 | famstruct == 4, race != 0) %>% 
  mutate(debt2inc = na_if(debt2inc, 10),
         race = labelled(race, c("White not hispanic" = 1,
                                 "Black not hispanic" = 2, 
                                 "Hispanic" = 3,
                                 "Other" = 5)),
         housecl = ifelse(housecl == 1, 1, 0),
         married = ifelse(married == 1, 1, 0),
         expenshilo = labelled(expenshilo, c("unusually high" = 1, 
                                             "unusually low" = 2,
                                             "normal" = 3)),
         laborforce = lf, across(race | expenshilo , ~as.character(as_factor(.x))),
         year = round(as.numeric(year), digits = 0),
         wt = wgt / 12) %>% 
  relocate(year) %>% 
  filter(year == 2007 | year == 2010 | year == 2013 | year == 2016 | year == 2019)

scf_test <- scf_raw %>% 
  filter(year >= 2007) %>% 
  mutate(debt2inc = na_if(debt2inc, 10),
         race = labelled(race, c("White not hispanic" = 1,
                                 "Black not hispanic" = 2, 
                                 "Hispanic" = 3,
                                 "Other" = 5)),
         housecl = ifelse(housecl == 1, 1, 0),
         married = ifelse(married == 1, 1, 0),
         expenshilo = labelled(expenshilo, c("unusually high" = 1, 
                                             "unusually low" = 2,
                                             "normal" = 3)),
         laborforce = lf, across(race | expenshilo , ~as.character(as_factor(.x))),
         year = round(as.numeric(year), digits = 0)) %>% 
  relocate(year) %>% 
  filter(year == 2007 | year == 2010 | year == 2013 | year == 2016 | year == 2019)


av_test <- scf_clean %>% 
  group_by(year, race) %>% 
  summarize(
    av_housecl = weighted.mean(housecl, w = wgt),
    av_hdebt = weighted.mean(hdebt, w = wgt),
    av_lf = weighted.mean(laborforce, w = wgt),
    av_debt = weighted.mean(debt, w = wgt),
    av_savres1 = weighted.mean(savres1, w = wgt),
    av_wageinc = weighted.mean(wageinc, w = wgt, na.rm = TRUE),
    av_pirtotal = weighted.mean(pirtotal, w = wgt),
    av_debt2inc = weighted.mean(debt2inc, w = wgt, na.rm = TRUE),
    av_levratio = weighted.mean(levratio, w = wgt),
    av_income = weighted.mean(income, w = wgt)
  )

test <- scf_clean %>% 
  group_by(year) %>%
  arrange(debt, by_group = TRUE) %>% 
  summarize(
    av_debt = weighted.mean(debt, w = wgt),
    med_debt = median(debt, na.rm = TRUE)
  )

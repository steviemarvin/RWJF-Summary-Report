library(ipumsr)
library(here)
library(tidyverse)
library(epiextractr)
library(stats)
library(epidatatools)
library(haven)
library(janitor)


ddi <-  read_ipums_ddi(here("data/usa_00001.xml"))
acs_raw_data <- read_ipums_micro(ddi, verbose = FALSE)
names(acs_raw_data) <- tolower(names(acs_raw_data))

acs_clean_data <- acs_raw_data %>% 
  filter(year >= 2007, nchild > 0, age >= 16) %>% 
  mutate(
    inctot = na_if(inctot, 999999999),
    trantime = na_if(trantime, 0),
    new_race = case_when(race == 1 & hispan == 0 ~ 1,
                         race == 2 & hispan == 0 ~ 2,
                         race == 3 & hispan == 0 ~ 3,
                         race >= 4 & race <= 6 & hispan == 0 ~ 4,
                         hispan > 0 & hispan != 9 ~ 5,
                         TRUE ~ NA),
    new_race = labelled(new_race, c("White" = 1, "Black" = 2, "AIAN" = 3, "AAPI" = 4, "Hispanic" = 5)),
    u18 = case_when(eldch < 18 ~ 1,
                    yngch < 18 ~ 1,
                    yngch >= 18 ~ 0),
    u6 = if_else(yngch < 6 | eldch < 6, 1, 0),
    hhwgt = hhwt / 12,
    pwgt = perwt / 12, 
    lowincome = if_else(poverty < 200, 1, 0),
    public_transit = if_else(tranwork >= 31 & tranwork <= 39, 1, 0),
    car_transit = if_else(tranwork == 10, 1, 0),
    vehicle_avail = case_when(vehicles > 0 & vehicles < 9 ~ 1,
                              vehicles == 9 ~ 0,
                              TRUE ~ NA),
    white = if_else(new_race == 1, 1, 0),
    black = if_else(new_race == 2, 1, 0),
    aian = if_else(new_race == 3, 1, 0),
    aapi = if_else(new_race == 4, 1, 0),
    hispan = if_else(new_race == 5, 1, 0)) %>% 
  filter(u18 == 1 | u6 == 1, !is.na(new_race)) 

acs_renter_data <- acs_clean_data %>% 
  filter(ownershp == 2) %>% 
  mutate(rent_income_ratio = rentgrs / (hhincome/12)) %>% 
  group_by(year, serial) %>% 
  summarize(
    u18 = min(u18),
    u6 = min(u6),
    lowincome = min(lowincome),
    wgt = min(hhwgt),
    rent_income_ratio = min(rent_income_ratio),
    white = min(white),
    black = min(black),
    aian = min(aian),
    aapi = min(aapi),
    hispan = min(hispan)) %>% 
  mutate(
    rburden_mod = if_else(rent_income_ratio >= 0.3 & rent_income_ratio <= 0.5, 1, 0),
    rburden_sev = if_else(rent_income_ratio > 0.5, 1, 0))

acs_transit_data <- acs_clean_data %>% 
  group_by(year, serial) %>% 
  summarize(
    u18 = min(u18),
    u6 = min(u6),
    lowincome = min(lowincome),
    wgt = min(hhwgt),
    white = min(white),
    black = min(black),
    aian = min(aian),
    aapi = min(aapi),
    hispan = min(hispan),
    car_transit = min(car_transit),
    public_transit = min(public_transit),
    vehicle_avail = min(vehicle_avail))

transit_time <- acs_clean_data %>% 
  group_by(year, serial) %>% 
  summarize(
    trantime_av = weighted.mean(trantime, w = pwgt, na.rm = TRUE)
  ) %>% 
  select(year, serial, trantime_av) %>% 
  full_join(acs_transit_data, by = c("year", "serial"))


source(here("functions_acs.R"), echo = TRUE)

renter_stats_white_u18 <- renter_stats_fun(white, u18)
renter_stats_black_u18 <- renter_stats_fun(black, u18)
renter_stats_aian_u18 <- renter_stats_fun(aian, u18)
renter_stats_aapi_u18 <- renter_stats_fun(aapi, u18)
renter_stats_hispan_u18 <- renter_stats_fun(hispan, u18)

renter_stats_white_u6 <- renter_stats_fun(white, u6)
renter_stats_black_u6 <- renter_stats_fun(black, u6)
renter_stats_aian_u6 <- renter_stats_fun(aian, u6)
renter_stats_aapi_u6 <- renter_stats_fun(aapi, u6)
renter_stats_hispan_u6 <- renter_stats_fun(hispan, u6)

transit_stats_white_u18 <- transit_stats_fun(white, u18)
transit_stats_black_u18 <- transit_stats_fun(black, u18)
transit_stats_aian_u18 <- transit_stats_fun(aian, u18)
transit_stats_aapi_u18 <- transit_stats_fun(aapi, u18)
transit_stats_hispan_u18 <- transit_stats_fun(hispan, u18)

transit_stats_white_u6 <- transit_stats_fun(white, u6)
transit_stats_black_u6 <- transit_stats_fun(black, u6)
transit_stats_aian_u6 <- transit_stats_fun(aian, u6)
transit_stats_aapiu6 <- transit_stats_fun(aapi, u6)
transit_stats_hispan_u6 <- transit_stats_fun(hispan, u6)


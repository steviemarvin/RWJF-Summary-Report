## function to make "wbhaa" variable, used for epimicrodata
# used in ant_racist_handbook/functions.R epimicrodata
make_wbhaa_var <- function(data){
  data %>% 
    mutate(wbhaa = case_when(wbhao == 1 ~ 1,
                             wbhao == 2 ~ 2,
                             wbhao == 3 ~ 3,
                             wbhao == 4 ~ 4,
                             (raceorig == 3 | raceorig == 11 | 
                                raceorig == 14 | raceorig == 17 |
                                raceorig == 19 | raceorig == 19 |
                                raceorig == 21 | raceorig == 23 |
                                raceorig == 24 | raceorig == 26 |
                                raceorig == 27 | raceorig == 28) & wbhao != 3 ~ 5,
                             TRUE ~ NA),
           wbhaa = labelled(wbhaa, c("White" = 1, "Black" = 2, "Hispanic" = 3,
                                     "AAPI" = 4, "AIAN" = 5)))
}

## used in RWJF/01_cleaning.R CPS
new_race_ethn = case_when(race == 100 & hispan == 0 & (eldch < 18 | yngch < 18)  ~ 1,
                          race == 200 & hispan == 0 & (eldch < 18 | yngch < 18) ~ 2,
                          race == 300 & hispan == 0 & (eldch < 18 | yngch < 18) ~ 3,
                          race >= 650 & race <= 652 & hispan == 0 & (eldch < 18 | yngch < 18) ~ 4,
                          hispan >= 1 & hispan <= 612 & (eldch < 18 | yngch < 18) ~ 5, 
                          TRUE ~ 6),
new_race_ethn = labelled(new_race_ethn, c("White not hispanic" = 1, "Black not hispanic" = 2, 
                                          "AIAN not hispanic" = 3, "AAPI not hispanic" = 4,
                                          "Hispanic" = 5, "Other" = 6)),


## used in annual wages and work hrs/code/wages_hours1.R CPS
race_new = case_when(
  race == 100 & hispan == 0 ~ 1,
  race == 200 & hispan == 0 ~ 2,
  (race == 801 | race == 805 | race == 806 | race == 810 | race == 811 | 
     race == 814 | race == 816 | race == 818) & hispan == 0 ~ 2,
  hispan > 0 & hispan <= 901 ~ 3,
  TRUE ~ 4),
race_new = labelled(race_new, c("r-white" = 1, "r-black" = 2, "r-hisp" = 3, "Other" = 4)),

## used in test/transit.R ACS
new_race = case_when(race == 1 & hispan == 0 ~ 1,
                     race == 2 ~ 2,
                     race == 3 ~ 3, 
                     race >= 4 & race <= 6 ~ 4,
                     hispan > 0 ~ 5),
new_race = labelled(new_race, c("White non-Hispanic" = 1, "Black" = 2, "AIAN" = 3, 
                                "AAPI" = 4, "Hispanic" = 5)),
new_race = as.character(as_factor(new_race)))

## used in trans/descriptive.R pulse data
wbhao = case_when(rrace == 1 & rhispanic == 1 ~ "White alone",
                  rrace == 2 & rhispanic == 1 ~ "Black alone",
                  rrace == 3 & rhispanic == 1 ~ "Asian alone",
                  rrace == 4 & rhispanic == 1 ~ "Other race or in combo",
                  TRUE ~ "Hispanic, any race"),


cps_race_fun <- function(race_var_type){
  if race_var_type == NULL {
    data %>% 
      wbho = case_when(
        race == 100 & hispan == 0 ~ 1,
        (race == 200 | race == 801 | race == 805 | race == 806 | race == 810 | race == 811 | 
                          race == 814 | race == 816 | race == 818) & hispan == 0 ~ 2,
        hispan > 0 & hispan <= 901 ~ 3,
        TRUE ~ 4),
      wbho = labelled(wbho, c("r-white" = 1, "r-black" = 2, "r_hisp" = 3, "Other" = 4)))
  }
}
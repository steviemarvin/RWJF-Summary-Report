# this file creates two types of outputs:
# 1. counts and shares of respondent's relationship to the household head by race/ethnicity.
  # The respondents must have a child that is under 18 to be counted in this output
# 2. the race/ethnicity distribution of all families.
  # There are four tables: (low income) families with children under 18 or 6 
  # The race/ethnicity distribution will not add up to 100% because there are mixed families
    # and we used the race/ethnicity of all people in the household 

# finding distribution of people with children under 18's relationship to household head
wbhaa_relate_total <- asec_clean %>% 
  # wbhaa race variable only includes people that have a child under 18
  filter(!is.na(wbhaa)) %>% 
  group_by(wbhaa, relatec) %>% 
  reframe(uw.count = n()) %>% 
  pivot_wider(id_cols = "relatec", names_from = "wbhaa", values_from = "uw.count", names_glue = "{wbhaa}_{.value}")

# finding share of people with children under 18 that are the hh head or spouse
wbhaa_relate_indic <- asec_clean %>% 
  filter(!is.na(wbhaa)) %>% 
  mutate(hh_head_sp = as.character(as_factor(labelled(hh_head_sp, c("not hh head or sp" = 0, "hh head or sp" = 1))))) %>% 
  group_by(wbhaa, hh_head_sp) %>% 
  reframe(uw.count = n()) %>% 
  pivot_wider(id_cols = "hh_head_sp", names_from = "wbhaa", values_from = "uw.count", names_glue = "{wbhaa}_{.value}")

# finding share of people with children under 18 that are not a related adult in the household (includes unmarried partners)
wbhaa_related_adult <- asec_clean %>% 
  filter(!is.na(wbhaa)) %>% 
  mutate(not_related_adult = as.character(as_factor(labelled(not_related_adult, c("not related adult" = 1, "related adult" = 0))))) %>% 
  group_by(wbhaa, not_related_adult) %>% 
  reframe(uw.count = n()) %>% 
  pivot_wider(id_cols = "not_related_adult", names_from = "wbhaa", values_from = "uw.count", names_glue = "{wbhaa}_{.value}")

# creating workbook to map descriptive stats
wb <- createWorkbook()

# mapping all of the relationship to household head dfs into one sheet
addWorksheet(wb, sheetName = "relate")
writeData(wb, sheet = "relate", x = wbhaa_relate_total, startCol = 1, startRow = 1)
writeData(wb, sheet = "relate", x = wbhaa_relate_indic, startCol = 1, startRow = nrow(wbhaa_relate_total)+3)
writeData(wb, sheet = "relate", x = wbhaa_related_adult, startCol = 1, startRow = nrow(wbhaa_relate_total)+nrow(wbhaa_relate_indic)+5)


# demographics of households that 
demographics <- asec_clean %>% 
  # keeping only household head and their spouse within respondents that have a child under 18
  mutate(across(starts_with("wbhaa_") | foreign_born, ~ if_else(hh_head_sp == 1 & .x == 1, 1, 0))) %>% 
  # serial: household serial number, unique to each household in given survey month and year
  group_by(year, serial) %>% 
  # indicates per household whether or not household head or spouse is of certain race, foreign_born, has child, is lowincome
  summarize(across(starts_with("wbhaa_") | foreign_born | u6 | u18 | hhwgt | lowincome, ~max(.x))) %>% 
  # renaming summarized variables
  rename_with(~gsub("wbhaa", "fam", .), starts_with("wbhaa_")) %>% 
  ungroup() %>% 
  # calculating number of "mixed families (household head and spouse are of different race)
  mutate(
    mixed_hh = case_when(fam_white >= 1 & (fam_black >= 1 | fam_AIAN >= 1 |
                                             fam_AAPI >= 1) ~ 1,
                         fam_black >= 1 & (fam_AIAN >= 1 | fam_AAPI >= 1) ~ 1,
                         fam_AIAN >= 1 & fam_AAPI >= 1 ~ 1,
                         TRUE ~ 0),
    mixed_white = if_else(fam_white >= 1 & mixed_hh == 1, 1, 0),
    mixed_black = if_else(fam_black >= 1 & mixed_hh == 1, 1, 0),
    mixed_AIAN = if_else(fam_AIAN >= 1 & mixed_hh == 1, 1, 0),
    mixed_AAPI = if_else(fam_AAPI >= 1 & mixed_hh == 1, 1, 0))

  
# population distribution by race/ethnicity by u18 or u6
demographics_fun <- function(data, age){
  data %>% 
    filter({{age}} == 1) %>% 
    group_by(year) %>% 
    summarize(across(contains("fam_") | foreign_born | mixed_hh, ~weighted.mean(.x, w = hhwgt, na.rm = TRUE), .names = "share_{.col}"),
              across((contains("fam_") & !contains("share")) | (contains("mixed_") & !contains("hh")), ~sum(.x * hhwgt, na.rm = TRUE))) %>% 
    rename_with(~gsub("fam_", "", .), contains("fam_")) %>% 
    # calculating share of mixed households as a percentage of respective population group
    mutate(mixed_white = mixed_white / white,
           mixed_black = mixed_black / black,
           mixed_AIAN = mixed_AIAN / AIAN,
           mixed_AAPI = mixed_AAPI / AAPI) %>% 
    select(year, contains("share"), share_mixed_hh) %>% 
    rename_with(~gsub("share_", "", .), contains("share_"))
}

# race and nativity distribution of families with children under 18
demo_18 <- demographics_fun(demographics, u18)

# race and nativity distribution of families with children under 6
demo_6 <- demographics_fun(demographics, u6)

# race and nativity distribution of lowincome families with children under 18
demo_li_18 <- demographics_fun(filter(demographics, lowincome == 1), u18)

# race and nativity distribution of lowincome families with children under 6
demo_li_6 <- demographics_fun(filter(demographics, lowincome == 1), u6)


addWorksheet(wb, sheetName = "distributions_hh+sp")
writeData(wb, sheet = "distributions", x = demo_18, startCol = 1, startRow = 2)
writeData(wb, sheet = "distributions", x = demo_li_18, startCol = ncol(demo_18)+2, startRow = 2)
writeData(wb, sheet = "distributions", x = demo_6, startCol = ncol(demo_18)*2+3, startRow = 2)
writeData(wb, sheet = "distributions", x = demo_li_6, startCol = ncol(demo_18)*3+4, startRow = 2)


# demographics of 



saveWorkbook(wb, here("output","RWJF_output.xlsx"), overwrite = TRUE)

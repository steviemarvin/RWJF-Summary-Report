## this script has code that helps us better understand and identify our sample universe.

# creates a df that shows who has a child u18 in each household by relationship to household head
families_bytype <- asec_clean %>% 
  filter(u18 == 1) %>% 
  mutate(ftype = as.character(as_factor(ftype))) %>% 
  group_by(year, serial, ftype) %>%
  summarize(n = n()) %>% 
  pivot_wider(id_cols = c("year", "serial"), names_from = c("ftype"), values_from = "n") %>% ungroup() %>% 
  rename_with(~tolower(gsub(" ", "_", .)))

families_df <- asec_clean %>% 
  filter(u18 == 1) %>% 
  mutate(relate = as.character(as_factor(relate))) %>% 
  group_by(year, serial, relate) %>%
  summarize(n = n()) %>% 
  pivot_wider(id_cols = c("year", "serial"), names_from = c("relate"), values_from = "n") %>% ungroup() %>% 
  rename_with(~gsub(", n.s.", "_ns", .), contains("n.s.")) %>% 
  rename_with(~gsub("/", "_", .), everything()) %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  rename_with(tolower) %>% 
  select(year, serial, order(names(.))) %>% 
  mutate(hh_type = case_when((!is.na(head_householder) & is.na(child) & is.na(parent) & 
                                is.na(other_relatives_ns) & is.na(sibling) & is.na(grandchild) & 
                                is.na(other_nonrelatives) & is.na(housemate_roomate) & is.na(roomer_boarder_lodger)) ~ "hhOnly",
                             ((!is.na(other_nonrelatives) | !is.na(housemate_roomate) | !is.na(roomer_boarder_lodger)) & 
                                is.na(head_householder) & is.na(child) & is.na(parent) & is.na(other_relatives_ns) &
                                is.na(sibling) & is.na(grandchild)) ~ "nrOnly",
                             ((!is.na(child) | !is.na(parent) | !is.na(other_relatives_ns) | !is.na(sibling) | !is.na(grandchild)) &
                                is.na(head_householder) & is.na(other_nonrelatives) & is.na(housemate_roomate) & is.na(roomer_boarder_lodger)) ~ "rOnly", 
                             (!is.na(head_householder) & 
                                (!is.na(other_nonrelatives) | !is.na(housemate_roomate) | !is.na(roomer_boarder_lodger)) &
                                (is.na(child) & is.na(parent) & is.na(other_relatives_ns) & is.na(sibling) & is.na(grandchild))) ~ "hhXnr",
                             (!is.na(head_householder) & 
                                (!is.na(child) | !is.na(parent) | !is.na(other_relatives_ns) | !is.na(sibling) | !is.na(grandchild)) &
                                is.na(other_nonrelatives) & is.na(housemate_roomate) & is.na(roomer_boarder_lodger)) ~ "hhXr",
                             ((!is.na(other_nonrelatives) | !is.na(housemate_roomate) | !is.na(roomer_boarder_lodger)) &
                                (!is.na(child) | !is.na(parent) | !is.na(other_relatives_ns) | !is.na(sibling) | !is.na(grandchild)) &
                                is.na(head_householder)) ~ "nrXr",
                             ((!is.na(other_nonrelatives) | !is.na(housemate_roomate) | !is.na(roomer_boarder_lodger)) &
                                (!is.na(child) | !is.na(parent) | !is.na(other_relatives_ns) | !is.na(sibling) | !is.na(grandchild)) &
                                !is.na(head_householder)) ~ "hhXnrXr")) %>% 
  inner_join(families_bytype, by = c("year", "serial"))

families_hh_type <- select(families_df, year, serial, hh_type)

li_families <- asec_clean %>% 
  filter(u18 == 1 & lowincome == 1) %>% 
  select(year, serial) %>% 
  group_by(year) %>% distinct(serial, .keep_all = TRUE) %>% 
  left_join(asec_clean, by = c("year", "serial"), multiple = "all")

# summary of who is designated as low income or not in mixed lowincome family
mixed_li_relate <- mixed_lowincome %>% 
  summarize(across(contains("0_") | contains("1_"), ~ sum(.x, na.rm = TRUE)))

mixed_li_ftype <- asec_clean %>% 
  right_join(mixed_lowincome, by = c("year", "serial")) %>% 
  mutate(ftype = as.character(as_factor(ftype))) %>% 
  group_by(ftype) %>% 
  summarize(n = n()) %>% ungroup()

all_lowincome <- li_stat %>% 
  filter(n_parent == n_lowincome) %>% 
  left_join(families_byrelate, by = c("year", "serial")) %>% 
  select(year, serial, contains("1_")) %>% 
  rename_with(~gsub("1_", "", .), contains("1_"))

all_lowincome_2022 <- filter(all_lowincome, year == 2022)

#X dfs that stores serials of households by household configuration
# only the household head (and spouse/ partner) have a child


  


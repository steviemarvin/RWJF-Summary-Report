# creates a df that shows who has a child u18 in each household by relationship to household head (var name: relate)
acs_families_df <- acs_clean %>% 
  filter(u18 == 1) %>% 
  mutate(related = as.character(as_factor(related))) %>% 
  group_by(year, serial, related) %>%
  summarize(n = n(), 
            hhwgt = max(hhwt)) %>% 
  pivot_wider(id_cols = c("year", "serial", "hhwgt"), names_from = c("related"), values_from = "n") %>% ungroup() %>% 
  rename_with(~gsub("/", "_", .), everything()) %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  rename_with(~gsub("-", "_", .), everything()) %>% 
  rename(other_non_rel = "Other_non_relatives_(1990_includes_employees)") %>% 
  rename_with(tolower) %>% 
  select(year, serial, hhwgt, order(names(.))) %>% 
  # categorizing households by who in household has child
  mutate(hh_type2 = case_when(!is.na(head_householder) ~ "hhYes",
                              is.na(head_householder) ~ "hhNo"),
         hh_type = case_when(!is.na(head_householder) & (is.na(adopted_child) & is.na(child) & is.na(child_in_law) & 
                                                           is.na(grandchild) & is.na(housemate_roomate) & is.na(other_non_rel) &
                                                           is.na(other_relatives) & is.na(parent) & is.na(parent_in_law) &
                                                           is.na(sibling) & is.na(sibling_in_law) & is.na(stepchild)) ~ "hhOnly",
                             (!is.na(adopted_child) | !is.na(child) | !is.na(child_in_law) | !is.na(grandchild) | 
                                !is.na(other_relatives) | !is.na(parent) | !is.na(parent_in_law) | !is.na(sibling) |
                                !is.na(sibling_in_law) | !is.na(stepchild)) & (is.na(head_householder) & is.na(housemate_roomate) &
                                                                                 is.na(other_non_rel)) ~ "rOnly",
                             (!is.na(housemate_roomate) | !is.na(other_non_rel)) & (is.na(adopted_child) & is.na(child) & 
                                                                                      is.na(child_in_law) & is.na(grandchild) &
                                                                                      is.na(other_relatives) & is.na(parent) & 
                                                                                      is.na(parent_in_law) & is.na(sibling) &
                                                                                      is.na(sibling_in_law) & is.na(stepchild) &
                                                                                      is.na(head_householder)) ~ "nrOnly",
                             !is.na(head_householder) & (!is.na(housemate_roomate) | 
                                                           !is.na(other_non_rel)) & (is.na(adopted_child) & is.na(child) & 
                                                                                       is.na(child_in_law) & is.na(grandchild) &
                                                                                       is.na(other_relatives) & is.na(parent) & 
                                                                                       is.na(parent_in_law) & is.na(sibling) &
                                                                                       is.na(sibling_in_law) & is.na(stepchild)) ~ "hhXnr",
                             !is.na(head_householder) & (!is.na(adopted_child) | !is.na(child) | !is.na(child_in_law) |
                                                           !is.na(grandchild) |  !is.na(other_relatives) | !is.na(parent) | !is.na(parent_in_law) |
                                                           !is.na(sibling) | !is.na(sibling_in_law) | !is.na(stepchild)) &
                               (is.na(housemate_roomate) & is.na(other_non_rel)) ~ "hhXr" ,
                             (is.na(head_householder) & (!is.na(housemate_roomate) | !is.na(other_non_rel) |
                                                           !is.na(adopted_child) | !is.na(child) | !is.na(child_in_law) |
                                                           !is.na(grandchild) |  !is.na(other_relatives) | !is.na(parent) | 
                                                           !is.na(parent_in_law) | !is.na(sibling) | !is.na(sibling_in_law) | 
                                                           !is.na(stepchild)) ~ "nrXr"),
                             !is.na(head_householder) & (!is.na(adopted_child) | !is.na(child) | !is.na(child_in_law) |
                                                           !is.na(grandchild) |  !is.na(other_relatives) | !is.na(parent) | !is.na(parent_in_law) |
                                                           !is.na(sibling) | !is.na(sibling_in_law) | !is.na(stepchild)) & 
                               (!is.na(housemate_roomate) | !is.na(other_non_rel)) ~ "hhXnrXr"))


# df with just household id info and household type 
acs_families_hh_type <- select(acs_families_df, year, serial, hh_type2, hh_type, hhwgt) 

acs_li_families <- filter(acs_clean, poverty < 200 & poverty != 0, u18 == 1, related != 1114) %>% 
  group_by(year) %>% distinct(serial, .keep_all = FALSE) %>% ungroup() %>% 
  left_join(acs_families_hh_type, by = c("year", "serial"))

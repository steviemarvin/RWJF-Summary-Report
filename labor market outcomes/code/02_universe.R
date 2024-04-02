## this script has code that helps us better understand and identify our sample universe.

# creates a df that shows who has a child u18 in each household by family type (var name: ftype)
families_bytype <- asec_clean %>% 
  filter(u18 == 1) %>% 
  mutate(ftype = as.character(as_factor(ftype))) %>% 
  group_by(year, serial, ftype) %>%
  summarize(n = n()) %>% 
  pivot_wider(id_cols = c("year", "serial"), names_from = c("ftype"), values_from = "n") %>% ungroup() %>% 
  rename_with(~tolower(gsub(" ", "_", .)))

# creates a df that shows who has a child u18 in each household by relationship to household head (var name: relate)
families_df <- asec_clean %>% 
  filter(u18 == 1) %>% 
  mutate(relate = as.character(as_factor(relate))) %>% 
  group_by(year, serial, relate) %>%
  summarize(n = n(), 
            hhwgt = max(asecwth)) %>% 
  pivot_wider(id_cols = c("year", "serial", "hhwgt"), names_from = c("relate"), values_from = "n") %>% ungroup() %>% 
  rename_with(~gsub(", n.s.", "_ns", .), contains("n.s.")) %>% 
  rename_with(~gsub("/", "_", .), everything()) %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  rename_with(tolower) %>% 
  select(year, serial, hhwgt, order(names(.))) %>% 
  # categorizing households by who in household has child
  mutate(hh_type = case_when((!is.na(head_householder) & is.na(child) & is.na(parent) & 
                                is.na(other_relatives_ns) & is.na(sibling) & is.na(grandchild) & 
                                is.na(other_nonrelatives) & is.na(housemate_roomate) & is.na(roomer_boarder_lodger)) ~ "hhOnly",
                             ((!is.na(other_nonrelatives) | !is.na(housemate_roomate) | !is.na(roomer_boarder_lodger)) & 
                                is.na(head_householder) & is.na(child) & is.na(parent) & is.na(other_relatives_ns) &
                                is.na(sibling) & is.na(grandchild)) ~ "nrOnly", # nr doesn't include unmarried spouse in the equation, since they would have same answer as hh head
                             ((!is.na(child) | !is.na(parent) | !is.na(other_relatives_ns) | !is.na(sibling) | !is.na(grandchild)) &
                                is.na(head_householder) & is.na(other_nonrelatives) & is.na(housemate_roomate) & is.na(roomer_boarder_lodger)) ~ "rOnly", 
                             (!is.na(head_householder) & 
                                (!is.na(other_nonrelatives) | !is.na(housemate_roomate) | !is.na(roomer_boarder_lodger)) &
                                (is.na(child) & is.na(parent) & is.na(other_relatives_ns) & is.na(sibling) & is.na(grandchild))) ~ "hhXnr", # household head and non related member have a child
                             (!is.na(head_householder) & 
                                (!is.na(child) | !is.na(parent) | !is.na(other_relatives_ns) | !is.na(sibling) | !is.na(grandchild)) &
                                is.na(other_nonrelatives) & is.na(housemate_roomate) & is.na(roomer_boarder_lodger)) ~ "hhXr",
                             ((!is.na(other_nonrelatives) | !is.na(housemate_roomate) | !is.na(roomer_boarder_lodger)) &
                                (!is.na(child) | !is.na(parent) | !is.na(other_relatives_ns) | !is.na(sibling) | !is.na(grandchild)) &
                                is.na(head_householder)) ~ "nrXr",
                             ((!is.na(other_nonrelatives) | !is.na(housemate_roomate) | !is.na(roomer_boarder_lodger)) &
                                (!is.na(child) | !is.na(parent) | !is.na(other_relatives_ns) | !is.na(sibling) | !is.na(grandchild)) &
                                !is.na(head_householder)) ~ "hhXnrXr"))

test <- asec_clean %>% 
  filter(u18 == 1) %>% 
  mutate(ftype = as.character(as_factor(ftype))) %>% 
  group_by(year, serial, ftype) %>%
  summarize(n = n(), 
            hhwgt = max(asecwth)) %>% 
  pivot_wider(id_cols = c("year", "serial", "hhwgt"), names_from = c("ftype"), values_from = "n") %>% ungroup() %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  rename_with(tolower) %>% 
  select(year, serial, hhwgt, order(names(.))) %>% 
  # categorizing households by who in household has child
  mutate(hh_type = case_when(!is.na(primary_family) & (is.na(secondary_individual) & is.na(related_subfamily) & is.na(unrelated_subfamily) & is.na(nonfamily_householder)) ~ "primary family only",
                             !is.na()
    
  ))

test <- demo_2022 %>% 
  filter(relate == 101 | relate == 1114 | relate == 201) %>%
  mutate(relate = as.character(as_factor(relate))) %>% 
  group_by(serial, relate) %>% 
  summarize(u18 = max(u18),
            nchild = max(nchild)) %>% 
  pivot_wider(id_cols = serial, names_from = relate, values_from = c("u18", "nchild")) %>% ungroup() %>% 
  rename_with(tolower) %>% 
  rename_with(~gsub("/", "_", .), everything()) %>% 
  rename_with(~gsub(" ", "_", .), everything()) %>% 
  mutate(mismatch_nchild = if_else((!is.na(nchild_spouse) & (nchild_head_householder != nchild_spouse)) | (!is.na(nchild_unmarried_partner) & (nchild_head_householder != nchild_unmarried_partner)), 1, 0),
         mismatch_u18 = if_else((!is.na(u18_spouse) & (u18_head_householder != u18_spouse)) | (!is.na(u18_unmarried_partner) & (u18_head_householder != u18_unmarried_partner)), 1, 0))
  
# df with just household id info and household type 
families_hh_type <- select(families_df, year, serial, hh_type, hhwgt) 

# df that shows unweighted sample size for types of families represented by year
hh_type_n <- families_hh_type %>% 
  group_by(year, hh_type) %>% 
  summarize(n = n(),
            wsum = sum(hhwgt)) %>% 
  pivot_wider(id_cols = "year", values_from = c("n", "wsum"), names_from = hh_type, names_sep = "_")

# df that indicates whether an unmarried partner to household head exists for each household in entire sample
## used later
partner_present <- asec_clean %>% 
  mutate(partner_present = if_else(relate == 1114, 1, 0)) %>% 
  group_by(year, serial) %>% 
  summarize(partner_present = max(partner_present))

# df of household and family ids of low income families
li_families <- asec_clean %>% 
  filter(u18 == 1 & lowincome == 1 & (relate != 1114)) %>% 
  group_by(year, serial) %>% distinct(famid, .keep_all = FALSE) %>% ungroup() %>% 
  left_join(families_hh_type, by = c("year", "serial"))

# df that counts number of children in the household and family unit within by age cut offs
n_children <- asec_clean %>%
  mutate(c_u18 = age < 18,
         c_u6 = age < 6) %>% 
  group_by(year, serial, famid) %>% 
  summarize(n_u18 = sum(c_u18),
            n_u6 = sum(c_u6)) %>% ungroup() %>% 
  filter(n_u18 != 0) %>% 
  inner_join(families_hh_type, by = c("year", "serial"))
  


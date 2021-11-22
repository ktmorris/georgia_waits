
roll <- readRDS("temp/processed_roll.rds") %>% 
  filter(v20, !eip_20) %>% 
  mutate(treated = !mail_20) %>% 
  select(-precinct,
         -special, -special_elig,
         -eip_20, -mail_20, -v20, -vjan, -vjan2) %>% 
  relocate(precinct_n, v16, v18) %>% 
  mutate_all(as.numeric) %>% 
  group_by(precinct_n) %>% 
  filter(mean(treated) > 0,
         mean(treated) < 1) %>% 
  ungroup() %>% 
  arrange(-precinct_n)

genout <- readRDS("temp/genout_all_1p.rds")

match_data <- select(roll, -voter_id, -treated,
                     -median_income, -some_college, -pop_dens,
                     -median_age)


mout <- Matchby(Tr = roll$treated, X = match_data, ties = F,
              replace = T, by = match_data$precinct_n,
              estimand = "ATT", Weight.matrix = genout, M = 1,
              exact = c(T, T, T, rep(F, 8)))

roll$id <- c(1:nrow(roll))

matches <- data.table(group = c(mout$index.treated, unique(mout$index.treated)),
                      voter = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))


matches <- left_join(matches, select(roll, id, voter_id), by = c("group" = "id")) %>% 
  select(-group) %>% 
  rename(group = voter_id)

matches <- left_join(matches, select(roll, id, voter_id), by = c("voter" = "id")) %>% 
  select(-voter)

saveRDS(matches, "temp/matches_precinct.rds")

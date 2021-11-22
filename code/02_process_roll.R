
roll <- readRDS("temp/geocoded_roll.rds")
####################################

hist_16 <- read_fwf("raw_data/2016_general/32668.TXT",
                    col_positions = fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)))

colnames(hist_16) <- c("county", "voter_id", "election_date",
                       "election_type", "party", "absentee", "provisional", "supplemental")

hist_18 <- read_fwf("raw_data/2018_general/34147.TXT",
                    col_positions = fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)))

colnames(hist_18) <- c("county", "voter_id", "election_date",
                       "election_type", "party", "absentee", "provisional", "supplemental")

hist_20 <- read_fwf("raw_data/2020_general/35209.TXT",
                    col_positions = fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)))

colnames(hist_20) <- c("county", "voter_id", "election_date",
                       "election_type", "party", "absentee", "provisional", "supplemental")

absentee_20 <- fread("raw_data/2020_general_absentee/STATEWIDE.csv")
colnames(absentee_20) <- clean_names(absentee_20)


hist_2021 <- read_fwf("raw_data/2021/2021.TXT",
                      col_positions = fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)))

colnames(hist_2021) <- c("county", "voter_id", "election_date",
                         "election_type", "party", "absentee", "provisional", "supplemental")

absentee_21 <- fread("raw_data/2021_absentee/35211/STATEWIDE.csv")
colnames(absentee_21) <- clean_names(absentee_21)

absentee_21 <- absentee_21 %>% 
  filter(ballot_status == "A") %>% 
  mutate(type = ifelse(ballot_style == "MAILED", "mail", "early in-person")) %>% 
  select(voter_id = voter_registration__, type)

########################

roll$v16 <- roll$voter_id %in% as.integer(hist_16$voter_id)
roll$v18 <- roll$voter_id %in% as.integer(hist_18$voter_id)
roll$v20 <- roll$voter_id %in% as.integer(hist_20$voter_id)
roll$vjan <- roll$voter_id %in% as.integer(filter(hist_2021, election_date == 20210105)$voter_id)
roll <- left_join(roll, absentee_21)

roll <- roll %>% 
  mutate(vjan2 = ifelse(is.na(type) & vjan, "election day",
                        ifelse(is.na(type) & !vjan, "abstain", type)))

roll$special <- roll$voter_id %in% as.integer(filter(hist_2021, election_date != 20210105)$voter_id)

roll$mail_20 <- roll$voter_id %in% filter(absentee_20,
                                          ballot_style == "MAILED",
                                          ballot_status == "A")$voter_registration__

roll$eip_20 <- roll$voter_id %in% filter(absentee_20,
                                         ballot_style == "IN PERSON",
                                         ballot_status == "A")$voter_registration__

roll$special_elig <- roll$house %in% c(90, 34, 156)

cleanup(c("roll"))


##########################################
roll <- filter(roll, is.finite(longitude),
               is.finite(latitude))

precincts <- readOGR("raw_data/VTD2020-General_Eli", "VTD2020-General-v3")

pings  <- SpatialPoints(roll[,c('longitude',
                                'latitude')], proj4string = precincts@proj4string)
roll$precinct <- over(pings, precincts)$DISTRICT

roll$precinct_n <- roll %>% 
  group_by(precinct) %>% 
  group_indices()

#####################

roll <- roll %>% 
  mutate(white = race == "White not of Hispanic Origin",
         black = race == "Black not of Hispanic Origin",
         latino = race == "Hispanic",
         asian = race == "Asian or Pacific Islander",
         dem = party == "D",
         rep = party == "R",
         male = gender == "M",
         age = 2020 - dob) %>% 
  select(voter_id, white, black, latino, asian, dem, rep, male,
         age,
         eip_20,
         mail_20,
         v16, v18, v20, vjan, vjan2, special, special_elig,
         precinct, precinct_n, median_income, some_college, pop_dens,
         median_age, GEOID)

roll <- roll[complete.cases(roll),]

saveRDS(roll, "temp/processed_roll.rds")


rolls <- fread("F:/rolls/georgia/GA 2020-11-02/Georgia_Daily_VoterBase.txt", select = c("REGISTRATION_NUMBER",
                                                                                        "SENATE_DISTRICT",
                                                                                        "HOUSE_DISTRICT"))


rolls <- rolls %>% 
  select(county = COUNTY_CODE,
         voter_id = REGISTRATION_NUMBER,
         status = VOTER_STATUS,
         street1 = RESIDENCE_HOUSE_NUMBER,
         street2 = RESIDENCE_STREET_NAME,
         street3 = RESIDENCE_STREET_SUFFIX,
         city = RESIDENCE_CITY,
         zip = RESIDENCE_ZIPCODE,
         dob = BIRTHDATE,
         reg_date = REGISTRATION_DATE,
         race = RACE_DESC,
         gender = GENDER,
         cd = CONGRESSIONAL_DISTRICT,
         party = PARTY_LAST_VOTED,
         senate = SENATE_DISTRICT,
         house = HOUSE_DISTRICT) %>% 
  mutate(state = "GA")

rolls <- clean_streets(rolls, c("street1", "street2", "street3"))

rolls <- geocode(rolls)

bgs <- block_groups(state = "GA", class = "sp")

pings  <- SpatialPoints(rolls[,c('longitude','latitude')], proj4string = bgs@proj4string)
rolls$GEOID <- over(pings, bgs)$GEOID

census <- readRDS("../regular_data/census_bgs_19.rds")

rolls <- left_join(rolls, census)

saveRDS(rolls, "temp/geocoded_roll.rds")

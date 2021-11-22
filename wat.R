

roll <- readRDS("temp/processed_roll.rds")

pto <- roll %>% 
  group_by(precinct) %>% 
  summarize(pto = mean(v20))

roll <- roll %>% 
  filter(v20, !eip_20) %>% 
  mutate(treated = !mail_20)
  
matches <- readRDS("temp/matches_precinct.rds")

roll <- left_join(roll, matches) %>% 
  filter(!is.na(weight))

roll <- left_join(roll, pto)

ll <- roll %>% 
  group_by(treated) %>% 
  summarize(v16 = mean(v16),
            v18 = mean(v18),
            v20 = mean(v20),
            vjan = mean(vjan))

####################################
waits <- fread("raw_data/waits/ga general eday - trimmed.csv") %>% 
  filter(EXPECTED_MINUTES_WAITING < 300)

precincts <- readOGR("raw_data/VTD2020-General_Eli", "VTD2020-General-v3")

pings  <- SpatialPoints(waits[,c('LONGITUDE',
                                'LATITUDE')], proj4string = precincts@proj4string)

waits$precinct2 <- over(pings, precincts)$DISTRICT

waits <- waits %>% 
  group_by(precinct = precinct2) %>% 
  summarize(mean_wait = mean(EXPECTED_MINUTES_WAITING),
            median_wait = quantile(EXPECTED_MINUTES_WAITING, 0.5),
            p10_wait = quantile(EXPECTED_MINUTES_WAITING, 0.1),
            p25_wait = quantile(EXPECTED_MINUTES_WAITING, 0.25),
            p75_wait = quantile(EXPECTED_MINUTES_WAITING, 0.75),
            p90_wait = quantile(EXPECTED_MINUTES_WAITING, 0.9),
            n = n())

roll <- left_join(roll, waits) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

roll <- roll %>%
  mutate(across(where(is.logical), 
                as.numeric))

m1 <- lm(vjan ~ treated*mean_wait +
             white + black + asian + latino +
             dem + rep + male + age +
             v16 + v18 + pto, filter(roll, n >= 5))

summary(m1)

j <- ggeffects::ggpredict(m1, terms = c("mean_wait[5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60]", "treated"))

##########################

ll <- roll %>%
  group_by(precinct) %>%
  filter(n >=5) %>%
  summarize(mean_wait = mean(mean_wait),
            pto = mean(pto),
            black = mean(black),
            median_income = mean(median_income),
            white = mean(white),
            age = mean(median_age),
            latino = mean(latino),
            pop_dens = mean(pop_dens),
            asian = mean(asian),
            some_college = mean(some_college))

summary(lm(mean_wait ~ poly(black, 2), ll))

ggplot(ll, aes(x = black, y = mean_wait)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))

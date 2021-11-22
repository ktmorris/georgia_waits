
## needs to be run on nyu hpc or its way too slow
## set seed for reproducibility. this is a random 5-digit number: floor(runif(1, min = 10000, max = 99999))
set.seed(16582)

library(Matching)
library(data.table)
library(scales)
library(kableExtra)
library(tidyverse)
require(snow)
require(parallel)


Sys.info()

NodeFile = Sys.getenv("MY_HOSTFILE")

print(NodeFile)

readLines(NodeFile)

cl<-makeCluster(c(readLines(NodeFile)), type="SOCK")
cl

##############################
samp <- readRDS("temp/processed_roll.rds") %>% 
  filter(v20, !eip_20) %>% 
  mutate(treated = !mail_20) %>% 
  select(-voter_id, -precinct,
         -special, -special_elig,
         -eip_20, -mail_20, -v20, -vjan, -vjan2) %>% 
  relocate(precinct_n) %>% 
  group_by(treated) %>% 
  slice_sample(prop = 0.01) %>% 
  ungroup()

match_data <- samp %>% 
  mutate_all(as.numeric) %>% 
  select(-treated)

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, pop.size = 1000,
                   exact = c(T, rep(F, 14)), cluster = cl)

saveRDS(genout, "temp/genout_all_1p.rds")



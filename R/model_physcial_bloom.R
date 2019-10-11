library(readr)
library(dplyr)
library(ggplot2)
library(randomForest)

# Read in data
dat <- read_csv(here::here("data/phsyical_bloom_data.csv"))

chla_rf_dat <- dat %>%
  filter(ph < 14, chla < 100, date >= "2018-01-01" & date <= "2018-12-31") %>%
  select(chla, do, ph, turb, spc, temp, date, waterbody) %>%
  mutate(waterbody = factor(waterbody),
         log_chla = log1p(chla)) %>%
  select(-chla) %>%
  na.omit()

chla_rf <- randomForest(log_chla~., data = chla_rf_dat, importance = TRUE, 
                        ntree = 1000)

phyco_rf_dat <- dat %>%
  filter(ph < 14, phyco < 100, date >= "2018-01-01" & date <= "2018-12-31") %>%
  select(phyco, do, ph, turb, spc, temp, date, waterbody) %>%
  mutate(waterbody = factor(waterbody),
         log_phyco = log1p(phyco)) %>%
  select(-phyco) %>%
  na.omit()

phyco_rf <- randomForest(log_phyco~., data = phyco_rf_dat, importance = TRUE, 
                         ntree = 1000)

library(readr)
library(dplyr)
library(ggplot2)

dat <- read_csv(here::here("data/phsyical_bloom_data.csv"))

chla_dat <- dat %>%
  filter(ph < 14, chla < 100, date >= "2018-01-01" & date <= "2018-12-31") %>%
  select(chla, do, ph, turb, spc, temp, date, waterbody) %>%
  mutate(waterbody = factor(waterbody),
         log_chla = log1p(chla)) %>%
  select(-chla) %>%
  na.omit()

chla_cor


phyco_dat <- dat %>%
  filter(ph < 14, phyco < 100, date >= "2018-01-01" & date <= "2018-12-31") %>%
  select(phyco, do, ph, turb, spc, temp, date, waterbody) %>%
  mutate(waterbody = factor(waterbody),
         log_phyco = log1p(phyco)) %>%
  select(-phyco) %>%
  na.omit()
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

dat <- read_csv(here::here("data/phsyical_bloom_data.csv"))

chla_cor <- dat %>%
  filter(ph < 14, chla < 100, date >= "2018-01-01" & date <= "2018-12-31") %>%
  mutate(log_chla = log1p(chla),
         year_day = yday(date)) %>%
  select(waterbody, site, year_day, log_chla, do, ph, turb, spc, temp) %>%
  na.omit() %>%
  group_by(waterbody) %>%
  summarize(date_cor = cor(year_day, log_chla, method = "kendall"),
         do_cor = cor(do, log_chla),
         ph_cor = cor(ph, log_chla),
         spc_cor = cor(spc, log_chla),
         temp_cor = cor(temp, log_chla))
  
phyco_cor <- dat %>%
  filter(ph < 14, phyco < 100, date >= "2018-01-01" & date <= "2018-12-31") %>%
  mutate(log_phyco = log1p(phyco),
         year_day = yday(date)) %>%
  select(waterbody, site, year_day, log_phyco, do, ph, turb, spc, temp) %>%
  na.omit() %>%
  group_by(waterbody) %>%
  summarize(date_cor = cor(year_day, log_phyco, method = "kendall"),
            do_cor = cor(do, log_phyco),
            ph_cor = cor(ph, log_phyco),
            spc_cor = cor(spc, log_phyco),
            temp_cor = cor(temp, log_phyco))

gg_all_chla <- dat %>%
  filter(ph < 14, chla < 100, date >= "2018-01-01" & date <= "2018-12-31") %>%
  mutate(logchla = log1p(chla))%>%
  select(date, waterbody, site, logchla, do, ph, spc, temp, turb) %>%
  gather("variable", "value", logchla:turb) %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  facet_grid(variable ~ waterbody, scales = "free_y")
gg_all_chla

gg_all_phyco <- dat %>%
  filter(ph < 14, phyco < 100, date >= "2018-01-01" & date <= "2018-12-31") %>%
  mutate(logphyco = log1p(phyco))%>%
  select(date, waterbody, site, logphyco, do, ph, spc, temp, turb) %>%
  gather("variable", "value", logphyco:turb) %>%
  ggplot(aes(x = date, y = value)) +
  geom_point() +
  facet_grid(variable ~ waterbody, scales = "free_y")
gg_all_phyco

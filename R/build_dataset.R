library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)


# Current cyano space time dataset from my local machine
# Get only 0, 1, 2 or integrated depths
# remov na
cst_dat <- read_csv(here::here("../cyano_space_time/data/cst_data_2017.csv"), 
                    na = c("", "NA", "na")) %>%
  filter(depth %in% c("0","1","2","int")) %>%
  mutate(variable = case_when(variable == "%do" ~
                                "perc_do",
                              TRUE ~ variable)) %>%
  na.omit()
  
# Current algae torch data
# negative values and zeroes to NA then remove
at_dat <- read_csv(here::here("../cyano_space_time/data/raw/algaetorch/merged_at.csv"), 
                   na = c("", "NA", "na")) %>%
  mutate(year = year(date_time), month = month(date_time), day = day(date_time),
         waterbody = case_when(str_detect(site_id,"warw") ~
                                 "warwick",
                               str_detect(site_id, "yawg") ~
                                 "yawgoo",
                               str_detect(site_id, "mash") ~
                                 "mashapaug",
                               TRUE ~ site_id),
         site = tolower(str_extract(site_id,"[A-Z0-9]$")),
         depth = 0,
         date = date(date_time)) %>% 
  group_by(site_id, date) %>% 
  mutate(from = 1, 
         to = n(),
         reps = full_seq(c(from, to), period = 1)) %>%
  ungroup() %>%
  mutate(torch_cyano = case_when(cyano_conc <= 0 & chla_conc <= 0 ~
                                   NA_real_,
                                 TRUE ~ cyano_conc),
         torch_chla = case_when(cyano_conc <= 0 & chla_conc <= 0 ~
                                  NA_real_,
                                TRUE ~ chla_conc),
         torch_turb = case_when(turb <= 0 & chla_conc <= 0 ~
                                  NA_real_)) %>%
  select(day, month, year, waterbody, site, depth, reps, torch_cyano, torch_turb,
         torch_chla) %>%
  gather("variable", "value",torch_cyano:torch_chla) %>%
  na.omit()
  

# Extracted chla
# remove blanks
# variable name to chla
chla_dat <- read_csv(here::here("data/chla_2017_2018_2019.csv"), 
                     na = c("", "NA", "na")) %>%
  mutate(day = day(mdy(Date)), month = month(mdy(Date)),
         depth = "int", variable = "chla") %>%
  group_by(day, month, year, waterbody, site) %>%
  mutate(from = 1, 
         to = n(),
         reps = full_seq(c(from, to), period = 1)) %>%
  ungroup() %>%
  select(day, month, year, waterbody, site, depth, reps, variable, 
         value = concentration) %>%
  filter(site != "blank") %>%
  na.omit()

# Extracted phyco
# negative values to zero
# dropped site p5
# variable name to phyco
phyco_dat <- read_csv(here::here("data/phyco_2017_2018_2019.csv"), 
                      na = c("", "NA", "na")) %>%
  mutate(day = day(mdy(Date)), month = month(mdy(Date)),
         depth = "int", variable = "phyco",
         concentration = case_when(concentration < 0 ~
                                     0.0,
                                   TRUE ~ concentration)) %>%
  group_by(day, month, year, waterbody, site) %>%
  mutate(from = 1, 
         to = n(),
         reps = full_seq(c(from, to), period = 1)) %>%
  ungroup() %>%
  select(day, month, year, waterbody, site, depth, reps, variable, 
         value = concentration) %>%
  na.omit()

# Merge 'em
dat <- rbind(cst_dat, at_dat, chla_dat, phyco_dat) %>%
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
  group_by(date, waterbody, site, variable) %>%
  summarize(mn_value = mean(value),
            sd_value = sd(value),
            n_value = n()) 

# Wide with at least both chla and phyco extracted
dat_wide <- dat %>%
  select(date, waterbody, site, variable, mn_value) %>%
  spread("variable", "mn_value") %>%
  filter(!is.na(chla),!is.na(phyco))

write_csv(dat_wide, "data/phsyical_bloom_data.csv")

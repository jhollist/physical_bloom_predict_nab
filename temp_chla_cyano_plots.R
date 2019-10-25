library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(hrbrthemes)

dat <- read_csv(here::here("data/phsyical_bloom_data.csv"))

mn_values <- dat %>%
  filter(date >= "2018-01-01" & date <= "2019-12-31") %>%
  select(date, waterbody, site, temp, torch_chla, torch_cyano) %>%
  group_by(date, waterbody) %>%
  summarize(torch_chla = mean(torch_chla, na.rm = TRUE),
            torch_cyano = mean(torch_cyano, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  gather("variable", "value", torch_chla:temp) 

sd_values <- dat %>%
  filter(date >= "2018-01-01" & date <= "2019-12-31") %>%
  select(date, waterbody, site, temp, torch_chla, torch_cyano) %>%
  group_by(date, waterbody) %>%
  summarize(torch_chla = sd(torch_chla, na.rm = TRUE),
            torch_cyano = sd(torch_cyano, na.rm = TRUE),
            temp = sd(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  gather("variable", "sd", torch_chla:temp)

plot_dat <- mn_values %>%
  left_join(sd_values)

# Thanks to: https://community.rstudio.com/t/separate-axis-breaks-for-facets/10352/2
my_lims <- function(x) { if (max(x) < 40) c(0, 35) else c(0, 78) }
my_labels <- c(temp = "Temperature", torch_chla = "Chlorophyll", 
               torch_cyano = "% Cyanobacteria")

gg_torch_temp <- plot_dat %>% 
  ggplot(aes(x = date, y = value, color = waterbody)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1) +
  facet_grid(variable ~ ., scales = "free_y", 
             labeller = labeller(variable = my_labels)) +
  scale_color_manual(values=c("red", "grey43","steelblue3")) +
  scale_y_continuous(limits = my_lims) +
  scale_x_date(breaks = "2 month") +
  theme_ipsum_rc()
gg_torch_temp 

ggsave("ri_ponds_plot.jpg", gg_torch_temp, width = 10, height = 7.5)  

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(hrbrthemes)

dat <- read_csv(here::here("data/phsyical_bloom_data.csv")) %>%
  mutate(torch_cyano = 100*(torch_cyano/torch_chla))

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

date_bnd <- plot_dat %>%
  filter(variable == "temp") %>%
  filter(!is.nan(value)) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(min_date = min(date, na.rm = TRUE),
            max_date = max(date, na.rm = TRUE))

# Thanks to: https://community.rstudio.com/t/separate-axis-breaks-for-facets/10352/2
my_y_lims <- function(x) { if (max(x) < 40) c(0, 35) 
  else if (max(x) <= 78) c(0, 78) else c(0,100)}
my_labels <- c(temp = "Temperature", torch_chla = "Chlorophyll", 
               torch_cyano = "% Cyanobacteria")
my_date_lims <- function(x) {if(year(x) == "2018") ymd(c("2018-06-01", "2018-12-15"))
                             else ymd(c("2019-06-01", "2019-12-15"))}

gg_torch_temp <- plot_dat %>% 
  mutate(year = year(date)) %>%
  left_join(date_bnd) %>%
  filter(date >= min_date & date <= max_date) %>%
  ggplot(aes(x = date, y = value, color = waterbody)) +
  geom_point(alpha = 0.3) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, alpha = 0.3) +
  geom_smooth(aes(group = interaction(year, waterbody)), method = "loess", se = FALSE) +
  facet_grid(variable ~ year, scales = "free", 
             labeller = labeller(variable = my_labels)) +
  scale_color_manual(values=c("red", "grey43","steelblue3")) +
  scale_y_continuous(limits = my_y_lims) +
  scale_x_date(breaks = "2 month", limits = my_date_lims) +
  theme_ipsum_rc(plot_title_size = 14,axis_text_size = 14, axis_title_size = 14, 
                 strip_text_size = 12)  
  
gg_torch_temp 

ggsave("ri_ponds_plot.jpg", gg_torch_temp, width = 12.5, height = 7.5)  

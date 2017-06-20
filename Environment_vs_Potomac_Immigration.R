library(ggplot2); library(lubridate); library(dplyr)

# Data import & munging
source('chain_bridge_wt.R')
load('secor.sb.rda')
chainbr <- chainbr %>% 
  filter(month(dates) %in% 2:5) %>% 
  mutate(year = year(dates),
         doy = yday(dates))

pot.return <- secor.sb %>% 
  filter(month(date.local) %in% 2:5,
         array %in% c('Upper Potomac', 'Mid Potomac')) %>% 
  mutate(week = floor_date(date.local, unit = 'week'),
         year = year(date.local),
         sex = ifelse(sex == '', 'Unknown', sex)) %>% 
  group_by(transmitter, year, sex) %>% 
  summarize(firstwk = min(week),
            doy = yday(firstwk))

# Discharge, water temperature, and FW Potomac entry v time
env.plots <- ggplot() + geom_line(data = chainbr, aes(x = doy, y = val)) +
  facet_grid(var ~ year, scales = 'free') +
  lims(x = c(32, 152)) + #Feb 1 - May 31
  labs(x = NULL) +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), 'mm'))

imm.plots <- ggplot() + geom_histogram(data = pot.return, aes(doy, fill = sex),
                          bins = 52) +
  facet_wrap( ~ year, ncol = 4) +
  lims(x = c(32, 152)) +
  labs(x = 'Day of Year', y = 'Count', fill = 'Sex') +
  theme_bw() +
  theme(legend.position = c(0.95, 0.66),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(0.75, 7.5, 2, 4.8), 'mm'))

library(gridExtra)
grid.arrange(env.plots, imm.plots,
             ncol = 1, heights = c(1.75, 1))


# Cumulative distribution of returns v water temperature
cdist.plots <- ggplot() + stat_ecdf(data = pot.return, aes(x = doy)) +
  facet_wrap(~ year, ncol = 4) +
  lims(x = c(32, 152)) +
  labs(x = 'Day of Year', y = 'Cumulative Frequency') +
  theme_bw() +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'mm'),
        strip.background = element_blank(),
        strip.text = element_blank())

wtemp.plots <- ggplot() +
  geom_line(data = chainbr[chainbr$var == 'wtemp',], aes(x = doy, y = val)) +
  facet_grid(~ year, scales = 'free') +
  lims(x = c(32, 152)) +
  labs(x = NULL) +
  labs(y = 'Water Temperature') +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1, 1, 1, 3.5), 'mm'))

grid.arrange(wtemp.plots, cdist.plots,
             ncol = 1)

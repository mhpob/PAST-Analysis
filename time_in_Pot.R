library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

# Data prep ----
# Select fish that lived long enough, flag those that didn't make it through
#   2015 season (mutate line)
valid.fish <- secor.sb %>% 
  filter(tag.date <= '2014-10-30') %>% 
  group_by(transmitter) %>%
  summarize(max = max(date.local)) %>% 
  filter(max >= '2015-03-31') %>% 
  mutate(yr.flag = ifelse(max <= '2016-03-31', 2014, 2015))

occ.data <- secor.sb %>% 
  left_join(valid.fish) %>% 
  # Use flag to remove observations if fish didn't make it through year
  mutate(yr.flag = ifelse(yr.flag == 2014 & date.local >= '2015-04-01',
                          NA, yr.flag)) %>% 
  filter(!is.na(yr.flag),
         date.local < '2016-04-01') %>% 
  mutate(tag.season = ifelse(tag.date == '2014-10-30', 'Fall', 'Spring'),
         date.floor = floor_date(date.local, unit = 'month'),
         year = ifelse(date.local < '2015-04-01', 2014, 2015),
         potomac = ifelse(grepl('Potomac', array), 1, 0),
         age.adjust = ifelse(date.local >= '2015-04-01',
                             age + 1, age))

pct.pot <- occ.data %>% 
  filter(potomac == T) %>% 
  distinct(transmitter, date.floor, .keep_all = T) %>%
  group_by(transmitter, age.adjust) %>% 
  summarize(pct.pot = n()/12)

occ.data <- left_join(occ.data, pct.pot) %>% 
  group_by(transmitter, year) %>% 
  mutate(potomac = ifelse(1 %in% potomac, 1, 0),
         pct.pot = ifelse(is.na(pct.pot), 0, pct.pot)) %>%
  ungroup() %>% 
  distinct(tag.season, transmitter, year, potomac, sex,
           pct.pot, age.adjust, length)

# Plotting ----
ggplot() + geom_jitter(data = occ.data,
                      aes(x = length, y = pct.pot, shape = tag.season),
                      width = 0.1, height = 0.01) +
  geom_smooth(data = occ.data,
              aes(x = length, y = pct.pot, shape = tag.season)) +
  labs(x = 'Length (mm)', y = '% of Year in Potomac',
       shape = 'Tag\nSeason') +
  facet_wrap(~year) +
  coord_cartesian(ylim = c(-0.01, 0.85)) +
  theme_bw()

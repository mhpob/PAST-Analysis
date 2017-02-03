library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

# Select fish that lived long enough
valid.fish <- secor.sb %>% 
  filter(tag.date <= '2014-10-30') %>% 
  group_by(transmitter) %>%
  summarize(max = max(date.local)) %>% 
  filter(max >= '2016-03-31')

# Prepare data
occ.data <- secor.sb %>% 
  filter(transmitter %in% valid.fish$transmitter,
         date.local < '2016-04-01') %>% 
  mutate(tag.season = ifelse(tag.date == '2014-10-30', 'Fall', 'Spring'),
         date.floor = floor_date(date.local, unit = 'month'),
         year = ifelse(date.local < '2015-04-01', 2014, 2015),
         coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                'Hudson', 'Long Island', 'Mass', 'New Jersey'),
                          T, F),
         age.adjust = ifelse(date.local >= '2015-04-01',
                             age + 1, age))

pct.coastal <- occ.data %>% 
  filter(coastal == T) %>% 
  distinct(transmitter, date.floor, .keep_all = T) %>%
  group_by(transmitter, age.adjust) %>% 
  summarize(pct.coastal = n()/24)

occ.data <- left_join(occ.data, pct.coastal) %>% 
  group_by(transmitter, year) %>% 
  mutate(coastal = ifelse(T %in% coastal, T, F),
         pct.coastal = ifelse(is.na(pct.coastal), 0, pct.coastal)) %>%
  ungroup() %>% 
  distinct(tag.season, transmitter, year, coastal,
           pct.coastal, age.adjust, length)

# Begin model
rm(pct.coastal, secor.sb, valid.fish)

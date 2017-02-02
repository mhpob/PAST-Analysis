library(ggplot2); library(dplyr)
load('secor.sb.rda')

## Coastal incidence
valid.fish <- secor.sb %>% 
  filter(tag.date <= '2014-10-30') %>% 
  group_by(transmitter) %>%
  summarize(max = max(date.local)) %>% 
  filter(max >= '2016-03-31')


occ.data <- secor.sb %>% 
  filter(transmitter %in% valid.fish$transmitter,
         date.local < '2016-04-01') %>% 
  mutate(tag.season = ifelse(tag.date == '2014-10-30', 'Fall', 'Spring'),
         date.floor = lubridate::floor_date(date.local, unit = 'month'),
         coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                'Hudson', 'Long Island', 'Mass', 'New Jersey'),
                          T, F),
         age.adjust = ifelse(date.local >= '2015-04-01',
                             age + 1, age))

  
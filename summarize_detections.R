load('secor.sb.rda')
library(dplyr)
number <- secor.sb %>%
  group_by(array) %>%
  summarize(det = n()) %>%
  arrange(-det)

fish <- secor.sb %>%
  group_by(array) %>%
  summarize(fish = length(unique(trans.num))) %>% 
  arrange(-fish)

# seasons dec-feb, mar-may june-aug, sept-nov
# sexes
secor.sb$season <- ifelse(lubridate::month(secor.sb$date.local) %in% c(3, 4, 5),
                          'Spring',
                   ifelse(lubridate::month(secor.sb$date.local) %in% c(6, 7, 8),
                          'Summer',
                   ifelse(lubridate::month(secor.sb$date.local) %in% c(9, 10, 11),
                          'Fall', 'Winter')))
season <- secor.sb %>%
  group_by(array, season) %>%
  summarize(fish = length(unique(trans.num))) %>% 
  arrange(-fish)

secor.sb$length.bin <- ifelse(secor.sb$length < 550, '<55',
                      ifelse(secor.sb$length >= 550 &
                               secor.sb$length < 650, '55-65',
                      ifelse(secor.sb$length >= 650 &
                               secor.sb$length < 800, '65-80', '>80')))

season_size <- secor.sb %>%
  group_by(array, season, length.bin) %>%
  summarize(fish = length(unique(trans.num))) %>% 
  arrange(-fish)

# Stations detecting fish, ches v coast per year
secor.sb %>% 
  mutate(coast = case_when(grepl('New|Long|Coast|Mass|NYB|Delaware|Hud', array) ~ 'Coastal',
                           T ~ 'Chesapeake'),
         year = lubridate::year(date.local)) %>% 
  distinct(year, coast, station) %>%
  xtabs(data = ., formula = ~ coast + year)

# Numbers of fish, receiver location (ches v coast) per year
secor.sb %>% 
  mutate(coast = case_when(grepl('New|Long|Coast|Mass|NYB|Delaware|Hud', array) ~ 'Coastal',
                           T ~ 'Chesapeake'),
         year = lubridate::year(date.local)) %>% 
  distinct(year, coast, transmitter) %>%
  xtabs(data = ., formula = ~ coast + year)

# Number of detections, receiver location (ches v coast) per year
secor.sb %>% 
  mutate(coast = case_when(grepl('New|Long|Coast|Mass|NYB|Delaware|Hud', array) ~ 'Coastal',
                           T ~ 'Chesapeake'),
         year = lubridate::year(date.local)) %>%
  xtabs(data = ., formula = ~ coast + year)

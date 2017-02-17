library(ggplot2); library(dplyr)
load('secor.sb.rda')

## Coastal occurrance vs Length, including log fit
occ.data <- secor.sb %>% 
  # filter(date.local >= '2014-03-30',
  #        date.local <= '2014-10-29') %>%
  mutate(yr.adjust = ifelse(date.local <= '2015-03-21', 2014,
                            ifelse(date.local > '2015-03-21' &
                                     date.local <= '2016-03-21', 2015,
                                   2016)),
         date.floor = lubridate::floor_date(date.local, unit = 'day'),
         coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                'Hudson', 'Long Island', 'Mass', 'New Jersey'),
                          'YES', 'NO')) %>% 
  filter(yr.adjust < 2016) %>% 
  distinct(transmitter, date.local, coastal, .keep_all = T) %>% 
  group_by(transmitter, yr.adjust) %>% 
  summarize(length = mean(length),
            coast = sum(coastal == 'YES'),
            bay = sum(coastal == 'NO'),
            prop = coast/(bay+coast)) %>% 
  filter(coast + bay > 5) %>% 
  arrange(length)
            
ggplot(data = occ.data, aes(x = length, y = prop, color = factor(yr.adjust))) +
  geom_point(size = 3) +
  stat_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  labs(x = 'Length (mm)', y = 'Coastal Occurrences (%)', color = '') +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 16))

## Presence
pres <- secor.sb %>% 
  # filter(date.local >= '2014-03-30',
  #        date.local <= '2014-10-29') %>% 
  mutate(coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                'Hudson', 'Long Island', 'Mass', 'New Jersey'),
                          1, 0)) %>%
  data.frame()

coast <- levels(factor(pres[pres$coastal == 1, 'transmitter']))

pres <- pres %>% 
  mutate(coastal = ifelse(transmitter %in% coast, 1, 0)) %>% 
  distinct(transmitter, coastal, .keep_all = T)
  
ggplot(data = pres, aes(x = length, y = coastal)) + geom_point(size = 3) +
  stat_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  labs(x = 'Length (mm)', y = 'Probability of Moving to Coastal Waters')

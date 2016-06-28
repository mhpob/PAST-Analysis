library(ggplot2); library(dplyr)
load('secor.sb.rda')

## Coastal occurrance vs Length, including log fit
occ.data <- secor.sb %>% 
  filter(date.local >= '2014-03-30',
         date.local <= '2014-10-29') %>% 
  mutate(date.floor = lubridate::floor_date(date.local, unit = 'day'),
         coastal = ifelse(array %in% c('MD Coast', 'DE Coast', 'Long Island',
                                       'Mass', 'New Jersey'), 'YES', 'NO')) %>% 
  distinct(trans.num, date.floor, coastal, .keep_all = T) %>% 
  group_by(trans.num) %>% 
  summarize(length = mean(length),
            coast = sum(coastal == 'YES'),
            bay = sum(coastal == 'NO'),
            prop = coast/(bay+coast)) %>% 
  arrange(length)
            
ggplot(data = occ.data, aes(x = length, y = prop)) + geom_point(size = 3) +
  stat_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  labs(x = 'Length (mm)', y = 'Coastal Occurrences (%)') +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 16))

## Presence
pres <- secor.sb %>% 
  filter(date.local >= '2014-03-30',
         date.local <= '2014-10-29') %>% 
  mutate(coastal = ifelse(array %in% c('MD Coast', 'DE Coast', 'Long Island',
                                       'Mass', 'New Jersey'), 1, 0)) %>% 
  data.frame()

coast <- levels(factor(pres[pres$coastal == 1, 'trans.num']))

pres <- pres %>% 
  mutate(coastal = ifelse(trans.num %in% coast, 1, 0)) %>% 
  distinct(trans.num, coastal, .keep_all = T)
  
ggplot(data = pres, aes(x = length, y = coastal)) + geom_point(size = 3) +
  stat_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  labs(x = 'Length (mm)', y = 'Probability of Moving to Coastal Waters')

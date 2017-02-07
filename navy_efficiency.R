library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

eff.base <- secor.sb %>% 
  mutate(yr.adjust = ifelse(date.local <= '2015-03-21', 2014,
                            ifelse(date.local > '2015-03-21' &
                                     date.local <= '2016-03-21', 2015,
                                   2016)),
         coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                       'Hudson', 'Long Island', 'Mass',
                                       'New Jersey'), T, F),
         mouth = ifelse(array == 'Bay Mouth', T, F),
         month = month(date.local),
         mouth.season = ifelse(month %in% seq(4, 6, 1), 'Spring',
                               ifelse(month %in% c(11, 12, 1),'Fall',
                                      'Other'))) 

overall.eff <- eff.base %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>%
  summarize(navy.detected = T %in% mouth) %>% 
  ungroup() %>% 
  summarize(overall.eff = sum(navy.detected == T)/n())
  
yr.eff <- eff.base %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>%
  summarize(navy.detected = T %in% mouth) %>% 
  group_by(yr.adjust) %>% 
  summarize(yr.eff = sum(navy.detected == T)/n())

season.eff <- eff.base %>% group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>%
  group_by(transmitter, yr.adjust, mouth.season) %>% 
  summarize(navy.detected = T %in% mouth) %>% 
  group_by(yr.adjust, mouth.season) %>% 
  summarize(season.eff = sum(navy.detected == T)/n())


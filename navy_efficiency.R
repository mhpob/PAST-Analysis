library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

data <- secor.sb %>% 
  mutate(yr.adjust = ifelse(date.local <= '2015-03-21', 2014,
                            ifelse(date.local > '2015-03-21' &
                                     date.local <= '2016-03-21', 2015,
                                   2016)),
         coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                       'Hudson', 'Long Island', 'Mass',
                                       'New Jersey'), T, F),
         mouth = ifelse(array == 'Bay Mouth', T, F)) %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>% 
  summarize(navy.detected = T %in% mouth) %>% 
  group_by(yr.adjust) %>% 
  summarize(navy.eff = sum(navy.detected == T)/n())

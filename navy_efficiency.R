library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

# Goal is to calculate % success of Navy array in detecting fish that went into
# coastal waters.
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
         mouth.season = ifelse(month %in% 4:9, 'Apr-Sep', 'Oct-Mar')) %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal)

overall.eff <- eff.base %>%
  summarize(navy.detected = T %in% mouth) %>% 
  ungroup() %>% 
  summarize(navy = sum(navy.detected == T),
            coastal = n(),
            overall.eff = sum(navy.detected == T)/n())
  
yr.eff <- eff.base %>% 
  summarize(navy.detected = T %in% mouth) %>% 
  group_by(yr.adjust) %>% 
  summarize(navy = sum(navy.detected == T),
            coastal = n(),
            yr.eff = sum(navy.detected == T)/n())

season.eff <- eff.base %>%
  group_by(transmitter, yr.adjust, mouth.season) %>% 
  summarize(navy.detected = T %in% mouth) %>% 
  group_by(mouth.season, yr.adjust) %>% 
  summarize(navy = sum(navy.detected == T),
            coastal = n(),
            season.eff = sum(navy.detected == T)/n())

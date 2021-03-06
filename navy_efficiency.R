library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

# Goal is to calculate % success of Navy array in detecting fish that went into
# coastal waters.
eff.base <- secor.sb %>% 
  mutate(yr.adjust = ifelse(month(date.local) %in% 1:2, year(date.local) - 1,
                            year(date.local)),
         coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                       'NYB', 'Hudson', 'Long Island', 'Mass',
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
            overall.eff = navy/coastal)
  
yr.eff <- eff.base %>% 
  summarize(navy.detected = T %in% mouth) %>% 
  group_by(yr.adjust) %>% 
  summarize(navy = sum(navy.detected == T),
            coastal = n(),
            yr.eff = navy/coastal)

season.eff <- eff.base %>%
  group_by(transmitter, yr.adjust) %>% 
  filter(mouth == T) %>%
  summarize(first.mouth = min(date.local)) %>% 
  mutate(mouth.season = ifelse(month(first.mouth) %in% 4:9,
                                  'Apr-Sep', 'Oct-Mar')) %>% 
  group_by(yr.adjust, mouth.season) %>%
  summarize(navy = n()) %>% 
  left_join(yr.eff[, c(1,3)]) %>% 
  group_by(yr.adjust, mouth.season) %>% 
  summarize(navy = navy,
            coastal = coastal,
            season.eff = navy/coastal)

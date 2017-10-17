library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

dat <- secor.sb %>% 
  filter(date.local <= '2017-03-21',
         !grepl('-53', secor.sb$transmitter)) %>% 
  mutate(yr.adjust = ifelse(date.local <= '2015-03-21', 2014,
                            ifelse(date.local > '2015-03-21' &
                                     date.local <= '2016-03-21', 2015,
                                   2016)))

coastal <- dat %>%
  mutate(coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                              'Hudson', 'Long Island', 'Mass',
                              'New Jersey'), T, F)) %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>% 
  distinct(transmitter)


potpax <- dat %>%
  mutate(nonresident = ifelse(!grepl('Pot|Pat|Lower MD Bay', dat$array),
                              T, F)) %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% nonresident) %>% 
  anti_join(dat, .) %>% 
  group_by(transmitter, yr.adjust) %>%
  distinct(transmitter)

ptpx_keep <- dat %>%
  group_by(transmitter) %>% 
  summarize(max = max(date.local)) %>% 
  right_join(potpax) %>% 
  mutate(flag = ifelse(max <= '2014-08-01' |
                         (max <= '2015-08-01' & yr.adjust == 2015) |
                         (max <= '2016-08-01' & yr.adjust == 2016), T, F)) %>% 
  filter(flag == F) %>% 
  arrange(transmitter, yr.adjust)



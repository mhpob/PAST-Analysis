load('secor.sb.rda')
library(lubridate); library(dplyr)

secor.sb$year <- year(secor.sb$date.local)

pot2mass <- secor.sb %>% 
  filter(grepl('1601-2', transmitter),
         month(date.local) %in% seq(2, 8, 1),
         array == 'Mass',
         date.local <= ymd('2019-01-01', tz = 'America/New_York')) %>% 
  distinct(transmitter, year) %>% 
  # Subset secor.sb by transmitter x year combinations that contain Mass array
  left_join(secor.sb) %>% 
  filter(grepl('Pot|Mass', array),
         month(date.local) %in% seq(2, 8, 1)) %>% 
  arrange(transmitter, date.local) %>% 
  mutate(next_array = c(.$array[seq(2, nrow(.), 1)], NA)) %>% 
  filter(next_array == 'Mass') %>% 
  distinct(transmitter, year, array, .keep_all = T) 

trans_yr_nest <- split(pot2mass, pot2mass$transmitter) %>% 
  lapply(., function(trans_nest) split(trans_nest, trans_nest$year))

transit_nest <- lapply(trans_yr_nest, function(trans_nest){
  lapply(trans_nest, function(yr_nest){
    yr_nest$date.local[2] - yr_nest$date.local[1]
  })
})

transit <- unlist(transit_nest)

range(transit, na.rm = T)
mean(transit, na.rm = T)

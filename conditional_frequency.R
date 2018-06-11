library(lubridate); library(dplyr)
load('secor.sb.rda')

names(secor.sb)
unique(secor.sb$array)

NYBprop <- function(yr){
  secor.sb %>% 
  mutate(year = year(date.local)) %>% 
  filter(array %in% c('Long Island', 'NYB', 'New Jersey'),
         year == yr) %>% 
  distinct(transmitter) %>% 
  left_join(filter(secor.sb, year(date.local) == yr)) %>% 
  xtabs(data = ., formula = ~transmitter + array) %>% 
  apply(., 2, function(x) sum(x > 0) / nrow(.))
}

NYBprop(2014)
NYBprop(2015)
NYBprop(2016)
NYBprop(2017)


j <- secor.sb %>% 
  mutate(year = year(date.local),
         coastal = case_when(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                              'Hudson', 'Long Island', 'Mass', 'New Jersey',
                              'NYB') ~ T,
                             T ~ F),
         NYB = case_when(array %in% c('Long Island', 'NYB', 'New Jersey') ~ T,
                         T ~ F)) %>% 
  filter(coastal == T)

setdiff(filter(j, year == 2014, NYB == T) %>%  distinct(year, transmitter),
        filter(j, year == 2014, NYB == F) %>%  distinct(year, transmitter))

k <- secor.sb %>% filter(grepl('498', transmitter))

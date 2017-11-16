library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

dat <- secor.sb %>% 
  filter(!grepl('-53', secor.sb$transmitter)) %>% 
  mutate(yr.adjust = ifelse(date.local <= '2015-03-21', 2014,
                            ifelse(date.local > '2015-03-21' &
                                     date.local <= '2016-03-21', 2015,
                                   ifelse(date.local > '2016-03-21' &
                                            date.local <= '2017-03-21', 2016,
                                          2017))))
## Coastal Designation per year----
max.date <- dat %>% 
  group_by(transmitter) %>%
  summarize(max = max(date.local),
            max = year(max))

coastal <- dat %>%
  mutate(coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                       'Delaware', 'Hudson', 'Long Island',
                                       'Mass', 'New Jersey'), T, F)) %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>% 
  distinct(transmitter) %>% # years coastal
  full_join(max.date) %>% 
  mutate(coastal = ifelse(is.na(yr.adjust), 0, 1)) %>% #Mark non-coastal fish
  tidyr::spread(yr.adjust, coastal)

# Final labels. NA = removed (false negative), 0 = non coastal (negative), 
#   1 = coastal (positive)
for(i in 3:6){
  coastal[i] <- ifelse(is.na(coastal[i]) &
                         coastal$max >= as.numeric(names(coastal)[i]), 0,
        ifelse(coastal[, i] == 1, 1,
                   NA))
}

coastal <- coastal[, !names(coastal) == '<NA>']
names(coastal)[3:6] <- paste0('Coast', names(coastal)[3:6])



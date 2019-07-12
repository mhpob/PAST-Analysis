library(dplyr)
load('secor.sb.rda')

secor.sb <- secor.sb %>% 
  filter(!is.na(array),
         date.local < '2018-01-01') %>% 
  mutate(year = lubridate::year(date.local),
         doy = lubridate::yday(date.local),
         size.bin = case_when(length < 800 ~ '< 800',
                              length >= 800 ~ '> 800'),
         array = case_when(grepl('Pot', array) ~ 'Potomac',
                           grepl('Chop|MD Bay|Nan|Pat', array) ~ 'MD Ches',
                           grepl('Eliz|James|Rapp|York', array) ~ 'VA Ches',
                           grepl('C\\&D|Del', array) ~ 'C&D',
                           grepl('NYB|Hud', array) ~ 'New York Harbor',
                           T ~ array),
         array = factor(array,
                        levels = c('Potomac', 'VA Ches', 'MD Ches', 'Bay Mouth',
                                   'C&D', 'VA Coast', 'MD Coast', 'DE Coast',
                                   'New Jersey', 'New York Harbor', 'Long Island',
                                   'Mass'), ordered = T))

reduced_pts <- distinct(secor.sb, transmitter, doy, array, .keep_all = T)

library(ggplot2)

ggplot() + geom_point(data = reduced_pts,
                      aes(x = doy, y = array, color = size.bin)) +
  facet_wrap(~ year) +
  labs(x = 'Day of Year', y = NULL, color = "Tag Length") +
  theme_bw()


temp <- secor.sb %>% 
  mutate(grp_array = case_when(grepl('Coast', array) ~ 'DelMarVa',
                               grepl('New|Long', array) ~ 'NYB',
                               array == 'Mass' ~ 'Mass',
                               T ~ 'bad'),
         mig_season = case_when(doy <= 250 ~ 'spring',
                                T ~ 'fall')) %>% 
  filter(grp_array != 'bad') %>% 
  group_by(year, mig_season) %>% 
  summarize(min = min(date.local),
            avg = mean(date.local),
            stdev = sd(doy),
            max = max(date.local),
            duration = difftime(max, min, units = 'days'))

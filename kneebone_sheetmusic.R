library(dplyr)
load('secor.sb.rda')

secor.sb <- secor.sb %>% 
  filter(!is.na(array),
         date.local < '2019-01-01',
         grepl('25[45]..', transmitter)) %>% 
  mutate(year = lubridate::year(date.local),
         doy = lubridate::yday(date.local),
         length = length / 10,
         size.bin = case_when(length < 80 ~ '< 80 cm',
                              length >= 80 ~ '> 80 cm'),
         array = case_when(grepl('Pot', array) ~ 'Potomac',
                           grepl('Chop|MD Bay|Nan|Pat', array) ~ 'MD Ches.',
                           grepl('Eliz|James|Rapp|York', array) ~ 'VA Ches.',
                           grepl('C\\&D|Del', array) ~ 'C&D Canal',
                           grepl('NYB|Hud', array) ~ 'NY Harbor',
                           array == 'Mass' ~ 'Massachusetts',
                           T ~ array),
         array = factor(array,
                        levels = c('Potomac', 'VA Ches.', 'MD Ches.', 'Bay Mouth',
                                   'C&D Canal', 'VA Coast', 'MD Coast', 'DE Coast',
                                   'New Jersey', 'NY Harbor', 'Long Island',
                                   'Massachusetts'), ordered = T))

reduced_pts <- distinct(secor.sb, transmitter, doy, array, .keep_all = T)

library(ggplot2)
# Manuscript copied at 860 * 550
ggplot() + geom_point(data = reduced_pts,
                      aes(x = doy, y = array, color = size.bin)) +
  scale_color_grey(start = 0.8, end = 0.2)+
  facet_wrap(~ year) +
  labs(x = 'Day of Year', y = NULL, color = "Length at Tagging") +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2),
        legend.justification = c(1, 0),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 12),
        axis.text.y.left = element_text(angle = 35),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12)) 


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

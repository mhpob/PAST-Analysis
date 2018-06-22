library(ggplot2); library(lubridate); library(dplyr)
load('secor.sb.rda')

base.data <- secor.sb %>% 
  mutate(coastal = case_when(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                         'NYB', 'Hudson', 'Long Island', 'Mass',
                                         'New Jersey') ~ T,
                            T ~ F),
         yr.adjust = case_when(date.local <= '2015-03-01' ~ 2014,
                               date.local > '2015-03-01' &
                                 date.local <= '2016-03-01' ~ 2015,
                               date.local > '2016-03-01' &
                                 date.local <= '2017-03-01' ~ 2016,
                               date.local > '2017-03-01' &
                                 date.local <= '2018-03-01' ~ 2017,
                               T ~ 2018),
         wk.num = ifelse(week(date.local) >= 13,
                         week(date.local) - 13,
                         week(date.local) + 40),
         wk = floor_date(date.local, unit = 'week'))

# First week of coastal incidence ----
first.coast <- base.data %>% 
  filter(coastal == T) %>% 
  group_by(transmitter, yr.adjust) %>% 
  summarize(c.firstnum = min(wk.num))

lab.func <- function(x){
  month(ymd('2014-03-21') %m+% weeks(x), label = T, abbr = T)
}

ggplot() + geom_histogram(data = first.coast, aes(c.firstnum), bins = 52) +
  facet_wrap(~ yr.adjust, ncol = 1) +
  scale_x_continuous(labels = lab.func) +
  labs(x = 'Arrival in coastal waters', y = 'Count') +
  theme_bw()

# First week of return to Chesapeake ----
first.return <- base.data %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>% 
  left_join(first.coast) %>% 
  filter(wk.num > c.firstnum,
         coastal == F) %>% 
  summarize(b.firstnum = min(wk.num))

ggplot() + geom_histogram(data = first.return, aes(b.firstnum), bins = 52) +
  facet_wrap(~ yr.adjust, ncol = 1) +
  scale_x_continuous(labels = lab.func) +
  labs(x = 'Return to Chesapeake Bay', y = 'Count') +
  theme_bw()

# First week above Rt 301 ----
pot.return <- base.data %>% 
  filter(month(date.local) %in% 2:5,
         # date.local > '2014-06-01',
         array %in% c('Upper Potomac', 'Mid Potomac')) %>% 
  mutate(year = year(date.local),
         sex = ifelse(sex == '', 'Unknown', sex)) %>% 
  group_by(transmitter, year, sex) %>% 
  summarize(p.firstnum = min(wk.num))

ggplot() + geom_histogram(data = pot.return, aes(p.firstnum, fill = sex),
                          bins = 52) +
  facet_wrap(~ year, ncol = 1) +
  scale_x_continuous(labels = lab.func) +
  labs(x = 'Movement above Rt. 301', y = 'Count', fill = 'Sex') +
  theme_bw()

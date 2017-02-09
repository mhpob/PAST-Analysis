library(ggplot2); library(lubridate); library(dplyr)
load('secor.sb.rda')

base.data <- secor.sb %>% 
  mutate(coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                       'Hudson', 'Long Island', 'Mass',
                                       'New Jersey'), T, F),
         yr.adjust = ifelse(date.local <= '2015-03-21', 2014,
                            ifelse(date.local > '2015-03-21' &
                                     date.local <= '2016-03-21', 2015,
                                   2016)),
         wk.num = ifelse(week(date.local) >= 13,
                         week(date.local) - 13,
                         week(date.local) + 40),
         wk = floor_date(date.local, unit = 'week'))

# First week of coastal incidence ----
first.coast <- base.data %>% 
  filter(coastal == T) %>% 
  group_by(transmitter, yr.adjust) %>% 
  summarize(c.firstnum = min(wk.num),
            c.firstwk = min(wk))

ggplot() + geom_histogram(data = first.coast, aes(c.firstnum)) +
  facet_wrap(~ yr.adjust, ncol = 1)

# First week of return to Chesapeake ----
first.return <- base.data %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>% 
  left_join(first.coast) %>% 
  filter(wk > c.firstwk,
         coastal == F) %>% 
  summarize(b.firstnum = min(wk.num),
            b.firstwk = min(wk))

ggplot() + geom_histogram(data = first.return, aes(b.firstnum), bins = 52) +
  facet_wrap(~ yr.adjust, ncol = 1)

# First week above Rt 301 ---- keep working...
left.pot <- base.data %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(!array %in% c('Upper Potomac', 'Mid Potomac', 'Lower Potomac')) %>%
  summarize(left.pot = min(wk)) %>% 
  left_join(base.data) %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(wk > left.pot,
         array %in% c('Upper Potomac', 'Mid Potomac')) %>% 
  summarize(p.firstnum = min(wk.num),
            p.firstwk = min(wk))

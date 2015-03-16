library(ggplot2); library(lubridate); library(dplyr)
load('secor.sb.rda')

secor.sb$system <-
  ifelse(secor.sb$array %in% c('Long Island', 'New Jersey'), 'NY Bight',
  ifelse(secor.sb$array %in% c('Upper MD Bay', 'Mid MD Bay', 'Lower MD Bay',
                  'Other', 'Patuxent'), 'Maryland',
  ifelse(secor.sb$array %in% c('Bay Mouth', 'Elizabeth', 'James',
                  'Rappahannock', 'York'), 'Virginia',
  secor.sb$array)))

secor.sb$trans.num <-
  ifelse(secor.sb$tag.date < ymd('2014-10-29', tz = 'America/New_York') &
           secor.sb$trans.num == '25465', '25465a',
  ifelse(secor.sb$tag.date >= ymd('2014-10-29', tz = 'America/New_York') &
           secor.sb$trans.num == '25465', '25465b',
         secor.sb$trans.num))

tag.info <- secor.sb %>% 
  distinct(trans.num, tag.date) %>% 
  mutate(tag.date = lubridate::ymd(tag.date, tz = 'America/New_York'),
         date.floor = lubridate::floor_date(tag.date, unit = 'day'),
         system = ifelse(tag.date < ymd('2014-10-29', tz = 'America/New_York'),
                         'Mid Potomac', 'Lower Potomac')) %>% 
  select(date.floor, trans.num, system, length)

secor.sb <- secor.sb %>%
  mutate(date.floor = lubridate::floor_date(date.local, unit = 'day')) %>% 
  distinct(date.floor, trans.num) %>% 
  select(date.floor, trans.num, system, length) %>%
  rbind(., tag.info)

secor.sb$trans.num <- factor(secor.sb$trans.num,
                             levels = secor.sb$trans.num[order(secor.sb$length)])

pot.cols <- colorRampPalette(c('lightgreen', 'darkgreen'))(3)
bay.cols <- colorRampPalette(c('red', 'orange'))(2)
else.cols <- colorRampPalette(c('blue', 'violet'))(4)

cols <- c('Upper Potomac' = pot.cols[1], 'Mid Potomac' = pot.cols[2],
          'Lower Potomac' = pot.cols[3], 'Maryland' = bay.cols[1],
          'Virginia' = bay.cols[2], 'C&D' = else.cols[1], 'Delaware' = else.cols[2],
          'Mass' = else.cols[3], 'NY Bight' = else.cols[4])


ggplot() + geom_raster(data = secor.sb, aes(x = date.floor, y = trans.num,
                                            fill = system)) +
  labs(x = 'Date', y = '', fill = 'System') +
  xlim(lubridate::ymd('2014-04-12'), lubridate::ymd('2015-02-25')) +
  scale_fill_manual(values = cols, breaks = c('Upper Potomac', 'Mid Potomac',
                        'Lower Potomac', 'Maryland', 'Virginia', 'C&D', 'Delaware',
                        'NY Bight', 'Mass'))+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

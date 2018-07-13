load('secor.sb.rda')

library(raster)
midstates <- shapefile('p:/obrien/midatlantic/matl_states_land.shp')

library(ggplot2)
midstates <- fortify(midstates)

base_map <- ggplot() +
  geom_polygon(data = midstates, aes(x = long, y = lat, group = group),
               fill  = 'lightgrey', color = 'black') +
  coord_map(xlim = c(-77.5, -69), ylim = c(36.5, 42.95)) +
  labs(x = NULL, y = NULL)

secor.sb$year <- lubridate::year(secor.sb$date.local)
stations <- unique(secor.sb[secor.sb$year != 2018, c('lat', 'long', 'year')])
base_map + geom_point(data = stations, aes(x = as.numeric(long), y = as.numeric(lat)),
                 color = 'red', size = 4, shape = 21) +
  facet_wrap(~year)

library(dplyr)
past2014 <- filter(secor.sb, grepl('-25', transmitter))
p14_stations <- unique(past2014[,c('lat', 'long')])
base_map + geom_point(data = p14_stations, aes(x = as.numeric(long),
                                               y = as.numeric(lat)),
                      color = 'red', size = 4, shape = 21) 


past2016 <- filter(secor.sb, grepl('-53', transmitter))
p16_stations <- unique(past2016[,c('lat', 'long')])
base_map + geom_point(data = p16_stations, aes(x = as.numeric(long),
                                               y = as.numeric(lat)),
                      color = 'red', size = 4, shape = 21) 

by_month <- secor.sb %>% 
  mutate(month = lubridate::month(date.local)) %>% 
  distinct(transmitter, month, lat, long) %>% 
  group_by(month, lat, long) %>% 
  summarize(n = n())


base_map + geom_point(data = by_month, aes(x = as.numeric(long),
                                           y = as.numeric(lat),
                                           size = n),
                      color = 'red', shape = 21) +
  facet_wrap(~month)

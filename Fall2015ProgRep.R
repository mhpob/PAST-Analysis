# Which tags have we not heard back from?
load('secor.sb.rda')
tags <- paste0('A69-1601-', seq(25434, 25533, 1))

tags[!tags %in% levels(factor(secor.sb$transmitter))]

library(dplyr)
fall.fish <- filter(secor.sb, date.local > lubridate::ymd('2014-10-29',
                                                  tz = 'America/New_York'))

tags2 <- c(paste0('A69-1601-', seq(25506, 25533, 1)), 'A69-1601-25465')
tags2[!tags2 %in% levels(factor(fall.fish$transmitter))]

# Updated map
library(raster); library(ggplot2); library(dplyr)
midstates <- shapefile('c:/users/secor lab/desktop/gis products/chesapeake/midatlantic/matl_states_land.shp')
pot <- midstates[midstates$STATE_ABBR %in% c('MD', 'VA', 'DC', 'DE'),]

pot <- fortify(pot)

stations <- read.csv('p:/obrien/biotelemetry/receivers/md csi receivers.csv',
                     stringsAsFactors = F)
stations <- stations[stations$Status %in% c('Deployed', 'Proposed'),]

load('secor.sb.rda')
det.sites <- unique(secor.sb[,6:7])

# Chesapeake
ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long, lat, group = group)) +
  coord_map(xlim = c(-77.35, -75.75), ylim = c(37.87, 39.6)) + 
  geom_point(data = filter(stations, Group %in% c('DDOE', 'CBL'),
                           Status == 'Deployed'),
             aes(x = Dec.Long, y = Dec.Lat, color = Group),
             size = 3.5) +
  geom_point(aes(x = -76.327180, y = 38.052251),
             col = 'red', shape = 7, size = 7) +
  geom_point(aes(x = -76.938432, y = 38.337408),
            col = 'red', shape = 7, size = 7) +
  labs(x = 'Longitude', y = 'Latitude') +
  theme_bw()

#All assets
stations <- read.csv('p:/obrien/biotelemetry/receivers/md csi receivers.csv',
                     stringsAsFactors = F)
stations <- stations %>% 
  filter(Status == 'Deployed') %>% 
  mutate(Group = ifelse(grepl('Cedar|Rt|Pine|Pier', Station), 'CBL - ASMFC',
                 ifelse(Group == 'CBL' & !grepl('Cedar|Rt|Pine|Pier', Station),
                        'CBL - Section 6',
                 ifelse(Group == 'MD DNR', 'MD DNR', 'Other'))))

ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, color = Group), size = 4) +
  scale_color_manual(values = c('blue', 'purple', 'red', 'orange')) +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat), size = 4, shape = 21) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 18),
                     legend.title = element_text(size = 20),
                     axis.text = element_text(size = 15),
                     axis.title = element_text(size = 18),
                     title = element_text(size = 20))

# How many detections were returned to us through ACT?
returns <- secor.sb %>% 
  filter(!grepl('Piney|Cedar|CBL|Kent|C&D|V-|T-|A-', station))

# Species, investigators, etc.
library(TelemetryR); library(dplyr)
dets <- vemsort('p:/obrien/biotelemetry/detections')
dets <- dets %>% 
  filter(!grepl('Choptank|Pocomoke|Nanticoke|Marshyhope', station))

species <- left_join(dets, ACTtrans, by = c('transmitter' = 'Tag.ID.Code.Standard'))

n_spec <- species %>% group_by(Common.Name, Primary.Researcher) %>% 
  distinct(transmitter) %>% 
  summarize(n = n())

# Residency
small <- filter(secor.sb, length <= 600)
escapes <- small %>% 
  filter(array %in% c('C&D', 'DE Coast', 'Delaware', 'Long Island',
                      'Mass', 'MD Coast', 'New Jersey', 'VA Coast'))
escapes <- levels(factor(escapes$trans.num))
bay.residents <- small %>% 
  filter(!trans.num %in% escapes)
bay.residents <- levels(factor(bay.residents$trans.num))
bay.residents <- small %>%
  filter(trans.num %in% bay.residents) %>% 
  arrange(date.local) %>% 
  data.frame

bay.residents <- split(bay.residents, bay.residents$trans.num)

lapply(bay.residents, gen.movement)

# Percent of big that don't leave
## Fish leaving Chesapeake Bay
bay.escapes <- secor.sb %>% 
  filter(length >= 800,
         date.local >= '2014-03-30',
         date.local <= '2014-10-29',
         array %in% c('Bay Mouth', 'C&D', 'DE Coast', 'Delaware', 'Long Island',
                      'Mass', 'MD Coast', 'New Jersey', 'VA Coast')) 
bay.escapes <- levels(factor(bay.escapes$trans.num))
bay.escapes <- secor.sb %>% 
  filter(trans.num %in% bay.escapes) %>%
  arrange(date.local) %>% 
  data.frame

big <- filter(secor.sb, length >= 800)
length(levels(factor(big$trans.num)))

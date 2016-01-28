library(raster); library(ggplot2); library(dplyr)
midstates <- shapefile('c:/users/secor lab/desktop/gis products/chesapeake/midatlantic/matl_states_land.shp')
pot <- midstates[midstates$STATE_ABBR %in% c('MD', 'VA', 'DC', 'DE'),]

pot <- fortify(pot)

stations <- read.csv('p:/obrien/biotelemetry/receivers/md csi receivers.csv',
                     stringsAsFactors = F)
stations <- stations[stations$Status %in% c('Deployed', 'Proposed'),]

load('secor.sb.rda')
det.sites <- unique(secor.sb[,6:7])

png('p:/obrien/biotelemetry/striped bass/MD Receivers_Group.png',
    width = 950, height = 600)
ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, color = Group), size = 3.5) +
  scale_color_manual(values = c('green', 'darkorange', 'blue', 'purple')) +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat), size = 3.5, shape = 21) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14))
dev.off()

# png('p:/obrien/biotelemetry/striped bass/MD Receivers_Status.png',
#     width = 950, heigh = 600)
ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, color = Status), size = 3.5) +
  scale_color_manual(values = c('green', 'yellow')) +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat), size = 3.5, shape = 21) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14))
# dev.off()

png('p:/obrien/biotelemetry/striped bass/MD Receivers_GroupStatus.png',
    width = 950, heigh = 600)
ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, shape = Group, color = Status),
             size = 3.5) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  scale_color_manual(values = c('green', 'yellow')) +
  geom_point(data = filter(stations, Group == "CBL"),
             aes(Dec.Long, Dec.Lat), size = 3.5, shape = 21) +
  geom_point(data = filter(stations, Group == "DDOE"),
             aes(Dec.Long, Dec.Lat), size = 3.5, shape = 24) +
  geom_point(data = filter(stations, Group == "MD DNR"),
             aes(Dec.Long, Dec.Lat), size = 3.5, shape = 22) +
  geom_point(data = filter(stations, Group == "SERC"),
             aes(Dec.Long, Dec.Lat), size = 3.5, shape = 23) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14))
dev.off()

ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6)) + 
  geom_point(data = det.sites, aes(x = long, y = lat),
             size = 6, color = 'blue') +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, color = Status), size = 3.5) +
  scale_color_manual(values = c('green', 'yellow')) +
  geom_point(aes(x = -76.327180, y = 38.052251),
             col = 'red', shape = 7, size = 7) +
  geom_point(aes(x = -76.938432, y = 38.337408),
            col = 'red', shape = 7, size = 7) +
  labs(x = 'Longitude', y = 'Latitude',
       title = 'Maryland Receivers (Stations with Striped Bass Detections in Blue)') +
  theme_bw() + theme(legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14))


ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.35, -76.22), ylim = c(37.87, 39)) + 
  geom_point(data = unique(secor.sb[secor.sb$array %in%
                      c('Piney Point', 'Rt 301', 'DDOE'),6:7]),
             aes(x = long, y = lat),
             size = 6, color = 'blue') +
  geom_point(data = stations[stations$System == 'Potomac',],
             aes(Dec.Long, Dec.Lat), size = 3.5, color = 'green') +
  geom_point(aes(x = -76.327180, y = 38.052251),
             col = 'red', shape = 7, size = 7) +
  geom_point(aes(x = -76.938432, y = 38.337408),
            col = 'red', shape = 7, size = 7) +
  labs(x = 'Longitude', y = 'Latitude',
       title = 'Potomac Receivers (Stations with Striped Bass Detections in Blue)') +
  theme_bw()


## Whole Bay
cbrecs <- secor.sb %>% filter(secor.sb$array %in% c('Bay Mouth', 'C&D',
            'Choptank', 'Elizabeth', 'James', 'Lower MD Bay', 'Lower Potomac',
            'MD Coast', 'Mid MD Bay', 'Mid Potomac', 'Patuxent', 'Rappahannock',
            'Upper MD Bay', 'Upper Potomac', 'York')) %>% 
  data.frame()
arr <- function(part){grepl(part, cbrecs[, 'station'], ignore.case = T)}

cbrecs$Group <- ifelse(arr('pot'), 'NOAA',
            ifelse(arr('elk|&| 32'), 'DSU',
            ifelse(arr('rapp|vims'), 'VIMS',
            ifelse(arr('^y|^nn|^nh|B1|CBB|LC|ts\\d|\\dch') | 
                     cbrecs$station %in% c('APM1', 'CC LS', 'CH', 'NCD'), 'Navy',
                    'Other'))))

sites <- rbind(stations[, c(10,12,13)],
              setNames(unique(cbrecs[, c(14, 6, 7)]),
                       names(stations[, c(10,12,13)])))

ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -75.3), ylim = c(36.8, 39.6)) +
  geom_point(data = sites,
             aes(Dec.Long, Dec.Lat, color = Group), size = 5) +
  scale_color_manual(values = c('green', 'darkorange', 'blue', 'yellow',
                                'purple', 'pink', 'gold', 'red')) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Chesapeake Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14))


### Difference between ASMFC and Section 6 receivers
stations <- stations %>% 
  mutate(Group = ifelse(grepl('Cedar|Rt|Pine|Pier', Station), 'CBL - ASMFC',
                 ifelse(Group == 'CBL' & !grepl('Cedar|Rt|Pine|Pier', Station),
                        'CBL - Section 6', Group)))

ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, color = Group), size = 5) +
  scale_color_manual(values = c('darkgreen', 'lightgreen', 'darkorange',
                                'blue', 'purple')) +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat), size = 5, shape = 21) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 18),
                     legend.title = element_text(size = 20),
                     axis.text = element_text(size = 15),
                     axis.title = element_text(size = 18),
                     title = element_text(size = 20))

### Difference between USCG, Humpty, and coastal receiver attachment
stations <- stations %>% 
  mutate(Group = ifelse(grepl('T-|V-|A-', Station), 'CBL - Coastal',
                 ifelse(grepl('Kent', Station) | Station %in% c('Piney Point - A',
                        'Piney Point - C', 'Piney Point - D', 'Cedar Point - A'),
                        'CBL - Humpty Buoy', Group)),
         Group = ifelse(Group == 'CBL', 'CBL - USCG', Group),
         Group = ifelse(Station == 'CBL Pier', 'CBL Pier', Group))

cblcols <- colorRampPalette(c('darkgreen', 'yellow'))(4)

ggplot() + geom_polygon(data = pot, fill = 'darkgrey', color = 'black',
                        aes(long,lat, group = group)) +
  coord_map(xlim = c(-77.4, -74), ylim = c(37.8, 39.6))  +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat, color = Group), size = 5) +
  scale_color_manual(values = c(cblcols[1], cblcols[2], cblcols[4], cblcols[3],
                                'darkorange', 'blue', 'purple')) +
  geom_point(data = stations,
             aes(Dec.Long, Dec.Lat), size = 5, shape = 21) +
  labs(x = 'Longitude', y = 'Latitude', title = 'Maryland Receivers') +
  theme_bw() + theme(legend.text = element_text(size = 18),
                     legend.title = element_text(size = 20),
                     axis.text = element_text(size = 15),
                     axis.title = element_text(size = 18),
                     title = element_text(size = 20))

# This creates an animated map of detections, which has a dot whose size is
# scaled by the number of individual fish detected that day. You need to have ggplot,
# raster (if you want to use a shapefile) or OpenStreetMap (if you want a
# satellite image), and animation packages installed. The animation package 
# also needs you to install ImageMagick (http://www.imagemagick.org) in order
# to create .gif files.

library(ggplot2); library(raster); library(animation); library(dplyr)
load('secor.sb.rda')

# Round down date/time
secor.sb <- secor.sb %>%
  mutate(date.floor = lubridate::floor_date(date.local, unit = 'day'))

anim.data <- secor.sb %>%
  # Drop repeated detections within the same day
  # (Unique trans.num, station, date.floor combinations)
  group_by(trans.num, station, date.floor) %>%
  distinct() %>%
  # Total number of fish detected per reciever per day
  group_by(station, date.floor) %>%
  summarize(tot.detect = n()) %>%
  # Merge back in station locations
  left_join(distinct(secor.sb[, c(5:7, 14)]))

# Attach date/place where the fish were tagged (i.e., their first observation)
anim.data <- rbind(anim.data, 
                    c('Piccowaxen', '2014-03-30', 18, 38.337413, -76.938424),
                    c('Piccowaxen', '2014-04-01', 27, 38.337413, -76.938424),
                    c('Piccowaxen', '2014-04-04', 13, 38.337413, -76.938424),
                    c('Piccowaxen', '2014-04-07', 5, 38.337413, -76.938424),
                    c('Piccowaxen', '2014-04-11', 8, 38.337413, -76.938424),
                    c('Pt Lookout', '2014-10-30', 29, 38.051951, -76.327386))
for(i in 3:5) {anim.data[,i] <- as.numeric(data.frame(anim.data)[, i])}
anim.data[, 2] <- as.Date(data.frame(anim.data)[, 2])

anim.data <- data.frame(anim.data)

# Maryland-only coords: c(39.356, -77.371), c(37.897, -75.626)
# Note: If you use MD-only map, you have to filter outside points off.
# MAB: c(42, -77.5), c(36.5, -69)
library(OpenStreetMap)
map <- openmap(c(42.95, -77.5), c(36.5, -69), type = 'mapquest-aerial')
map <- autoplot.OpenStreetMap(openproj(map))

# Use code below if there is a shapefile you'd like to use. Note that capitalization matters in actual file name!!
# mapdat <- shapefile('p:/obrien/gis/shapefiles/10m coastline_natural earth/ne_10m_land.shp')
# mapdat <- fortify(mapdat)
# map <- ggplot() + geom_path(data = mapdat, aes(long, lat, group = group)) +
#   coord_map(xlim = c(-77.5, -69), ylim = c(36.5, 42))

dates <- seq(as.Date('2014-03-30'),
             as.Date('2014-12-11'), by = 'day')
max.freq <- max(anim.data$tot.detect)

# # Map with no inset
# saveHTML({
#   for (i in 1:length(dates)){
#   plot <- map + geom_point(data = filter(anim.data, date.floor == dates[i]),
#                       aes(x = long, y = lat, size = Freq), color = 'red') +
#                    scale_size_area(limits = c(1,27), 
#                                    breaks = c(1,2,3,seq(4,16,2),27),
#                                    max_size = 20)+
#                   annotate("text", x = -76, y = 42, size = 10,
#                            label = dates[i], color = 'white') +
#                   ggtitle('Striped Bass Detections') +
#                   theme(legend.position = 'none',
#                         plot.background = element_blank(),
#                         axis.text = element_blank(),
#                         axis.title = element_blank(),
#                         rect = element_blank(),
#                         line = element_blank())
#   print(plot)
#   ani.pause()
#   }
#   for(k in 1:3){
#     print(plot)
#     ani.pause()
#   }
#   }, interval = 0.5, verbose = F, nmax = length(dates), navigator = F,
#   outdir = 'c:/users/secor lab/desktop/animation')



## Create an inset map of MD Chesapeake Bay
map2 <- openmap(c(39.356, -77.371), c(37.897, -75.626),
                type = 'mapquest-aerial')
map2 <- autoplot.OpenStreetMap(openproj(map2))
 
saveVideo({
  for (i in 1:length(dates)){
  plot <- map + geom_point(data = filter(anim.data, date.floor == dates[i]),
                      aes(x = long, y = lat, size = tot.detect),
                      color = 'red') +
                   scale_size_area(limits = c(1, 29), 
                                   breaks = c(1, 2, 3, seq(4, 20, 2), 27, 29),
                                   max_size = 20)+
                  annotate("text", x = -76, y = 41.5, size = 10,
                           label = dates[i], color = 'white') +
                  ggtitle('Striped Bass Detections') +
                  theme(legend.position = 'none',
                        plot.background = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        rect = element_blank(),
                        line = element_blank())
  
  plot2 <- ggplotGrob(map2 + geom_point(data =
                               filter(anim.data, date.floor == dates[i],
                                        lat >= 37.897, lat <= 39.356,
                                        long <= -75.626, long >= -77.371),
                   aes(x = long, y = lat, size = tot.detect), color = 'red') +
                   scale_size_area(limits = c(1, 29), 
                                   breaks = c(1, 2, 3, seq(4, 20, 2), 27, 29),
                                   max_size = 20)+
                  theme(legend.position = 'none',
                        plot.background = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        rect = element_blank(),
                        line = element_blank()))
  
  plot <- plot + annotation_custom(plot2, xmin = -72.9, xmax = Inf,
                                          ymin = 36.2, ymax = 41)
  
  print(plot)
  ani.pause()
  }
  for(k in 1:3){
    print(plot)
    ani.pause()
  }
  }, interval = 0.2, video.name = 'sbani.mp4',
  ffmpeg = 'c:/ffmpeg/bin/ffmpeg.exe',
  ani.height = 720, ani.width = 1280,
  other.opts = "-b 300k")

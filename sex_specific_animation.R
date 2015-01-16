library(ggplot2); library(raster); library(animation); library(dplyr)
load('secor.sb.rda')

secor.sb <- secor.sb %>%
  mutate(date.floor = lubridate::floor_date(date.local, unit = 'day'))

anim.data <- secor.sb %>%
  filter(sex %in% c('M', 'F')) %>%  #use this to drop UNID sexes
  group_by(trans.num, station, date.floor) %>%
  distinct() %>%
  group_by(station, date.floor, sex) %>%
  summarize(tot.detect = n()) %>%
  left_join(distinct(secor.sb[, c(5:7, 14)]))

# Attach date/place where the fish were tagged (i.e., their first observation)
anim.data <- rbind(anim.data,
                   #c('Piccowaxen', '2014-03-30', '', 18, 38.337413, -76.938424),
                   c('Piccowaxen', '2014-04-01', 'M', 11, 38.337413, -76.938424),
                   c('Piccowaxen', '2014-04-01', 'F', 5, 38.337413, -76.938424),
                   #c('Piccowaxen', '2014-04-01', '', 12, 38.337413, -76.938424),
                   c('Piccowaxen', '2014-04-04', 'M', 8, 38.337413, -76.938424),
                   c('Piccowaxen', '2014-04-04', 'F', 5, 38.337413, -76.938424),
                   c('Piccowaxen', '2014-04-07', 'M', 4, 38.337413, -76.938424),
                   c('Piccowaxen', '2014-04-07', 'F', 1, 38.337413, -76.938424),
                   c('Piccowaxen', '2014-04-11', 'M', 1, 38.337413, -76.938424),
                   c('Piccowaxen', '2014-04-11', 'F', 7, 38.337413, -76.938424),
                   #c('Pt Lookout', '2014-10-30', '', 7, 38.051951, -76.327386),
                   c('Pt Lookout', '2014-10-30', 'M', 19, 38.051951, -76.327386),
                   c('Pt Lookout', '2014-10-30', 'F', 3, 38.051951, -76.327386))
for(i in 4:6) {anim.data[,i] <- as.numeric(data.frame(anim.data)[, i])}
anim.data[, 2] <- as.Date(data.frame(anim.data)[, 2])

anim.data <- data.frame(anim.data)


library(OpenStreetMap)
map <- openmap(c(42.95, -77.5), c(36.5, -69), type = 'mapquest-aerial')
map <- autoplot.OpenStreetMap(openproj(map))

dates <- seq(as.Date('2014-03-30'),
             as.Date('2015-01-15'), by = 'day')

map2 <- openmap(c(39.356, -77.371), c(37.897, -75.626),
                type = 'mapquest-aerial')
map2 <- autoplot.OpenStreetMap(openproj(map2))
 
saveVideo({
  for (i in 1:length(dates)){
  plot <- map + geom_point(data = filter(anim.data, date.floor == dates[i]),
                      aes(x = long, y = lat, size = tot.detect, color = sex),
                      position = position_jitter(w = 0.05, h = 0.05)) +
                scale_size_area(limits = c(1, 19),
                                breaks = c(1, 2, 3, seq(4, 12, 2), 18, 19),
                                max_size = 20, guide = F) +
                scale_color_manual(values = c('pink', 'lightblue'),
                                   name = 'Sex', labels = c('F', 'M'),
                                   limits = c('F', 'M'),
                                   guide = guide_legend(override.aes =
                                                          list(size=10))) +
                annotate("text", x = -76, y = 41.5, size = 10,
                           label = dates[i], color = 'white') +
                ggtitle('Striped Bass Detections') +
                theme(plot.background = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        rect = element_blank(),
                        line = element_blank())
  
  plot2 <- ggplotGrob(map2 + geom_point(data =
                               filter(anim.data, date.floor == dates[i],
                                        lat >= 37.899, lat <= 39.354,
                                        long <= -75.626, long >= -77.371),
                   aes(x = long, y = lat, size = tot.detect, color = sex),
                   position = position_jitter(w = 0.05, h = 0.05)) +
                   scale_size_area(limits = c(1, 19),
                                breaks = c(1, 2, 3, seq(4, 12, 2), 18, 19),
                                max_size = 20) +
                   scale_color_manual(values = c('pink', 'lightblue'),
                                      limits = c('F', 'M')) +
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
  }, interval = 0.5, video.name = 'sb_sexspec_ani.mp4',
  ffmpeg = 'C:/Program Files/ImageMagick-6.9.0-Q8/ffmpeg.exe',
  ani.height = 720, ani.width = 1280,
  other.opts = "-b 300k")
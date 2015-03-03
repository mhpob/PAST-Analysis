library(ggplot2); library(raster); library(animation); library(dplyr)
load('secor.sb.rda')

secor.sb <- secor.sb %>%
  mutate(date.floor = lubridate::floor_date(date.local, unit = 'day'),
         length.bin = ifelse(length < 550, '<55',
                      ifelse(length >= 550 & length < 650, '55-65',
                      ifelse(length >= 650 & length < 800, '65-80', '>80'))))

anim.data <- secor.sb %>%
  group_by(trans.num, station, date.floor) %>%
  distinct() %>%
  group_by(station, date.floor, length.bin) %>%
  summarize(tot.detect = n()) %>%
  left_join(distinct(secor.sb[, c(5:7, 14)]))

# Attach date/place where the fish were tagged (i.e., their first observation)
anim.data <- rbind(anim.data,
              c('Piccowaxen', '2014-03-30', '55-65', 11, 38.337413, -76.938424),
              c('Piccowaxen', '2014-03-30', '65-80', 4, 38.337413, -76.938424),
              c('Piccowaxen', '2014-03-30', '>80', 3, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-01', '55-65', 10, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-01', '65-80', 14, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-01', '>80', 4, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-04', '65-80', 9, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-04', '>80', 4, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-07', '55-65', 1, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-07', '65-80', 1, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-07', '>80', 3, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-11', '65-80', 1, 38.337413, -76.938424),
              c('Piccowaxen', '2014-04-11', '>80', 7, 38.337413, -76.938424),
              c('Pt Lookout', '2014-10-30', '<55', 20, 38.051951, -76.327386),
              c('Pt Lookout', '2014-10-30', '55-65', 7, 38.051951, -76.327386),
              c('Pt Lookout', '2014-10-30', '65-80', 2, 38.051951, -76.327386))
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
                  aes(x = long, y = lat, size = tot.detect, color = length.bin),
                  position = position_jitter(w = 0.1, h = 0.1)) +
                scale_size_area(limits = c(1, 20),
                                breaks = c(1, 2, 3, seq(4, 12, 2), 18, 20),
                                max_size = 20, guide = F) +
                scale_color_manual(values = c('pink', 'lightblue',
                                              'lightgreen', 'yellow'),
                                   name = 'Length (cm)',
                                   labels = c('<55', '55-65', '65-80', '>80'),
                                   limits = c('<55', '55-65', '65-80', '>80'),
                                   guide = guide_legend(override.aes =
                                                          list(size = 10))) +
                annotate("text", x = -76, y = 41.5, size = 10,
                           label = dates[i], color = 'white') +
                ggtitle('Striped Bass Detections') +
                theme(plot.background = element_blank(),
                      axis.text = element_blank(),
                      axis.title = element_blank(),
                      rect = element_blank(),
                      line = element_blank(),
                      legend.text = element_text(size = 20),
                      legend.title = element_text(size = 20),
                      title = element_text(size = 20))
  
  plot2 <- ggplotGrob(map2 + geom_point(data =
                               filter(anim.data, date.floor == dates[i],
                                        lat >= 37.899, lat <= 39.354,
                                        long <= -75.626, long >= -77.371),
                   aes(x = long, y = lat, size = tot.detect, color = length.bin),
                   position = position_jitter(w = 0.05, h = 0.05)) +
                   scale_size_area(limits = c(1, 20),
                                breaks = c(1, 2, 3, seq(4, 12, 2), 18, 20),
                                max_size = 20) +
                   scale_color_manual(values = c('pink', 'lightblue',
                                              'lightgreen', 'yellow'),
                                  limits = c('<55', '55-65', '65-80', '>80')) +
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
  }, interval = 0.5, video.name = 'sb_sizespec_ani.avi',
  ffmpeg = 'C:/Program Files/ImageMagick-6.9.0-Q8/ffmpeg.exe',
  ani.height = 720, ani.width = 1280)
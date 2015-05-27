library(ggplot2); library(raster); library(animation); library(dplyr)
load('secor.sb.rda')

secor.sb <- secor.sb %>%
  mutate(date.floor = lubridate::floor_date(date.local, unit = 'day'))

### Delaware Harvested ---------------------------------------------------------
deharv <- secor.sb %>%
  filter(trans.num == 25479) %>% 
  group_by(station, date.floor) %>%
  distinct() %>%
  # Total number of fish detected per reciever per day
  group_by(station, date.floor) %>%
  summarize(tot.detect = n()) %>%
  # Merge back in station locations
  left_join(distinct(secor.sb[, c(5:7, 14)]))

anim.data <- rbind(deharv,
                   c('Piccowaxen', '2014-04-01', 1, 38.337413, -76.938424))
for(i in 3:5) {anim.data[,i] <- as.numeric(data.frame(anim.data)[, i])}
anim.data[, 2] <- as.Date(data.frame(anim.data)[, 2])

anim.data <- data.frame(anim.data)

library(OpenStreetMap)
map <- openmap(c(39.8, -77.349), c(37.897, -75.359), type = 'mapquest-aerial')
map <- autoplot.OpenStreetMap(openproj(map))

dates <- seq(as.Date('2014-04-01'),
             as.Date('2014-06-21'), by = 'day')
max.freq <- max(anim.data$tot.detect)

# Map with no inset
saveVideo({
  for (i in 1:length(dates)){
  plot <- map + geom_point(data = filter(anim.data, date.floor == dates[i]),
                      aes(x = long, y = lat), color = 'red', size = 10) +
                  annotate("text", x = -76.7, y = 39.5, size = 10,
                           label = dates[i], color = 'white') +
                  theme(legend.position = 'none',
                        plot.background = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        rect = element_blank(),
                        line = element_blank())
  print(plot)
  ani.pause()
  }
  for(k in 1:3){
    print(plot)
    ani.pause()
  }
  }, interval = 0.2, video.name = 'deharv.avi',
  ffmpeg = 'c:/ffmpeg/bin/ffmpeg.exe',
  ani.height = 720, ani.width = 1280)

### Mouth to Mass --------------------------------------------------------------
m2m <- secor.sb %>%
  filter(trans.num %in% c(25458, 25490)) %>% 
  group_by(trans.num, station, date.floor) %>%
  distinct() %>% 
  select(one_of(c('trans.num', 'date.floor', 'lat', 'long'))) %>% 
  data.frame()

anim.data <- rbind(m2m,
                   c('Piccowaxen', 25458, '2014-04-01', 38.337413, -76.938424),
                   c('Piccowaxen', 25490, '2014-04-04', 38.337413, -76.938424))
for(i in c(2,4,5)) {anim.data[,i] <- as.numeric(data.frame(anim.data)[, i])}
anim.data[, 3] <- as.Date(data.frame(anim.data)[, 3])


library(OpenStreetMap)
map <- openmap(c(42.95, -77.5), c(36.5, -69), type = 'mapquest-aerial')
map <- autoplot.OpenStreetMap(openproj(map))

dates <- seq(as.Date('2014-04-01'),
             as.Date('2014-12-09'), by = 'day')

# Map with no inset
saveVideo({
  for (i in 1:length(dates)){
  plot <- map + geom_point(data = filter(anim.data, date.floor == dates[i]),
                      aes(x = long, y = lat, color = factor(trans.num)), size = 5) +
                  annotate("text", x = -76, y = 41.5, size = 10,
                           label = dates[i], color = 'white') +
                  scale_color_manual(values = c('blue', 'red'),
                                  limits = c('25458', '25490')) +
                  theme(legend.position = 'none',
                        plot.background = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        rect = element_blank(),
                        line = element_blank())
  print(plot)
  ani.pause()
  }
  for(k in 1:3){
    print(plot)
    ani.pause()
  }
  }, interval = 0.2, video.name = 'cbres.avi',
  ffmpeg = 'c:/ffmpeg/bin/ffmpeg.exe',
  ani.height = 720, ani.width = 1280)

### Potomac Resident -----------------------------------------------------------
potres <- secor.sb %>% 
  filter(trans.num == 25447) %>% 
  group_by(trans.num, station, date.floor) %>%
  distinct() %>% 
  select(one_of(c('trans.num', 'date.floor', 'lat', 'long'))) %>% 
  data.frame()

anim.data <- rbind(potres,
                   c('Piccowaxen', 25447, '2014-03-30', 38.337413, -76.938424))
for(i in c(2,4,5)) {anim.data[,i] <- as.numeric(data.frame(anim.data)[, i])}
anim.data[, 3] <- as.Date(data.frame(anim.data)[, 3])

library(OpenStreetMap)
map <- openmap(c(39, -77.35), c(37.87, -76.22), type = 'mapquest-aerial')
map <- autoplot.OpenStreetMap(openproj(map))

dates <- seq(as.Date('2014-04-01'),
             as.Date('2015-01-25'), by = 'day')

saveVideo({
  for (i in 1:length(dates)){
  plot <- map + geom_point(data = filter(anim.data, date.floor == dates[i]),
                      aes(x = long, y = lat), color = 'red', size = 5) +
                  annotate("text", x = -77.1, y = 38.95, size = 10,
                           label = dates[i], color = 'white') +
                  theme(legend.position = 'none',
                        plot.background = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        rect = element_blank(),
                        line = element_blank())
  print(plot)
  ani.pause()
  }
  for(k in 1:3){
    print(plot)
    ani.pause()
  }
  }, interval = 0.2, video.name = 'potres.avi',
  ffmpeg = 'c:/ffmpeg/bin/ffmpeg.exe',
  ani.height = 720, ani.width = 1280)

### Bay Runs -------------------------------------------------------------------
bayres <- secor.sb %>% 
  filter(trans.num == 25491) %>% 
  group_by(trans.num, station, date.floor) %>%
  distinct() %>% 
  select(one_of(c('trans.num', 'date.floor', 'lat', 'long'))) %>% 
  data.frame()

anim.data <- rbind(bayres,
                   c('Piccowaxen', 25491, '2014-04-04', 38.337413, -76.938424))
for(i in c(2,4,5)) {anim.data[,i] <- as.numeric(data.frame(anim.data)[, i])}
anim.data[, 3] <- as.Date(data.frame(anim.data)[, 3])

library(OpenStreetMap)
map <- openmap(c(39.8, -77.349), c(36.75, -75.7), type = 'mapquest-aerial')
map <- autoplot.OpenStreetMap(openproj(map))

dates <- seq(as.Date('2014-04-04'),
             as.Date('2014-12-24'), by = 'day')

saveVideo({
  for (i in 1:length(dates)){
  plot <- map + geom_point(data = filter(anim.data, date.floor == dates[i]),
                      aes(x = long, y = lat), color = 'red', size = 5) +
                  annotate("text", x = -76.7, y = 39.5, size = 10,
                           label = dates[i], color = 'white') +
                  theme(legend.position = 'none',
                        plot.background = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        rect = element_blank(),
                        line = element_blank())
  print(plot)
  ani.pause()
  }
  for(k in 1:3){
    print(plot)
    ani.pause()
  }
  }, interval = 0.2, video.name = 'bayres.avi',
  ffmpeg = 'c:/ffmpeg/bin/ffmpeg.exe',
  ani.height = 720, ani.width = 1280)
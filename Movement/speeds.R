## See distances.R for creation of 'distances.csv'.
library(dplyr)
load('secor.sb.rda')

secor.sb$system <- ifelse(secor.sb$station %in% c('Report1', 'Report2') |
                            secor.sb$array %in% c('CBIBS', 'DDOE', 'Rt 301',
                                                  'Piney Point'),
                            'Potomac',
              ifelse(secor.sb$station %in% c('Report3', 'Report5') |
                       secor.sb$array %in% c('Kent Island'),
                            'Upper Bay',
              ifelse(secor.sb$array %in% c('CBL Pier', 'Cedar Point', 'SERC'),
                            'Mid Bay',
              ifelse(secor.sb$station == 'Urbanna Crk/Rapp' |
                       secor.sb$array %in% c('Navy', 'York'),
                            'Lower Bay',
              ifelse(secor.sb$station %in% c('Report4') |
                       secor.sb$array %in% c('New Jersey', 'Long Island'),
                            'MAB',
                            'NE')))))
secor.sb <- secor.sb %>% 
  filter(tag.date < '2014-10-01',
         !grepl('Report', station)) %>% 
  mutate(phase = ifelse(date.local >= '2014-04-01' & date.local <= '2014-05-15',
                        'spawning', 'running'),
         length.bin = ifelse(length < 550, '<55',
                      ifelse(length >= 550 & length < 650, '55-65',
                      ifelse(length >= 650 & length < 800, '65-80', '>80'))),
                      length.bin = factor(length.bin,
                                   levels = c('<55', '55-65', '65-80', '>80'),
                                   ordered = T)) %>% 
  arrange(date.local) %>% 
  as.data.frame()

dist <- read.csv('movement/distances.csv', stringsAsFactors = F)
row.names(dist) <- dist[, 1]
names(dist)[2:dim(dist)[2]] <- dist[, 1]
dist <- dist[, 2:dim(dist)[2]]

d.adj <- function(data){
  spl <- split(secor.sb, secor.sb[, 'transmitter'])
  for(k in 1:length(spl)) {
    for(i in 1:dim(spl[[k]])[1]) {
      if(i == dim(spl[[k]])[1] | i == 1){
        spl[[k]]$dist[i] <- 0
      }
      else{
        a <- as.character(spl[[k]][i, 'station'])
        b <- as.character(spl[[k]][i + 1, 'station'])
        spl[[k]]$dist[i + 1] <- dist[a, b]
      }
    }
  }
  
  for(k in 1:length(spl)) {
    for(i in 1:dim(spl[[k]])[1]) {
      spl[[k]]$time[i] <- ifelse(i == 1, 0,
                          as.numeric(difftime(spl[[k]][i, 'date.utc'],
                                              spl[[k]][i-1, 'date.utc'],
                                              units= 'secs')))
    }
  }
  do.call(rbind.data.frame, spl)
}

secor.sb <- d.adj(secor.sb)

speed <- function(data){
  hold <- data %>% mutate(mean.sp = dist / time * 1000,
                      mean.sp.bl = (mean.sp * 1000) / length,
                      max.sp = length / 1000 * 8) #(8 body lengths/s in m/s)
  
  hold <- hold[!is.infinite(hold$mean.sp) & !is.infinite(hold$mean.sp.bl) &
                 !is.na(hold$mean.sp) & !is.na(hold$mean.sp.bl),]

  hold <- hold[hold$mean.sp <= hold$max.sp,]
  hold <- hold[hold$mean.sp.bl <= 8,] #(8 body lengths/s)
}

secor.sb <- speed(secor.sb)


rm(dist, d.adj, speed)

save(secor.sb, file = 'movement/sb_speed.rda')

# ## Plotting --------------------------------------------------------------------
# library(ggplot2)
# 
# ggplot() + geom_histogram(data = filter(secor.sb,
#                                         mean.sp.bl > 0,
#                                         sex %in% c('M', 'F')),
#                           aes(x= mean.sp.bl, fill = sex),
#                           binwidth = 0.5, position = 'dodge')
# 
# ggplot() + geom_histogram(data = filter(secor.sb,
#                                         mean.sp.bl > 0),
#                           aes(x= mean.sp, fill = length.bin),
#                           binwidth = 0.5, position = 'dodge')
# 
# ggplot() + geom_histogram(data = filter(secor.sb,
#                                         mean.sp.bl > 0),
#                           aes(x= mean.sp, fill = phase),
#                           binwidth = 0.5, position = 'dodge')
# 
# ggplot() + geom_bar(data = filter(secor.sb,
#                                   mean.sp.bl > 0),
#                     aes(x= mean.sp.bl, fill = length.bin),
#                     binwidth = 0.5, position = 'dodge') +
#   labs(x = expression('Mean Speed (Body length sec' ^-1 *')'))
# 
# ggplot() + geom_bar(data = filter(secor.sb, mean.sp > 0, sex %in% c('M', 'F')),
#                     aes(x= mean.sp.bl, y = ..density.., fill = sex),
#                     binwidth = 0.5, position = 'dodge') +
#   facet_wrap(~system) +
#   labs(x = expression('Mean Speed (Body length sec' ^-1 *')'),
#        y = 'Density')
# 
# ggplot() + geom_point(data = filter(secor.sb, mean.sp > 0),
#                       aes(x = date.local, y = mean.sp.bl,
#                           color = sex))+
#   facet_wrap(~system)
# 
# 
# ## Leftover stuff I might need later.
# ches <- old %>%
#   filter(lat >= 36.871, lat <= 39.648, long >= -77.498, long <= -75.619)
# 
# left <- old %>%
#   anti_join(ches, by = 'station')
# 
# k <- left %>%
#   select(transmitter, station, length, weight, sex) %>%
#   distinct()
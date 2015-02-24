## Data Import -----------------------------------------------------------------
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

dist <- read.csv('distances.csv', stringsAsFactors = F)
row.names(dist) <- dist[, 1]
names(dist)[2:dim(dist)[2]] <- dist[, 1]
dist <- dist[,2:dim(dist)[2]]

d.adj <- function(data){
  spl <- split(data, data[, 'transmitter'])
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

spawn <- d.adj(secor.sb)

speed <- function(data){
  hold <- data %>% mutate(mean.sp = dist / time * 1000,
                      mean.sp.bl = (mean.sp * 1000) / length,
                      max.sp = length / 1000 * 8) #(8 body lengths/s in m/s)
  
  hold[is.na(hold)] <- 0
  hold[is.infinite(hold$mean.sp), 'mean.sp'] <- 0
  hold[is.infinite(hold$mean.sp.bl), 'mean.sp.bl'] <- 0
  hold[hold$mean.sp > hold$max.sp, 'mean.sp'] <- 0
  hold[hold$mean.sp.bl > 8, 'mean.sp.bl'] <- 0 #(8 body lengths/s)
  hold
}

spawn <- speed(spawn)


rm(dist, secor.sb, d.adj, speed)

#Lévy work ---------------------------------------------------------------------
binned <- spawn %>%
  filter(mean.sp.bl > 0 & mean.sp.bl < 8.513657) %>% 
  mutate(mean.sp.bl = mean.sp.bl + 1,
    bins = cut(mean.sp.bl, breaks = c(2^seq(0, 3.25, 0.25)))) %>% 
  group_by(phase, bins) %>%
  tally()

j <- levels(binned$bins)
j <- strsplit(j, ',')
j <- sapply(j, strsplit, ']')
j <- data.frame(j)
j <- slice(j, 2)
j <- t(j)
j <- as.numeric(j)
binned$bins <- j

for(i in 1:length(binned$bins)){
  binned[i, 'width'] <- ifelse(binned[i, 'bins'] == 1.19, 0.19,
                           binned[i, 'bins'] - binned[i - 1, 'bins'])
}

binned <- binned %>% 
  group_by(phase) %>% 
  mutate(width = as.numeric(width),
    log.nfreq = log10(n / width / 13),
    log.bins = log10(bins))

library(ggplot2)
ggplot(data = binned, aes(x = log.bins, y = log.nfreq, color = phase)) + geom_point() +stat_smooth(method = 'lm')

summary(lm(log.nfreq~log.bins, data = subset(binned, phase == 'spawning')))
summary(lm(log.nfreq~log.bins, data = subset(binned, phase == 'running')))

## t-test between the two to see if there are differences in slope???
## µ near 2 is Lévy, µ near 1 is random, µ near 3 is Gaussian
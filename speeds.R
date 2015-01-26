## See distances.R for creation of 'distances.csv'.
library(dplyr)
load('secor.sb.rda')

old <- secor.sb %>%
  filter(tag.date < '2014-10-01', date.local >= '2014-04-15',
         date.local <= '2014-09-15') %>%
  arrange(date.local) %>%
  as.data.frame()

old.split <- split(old, old$transmitter)

dist <- read.csv('distances.csv', stringsAsFactors = F)
row.names(dist) <- dist[, 1]
names(dist)[2:114] <- dist[, 1]
dist <- dist[,2:114]


for(k in 1:length(old.split)) {
  for(i in 1:dim(old.split[[k]])[1]) {
    if(i == dim(old.split[[k]])[1] | i == 1){
      old.split[[k]]$dist[i] <- 0
    }
    else{
      a <- as.character(old.split[[k]][i, 'station'])
      b <- as.character(old.split[[k]][i + 1, 'station'])
      old.split[[k]]$dist[i + 1] <- dist[a, b]
    }
  }
}

for(k in 1:length(old.split)) {
  for(i in 1:dim(old.split[[k]])[1]) {
    old.split[[k]]$time[i] <- ifelse(i == 1, 0,
                  as.numeric(difftime(old.split[[k]][i, 'date.utc'],
                                      old.split[[k]][i-1, 'date.utc'],
                                      units= 'secs')))
  }
  old.split[[k]]$mean.sp <- (old.split[[k]]$dist*1000)/old.split[[k]]$time
}

old <- do.call(rbind.data.frame, old.split)
row.names(old) <- NULL
old <- old %>% mutate(length.bin = ifelse(length < 550, '<55',
                      ifelse(length >= 550 & length < 650, '55-65',
                      ifelse(length >= 650 & length < 800, '65-80', '>80'))),
                      length.bin = factor(length.bin,
                                   levels = c('<55', '55-65', '65-80', '>80'),
                                   ordered = T),
                      max.sp = length/1000 * 5, #(5 body lengths/s in m/s)
                      mean.sp.bl = (dist/time * 1000000)/length)
old[is.na(old)] <- 0
old[is.infinite(old$mean.sp), 'mean.sp'] <- 0
old[old$mean.sp > old$max.sp, 'mean.sp'] <- 0

rm(dist, secor.sb, a, b, i, k, old.split)
## Plotting --------------------------------------------------------------------
library(ggplot2)

ggplot() + geom_histogram(data = filter(old, mean.sp > 0, sex %in% c('M', 'F')),
                          aes(x= mean.sp, fill = sex),
                          binwidth = 0.5)

ggplot() + geom_histogram(data = filter(old, mean.sp > 0),
                          aes(x= mean.sp, fill = length.bin),
                          binwidth = 0.5)

ggplot() + geom_bar(data = filter(old, mean.sp > 0),
                          aes(x= mean.sp.bl, y = ..density.., fill = length.bin),
                          binwidth = 0.5, position = 'dodge') +
  labs(x = expression('Mean Speed (Body length sec' ^-1 *')'),
       y = 'Density')

ggplot() + geom_bar(data = filter(old, mean.sp > 0, sex %in% c('M', 'F')),
                          aes(x= mean.sp.bl, y = ..density.., fill = sex),
                          binwidth = 0.5, position = 'dodge') +
  labs(x = expression('Mean Speed (Body length sec' ^-1 *')'),
       y = 'Density')




## Leftover stuff I might need later.
ches <- old %>%
  filter(lat >= 36.871, lat <= 39.648, long >= -77.498, long <= -75.619)

left <- old %>%
  anti_join(ches, by = 'station')

k <- left %>%
  select(transmitter, station, length, weight, sex) %>%
  distinct()
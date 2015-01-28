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
}

old <- do.call(rbind.data.frame, old.split)
row.names(old) <- NULL
old <- old %>% mutate(length.bin = ifelse(length < 550, '<55',
                      ifelse(length >= 550 & length < 650, '55-65',
                      ifelse(length >= 650 & length < 800, '65-80', '>80'))),
                      length.bin = factor(length.bin,
                                   levels = c('<55', '55-65', '65-80', '>80'),
                                   ordered = T),
                      mean.sp = dist / time * 1000,
                      mean.sp.bl = (mean.sp * 1000) / length,
                      max.sp = length / 1000 * 8) #(8 body lengths/s in m/s)
                      
old$system <- ifelse(old$station %in% c('Alexandria', 'Dogue Creek',
                      'Hains Point', 'Mattawoman', 'National Harbor',
                      'Piney Point B', 'Piscataway', 'Pomonkey', 'Potomac',
                      'Radar Tower', 'Report2', 'Roosevelt Br.', 'Rt 301 A',
                      'Rt 301 B', 'S. Capitol Br.', 'S. Craney Isl.'),
                            'Potomac',
              ifelse(old$station %in% c('Kent Island A', 'Kent Island B',
                      'Kent Island C', 'Kent Island D', 'Report3', 'Report5'),
                            'Upper Bay',
              ifelse(old$station %in% c('Broomes', 'CBL Pier', 'Cedar Point A',
                      'Cedar Point B', 'Cedar Point D', 'Cedar Point E',
                      'Jacks North', 'Jacks South'),
                            'Mid Bay',
              ifelse(old$station %in% c('CC LS', 'LC2', 'NCD', 'NN 1ER FWS',
                      'NN 22 NOAA SP', 'NN DANGER FWS', 'VIMS Pier', 'Y wat'),
                            'Lower Bay',
              ifelse(old$station %in% c('Barnegat 11', 'Barnegat 12',
                      'Barnegat 13', 'Barnegat 2', 'Barnegat 3', 'Barnegat 5',
                      'Barnegat 7', 'Barnegat 8', 'Fire Island 10',
                      'Fire Island 11', 'Fire Island 12', 'Fire Island 13',
                      'Fire Island 6', 'Fire Island 7', 'Fire Island 8',
                      'Fire Island 9', 'Jones Beach 3', 'Jones Beach 4',
                      'Jones Beach 5', 'Jones Beach 6', 'Jones Beach 7',
                      'Jones Beach 8', 'Montauk 1', 'Montauk 2', 'Montauk 3',
                      'Montauk 4', 'Montauk 5', 'Montauk 6', 'Montauk 7',
                      'Montauk 8', 'NJ SANDY HOOK 11', 'NJ SANDY HOOK 12',
                      'NJ SANDY HOOK 13', 'NJ SANDY HOOK 15', 'NJ SANDY HOOK 16',
                      'NJ SANDY HOOK 18', 'NJ SANDY HOOK 20', 'Report4',
                      'Rockaway 23', 'Rockaway 24', 'Rockaway 25', 'Rockaway 26',
                      'Shark River 1', 'Shark River 2', 'Shark River 3',
                      'Shark River 4', 'Shark River 5', 'Shark River 6',
                      'Shark River 7', 'Shark River 8', 'Shinnecock 10',
                      'Shinnecock 11', 'Shinnecock 2', 'Shinnecock 3',
                      'Shinnecock 4', 'Shinnecock 5', 'Shinnecock 6',
                      'Shinnecock 7', 'Shinnecock 8', 'Shinnecock 9', 'SWG D3',
                      'SWG D5', 'SWG M3', 'SWG M4', 'SWG M6', 'SWG S4'),
                            'MAB',
                            'NE')))))
old[is.na(old)] <- 0
old[is.infinite(old$mean.sp), 'mean.sp'] <- 0
old[is.infinite(old$mean.sp.bl), 'mean.sp.bl'] <- 0
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
                    aes(x= mean.sp.bl, fill = length.bin),
                    binwidth = 0.5, position = 'dodge') +
  labs(x = expression('Mean Speed (Body length sec' ^-1 *')'))+
  scale_x_log10()+
  scale_y_log10()

ggplot() + geom_bar(data = filter(old, mean.sp > 0, sex %in% c('M', 'F')),
                    aes(x= mean.sp.bl, y = ..density.., fill = sex),
                    binwidth = 0.5, position = 'dodge') +
  facet_wrap(~system) +
  labs(x = expression('Mean Speed (Body length sec' ^-1 *')'),
       y = 'Density')

ggplot() + geom_point(data = filter(old, mean.sp > 0),
                      aes(x = date.local, y = mean.sp.bl,
                          color = sex))+
  facet_wrap(~system)


## Leftover stuff I might need later.
ches <- old %>%
  filter(lat >= 36.871, lat <= 39.648, long >= -77.498, long <= -75.619)

left <- old %>%
  anti_join(ches, by = 'station')

k <- left %>%
  select(transmitter, station, length, weight, sex) %>%
  distinct()
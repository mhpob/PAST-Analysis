## Calculate distances ---------------------------------------------------------
library(gdistance); library(raster); library(rgdal); library(dplyr)
midstates <- shapefile(
  'p:/obrien/gis/shapefiles/midatlantic/matl_states_land.shp')

# Create nonsense raster file to clip shapefile
ras.back <- raster(extent(-77.344, -69.8, 36.862, 42.872),
                   resolution = 1/360, #5 arc-second grids = 720, 10 = 360
                   vals = 1,
                   crs = proj4string(midstates))
mem.crop <- cbind(c(-75.8, -69.8, -69.8, -75.8), c(36.85, 36.85, 41.2, 36.85))
mem.crop <- SpatialPolygons(list(Polygons(list(Polygon(mem.crop)),
                                          'Memory-Wasting Ocean')),
                            proj4string = CRS(proj4string(midstates)))

ras.water <- mask(mask(ras.back, mem.crop, inverse = T),
                  midstates, inverse = T)

rm(midstates, ras.back, mem.crop)

trans <- transition(ras.water, transitionFunction = function(x){1}, 16)
geo <- geoCorrection(trans, type = 'c')

load('secor.sb.rda')

old <- secor.sb %>%
  filter(tag.date < '2014-10-01', date.local >= '2014-04-15',
         date.local <= '2014-09-15') %>%
  group_by(station) %>%
  summarize(lat = mean(lat), lon = mean(long)) %>%
  as.data.frame()

row.names(old) <- old[, 1]
old <- old[, c(3, 2)]

rm(trans, secor.sb)

lc.dist <- function (trans, loc, res = c("dist", "path")){
  # Code directly stolen then slightly edited from marmap package
  if (res == "dist") {
    cost <- costDistance(trans, as.matrix(loc))/1000
    return(round(cost, digits = 2))
  }
    if (res == "path") {
        nb.loc <- nrow(loc)
        path <- list()
        comb <- combn(1:nb.loc, 2)
        pb <- txtProgressBar(min = 0, max = ncol(comb), style = 3)
        for (i in 1:ncol(comb)) {
            origin <- sp::SpatialPoints(loc[comb[1, i], ])
            goal <- sp::SpatialPoints(loc[comb[2, i], ])
            temp <- gdistance::shortestPath(trans, origin, goal, 
                output = "SpatialLines")
            path[[i]] <- temp@lines[[1]]@Lines[[1]]@coords
            setTxtProgressBar(pb, i)
        }
        close(pb)
        return(path)
    }
}

distances <- lc.dist(geo, old, res = 'dist')
paths <- lc.dist(geo, old, res = 'path')

distances <- as.matrix(distances)
write.csv(distances,'distances.csv')

## Data Manipulation -----------------------------------------------------------
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

# rm(dist, secor.sb, a, b, i, k, old.split)
#need to filter speeds that are too great.

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



ches <- old %>%
  filter(lat >= 36.871, lat <= 39.648, long >= -77.498, long <= -75.619)

left <- old %>%
  anti_join(ches, by = 'station')

k <- left %>%
  select(transmitter, station, length, weight, sex) %>%
  distinct()

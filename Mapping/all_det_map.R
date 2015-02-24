library(OpenStreetMap)
map <- openmap(c(42.95, -77.5), c(36.5, -69), type = 'mapquest-aerial')
map <- autoplot.OpenStreetMap(openproj(map))
map
load('secor.sb.rda')
names(secor.sb)
stations <- unique(secor.sb[,c('lat', 'long')])
library(ggplot2)
map + geom_point(data = stations, aes(x = long, y = lat),
                 color = 'red', size = 3)

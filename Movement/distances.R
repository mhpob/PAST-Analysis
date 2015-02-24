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

secor.sb <- secor.sb %>%
  filter(!grepl('Report', station)) %>% 
  group_by(station) %>%
  summarize(lat = mean(lat), lon = mean(long)) %>%
  as.data.frame()

row.names(secor.sb) <- secor.sb[, 1]
secor.sb <- secor.sb[, c(3, 2)]

rm(trans)

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

distances <- lc.dist(geo, secor.sb, res = 'dist')
beepr::beep(8)
# paths <- lc.dist(geo, secor.sb, res = 'path')

distances <- as.matrix(distances)
write.csv(distances,'distances.csv')
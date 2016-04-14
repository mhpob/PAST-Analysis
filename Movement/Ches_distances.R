library(dplyr); library(raster); library(rgdal)
ches <- shapefile(
  'c:/users/secor lab/desktop/gis products/chesapeake/ches shape/shoreline/shoreline.shp')

ches <- spTransform(ches,
                    CRS('+proj=utm +zone=18 +datum=NAD83 +units=m'))

j <- raster(ches,
            resolution = c(100, 100),
            vals = 0.999,
             crs = proj4string(ches))
k <- rasterize(ches, j, background = 0.001, mask = T)
library(gdistance)
trans <- transition(j, transitionFunction = function(x){1}, 16)
geo <- geoCorrection(trans, type = 'c')

load('secor.sb.rda')

secor.sb <- secor.sb %>%
  filter(array %in% c('Bay Mouth', 'C&D', 'Choptank', 'Elizabeth', 'James',
                      'Lower MD Bay', 'Lower Potomac', 'Mid MD Bay',
                      'Mid Potomac', 'Patuxent', 'Rappahannock', 'Upper MD Bay',
                      'Upper Potomac', 'York')) %>% 
  group_by(station) %>%
  summarize(lat = mean(lat), lon = mean(long)) %>%
  as.data.frame()

row.names(secor.sb) <- secor.sb[, 1]
secor.sb <- secor.sb[, c(3, 2)]
secor.sb <- SpatialPoints(secor.sb, CRS('+proj=longlat'))

secor.sb <- spTransform(secor.sb,
                         CRS('+proj=utm +zone=18 +datum=NAD83 +units=m'))
pts_in <- over(secor.sb, ches)
secor.sb <- secor.sb[!is.na(pts_in[,1]),]
secor.sb <- secor.sb@coords

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
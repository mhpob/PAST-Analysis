library(raster); library(rgdal)
midstates <- shapefile(
  'c:/users/secor lab/desktop/gis products/chesapeake/midatlantic/matl_states_land.shp')

midstates <- spTransform(midstates,
                         CRS('+proj=utm +zone=18 +datum=NAD83 +units=m'))

# Create nonsense raster file to clip shapefile
ras.back <- raster(extent(260000, 930000, 4070000, 4750000),
                   # resolution = 1/100, #5 arc-second grids = 720, 10 = 360,
                   # ncol = 6699, nrow = 7045,
                   resolution = c(100, 100),
                   vals = 1,
                   crs = proj4string(midstates))


mem.crop <- cbind(c(425000, 930000, 930000),
                  c(4070000, 4070000, 4550000))
mem.crop <- SpatialPolygons(list(Polygons(list(Polygon(mem.crop)),
                                          'Memory-Wasting Ocean')),
                            proj4string = CRS(proj4string(midstates)))

ras.crop <- mask(ras.back, mem.crop, inverse = T)

ras.water <- mask(ras.crop, midstates, inverse = T)

writeRaster(ras.water, "ras_water.grd")
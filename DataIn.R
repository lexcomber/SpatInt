##### Development Script 	##### 
##### Lex Comber		##### 
##### January 2018		##### 

#### -1. Check and load packages
if (!is.element("pycno", installed.packages())) 
    install.packages("pycno", dep = T)
if (!is.element("tmap", installed.packages()))
    install.packages("tmap", dep = T)
if (!is.element("GISTools", installed.packages()))
    install.packages("GISTools", dep = T)
if (!is.element("sf", installed.packages()))
    install.packages("sf", dep = T)
if (!is.element("gstat", installed.packages()))
    install.packages("gstat", dep = T)
if (!is.element("grid", installed.packages()))
    install.packages("grid", dep = T)
if (!is.element("osmdata", installed.packages()))
    install.packages("osmdata", dep = T)
if (!is.element("reshape2", installed.packages()))
    install.packages("rgdal", dep = T)
if (!is.element("rgdal", installed.packages()))
    install.packages("reshape2", dep = T)
if (!is.element("OpenStreetMap", installed.packages()))
    install.packages("OpenStreetMap", dep = T)
# load packages into the R session
library(pycno)
library(tmap)
library(GISTools)
library(sf)
library(gstat)
library(grid)
library(osmdata)
library(reshape2)
library(rgdal)
library(OpenStreetMap)

#### 0. Load and set up data
data(newhaven)

# Source Zones sz
sz <- tracts
proj4string(sz) <- CRS(proj4string(blocks))
sz$SID <- as.numeric(rownames(sz@data))+1
rownames(sz@data) <- sz$SID
sz <- sz[, c("SID", "HSE_UNITS")]
.proj <- CRS("+proj=lcc +lat_1=41.86666666666667 +lat_2=41.2 +lat_0=40.83333333333334 +lon_0=-72.75 +x_0=304800.6096 +y_0=152400.3048 +ellps=GRS80 +units=m +no_defs ")
sz <- spTransform(sz, .proj)

# Target Zones tz
bb <- bbox(sz)
fac = 500
grd <- GridTopology(cellcentre.offset=
    c(bb[1,1]+(fac/2),bb[2,1]+(fac/2)), 
    cellsize=c(fac,fac),  cells.dim = c(25,25))
tz <- SpatialPolygonsDataFrame(
    as.SpatialPolygons.GridTopology(grd), 
    data = data.frame(c(1:(25^2))), match.ID = FALSE)
proj4string(tz) <- .proj
names(tz) <- "TID"
# plot to check 
#plot(tz)
#plot(sz, add = T)
# convert to SF
sz_sf <- st_as_sf(sz)
tz_sf <- st_as_sf(tz)
sz_sf <- st_transform(sz_sf, crs = 2775)
tz_sf <- st_transform(tz_sf, crs = 2775)
# subset 
tz_sf <- tz_sf[sz_sf, ]  

# trim the grid with pycno (to ensure all analyses have the same TZs)
tz_sp <- as(tz_sf,"Spatial")
sz_sp <- as(sz_sf,"Spatial")
tz_sp2 <- SpatialPoints(tz_sp)
tz_sp2 <- as(tz_sp2, "SpatialPixels")
tz_sp2 <- as(tz_sp2, "SpatialGrid")
# do quick pycno
py_res <- pycno(x = sz_sp, pops = sz_sp$HSE_UNITS, celldim = tz_sp2, r = 0.1, 5)
py_res <- as(py_res, "SpatialPolygonsDataFrame")
tz_sf <- st_as_sf(py_res)
# renumber the intersect layer
tz_sf$TID <- 1:nrow(tz_sf) 
tz_sf <- tz_sf[, "TID"]
rownames(tz_sf) <- tz_sf$TID
# plot to check
#tmap_mode("view")
tmap_mode("plot")
data.p <- tm_shape(sz_sf) + tm_polygons("HSE_UNITS", palette = "YlGnBu",
              style = "kmeans", n = 9, title = "Houses Tracts") +
  tm_layout(frame = F, legend.show = T) +
  tm_shape(tz_sf) + tm_borders() +
  tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)
data.p

# save 
# sz_sf 	transformed source zones in sf format 
# tz_sf 	transformed target zones in sf format 
# setwd("/Users/geoaco/Desktop/my_docs_mac/leeds_work/research/wen/reviewanal")
# save.image(file = "part0.rda")
save(list = c("tz_sf", "sz_sf", ".proj"), file = "DataIn.RData")
##### END		##### 

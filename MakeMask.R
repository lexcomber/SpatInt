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
if (!is.element("repmis", installed.packages()))
    install.packages("repmis", dep = T)
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
library(repmis)

## Mask for Dasymtric

## Parks data from here: https://data.ct.gov/Government/City-of-New-Haven-Offices/724p-h6p6
library(rgdal)
parks <- readOGR("City of New Haven Parks/geo_export_6cc83356-382c-43d7-a044-9f4770e0ff05.shp")
parks <- parks[, "category"]
parks <- st_as_sf(parks)
parks <- st_transform(parks, 4326)

## Use OpenStreetMap to get data
# from https://cran.r-project.org/web/packages/osmdata/vignettes/osm-sf-translation.html
#https://wiki.openstreetmap.org/wiki/Key:landuse
## 1. Landuse
osm_sf <- opq ("New Haven, USA") %>%
            add_osm_feature ("landuse") %>%
            osmdata_sf 
# inspect the object that we have extracted
#osm_sf
#class(osm_sf$osm_polygons)
#names(osm_sf$osm_polygons)
#summary(osm_sf$osm_polygons)
# get rid of the things we are not interested in 
index <- osm_sf$osm_polygons$landuse != "residential"
index <- which(index)
dasy.mask1 <- osm_sf$osm_polygons[index,"landuse"]
names(dasy.mask1)[1] <- "category"
## 2. Education
osm_sf <- opq ("New Haven, USA") %>%
            add_osm_feature ("amenity", "university") %>%
            osmdata_sf 
# inspect the object that we have extracted
summary(osm_sf$osm_polygons)
dasy.mask2 <- osm_sf$osm_polygons[, "amenity"]
dasy.mask2$amenity <- "university"
names(dasy.mask2)[1] <- "category"
## 3. Amenities
osm_sf <- opq ("New Haven, USA") %>%
            add_osm_feature ("amenity", "college") %>%
            osmdata_sf 
summary(osm_sf$osm_polygons)
dasy.mask3 <- osm_sf$osm_polygons[, "amenity"]
dasy.mask3$amenity <- "college"
names(dasy.mask3)[1] <- "category"
# check 
#tmap_mode("view")
#tm_shape(parks )+tm_polygons(col = "green", alpha = 0.5)+
#tm_shape(dasy.mask1 )+tm_polygons(col = "red", alpha = 0.5) +
#tm_shape(dasy.mask2 )+tm_polygons(col = "blue", alpha = 0.5)
# Join through union 
test <- st_union(dasy.mask1, parks)
test <- st_union(test, dasy.mask2)
test <- st_union(test, dasy.mask3)
test%>%  
  split(.$category) %>% 
  lapply(st_union) %>% 
  do.call(c, .) %>% # bind the list element to a single sfc
  st_cast() -> test
t2 <- st_simplify(test, dTolerance = 0.000001)
t2 <- st_buffer(test, dist = 0)
t2 <- (st_sf(t2))
t2$SID <- 1:nrow(t2)
t2 <- st_as_sf(t2)
# write out 
st_write(t2, "dm.shp")
# this was dissolved in qgis to create mask.shp

## 4. Coastline
# now add coast
osm_sf <- opq ("New Haven, USA") %>%
            add_osm_feature ("natural", "coastline") %>%
            osmdata_sf 
#class(osm_sf$osm_polygons)
tm_shape(osm_sf$osm_lines)+tm_lines(lwd = 2)
coast <- osm_sf$osm_lines
# write out 
st_write(coast, "coast.shp")
## edited in QGIS and converted line to polygon

## 5. Join coast and mask together
coast <- st_read("coast_pol.shp")
mask <- st_read("mask.shp")
# join
test <- st_union(mask, coast)
tm_shape(test)+tm_polygons("red")
# save 
# st_write(test, "mask.shp")
save("mask_sf", file = "mask.RData")

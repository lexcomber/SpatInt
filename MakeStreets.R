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

#### Create Street data 
# Use OpenStreetMap to get streets data
# from https://cran.r-project.org/web/packages/osmdata/vignettes/osm-sf-translation.html
#https://wiki.openstreetmap.org/wiki/Key:landuse
osm_sf <- opq ("New Haven, USA") %>%
            add_osm_feature ("highway") %>%
            osmdata_sf 
streets <- osm_sf$osm_lines[, "highway"] 
# table(osm_sf$osm_lines$highway)
# select streets of interest
index <- streets$highway != "unclassified" & 
	streets$highway != "pedestrian" &
	streets$highway != "footway" &
	streets$highway != "path" &
	streets$highway != "track" & 
	streets$highway != "steps" &
	streets$highway != "cycleway" & 
	streets$highway != "construction" &
	streets$highway != "service" &
	streets$highway != "motorway" &
	streets$highway != "motorway_link" &
	streets$highway !=  "platform" 
streets <- streets[index, ]
streets <- st_transform(streets, crs = 2775)
streets <- streets[tz_sf,]

# plot 
tmap_mode("view")
tm_shape(streets)+tm_lines() +  
	tm_shape(tz_sf)+tm_borders(col = "dodgerblue")+
	tm_view(basemaps = "OpenStreetMap", set.view = 13)

save("streets", file = "streets.RData")

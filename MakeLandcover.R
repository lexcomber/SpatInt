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

#### Prepapre land cover data 
# USGS NLCD downlaoded from https://www.usgs.gov/core-science-systems/ngp/tnm-delivery/
# classes desceibed here https://www.mrlc.gov/sites/default/files/metadata/landcover.html
# load data
lc.orig <- readGDAL("nlcd/NLCD2011_LC_Connecticut.tif")
# convert to points
lc <- as(lc.orig, "SpatialPointsDataFrame")
lc <- spTransform(lc, .proj)
summary(lc)
# load labels
lc_labels <- read.csv("lc_class.csv")
lc_labels[,1:2]
table(lc$band1)
head(lc@data)
# link data to class labels
index <- match(lc$band1, lc_labels$LC_class)
lc$label <- lc_labels$Label[index]

# overlay Land cover points to SZ (in sp format)
lc_sz <- lc[as(sz_sf, "Spatial"), ]
ol <- SpatialPoints(coordinates(lc_sz),proj4string=.proj) %over% as(sz_sf,"Spatial")
summary(ol) 
lc_sz$SID <- ol$SID
head(lc_sz@data)
lc_sz$count <- 1

# save 
save("lc_sz", file = "landcover.RData")

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
# load packages into the R session
library(pycno)
library(tmap)
library(GISTools)
library(sf)
library(gstat)
library(grid)

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
# remove unwanted data
rm(list = c("blocks", "breach", "burgres.f", "burgres.n", "famdisp", "places", "roads", "tracts"))

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
# renumber the intersect layer
tz_sf$TID <- 1:nrow(tz_sf) 
rownames(tz_sf) <- tz_sf$TID
# plot to check
#tmap_mode("view")
tmap_mode("plot")
data.p <- tm_shape(sz_sf) + tm_polygons("HSE_UNITS", palette = "Greens",
              style = "kmeans", n = 9, title = "Houses Tracts") +
  tm_layout(frame = F, legend.show = T) +
  tm_shape(tz_sf) + tm_borders() +
  tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)
data.p
# remove unwanted objects
rm(list = c("bb", "fac", "grd", "sz", "tz"))

# save 
# sz_sf 	transformed source zones in sf format 
# tz_sf 	transformed target zones in sf format 
# setwd("/Users/geoaco/Desktop/my_docs_mac/leeds_work/research/wen/reviewanal")
save.image(file = "part0.rda")

#### 1. Areal Weighting
# setwd("/Users/geoaco/Desktop/my_docs_mac/leeds_work/research/wen/reviewanal")
load(file = "part0.rda")
aw_res <- st_interpolate_aw(sz_sf, tz_sf, extensive = T)
names(aw_res)
aw.p <- tm_shape(aw_res) + tm_polygons("HSE_UNITS", palette = "Greens", 
              style = "kmeans", n = 9, title = "Houses") +
              tm_layout(frame = F, legend.show = T) +
              tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)
sum(aw_res$HSE_UNITS)

#### 2. Pycno
# prepare data
tz_sp <- as(tz_sf,"Spatial")
sz_sp <- as(sz_sf,"Spatial")
tz_sp2 <- SpatialPoints(tz_sp)
tz_sp2 <- as(tz_sp2, "SpatialPixels")
tz_sp2 <- as(tz_sp2, "SpatialGrid")
# do pycno
py_res <- pycno(x = sz_sp, pops = sz_sp$HSE_UNITS, celldim = tz_sp2, r = 0.1, 5)
py_res <- as(py_res, "SpatialPolygonsDataFrame")
# plot 
py.p <- tm_shape(py_res) + tm_polygons("dens", palette = "Greens", 
              style = "kmeans", n = 9, title = "Houses") +
              tm_layout(frame = F, legend.show = T) +
              tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)
sum(py_res$dens)

#### 3. A2P 
# Create control point for each source zone
sz_pt_sp <- SpatialPointsDataFrame(as(sz_sf, "Spatial"), data = data.frame(sz_sf), proj4string = .proj)
# These are interpolated to a regular grid of points using one of the point interpolation methods
# gstat requires sp format
bb <- bbox(as(sz_sf, "Spatial"))
fac = 100
grd <- GridTopology(cellcentre.offset=
    c(bb[1,1]+(fac/2),bb[2,1]+(fac/2)), 
    cellsize=c(fac,fac),  cells.dim = c(125,125))
tz_pt_sp <- SpatialPolygonsDataFrame(
    as.SpatialPolygons.GridTopology(grd), 
    data = data.frame(c(1:(125^2))), match.ID = FALSE)
proj4string(tz_pt_sp) <- .proj
# subset
tz_pt_sp <- tz_pt_sp[as(sz_sf, "Spatial"), ]
#plot(tz_pt_sp)
#plot(sz_pt_sp, add = T)
# Interpolate with IDW 
idw_res <- krige(HSE_UNITS~1,sz_pt_sp,tz_pt_sp)
# plot of IDW (not used)
idw.p <- tm_shape(idw_res) + 
	tm_dots(col='var1.pred',palette = "Greens", 
              style = "kmeans", n = 9, title = "Dens", size = 0.17)+
 			tm_layout(frame = F, legend.show = T) +
	tm_shape(tz_sf) + tm_borders()+
	tm_shape(sz_sf) + tm_borders()
# Then, the density value for each grid cell is converted back to a count value 
fac <- sum(sz_sf$HSE_UNITS)/sum(idw_res$var1.pred)
idw_res$houses <- idw_res$var1.pred * fac
sum(idw_res$houses)

# and the values are reaggregated to the target zones
ol <- over(idw_res, tz_sp)
tid.list <- sort(unique(tz_sp$TID))
val_vec <- vector()
for (i in 1:length(tid.list)){
	tid.i <- tid.list[i]
	index.i <- ol$TID == tid.i
	idw_res.i <- idw_res[index.i, ]
	val.i <- sum(idw_res.i$houses, na.rm = T)
	val_vec <- append(val_vec, val.i)
}
idw_res2 <- tz_sp
idw_res2$houses <- val_vec
sum(idw_res2$houses)

# BUT point-based interpolators are not volume preserving
# Because of this a scaling step needs to be added, in which the initial raster estimates are multiplied by the ratio between the value of the source feature containing the raster and the inferred value of that source feature 
ol <- over(idw_res2, sz_sp)
sid.list <- sort(unique(sz_sp$SID))
for (i in 1:length(sid.list)){
	sid.i <- sid.list[i]
	index.i <- ol$SID == sid.i
	idw_res2.i <- idw_res2[index.i, ]
	val.i <- sum(idw_res2.i$houses, na.rm = T)
	real.val.i <- sz_sp@data[sz_sp@data$SID == sid.i, "HSE_UNITS"]
	fac <- real.val.i/val.i
	idw_res2@data[index.i, "houses"] <- 	(idw_res2@data[index.i, "houses"] * fac)
	#cat(i, "\t", ": real: ", real.val.i, "\t", "pred: ", val.i, "\t", "sum: ", sum(idw_res2@data[index.i, "houses"]), "\n")	
}
sum(idw_res2$houses)
idw.p2a <- tm_shape(idw_res2) + 
	tm_polygons(col='houses',palette = "Greens", 
              style = "kmeans", n = 9, title = "Houses", size = 0.17)+
 			tm_layout(frame = F, legend.show = T) +
			tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)

#### Standardised plots
breaks = c(0, 50, 100, 200, 300, 500, 700, 1200, 1700, 2200)
atp.p <- tm_shape(idw_res2) + 
	tm_polygons(col='houses',palette = "Greens", 
            breaks = breaks, title = "Houses AtP")+
 			tm_layout(frame = F, legend.show = T) +
			tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)
py.p <- tm_shape(py_res) + tm_polygons("dens", palette = "Greens", 
            breaks = breaks, title = "Houses Pycno")+
              tm_layout(frame = F, legend.show = T) +
              tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)
aw.p <- tm_shape(aw_res) + tm_polygons("HSE_UNITS", palette = "Greens", 
            breaks = breaks, title = "Houses AW")+
              tm_layout(frame = F, legend.show = T) +
              tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)

# setwd("/Users/geoaco/Desktop/my_docs_mac/leeds_work/research/wen/reviewanal")
png(filename = "F1.png", w = 15/2, h = 15/2, units = "in", res = 300)
pushViewport(viewport(layout=grid.layout(2,2)))
# plot using he print command
print(data.p, vp=viewport(layout.pos.col = 1, layout.pos.row = 1, height = 5))
print(aw.p, vp=viewport(layout.pos.col = 1, layout.pos.row = 2, height = 5))
print(py.p, vp=viewport(layout.pos.col = 2, layout.pos.row = 1, height = 5))
print(atp.p, vp=viewport(layout.pos.col = 2, layout.pos.row = 2, height = 5))
dev.off()

# save data
# setwd("/Users/geoaco/Desktop/my_docs_mac/leeds_work/research/wen/reviewanal")
save.image(file = "part1.rda")

#### 4. Dasymetric
# setwd("/Users/geoaco/Desktop/my_docs_mac/leeds_work/research/wen/reviewanal")
load(file = "part1.rda")


#### OLD Stuff

#### Z. Fsquare prep
setwd("/Users/geoaco/Desktop/my_docs_mac/leeds_work/research/wen/reviewanal")
load(file = "part0.rda")

tz_ll <- st_transform(tz_sf, 4326)
ll.list <- st_coordinates(tz_ll)[, c("X", "Y")]

colnames(ll.list) <- c("longtitude", "latitude")
ll.list[500,]


idw_sf <- st_as_sf(idw_res)
aw_idw <- st_interpolate_aw(idw_sf, tz_sf, extensive = T)


grd <- GridTopology(cellcentre.offset=
    c(bb[1,1],bb[2,1]), 
    cellsize=c(1500,1500),  cells.dim = c(100,100))
int_layer <- SpatialPolygonsDataFrame(
    as.SpatialPolygons.GridTopology(grd), 
    data = data.frame(c(1:10000)), match.ID = FALSE)
ct <- proj4string(blocks)
proj4string(int_layer) <- ct
int_layer <- SpatialPoints(int_layer)
proj4string(int_layer) <- ct
int_layer <- spTransform(int_layer, wgs.proj)



py_res <- pycno(tracts_ll, tracts_ll$HSE_UNITS, celldim = 100)
summary(py_res)
py_res <- as(py_res, "SpatialPolygonsDataFrame")
py_res <- st_as_sf(py_res)
py_res_cl <- py_


int.res$IntArea <- st_area(int.res)
index <- as.vector(int.res$IntArea) > 0
int.res <- int.res[index, ]

tmap_mode("plot")

tm_shape(int.res)+tm_borders()





tmap_mode("view")
tm_shape(tracts) + tm_borders() +
  tm_layout(frame = F) +
  tm_text("ID", size = 0.7) 
  
  tmap_mode("view")
  tm_shape(blocks) +tm_borders(col = "red")+
  tm_shape(tracts) + tm_borders() +
  tm_text("T009075H_", size = 0.7) +



index <- c(5, 13, 20, 25, 18, 14, 7, 6, 15, 21, 7, 8, 19, 23)  
dim(tracts)
length(index)

tr <- tracts[(index - 1), ]


int.layer_sf <- st_as_sf(blocks)
tracts_sf <- st_as_sf(tr)
int.res_sf <- st_intersection(int.layer_sf, tracts_sf)



NEWH075H_

## define sample grid in polygons
bb <- bbox(tracts)
grd <- GridTopology(cellcentre.offset=
    c(bb[1,1]-200,bb[2,1]-200), 
    cellsize=c(10000,10000),  cells.dim = c(5,5))
int.layer <- SpatialPolygonsDataFrame(
    as.SpatialPolygons.GridTopology(grd), 
    data = data.frame(c(1:25)), match.ID = FALSE)
ct <- proj4string(blocks)
proj4string(int.layer) <- ct
proj4string(tracts) <- ct
names(int.layer) <- "ID"


int.layer_sf <- st_as_sf(int.layer)
tracts_sf <- st_as_sf(tracts)
int.res_sf <- st_intersection(int.layer_sf, tracts_sf)

p1 <- tm_shape(int.layer_sf) + tm_borders(lty = 2) +
  tm_layout(frame = F) +
  tm_text("ID", size = 0.7) +
  # plot the tracts
  tm_shape(tracts_sf) + tm_borders(col = "red", lwd = 2) 
# plot the intersection, scaled by int.later_sf
p2 <- tm_shape(int.layer_sf) + tm_borders(col="white") +
  tm_shape(int.res_sf) + tm_polygons("HSE_UNITS", palette = blues9) +
  tm_layout(frame = F, legend.show = F)
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(p1, vp=viewport(layout.pos.col = 1))
print(p2, vp=viewport(layout.pos.col = 2))



```{r Op39a, eval=FALSE, echo=TRUE, message=FALSE}
head(int.res_sf) 
```

```{r Op41a, eval=TRUE, echo=TRUE, message=FALSE}
# generate area and proportions
int.areas <- st_area(int.res_sf)
tract.areas <- st_area(tracts_sf)
# match tract area to the new layer
index <- match(int.res_sf$T009075H_I, tracts$T009075H_I)
tract.areas <- tract.areas[index]
tract.prop <- as.vector(int.areas)/as.vector(tract.areas)
```

```{r eval = T}
int.res_sf$houses <- tracts$HSE_UNITS[index] * tract.prop
```

```{r eval = T}
library(tidyverse)
houses <- summarise(group_by(int.res_sf, ID), count = sum(houses))
# create an empty vector 
int.layer_sf$houses <- 0
# and populate this using houses$ID as the index
int.layer_sf$houses[houses$ID] <- houses$count
```

```{r Op44c, eval=TRUE, echo=TRUE, message=TRUE, fig.cap = "The zones shaded by the number of households after intersection with the census tracts",fig.height=5}
tm_shape(int.layer_sf) + 
  tm_polygons("houses", palette = "Greens", 
              style = "kmeans", title = "No. of houses") +
  tm_layout(frame = F, legend.position = c(1,0.5)) +
  tm_shape(tracts_sf) + tm_borders(col = "black")
```


```{r Op45, eval=FALSE, echo=TRUE, message=FALSE}
## in rgdal
library(rgdal)
ct <- proj4string(blocks)
proj4string(int.layer) <- CRS(ct)
blocks <- spTransform(blocks, CRS(proj4string(int.layer)))
## in sf
library(sf)
st_transform` (`sf`) functions to put the data into the same projection. 
ct <- st_crs(blocks_sf)
st_crs(int.layer_sf) <- (ct)
blocks_sf <- st_transform(blocks_sf, st_crs(int.layer_sf))
```

```{r eval = F}
# Use the IDs to assign ID variables to both inputs
# this makes the processing easier later on 
int.ID <- "ID"
layer.ID <- "T009075H_I"
int_sf$IntID <- int_sf[,int.ID]
layer_sf$LayerID <- layer_sf[, layer.ID]
# do the same for the target.var
target.var <- HSE_UNITS
layer_sf$target.var <- layer_sf[, target.var]

``` 

```{r eval = F}
# directly from the data frame
as.vector(data.frame(int.res_sf[,"T009075H_I"])[,1])
as.vector(unlist(select(as.data.frame(int.res_sf), T009075H_I)))
# set the geometry to null and then extract
st_geometry(int.res_sf) <- NULL
int.res_sf[,"T009075H_I"]
# using select from dplyr
as.vector(unlist(select(as.data.frame(int.res_sf), T009075H_I)))
```


###### OLD Play



#plot(blocks, add = F, lty = 2, border = "red")
#plot(tracts, add = T, lwd = 2)
# select variable
blocks$BlockID <- as.numeric(rownames(blocks@data))+1
blocks <- blocks[, "BlockID"]
tracts$TractID <- as.numeric(rownames(tracts@data))+1
tracts <- tracts[, c("TractID", "HSE_UNITS")]
# convert to SF and WGS84
blocks_sf <- st_as_sf(blocks)
tracts_sf <- st_as_sf(tracts)
blocks_sf <- st_transform(blocks_sf, crs = 4326)
tracts_sf <- st_transform(tracts_sf, crs = 4326)
# calulate areas
blocks_sf$BlockArea <- st_area(blocks_sf)
tracts_sf$TractArea <- st_area(tracts_sf)

## define sample grid in polygons
bb <- bbox(tracts)
grd <- GridTopology(cellcentre.offset=
    c(bb[1,1]-200,bb[2,1]-200), 
    cellsize=c(10000,10000),  cells.dim = c(5,5))
int.layer <- SpatialPolygonsDataFrame(
    as.SpatialPolygons.GridTopology(grd), 
    data = data.frame(c(1:25)), match.ID = FALSE)
ct <- proj4string(blocks)
proj4string(int.layer) <- ct
proj4string(tracts) <- ct
names(int.layer) <- "ID"


##### Code Development Script 	##### 
##### Lex Comber				##### 
##### a.comber@leeds.ac.uk		#####
##### July 2019					##### 
##### Full CODE and DATA CREATION Details are at 	#####
##### https://github.com/lexcomber/SpatInt 			#####

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
if (!is.element("gclus", installed.packages()))
    install.packages("gclus", dep = T)
if (!is.element("raster", installed.packages()))
    install.packages("raster", dep = T)
if (!is.element("ggplot2", installed.packages()))
    install.packages("ggplot2", dep = T)
if (!is.element("GGally", installed.packages()))
    install.packages("GGally", dep = T)
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
library(gclus)
library(raster)
library(ggplot2)
library(GGally)

#### 1. Load Data
# load source zones and target zones
# see GitHub site for details of the creation of this data
# https://github.com/lexcomber/SpatInt 
source_data("https://github.com/lexcomber/SpatInt/blob/master/DataIn.RData?raw=True")
# or if saved locally then set your working director then load
# load("DataIn.RData")

#### 2. Approaches with No Ancillary Data ####

#### 2.1 Areal Weighting
aw_res <- st_interpolate_aw(sz_sf, tz_sf, extensive = T)
fac <- sum(sz_sf$HSE_UNITS)/sum(aw_res$HSE_UNITS)
aw_res$HSE_UNITS = aw_res$HSE_UNITS*fac

#### 2.2 Pycno
# pycno only takes sp format and SpatialGrid as TZs
# prepare data 
tz_sp <- as(tz_sf,"Spatial")
sz_sp <- as(sz_sf,"Spatial")
tz_sp2 <- SpatialPoints(tz_sp)
tz_sp2 <- as(tz_sp2, "SpatialPixels")
tz_sp2 <- as(tz_sp2, "SpatialGrid")
# do pycno
py_res <- pycno(x = sz_sp, pops = sz_sp$HSE_UNITS, celldim = tz_sp2, r = 0.1, 5)
py_res <- as(py_res, "SpatialPolygonsDataFrame")

#### 2.3 Area to Point  
# Create control point for each source zone
sz_pt_sp <- SpatialPointsDataFrame(as(sz_sf, "Spatial"), 
	data = data.frame(sz_sf), proj4string = .proj)
# These are interpolated to a regular grid of points 
# using one of the point interpolation methods
# gstat for IDW requires sp format
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
# Interpolate with IDW - this takes time
idw_res <- krige(HSE_UNITS~1,sz_pt_sp,tz_pt_sp)
# Then, the density value for each grid cell is converted back to a count value 
fac <- sum(sz_sf$HSE_UNITS)/sum(idw_res$var1.pred)
idw_res$houses <- idw_res$var1.pred * fac
# and the values are reaggregated to the target zones
ol <- over(idw_res, tz_sp)
tid.list <- sort(unique(tz_sp$TID))
val_vec <- vector()
for (i in 1:length(tid.list)){
	tid.i <- tid.list[i]
	index.i <- which(ol$TID == tid.i)
	idw_res.i <- idw_res[index.i, ]
	val.i <- sum(idw_res.i$houses, na.rm = T)
	val_vec <- append(val_vec, val.i)
}
# assign to TZ
a2p_res <- tz_sp
a2p_res$houses <- val_vec

# BUT point-based interpolators are not volume preserving
# Because of this a scaling step needs to be added, in which the initial raster estimates are multiplied by the ratio between the value of the source feature containing the raster and the inferred value of that source feature 
ol <- over(a2p_res, sz_sp)
sid.list <- sort(unique(sz_sp$SID))
for (i in 1:length(sid.list)){
	sid.i <- sid.list[i]
	index.i <- ol$SID == sid.i
	a2p_res.i <- a2p_res[index.i, ]
	val.i <- sum(a2p_res.i$houses, na.rm = T)
	real.val.i <- sz_sp@data[sz_sp@data$SID == sid.i, "HSE_UNITS"]
	fac <- real.val.i/val.i
	a2p_res@data[index.i, "houses"] <- 	(a2p_res@data[index.i, "houses"] * fac)
}

## Make tmap plot items of results for Figure 1
# Standardised plot breaks
breaks = c(0, 50, 100, 200, 300, 500, 700, 1200, 1700, 2200)
# make tmap plot item of Input data
data.p <- tm_shape(tz_sf) + tm_borders()+ 
	tm_shape(sz_sf) + 
	tm_polygons("HSE_UNITS", palette = "YlGnBu",
              style = "kmeans", n = 9, title = "Houses Tracts") +
              tm_layout(frame = F, legend.show = T) +
              tm_shape(tz_sf) + tm_borders() +
              tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
              tm_scale_bar(position = c(0.3))+
              tm_compass(position = c(0.378, 0.1))
atp.p <- tm_shape(a2p_res) + 
	tm_polygons(col='houses',palette = "YlGnBu", 
            breaks = breaks, title = "Houses AtP")+
 			tm_layout(frame = F, legend.show = T) +
			tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
			tm_scale_bar(position = c(0.3))+
              tm_compass(position = c(0.378, 0.1))
py.p <- tm_shape(py_res) + tm_polygons("dens", palette = "YlGnBu", 
            breaks = breaks, title = "Houses Pycno")+
              tm_layout(frame = F, legend.show = T) +
              tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
              tm_scale_bar(position = c(0.3))+
              tm_compass(position = c(0.378, 0.1))
aw.p <- tm_shape(aw_res) + tm_polygons("HSE_UNITS", palette = "YlGnBu", 
            breaks = breaks, title = "Houses AW")+
              tm_layout(frame = F, legend.show = T) +
              tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
              tm_scale_bar(position = c(0.3))+
              tm_compass(position = c(0.378, 0.1))

# write PNG of maps 
# you may want to set your working directory with setwd()
png(filename = "F1.png", w = 15/1.5, h = 15/1.5, units = "in", res = 300)
pushViewport(viewport(layout=grid.layout(2,2)))
print(data.p, vp=viewport(layout.pos.col = 1, layout.pos.row = 1, height = 5))
print(aw.p, vp=viewport(layout.pos.col = 1, layout.pos.row = 2, height = 5))
print(py.p, vp=viewport(layout.pos.col = 2, layout.pos.row = 1, height = 5))
print(atp.p, vp=viewport(layout.pos.col = 2, layout.pos.row = 2, height = 5))
dev.off()

# summary table Table 2
f1.df = data.frame(sz = append(summary(sz_sf$HSE_UNITS), sum(sz_sf$HSE_UNITS)),
				a2p = append(summary(a2p_res$houses),sum(a2p_res$houses)),
				pycno = append(summary(py_res$dens), sum(py_res$dens)),
				aw = append(summary(aw_res$HSE_UNITS), sum(aw_res$HSE_UNITS)))
rownames(f1.df)[7] = "Total"
write.csv(round(f1.df,0),"tab2.csv")

#### 3. With Ancillary Data ####

#### 3.1 Dasymetric
# load in the mask data
# see MakeMask.R for the creation of this data
source_data("https://github.com/lexcomber/SpatInt/blob/master/mask.RData?raw=True")
# transform to projection to match that of .proj
mask_sf <- st_transform(mask_sf, crs = 2775)
# and create the target zone mask
tz_m <- st_difference(tz_sf, mask_sf)
## uncomment the below to check
#tm_shape(mask_sf)+tm_polygons("red")
#tm_shape(tz_m)+tm_borders("red")
#tmap_mode("view")
#tm_shape(tz_m)+tm_polygons(col = "lightgrey", alpha = 0.7)+
#    tm_view(basemaps = "OpenStreetMap", set.view = 13)
#tm_shape(tz_m[211, ])+tm_fill("red")+
#	tm_shape(dasy_res[211,])+tm_borders(lwd = 2)

## interpolation with the mask
# and then put the results bask to the original TZs
dasy_res <- st_interpolate_aw(sz_sf, tz_m, extensive = T)
# rescale by ratio of loss to total
dasy_res$HSE_UNITS = dasy_res$HSE_UNITS * (sum(sz_sf$HSE_UNITS)/sum(dasy_res$HSE_UNITS))
dasy_res$TID <- tz_m$TID
index <- match(dasy_res$TID, tz_sf$TID)
dasy <- tz_sf
dasy$HSE_UNITS <- 0  
dasy$HSE_UNITS[index] <- dasy_res$HSE_UNITS
dasy_res <- dasy
## make tmap plot item of the results
dasy.p<- 
	tm_shape(dasy_res) + tm_polygons(col='HSE_UNITS',palette = "YlGnBu", 
            breaks = breaks, title = "Houses Dasy")+
 			tm_layout(frame = F, legend.show = T) +
			tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
            tm_scale_bar(position = c(0.3))+
            tm_compass(position = c(0.378, 0.1))

#### 3.2 Street weighted
# load in the OSM street data
# see MakeStreets.R for the creation of this data
source_data("https://github.com/lexcomber/SpatInt/blob/master/streets.RData?raw=True")

# determine the proportion of SZ streets in each TZ
ol <- st_intersection(streets, sz_sf)
ol <- st_intersection(ol, tz_sf)
ol$Length <- st_length(ol)
df <- data.frame(ol[, c("TID", "SID", "Length")])[, 1:3]
df <- as.data.frame.matrix(xtabs(Length~TID+SID, df))
# convert to proprtions
for (i in 1:ncol(df)){
	df[, i] <- df[,i]/sum(df[,i])
}
# insert non-overlapping TZ (ie those without streets)
tid.list <- sort(unique(tz_sf$TID))
sid.list <- sort(unique(sz_sf$SID))
df_res <- matrix(0, ncol = length(sid.list), nrow = length(tid.list))
rownames(df_res) <- tid.list
colnames(df_res) <- sid.list
index <- match(rownames(df), rownames(df_res))
for (i in 1:length(index)) {
	index.i <- index[i]
	df_res[index.i, ] <- as.vector(unlist(df[i,]))	
}
# do allocation
pops <- sz_sf$HSE_UNITS 
for (i in 1:length(pops)){
	pops.i <- pops[i]
	df_res[, i] <- as.vector(unlist(df_res[, i] * pops.i))
}
sw_res <- tz_sf
sw_res$Houses <- rowSums(df_res)

## make tmap plot item of the results
sw.p<- tm_shape(sw_res) + tm_polygons(col='Houses',palette = "YlGnBu", 
            breaks = breaks, title = "Houses Street")+
 	tm_layout(frame = F, legend.show = T) +
 	#tm_shape(streets) +tm_lines("darkgray")+
	tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
	tm_scale_bar(position = c(0.3))+
	tm_compass(position = c(0.378, 0.1))
# save.image("upto3.3.RData")

#### 3.3 Statistical
# load in the USGS land cover  data
# see MakeLandcover.R for the creation of this data
source_data("https://github.com/lexcomber/SpatInt/blob/master/landcover.RData?raw=True")

# constuct data for regression for SZ
df <- data.frame(lc_sz@data[, c("label", "SID", "count")])
df <- as.data.frame.matrix(xtabs(count~SID+label, df))
df$Houses <- sz_sf$HSE_UNITS
# create regression model
reg.mod <- as.formula(Houses~`Developed, High Intensity`+ `Developed, Medium Intensity`+`Grassland/Herbaceous`+0)
mod <- lm(reg.mod, df)
# summary(mod)
# overlay points to TZ
#lc_tz <- lc[as(tz_sf, "Spatial"), ]
ol <- SpatialPoints(coordinates(lc_tz),proj4string=.proj) %over% as(tz_sf,"Spatial")
lc_tz$TID <- ol$TID
lc_tz$count <- 1
# constuct data for regression TZ
df_pred <- data.frame(lc_tz@data[, c("label", "TID", "count")])
df_pred <- as.data.frame.matrix(xtabs(count~TID+label, df_pred))
# use as input to model to predict houses
pred <- predict(mod, newdata = df_pred)
# rescale the data     
fac <- sum(sz_sf$HSE_UNITS)/sum(pred)
pred <- pred*fac     
# assign to TZ
stat_res <- tz_sf
stat_res$houses <- pred

## make tmap plot item of the results
stat.p<- tm_shape(stat_res) + tm_polygons(col='houses',palette = "YlGnBu", 
            breaks = breaks, title = "Houses Stat")+
 	tm_layout(frame = F, legend.show = T) +
	tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
	tm_scale_bar(position = c(0.3))+
	tm_compass(position = c(0.378, 0.1))

#### 3.4 Point-Based ancillary information
# load in the OSM building  data
# see below for the creation of this data
source_data("https://github.com/lexcomber/SpatInt/blob/master/buildings.RData?raw=True")
# get data from OSM - this was done in January 2019
#osm_sf <- opq ("New Haven, USA") %>%
#            add_osm_feature ("building") %>%
#            osmdata_sf 
#tm_shape(osm_sf$osm_points)+tm_dots()
#buildings <- st_transform(osm_sf$osm_points, crs = 2775)
#buildings <- buildings[tz_sf, "osm_id"]
#save("buildings", file = "buildings.RData")

# intersection
ol <- st_intersection(buildings, tz_sf)
ol <- st_intersection(ol, sz_sf)
ol$count <- 1
df <- data.frame(ol[, c("TID", "SID", "count")])[, 1:3]
df <- as.data.frame.matrix(xtabs(count~TID+SID, df))
# convert to proportions
for (i in 1:ncol(df)){
	df[, i] <- df[,i]/sum(df[,i])
}
# insert non-overlapping TZ (ie those without streets)
tid.list <- sort(unique(tz_sf$TID))
sid.list <- sort(unique(sz_sf$SID))
df_res <- matrix(0, ncol = length(sid.list), nrow = length(tid.list))
rownames(df_res) <- tid.list
colnames(df_res) <- sid.list
index <- match(rownames(df), rownames(df_res))
for (i in 1:length(index)) {
	index.i <- index[i]
	df_res[index.i, ] <- as.vector(unlist(df[i,]))	
}
head(df_res)
# do allocation
pops <- sz_sf$HSE_UNITS 
for (i in 1:length(pops)){
	pops.i <- pops[i]
	df_res[, i] <- as.vector(unlist(df_res[, i] * pops.i))
}
pt_res <- tz_sf
pt_res$Houses <- rowSums(df_res)

## make tmap plot item of the results
pt.p <- tm_shape(pt_res) + tm_polygons(col='Houses',palette = "YlGnBu", 
            breaks = breaks, title = "Houses Point")+
 	tm_layout(frame = F, legend.show = T) +
 	#tm_shape(buildings) +tm_dots("darkgray", alpha = 0.5, size = 0.002)+
	tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
	tm_scale_bar(position = c(0.3))+
	tm_compass(position = c(0.378, 0.1))

#### 4. Plots

# Figure 2 
png(filename = "F2.png", w = 15/1.5, h = 15/1.5, units = "in", res = 300)
pushViewport(viewport(layout=grid.layout(2,2)))
print(dasy.p, vp=viewport(layout.pos.col = 1, layout.pos.row = 1, height = 5))
print(sw.p, vp=viewport(layout.pos.col = 1, layout.pos.row = 2, height = 5))
print(stat.p, vp=viewport(layout.pos.col = 2, layout.pos.row = 1, height = 5))
print(pt.p, vp=viewport(layout.pos.col = 2, layout.pos.row = 2, height = 5))
dev.off()

f2.df = data.frame(dasy = append(summary(dasy_res$HSE_UNITS), sum(dasy_res$HSE_UNITS)),
				stat = append(summary(stat_res$houses), sum(stat_res$houses)),
				sw = append(summary(sw_res$Houses),sum(sw_res$Houses)),
				pt = append(summary(pt_res$Houses), sum(pt_res$Houses)))
rownames(f2.df)[7] = "Total"
write.csv(round(f2.df,0),"tab3.csv")

# Figure 3 (input data)
# plot study area detail
tmp <- as(st_transform(tz_sf, 4326), "Spatial")
buf <- st_buffer(tz_sf, 500)
buf <- as(st_transform(buf, 4326), "Spatial")
ul <- as.vector(cbind(bbox(buf)[2,2], bbox(buf)[1,1]))
lr <- as.vector(cbind(bbox(buf)[2,1], bbox(buf)[1,2])) 
# download the map tile
MyMap <- openmap(ul,lr,13,'osm')
# now plot the layer and the backdrop

png(filename = "F3a.png", w = 15/3, h = 15/3, units = "in", res = 300)
par(mar = c(1,1,1,1))
plot(MyMap, removeMargin=F) 
plot(spTransform(as(tz_m, "Spatial"), osm()), add = TRUE, col = rgb(0.75,0.25,0.25,0.15))
#scalebar(3000, label = c(0, 1.5, 3), type = "bar", below = "km", xy = c(-8119700,5047700))
title("Binary Mask (Pycno)", font.main = 1)
dev.off()

png(filename = "F3b.png", w = 15/3, h = 15/3, units = "in", res = 300)
par(mar = c(1,1,1,1))
plot(MyMap, removeMargin=FALSE) 
plot(spTransform(as(streets, "Spatial"), osm()), add = TRUE)
#scalebar(3000, label = c(0, 1.5, 3), type = "bar", below = "km", xy = c(-8119700,5047700))
title("OSM road network (Streets)", font.main = 1)
dev.off()

index <- lc_tz$band1 == 24 | lc_tz$band1 == 23 | lc_tz$band1 == 71
summary(index)
lc_tmp <- lc_tz[index,]
col.vec <- rep("#FB6A4A", nrow(lc_tmp))
index <- lc_tmp$band1 == 24
col.vec[index] <- "#252525"
index <- lc_tmp$band1 == 23
col.vec[index] <- "#969696"

png(filename = "F3c.png", w = 15/3, h = 15/3, units = "in", res = 300)
par(mar = c(1,1,1,1))
plot(MyMap, removeMargin=FALSE) 
plot(spTransform(lc_tmp, osm()), add = TRUE, cex = 0.2, pch = 15, col = col.vec)
#scalebar(3000, label = c(0, 1.5, 3), type = "bar", below = "km", xy = c(-8119700,5047700))
title("Land cover (Statistical)", font.main = 1)
dev.off()

png(filename = "F3d.png", w = 15/3, h = 15/3, units = "in", res = 300)
par(mar = c(1,1,1,1))
plot(MyMap, removeMargin=FALSE) 
plot(spTransform(as(buildings, "Spatial"), osm()), add = TRUE, cex = 0.1, pch = 15)
#scalebar(3000, label = c(0, 1.5, 3), type = "bar", below = "km", xy = c(-8119700,5047700))
title("OSM Buildings (Point)", font.main = 1)
dev.off()

# save.image(file = "upto5.RData")

#### 5.Housing sales / rental website data
# load in the USGS land cover  data
# see ScrapeWeb.R for the creation of this data
source_data("https://github.com/lexcomber/SpatInt/blob/master/zill.RData?raw=True")
# transform the projection
props_sf <- st_transform(props_sf, crs = 2775)

# Figure 4a 
png(filename = "F4a.png", w = 15/3, h = 15/3, units = "in", res = 300)
par(mar = c(1,1,1,1))
plot(MyMap, removeMargin=FALSE) 
plot(spTransform(as(props_sf, "Spatial"), osm()), add = TRUE, 
	col = rgb(0,0,0,0.3), pch = 19, cex = 0.5)
title("Web Properties", font.main = 1)
dev.off()

#  same approach as Point-Based ancillary information
ol <- st_intersection(props_sf, tz_sf)
ol <- st_intersection(ol, sz_sf)
ol$count <- 1
df <- data.frame(ol[, c("TID", "SID", "count")])[, 1:3]
df <- as.data.frame.matrix(xtabs(count~TID+SID, df))
# convert to proportions
for (i in 1:ncol(df)){
	df[, i] <- df[,i]/sum(df[,i])
}
# insert non-overlapping TZ (ie those without streets)
tid.list <- sort(unique(tz_sf$TID))
sid.list <- sort(unique(sz_sf$SID))
df_res <- matrix(0, ncol = length(sid.list), nrow = length(tid.list))
rownames(df_res) <- tid.list
colnames(df_res) <- sid.list
index <- match(rownames(df), rownames(df_res))
for (i in 1:length(index)) {
	index.i <- index[i]
	df_res[index.i, ] <- as.vector(unlist(df[i,]))	
}
# do allocation
pops <- sz_sf$HSE_UNITS 
for (i in 1:length(pops)){
	pops.i <- pops[i]
	df_res[, i] <- as.vector(unlist(df_res[, i] * pops.i))
}
web_res <- tz_sf
web_res$Houses <- rowSums(df_res)

## make tmap plot item of the results
web.p <- tm_shape(web_res) + tm_polygons(col='Houses',palette = "YlGnBu", 
            breaks = breaks, title = "Houses Web")+
 	tm_layout(frame = F, legend.show = T) +
 	#tm_shape(buildings) +tm_dots("darkgray", alpha = 0.5, size = 0.002)+
	tm_shape(sz_sf) + tm_borders(col = "black", lwd = 2)+
	tm_scale_bar(position = c(0.3))+
	tm_compass(position = c(0.378, 0.1))

# Figure 4b
png(filename = "F4b.png", w = 15/3, h = 15/3, units = "in", res = 300)
pushViewport(viewport(layout=grid.layout(1,1)))
# plot using he print command
print(web.p, vp=viewport(layout.pos.col = 1, layout.pos.row = 1, height = 5))
#print(wwt.p, vp=viewport(layout.pos.col = 2, layout.pos.row = 1, height = 5))
dev.off()
vals = matrix(c(round(summary(web_res$Houses), 0), sum(web_res$Houses)))
tab = c(names(summary(web_res$Houses)), "Total")
tab = cbind(tab, vals)
write.csv(tab, file = "tab4.csv")

#### 6.Comparisons in Figure 5
wf <- data.frame(	AW = aw_res$HSE_UNITS,
					Pycno = py_res$dens,
					AtP = a2p_res$houses,
					Dasy = dasy_res$HSE_UNITS,
					Street = sw_res$Houses, 
					Stat = stat_res$houses,
					Point = pt_res$Houses,
					Web = web_res$Houses)

# original panel plot
# ggpairs(wf, aes(alpha = 0.4),
#          upper = list(continuous = wrap('cor', size = 6, colour = "black")),
#          lower = list(continuous = wrap('smooth',alpha = 0.3, cex=0.2   ))) +
#          theme(axis.line=element_blank(),
#          	axis.text=element_blank(),
#          	axis.ticks=element_blank())

# functions change the layout modified from https://github.com/ggobi/ggally/issues/139
# for lower panel of plots
my_custom_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = I("blue"), alpha = 0.3, cex = 0.5) + 
    geom_smooth(method = "lm", lwd = 0.5, color = I("red3"), ...)
}
#my_custom_smooth(iris, aes(Sepal.Length, Sepal.Width))

# for upper panel plot
my_custom_cor <- function(data, mapping, color = I("black"), sizeRange = c(1.5, 3), ...) {
  # get the x and y data to use the other code
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  ct <- cor.test(x,y)
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " "))
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  # since we can't print it to get the strsize, just use the max size range
  cex <- max(sizeRange)
  # helper function to calculate a useable size
  percent_of_range <- function(percent, range) {
    percent * diff(range) + min(range, na.rm = TRUE)}
  # plot the cor value
  ggally_text(
    label = as.character(rt), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = I(percent_of_range(cex * abs(r), sizeRange)),
    color = color,
    ...) + 
    # add the sig stars
    geom_text(
      aes_string(
        x = 0.8,
        y = 0.8),
      label = sig, 
      size = I(cex),
      color = color,
      ...) + 
    # remove all the background stuff and wrap it with a dashed line
    theme_classic() + 
    theme(
      panel.background = element_rect(
        color = "grey50", 
        linetype = "longdash"), 
      axis.line = element_blank(), 
      axis.ticks = element_blank(), 
      axis.text.y = element_blank(), 
      axis.text.x = element_blank())
}
# my_custom_cor(iris, aes(Sepal.Length, Sepal.Width))

# now apply to data
png(filename = "F5.png", w = 15/3, h = 15/3, units = "in", res = 300)
ggpairs(wf, aes(alpha = 0.4),
          upper = list(continuous = my_custom_cor),
          lower = list(continuous = my_custom_smooth)) + 
          theme(axis.line=element_blank(), 
          	axis.text=element_blank(), axis.ticks=element_blank())
dev.off()

#save.image(file = "all_data.RData")

##### END





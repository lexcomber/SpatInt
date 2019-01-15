library(RCurl)
library(jsonlite)
library('rvest')
library(tidyverse)
library(GISTools)
library(sf)

# see https://cfss.uchicago.edu/webdata005_scraping.html
# done at "Mon Jan 14 21:54:27 2019"

# 1.  Create list of property IDs from the website
zpid_list <- vector()
for (i in 1:50){	
	#url <- paste0("https://www.zillow.com/homes/for_sale/New-Haven-CT/6155_rid/mostrecentchange_sort/41.449417,-72.700654,41.146603,-73.157273_rect/10_zm/X1.dash.SS.dash.13t86598jwfzp_4wzn5_sse/", i,"_p/1_rs/1_fr/")
	#url <- paste0("https://www.zillow.com/homes/recently_sold/New-Haven-CT/6155_rid/globalrelevanceex_sort/41.392907,-72.81498,41.203585,-73.04329_rect/11_zm/", i, "_p/")
	url <- paste0("https://www.zillow.com/homes/for_rent/New-Haven-CT/6155_rid/41.392907,-72.81498,41.203585,-73.04329_rect/11_zm/", i, "_p/")
	webpage <- read_html(url)
	data <- html_text(webpage)
	length(data)
	nchar(data)
	data <- (strsplit(data, '\"search\",data:')[[1]][2])
	data <- (strsplit(data, ',namespace:\"search\"')[[1]][1])
	data <- gsub("\\{zpid:\\[", "", data)
	data <- gsub("\\],pg:\"1\"\\}", "", data)
	data <- unlist(strsplit(data, ','))
	zpid_list <- append(zpid_list, data)
	cat(i, "\t")
}
length(unique(zpid_list))

# 2. use this to download lat lon for each property
p.list <- sort(unique(zpid_list))
res_list <- matrix(nrow = 0, ncol = 2)
for (i in 253:length(p.list)) {
	pid.i <- p.list[i]
	#pid.i <- "57968605"
	url <- paste0("https://www.zillow.com/new-haven-ct/", pid.i, "_zpid/")
	if(url.exists(url)){
		webpage <- read_html(url)		
		webpage %>%
		html_nodes("*")  %>%  
		html_attr("href") -> tmp
		res.i <- tmp[grep("/homes/for_sale/", tmp)]
		if (length(res.i) > 0) {
			res_list <- rbind(res_list, cbind(pid.i, res.i))
			rm(list = c("res.i", "webpage"))
			cat("1:",i, "\t")
		}
	}
}

# now clean and tidy to get the data.frame 
#length(which(is.na(res_list)))
#head(res_list)
#length(unique(res_list[,"pid.i"]))

options(digits=9)
pid.list <- sort(unique(res_list[,"pid.i"]))
pid.list <- gsub("[[:punct:]]", "", pid.list)
X <- vector()
Y <- vector()
pid <- vector()
for (i in 1:length(pid.list)){
	pid.i <- pid.list[i]
	index.i <- res_list[,"pid.i"] == pid.i
	res_list.i <- res_list[index.i, ]
	if( length(grep("rect", res_list.i)) > 0) {
		res_list.i <- res_list.i[grep("rect", res_list.i[, "res.i"])[2],]
		tmp <- gsub("[[:alpha:]]", "", res_list.i[2])
		tmp <- gsub("_/0_", " ", tmp)
		tmp <- gsub("([.,-])|[[:punct:]]", "\\1", tmp)
		tmp <- gsub("--, ", "", tmp)
		tmp <- as.numeric(unlist(strsplit(tmp, ",")))
		Y <- append(Y, round(mean(tmp[1], tmp[3]), 6))
		X <- append(X, round(mean(tmp[2], tmp[4]), 6))
		pid <- append(pid, pid.i)
		cat(i, "\t")
	}
}
# check for fuckups
index <- which(is.na(Y))
# create df
df <- data.frame(ID = pid, X = X, Y = Y) 
# create sf
props_sf <- st_as_sf(SpatialPointsDataFrame(df[, 2:3], 
					data = data.frame(df[,1]), 
					proj4string = CRS("+proj=longlat +datum=WGS84")))
names(props_sf)[1] <- "PID"
save(props_sf, file = "zill.RData")

  

 
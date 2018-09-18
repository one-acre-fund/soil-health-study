rm(list=ls())
cat("/014")
# purpose: script template
# written by: matt lowes (matt.lowes@oneacrefund.org)
# written for: alex villec (alex.villec@oneacrefund.org), Jesse Goldfarb
# and Eric Solomonson
# last edited: 8 18 15

# objectives and hypotheses:
# ---------------------------------------------------------------------------------
# clean and tidy up SHS survey data
# eventually link this with the soil test results.

#libraries
library(ggplot2)
library(reshape2)
suppressMessages(library(dplyr))

#geospatial libraries
library(raster)
library(rgeos)
library(maptools)
library(dismo)
library(RColorBrewer)

#graphing libraries
library(grid)
library(gridExtra)

#directories:
wd <- "/Users/mlowes/drive/soil health study/data/rw baseline"
dd <- paste(wd, "data", sep="/")
od <- paste(wd, "output", sep="/")
md <- paste(wd, "maps", sep="/")
drive <- "~/drive/r_help/4_output/statistical_test_outputs"

#load data:
load(paste(dd, "shs rw baseline.Rdata", sep = "/"))
load(paste(drive, "output_functions.Rdata", sep="/"))

# ---------------------------------------------------------------------------------
# visualizations and mapping
# ---------------------------------------------------------------------------------
# http://www.milanor.net/blog/maps-in-r-plotting-data-points-on-a-map/
# keep only rows with both lon and lat
f <- d[!is.na(d$lon), ]

xy <- f[,c("lon", "lat")]
crdref <- CRS('+proj=longlat +datum=WGS84')
s <- SpatialPointsDataFrame(coords=xy, data=f, proj4string = crdref)
plot(s)

rw <- getData("GADM", country='RW', level=2, path = "/Users/mlowes/drive/soil health study/data") # level 2 has more detail than 1. how 
# high does it go? 3 has more! (or at least the file is larger)
plot(rw)

# save data for interpolation
save(s, rw, file=paste(dd, "rw_shs_interpolation.Rdata", sep = "/"))

# grey map with white lines
pdf(file=paste(md, "simple rw points.pdf", sep = "/"), width=11, height=8.5)
plot(rw, border='white', lwd=2, col="lightgrey")
# add points
points(s, cex=.25)
dev.off()

#plot ph ranges 
cuts <- c(4:8)
pols <- list("sp.polygons", rw, fill = "lightgray")
# set up a palette of interpolated colors for the points
blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
spplot(s, 'pH_RF', col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
# this automatically trims the map to only show the areas with the data


# try making voronoi polygons for the ph data
# should I transform everything into planar coordinates? If so, why and how?
# voronoi is making polygons around the points. If the points are clustered, this isn't
# that useful. Instead I want to associate points with polygons in the shape vector and 
# color those.
# v <- voronoi(s) # proximity polygons
# plot(v)
# v <- crop(v, rw) # show only the bounds of rw
# plot(v)
# as.character(crs(v))==as.character(crs(rw))
# # trim to rwanda
# rwb <- aggregate(rw)
# plot(rwb)
# as.character(crs(v))==as.character(crs(rwb))
# crs(v)
# crs(rwb)
# crs(rwb) <- crs(v) # check that this is an okay thing to do
# vrw <- intersect(v, rwb)
# 
# spplot(vrw, 'pH_BART', col.regions=rev(get_col_regions()))
# hm. not that useful. the voronoi was sort of weird
# now that I know what names are associated with each polygon, I want to show
# the average pH (or any value) in different maps.
as.character(crs(s))==as.character(crs(rw))
crs(s)
crs(rw)
crs(rw) <- crs(s) # check that this is an okay thing to do

e <- extract(rw[, "NAME_2"], s)
s$spatialname <- e$NAME_2

# aggregate - taking points and grouping them with the polygon - going points to polygons
ag <- aggregate()
# intersect - this adds polygons to the points
rwc <- intersect(s, rw)
# now try aggregating values by NAME_2
test <- aggregate(rwc@data[,99:120], by=list(rwc@data$NAME_2), FUN=mean, na.rm=T)

# ---------------------------------------------------------------------------------
# create lines with ggplot; make certain it's working
# ---------------------------------------------------------------------------------

frw <- fortify(rw, region="NAME_2")
# frw$id <- as.numeric(frw$id)
ggplot(frw, aes(x=long, y = lat, group=group)) + geom_path()

# data to be used by shiny app
save(frw,s, file=paste(dd, "rw_shs_mapping_data_shapes.Rdata", sep = "/"))


# add in data to frw, fortified data
#df <- dplyr::left_join(frw, s@data, by=c("id"="spatialname"))

# okay, the left join doesn't work so instead let me aggregate the point data by 
#  spatialname and then merge that data. I'll create a mean and sd by each trial

# according to emmanuel these outcomes are okay
# from BART, TN, Ca, Mg, Al, pH and from PLSR, Ca, OM, Mg, TN, Exch, Al, Hp

s.soil <- aggregate(s@data[,99:120], by=list(s@data$spatialname), function(x){
  mean(x, na.rm=T)
})


df <- dplyr::left_join(frw, s.soil, by=c("id"="Group.1"))


# ---------------------------------------------------------------------------------
# try making a simplified ph map
# ---------------------------------------------------------------------------------
map <- ggplot(df, aes(x=long, y=lat, group=group)) + geom_path() + 
  geom_polygon(aes(fill=pH_BART)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9,"Reds")), # define colors
                       name = "Soil pH",
                       guide = guide_colorbar(legend.direction = "vertical")) + 
  theme_bw() + 
  labs(title="Rwanda long term soil health baseline - 2016", x = "Longitude", y="Latitude")
  
map

# adjust TvC and if they grew maize recently?
# combine the data so that as options are selected it recalculates. How should
# the data be combined to achieve this?

# ---------------------------------------------------------------------------------
# compare this to 2015 results - use rw2
# basically repeat the process above but for the 2014 data
# ---------------------------------------------------------------------------------
load(file=paste("~/drive/oaf/Global Projects/Soil Samples 2014", "soil data.RData", sep="/"))
rw2 <- rw2 %>% dplyr::select(id, soil_id, district, cell, status, n_seasons, latitude_cc,
                      longtitude_cc, altitude_cc, ton.hectare, pH)

rw2 <- rw2[-which(is.na(rw2$longtitude_cc)),]
rw2 <- SpatialPointsDataFrame(coords = rw2[,c("longtitude_cc", "latitude_cc")],
                              data=rw2)

# get the name of the polygon from rw and put it in rw2
# load rw again as it was overwritten
rw <- getData("GADM", country='RW', level=2, path = "/Users/mlowes/drive/soil health study/data") # level 2 has more detail than 1. how 
crs(rw) <- crs(rw2)
# high does it go? 3 has more! (or at least the file is larger)
# plot(rw)
f <- extract(rw[, "NAME_2"], rw2)
rw2$spatialname <- f$NAME_2 # the soil data now has the polygon name

rw <- fortify(rw, region="NAME_2")

# soil 2014 parameters - for comparison
s14 <- aggregate(rw2@data[,"pH"], by=list(rw2@data$spatialname), function(x){
  mean(x, na.rm=T)
})

df14 <- dplyr::left_join(rw, s14, by=c("id"="Group.1"))

# and map the old results
# try making a simplified ph map
map14 <- ggplot(df14, aes(x=long, y=lat, group=group)) + geom_path() + 
  geom_polygon(aes(fill=x)) + 
  scale_fill_gradientn(colours = rev(brewer.pal(9,"Reds")), # define colors
                       name = "Soil pH",
                       guide = guide_colorbar(legend.direction = "vertical")) + 
  theme_bw() + 
  labs(title="Rwanda soil health results - 2014", x = "Longitude", y="Latitude")

map14

# put the two maps together and export
pdf(file=paste(md, "rwanda ph summary 14-16.pdf", sep = "/"), width=11, height=8)
print(map)
print(map14)
print(grid.arrange(map, map14, ncol=2))
dev.off()


#--------------------------------------------------------------------------------------
# interpolation
#--------------------------------------------------------------------------------------

# start with rw shs baseline.Rdata and rw shape file

# to match points to polygons - and know which polygon each point is in
# what do level=2 polygons represent?



# are these the same names as the ones given by OAF? NO
# s$mark <- ifelse(s$spatialname != s$district, 1,0)








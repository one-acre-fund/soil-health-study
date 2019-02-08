rm(list=ls())
cat("/014")
# purpose: script template
# written by: matt lowes (matt.lowes@oneacrefund.org)
# written for: alex villec (alex.villec@oneacrefund.org), Jesse Goldfarb
# and Eric Solomonson
# last edited: 8 18 15

# objectives and hypotheses:
# ---------------------------------------------------------------------------------
# generate CV validated interpolated values for all validated 
# soil parameters in Rwanda study


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

# Tps and IDW
library(fields)
library(gstat)
library(dismo)


#directories:
wd <- "/Users/mlowes/drive/soil health study/data/rw baseline"
dd <- paste(wd, "data", sep="/")
od <- paste(wd, "output", sep="/")
md <- paste(wd, "maps", sep="/")
drive <- "~/drive/r_help/4_output/statistical_test_outputs"

#load data:
load(paste(dd, "rw_shs_interpolation.Rdata", sep = "/"))
load(paste(drive, "output_functions.Rdata", sep="/"))

#--------------------------------------------------------------------------------------
# interpolation - following day3_lab1_CA.r example sort of.
#--------------------------------------------------------------------------------------

# start witih basic plot of points
# grey map with white lines
# plot(rw, border='white', lwd=2, col="lightgrey")
# # add points
# points(s, cex=.25)
# 
# #plot ph ranges 
# cuts <- c(4:8)
# pols <- list("sp.polygons", rw, fill = "lightgray")
# # set up a palette of interpolated colors for the points
# blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
# spplot(s, 'pH_BART', col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
# this automatically trims the map to only show the areas with the data

# according to emmanuel these outcomes are okay
# from BART, TN, Ca, Mg, Al, pH and from PLSR, Ca, OM, Mg, TN, Exch, Al, Hp


# start with global grid and then get 1km cutout of rwanda
#r <- raster(res=1/120)
r <- raster(res=1/12)
r <- crop(r, floor(extent(rw)))

# turn rwanda shape file into raster? it's not working. I'm losing the features
#r <- raster(rw, res=10000) # make rw spatial polygons into raster

vars = names(s)[99:120]
rrw <- rasterize(s, r, vars, mean) # points and rw polygons

#--------------------------------------------------------------------------------------
# thin plate spline - to try out the process
#--------------------------------------------------------------------------------------
# http://www.statmethods.net/advgraphs/axes.html - for base plot()
vars <- names(s)[c(110,101,108,99,113,120)]
# 
# pdf(file=paste(md, "rw_shs_bl_interpolation_soil_vars.pdf", sep = '/'), width=11, height=8.5)
# lapply(vars, function(x) {
#   m <- Tps(coordinates(s), s@data[,x])
#   # make raster layer with model, raster is rwanda empty raster, model is m
#   tps <- interpolate(r, m)
#   tps <- crop(tps, rw)
#   tps <- mask(tps, rw) # cuts the tps raster down to the rw boundaries
#   x <- gsub("_BART", "", x)
#   x <- gsub("_PLSR", "", x)
#   print(
#     plot(tps, main= paste("Soil TPS Interpolation ", x, sep="- ")),
#     plot(rw, add=T)
#   )
# })
# dev.off()

#--------------------------------------------------------------------------------------
# inverse distance weighting (IDW)
#--------------------------------------------------------------------------------------

## [inverse distance weighted interpolation]
# *** what does it mean to run an optimal IDW?
gs <- gstat(formula=pH_BART~ 1, locations=subset(s, !is.na(s$pH_BART)))
idw <- interpolate(r, gs)
idw <- crop(idw, rw)
idw <- mask(idw, rw)
pdf(file=paste(md, "rw_shs_bl_idw_ph.pdf", sep = '/'), width=11, height=8.5)
print(
  plot(idw, main= "Soil Inverse Distance Weighted Interpolation - pH"),
  plot(rw, add=T)
)  
dev.off()


#--------------------------------------------------------------------------------------
# CV different methods - mean (NULL), TPS, IDW, Randomforest if we have covariates?
#--------------------------------------------------------------------------------------

# *** questions
# optimization of idw
# ordinary kriging - have to look at variogram?


# root mean sq error for evaluating models
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(s$pH_BART,na.rm=T), s$pH_BART)
null

# set k folds to 5
set.seed(2060819)
nfolds <- 5
k <- kfold(s, nfolds) # from dismo

# cross validation of models
ensrmse <- tpsrmse <- idwrmse <- rep(NA, 5) # assing multiple objects at once

cv <- function(x) {
  
  for(i in 1:nfolds) {
    train <- s[k!=i,]
    test <- s[k==i,]

  train <- train[!is.na(train@data[,x]),]
    
  m <- gstat(formula=as.formula(paste(x, '~ 1')), locations=train)
  p1 <- predict(m, newdata=test, debug.level=0)$var1.pred
  idwrmse[i] <-  RMSE(test@data[,x], p1) #idw rsme
  
  # m <- gstat(formula=pH_BART~1, locations=train, model=fve) #kriging result
  # p2 <- predict(m, newdata=test, debug.level=0)$var1.pred
  # krigrmse[i] <-  RMSE(test$OZDLYAV, p2)
  
  m <- Tps(coordinates(train), train@data[,x]) # thin plate spline approach
  p3 <- predict(m, coordinates(test))
  tpsrmse[i] <-  RMSE(test@data[,x], p3)
  
  w <- c(idwrmse[i], tpsrmse[i]) # combine the rmse
  weights <- w / sum(w) # weight them
  ensemble <- p1 * weights[1] + p3 * weights[2] 
  # multiply predictions by weights
  ensrmse[i] <-  RMSE(test@data[,x], ensemble) # truly an ensemble result?
  }
  
  output <- rbind(idwrmse, tpsrmse, ensrmse)
  return(output)
  
}

# # call the funciton for each of the variables of interest
# *** not working  - golden.section.search - maximum iterations reached?
# *** what does this mean?
output <- lapply(vars, function(x){
  ini <- data.frame(cv(x))
  ini$ave <- apply(ini[,1:5], 1, function(y){mean(y, na.rm=T)})
  res <- paste("Best model is ", row.names(ini[which.min(ini$ave),]),  sep = "")
  return(list(ini, res))
})

#--------------------------------------------------------------------------------------
# Run tps for all covariates as it seems to be doing the best?
#--------------------------------------------------------------------------------------






rm(list=ls())
cat("/014")
# purpose: script template
# written by: matt lowes (matt.lowes@oneacrefund.org)
# written for: alex villec (alex.villec@oneacrefund.org), Jesse Goldfarb
# and Eric Solomonson
# last edited: 9 7 15

# objectives and hypotheses:
# ---------------------------------------------------------------------------------
# make simple boxplots of outcomes
# split by key variables to understand changes in distribution


#libraries
library(ggplot2)
library(reshape2)
suppressMessages(library(dplyr))

#geospatial libraries
library(raster)
library(rgeos)
library(dismo)

#directories:
wd <- "/Users/mlowes/drive/soil health study/data/rw baseline"
dd <- paste(wd, "data", sep="/")
od <- paste(wd, "output", sep="/")
md <- paste(wd, "maps", sep="/")
gd <- paste(wd, "graphs", sep = '/')
drive <- "~/drive/r_help/4_output/statistical_test_outputs"

#load data:
load(paste(dd, "shs rw baseline.Rdata", sep = "/"))
load(paste(drive, "output_functions.Rdata", sep="/"))

# ---------------------------------------------------------------------------------
# boxplots
# ---------------------------------------------------------------------------------

d$client <- factor(d$client, labels=c("Control", "Treatment"))

pdf(file=paste(gd, "rwanda shs boxplots.pdf", sep = "/"), width=11, height=8.5) 
for(i in names(d)[99:120]){
  print(ggplot(d, aes(x= client, y = d[,i])) +
          geom_boxplot() +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("Rwanda Soil Health Baseline ", i, sep = "- ") ,
               x = "Treatment", y = i))
}
dev.off()
  
  
  
  
  
  
  
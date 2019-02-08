rm(list=ls())
cat("/014")
# purpose: script template
# written by: matt lowes (matt.lowes@oneacrefund.org)
# written for: alex villec (alex.villec@oneacrefund.org), Jesse Goldfarb
# and Eric Solomonson
# last edited: 9 7 15

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
library(dismo)

#directories:
wd <- "/Users/mlowes/drive/soil health study/data/rw baseline"
dd <- paste(wd, "data", sep="/")
od <- paste(wd, "output", sep="/")
md <- paste(wd, "maps", sep="/")
drive <- "~/drive/r_help/4_output/statistical_test_outputs"

#load data:
load(paste(dd, "shs rw baseline.Rdata", sep = "/"))
load(paste(drive, "output_functions.Rdata", sep="/"))

# variable creation
# ---------------------------------------------------------------------------------
# baseline comparison of total sample and by district
# client v. non-client
# ---------------------------------------------------------------------------------

out.list <- c("female", "age", "hhsize", "own", "field.size",
              "n_season_fert", "n_season_compost", "n_season_lime", "n_season_fallow",
              "n_seasons_leg_1", "n_seasons_leg_2")

output <- do.call(rbind, lapply(out.list, function(x) {
  
  out <- t.test(d[,x] ~ d[,"client"], data=d)
  tab <- data.frame(out[[5]][[1]], out[[5]][[2]], out[3])
  tab[,1:2] <- round(tab[,1:2],3)
  names(tab) <- c(names(out[[5]]), "pvalue")
  return(tab)
}))

# use p.adjust with bonferroni correction
output$pvalue <- p.adjust(output$pvalue, method="holm")
output$pvalue <- ifelse(output[, 3] < 0.001, "< 0.001", round(output[, 3], 3)) 

rownames(output) <- out.list
colnames(output) <- c("Non-Tubura", "Tubura Client", "p-value")	

#write table
write.csv(output, file=paste(od, "baseline balance.csv", sep="/"), row.names=T)

# balance by district
# ---------------------------------------------------------------------------------
dist.output <- do.call(rbind, lapply(split(d, d$district), function(x) {
  
  tab <- do.call(rbind, lapply(out.list, function(y) {
    
    out <- t.test(x[,y] ~ x[,"client"], data=x)
    tab <- data.frame(out[[5]][[1]], out[[5]][[2]], out[3])
    tab[,1:2] <- round(tab[,1:2],3)
    names(tab) <- c(names(out[[5]]), "pvalue")
    #tab[,3] <- p.adjust(tab[,3], method="holm")
    tab[,3] <- ifelse(tab[,3] < 0.001, "< 0.001", round(tab[,3],3))
    #print(tab)
    return(tab)
  }))
  
  return(data.frame(district = unique(x$district), tab))
}))

rownames(dist.output) <- NULL
dist.output$variable <- rep(out.list,13)	

# order variables 
dist.output <- dist.output[, c(1, 5, 2:4)]
colnames(dist.output) <- c("District", "Varible", "Non-Tubura", "Tubura Client", "p-value")	

write.csv(dist.output, file=paste(od, "district balance.csv", sep="/"), row.names=T)

# count of T and C and total samples by district
# ---------------------------------------------------------------------------------

count <- d %>% group_by(district) %>% 
  dplyr::summarize(
    t.count = sum(ifelse(client==1,1,0)),
    c.count = sum(ifelse(client==0,1,0)),
    total = n()
  ) %>% ungroup()

count <- as.data.frame(count)
write.csv(count, file=paste(od, "final rw sample breakdown.csv", sep="/"), row.names=F)

# multivariate regressions evaluating relationship b/w soil health and practices
# ---------------------------------------------------------------------------------



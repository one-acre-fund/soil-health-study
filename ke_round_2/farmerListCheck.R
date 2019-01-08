# description of file
# written by:  @oneacrefund.org
# written for: @oneacrefund.org
# last edited: dd mmmm yyyy

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")


## load libraries

# dplyr is for working with tables
# reshape is for easy table transformation
# knitr is to make pretty tables at the end
# ggplot2 is for making graphs
libs <- c("dplyr", "reshape2", "knitr", "ggplot2", "readxl", "readr")
lapply(libs, require, character.only = T)

#### load data ####
# load the reference sheet
ref <- read_xls("Control Sheet Soil Sample 2017.xls", sheet = 1) %>%
  mutate(UNIQUEID = tolower(UNIQUEID),
         UNIQUEID = gsub(" ", "", UNIQUEID))


# load baseline data
baseDir <- normalizePath(file.path("..", "ke_shs_baseline", "data"))
load(paste(baseDir, "shs ke baseline.Rdata", sep = "/"))

# compare ref to baseline
mergeDir <- normalizePath(file.path("..", "mergeReport"))
source(paste(mergeDir, "mergeReport.R", sep = "/"))

mergeReport(ref$UNIQUEID, d$Soil_Sample_Id)

# merge and sample a couple farmers from each to make certain the demographic data 
# matches

ref <- ref %>%
  setNames(paste0(names(.), "_ref"))

combo <- d %>%
  select(Soil_Sample_Id, RegionName, DistrictName, SiteName, Last_Name, First_Name,
         oaf) %>%
  left_join(ref, ., by=c("UNIQUEID_ref" = "Soil_Sample_Id"))


combo %>%
  select(DistrictName_ref, DistrictName, SiteName_ref, SiteName, Last_Name_ref, Last_Name,
         First_Name_ref, First_Name) %>%
  as.data.frame() %>%
  sample_n(20)

# This file cleans the kenya shs data post r1 for analysis. The analysis is done
# is done in the shs_anlaysis file
# written by:  @oneacrefund.org written for:
# @oneacrefund.org last edited: dd mmmm yyyy

#### set up ####
# clear environment and console
rm(list = ls())
cat("\014")


## load libraries

# dplyr is for working with tables
# reshape is for easy table transformation
# knitr is to make pretty tables at the end
# ggplot2 is for making graphs
libs <- c("tidyverse", "knitr", "readxl")
lapply(libs, require, character.only = T)

select <- dplyr::select

#### load data ####
# load the ke r1 data to combine the new data with
load("../ke_round_1/ke_r1_cleaned_combined.Rdata") # fieldDat

# and the latest kenya survey rounds, should be 2 and 3??
source("../../oaflib/commcareExport.R")
source("../../oaflib/misc.R")


# load the latest Kenya data to add to the existing data
keDat17 <- read.csv("Kenya_2017_soil_health_study_data.csv")

# update names to the fieldDat names
names(fieldDat)


thingsToRemove <- paste(c("form\\.consent_yes\\.", "demography\\.", "basic_information\\.", "field\\.", "plot_information\\.", "inputs\\.", "other_respondent_details\\.", "field_information\\.", "livestock\\."), collapse = "|")
names(keDat17) <- gsub(thingsToRemove, "", names(keDat17))
names(keDat17) <- tolower(names(keDat17))
names(keDat17) <- gsub("\\_", ".", names(keDat17))

keDat17 <- keDat17 %>%
  rename(region = region.name,
         d_client = oaf,
         respondent.gender = gender,
         plot.size = field.size,
         main.crop = maincrop.maincrop,
         main.crop.specify = maincrop.specify.other.main.crop,
         seed.type = maincrop.seedtype,
         seed.kgs = maincrop.seedkgs,
         fertilizer.main = input.maincrop,
         fertilizer.intercrop = input.intercrop,
         dap.main = dap.kg,
         can.main = can.kg,
         npk.main = npk.kg,
         urea.main = urea.kg,
         dap.intercrop = dap.kg.intercrop,
         can.intercrop = can.kg.intercrop,
         npk.intercrop = npk.kg.intercrop,
         urea.intercrop = urea.kg.intercrop,
         lime.kgs = lime.kg, # continue from here harmonizing names...
         plot.location = field.location,
         field.location = location,
         intercrop.seed.kgs = intercrop.seedkgs,
         intercrop.seed.type = intercrop.seedtype,
         yield = maincrop.maincrop.total.yield.in.kilograms,
         harvestcomp.2016 = maincrop.yield.comparative,
         intercrop.harvestcomp = intercrop.yield.comparative,
         erosion = anti.erosion,
         intercrop = intercrop.intercrop,
         intercrop.specify = intercrop.specify.other.main.crop,
         intercrop.yield = intercrop.intercrop.yiedl,
         sample_id = soil.sample.id
         ) %>%
  mutate(season = 2017)
  

# and now figure out how to match variable names as quickly as possible. 
# I'll first weed out any variable names that already match and only focus on the remaining ones

matchedNames <- names(keDat17)[names(keDat17) %in% names(fieldDat)]

# and now just look at remaining names until matched:
focusVariables <- names(keDat17)[!names(keDat17) %in% matchedNames]

names(fieldDat)[!names(fieldDat) %in% matchedNames]
focusVariables


## okay, I've merged the variables that I can. I now need the soil data. I hope we have it!


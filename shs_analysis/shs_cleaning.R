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



#### load data ####
# load the ke r1 data to combine the new data with
load("../ke_round_1/ke_r1_cleaned_combined.Rdata")

# and the latest kenya survey rounds, should be 2 and 3??

#### prep data ####



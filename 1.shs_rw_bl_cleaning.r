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
library(stringr)

#directories:
wd <- "/Users/mlowes/drive/soil health study/data/rw baseline"
dd <- "/Users/mlowes/drive/One Acre Fund Soil Data/Rwanda/2015_A/Soil_Health_Study"
od <- paste(wd, "output", sep="/")
drive <- "~/drive/r_help/4_output/statistical_test_outputs"

#load data:
# This data is being drawn from the Soil lab repository. It has the baseline data with it
d <- read.csv(paste(dd, "Rwanda_2015_A_Complete_Soil_Predictions.csv", sep="/"),  stringsAsFactors=FALSE)

load(paste(drive, "output_functions.Rdata", sep="/"))

# cleaning
# ---------------------------------------------------------------------------------

# take out weird CommCare stuff
d[d=="---"] <- NA

# take out demographic and text_final_question from variable names
#to.change <- c("text_final_questions.", "intro_champ_echantillon.",
#	"demographic_info.", "other_inputs_", "crop1_15b_inputs.", "crop2_15b_inputs.",
#	"^15b.", "historical_intro.")

#names(d) <- tapply(to.change, function(x) { gsub(x, "", names(d))})

names(d) <- gsub("text_fil_questions", "", names(d))
names(d) <- gsub("intro_champ_echantillon", "", names(d))
names(d) <- gsub("demographic_info", "", names(d))
names(d) <- gsub("other_inputs_", "", names(d))
names(d) <- gsub("crop1_15b_inputs", "", names(d))
names(d) <- gsub("crop2_15b_inputs", "", names(d))
names(d) <- gsub("^15b", "", names(d))
names(d) <- gsub("historical_intro", "", names(d))

names(d)[names(d)=="field_dim"] <- "field_dim1"
names(d)[names(d)=="v51"] <- "field_dim2"


# deal with names and drop unnecessary variables
d <- d %>% 
	dplyr::select(-c(rownumber, infoformid, introductiond_accept, photo,
		infocompleted_time, 
		enumerator_me, contains("phone"), farmer_me, farmersurme, farmerme,
		d_respondent, additiolsamplepackedandsenttol, additiolsamplerequestedfromlab,
		datedryingcompleteifnecessary, driedindistrictifnecessary, senttohqyo,
		collectedindistrictyo, excessstoredathq_, receivedathq_,dateofinitialdryingifnecessary,
		samplecollectedinfieldyo, field_des, samplewetordry)) %>%
	rename(
	female = sex,
	age = age_cultivateur,
	own = d_own,
	client = d_client) %>%
	mutate(
	female= ifelse(female=="gore", 1,0),
	field.size = field_dim1*field_dim2
	)

d$total.seasons <- apply(d[, grep("d_season_list", names(d))], 1, function(x) {
	sum(x, na.rm=T)})

# check season sum with client
# table(d$total.season, d$client)

# FIX VARIABLE NAMES!
names(d)[names(d)=="field_kg_fert1_1"] <- "field_kg_fert1_15b"
names(d)[names(d)=="field_kg_fert2_1"] <- "field_kg_fert2_15b"
names(d)[names(d)=="field_kg_compost"] <- "field_kg_compost_15b"


# recode to numeric
varlist <- c("client", "own", "crop1_15b_seedkg", "crop1_15b_yield", "crop1_15b_yield_",
	"crop2_15b_seedkg", "crop2_15b_yield", "crop2_15b_yield_", "field_kg_fert1_15b",
	"field_kg_fert2_15b", "field_kg_compost_15b", "d_lime_15b", "kg_lime_15b")

# check that there aren't values hidden in the character variables
#apply(d[,varlist], 2, function(x){table(x, useNA='ifany')})

# recode characters to numerics
d[, varlist] <- sapply(d[,varlist], as.numeric)

# divide out GPS coordinates
# http://rfunction.com/archives/1499

# replace the blank gps_pic_guide with info
d <- cbind(d, str_split_fixed(d$gps_pic_guid, " ", n=4))
names(d)[121:124] <- c("lat", "lon", "alt", "precision")
d[,c("lat", "lon", "alt", "precision")] <- sapply(d[,c("lat", "lon", "alt", "precision")],
                                                  function(x){as.numeric(as.character(x))})

# check out missing district records
# ---------------------------------------------------------------------------------
table(d$district, useNA = 'ifany')
d[d$district=="",]
# these are the sample for which we have soil data but not survey data drop these for now
d <- d[-which(d$district==""),]


# # for david: 
# out <- as.matrix(table(d$district, d$client))
# write.csv(out, file=paste(od, "rw sample breakdown.csv", sep="/"))

save(d, file=paste(paste(wd, "data", sep = "/"), "shs rw baseline.Rdata", sep = "/"))

# go to shs rw analysis.R for analysis.

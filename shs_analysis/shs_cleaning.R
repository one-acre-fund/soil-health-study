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

####### LOAD MERGING IDS BETWEEN SURVEY AND LAB #######

soilDir <- normalizePath(file.path("..", "..", "..", "OAF Soil Lab Folder", "Projects", "ke_shs_17", "5_merged"))

mergeIds <- read_xlsx(paste(soilDir, "soil_texture_results.xlsx", sep = "/")) %>%
  mutate(reception_barcode = gsub("_", "", reception_barcode)) %>%
  as.data.frame()

####### LOAD SOIL DATA FOR KENYA SHS 17 #######

ke17soil <- readRDS("kenya_shs_17_soil_values_dw.rds")

## check for right merge variable
table(ke17soil$SSN %in% mergeIds$reception_barcode)

# look at the one unmatched value
ke17soil$SSN[!ke17soil$SSN %in% mergeIds$reception_barcode]


# and the latest kenya survey rounds, should be 2 and 3??
source("../../oaflib/commcareExport.R")
source("../../oaflib/misc.R")


# load the latest Kenya data to add to the existing data
keDat17 <- read.csv("Kenya_2017_soil_health_study_data.csv", stringsAsFactors = F, na.strings = "---")


# check that keDat17 has a variable that can merge with the mergeid data to link in soil data
keDat17$formid <- gsub("-", "", keDat17$formid)


table(keDat17$formid %in% mergeIds$info.caseid)  
head(keDat17$formid)
head(mergeIds$info.caseid)

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

## but then keep the keDat17$formid variable to try to match it.


## do some cleaning to make data ready to combine
# these functions come from the ke_round_1.Rmd file

identifyCatVars <- function(dat){
  # takes a df and returns the variable names that are categorical variables
  return(names(dat)[sapply(dat, function(x){is.character(x)})])
}

identifyCatVars(fieldDat)


varClean <- function(dat, x, toRemove){
  dat[,x] <- ifelse(dat[,x] %in% toRemove, NA, dat[,x])
  return(dat[,x])
}

strTable <- function(dat, x){
  varName = x
  tab = as.data.frame(table(dat[,x], useNA = 'ifany'))
  tab = tab[order(tab$Freq, decreasing = T),]
  end = ifelse(length(tab$Var1)<10, length(tab$Var1), 10)
  repOrder = paste(tab$Var1[1:end], collapse=", ")
  out = data.frame(variable = varName,
                   responses = repOrder)
  
  return(out)
}

# clean up known values
catEnumVals <- c("-99", "-88", "- 99", "-99.0", "88", "_88", "- 88", "0.88",
                 "--88", "__88", "-88.0", "99.0")


## !!! change the data slightly so that I'm only cleaning the variables I care about !!!
## okay, I've merged the variables that I can. I now need the soil data. I hope we have it!
# subset the data so I can only append the columns that match.
keDat17Append <- keDat17[, names(keDat17) %in% names(fieldDat)] 


# and then force all factors into characters to simplify working with the data
keDat17Append <- keDat17Append %>%
  mutate_if(is.factor, as.character) %>%
  as.data.frame()

# clean up keDat17 categorical variables
keDat17Append[,identifyCatVars(keDat17Append)] <- sapply(identifyCatVars(keDat17Append), function(y){
  keDat17Append[,y] <- varClean(keDat17Append,y, catEnumVals)
})

# and plot
repGraphs <- function(dat, x){
  tab = as.data.frame(table(dat[,x], useNA = 'ifany'))
  tab = tab[order(tab$Freq, decreasing = T),]
  # the printing of the graph happens here!!
  
    print(
      ggplot(data=tab, aes(x=Var1, y=Freq)) + geom_bar(stat="identity") +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title =paste0("Composition of variable: ", x))
    )
}

catVarsToIgnore <- c("plot.location", "photo", "sample_id", "gps", "site")
catVarsToPlot <- identifyCatVars(keDat17Append)[!identifyCatVars(keDat17Append) %in% catVarsToIgnore]

pdf(file = "ke17_shs_analysis_categorical_cleaning.pdf", width = 11, height = 8.5)
for(i in 1:length(catVarsToPlot)){
  repGraphs(keDat17Append, catVarsToPlot[i])
}
dev.off()

# and then clean up issues
keDat17Append <- keDat17Append %>%
  mutate(region = ifelse(grepl("western", tolower(region)), "Western Province",
                         ifelse(grepl("nyan", tolower(region)), "Nyanza",
                                ifelse(region == "OK", NA, region))),
         intercrop = ifelse(intercrop == "0", NA, intercrop),
         erosion = ifelse(erosion == "0", NA, erosion)
         )

# clean up numeric variables

identifyNumVars <- function(dat){
  # takes a df and returns the variable names that are categorical variables
  return(names(dat)[sapply(dat, function(x){is.numeric(x)})])
}


#Basic cleaning of known issues like enumerator codes for DK, NWR, etc.

enumVals <- c(-88,-85, -99, -66)

keDat17Append[,identifyNumVars(keDat17Append)] <- sapply(identifyNumVars(keDat17Append), function(y){
  keDat17Append[,y] <- varClean(keDat17Append,y, enumVals)
})

numVarsToIgnore <- c("oafid", "season")
numVarsToPlot <- identifyNumVars(keDat17Append)[!identifyNumVars(keDat17Append) %in% numVarsToIgnore]

# and plot
pdf(file = "ke17_shs_analysis_numerical_cleaning.pdf", width = 11, height = 8.5)
for(i in 1:length(numVarsToPlot)){
  base <- ggplot(keDat17Append, aes(x=keDat17Append[,numVarsToPlot[i]])) + labs(x = numVarsToPlot[i])
  temp1 <- base + geom_density()
  temp2 <- base + geom_histogram(bins = 10)
  #temp2 <- boxplot(r[,numVars[i]],main=paste0("Variable: ", numVars[i]))
  multiplot(temp1, temp2, cols = 2)
}
dev.off()

# and then addressing some potentially large values
# 300 chickens
keDat17Append %>%
  filter(chickens > 250)

keDat17Append$chickens <- ifelse(keDat17Append$chickens==300, 30, keDat17Append$chickens)
  
# 20 cows
keDat17Append %>%
  filter(cows > 15) # okay, leave it.

# 200 kg dap >> check plot size
keDat17Append %>% filter(dap.main > 150) # okay, seems okay reasonable.
# 200 kg can >> check plot size
# 100 kg urea >> check plot size
keDat17Append %>% filter(urea.main > 75) # okay, seems reasonable
# compost >> 15000 kgs?
keDat17Append %>% filter(compost.kgs > 10000) # 15k compost? Seems unreasonable, set to median

keDat17Append$compost.kgs <- ifelse(keDat17Append$compost.kgs > 10000, median(keDat17Append$compost.kgs), keDat17Append$compost.kgs)

#### make new outcome variables like inputs per acreage

m2ToAcres <- function(input, meters) {
  # function that takes inputs in kg/m2 and converts it to kg/acres
  res <- (input/meters)*4046
  return(res)
}


keDat17Append <- keDat17Append %>% 
  mutate(
    plot.m2 = plot.size*4046,
    can.acre = m2ToAcres(can.main, plot.m2),
    dap.acre = m2ToAcres(dap.main,plot.m2),
    npk.acre = m2ToAcres(npk.main,plot.m2),
    urea.acre = m2ToAcres(urea.main,plot.m2),
    compost.acre = m2ToAcres(compost.kgs,plot.m2),
    seed.acre = m2ToAcres(seed.kgs,plot.m2),
    intercrop.seed.acre = m2ToAcres(intercrop.seed.kgs, plot.m2),
    intercrop.dap.acre = m2ToAcres(dap.intercrop, plot.m2),
    intercrop.can.acre = m2ToAcres(can.intercrop, plot.m2),
    intercrop.npk.acre = m2ToAcres(npk.intercrop, plot.m2),
    intercrop.urea.acre = m2ToAcres(urea.intercrop, plot.m2))


# import and clean the most recent soil data
#source("soil_values_access.R") # this pulls the right data from the dw

names(keDat17Append)[grep("id", names(keDat17Append))] # find id var
names(keDat17)[grep("case", names(keDat17))] # find case id var

keDat17Append %>%
  select(sample_id) %>%
  sample_n(10)




# then combine with fieldDat!!

fieldDat <- plyr::rbind.fill(fieldDat, keDat17Append)


# then do some quick reality checking of the overall data set

table(fieldDat$district)

#################### RWANDA 17 CLEANING ################

# IMPORT THE CLEANED AND COMBINED RWANDA DATA
rw1Dir <- normalizePath(file.path("..", "rw_round_1"))


rwFieldDat <- readRDS(paste(rw1Dir, "r1FieldDat.rds", sep = "/"))

#### IMPORT THE MOST RECENT SHS SURVEYS FROM RWANDA
# in an ideal world this comes from CC but I don't want to absorb that complication right now.
# just use the export and move on.

#rw17b <- getFormData("oafrwanda", "M&E", "17B Ubutaka (Soil)", forceUpdate=F)

#names(rw17b) <- tolower(make.names(names(rw17b)))

# rw17bClean <- rw17b %>%
#   setNames(gsub("x.form.", "", names(.))) %>%
#   mutate(sample_id = tolower(sample_id))

# also import the csv from CC to get the variable names more easily:
rw17b <- read.csv("rwanda_17b.csv", stringsAsFactors = F, na.strings = "---") %>%
  setNames(gsub("form\\.","", names(.)))


# and import the linking information for rw_17b from the OAF Soil Lab Folder
rw17bSoilDir <- normalizePath(file.path("..", "..", "..", "OAF Soil Lab Folder", "Projects", "rw_shs_second_round", "5_merged"))

# sample_id looks like it'll match the data.
rw17BMergeId <- read_xlsx(paste(rw17bSoilDir, "database.xlsx", sep = "/")) %>%
  setNames(make.names(tolower(names(.))))

rw17bSoilValues <- readRDS("rwanda_shs_17b_soil_values_dw.rds") %>%
  setNames(make.names(tolower(names(.))))

# make variables numeric that should be numeric
rw17SoilVars <- c(names(rw17bSoilValues)[which(names(rw17bSoilValues)=="zinc"):
                                         which(names(rw17bSoilValues)=="hp")], "ph")

rw17bSoilValues[,rw17SoilVars] <- sapply(rw17bSoilValues[,rw17SoilVars], function(x){as.numeric(x)})

# compare the database result with the soil values to make sure there are
# matches across all the data

# check the ids and the soil values first << comparing ssn
table(rw17BMergeId$lab.ssn %in% rw17bSoilValues$ssn) # they're all there!

# this is what I want to combine with the survey data!
rw17bSoilMerged <- left_join(rw17BMergeId, rw17bSoilValues, by=c("lab.ssn" = "ssn"))

# check the merge with the survey data << okay, some of the surveys don't have
# soil. That's probalby okay but we should look into that
table(rw17b$sample_id %in% rw17bSoilMerged$sample.id)

# fully combined. we're missing some connetions but that's not the worst thing. 
# let's look at where we're not getting matches and note
rw17bMerged <- left_join(rw17b, rw17bSoilMerged, by = c("sample_id" = "sample.id"))

# clean up objects from here to simplify code a bit.
rm(rw17bSoilDir, rw17BMergeId, rw17bSoilValues, rw17bSoilMerged, rw17b)


#### here's where I can import the rw_18 survey data but I don't think we yet
#have spectral data so there's nothing to do  ######


#and now reference the cleaning in the rw_round_2_check to merge this latest
#data with the exiting data.
# also reference the rw_round_1 code to see how to align the data with previous seasons

#### CATEGORICAL VALUE CLEANING #####
# examine categorical and numeric variables for odd values:
rw17bMerged[,identifyCatVars(rw17bMerged)] <- sapply(identifyCatVars(rw17bMerged), function(y){
  rw17bMerged[,y] <- varClean(rw17bMerged,y, catEnumVals)
})

rw17bMerged <- rw17bMerged %>%
  mutate_if(is.factor, as.character) %>%
  as.data.frame()


# instead build in check to look at number of uniqe values and exclude those with more than 20+
catVarsToIgnore <- c("formid", "start_time", "enumerator", "not_find_why", "not_find_why_other",
                     "intro.intro", "soil_id", "soil_id2_", "text_final.photo", "text_final.d_photo",
                     "text_final.d_soilsample", "farmer.cell_input_17b", "farmer.cell_field_17b",
                     "farmer.village_17b", "farmer.farmer_name", "farmer.name_respondent",
                     "farmer.farmer_tel", "farmer.tel_respondent", "farmer.neighbor_tel",
                     "other_comments", "finish_time", "Description", "Enumerator_in_16B",
                     "Respondent_in_16B", "sample_id", "gps_hidden", "Farmer_phone", "Enumerator_in_15B",
                     "Respondent_Phone", "name", "completed_time", "started_time", "username", "received_on",
                     "case..case_id", "umudugudu", "lab.ssn", "comments", "condition")
catVarsToPlot <- identifyCatVars(rw17bMerged)[!identifyCatVars(rw17bMerged) %in% catVarsToIgnore]


# plot
pdf(file = "rw17_shs_analysis_categorical_cleaning.pdf", width = 11, height = 8.5)
for(i in 1:length(catVarsToPlot)){
  repGraphs(rw17bMerged, catVarsToPlot[i])
}
dev.off()


### notes:
# clean up district << or just use the other variable
# make cell to lower, check for duplicates
# clean up village

# and then clean up issues
rw17bMerged <- rw17bMerged %>%
  mutate(female = ifelse(farmer.farmer_gender == "female", 1, 0),
         client15b = ifelse(Client_in_15B == "Yego", 1, 0)
  )


#### NUMERICAL VALUE CLEANING ####
rw17bMerged[,identifyNumVars(rw17bMerged)] <- sapply(identifyNumVars(rw17bMerged), function(y){
  rw17bMerged[,y] <- varClean(rw17bMerged,y, enumVals)
})

numVarsToIgnore <- c("number", "intro.intro", "ext_final.d_photo", "text_final.d_soilsample")
numVarsToPlot <- identifyNumVars(rw17bMerged)[!identifyNumVars(rw17bMerged) %in% numVarsToIgnore]

# and plot
pdf(file = "rw17_shs_analysis_numerical_cleaning.pdf", width = 11, height = 8.5)
for(i in 1:length(numVarsToPlot)){
  base <- ggplot(rw17bMerged, aes(x=rw17bMerged[,numVarsToPlot[i]])) + labs(x = numVarsToPlot[i])
  temp1 <- base + geom_density()
  temp2 <- base + geom_histogram(bins = 10)
  #temp2 <- boxplot(r[,numVars[i]],main=paste0("Variable: ", numVars[i]))
  multiplot(temp1, temp2, cols = 2)
}
dev.off()

# notes
# something weird in age
rw17bMerged %>%
  filter(farmer.age_farmer > 100)

rw17bMerged <- rw17bMerged %>%
  mutate(farmer.age_farmer = ifelse(farmer.age_farmer == 6161, 61,
                                    ifelse(farmer.age_farmer == 122, NA, farmer.age_farmer)))


# hh size
rw17bMerged %>%
  filter(farmer.hh_n > 20) 

rw17bMerged %>%
  filter(!is.na(farmer.hh_n)) %>%
  ggplot(., aes(x = farmer.hh_n)) + geom_histogram(bins = 20)


rw17bMerged <- rw17bMerged %>%
  mutate(farmer.hh_n = ifelse(farmer.hh_n > 20, NA, farmer.hh_n))

# check out cows and goats and chickens and pigs and sheep >> very big values
rw17bMerged %>%
  filter(n_cows > 10 | n_chickens > 20 | n_pigs > 10 | n_sheep > 10)

# it doesn't seem that the big animal values are with the same farmers. These are 
# much larger than the other values but it doesn't necessarily mean they're impossible.
# 42 sheep seems like a lot but it's not inconceivable.

# field length?
rw17bMerged %>%
  filter(soil_field.length_17b > 150) %>%
  select(soil_field.length_17b, soil_field.width_17b, soil_field.field_area_17b, soil_field.field_ares)

# there's one field that's 152 x 3. That seems unlikely?
# or 170 x 10? Also seems unlikely >> get M&E advice on these.

# with seed quantities>>
rw17bMerged %>%
  select(contains("seed"), contains("field.")) %>%
  filter(prim_17a_.kg_seed_crop1_17a > 100)

# seed quantities don't match up with field sizes...why aren't these checks automated.

# compost amounts
rw17bMerged %>%
  select(contains("seed"), contains("compost"), contains("field")) %>%
  filter(ammend_17b_.kg_compost_17b > 1000)

# there's one clear strange value, fix that
rw17bMerged <- rw17bMerged %>%
  mutate(ammend_17b_.kg_compost_17b = ifelse(ammend_17b_.kg_compost_17b > 10000, NA, ammend_17b_.kg_compost_17b))

# types of crops
rw17bMerged %>%
  filter(n_types_crop_17b > 10)

# 25? That seems like too many in a single plot. Set this to missing but also ask M&E.s
rw17bMerged <- rw17bMerged %>%
  mutate(n_types_crop_17b = ifelse(n_types_crop_17b > 10, NA, n_types_crop_17b))

# ares
rw17bMerged %>%
  filter(soil_field.field_ares > 10) %>%
  select(contains("are")) %>%
  sample_n(10)

# these seem reasonable. Leave as they are.

# large aluminum value << set to median value
rw17bMerged %>%
  filter(exchangeable_aluminium > 30)

rw17bMerged <- rw17bMerged %>%
  mutate(exchangeable_aluminium = ifelse(exchangeable_aluminium > 30, median(exchangeable_aluminium, na.rm=T), exchangeable_aluminium))

##### NOW RESHAPE THE NEW DATA TO MATCH THE OLDER DATA, COMBINE INTO ONE DF ######
# do some additional variable name cleaning to simplify rehaping the data
names(rw17bMerged) <- gsub("ammend_17._\\.|farmer\\.", "", names(rw17bMerged))

names(rw17bMerged)[duplicated(names(rw17bMerged)) | duplicated(names(rw17bMerged), fromLast = T)]

varsToUpdate <-  names(rw17bMerged)[grep("fert\\d|crop\\d", names(rw17bMerged))]

# then use this vector to update these variable names efficiently rather than
# typing them all out. So for all these variables add the right number to the end
# and then update the portion in the middle.

# this is tailored for this survey but should be more flexible. not sure how so
# just going to keep it simple
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

namesForReshape <- function(name){
  options = paste(c(paste0("fert", 1:2), paste0("crop", 1:2)), collapse = "|")
  whichToUse = strapplyc(name, options, simplify = T)
  temp1 = sub(whichToUse, "", name)
  # now take the last digit and put a _ before it and tack it on the end.
  tack = paste0("_", substrRight(whichToUse, 1))
  temp2 = paste0(temp1, tack)
  temp2 = gsub("_\\.", "", temp2)
  return(temp2)
  
}

lapply(varsToUpdate, function(x){namesForReshape(x)})





















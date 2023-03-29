###########################################################################
### Fertilizer Files Code for 2023 UF Advanced Environmental Journalism ###
######################### Joan Meiners 2023 ###############################


### This script is for analyzing phosphorus site location data ####

#load libraries
library(dplyr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(MASS) 
library(reshape2) 
library(reshape) 
library(quantreg)


# prep data
# Step 1: Phosphorus plant/mine/stack data compiled by Katie Delk
# Step 2: Reformat column titles, remove extra header rows, save as csv
# Step 3: Remove duplicates and year column, clean up some name spelling
# Step 4: Find sociodemographic Census data for Florida to join with Katie's data
# Step 5: Copy list of zip codes in Florida to use to confine Census data from https://www.florida-demographics.com/zip_codes_by_population

# load data
setwd("C:/Users/JMeiners/OneDrive - Gannett Company, Incorporated/Desktop/UF Adjunct Teaching/Fertilizer Data/Student Data/Katie Delk")
katie_plant_data = read.csv("PhosphorusSites_katie_cleaned.csv", header = TRUE)
dim(katie_plant_data) #36 9
View(katie_plant_data)

census = read.csv("ACSDP5Y2021.DP05-Shortened_National_extraheaderremoved.csv", header = TRUE)
dim(census) #33774 62
View(census)

income_FL = read.csv("ACSST5Y2021.S1901-IncomeFlorida_extraheadersremoved.csv", header = TRUE)
dim(income_FL) #2026 57
View(income_FL)

#remove duplicate rows in income df
income_FL <- unique(income_FL)
dim(income_FL) #1013 58

FL_zips = read.csv("Florida_ZipCodes_Population.csv", header = TRUE)
dim(FL_zips) #937 4
View(FL_zips)
write.csv(FL_zips, file = "FL_zips.csv", row.names= FALSE)

# create new column in census file for just the five digit zip code, from their ZCTA5
census$Geographic.Area.Name = trimws(census$Geographic.Area.Name) #remove extra whitespaces
census$zip = substr(census$Geographic.Area.Name, 6, 11)
census$zip = as.numeric(census$zip)
#write.csv(census, file = "census.csv", row.names= FALSE)

# create new column in income file for just five digit zip code
income_FL$Geographic.Area.Name = trimws(income_FL$Geographic.Area.Name) #remove extra whitespaces
income_FL$zip = substr(income_FL$Geographic.Area.Name, 6, 11)
income_FL$zip = as.numeric(income_FL$zip)

# join income and census data
census_income = join(census, income_FL, by = "zip",  type = "left")
dim(census_income) #33774 119
View(census_income)

# join zipcode data to limit census data by Florida
census_FL = join(FL_zips, census_income, by = "zip",  type = "left")
dim(census_FL) #937 120
View(census_FL)

# join to katie's data
katie_FL = join(census_FL, katie_plant_data, by = "zip",  type = "left")
dim(katie_FL) #938 128
View(katie_FL)

# tally phosphorus sites per zip code
phosphorus_tally = katie_FL %>%
  group_by(type, zip) %>%
  tally()
View(phosphorus_tally)

# join back in the demographics data
katie_FL_sites = join(phosphorus_tally, census_FL, by = "zip",  type = "left")
dim(katie_FL_sites) #937 122
View(katie_FL_sites)

# limit df to relevant variables
colnames(katie_FL_sites)
katie_short = katie_FL_sites[, c(2,1,3,4,5,6,40,60,68,69,70,71,72,73,74,75,76,77,78,79,80)]
dim(katie_short) #945 21
View(katie_short)
katie_short$n = as.numeric(katie_short$n)
#write.csv(katie_short, file = "katie_short.csv", row.names= FALSE)

# change "n" variable to read "0" for type = NA
katie_type = katie_short %>% mutate(type = ifelse(is.na(type), "none", type))
katie_type$n[katie_type$type == "none"] <- 0 
katie_type$n = as.numeric(katie_type$n)
View(katie_type)

# analyze data for patterns with plant numbers and race data
colnames(katie_type)
hist(katie_type$n)
hist(as.numeric(katie_type$Percent..RACE..Total.population..One.race..White)) # normal enough??
racediff_type = glm(katie_type$n ~ as.numeric(katie_type$Percent..RACE..Total.population..One.race..White))
summary(racediff_type) # "." moderately significant

# analyze data for patterns with plant numbers and median income
colnames(katie_type)
hist(katie_type$n)
hist(as.numeric(katie_type$Estimate..Households..Median.income..dollars.)) # normal enough??
incomediff_type = glm(katie_type$n ~ as.numeric(katie_type$Estimate..Households..Median.income..dollars.))
summary(incomediff_type) # moderately significant, but why are there two P-vals??


# take out site type and tally all sites by zip code
colnames(katie_FL)
katie_notype = katie_FL[, c(1:128)]
dim(katie_notype) #990 128
katie_notype = katie_notype %>%
  group_by(zip) %>%
  tally()
dim(katie_notype) #937 2
View(katie_notype)

# add back in demographic data 
katie_notype = join(katie_notype, census_FL, by = "zip",  type = "left")
dim(katie_notype) #937 121
View(katie_notype)

# analyze race patterns again without site type
colnames(katie_notype)
hist(katie_notype$n)
hist(as.numeric(katie_notype$Percent..RACE..Total.population..One.race..White)) # normal enough??
racediff_notype = glm(katie_notype$n ~ as.numeric(katie_notype$Percent..RACE..Total.population..One.race..White))
summary(racediff_notype) # *** highly significant

# analyze income patterns again without site type
colnames(katie_notype)
hist(katie_notype$n)
hist(as.numeric(katie_notype$Estimate..Households..Median.income..dollars.)) # normal enough??
incomediff_notype = glm(katie_notype$n ~ as.numeric(katie_notype$Estimate..Households..Median.income..dollars.))
summary(incomediff_notype) # *** highly significant

###########################################################################
### Fertilizer Files Code for 2023 UF Advanced Environmental Journalism ###
######################### Joan Meiners 2023 ###############################
###########################################################################

rm(list=ls()) #clear old dfs, etc.

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
library(lme4)
library(sjPlot)


# prep data (see more detail on GitHub repo wiki)
# Step 1: Phosphorus plant/mine/stack data compiled by Katie Delk
# Step 2: Reformat column titles, remove extra header rows, save as csv
# Step 3: Remove duplicates and year column, clean up some name spelling
# Step 4: Find sociodemographic Census data for Florida to join with Katie's data
# Step 5: Copy list of zip codes in Florida to use to confine Census data from https://www.florida-demographics.com/zip_codes_by_population


######################
##### load data ######
######################

setwd("C:/Users/JMeiners/OneDrive - Gannett Company, Incorporated/Desktop/UF Adjunct Teaching/Fertilizer Data/Student Data/Katie Delk")
katie_sites_data = read.csv("PhosphorusSites_katie_cleaned.csv", header = TRUE)
dim(katie_sites_data) #91 11
View(katie_sites_data)

census = read.csv("ACSDP5Y2021.DP05-Shortened_National_extraheaderremoved.csv", header = TRUE)
dim(census) #33774 61
View(census)

income_FL = read.csv("ACSST5Y2021.S1901-IncomeFlorida_extraheadersremoved.csv", header = TRUE)
dim(income_FL) #2026 57
View(income_FL)

FL_zips = read.csv("Florida_ZipCodes_Population.csv", header = TRUE)
dim(FL_zips) #937 4
View(FL_zips)

FL_zipsarea = read.csv("Florida_zip_area.csv", header = TRUE)
dim(FL_zipsarea) #911 6
View(FL_zipsarea)


###########################
###### modify data ########
###########################

# limit katie's site data to FL sites and then simplify df
katie_sites_data = katie_sites_data %>% filter(katie_sites_data$State == "FL")
dim(katie_sites_data) #80 11
colnames(katie_sites_data)
katie_sites = katie_sites_data[, c(2,4,7,10)]
dim(katie_sites) #80 4
View(katie_sites)
write.csv(katie_sites, file = "katie_sites.csv", row.names= FALSE)

# tally facilities per zip code in katie_sites data
sites_tally = katie_sites %>%
  group_by(zip) %>%
  tally()
dim(sites_tally) #25 2
View(sites_tally)

#remove duplicate rows in income df
income_FL <- unique(income_FL)
dim(income_FL) #1013 57

# create new column in census file for just the five digit zip code, from their ZCTA5
census$Geographic.Area.Name = trimws(census$Geographic.Area.Name) #remove extra whitespaces
census$zip = substr(census$Geographic.Area.Name, 6, 11)
#write.csv(census, file = "census.csv", row.names= FALSE)

# create new column in income file for just five digit zip code
income_FL$Geographic.Area.Name = trimws(income_FL$Geographic.Area.Name) #remove extra whitespaces
income_FL$zip = substr(income_FL$Geographic.Area.Name, 6, 11)

# change a few columns to be numeric values and rename
census$PercentWhite = as.numeric(census$Percent..RACE..Total.population..One.race..White)
census$PercentMale = as.numeric(census$Percent..SEX.AND.AGE..Total.population..Male)
income_FL$medianincome = as.numeric(income_FL$Estimate..Households..Median.income..dollars.)

# create a new column in income file to divide median income by 1000
income_FL$medianK = as.integer(income_FL$medianincome/1000)
View(income_FL)

# simplify a few dataframes
colnames(census)
katie_census = census[, c(62,63,64)]
dim(katie_census) #33774 3
View(katie_census)

colnames(income_FL)
katie_income_FL = income_FL[, c(58,59,60)]
dim(katie_income_FL) #1013 3
View(katie_income_FL)

colnames(FL_zipsarea)
FL_zipsarea = FL_zipsarea[, c(1,5)]
FL_zipsarea$sqmiles = as.numeric(FL_zipsarea$sqmiles)
View(FL_zipsarea)


##############################
##### join dataframes ########
##############################

# join income and census data
census_income = join(katie_income_FL, katie_census, by = "zip",  type = "left")
dim(census_income) #1013 5
View(census_income)
write.csv(census_income, file = "census_income.csv", row.names= FALSE)

# for some weird reason, I need to reload this data to get the next step to work
#rm(list=ls()) #clear old dfs, etc.
census_income = read.csv("census_income.csv", header = TRUE)

# join to zips area data
census_zipsarea = join(FL_zipsarea, census_income, by = "zip", type = "full")
dim(census_zipsarea) #1014 6
View(census_zipsarea)

# join to zip code population data
census_FL = join(census_zipsarea, FL_zips, by = "zip", type = "left")
dim(census_FL) #1014 9
View(census_FL) 
write.csv(census_FL, file = "census_FL.csv", row.names= FALSE)

# join to the sites tally data (all three site types combined)
census_FL = read.csv("census_FL.csv", header = TRUE)
katie_FL = join(census_FL, sites_tally, by = "zip",  type = "left")
dim(katie_FL) #1014 10
View(katie_FL)

# change "n" variable to read "0" for type = NA
katie_FL = katie_FL %>% mutate(n = ifelse(is.na(n), "0", n))
katie_FL$n = as.numeric(katie_FL$n) 
dim(katie_FL) #1014 10






#####################################################
### putting the code below on hold for right now ####
#####################################################

# complete katie_FL df for site type per zip
katie_complete = katie_FL %>% complete(zip, type)
dim(katie_complete) #4101 12
View(katie_complete)

# tally phosphorus sites per zip code
phosphorus_tally = katie_complete %>%
  group_by(type, zip) %>%
  tally()
dim(phosphorus_tally) #4056 3
View(phosphorus_tally)

# change "n" variable to read "0" for type = NA
#phosphorus_tally = phosphorus_tally %>% mutate(type = ifelse(is.na(type), "none", type))
#phosphorus_tally$n[phosphorus_tally$type == "none"] <- 0 
#phosphorus_tally$n = as.numeric(phosphorus_tally$n)
#dim(phosphorus_tally) #4056 3
#View(phosphorus_tally)

# complete phosphorus df for site type per zip
phosphorus_complete = phosphorus_tally %>% complete(zip, type)
dim(phosphorus_complete) #122 4
View(phosphorus_complete)

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

# remove zip codes with an area of less than 4 sqmiles (because I don't trust those data points)
katie_FL_sqmile4 = katie_FL %>% filter(sqmiles >= 4)
dim(katie_FL_sqmile4) #815 10
View(katie_FL_sqmile4)
write.csv(katie_FL_sqmile4, file = "katie_FL_sqmile4.csv", row.names= FALSE)




#############################
######## analysis ###########
#############################

##################################################
##### not using the version below right now ######
##################################################

# # analyze data for patterns with plant numbers and race data
# colnames(katie_type)
# hist(katie_type$n)
# hist(katie_type$Percent..RACE..Total.population..One.race..White) # normal enough??
# racediff_type = glm(katie_type$n ~ katie_type$Percent..RACE..Total.population..One.race..White)
# summary(racediff_type) # "." moderately significant
# 
# # analyze data for patterns with plant numbers and median income
# colnames(katie_type)
# hist(katie_type$n)
# hist(katie_type$Estimate..Households..Median.income..dollars.) # normal enough??
# incomediff_type = glm(katie_type$n ~ katie_type$Estimate..Households..Median.income..dollars.)
# summary(incomediff_type) # moderately significant, but why are there two P-vals??
# 
# 
# # take out site type and tally all sites by zip code
# colnames(katie_FL)
# katie_notype = katie_FL[, c(1:128)]
# dim(katie_notype) #990 128
# katie_notype = katie_notype %>%
#   group_by(zip) %>%
#   tally()
# dim(katie_notype) #937 2
# View(katie_notype)
# 
# # add back in demographic data 
# katie_notype = join(katie_notype, census_FL, by = "zip",  type = "left")
# dim(katie_notype) #937 121
# View(katie_notype)
# 
# # analyze race patterns again without site type
# colnames(katie_notype)
# hist(katie_notype$n)
# hist(as.numeric(katie_notype$Percent..RACE..Total.population..One.race..White)) # normal enough??
# racediff_notype = glm(katie_notype$n ~ as.numeric(katie_notype$Percent..RACE..Total.population..One.race..White))
# summary(racediff_notype) # *** highly significant
# 
# # analyze income patterns again without site type
# colnames(katie_notype)
# hist(katie_notype$n)
# hist(as.numeric(katie_notype$Estimate..Households..Median.income..dollars.)) # normal enough??
# incomediff_notype = glm(katie_notype$n ~ as.numeric(katie_notype$Estimate..Households..Median.income..dollars.))
# summary(incomediff_notype) # *** highly significant

#############################################
#############################################

# mixed effects model
katie_mixed =  lmer(n ~ PercentWhite + medianK  + (1 | sqmiles), data = katie_FL_sqmile4)
summary(katie_mixed)
katie_mixed

# version with area as a fixed effect
katie_mixed_area =  glm(n ~ PercentWhite + medianK  + sqmiles, data = katie_FL_sqmile4)
summary(katie_mixed_area)
katie_mixed_area

# graph model to see effects
sjPlot:: tab_model(katie_mixed)
sjPlot::plot_model(katie_mixed)
#sjPlot::plot_model(katie_mixed, axis.labels=c("Urchin", "Depth", "Fish"),
                  # show.values=TRUE, show.p=TRUE, title="Effect of Herbivores on Coral Cover")

# repeat for model with area
sjPlot:: tab_model(katie_mixed_area)
sjPlot::plot_model(katie_mixed_area)

# put effect sizes into a df to graph against raw data
effects_katie_race <- effects::effect(term= "PercentWhite", mod= katie_mixed)
x_katie_race <- as.data.frame(effects_katie_race)

# plot effect of race
katie_plot_race <- ggplot() + 
  geom_point(data = katie_FL, aes(PercentWhite, n)) + 
  geom_point(data=x_katie_race, aes(x=PercentWhite, y=fit), color="blue") +
  geom_line(data=x_katie_race, aes(x=PercentWhite, y=fit), color="blue") +
  geom_ribbon(data= x_katie_race, aes(x=PercentWhite, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Percent White Residents", y="Number of Phosphorus Sites")
katie_plot_race

# repeat for income
effects_katie_income <- effects::effect(term= "medianK", mod= katie_mixed)
x_katie_income <- as.data.frame(effects_katie_mixed)

# plot effects of income
katie_plot_income <- ggplot() + 
  geom_point(data = katie_FL, aes(medianK, n)) + 
  geom_point(data=x_katie_income, aes(x=medianK, y=fit), color="blue") +
  geom_line(data=x_katie_income, aes(x=medianK, y=fit), color="blue") +
  geom_ribbon(data= x_katie_income, aes(x=medianK, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Median Income (in thousands)", y="Number of Phosphorus Sites")
katie_plot_income

# same for area model
effects_katie_area <- effects::effect(term= "sqmiles", mod= katie_mixed_area)
x_katie_area <- as.data.frame(effects_katie_area)

# plot effects of area
katie_plot_area <- ggplot() + 
  geom_point(data = katie_FL, aes(sqmiles, n)) + 
  geom_point(data=x_katie_area, aes(x=sqmiles, y=fit), color="blue") +
  geom_line(data=x_katie_area, aes(x=sqmiles, y=fit), color="blue") +
  geom_ribbon(data= x_katie_area, aes(x=sqmiles, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Zip code square mileage", y="Number of Phosphorus Sites")
katie_plot_area

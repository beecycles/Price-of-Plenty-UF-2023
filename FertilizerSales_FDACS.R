###################################################################################
###### Fertilizer Files Code for 2023 UF Advanced Environmental Journalism ########
######################### Joan Meiners 2023 #######################################


### This script is for analyzing fertilizer licensure and tonnage data ####
setwd("C:/Users/JMeiners/OneDrive - Gannett Company, Incorporated/Desktop/UF Adjunct Teaching/Fertilizer Data/Florida Tonnage and Licensure/")

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
# Step 1: Download from https://www.fdacs.gov/Agriculture-Industry/Fertilizer-Licensing-and-Tonnage-Reporting and convert from pdf to xlsx
# Step 2: Reformat spreadsheet column titles, remove extra header rows, save as csv

# load data
FLFert.21.22 = read.csv("FiscalYear2021-2022.csv", header = TRUE)
FLFert.20.21 = read.csv("Fertiilzer-Report-FY-2020-2021.csv", header = TRUE)
FLFert.19.20 = read.csv("Tonnage-Sales-per-Quarter-for-FY-2019-2020.csv", header = TRUE)
FLFert.18.19 = read.csv("Tonnage-Sales-per-Quarter-for-FY-2018-2019.csv", header = TRUE)
FLFert.17.18 = read.csv("Tonnage-by-Licensee-2017-2018.csv", header = TRUE)

# remove extra white spaces in licensee field
FLFert.21.22$Licensee = trimws(FLFert.21.22$Licensee)
FLFert.20.21$Licensee = trimws(FLFert.20.21$Licensee)
FLFert.19.20$Licensee = trimws(FLFert.19.20$Licensee)
FLFert.18.19$Licensee = trimws(FLFert.18.19$Licensee)
FLFert.17.18$Licensee = trimws(FLFert.17.18$Licensee)

# create a new ID column to join on
FLFert.21.22$ID = paste(FLFert.21.22$Licensee,FLFert.21.22$ReportingPlant)
FLFert.20.21$ID = paste(FLFert.20.21$Licensee,FLFert.20.21$ReportingPlant)
FLFert.19.20$ID = paste(FLFert.19.20$Licensee,FLFert.19.20$ReportingPlant)
FLFert.18.19$ID = paste(FLFert.18.19$Licensee,FLFert.18.19$ReportingPlant)
FLFert.17.18$ID = paste(FLFert.17.18$Licensee,FLFert.17.18$ReportingPlant)

# view data
View(FLFert.21.22)
View(FLFert.20.21)
View(FLFert.19.20)
View(FLFert.18.19)
View(FLFert.17.18)

# check dimensions
dim(FLFert.21.22) # 456 8
dim(FLFert.20.21) # 448 8
dim(FLFert.19.20) # 434 8
dim(FLFert.18.19) # 434 8
dim(FLFert.17.18) # 409 8

# join datasets by Licensee number
FLFert_all = join(FLFert.21.22, FLFert.20.21, by = "ID")
FLFert_all = join(FLFert_all, FLFert.19.20, by = "ID")
FLFert_all = join(FLFert_all, FLFert.18.19, by = "ID")
FLFert_all = join(FLFert_all, FLFert.17.18, by = "ID")
names(FLFert_all) <- sub("^X", "", names(FLFert_all))
View(FLFert_all)
dim(FLFert_all) # 456 36
colnames(FLFert_all)

# Note: the 456 length on the combined file seems suspiciously short -- check that out

# next try to graph use over time (year) for each company

# create a new df with only the licensee and year total columns
FLFert_all2 = FLFert_all[, c(8, 1, 2, 36, 29, 22, 15, 7)]
View(FLFert_all2)
dim(FLFert_all2) #456 8
colnames(FLFert_all2)

# create a new df that calculates the difference between use in 2017 and 2022
FLFert_diff = FLFert_all2
sapply(FLFert_diff, class)
colnames(FLFert_diff)
names(FLFert_diff)[4] ="Year2017"
names(FLFert_diff)[5] ="Year2018"
names(FLFert_diff)[6] ="Year2019"
names(FLFert_diff)[7] ="Year2020"
names(FLFert_diff)[8] ="Year2021"
FLFert_diff = transform(FLFert_diff, Year2017 = as.numeric(Year2017), 
                        Year2018 = as.numeric(Year2018),
                        Year2019 = as.numeric(Year2019),
                        Year2020 = as.numeric(Year2020),
                        Year2021 = as.numeric(Year2021))
FLFert_diff$Difference = FLFert_diff$Year2021 - FLFert_diff$Year2017
View(FLFert_diff)
write.csv(FLFert_diff, file = "FLFert_diff.csv", row.names= FALSE)

# melt data to collapse columns for graphing
FLFert_melt = melt(FLFert_all2, na.rm = FALSE, id = c("ID", "Licensee", "ReportingPlant"))
colnames(FLFert_melt)
dim(FLFert_melt) #2280 5
names(FLFert_melt)[4] ="Year"
names(FLFert_melt)[5] ="Tonnage"
colnames(FLFert_melt)
View(FLFert_melt)

# graph data to look for major trends and plants to highlight
#tiff(filename = "FLFertPlantUse.17.22.tiff", units = "in", compression = "lzw", res = 300, width = 12, height = 6)

# this code copied and pasted below, still need to customize

ggplot(aes(x = Year, y= Tonnage, group = ReportingPlant), data = FLFert_melt) +
  geom_point(color = "grey") +
  xlab("Growing Year (Start)") + ylab("Fertilizer Tonnage Used") +
  geom_smooth(method = "lm", se=FALSE, color="darkgreen")
# dev.off() # run this line after figure code to finish saving out figure to file
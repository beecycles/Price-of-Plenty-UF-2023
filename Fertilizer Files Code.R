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
View(FLFert_all)
dim(FLFert_all) # 456 36

# the 456 length on the combined file seems suspiciously short

# next try to graph use over time for each company

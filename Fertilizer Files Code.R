###################################################################################
###### Fertilizer Files Code for 2023 UF Advanced Environmental Journalism ########
######################### Joan Meiners 2023 #######################################


### This script is for analyzing fertilizer licensure and tonnage data ####
setwd("C:/Users/JMeiners/OneDrive - Gannett Company, Incorporated/Desktop/UF Adjunct Teaching/Fertilizer Data/Florida Tonnage and Licensure/")

#load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# prep data
# Step 1: Download from https://www.fdacs.gov/Agriculture-Industry/Fertilizer-Licensing-and-Tonnage-Reporting and convert from pdf to xlsx
# Step 2: Reformat spreadsheet column titles, remove extra header rows, save as csv

# load data
FloridaFert.21.22 = read.csv("FiscalYear2021-2022.csv", header = TRUE)
FloridaFert.20.21 = read.csv("Fertiilzer-Report-FY-2020-2021.csv", header = TRUE)
FloridaFert.19.20 = read.csv("Tonnage-Sales-per-Quarter-for-FY-2019-2020.csv", header = TRUE)
FloridaFert.18.19 = read.csv("Tonnage-Sales-per-Quarter-for-FY-2018-2019.csv", header = TRUE)
FloridaFert.17.18 = read.csv("Tonnage-by-Licensee-2017-2018.csv", header = TRUE)

# join data


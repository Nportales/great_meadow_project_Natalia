## Script for merging new veg data with old ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(stringr)
library(sf)
library(gt)
library(purrr)
library(ggplot2)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

species_list <- read.csv("data/processed_data/species_list_arcgis.csv") %>%
  as_tibble()

vmmi <- read.csv("data/processed_data/FOA_NETN_VMMI_2011_2024.csv")




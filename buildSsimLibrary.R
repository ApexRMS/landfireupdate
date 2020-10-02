### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### September 2020
### Extract tables from db and script library

##  Load packages
library(tidyverse)
library(rsyncrosim)
library(RODBC)

## Load database
landFireDB <- 
  "db/NW_GeoArea_VegTransitions_Update_for_Remap_KCH_complete_2020_09_22.accdb"
db <- 
  odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", 
                           landFireDB))

## Load crosswalk 
distCrosswalk <- 
  read_csv("data/raw/non_spatial/LimUpdate2021_VDISTxFDIST_v02_20200925.csv")

## Collect tables of interest
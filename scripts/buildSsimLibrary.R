### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### September 2020
### Extract tables from db and script library

##  Load packages
library(tidyverse)
library(rsyncrosim)
library(raster)
library(RODBC)

## Load database
landFireDB <- 
  "db/NW_GeoArea_VegTransitions_Update_for_Remap_KCH_complete_2020_10_06.accdb"
db <- 
  odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", 
                           landFireDB))

## Load crosswalk 
distCrosswalk <- 
  read_csv("data/raw/non_spatial/LimUpdate2021_VDISTxFDIST_v02_20200925.csv") %>% 
  # Remove unimportant categories
  dplyr::select(-c(d_severity, d_severity_1, 
                   `Tree Rules`,  `Shrub Rules`, `Herb Rules`)) %>% 
  # Rename for easier handling
  rename(d_type_f = d_type_1, d_time_f = d_time_1)

### Building Library
# Create library with a project ("Definitions") and a scenario ("Test")
libraryName <- "LandFire_Test_SmallExtent.ssim"
mylibrary <- ssimLibrary(paste0("library/", libraryName), overwrite = TRUE)
myproject <- project(mylibrary, "Definitions", overwrite = TRUE)
myscenario <- scenario(myproject, "Test")

## PRE PROCESSING

# Transition table
# Should be Unique for VDIST, PrimaryStratum, EvT, SourceStateClass

transTbl <- sqlFetch(db, "vegtransf_rv02i_d") %>% 
  # Turn all factors into strings
  mutate_if(is.factor, as.character) %>%
  # Select variables of importance then rename them
  dplyr::select(MZ, VDIST, EVT7B, EVT7B_Name,
                EVCB, EVHB, EVCR, EVHR) %>% 
  rename(StratumIDSource = EVT7B_Name) %>% 
  # Change the naming convention of MapZones e.g. from "1" to "MX01"
  mutate(SecondaryStratumID = 
           paste0("MZ", str_pad(MZ, 2, "left", "0"))) %>% 
  # Take unique values
  unique()

# EVC and EVH

EVClookup <- sqlFetch(db, "EVC_LUT") %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(CLASSNAMES = ifelse(is.na(.$CLASSNAMES), 
                             paste0(.$EVT_LIFEFORM, "_", .$VALUE), 
                             .$CLASSNAMES)) %>% 
  dplyr::select(VALUE, CLASSNAMES) %>% 
  rename(EVC_ID = VALUE, StateLabelXID = CLASSNAMES)

EVHlookup <- sqlFetch(db, "EVH_LUT") %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(CLASSNAMES = ifelse(is.na(.$CLASSNAMES), 
                             paste0(as.character(.$LIFEFORM), "_", .$VALUE), 
                             as.character(.$CLASSNAMES))) %>% 
  dplyr::select(VALUE, CLASSNAMES) %>% 
  rename(EVH_ID = VALUE, StateLabelYID = CLASSNAMES)

# JOIN Transitions to EVC/EVH
transTblWithNames <- transTbl %>% 
  
  left_join(EVClookup, by = c("EVCB" = "EVC_ID")) %>% 
  rename(EVCB_Name = StateLabelXID) %>% 
  left_join(EVClookup, by = c("EVCR" = "EVC_ID")) %>% 
  rename(EVCR_Name = StateLabelXID) %>%
  
  left_join(EVHlookup, by = c("EVHB" = "EVH_ID")) %>% 
  rename(EVHB_Name = StateLabelYID) %>% 
  left_join(EVHlookup, by = c("EVHR" = "EVH_ID")) %>% 
  rename(EVHR_Name = StateLabelYID) %>% 
  
  mutate(StateClassIDSource = paste0(EVCB_Name, " : ", EVHB_Name), 
         StateClassIDDest = paste0(EVCR_Name, " : ", EVHR_Name)) %>%
  
  mutate(Probability = 1)

# Primary stratums 
allEVT <- unique(transTbl$StratumIDSource)

# Secondary stratum
allZones <- unique(transTbl$SecondaryStratumID)

### DATASHEETS

# Terminology
term <- data.frame(
  AmountLabel = "Area", 
  AmountUnits = "Hectares", 
  StateLabelX = "EVC", 
  StateLabelY = "EVH", 
  PrimaryStratumLabel = "EVT",
  SecondaryStratumLabel = "MapZones",
  # TertiaryStratumLabel = "ESP", 
  TimestepUnits = "Timestep"
)
saveDatasheet(ssimObject = myproject, data = term, 
              name = "stsim_Terminology")

## STRATUMS

primary <- data.frame(ID = transTblWithNames$EVT7B, 
                      Name = transTblWithNames$StratumIDSource) %>% 
  unique()
saveDatasheet(myproject, primary, "Stratum")

secondary <- data.frame(ID = transTblWithNames$MZ, 
                        Name = transTblWithNames$SecondaryStratumID) %>% 
  unique()
saveDatasheet(myproject, secondary, "SecondaryStratum")

## STATE CLASSES

state_x <- data.frame(
  Name = EVClookup$StateLabelXID,
  Description = EVClookup$StateLabelXID
) %>% 
  unique()
saveDatasheet(myproject, state_x, "stsim_StateLabelX")

state_y <- data.frame(
  Name = EVHlookup$StateLabelYID,
  Description = EVHlookup$StateLabelYID
) %>% 
  unique()
saveDatasheet(myproject, state_y, "stsim_StateLabelY")

# Make IDS
IDs <- c((transTblWithNames$EVCB*1000 + transTblWithNames$EVHB), 
         (transTblWithNames$EVCR*1000 + transTblWithNames$EVHR))

stateClasses <- data.frame(
  ID = IDs,
  Name = c(transTblWithNames$StateClassIDSource, transTblWithNames$StateClassIDDest), 
  StateLabelXID = c(transTblWithNames$EVCB_Name, transTblWithNames$EVCR_Name), 
  StateLabelYID = c(transTblWithNames$EVHB_Name, transTblWithNames$EVHR_Name)) %>% 
  unique()
saveDatasheet(myproject, stateClasses, "stsim_StateClass")

## TRANSITION TYPES
vdistLookup <-  sqlFetch(db, "VDIST") %>% 
  dplyr::select(value, d_type, d_severity, d_time)

transitionTypes <- vdistLookup %>% 
  rename(ID = value) %>% 
  mutate(d_type = str_replace_all(string = .$d_type, 
                                  pattern = " ", 
                                  replacement = "_"), 
         d_time = str_replace_all(string = .$d_time, 
                                  pattern = " ", 
                                  replacement = "_")) %>% 
  mutate(Name = paste(d_type, d_severity, d_time, sep = "_")) %>% 
  dplyr::select(ID, Name) %>% 
  filter(ID != 0) %>% 
  unique()

saveDatasheet(myproject, transitionTypes, "stsim_TransitionType")

# SCENARIO TEST -----------------------------------------------------------

## TRANSITIONS
# Determine what can be applied
fDISTCropped <- raster("data/clean/cropped/nw_fDIST_clean_small.tif")
allValues <- unique(fDISTCropped)
transitionTypesCropped <- transitionTypes %>% 
  filter(ID %in% allValues)

# Deterministic
# TODO specify the stratum here ??

# Create the grid
# This wrangles the locations of state classes in the UI
# TODO come back here for the colors later, the df is already being splitted once
LocationDf <- stateClasses %>% 
  mutate(letter = ifelse(str_detect(StateLabelXID, "Tree"), "A", 
                         ifelse(str_detect(StateLabelXID, "Shrub"), "B",
                                ifelse(str_detect(StateLabelXID, "Herb"), "C", "D")))) %>% 
  arrange(letter, StateLabelYID) %>% 
  split(f = .$letter) %>% 
  lapply(., function(x){x %>% mutate(row = 1:nrow(x))}) %>% 
  bind_rows() %>% 
  mutate(Location = paste0(letter, row)) %>% 
  dplyr::select(Name, Location)

# Join the locations to the state classes df
stateClassesjoined <- stateClasses %>% 
  left_join(LocationDf, by = "Name")

# Extract the columns for the datasheet
deterministicTransitions <- stateClassesjoined %>% 
  dplyr::select(Name, Location) %>% 
  rename(StateClassIDSource = Name) %>% 
  unique()

saveDatasheet(myscenario, deterministicTransitions, 
              "stsim_DeterministicTransition") 

# Probabilistic
transTblWithNamesDatasheet <- transTblWithNames %>%
  left_join(transitionTypes, by = c("VDIST" = "ID")) %>% 
  rename(TransitionTypeID = Name) %>% 
  dplyr::select(StratumIDSource, SecondaryStratumID,
                StateClassIDSource, StateClassIDDest, 
                TransitionTypeID, Probability)

transTblWithNamesDatasheet <- transTblWithNamesDatasheet %>% 
  filter(TransitionTypeID %in% transitionTypesCropped$Name)

saveDatasheet(myscenario, transTblWithNamesDatasheet, "stsim_Transition")

## TRANSITION MULTIPLIERS

multiplierGroupNames <- paste0(transitionTypesCropped$Name, " [Type]")
multiplierFileNames <- paste0(getwd(), 
                              "/data/clean/cropped/FDIST/FDIST_value_", 
                              transitionTypesCropped$ID, ".tif")

spatialMultiplier <- data.frame(
  TransitionGroupID = multiplierGroupNames,
  MultiplierFileName = multiplierFileNames
)

saveDatasheet(myscenario, spatialMultiplier, 
              "stsim_TransitionSpatialMultiplier")

## INITIONAL CONDITIONS

initialConditionsSpatial <- data.frame(
  StateClassFileName = paste0(getwd(), "/data/clean/cropped/nw_EVC_EVH_StateClasses.tif"),
  StratumFileName = paste0(getwd(), "/data/clean/cropped/nw_Mapzones_small.tif"), 
  SecondaryStratumFileName = paste0(getwd(), "/data/clean/cropped/nw_EVT_clean_small.tif") 
)

saveDatasheet(myscenario, initialConditionsSpatial, 
              "stsim_InitialConditionsSpatial")

## RUN CONTROL

runControl <- data.frame(
  MinimumIteration = 1,
  MaximumIteration = 1, 
  MinimumTimestep = 2017,
  MaximumTimestep = 2018, 
  IsSpatial = TRUE
)

saveDatasheet(myscenario, runControl, 
              "stsim_RunControl")

## End the db connection
odbcClose(db)

# -------------------------------------------------------------------------
# QA Code

## Old tests for QA
# the_list <- transTbl %>%
#   group_split(VDIST, SecondaryStratumID, StratumIDSource, EVCB, EVHB)
# the_vec <- sapply(the_list, nrow)
# larger <- which(the_vec > 1)
# the_list_of_larger <- the_list[larger]
# the_list_of_larger[1]

# raw <- sqlFetch(db, "vegtransf_rv02i_d")
# test <- raw[which(duplicated(transTbl)),]
# View(test)

# raw <- sqlFetch(db, "vegtransf_rv02i_d")
# trans_vdist <- raw$VDIST %>% unique()
# key_Vdist <- distCrosswalk$VDIST %>% unique()
# trans_vdist[which(!(trans_vdist %in% key_Vdist))]

# raw_filtered <- raw %>%
#   filter(MZ %in% c(10,19)) %>%
#   filter(VDIST %in% allValues) %>%
#   filter(EVT7B %in% unique(raster("data/clean/cropped/nw_EVT_clean_small.tif"))) %>% 

## Checks
# the_stack <- stack(raster(""))

# -------------------------------------------------------------------------
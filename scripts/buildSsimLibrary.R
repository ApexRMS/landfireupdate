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
# (DONE) duplicates in this table when "3" is removed
# TODO Check only one rule per source cause dist cannot have multiple destination

transTbl <- sqlFetch(db, "vegtransf_rv02i_d") %>% 
  # Turn all factors into strings
  mutate_if(is.factor, as.character) %>%
  # Select variables of importance then rename them
  dplyr::select(MZ, VDIST, EVT7B, EVT7B_Name,
                EVCB, EVHB, EVCR, EVHR) %>% 
  rename(StratumIDSource = MZ, 
         SecondaryStratumID = EVT7B_Name) %>% 
  # Take unique values
  unique()

## Old test for QA
# raw <- sqlFetch(db, "vegtransf_rv02i_d")
# test <- raw[which(duplicated(transTbl)),]
# View(test)

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
# raw <- sqlFetch(db, "vegtransf_rv02i_d")
# trans_vdist <- raw$VDIST %>% unique()
# key_Vdist <- distCrosswalk$VDIST %>% unique()
# trans_vdist[which(!(trans_vdist %in% key_Vdist))]
# TODO non matching VDIST => ex 733 leads to NA transitiontypes => 
#                       need to give all the values that are not showing up
# => fixed cause not joining anymore

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

# Primary stratum
allZones <- unique(transTbl$MZ)

# Secondary stratums 
allEVT <- unique(c(transTbl$EVT7B_Name, transTbl$EVT7R_Name))

### DATASHEETS

# Terminology
term <- data.frame(
  AmountLabel = "Area", 
  AmountUnits = "Hectares", 
  StateLabelX = "EVC", 
  StateLabelY = "EVH", 
  PrimaryStratumLabel = "MapZones",
  SecondaryStratumLabel = "EVT",
  # TertiaryStratumLabel = "ESP", 
  TimestepUnits = "Timestep"
)
saveDatasheet(ssimObject = myproject, data = term, 
              name = "stsim_Terminology")

## STRATUMS

primary <- data.frame(ID = transTblWithNames$StratumIDSource, 
                      Name = transTblWithNames$StratumIDSource) %>% 
  unique()
saveDatasheet(myproject, primary, "Stratum")

secondary <- data.frame(ID = transTblWithNames$EVT7B, 
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
IDs = c((transTblWithNames$EVCB*1000 + transTblWithNames$EVHB), 
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
                                  replacement = "_")) %>% 
  mutate(Name = paste(d_type, d_severity, d_time, sep = "_")) %>% 
  dplyr::select(ID, Name) %>% 
  unique()

saveDatasheet(myproject, transitionTypes, "stsim_TransitionType")


# SCENARIO TEST -----------------------------------------------------------

## TRANSITIONS

# Deterministic
deterministicTransitions <- data.frame(
  StateClassIDSource = unique(stateClasses$Name),
  Location = paste0("A", c(1:length(unique(stateClasses$Name))))
)

saveDatasheet(myscenario, deterministicTransitions, 
              "stsim_DeterministicTransition") 

# Probabilistic
transTblWithNamesDatasheet <- transTblWithNames %>%
  left_join(transitionTypes, by = c("VDIST" = "ID")) %>% 
  rename(TransitionTypeID = Name) %>% 
  dplyr::select(StratumIDSource, SecondaryStratumID,
                StateClassIDSource, StateClassIDDest, 
                TransitionTypeID, Probability)

saveDatasheet(myscenario, transTblWithNamesDatasheet, "stsim_Transition")

## TRANSITION MULTIPLIERS



## INITIONAL CONDITIONS etc..

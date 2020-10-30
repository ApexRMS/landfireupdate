### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### September 2020
### Extract tables from db and script library

# Load packages -----------------------------------------------------------

# The package rgdal is also required. Rstudio might detect this requirement. If
# it doesn't, simply run: install.packages("rgdal")

library(tidyverse)
library(rsyncrosim)
library(rgdal)
library(raster)
library(RODBC)
library(readxl)

# Access database ---------------------------------------------------------

# Database path

landFireDB <- 
  "db/NW_GeoArea_VegTransitions_Update_for_Remap_KCH_complete_2020_10_21.accdb"

# Connect to database 

db <- 
  odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", 
                           landFireDB))

## Load crosswalk 

distCrosswalk <- 
  read_xlsx("data/raw/non_spatial/LimUpdate2021_VDISTxFDIST_v03_20201009.xlsx") %>% 
  # Remove unimportant categories
  dplyr::select(-c(d_severity...3, d_severity...10, d_type...9, d_time...11,
                   `Tree Rules`,  `Shrub Rules`, `Herb Rules`)) %>% 
  # Rename for easier handling
  rename(d_type_f = d_type...2, d_time_f = d_time...4)

# Build the SyncroSim Library ---------------------------------------------

# Create library with a project ("Definitions") and a scenario ("Test")

# libraryName <- "LandFire_Test_SmallExtent.ssim"
libraryName <- "LandFire_Test_SmallExtent_2.ssim"
mylibrary <- ssimLibrary(paste0("library/", libraryName), overwrite = TRUE)
myproject <- rsyncrosim::project(mylibrary, "Definitions", overwrite = TRUE)
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
  # Change the naming convention of MapZones e.g. from "1" to "MZ01"
  mutate(SecondaryStratumID = 
           paste0("MZ", str_pad(MZ, 2, "left", "0"))) %>% 
  # Take unique values
  unique()

# EVC and EVH

EVClookup <- sqlFetch(db, "EVC_LUT") %>% 
  # Turn factors into characters
  mutate_if(is.factor, as.character) %>% 
  # Make unique names for when class names are repeated
  mutate(CLASSNAMES = ifelse(is.na(.$CLASSNAMES), 
                             paste0(.$EVT_LIFEFORM, "_", .$VALUE), 
                             .$CLASSNAMES)) %>% 
  # Select relevant columns and rename with stsim relevant column names
  dplyr::select(VALUE, CLASSNAMES) %>% 
  rename(EVC_ID = VALUE, StateLabelXID = CLASSNAMES)

# Similarly, for EVH:

EVHlookup <- sqlFetch(db, "EVH_LUT") %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(CLASSNAMES = ifelse(is.na(.$CLASSNAMES), 
                             paste0(as.character(.$LIFEFORM), "_", .$VALUE), 
                             as.character(.$CLASSNAMES))) %>% 
  dplyr::select(VALUE, CLASSNAMES) %>% 
  rename(EVH_ID = VALUE, StateLabelYID = CLASSNAMES)

# JOIN Transitions to EVC/EVH

transTblWithNames <- transTbl %>% 
  
  # First jin EVC B and R, renaming appropriately
  left_join(EVClookup, by = c("EVCB" = "EVC_ID")) %>% 
  rename(EVCB_Name = StateLabelXID) %>% 
  left_join(EVClookup, by = c("EVCR" = "EVC_ID")) %>% 
  rename(EVCR_Name = StateLabelXID) %>%
  
  # Similarly for EVH
  left_join(EVHlookup, by = c("EVHB" = "EVH_ID")) %>% 
  rename(EVHB_Name = StateLabelYID) %>% 
  left_join(EVHlookup, by = c("EVHR" = "EVH_ID")) %>% 
  rename(EVHR_Name = StateLabelYID) %>% 
  
  # Create the State Class names from EVC : EVH combinations
  mutate(StateClassIDSource = paste0(EVCB_Name, " : ", EVHB_Name), 
         StateClassIDDest = paste0(EVCR_Name, " : ", EVHR_Name)) %>%
  
  # Add the propability column with all set to 1
  mutate(Probability = 1)

# Primary stratums (all unique EVT values)
allEVT <- unique(transTbl$StratumIDSource)

# Secondary stratum (all unique MZ values)
allZones <- unique(transTbl$SecondaryStratumID)

# Compose and load STSIM datasheets ---------------------------------------

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

## Stratums
# EVT is the primary stratum, MZ is the secondary stratum
# No tertiary stratum
# Important to take the unique values every time

primary <- data.frame(ID = transTblWithNames$EVT7B, 
                      Name = transTblWithNames$StratumIDSource) %>% unique()

# Extract colors from the evt200 sheet
# TODO: An issue here where some IDs do not have colors and some colors do not
# have matching IDs. 

primaryWithColors <- sqlFetch(db, "nw_evt200") %>% 
  # Select relevant columns
  dplyr::select(VALUE, R, G, B) %>% 
  # Take unique and rename for later joining
  unique() %>% rename(ID = VALUE) %>% 
  # Create the color using the SyncroSim pattern of T, R, G, B
  mutate(Color = paste("255", R, G, B, sep = ",")) %>% 
  # Join and select relevant columns
  right_join(primary, by = "ID") %>% 
  dplyr::select(ID, Color, Name)

# Save the datasheet
saveDatasheet(myproject, primaryWithColors, "Stratum")

# For secondary, no colors
secondary <- data.frame(ID = transTblWithNames$MZ, 
                        Name = transTblWithNames$SecondaryStratumID) %>% unique()
saveDatasheet(myproject, secondary, "SecondaryStratum")

## State Ckasses

state_x <- data.frame(
  Name = EVClookup$StateLabelXID,
  Description = EVClookup$StateLabelXID) %>% 
  unique()

saveDatasheet(myproject, state_x, "stsim_StateLabelX")

state_y <- data.frame(
  Name = EVHlookup$StateLabelYID,
  Description = EVHlookup$StateLabelYID) %>% 
  unique()

saveDatasheet(myproject, state_y, "stsim_StateLabelY")

# Make unique IDS
# We do ECV * 1000 + EVH

IDs <- c((transTblWithNames$EVCB*1000 + transTblWithNames$EVHB), 
         (transTblWithNames$EVCR*1000 + transTblWithNames$EVHR))

# Build the datasheet
stateClasses <- data.frame(
  ID = IDs,
  Name = c(transTblWithNames$StateClassIDSource, transTblWithNames$StateClassIDDest), 
  StateLabelXID = c(transTblWithNames$EVCB_Name, transTblWithNames$EVCR_Name), 
  StateLabelYID = c(transTblWithNames$EVHB_Name, transTblWithNames$EVHR_Name)) %>% 
  unique()

saveDatasheet(myproject, stateClasses, "stsim_StateClass")

## Transition Types
# We gather disturbance types from the VDIST table

vdistLookup <-  sqlFetch(db, "VDIST") %>%
  # Turn all factors into charactors
  mutate_if(is.factor, as.character) %>%
  # Select only what we need, then rename
  dplyr::select(value, d_type, d_severity, d_time, R, G, B) %>% 
  rename(ID = value,  TransitionGroupID = d_type) %>%
  # Filter out the NO Disturbance category
  filter(ID != 0) %>%
  # Create unique transition/disturbance name, and format color
  # The format of the name is : Group, Severity, Frequency
  mutate(Name = paste(TransitionGroupID, d_severity, d_time, sep = ", ")) %>% 
  mutate(Color = paste("255", R, G, B, sep = ","))

# Select what we want for the datasheet and save it
transitionTypes <- vdistLookup %>% 
  dplyr::select(ID, Name, Color) %>% 
  unique()

saveDatasheet(myproject, transitionTypes, "stsim_TransitionType")

## Transition Groups
# For groups, we append the disturbance class to the existing datasheet

TransitionGroups <- datasheet(myproject, "stsim_TransitionGroup") %>%
  mutate_if(is.factor, as.character) %>% 
  bind_rows(vdistLookup %>% 
              dplyr::select(Name = TransitionGroupID) %>% 
              unique())

saveDatasheet(myproject, TransitionGroups, "stsim_TransitionGroup")

## Transition Types by groups

TypesByGroup <- vdistLookup %>% 
  dplyr::select(Name, TransitionGroupID) %>% 
  unique() %>% 
  rename(TransitionTypeID = Name)

saveDatasheet(myproject, TypesByGroup, "stsim_TransitionTypeGroup")

## Transition simulation groups
# The same than groups, used for vizualization

SimulationGroups <- data.frame(
  TransitionGroupID = unique(vdistLookup$TransitionGroupID))

saveDatasheet(myproject, SimulationGroups, "stsim_TransitionSimulationGroup")

# Create Test Scenario ----------------------------------------------------

## Transitions
# Because we are running under a smaller extent we first need to know what
# disturbance type exist in this small extent

# Load raster
# fDISTCropped <- raster("data/clean/cropped/nw_fDIST_clean_MZ19.tif")
fDISTCropped <- raster("data/clean/cropped/nw_fDIST_clean_MZ19_cropped.tif")
# EVTCropped <- raster("data/clean/cropped/nw_EVT_clean_MZ19.tif")
EVTCropped <- raster("data/clean/cropped/nw_EVT_clean_MZ19_cropped.tif")

# Get all avlues
allValues <- unique(fDISTCropped)
allPrimaries <- unique(EVTCropped)

# Filter for the types that are there
transitionTypesCropped <- transitionTypes %>% 
  filter(ID %in% allValues)
primaryFiltered <- primary %>% 
  filter(ID %in% allPrimaries)

# Deterministic

# This wrangles the locations of state classes in the UI and stores thus 
# information in a dataframe for joining later

LocationDf <- stateClasses %>% 
  # Create unique positions for them
  mutate(letter = ifelse(str_detect(StateLabelXID, "Tree"), "A", 
                         ifelse(str_detect(StateLabelXID, "Shrub"), "B",
                                ifelse(str_detect(StateLabelXID, "Herb"), "C", "D")))) %>% 
  # Arrange, split and lapply across all rows
  arrange(letter, StateLabelYID) %>% 
  split(f = .$letter) %>% 
  lapply(., function(x){x %>% mutate(row = 1:nrow(x))}) %>% 
  bind_rows() %>% 
  mutate(Location = paste0(letter, row)) %>% 
  dplyr::select(Name, Location)

# Join the locations to the state classes df
stateClassesjoined <- stateClasses %>% 
  # Make sure to expand the grid to all combinations of MZ present (only 1 here)
  expand_grid(StratumIDSource = primaryFiltered$Name) %>% 
  # Join with the location info
  left_join(LocationDf, by = "Name")

# Extract/rename the columns to compose the datasheet and save it
deterministicTransitions <- stateClassesjoined %>% 
  dplyr::select(Name, Location, StratumIDSource) %>% 
  rename(StateClassIDSource = Name) %>% 
  unique() %>% 
  as.data.frame()

saveDatasheet(myscenario, deterministicTransitions, 
              "stsim_DeterministicTransition") 

# Probabilistic
transTblWithNamesDatasheet <- transTblWithNames %>%
  # Join the IDS
  left_join(transitionTypes, by = c("VDIST" = "ID")) %>% 
  # Rename and select what we need 
  rename(TransitionTypeID = Name) %>% 
  dplyr::select(StratumIDSource, SecondaryStratumID,
                StateClassIDSource, StateClassIDDest, 
                TransitionTypeID, Probability)

# Filter for values present in the smaller extent
transTblWithNamesDatasheet <- transTblWithNamesDatasheet %>% 
  filter(TransitionTypeID %in% transitionTypesCropped$Name) %>% 
  filter(StratumIDSource %in% primaryFiltered$Name)

saveDatasheet(myscenario, transTblWithNamesDatasheet, "stsim_Transition")

## Transition multipliers

# Collect the names and cretae path files
multiplierGroupNames <- paste0(transitionTypesCropped$Name, " [Type]")
# multiplierFileNames <- paste0(getwd(), 
#                               "/data/clean/cropped/FDIST/MZ19/FDIST_value_", 
#                               transitionTypesCropped$ID, ".tif")
multiplierFileNames <- paste0(getwd(), 
                              "/data/clean/cropped/FDIST/MZ19/FDIST_value_", 
                              transitionTypesCropped$ID, "_cropped.tif")

# Compose and save the data frame
spatialMultiplier <- data.frame(
  TransitionGroupID = multiplierGroupNames,
  MultiplierFileName = multiplierFileNames)

saveDatasheet(myscenario, spatialMultiplier, 
              "stsim_TransitionSpatialMultiplier")

## Initial conditions

# initialConditionsSpatial <- data.frame(
#   StateClassFileName = paste0(getwd(), "/data/clean/cropped/nw_EVC_EVH_StateClasses_MZ19.tif"),
#   StratumFileName = paste0(getwd(), "/data/clean/cropped/nw_EVT_clean_MZ19.tif"), 
#   SecondaryStratumFileName = paste0(getwd(), "/data/clean/cropped/nw_Mapzones_MZ19.tif"))
initialConditionsSpatial <- data.frame(
  StateClassFileName = paste0(getwd(), "/data/clean/cropped/nw_EVC_EVH_StateClasses_MZ19_cropped.tif"),
  StratumFileName = paste0(getwd(), "/data/clean/cropped/nw_EVT_clean_MZ19_cropped.tif"), 
  SecondaryStratumFileName = paste0(getwd(), "/data/clean/cropped/nw_Mapzones_MZ19_cropped.tif"))

saveDatasheet(myscenario, initialConditionsSpatial, 
              "stsim_InitialConditionsSpatial")

## Run Control

runControl <- data.frame(
  MinimumIteration = 1,
  MaximumIteration = 1, 
  MinimumTimestep = 2017,
  MaximumTimestep = 2018, 
  IsSpatial = TRUE
)

saveDatasheet(myscenario, runControl, "stsim_RunControl")

## Output Options

outputOptionsSummary <- 
  data.frame(SummaryOutputSC = TRUE, SummaryOutputSCTimesteps = 1, 
             SummaryOutputTR = TRUE, SummaryOutputTRTimesteps = 1)
outputOptionsSpatial <- 
  data.frame(RasterOutputSC = TRUE, RasterOutputSCTimesteps = 1,
             RasterOutputST = TRUE, RasterOutputSTTimesteps = 1,
             RasterOutputTR = TRUE, RasterOutputTRTimesteps = 1)

saveDatasheet(myscenario, outputOptionsSummary, "stsim_OutputOptions")
saveDatasheet(myscenario, outputOptionsSpatial, "stsim_OutputOptionsSpatial")

## Spatial multiprocessing 

spatial_multi <- datasheet(myscenario, "corestime_Multiprocessing")
# spatial_multi <- add_row(spatial_multi, 
#         MaskFileName = file.path(getwd(), 
#                                  "data/clean/cropped/nw_Mapzones_MZ19.tif"))
spatial_multi <- add_row(spatial_multi, 
        MaskFileName = file.path(getwd(), 
                                 "data/clean/cropped/Tiling_MZ19_cropped.tif"))

saveDatasheet(myscenario, spatial_multi, "corestime_Multiprocessing")

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
#   filter(MZ %in% c(19)) %>%
#   filter(VDIST %in% allValues) %>%
#   filter(EVT7B %in% unique(raster("data/clean/cropped/nw_EVT_clean_small.tif")))

# -------------------------------------------------------------------------
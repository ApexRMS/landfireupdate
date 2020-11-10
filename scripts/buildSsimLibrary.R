### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### September 2020
### Extract tables from db and script library

# Load packages -----------------------------------------------------------

# The package rgdal is also required. Rstudio might detect this requirement. If
# it doesn't, simply run: install.packages("rgdal")

library(raster)
library(rgdal)
library(RODBC)
library(rsyncrosim)
library(tidyverse)


# Set global variables ----------------------------------------------------

# Global variables and paths are set in the header file
source("scripts/headers.R")

# Prepare input data ---------------------------------------------------

## Load data 

# Load the main Vegetation Transition database from Land Fire
db <- 
  odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", 
                           landFireDBPath))

# Load the lookup table that will be used to color state classes by their EVC code
evcColors <- read_csv(evcColorsPath) %>%
  transmute(
    evcCode = VALUE,
    Color = paste("255", R, G, B, sep = ",")
  )

# Load the input rasters
vdistRaster <- raster(vdistRasterPath)
evtRaster <- raster(primaryStratumRasterPath) # Since primary stratum is EVT

## Generate a table of all unique transitions

# Should be unique for every VDIST, PrimaryStratum, EvT, SourceStateClass
transitionTable <- sqlFetch(db, transitionTableName) %>% 
  # Select and rename variables of importance
  select(MZ, VDIST, EVT7B, StratumIDSource = EVT7B_Name,
         EVCB, EVHB, EVCR, EVHR) %>%
  # Turn all factors into strings
  mutate_if(is.factor, as.character) %>%
  # Change the naming convention of MapZones e.g. from "1" to "MZ01"
  mutate(SecondaryStratumID = paste0("MZ", str_pad(MZ, 2, "left", "0"))) %>%
  # Keep only unique rows
  unique()

## Generate look-up tables for EVC and EVH codes and names

EVClookup <- sqlFetch(db, evcTableName) %>% 
  # Turn factors into characters
  mutate_if(is.factor, as.character) %>% 
  # Make unique names for when class names are repeated
  mutate(CLASSNAMES = coalesce(CLASSNAMES, str_c(EVT_LIFEFORM, "_", VALUE))) %>%
  # Select relevant columns and rename with stsim relevant column names
  select(VALUE, CLASSNAMES) %>% 
  rename(EVC_ID = VALUE, StateLabelXID = CLASSNAMES) %>%
  # Shorten EVC names for cleaner SyncroSim UI
  mutate(
    StateLabelXID = str_replace(StateLabelXID, " and <=? ", "-"),
    StateLabelXID = str_replace(StateLabelXID, "Tree Cover >=",  "Tr"),
    StateLabelXID = str_replace(StateLabelXID, "Tree Cover <",  "Tr <"),
    StateLabelXID = str_replace(StateLabelXID, "Shrub Cover >=", "Sh"),
    StateLabelXID = str_replace(StateLabelXID, "Herb Cover >=",  "Hb")
  )

EVHlookup <- sqlFetch(db, evhTableName) %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(CLASSNAMES = coalesce(CLASSNAMES, str_c(LIFEFORM, "_", VALUE))) %>%
  select(VALUE, CLASSNAMES) %>% 
  rename(EVH_ID = VALUE, StateLabelYID = CLASSNAMES) %>%
  # Shorten EVh names for cleaner SyncroSim UI
  mutate(
    StateLabelYID = str_replace(StateLabelYID, " to ", "-"),
    StateLabelYID = str_replace(StateLabelYID, " meters?", "m"),
    StateLabelYID = str_replace(StateLabelYID, "Forest Height", "Fr"),
    StateLabelYID = str_replace(StateLabelYID, "Shrub Height",  "Sh"),
    StateLabelYID = str_replace(StateLabelYID, "Herb Height",   "Hb"),
    StateLabelYID = str_replace(StateLabelYID, " 0-",   " < "),
  )

# Add EVC and EVH names to transition table

transitionTable <- transitionTable %>% 
  
  # First join EVC B and R, renaming appropriately
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

## Generate vectors of all vDIST and EVT codes present in data

allVDIST <- unique(vdistRaster)

allEVT <- unique(evtRaster)

# Build the SyncroSim Library ---------------------------------------------

# Create library with a project ("Definitions") and a scenario ("Test")
mylibrary <- ssimLibrary(libraryName, overwrite = TRUE)
myproject <- project(mylibrary, projectName, overwrite = TRUE)
myscenario <- scenario(myproject, scenarioName)

## +Terminology -----------------------------------------------------------

term <- data.frame(
  AmountLabel = "Area", 
  AmountUnits = "Acres", 
  StateLabelX = "EVC", 
  StateLabelY = "EVH", 
  PrimaryStratumLabel = "EVT",
  SecondaryStratumLabel = "MapZones",
  TimestepUnits = "Timestep"
)

saveDatasheet(ssimObject = myproject, data = term, 
              name = "stsim_Terminology")


## +Strata ----------------------------------------------------------------

# EVT is the primary stratum, MZ is the secondary stratum
# No tertiary stratum
# Important to take the unique values every time
# Filter this dataframe by EVT values that are actually present in the input


primary <- data.frame(ID = transitionTable$EVT7B, 
                      Name = transitionTable$StratumIDSource) %>%
  unique() %>%
  filter(ID %in% allEVT)

# Extract colors from the evt200 sheet
# TODO: An issue here where some IDs do not have colors and some colors do not
# have matching IDs. 

primaryWithColors <- sqlFetch(db, evtColorTableName) %>% 
  # Select relevant columns
  select(VALUE, R, G, B) %>% 
  # Take unique and rename for later joining
  unique() %>% rename(ID = VALUE) %>% 
  # Create the color using the SyncroSim pattern of T, R, G, B
  mutate(Color = paste("255", R, G, B, sep = ",")) %>% 
  # Join and select relevant columns
  right_join(primary, by = "ID") %>% 
  select(ID, Color, Name)

# Save the datasheet
saveDatasheet(myproject, primaryWithColors, "Stratum")

# Repeat for secondary stratum (MapZone), with no colors

secondary <- data.frame(ID = transitionTable$MZ, 
                        Name = transitionTable$SecondaryStratumID) %>% unique()
saveDatasheet(myproject, secondary, "SecondaryStratum")

## +State Classes --------------------------------------------------------

# Copy in X and Y state names from the EVC and EVH lookups, respectively

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

# Generate unique State IDS based on the combination of X and Y state
# To do this, we "paste" the X and Y state IDs together by multiplying
# EVC by 1000 and adding it to EVH

stateIDs <- c((transitionTable$EVCB*1000 + transitionTable$EVHB), 
              (transitionTable$EVCR*1000 + transitionTable$EVHR))

# Build the datasheet
stateClasses <- data.frame(
  ID = stateIDs,
  Name = c(transitionTable$StateClassIDSource, transitionTable$StateClassIDDest), 
  StateLabelXID = c(transitionTable$EVCB_Name, transitionTable$EVCR_Name),
  evcCode = c(transitionTable$EVCB, transitionTable$EVCR),
  StateLabelYID = c(transitionTable$EVHB_Name, transitionTable$EVHR_Name)) %>%
  left_join(evcColors, by = "evcCode") %>% # Use EVC to decide state color
  select(-evcCode) %>%                     # Remove code used to add colors
  unique()

saveDatasheet(myproject, stateClasses, "stsim_StateClass")

## +Transition Types and Groups ------------------------------------------------

# We gather disturbance types from the VDIST table
vdistLookup <-  sqlFetch(db, vdistTableName) %>%
  # Select only what we need, then rename
  select(value, d_type, d_severity, d_time, R, G, B) %>% 
  rename(ID = value,  TransitionGroupID = d_type) %>%
  # Turn all factors into charactors
  mutate_if(is.factor, as.character) %>%
  # Filter out the NO Disturbance category
  filter(ID != 0) %>%
  # Create unique transition/disturbance name, and format color
  # The format of the name is : Group, Severity, Frequency
  mutate(Name = paste(TransitionGroupID, d_severity, d_time, sep = " - ")) %>% 
  mutate(Color = paste("255", R, G, B, sep = ","))

# Select the relevant columns, and filter by disturbances that are actually
# present in the input raster
transitionTypes <- vdistLookup %>% 
  select(ID, Name, Color) %>% 
  unique() %>%
  filter(ID %in% allVDIST)

saveDatasheet(myproject, transitionTypes, "stsim_TransitionType")

## Transition Groups
# For groups, we append the disturbance class to the existing datasheet

transitionGroups <- datasheet(myproject, "stsim_TransitionGroup") %>%
  mutate_if(is.factor, as.character) %>% 
  bind_rows(vdistLookup %>% 
              select(Name = TransitionGroupID) %>% 
              unique())

saveDatasheet(myproject, transitionGroups, "stsim_TransitionGroup")

## Transition Types by groups

typesByGroup <- vdistLookup %>% 
  select(ID, Name, TransitionGroupID) %>% 
  unique() %>%
  filter(ID %in% allVDIST) %>%
  select(-ID) %>%
  rename(TransitionTypeID = Name)

saveDatasheet(myproject, typesByGroup, "stsim_TransitionTypeGroup")

## Transition simulation groups
# The same than groups, used for vizualization

simulationGroups <- data.frame(
  TransitionGroupID = unique(vdistLookup$TransitionGroupID))

saveDatasheet(myproject, simulationGroups, "stsim_TransitionSimulationGroup")

## +Transitions  ---------------------------------------------------------------

## Deterministic

# Generate locations for unique state classes to be used in the SyncroSim
# Transition Pathways Diagrams

locations <- stateClasses %>% 
  # Create unique positions for them
  mutate(
    letter = case_when(
      str_detect(StateLabelXID, "Tree")  ~ "A",
      str_detect(StateLabelXID, "Shrub") ~ "B",
      str_detect(StateLabelXID, "Herb")  ~ "C",
      TRUE                               ~ "D")) %>%
  # Arrange, split and lapply across all rows
  arrange(letter, StateLabelYID) %>% 
  group_by(letter) %>% 
  mutate(Location = str_c(letter, row_number())) %>% 
  ungroup() %>%
  select(Name, Location)

# Join the locations back into the state class table and clean up the datasheet
deterministicTransitions <- stateClasses %>% 
  # Make sure to expand the grid to all combinations of MZ present
  expand_grid(StratumIDSource = primary$Name) %>% 
  # Join with the location info
  left_join(locations, by = "Name") %>%
  # Cleanup
  select(Name, Location, StratumIDSource) %>% 
  rename(StateClassIDSource = Name) %>% 
  unique() %>%
  as.data.frame()

saveDatasheet(myscenario, deterministicTransitions, 
              "stsim_DeterministicTransition") 

## Probabilistic
probabilisticTransitions <- transitionTable %>%
  # Join the IDS
  left_join(transitionTypes, by = c("VDIST" = "ID")) %>% 
  # Rename and select what we need 
  rename(TransitionTypeID = Name) %>% 
  select(StratumIDSource, SecondaryStratumID,
                StateClassIDSource, StateClassIDDest, 
                TransitionTypeID, Probability) %>%
# Filter for values present in the input raster
  filter(TransitionTypeID %in% transitionTypes$Name) %>% 
  filter(StratumIDSource %in% primary$Name)

saveDatasheet(myscenario, probabilisticTransitions, "stsim_Transition")

## +Transition multipliers ---------------------------------------------------

# Collect the names and cretae path files
multiplierGroupNames <- paste0(transitionTypes$Name, " [Type]")
multiplierFileNames <- paste0(transitionMultiplierDirectory,
                              transitionTypes$Name,
                              cleanRasterSuffix,
                              ".tif")

# Compose and save the data frame
spatialMultiplier <- data.frame(
  TransitionGroupID = multiplierGroupNames,
  MultiplierFileName = multiplierFileNames)

saveDatasheet(myscenario, spatialMultiplier, 
              "stsim_TransitionSpatialMultiplier")

## +Initial conditions --------------------------------------------------------

initialConditionsSpatial <- data.frame(
  StateClassFileName = stateClassRasterPath,
  StratumFileName = primaryStratumRasterPath,
  SecondaryStratumFileName = secondaryStratumRasterPath)

saveDatasheet(myscenario, initialConditionsSpatial, 
              "stsim_InitialConditionsSpatial")

## +Run Control --------------------------------------------------------------

runControl <- data.frame(
  MinimumIteration = minimumIteration,
  MaximumIteration = maximumIteration, 
  MinimumTimestep =  minimumTimestep,
  MaximumTimestep =  maximumTimestep, 
  IsSpatial = TRUE
)

saveDatasheet(myscenario, runControl, "stsim_RunControl")

## +Output Options -----------------------------------------------------------

outputOptionsSummary <- 
  data.frame(SummaryOutputSC = TRUE, SummaryOutputSCTimesteps = 1, 
             SummaryOutputTR = TRUE, SummaryOutputTRTimesteps = 1)
outputOptionsSpatial <- 
  data.frame(RasterOutputSC = TRUE, RasterOutputSCTimesteps = 1,
             RasterOutputST = TRUE, RasterOutputSTTimesteps = 1,
             RasterOutputTR = TRUE, RasterOutputTRTimesteps = 1)

saveDatasheet(myscenario, outputOptionsSummary, "stsim_OutputOptions")
saveDatasheet(myscenario, outputOptionsSpatial, "stsim_OutputOptionsSpatial")

## +Spatial multiprocessing ---------------------------------------------------

spatial_multi <- datasheet(myscenario, "corestime_Multiprocessing")
spatial_multi <- add_row(spatial_multi, MaskFileName = tilingRasterPath)

saveDatasheet(myscenario, spatial_multi, "corestime_Multiprocessing")

# Cleanup ---------------------------------------------------------------------

# Close the database connection
odbcClose(db)

# QA Code -----------------------------------------------------------------

## Old tests for QA
# the_list <- transitionTable %>%
#   group_split(VDIST, SecondaryStratumID, StratumIDSource, EVCB, EVHB)
# the_vec <- sapply(the_list, nrow)
# larger <- which(the_vec > 1)
# the_list_of_larger <- the_list[larger]
# the_list_of_larger[1]

# raw <- sqlFetch(db, "vegtransf_rv02i_d")
# test <- raw[which(duplicated(transitionTable)),]
# View(test)

# raw <- sqlFetch(db, "vegtransf_rv02i_d")
# trans_vdist <- raw$VDIST %>% unique()
# key_Vdist <- distCrosswalk$VDIST %>% unique()
# trans_vdist[which(!(trans_vdist %in% key_Vdist))]

# raw_filtered <- raw %>%
#   filter(MZ %in% c(19)) %>%
#   filter(VDIST %in% allValues) %>%
#   filter(EVT7B %in% unique(raster("data/clean/cropped/nw_EVT_clean_small.tif")))

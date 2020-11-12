### LANDFIRE Project 
### APEX RMS - Shreeram Senthivasan 
### November 2020
### This a header file with global variables used by other scripts in this directory.
### Please check the paths and options set here prior to running the other scripts.


# Overall Run Options -----------------------------------------------------

# A tag used to identify the run, eg the mapzone, parameters that are being varied.
# Used to generate names for output folders, SyncroSim scenarios, etc
runTag <- "Map Zone 19"

# The name of the SyncroSim library to store the scenario, etc. in
runLibrary <- "LANDFIRE Update"

# Which mapzone to process
mapzoneToKeep <- 19

# Threads to use when parallel processing
nThreads <- future::availableCores() - 1

# Raw Inputs --------------------------------------------------------------

# Directory holding raw rasters
rawRasterDirectory <- "data/raw/"

# Raw input rasters
mapzoneRawRasterPath <- paste0(rawRasterDirectory, "nw_mapzone.tif")
evtRawRasterPath     <- paste0(rawRasterDirectory, "nw_evt20.tif")
evhRawRasterPath     <- paste0(rawRasterDirectory, "nw_evh_m.tif")
evcRawRasterPath     <- paste0(rawRasterDirectory, "nw_evc_m.tif")
fdistRawRasterPath   <- paste0(rawRasterDirectory, "nw_fdist.tif")

# Database Path
landFireDBPath <- 
  "db/NW_GeoArea_VegTransitions_Update_for_Remap_KCH_complete_2020_10_21.accdb"

# FDIST - VDIST Crosswalk
distCrosswalkPath <- "data/raw/nonspatial/LimUpdate2021_VDISTxFDIST_v03_20201009.xlsx"

# EVC color reference - Used to color stateclass map
evcColorsPath <- "data/raw/nonspatial/LF_Remap_EVC_03052019.csv"

# Database Table Names ---------------------------------------------------

#  These are the names of relevant SQL tables withing the database
transitionTableName <- "vegtransf_rv02i_d"
evcTableName        <- "EVC_LUT"
evhTableName        <- "EVH_LUT"
evtColorTableName   <- "nw_evt200"
vdistTableName      <- "VDIST"

# Cleaned Raster Options ---------------------------------------------------

# Directory to store cleaned rasters
# Note that the working directory is prepended since SyncroSim needs absolute paths
cleanRasterDirectory <- paste0(getwd(), "/data/clean/", runTag, "/")
dir.create(cleanRasterDirectory, showWarnings = F)

# Directory and prefix for FDIST binary rasters (spatial multipliers)
transitionMultiplierDirectory <- paste0(cleanRasterDirectory, "transitionMultipliers/")
dir.create(transitionMultiplierDirectory, showWarnings = F)

# Clean Raster Paths
mapzoneRasterPath <- paste0(cleanRasterDirectory, "MapZone.tif")
vdistRasterPath <- paste0(cleanRasterDirectory, "vDIST.tif")
stateClassRasterPath <- paste0(cleanRasterDirectory, "StateClass.tif")
tilingRasterPath <- paste0(cleanRasterDirectory, "Tiling.tif")

primaryStratumRasterPath <- paste0(cleanRasterDirectory, "EVT.tif")
secondaryStratumRasterPath <- mapzoneRasterPath


# A tiling mask is produced to allow for Spatial Multiprocessing in SyncroSim
# The size of the rows is dictated by the size of the raster and memory constraints,
# but the number of columns can be set here
tileCols <- 3

# Run Controls  -------------------------------------------------------

minimumIteration <- 1
maximumIteration <- 1
minimumTimestep <- 2017
maximumTimestep <- 2018

# SyncroSim Options -----------------------------------------------------
libraryName <- paste0("library/", runLibrary)
projectName <- "NW GeoArea"
scenarioName <- runTag

# Set the owner for all SyncroSim objects
ssimOwner <- "LANDFIRE"

# Set descriptions for SyncroSim objects
libraryDescription <-  paste0("ST-Sim library for updating LANDFIRE EVC and ",
                              "EVH based on starting MZ, EVT, EVC, EVH and ",
                              "disturbances during the update period.")
projectDescription <-  paste0("Models for updating the NW GeoArea.")
scenarioDescription <- paste0("Test updated on Map Zone 19. Note that rules ",
                              "used here will be updated.")

### LANDFIRE Project
### APEX RMS - Shreeram Senthivasan
### November 2020
### This a header file with global variables used by other scripts in this directory.
### Please check the paths and options set here prior to running the other scripts.


# Overall Options --------------------------------------------------------

# Which mapzones to process
#mapzonesToKeep <- c(1, 2, 7, 8, 9, 10, 18, 19, 20, 21, 22, 29) # Full set for NW GeoArea
mapzonesToKeep <- 19 # for testing

# Tags used to identify the independent runs (one per Map Zone)
# They are used to generate names for output folders, etc
runTags <- str_c("Map Zone ", mapzonesToKeep)

# The name of the SyncroSim library to store the scenario, etc. in
runLibrary <- "LANDFIRE Update"

# Threads to use when pre-processing rasters
nThreads <- future::availableCores()

# SyncroSim isntallation directory (leave as NULL to use default)
ssimDir <- NULL

# Log file path
logFilePath <- "run.log"
dir.create("log", showWarnings = F)

# Raw Inputs --------------------------------------------------------------

# Directory holding raw rasters
rawRasterDirectory <- "data/raw/"

# Raw input rasters
mapzoneRawRasterPath <- paste0(rawRasterDirectory, "nw_mapzone.tif")
evtRawRasterPath     <- paste0(rawRasterDirectory, "nw_evt.tif")
evhRawRasterPath     <- paste0(rawRasterDirectory, "nw_evh.tif")
evcRawRasterPath     <- paste0(rawRasterDirectory, "nw_evc.tif")
fdistRawRasterPath   <- paste0(rawRasterDirectory, "nw_fdist.tif")

# Raw non-spatial data directory
rawNonSpatialDirectory <- "data/raw/nonspatial/"

# FDIST to VDIST crosswalk
distCrosswalkPath <- paste0(rawNonSpatialDirectory, "Disturbance Crosswalk 2020-10-09.xlsx")

# EVC color reference - Used to color stateclass map
evcColorsPath <-     paste0(rawNonSpatialDirectory, "EVC Colors 2019-05-03.csv")

# The following were exported from "NW GeoArea VegTransitions 2020-10-21.accdb"
transitionTablePath <- paste0(rawNonSpatialDirectory, "Transition Table.xlsx")
evcTablePath        <- paste0(rawNonSpatialDirectory, "EVC LUT.xlsx")
evhTablePath        <- paste0(rawNonSpatialDirectory, "EVH LUT.xlsx")
evtColorTablePath   <- paste0(rawNonSpatialDirectory, "EVT Colors.xlsx")
vdistTablePath      <- paste0(rawNonSpatialDirectory, "VDIST Table.xlsx")
allowedStatesPath   <- paste0(rawNonSpatialDirectory, "Allowed EVC-EVH.csv")

# Output Raster Options ---------------------------------------------------

# A tiling mask is produced to allow for Spatial Multiprocessing in SyncroSim
# The size of the rows is dictated by the size of the raster and memory constraints,
# but the number of columns can be set here
tileCols <- 10

# Run Controls  -------------------------------------------------------

minimumIteration <- 1
maximumIteration <- 1
minimumTimestep <- 2017
maximumTimestep <- 2018

# SyncroSim Options -----------------------------------------------------
libraryName <- paste0("library/", runLibrary)
projectName <- "NW GeoArea"
subScenarioName <- "Shared Model Info"
scenarioNames <- runTags

# Set the owner for all SyncroSim objects
ssimOwner <- "LANDFIRE"

# Set descriptions for SyncroSim objects
libraryDescription <-     paste0("ST-Sim library for updating LANDFIRE EVC and ",
                                 "EVH based on starting MZ, EVT, EVC, EVH and ",
                                 "disturbances during the update period.")
projectDescription <-     paste0("Models for updating the NW GeoArea.")
subScenarioDescription <- paste0("Sub Scenario used to define common model info ",
                                 "used by all other scenarios in the project.")
scenarioDescriptions <-   paste0("Model scenario for Map Zone ", mapzonesToKeep)

# Set the number of concurrent jobs to in SyncroSim
ssimJobs <- 8

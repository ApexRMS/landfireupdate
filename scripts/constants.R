### LANDFIRE Project
### APEX RMS - Shreeram Senthivasan
### November 2020
### This a header file with global variables used by other scripts in this directory.
### Please check the paths and options set here prior to running the other scripts.


# Overall Options --------------------------------------------------------

# Which mapzones to process
# mapzonesToKeep <- c(1, 2, 3, 7, 8, 9, 10, 12, 16, 17, 18, 19, 20, 21, 22, 23, 28, 29, 30, 31, 33)
mapzonesToKeep <- 19:20 # testing

# Tags used to identify the independent runs (one per Map Zone)
# They are used to generate names for output folders, etc
runTags <- str_c("Map Zone ", mapzonesToKeep)

# The name of the SyncroSim library to store the scenario, etc. in
runLibrary <- "LANDFIRE Update"

# Threads to use when pre-processing rasters
nThreads <- future::availableCores()

# Raw Inputs --------------------------------------------------------------

# Directory holding raw rasters
rawRasterDirectory <- "data/raw/"

# Raw input rasters
mapzoneRawRasterPath <- paste0(rawRasterDirectory, "nw_mapzone.tif")
evtRawRasterPath     <- paste0(rawRasterDirectory, "nw_evt.tif")
evhRawRasterPath     <- paste0(rawRasterDirectory, "nw_evh.tif")
evcRawRasterPath     <- paste0(rawRasterDirectory, "nw_evc.tif")
fdistRawRasterPath   <- paste0(rawRasterDirectory, "nw_fdist.tif")

# Database Path
landFireDBPath <-
  "db/NW GeoArea VegTransitions 2020-10-21.accdb"

# FDIST - VDIST Crosswalk
distCrosswalkPath <- "data/raw/nonspatial/Disturbance Crosswalk 2020-10-09.xlsx"

# EVC color reference - Used to color stateclass map
evcColorsPath <- "data/raw/nonspatial/EVC Colors 2019-05-03.csv"

# Database Table Names ---------------------------------------------------

#  These are the names of relevant SQL tables withing the database
transitionTableName <- "vegtransf_rv02i_d"
evcTableName        <- "EVC_LUT"
evhTableName        <- "EVH_LUT"
evtColorTableName   <- "nw_evt200"
vdistTableName      <- "VDIST"

# Cleaned Raster Options ---------------------------------------------------

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
scenarioNames <- runTags

# Multiple libraries are made in parallel to avoid collisions, they require unique names
parallelLibraryNames <- paste0("library/", runLibrary, "-", mapzonesToKeep)

# Set the owner for all SyncroSim objects
ssimOwner <- "LANDFIRE"

# Set descriptions for SyncroSim objects
libraryDescription <-  paste0("ST-Sim library for updating LANDFIRE EVC and ",
                              "EVH based on starting MZ, EVT, EVC, EVH and ",
                              "disturbances during the update period.")
projectDescription <-  paste0("Models for updating the NW GeoArea.")
scenarioDescriptions <- paste0("Test updated on Map Zone ", mapzonesToKeep,
                               ". Note that rules used here will be updated.")

# Set the number of concurrent jobs to in SyncroSim
ssimJobs <- 5

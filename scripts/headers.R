### LANDFIRE Project 
### APEX RMS - Shreeram Senthivasan 
### September 2020
### This a header file with global variables used by other scripts in this directory.
### Please check the paths and options set here prior to running the other scripts.


# Overall Run Options -----------------------------------------------------

# A tag used to identify the run, eg the mapzone, parameters that are being varied 
# used to generate names for output folders, SyncroSim scenarios, etc
runTag <- "MZ19"

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

# Database table names ---------------------------------------------------

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

# Suffix to add to output rasters (use to indicate crop options, etc)
cleanRasterSuffix <- ""

# Directory and prefix for FDIST binary rasters (spatial multipliers)
transitionMultiplierDirectory <- paste0(cleanRasterDirectory, "transitionMultipliers/")
dir.create(transitionMultiplierDirectory, showWarnings = F)

# Clean Raster Paths
mapzoneRasterPath <- paste0(cleanRasterDirectory, "Mapzones", cleanRasterSuffix, ".tif")
vdistRasterPath <- paste0(cleanRasterDirectory, "vDIST", cleanRasterSuffix, ".tif")
stateClassRasterPath <- paste0(cleanRasterDirectory, "StateClass", cleanRasterSuffix, ".tif")
tilingRasterPath <- paste0(cleanRasterDirectory, "Tiling", cleanRasterSuffix, ".tif")

primaryStratumRasterPath <- paste0(cleanRasterDirectory, "EVT", cleanRasterSuffix, ".tif")
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
libraryName <- paste0("library/LandFire", cleanRasterSuffix, ".ssim")
projectName <- "Landfire"
scenarioName <- runTag
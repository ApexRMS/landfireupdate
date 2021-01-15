### LANDFIRE Project
### APEX RMS - Shreeram Senthivasan
### December 2020
### This script is used to reconstruct EVC and EVH raster maps for an entire
### Geo Area using the output of the SyncroSim library constructed by the
### `batchProcess.R` script

# Setup -----------------------------------------------------------------------

library(rsyncrosim) # for building and connecting to SyncroSim files
library(raster)     # provides functions for manipulating rasters
library(rgdal)      # provides some optional dependencies for raster
library(tidyverse)  # provides general data manipulation functions
library(yaml)       # used to read configuration file

# Load configuration options and global constants
source("scripts/constants.R")

# Load custom raster functions optimized for working with large rasters
source("scripts/rasterFunctions.R")

# Generate Directories and Paths -----------------------------------------------

message("Preparing stitched raster map output folder.")

# Create directory and paths to store stitched raster maps
dir.create(stitchedRasterDirectory, showWarnings = F)
stateClassSitchedRasterPath <- paste0(stitchedRasterDirectory, "/StateClass.tif")
evcStitchedRasterPath <- paste0(stitchedRasterDirectory, "/EVC.tif")
evhStitchedRasterPath <- paste0(stitchedRasterDirectory, "/EVH.tif")
evcContinuousRasterPath <- paste0(stitchedRasterDirectory, "/Continuous EVC.tif")
evhContinuousRasterPath <- paste0(stitchedRasterDirectory, "/Continuous EVH.tif")
evcOverlaidRasterPath <- paste0(stitchedRasterDirectory, "/Overlaid EVC.tif")
evhOverlaidRasterPath <- paste0(stitchedRasterDirectory, "/Overlaid EVH.tif")

# Calculate final extent of the stitched rasters
fullExtent <- extent(raster(mapzoneRawRasterPath))

# Load necessary raw raster maps
evcContinuousRawRaster <- raster(evcContinuousRawRasterPath)
evhContinuousRawRaster <- raster(evhContinuousRawRasterPath)

# Extract Data from SyncroSim Library ------------------------------------------

message("Loading data from SyncroSim.")

# Connect to the SyncroSim Library
ssimSession <- session(ssimDir)
mylibrary <- ssimLibrary(libraryName, session = ssimSession)
myproject <- rsyncrosim::project(mylibrary, projectName)

# Find the list of result scenarios
# - Keep only the last successful run from each scenario

# Create a sink to capture verbose output
sink("temp.sink")

resultScenarios <- scenario(mylibrary) %>%
  

  # Only consider result scenarios
  filter(isResult == "Yes") %>%
  
  # Examine run logs to see which runs failed
  mutate(
    failed = map_lgl(
      scenarioId,
      ~ scenario(mylibrary, .x) %>%
        runLog %>%
        str_detect("Failure"))) %>%
  
  # Remove failed runs
  filter(failed == FALSE) %>%
  
  # Only keep last run from each parent scenario
  group_by(parentID) %>%
  filter(scenarioId == max(scenarioId)) %>%
  pull(scenarioId)

# Close and delete sink
sink()
unlink("temp.sink")

if(length(resultScenarios) == 0)
  stop(str_c(
    "No result scenarios found! Please ensure that you've run the relevant ",
    "SyncroSim scenarios and that the library and project name set in the ",
    "config file match those of the maps you are trying to reconstruct."))

# Pull out the relevant raster from each result scenario as a list
stateClassRasters <- 
  map(resultScenarios,
    ~ datasheetRaster(
        scenario(mylibrary, .x), 
        datasheet = "OutputSpatialState", 
        iteration = 1, 
        timestep = maximumTimestep)
  )

# Stitch together State Class raster -------------------------------------------

message("Stitching raster maps.")

# Append arguments for `raster::merge()` to the previously generated list of rasters
# This will allow us to pass a variable number of arguments to `raster::merge()`
# using `do.call()`

mergeArgs <- c(stateClassRasters,                      # the rasters to stitch together
               filename = stateClassSitchedRasterPath, # output file name
               ext = fullExtent,                       # the final extent of the raster
               overwrite = T)

# Stitch together and save
# - if there is only one result scenario, just save the one Map Zone
if(length(stateClassRasters) == 1) {
  stateClassSitchedRaster <- writeRaster(stateClassRasters[[1]], stateClassSitchedRasterPath, overwrite = T)
} else {
  stateClassSitchedRaster <- do.call(raster::merge, mergeArgs)
}

# Generate EVC and EVH from State Class ----------------------------------------

message("Separating out EVC and EVH.")

# Create empty rasters to hold EVC and EVH data
evcStitchedRaster <-  raster(stateClassSitchedRaster)
evhStitchedRaster <-  raster(stateClassSitchedRaster)

## Split state class raster into manageable chunks
blockInfo <- blockSize(stateClassSitchedRaster)

## Calculate EVC and EVH from State Class block-by-block and write the results to their respective files
evcStitchedRaster <- writeStart(evcStitchedRaster, evcStitchedRasterPath, overwrite=TRUE)
evhStitchedRaster <- writeStart(evhStitchedRaster, evhStitchedRasterPath, overwrite=TRUE)

for(i in seq(blockInfo$n)) {
  stateClassValues <-
    getValuesBlock(stateClassSitchedRaster, row = blockInfo$row[i], nrows = blockInfo$nrows[i])
  
  EVCValues <- as.integer(stateClassValues / 1000) # EVC is the first three digits of the six digit state class code
  EVHValues <- stateClassValues %% 1000            # EVH is the last three digits, `%%` is the modulo, or remainder function
  
  evcStitchedRaster <- writeValues(evcStitchedRaster, EVCValues, blockInfo$row[i])
  evhStitchedRaster <- writeValues(evhStitchedRaster, EVHValues, blockInfo$row[i])
}

evcStitchedRaster <- writeStop(evcStitchedRaster)
evhStitchedRaster <- writeStop(evhStitchedRaster)

# Convert EVC and EVH to continuous codes --------------------------------------

message("Converting EVC and EVH to continuous codes.")

# Setup the two crosswalks from class codes to continuous codes
evcCrosswalk <- 
  read_csv(evcTablePath) %>%
    select(from = VALUE, to = CONTINUOUS) %>%
    as.matrix

evhCrosswalk <- 
  read_csv(evhTablePath) %>%
    select(from = VALUE, to = CONTINUOUS) %>%
    as.matrix

# Reclassify both rasters using the constructed crosswalks
evcContinuousRaster <-
  reclassify(
    evcStitchedRaster,
    evcCrosswalk,
    filename = evcContinuousRasterPath,
    overwrite = T)

evhContinuousRaster <-
  reclassify(
    evhStitchedRaster,
    evhCrosswalk,
    filename = evhContinuousRasterPath,
    overwrite = T)

message("Done converting to continuous codes!")

# Overlay disturbed EVC and EVH over initial EVC and EVH -----------------------

message("Overlaying disturbed EVC and EVH.")

# If in test mode, crop down the raw continuous EVC and EVH maps
if(cropToExtent) {
  evcContinuousRawRaster <- crop(evcContinuousRawRaster, cropExtent)
  evhContinuousRawRaster <- crop(evhContinuousRawRaster, cropExtent)
  
  # Also update the extent to use for merging
  fullExtent <- cropExtent
}

# Use raster::merge() to overlay the new continuous data over the old continuous
# EVC and EVH raster maps
evcOverlaidRaster <-
  raster::merge(
    evcContinuousRaster,
    evcContinuousRawRaster,
    filename = evcOverlaidRasterPath,
    ext = fullExtent,
    overwrite = T)

evhOverlaidRaster <-
  raster::merge(
    evhContinuousRaster,
    evhContinuousRawRaster,
    filename = evhOverlaidRasterPath,
    ext = fullExtent,
    overwrite = T)

message("Done overlaying distrubed cells!")

# Wrap up ----------------------------------------------------------------------

message(str_c("Done reconstructing Geo Area! ", 
              "Stitched raster maps can be found in ",
              stitchedRasterDirectory))
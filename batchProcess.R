### LANDFIRE Project
### APEX RMS - Shreeram Senthivasan
### November 2020
###
### This script streamlines batch processing of all Map Zones within a single
### GeoArea to generate a single SyncroSim library with a scenario for each Map
### Zone. Please check over the `scripts/constants.R` script to ensure all paths
### and options are set. See the Configuration section of the README file for
### more information

# Setup -----------------------------------------------------------------------

library(rsyncrosim) # for building and connecting to SyncroSim files
library(terra)     # provides functions for manipulating rasters
library(furrr)      # for parallel iteration
library(logr)       # for generating logs with RScript
library(tidyverse)  # provides general data manipulation functions
library(yaml)       # used to parse the config file

# Load configuration options and global constants
source("scripts/constants.R")

terraOptions(memmax = 3.5)

# Load custom raster functions optimized for working with large rasters
source("scripts/rasterFunctions.R")

# Load functions for preprocessing image files, layerizing disturbance maps, and
# building the SyncroSim library

source("scripts/processSpatialData.R")
source("scripts/layerizeDisturbance.R")
source("scripts/buildSsimLibrary.R")
source("scripts/checkLibrary.R")

# Begin logging ------------------------------------------------------------

logFile <- log_open(logFilePath)

# Process rasters ----------------------------------------------------------

# The first step is to split the entire GeoArea rasters down into the Map Zones
# we wish to process and generate related rasters, including a State Class
# raster and a Tiling raster. Please see `scripts/processSpatialData.R` for
# details.

log_print("Starting pre-process!")

# Remove old folder of cleaned data to avoid mixing data from runs
unlink(file.path(cleanRasterDirectoryRelative), recursive = T)

# This function is safe to parallelize
# Begin parallel processing
plan(multisession, workers = nThreads)

future_walk2(
  mapzonesToKeep,
  runTags,
  processSpatialData,
  .options =
    furrr_options(
      seed = TRUE)
  )

# Return to sequential operation
plan(sequential)

# Layerize disturbance maps ------------------------------------------------

# Next we need to split the disturbance raster for each Map Zone into binary
# layers for SyncroSim to use as transition multipliers. Please see
# `scripts/layerizeDisturbance.R` for details.

log_print("Starting layerizing of disturbances!")

# Note: Temporarily restructuring parallelization for new layerizing code
# # It is more efficient to parallelize this step within each Map Zone, see
# # `scripts/layerizeDisturbance.R` for more info

# This function is safe to parallelize
# Begin parallel processing
plan(multisession, workers = nThreads)

future_walk(runTags, layerizeDisturbance, .options = furrr_options(seed = TRUE))

# Return to sequential operation
plan(sequential)

# Build SyncroSim Library --------------------------------------------------

# Finally we need to build the SyncroSim library, project, and scenarios and
# connect them to the processed rasters. See `scripts/buildSsimLibrary.R` for
# details.

log_print("Starting Syncrosim library building!")

# Start by building a library with a template scenario
initializeSsimLibrary(libraryName, projectName)


log_print("Checking states and rules in SyncroSim Library!")

# Check the library and cleaned data for errors before continuing
checkLibrary(libraryName, projectName, runTags)

log_print("Building scenarios for each Map Zone!")

# For each Map Zone, copy the template and connect the relevant rasters
pwalk(
  list(
    runTag = runTags,
    scenarioName = scenarioNames,
    scenarioDescription = scenarioDescriptions
  ),
  buildSsimScenarios,
  libraryName = libraryName,
  projectName = projectName
)

log_print("Done building SyncroSim Library!")

# End logging ---------------------------------------------------------------
log_close()

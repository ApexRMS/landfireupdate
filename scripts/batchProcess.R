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
library(raster)     # provides functions for manipulating rasters
library(rgdal)      # provides some optional dependencies for raster
library(readxl)     # for reading disturbance crosswalk
library(furrr)      # for parallel iteration
library(RODBC)      # for connection to MS database files
library(tidyverse)  # provides general data manipulation functions

# Load configuration options and global constants
source("scripts/constants.R")

# Load custom raster functions optimized for working with large rasters
source("scripts/rasterFunctions.R")

# Load functions for preprocessing image files, layerizing disturbance maps, and
# building the SyncroSim library

source("scripts/processSpatialData.R")
source("scripts/layerizeDisturbance.R")
source("scripts/buildSsimLibrary.R")

# Process rasters ----------------------------------------------------------

# The first step is to split the entire GeoArea rasters down into the Map Zones
# we wish to process and generate related rasters, including a State Class
# raster and a Tiling raster. Please see `scripts/processSpatialData.R` for
# details.

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

# It is more efficient to parallelize this step within each Map Zone, see
# `scripts/layerizeDisturbance.R` for more info
walk(runTags, layerizeDisturbance)

# Build SyncroSim Library --------------------------------------------------

# Finally we need to build the SyncroSim library, project, and scenarios and
# connect them to the processed rasters. See `scripts/buildSsimLibrary.R` for
# details.

# Start by building a library with a template scenario
initializeSsimLibrary(libraryName, projectName)

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

# Generate a backup of the library and move it to a standard location
outputLibrary <- ssimLibrary(libraryName)
rsyncrosim::backup(outputLibrary)

backupFilePath <- list.files(paste0(libraryName, ".ssim.backup"), full.names = TRUE) %>% tail(1)
file.rename(backupFilePath, "Library Backup.zip")


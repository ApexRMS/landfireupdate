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

# Load configuration options and global constants
source("scripts/constants.R")

# Load custom raster functions optimized for working with large rasters
source("scripts/rasterFunctions.R")

# Generate Directories and Paths -----------------------------------------------

# Create directory to store stitched raster maps
dir.create(stitchedRasterDirectory, showWarnings = F)
stateClassSitchedRasterPath <- paste0(stitchedRasterDirectory, "/StateClass.tif")
EVCSitchedRasterPath <- paste0(stitchedRasterDirectory, "/EVC.tif")
EVHSitchedRasterPath <- paste0(stitchedRasterDirectory, "/EVH.tif")

# Calculate final extent of the stitched rasters
fullExtent <- extent(raster(mapzoneRawRasterPath))

# Extract Data from SyncroSim Library ------------------------------------------

# Connect to the SyncroSim Library
ssimSession <- session(ssimDir)
mylibrary <- ssimLibrary(libraryName, session = ssimSession)
myproject <- rsyncrosim::project(mylibrary, projectName)

# Find the list of result scenarios
resultScenarios <- scenario(mylibrary) %>%
  filter(isResult == "Yes") %>%
  pull(scenarioId)

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

# Append arguments for `raster::merge()` to the previously generated list of rasters
# This will allow us to pass a variable number of arguments to `raster::merge()`
# using `do.call()`

mergeArgs <- c(stateClassRasters,                      # the rasters to stitch together
               filename = stateClassSitchedRasterPath, # output file name
               ext = fullExtent,                       # the final extent of the raster
               overwrite = T)

# Stitch together and save
stateClassSitchedRaster <- do.call(merge, mergeArgs)

# Generate EVC and EVH from State Class ----------------------------------------

# Create empty rasters to hold EVC and EVH data
EVCSitchedRaster <-  raster(stateClassSitchedRaster)
EVHSitchedRaster <-  raster(stateClassSitchedRaster)

## Split state class raster into manageable chunks
blockInfo <- blockSize(stateClassSitchedRaster)

## Calculate EVC and EVH from State Class block-by-block and write the results to their respective files
EVCSitchedRaster <- writeStart(EVCSitchedRaster, EVCSitchedRasterPath, overwrite=TRUE)
EVHSitchedRaster <- writeStart(EVHSitchedRaster, EVHSitchedRasterPath, overwrite=TRUE)

for(i in seq(blockInfo$n)) {
  stateClassValues <-
    getValuesBlock(stateClassSitchedRaster, row = blockInfo$row[i], nrows = blockInfo$nrows[i])
  
  EVCValues <- as.integer(stateClassValues / 1000) # EVC is the first three digits of the six digit state class code
  EVHValues <- stateClassValues %% 1000            # EVH is the last three digits, `%%` is the modulo, or remainder function
  
  EVCSitchedRaster <- writeValues(EVCSitchedRaster, EVCValues, blockInfo$row[i])
  EVHSitchedRaster <- writeValues(EVHSitchedRaster, EVHValues, blockInfo$row[i])
}

EVCSitchedRaster <- writeStop(EVCSitchedRaster)
EVHSitchedRaster <- writeStop(EVHSitchedRaster)



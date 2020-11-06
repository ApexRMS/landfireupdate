### LANDFIRE Project 
### APEX RMS - Valentin Lucet 
### September 2020
### Cleaning spatial data obtained from client

# Load packages ------------------------------------------------------------
library(raster) # Raster packages deals with tiff files (grid files)
library(tidyverse)
library(pryr)   # Testing - check mem usage
library(furrr) # For parallel iteration

# Set global variables ----------------------------------------------------

## +Inputs -----------------------------------------------------------------

# Directory holding raw rasters
rawRasterDirectory <- "data/raw/"
rawRasterDirectory <- "D:/A236/a236/data/raw/" # testing

# Raw input rasters
mapzoneRasterPath <- paste0(rawRasterDirectory, "nw_mapzone_tiff/nw_mapzone.tif")
evtRasterPath     <- paste0(rawRasterDirectory, "nw_evt2.0_tiff/nw_evt20.tif")
evhRasterPath     <- paste0(rawRasterDirectory, "nw_evh2.0class1.4_tiff/nw_evh_m.tif")
evcRasterPath     <- paste0(rawRasterDirectory, "nw_evc2.0class1.4_tiff/nw_evc_m.tif")
fdistRasterPath   <- paste0(rawRasterDirectory, "NW_FDIST2014_TIFF/nw_fdist2014.tif")

## +Outputs ---------------------------------------------------------------

# Directory to store cleaned rasters
cleanRasterDirectory <- "data/clean/test/"
dir.create(cleanRasterDirectory, showWarnings = F)

# Suffix to add to output rasters (use to indicate crop options, etc)
cleanRasterSuffix <- ""

# Directory and prefix for FDIST binary rasters (spatial multipliers)
multiplierRasterFolder <- paste0(cleanRasterDirectory, "fdistRaster/")
multiplierRasterPrefix <- paste0(multiplierRasterFolder, "FDIST_value_")
dir.create(multiplierRasterFolder, showWarnings = F)

# Tile Resolution
# A tile mask is produced to allow for Spatial Multiprocessing in SyncroSim
# Choose the number of rows and columns to split the rasters into for the tiling
tileRows <- 5
tileCols <- 5

## +Other options -----------------------------------------------------------

# Option to crop to fixed extent
cropToExtent <- TRUE # testing
cropExtent   <- extent(-1449683,-1055783, 2413797, 3024897) # testing - bounding box of MZ19

# Switches to skip steps to save time while testing
skipCrop <- TRUE

# Parallel options
# Using all cores can slow down user interface when run interactively
# This is not a concern in HPC situations
nThreads <- availableCores() - 1

# Used in conjunction with the size of the rasters to decide how many blocks to 
# split the maps into when possible
# This value does not account for overhead memory usage and is only an
# approximation, please use a generous safety factor!
gbMemoryPerThread <- 1

# Load spatial data -------------------------------------------------------

# Mapzones for north west GeoRegion
mapzoneRaster <- raster(mapzoneRasterPath)

# EVT, EVH, EVC
evtRaster <- raster(evtRasterPath)
evhRaster <- raster(evhRasterPath)
evcRaster <- raster(evcRasterPath)

# fdistRaster
fdistRaster <- raster(fdistRasterPath)

# Change the origin of mapzone raster
origin(mapzoneRaster) <- origin(evtRaster)


# Crop data --------------------------------------------------------------
if(!skipCrop){
  if (cropToExtent)
    mapzoneRaster <- crop(mapzoneRaster, cropExtent)
  
  evtRaster <- crop(evtRaster, mapzoneRaster)
  evhRaster <- crop(evhRaster, mapzoneRaster)
  evcRaster <- crop(evcRaster, mapzoneRaster)
  fdistRaster <- crop(fdistRaster, mapzoneRaster)
  
  ## Save clean data
  writeRaster(mapzoneRaster, 
              paste0(cleanRasterDirectory, "nw_Mapzones", cleanRasterSuffix,".tif"),
              overwrite = TRUE)
  writeRaster(evtRaster, 
              paste0(cleanRasterDirectory, "nw_EVT", cleanRasterSuffix,".tif"),
              overwrite = TRUE)
  writeRaster(evhRaster,
              paste0(cleanRasterDirectory, "nw_EVH", cleanRasterSuffix,".tif"),
              overwrite = TRUE)
  writeRaster(evcRaster,
              paste0(cleanRasterDirectory, "nw_EVC", cleanRasterSuffix,".tif"),
              overwrite = TRUE)
  writeRaster(fdistRaster,
              paste0(cleanRasterDirectory, "nw_fDIST", cleanRasterSuffix,".tif"),
              overwrite = TRUE)
} else{
  mapzoneRaster <- raster(paste0(cleanRasterDirectory, "nw_Mapzones", cleanRasterSuffix,".tif"))
  evtRaster <- raster(paste0(cleanRasterDirectory, "nw_EVT", cleanRasterSuffix,".tif"))
  evhRaster <- raster(paste0(cleanRasterDirectory, "nw_EVH", cleanRasterSuffix,".tif"))
  evcRaster <- raster(paste0(cleanRasterDirectory, "nw_EVC", cleanRasterSuffix,".tif"))
  fdistRaster <- raster(paste0(cleanRasterDirectory, "nw_fDIST", cleanRasterSuffix,".tif"))
}

# Layerize disturbance raster --------------------------------------------------

# Memory safe function to return unique values from sections of a raster
uniqueInBlock <- function(blockID, fullRaster, totalBlocks){
  blockHeight <- ceiling(nrow(fullRaster) / totalBlocks)
  blockRow <- 1 + (blockID - 1) * blockHeight
  return(unique(getValuesBlock(fullRaster, row = blockRow, nrows = blockHeight)))
}

# Function to create and save a binary raster given a non-binary raster and the
# value to test for
saveFdistLayer <- function(fdistValue, fullRaster) {
  writeRaster(
    layerize(fullRaster, classes = fdistValue),
    paste0(multiplierRasterPrefix, fdistValue, ".tif"),
    overwrite = TRUE
  )
}

# Choose number of blocks to split rasters into when processing to limit memory
# usage per thread (important for HPC)
nBlocks <- max(
  nThreads,                         # Use at least as many blocks as threads
  ceiling(
    ncell(fdistRaster) * 16 /       # Each cell, uncompressed is 8bytes, most operations require two copies
    (gbMemoryPerThread * 1e9 - 3e8) # 3e8, or 300MB is an experimental approximate overhead memory use per thread
  )
)

# Begin parallel processing
plan(multisession, workers = nThreads)

# Split the FDIST raster into blocks to calculate unique vector of FDIST codes
# without using excessive memory per thread
fdistLevels <- 
  future_map(
    seq(nBlocks),
    uniqueInBlock,
    fullRaster = fdistRaster,
    totalBlocks = nBlocks,
    .options = furrr_options(
      seed = TRUE,
      globals = c("fdistRaster", "uniqueInBlock"),
      packages = "raster"
      )) %>%
  flatten_dbl %>%
  unique() %>%
  na.omit

# Split the FDIST raster into binary layers
# One layer is constructed at a time per thread to limit memory use per thread
future_walk(
  fdistLevels,
  saveFdistLayer,
  fullRaster = fdistRaster,
  .options = furrr_options(seed = TRUE))

# Return to sequential operation
plan(sequential)

# Create composite state class map ----------------------------------------

# EVC and EVH codes are both three digits
# We can "paste" these codes together by multiplying EVC by 1000 and summing
stateClasses <- evcRaster * 1000 + evhRaster
writeRaster(stateClasses,
            paste0(cleanRasterDirectory, "nw_EVC_EVH_StateClasses.tif"),
            overwrite = TRUE)

# Tiling for spatial multiprocessing -------------------------------------

# Function to generate a tiling mask given template
# To avoid holding the entire raster in memory, the raster is written directly
# to file row-by-row. This way only one row of the tiling needs to be held in
# memory at a time. This is also why the number of rows (and implicitly the size
# of a given row) cannot be chosen manually
tilize <- function(templateRaster, filename, nx = 3) {
  # Calculate recommended block size of template
  blockInfo <- blockSize(templateRaster)
  ny <- blockInfo$n
  if(missing(nx))
    nx <- ceiling(minCells / ny)
  
  # Calculate dimensions of each tile
  tileHeight <- blockInfo$nrows[1]
  tileWidth <- ceiling(ncol(templateRaster) / nx)

  # Generate a string of zeros the width of one tile
  oneTileWidth <- rep(0, tileWidth)
  
  # Generate one line of one row, repeat to the height of one row
  oneRow <- 
    as.vector(vapply(seq(nx), function(i) oneTileWidth + i, FUN.VALUE = numeric(tileWidth))) %>%
    `[`(1:ncol(templateRaster)) %>% # Trim the length of one row to fit in template
    rep(tileHeight)
  
  # Write an empty raster with the correct metadata to file
  tileRaster <- raster(
    nrows = nrow(templateRaster),
    ncols = ncol(templateRaster),
    ext = extent(templateRaster),
    crs = crs(templateRaster)
  )
  
  # Write tiling to file row-by-row
  tileRaster <- writeStart(tileRaster, filename,  overwrite=TRUE)
  for(i in seq(blockInfo$n)) {
    if(blockInfo$nrows[i] < tileHeight)
      oneRow <- oneRow[1:(ncol(tileRaster) * blockInfo$nrows[i])]
    #browser()
    tileRaster <- writeValues(tileRaster, oneRow, blockInfo$row[i])
    oneRow <- oneRow + nx
  }
  tileRaster <- writeStop(tileRaster)
  
  # Return
  return(tileRaster)
}

# Generate and write the tiling raster to file
tileRaster <- 
  tilize(
    mapzoneRaster, 
    paste0(cleanRasterDirectory, "Tiling", cleanRasterSuffix, ".tif"),
    nx = 3)

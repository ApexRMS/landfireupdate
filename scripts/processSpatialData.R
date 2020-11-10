### LANDFIRE Project 
### APEX RMS - Valentin Lucet and Shreeram Senthivasan 
### September 2020
### This script is used to clean and pre-process raw spatial data obtained from 
### Land Fire for simulation in SyncroSim

# Load packages ------------------------------------------------------------
library(raster) # Raster packages deals with tiff files (grid files)
library(tidyverse)
library(pryr)   # Testing - check mem usage
library(furrr)  # For parallel iteration
library(readxl) # For reading disturbance crosswalk

# Load global options ----------------------------------------------------

# Global options are set in the header file
source("scripts/headers.R")

# Load non-spatial data --------------------------------------------------
distCrosswalk <-   read_xlsx(distCrosswalkPath) %>%
  mutate(name = paste(d_type...2, d_severity...3, d_time...4, sep = " - ")) %>% 
  select(fdist = FDIST, vdist = VDIST, name)
# Load spatial data -------------------------------------------------------

# Mapzones for north west GeoRegion
mapzoneRaster <- raster(mapzoneRawRasterPath)

# EVT, EVH, EVC
evtRaster <- raster(evtRawRasterPath)
evhRaster <- raster(evhRawRasterPath)
evcRaster <- raster(evcRawRasterPath)

# fdistRaster
fdistRaster <- raster(fdistRawRasterPath)

# Change the origin of mapzone raster
origin(mapzoneRaster) <- origin(evtRaster)


# Setup mask --------------------------------------------------------------

# A memory-safe raster mask function optimized for large rasters
# Requires an output filename, slower than raster::mask for small rasters
# input and mask rasters should have the same extent
maskByMapzone <- function(inputRaster, maskRaster, maskValue, filename){
  # Integer mask values save memory
  maskValue <- as.integer(maskValue)
  
  # Let raster::blockSize() decide appropriate blocks to break the raster into
  blockInfo <- blockSize(inputRaster)
  
  # Generate empty raster with appropriate dimensions
  outputRaster <- raster(
    nrows = nrow(inputRaster),
    ncols = ncol(inputRaster),
    ext = extent(inputRaster),
    crs = crs(inputRaster)
  )
  
  # Calculate mask and write to output block-by-block
  outputRaster <- writeStart(outputRaster, filename, overwrite=TRUE)
  
  for(i in seq(blockInfo$n)) {
    blockMask <- 
      getValuesBlock(inputRaster, row = blockInfo$row[i], nrows = blockInfo$nrows[i]) %>%
      `==`(maskValue) %>%
      if_else(true = maskValue, false = NA_integer_)
    outputRaster <- writeValues(outputRaster, blockMask, blockInfo$row[i])
  }
  
  outputRaster <- writeStop(outputRaster)
  
  return(outputRaster)
}

# A raster trim function that uses binary search to handle large rasters
# - Requires an output filename, slower than raster::trim for small rasters
# - maxBlockSizePower is an integer that will be used to calculate the maximum
#   of rows / cols to load into memory. Specifically 2^maxBlockSizePower rows and
#   cols will be loaded at most at a time
trimRaster <- function(inputRaster, filename, maxBlockSizePower = 11){
  # Reading portions of large rasters that can't be held in memory is slow, so
  # we want to minimize the number of reads as we identify how much we can trim
  # off each of the four sides of the raster
  
  # One simplistic approach is to check large blocks at a time until a block
  # with non-NA data is found. Next halve the size of the search block and
  # continue searching. Keep halving the the width of the search block until you
  # find the single first column with data.
    
  # Decide how to split input into manageable blocks
  maxBlockSize <- 2^(maxBlockSizePower)
  descendingBlockSizes <- 2^((maxBlockSizePower-1):0)
  
  # Initialize counters
  trimAbove <- 1
  trimBelow <- nrow(inputRaster) + 1
  trimLeft  <- 1
  trimRight <- ncol(inputRaster) + 1

  ## Top
  # Search for the first block from the top with data
  while(
    getValuesBlock(inputRaster,
                   row = trimAbove,
                   nrows = maxBlockSize) %>%
    is.na %>%
    all
  )
    trimAbove <- trimAbove + maxBlockSize

  # Now do a binary search for the first row with data
  for(i in descendingBlockSizes)
    if(getValuesBlock(inputRaster, row = trimAbove, nrows = i) %>% is.na %>% all)
      trimAbove <- trimAbove + i
  
  ## Bottom
  # Repeat from the bottom up, first finding a block that is not all NA
  while(
    getValuesBlock(inputRaster,
                   row = trimBelow - maxBlockSize,
                   nrows = maxBlockSize) %>%
    is.na %>%
    all
  )
    trimBelow <- trimBelow - maxBlockSize
  
  # Binary search for last row with data
  for(i in descendingBlockSizes)
    if(getValuesBlock(inputRaster, row = trimBelow - i,nrows = i) %>% is.na %>% all)
      trimBelow <- trimBelow - i
  
  # Calculate height of the trimmed raster
  outputRows <- trimBelow - trimAbove - 1

  ## Left
  # Search for the first block from the left with data
  while(
    getValuesBlock(inputRaster,
                   col   = trimLeft,
                   ncols = maxBlockSize,
                   row   = trimAbove,
                   nrows = outputRows) %>%
    is.na %>%
    all
  )
    trimLeft <- trimLeft + maxBlockSize
  
  # Now do a binary search for the first row with data
  for(i in descendingBlockSizes)
    if(getValuesBlock(inputRaster,
                      col   = trimLeft,
                      ncols = i,
                      row   = trimAbove,
                      nrows = outputRows) %>%
       is.na %>%
       all)
      trimLeft <- trimLeft + i
  
  ## Right
  # Repeat for the first block from the right with data
  while(
    getValuesBlock(inputRaster,
                   col   = trimRight - maxBlockSize,
                   ncols = maxBlockSize,
                   row   = trimAbove,
                   nrows = outputRows) %>%
    is.na %>%
    all
  )
    trimRight <- trimRight - maxBlockSize

  # Now do a binary search for the first row with data
  for(i in descendingBlockSizes)
    if(getValuesBlock(inputRaster,
                      col   = trimRight - i,
                      ncols = i,
                      row   = trimAbove,
                      nrows = outputRows) %>%
       is.na %>%
       all)
      trimRight <- trimRight - i
  
  # Calculate height of the trimmed raster
  outputCols <- trimRight - trimLeft
  
  # COnvert trim variables to x,y min,max
  outXmin <- xmin(extent(inputRaster)) + trimLeft * res(inputRaster)[1]
  outXmax <- outXmin + outputCols *  res(inputRaster)[1]
  outYmax <- ymax(extent(inputRaster)) - trimAbove * res(inputRaster)[2]
  outYmin <- outYmax - outputRows * res(inputRaster)[2]
  
  # Create empty raster to hold trim results
  outputRaster <-  raster(
    nrows = nrow(inputRaster),
    ncols = ncol(inputRaster),
    ext = extent(inputRaster),
    crs = crs(inputRaster)
  ) %>%
  crop(extent(c(xmin = outXmin,
                xmax = outXmax,
                ymin = outYmin,
                ymax = outYmax)))
  
  ## Fill with values from input
  blockInfo <- blockSize(outputRaster)
  
  # Calculate mask and write to output block-by-block
  outputRaster <- writeStart(outputRaster, filename, overwrite=TRUE)
  
  for(i in seq(blockInfo$n))
    outputRaster <- 
      writeValues(outputRaster,
                  getValuesBlock(inputRaster,
                                 row   = blockInfo$row[i] + trimAbove -1,
                                 nrows = blockInfo$nrows[i],
                                 col   = trimLeft,
                                 ncols = outputCols),
                  blockInfo$row[i])
  
  outputRaster <- writeStop(outputRaster)
  
  return(outputRaster)
}

# Mask and trim mapzone map by the chosen mapzone
# - Note that trimRaster also saves the raster to the cleaned raster directory
mapzoneRaster <-
  maskByMapzone(
    inputRaster = mapzoneRaster, 
    maskRaster = mapzoneRaster,
    maskValue = mapzoneToKeep,
    filename = "temp.tif") %>%
  trimRaster(
    filename = mapzoneRasterPath
  )

# Remove temp files made during masking
unlink("temp.tif")

# Crop and mask data ----------------------------------------------------------

# Begin parallel processing
plan(multisession, workers = nThreads)

# Use a named list to define the rasters and file names to iterate over as we
# crop, mask, and write the outputs to file
rasterList <- list(
  "EVT" = evtRaster,
  "EVH" = evhRaster,
  "EVC" = evcRaster,
  "fDIST" = fdistRaster) %>%
  future_imap(
    function(raster, name, maskRaster){
      crop(raster, maskRaster) %>%
        mask(maskRaster) %>%
        writeRaster(
          filename = paste0(cleanRasterDirectory, name, cleanRasterSuffix, ".tif"),
          overwrite = TRUE
        )
    },
    maskRaster = mapzoneRaster,
    .options = furrr_options(
      seed = TRUE,
      packages = "raster"
    )
  )

# End parallel processing
plan(sequential)

# Assign the results from the list back to the original variables
evtRaster <- rasterList$EVT
evhRaster <- rasterList$EVH
evcRaster <- rasterList$EVC
fdistRaster <- rasterList$fDIST
rm(rasterList)

# Convert disturbance to VDIST ------------------------------------------------

# Choose number of blocks to split rasters into when processing to limit memory
fdistBlockInfo <- blockSize(fdistRaster)

# Begin parallel processing
plan(multisession, workers = nThreads)

# Split the FDIST raster into blocks to calculate the unique set of FDIST codes
# present in the data without using excessive memory per thread
fdistLevels <- 
  future_map(
    seq(fdistBlockInfo$n),
    ~ unique(getValuesBlock(fdistRaster,
                            row   = fdistBlockInfo$row[.x],
                            nrows = fdistBlockInfo$nrows[.x])),
    .options = furrr_options(
      seed = TRUE,
      globals = c("fdistRaster", "fdistBlockInfo"),
      packages = "raster"
      )) %>%
  flatten_int %>%
  unique() %>%
  `[`(!. %in% c(0, NA)) # Remove NA (no data) and 0 (no disturbance)

# End parallel processing
plan(sequential)

# Check which fdist codes need to be reclassified to a different vdist code
distReclassification <- distCrosswalk %>%
  filter(
    fdist %in% fdistLevels,
    fdist != vdist) %>%
  select(-name) %>%
  as.matrix

# Reclassify as necessary and save
vdistRaster <- reclassify(fdistRaster, distReclassification)
writeRaster(vdistRaster,
            vdistRasterPath,
            overwrite = TRUE)

# Generate list of unique VDIST codes
vdistLevels <- distCrosswalk %>%
  filter(fdist %in% fdistLevels) %>%
  pull(vdist) %>%
  unique %>%
  sort 

vdistNames <- distCrosswalk %>%
  select(-fdist) %>%
  filter(vdist %in% vdistLevels) %>%
  arrange(vdist) %>%
  unique() %>%
  pull(name)

# Layerize disturbace raster -------------------------------------------------

# Function to create and save a binary raster given a non-binary raster and the
# value to test for
saveDistLayer <- function(distValue, distName, fullRaster) {
  writeRaster(
    layerize(fullRaster, classes = distValue),
    paste0(transitionMultiplierDirectory, distName, ".tif"),
    overwrite = TRUE
  )
}

# Begin parallel processing
plan(multisession, workers = nThreads)

# Split the VDIST raster into binary layers
# One layer is constructed at a time per thread to limit memory use per thread
future_walk2(
  vdistLevels,
  vdistNames,
  saveDistLayer,
  fullRaster = vdistRaster,
  .options = furrr_options(seed = TRUE))

# Return to sequential operation
plan(sequential)

# Create composite state class map ----------------------------------------

# EVC and EVH codes are both three digits
# We can "paste" these codes together by multiplying EVC by 1000 and summing
stateClasses <- evcRaster * 1000 + evhRaster
writeRaster(stateClasses,
            stateClassRasterPath,
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
    tilingRasterPath,
    nx = tileCols)

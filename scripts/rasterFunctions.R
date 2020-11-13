### LANDFIRE Project 
### APEX RMS - Shreeram Senthivasan 
### November 2020
### This a header file that defines memory-safe raster functions that are optimized
### for large data files that cannot easily be held in memory.

# A memory-safe function to convert a raster with multiple values (map zones)
# into a mask for a single value (map zone)
# - Requires an output filename, slower than raster::mask for small rasters
maskByMapzone <- function(inputRaster, maskValue, filename){
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

# A function to crop rasters down to remove borders filled with only NA's
# - Uses binary search to quickly process large rasters with large empty borders
# - Requires an output filename, slower than raster::trim for small rasters
# - maxBlockSizePower is an integer that will be used to calculate the max number
#   of rows / cols to load into memory. Specifically 2^maxBlockSizePower rows and
#   cols will be loaded at most at a time
trimRaster <- function(inputRaster, filename, maxBlockSizePower = 11){
  # Reading portions of large rasters that can't be held in memory is slow, so
  # we want to minimize the number of reads as we identify how much we can trim
  # off each of the four sides of the raster
  
  # One simplistic approach is to check large blocks at a time until a block
  # with non-NA data is found. Next halve the size of the search block and
  # continue searching. Keep halving the the width of the search block until you
  # find the single first column with data. This is effectively a binary search
  # once the first (largest) block with non-NA data is found.
  
  # Setup --------------------------------------------------------------------
  
  # Decide how to split input into manageable blocks
  maxBlockSize <- 2^(maxBlockSizePower)
  descendingBlockSizes <- 2^((maxBlockSizePower-1):0)
  
  # Initialize counters
  trimAbove <- 1
  trimBelow <- nrow(inputRaster) + 1
  trimLeft  <- 1
  trimRight <- ncol(inputRaster) + 1
  
  # Top ----------------------------------------------------------------------
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
  
  # Bottom  -------------------------------------------------------------------
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
  
  # Left  --------------------------------------------------------------------
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
  
  # Right  --------------------------------------------------------------------
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
  
  # Crop  ---------------------------------------------------------------------
  
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

# Function to create and save a binary raster given a non-binary raster and the
# value to keep
# - Used to binarize disturbance maps for use as spatial multipliers in SyncroSim
saveDistLayer <- function(distValue, distName, fullRaster) {
  writeRaster(
    layerize(fullRaster, classes = distValue),
    paste0(transitionMultiplierDirectory, distName, ".tif"),
    overwrite = TRUE
  )
}

# Function to generate a tiling mask given template
# - To avoid holding the entire raster in memory, the raster is written directly
#   to file row-by-row. This way only one row of the tiling needs to be held in
#   memory at a time. This is also why the number of rows (and implicitly the size
#   of a given row) cannot be chosen manually
tilize <- function(templateRaster, filename, nx) {
  # Calculate recommended block size of template
  blockInfo <- blockSize(templateRaster)
  ny <- blockInfo$n
  
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
  
  # Mask raster by template
  tileRaster <-
    writeRaster(mask(tileRaster, templateRaster), filename, overwrite = TRUE)
  
  return(tileRaster)
}